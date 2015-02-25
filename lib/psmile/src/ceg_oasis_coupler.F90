MODULE ceg_oasis_coupler
!     - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
  USE mod_oasis_kinds
  USE mod_oasis_data
  USE mod_oasis_parameters
  USE mod_oasis_namcouple
  USE mod_oasis_sys
!  USE mod_oasis_var, only : prism_nvar
  USE mod_oasis_part
  USE mod_oasis_var
  USE mod_oasis_mpi
  USE mod_oasis_string
  USE mod_oasis_io
  USE mod_oasis_timer
  USE mct_mod
  USE grids    ! scrip
  USE netcdf

  IMPLICIT NONE

   !--- local kinds ---
   integer,private,parameter :: R8 = ip_double_p
   integer,private,parameter :: IN = ip_i4_p

   !--- variable assocaited with local data buffers and reallocation
   real(R8),private,allocatable :: Snew(:,:),Sold(:,:)  ! reals
   integer,private,allocatable :: Rnew(:),Rold(:)  ! ints
   integer,private,allocatable :: Cnew(:),Cold(:)  ! ints

  public 


!------------------------------------------------------------
CONTAINS
!------------------------------------------------------------


!BOP ===========================================================================
!
! !IROUTINE:  oasis_coupler_sMatReaddnc - Do a distributed read of a NetCDF SCRIP file and
!                                return weights in a distributed SparseMatrix
!
! !DESCRIPTION: 
!     Read in mapping matrix data from a SCRIP netCDF data file using
!     a low memory method and then scatter to all pes.  Based on 
!     oasis_coupler_sMatReaddnc from CESM1.0.3
!
! !REMARKS:
!   This routine leverages gsmaps to determine scatter pattern
!   The scatter is implemented as a bcast of all weights then a local
!     computation on each pe to determine with weights to keep based
!     on gsmap information.
!   The algorithm to determine whether a weight belongs which pe involves
!     checking whether a global index is on the local pe.
!   The local buffer sizes are estimated up front based on ngridcell/npes
!     plus 20% (see 1.2 below).  If the local buffer size fills up, then
!     the buffer is reallocated 50% large (see 1.5 below) and the fill
!     continues.  The idea is to trade off memory reallocation and copy
!     with memory usage.  1.2 and 1.5 are arbitary, other values may
!     result in better performance.
!   Once all the matrix weights have been read, the sMat is initialized,
!     the values from the buffers are copied in, and everything is deallocated.
!
! !INTERFACE:  -----------------------------------------------------------------

subroutine ceg_coupler_sMatReaddnc(sMat,SgsMap,DgsMap,newdom, &
                            fileName,mytask,mpicom,nwgts, &
                            areasrc,areadst,ni_i,nj_i,ni_o,nj_o )

! !USES:

! !INPUT/OUTPUT PARAMETERS:

   type(mct_sMat)  ,intent(out),pointer   :: sMat(:) ! mapping data
   type(mct_gsMap) ,intent(in) ,target    :: SgsMap  ! src gsmap
   type(mct_gSMap) ,intent(in) ,target    :: DgsMap  ! dst gsmap
   character(*)    ,intent(in)            :: newdom  ! type of sMat (src or dst)
        ! src = rearrange and map (bfb), dst = map and rearrange (partial sums)
   character(*)    ,intent(in)            :: filename! netCDF file to read
   integer(IN)     ,intent(in)            :: mytask   ! processor id
   integer(IN)     ,intent(in)            :: mpicom  ! communicator
   integer(IN)     ,intent(out)           :: nwgts   ! number of weights 
   type(mct_Avect) ,intent(out), optional :: areasrc ! area of src grid from mapping file
   type(mct_Avect) ,intent(out), optional :: areadst ! area of dst grid from mapping file
   integer(IN)     ,intent(out), optional :: ni_i    ! number of lons on input grid   
   integer(IN)     ,intent(out), optional :: nj_i    ! number of lats on input grid   
   integer(IN)     ,intent(out), optional :: ni_o    ! number of lons on output grid   
   integer(IN)     ,intent(out), optional :: nj_o    ! number of lats on output grid   

! !EOP

   !--- local ---
   integer           :: n,m     ! generic loop indicies
   integer           :: na      ! size of source domain
   integer           :: nb      ! size of destination domain
   integer           :: ns      ! number of non-zero elements in matrix
   integer           :: ni,nj   ! number of row and col in the matrix
   integer           :: igrow   ! aVect index for matrix row
   integer           :: igcol   ! aVect index for matrix column
   integer           :: iwgt    ! aVect index for matrix element
   integer           :: iarea   ! aVect index for area
   integer           :: rsize   ! size of read buffer
   integer           :: cnt     ! local num of wgts
   integer           :: start(1)! netcdf read
   integer           :: count(1)! netcdf read
   integer           :: start2(2)! netcdf read
   integer           :: count2(2)! netcdf read
   integer           :: bsize   ! buffer size
   integer           :: nread   ! number of reads 
   logical           :: mywt    ! does this weight belong on my pe
   integer           :: dims(2) 

   !--- buffers for i/o ---
   real(R8)   ,allocatable :: rtemp(:) ! real temporary
   real(R8)   ,allocatable :: Sbuf(:,:)  ! real weights
   real(R8)   ,allocatable :: remaps(:,:)  ! real weights with num_wgts dim
   integer,allocatable     :: Rbuf(:)  ! ints rows
   integer,allocatable     :: Cbuf(:)  ! ints cols
   real(R8),allocatable    :: SReadData(:,:)   !
   integer,allocatable     :: RReadData(:)     !
   integer,allocatable     :: CReadData(:)     !
   integer,allocatable     :: pesave(:)        !
   real(R8),allocatable    :: SDistData(:,:) !
   integer,allocatable     :: RDistData(:)   !
   integer,allocatable     :: CDistData(:)   !

   !--- variables associated with local computation of global indices
   integer             :: commsize  ! size of local communicator
   type(mct_gsMap),pointer :: mygsmap ! pointer to one of the gsmaps
   integer             :: l1,l2     ! generice indices for sort
   logical             :: found     ! for sort
   integer             :: pe          ! Process ID of owning process
   integer             :: reclen      ! Length of Rbuf/Cbuf to be received
   integer             :: ierr        ! MPI error check
   integer,allocatable :: cntrs(:)    ! Counters of number of elements stored for each processor
   integer             :: mpistatus(MPI_STATUS_SIZE)      ! MPI_Status object

   character,allocatable :: str(:)  ! variable length char string
   character(len=ic_long):: attstr  ! netCDF attribute name string
   integer           :: status   ! netCDF routine return code
   integer           :: fid     ! netCDF file      ID
   integer           :: vid     ! netCDF variable  ID
   integer           :: did     ! netCDF dimension ID
   !--- arbitrary size of read buffer, this is the chunk size weights reading
   integer,parameter :: rbuf_size = 100000
   integer           :: dimbuffer(8)

   !--- global source and destination areas ---
   type(mct_Avect) :: areasrc0   ! area of src grid from mapping file
   type(mct_Avect) :: areadst0   ! area of src grid from mapping file

   character(*),parameter :: areaAV_field = 'aream'

   !--- formats ---
   character(*),parameter :: subName = '(ceg_coupler_sMatReaddnc)'
   character*80 :: fname
  
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

 call oasis_debug_enter(subname)
 call oasis_mpi_commsize(mpicom,commsize)
 nwgts = -1
 if (mytask == 0) then
   if (OASIS_debug >= 2) write(nulprt,*) subname," reading mapping matrix data decomposed..."

   !----------------------------------------------------------------------------
   ! open & read the file
   !----------------------------------------------------------------------------
   if (OASIS_debug >=2 ) write(nulprt,*) subname," * file name                  : ",trim(fileName)
   status = nf90_open(trim(filename),NF90_NOWRITE,fid)
   if (status /= NF90_NOERR) then
      write(nulprt,*) subname,' nf90_strerror = ',trim(nf90_strerror(status))
      WRITE(nulprt,*) subname,estr,'mapping file not found = ',trim(filename)
      call oasis_abort()
   endif

   !--- get matrix dimensions ----------
!  status = nf90_inq_dimid (fid, 'n_s', did)  ! size of sparse matrix
   status = nf90_inq_dimid (fid, 'num_links', did)  ! size of sparse matrix
   status = nf90_inquire_dimension(fid, did  , len = ns)
!  status = nf90_inq_dimid (fid, 'n_a', did)  ! size of  input vector
   status = nf90_inq_dimid (fid, 'src_grid_size', did)  ! size of  input vector
   status = nf90_inquire_dimension(fid, did  , len = na)
!  status = nf90_inq_dimid (fid, 'n_b', did)  ! size of output vector
   status = nf90_inq_dimid (fid, 'dst_grid_size', did)  ! size of output vector
   status = nf90_inquire_dimension(fid, did  , len = nb)
   status = nf90_inq_dimid (fid, 'num_wgts', did)  ! size of output vector
   status = nf90_inquire_dimension(fid, did  , len = nwgts)
   
   if (present(ni_i) .and. present(nj_i) .and. present(ni_o) .and. present(nj_o)) then
!     status = nf90_inq_dimid (fid, 'ni_a', did)  ! number of lons in input grid
!     status = nf90_inquire_dimension(fid, did  , len = ni_i)
!     status = nf90_inq_dimid (fid, 'nj_a', did)  ! number of lats in input grid
!     status = nf90_inquire_dimension(fid, did  , len = nj_i)
!     status = nf90_inq_dimid (fid, 'ni_b', did)  ! number of lons in output grid
!     status = nf90_inquire_dimension(fid, did  , len = ni_o)
!     status = nf90_inq_dimid (fid, 'nj_b', did)  ! number of lats in output grid
!     status = nf90_inquire_dimension(fid, did  , len = nj_o)
      status = nf90_inq_varid(fid, 'src_grid_dims', vid)
      status = nf90_get_var(fid, vid, dims)
      ni_i = dims(1)
      nj_i = dims(2)
      status = nf90_inq_varid(fid, 'dst_grid_dims', vid)
      status = nf90_get_var(fid, vid, dims)
      ni_o = dims(1)
      nj_o = dims(2)
   end if

   if (OASIS_debug >= 2) write(nulprt,*) subname," * matrix dims src x dst      : ",na,' x',nb
   if (OASIS_debug >= 2) write(nulprt,*) subname," * number of non-zero elements: ",ns

 endif
 
   !--- read and load area_a ---
   if (present(areasrc)) then
   if (mytask == 0) then
      call mct_aVect_init(areasrc0,' ',areaAV_field,na)
!     status = nf90_inq_varid     (fid,'area_a',vid)
      status = nf90_inq_varid     (fid,'src_grid_area',vid)
      IF (status /= NF90_NOERR) THEN
          WRITE(nulprt,*) subname,' nf90_strerrr = ',TRIM(nf90_strerror(status))
          WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
      ENDIF
      status = nf90_get_var(fid, vid, areasrc0%rAttr)
      IF (status /= NF90_NOERR) THEN
          WRITE(nulprt,*) subname,' nf90_strerror = ',TRIM(nf90_strerror(status))
          WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
      ENDIF
   endif
   call mct_aVect_scatter(areasrc0, areasrc, SgsMap, 0, mpicom, status)
   if (status /= 0) call mct_die(subname,"Error on scatter of areasrc0")
   if (mytask == 0) then
!      if (present(dbug)) then
!         if (dbug > 2) then
!            write(nulprt,*) subName,'min/max src ',minval(areasrc0%rAttr(1,:)),maxval(areasrc0%rAttr(1,:))
!         endif
!      end if
      call mct_aVect_clean(areasrc0)
   end if
   end if

   !--- read and load area_b ---
   if (present(areadst)) then
   if (mytask == 0) then
      call mct_aVect_init(areadst0,' ',areaAV_field,nb)
!     status = nf90_inq_varid     (fid,'area_b',vid)
      status = nf90_inq_varid     (fid,'dst_grid_area',vid)
      IF (status /= NF90_NOERR) THEN
          WRITE(nulprt,*) subname,' nf90_strerror = ',TRIM(nf90_strerror(status))
          WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
      ENDIF
      status = nf90_get_var(fid, vid, areadst0%rAttr)
      IF (status /= NF90_NOERR) THEN
          WRITE(nulprt,*) subname,' nf90_strerror = ',TRIM(nf90_strerror(status))
          WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
      ENDIF
   endif
   call mct_aVect_scatter(areadst0, areadst, DgsMap, 0, mpicom, status)
   if (status /= 0) call mct_die(subname,"Error on scatter of areadst0")
   if (mytask == 0) then
!      if (present(dbug)) then
!         if (dbug > 2) then
!            write(nulprt,*) subName,'min/max dst ',minval(areadst0%rAttr(1,:)),maxval(areadst0%rAttr(1,:))
!         endif
!      end if
      call mct_aVect_clean(areadst0)
   endif
   endif

  ! Replace 8 MPI_Bcasts with just one
   if (mpi_rank_local.eq.0) then
      dimbuffer(1) = ns
      dimbuffer(2) = na
      dimbuffer(3) = nb
      dimbuffer(4) = nwgts
      if (present(ni_i) .and. present(nj_i) .and. present(ni_o) .and. present(nj_o)) then
         dimbuffer(5) = ni_i
         dimbuffer(6) = nj_i
         dimbuffer(7) = ni_o
         dimbuffer(8) = nj_o
      end if
   endif
   call oasis_mpi_bcast(dimbuffer,mpicom,subName//" MPI of dimbuffer")
   if (mpi_rank_local.ne.0) then
      ns = dimbuffer(1)
      na = dimbuffer(2)
      nb = dimbuffer(3)
      nwgts = dimbuffer(4)
      if (present(ni_i) .and. present(nj_i) .and. present(ni_o) .and. present(nj_o)) then
         ni_i = dimbuffer(5)
         nj_i = dimbuffer(6)
         ni_o = dimbuffer(7)
         nj_o = dimbuffer(8)
      end if
   endif      

!         call oasis_mpi_bcast(ni_i,mpicom,subName//" MPI in ni_i bcast")
!         call oasis_mpi_bcast(nj_i,mpicom,subName//" MPI in nj_i bcast")
!         call oasis_mpi_bcast(ni_o,mpicom,subName//" MPI in ni_o bcast")
!         call oasis_mpi_bcast(nj_o,mpicom,subName//" MPI in nj_o bcast")

   !--- setup local seg map, sorted
   if (newdom == 'src') then
      mygsmap => DgsMap
   elseif (newdom == 'dst') then
      mygsmap => SgsMap
   else
      write(nulprt,*) subname,estr,'invalid newdom value, expect src or dst = ',newdom
      call oasis_abort()
   endif

   rsize = min(rbuf_size,ns)                     ! size of i/o chunks
   bsize = ((ns/commsize) + 1 ) * 1.2   ! local temporary buffer size
   if (ns == 0) then
      nread = 0
   else
      nread = (ns-1)/rsize + 1                      ! num of reads to do
   endif

   allocate(Smat(nwgts),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Smat',status)
   allocate(Sbuf(nwgts,rsize),Rbuf(rsize),Cbuf(rsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Sbuf',status)
   allocate(Snew(nwgts,bsize),Cnew(bsize),Rnew(bsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Snew1',status)
!   write(nulprt,*) subname,mpi_rank_local,rsize,rsize*nwgts

   cnt = 1

   if (mytask== 0) then
      ! Allocate memory for reading and distributing data
      allocate(remaps(nwgts,rsize),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate remaps',status)
      allocate(SReadData(nwgts,rsize),RReadData(rsize),CReadData(rsize),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate SReadData',status)
      allocate(pesave(rsize),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate pesave',status)
      allocate(SDistData(nwgts,rsize),RDistData(rsize),CDistData(rsize), stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate SDistData',status)
      allocate(cntrs(0:mpi_size_local), stat=status) ! Purposefully allocating one extra
      if (status /= 0) call mct_perr_die(subName,':: allocate Smat',status)

      ! Loop over number of chunks to read fromNetCDF file
      do n = 1,nread
         start(1) = (n-1)*rsize + 1
         count(1) = min(rsize,ns-start(1)+1)
         start2(1) = 1
         count2(1) = nwgts
         start2(2) = start(1)
         count2(2) = count(1)

      !--- read data on root pe
!         write(nulprt,*) subname,'Hey Chris, reading chunk ', n
!        status = nf90_inq_varid      (fid,'S'  ,vid)
         status = nf90_inq_varid      (fid,'remap_matrix'  ,vid)
!        status = nf90_get_var(fid,vid,start,count,Sbuf)
         status = nf90_get_var(fid,vid,remaps,start2,count2)
         SReadData(:,:) = remaps(:,:)
         IF (status /= NF90_NOERR) THEN
             WRITE(nulprt,*) subname,' nf90_strerror = ',TRIM(nf90_strerror(status))
             WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
         ENDIF
!        status = nf90_inq_varid      (fid,'row',vid)
         status = nf90_inq_varid      (fid,'dst_address',vid)
         status = nf90_get_var   (fid,vid,RReadData,start,count)
         IF (status /= NF90_NOERR) THEN
             WRITE(nulprt,*) subname,' nf90_strerror = ',TRIM(nf90_strerror(status))
             WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
         ENDIF

!        status = nf90_inq_varid      (fid,'col',vid)
         status = nf90_inq_varid      (fid,'src_address',vid)
         status = nf90_get_var   (fid,vid,CReadData,start,count)
         IF (status /= NF90_NOERR) THEN
             WRITE(nulprt,*) subname,' nf90_strerror = ',TRIM(nf90_strerror(status))
             WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
         ENDIF

!
!       TESTING    newdom=src   hence set using Rbuf
!
        ! Two stage process
        !   1. Count how many to send to each process
        !   2. Build the sending array up in process receiving order
        
       
         ! Initialize cntrs array for number of accumulations per process
         cntrs  =0
         pesave = -99

         !--- decide which pe keeps each point
         do m = 1,count(1)
            !--- which process owns this point?
            if (newdom == 'src') then
               pe = get_cegindex(RReadData(m),mygsmap)
            else if (newdom == 'dst') then
               pe = get_cegindex(CReadData(m),mygsmap)
            endif
            pesave(m) = pe
            
            ! Copy data, incrementing index array, pe = 1 to mpi_size_local
            if (pe+1 < 1 .or. pe+1  > mpi_size_local) then
               write(nulprt,*) subname,wstr,'get_cegindex search error', m,count(1),CReadData(m),RReadData(m),SReadData(:,m)
            else
               cntrs(pe+1) = cntrs(pe+1) + 1  ! Note incrementing 1->noprocs rather than 0->noprocs-1
            endif

         end do
         
         ! Now turn cntrs into an array showing the offsets into the send array for each process
         cntrs(0) = 1
         do pe = 1, mpi_size_local
            cntrs(pe) = cntrs(pe-1) + cntrs(pe)
         end do

         !--- decide which pe keeps each point
         do m = 1,count(1)
            !--- which process owns this point?
            if (newdom == 'src') then
               pe = get_cegindex(RReadData(m),mygsmap)
            else if (newdom == 'dst') then
               pe = get_cegindex(CReadData(m),mygsmap)
            endif

            pe = pesave(m)

            ! Copy data, incrementing index array, pe = 1 to mpi_size_local
            if (pe+1 < 1 .or. pe+1  > mpi_size_local) then
               ! skip it
               ! write(nulprt,*) subname,'get_cegindex search error2', m,count(1),CReadData(m),RReadData(m),SReadData(:,m)
            else
               ! Copy data, incrementing index array
               SDistData(:,cntrs(pe)) = SReadData(:,m)
               RDistData(cntrs(pe)) = RReadData(m)
               CDistData(cntrs(pe)) = CReadData(m)
            
               cntrs(pe) = cntrs(pe) + 1
            endif

         enddo  ! count

         ! Dispatch data if buffer is non-zero
         if (cntrs(0).gt.1) then ! Need to do it differently if it is for me!
            reclen = cntrs(0)-1
            call augment_arrays(cnt, reclen, bsize, nwgts)
           
            !--- now p0 copies straight into the ?new arrays
            Snew(1:nwgts,cnt:cnt+reclen-1) = SDistData(1:nwgts,1:reclen)
            Rnew(cnt:cnt+reclen-1) = RDistData(1:reclen)
            Cnew(cnt:cnt+reclen-1) = CDistData(1:reclen)
            cnt = cnt + reclen

         endif
         
         do pe = 1, mpi_size_local
            if (cntrs(pe).gt.cntrs(pe-1)) then
               ! Send data out
               m = cntrs(pe)-cntrs(pe-1)
!               write(nulprt,*) subname, 'Sending [to, datalen] ', pe, m, cntrs(pe-1), cntrs(pe)
               call MPI_Send(m, 1, MPI_INTEGER, pe, 4000, mpicom, ierr)
               call MPI_Send(SDistData(1,cntrs(pe-1)), nwgts*m, MPI_REAL8, pe, 1000, mpicom, ierr)
               call MPI_Send(RDistData(cntrs(pe-1)), m, MPI_INTEGER, pe, 2000, mpicom, ierr)
               call MPI_Send(CDistData(cntrs(pe-1)), m, MPI_INTEGER, pe, 3000, mpicom, ierr)
            endif
         enddo               

      enddo   ! nread

      !--- Send signal saying all data is sent
      m = -1
      do pe = 1, mpi_size_local-1
         !write(nulprt,*) subname, 'Final sending [to, datalen] ', m, cntrs(m)
         call MPI_Send(m, 1, MPI_INTEGER, pe, 4000, mpicom, ierr)
      end do

      ! Free memory used during the distribution
      deallocate(pesave, stat=status)
      if (status /= 0) call mct_perr_die(subName,':: deallocate pesave',status)
      deallocate(SReadData,RReadData,CReadData, stat=status)
      if (status /= 0) call mct_perr_die(subName,':: deallocate SReadData',status)
      deallocate(SDistData,RDistData,CDistData, stat=status)
      if (status /= 0) call mct_perr_die(subName,':: deallocate SDistData',status)
      deallocate(cntrs, stat=status)
      if (status /= 0) call mct_perr_die(subName,':: deallocate cntrs',status)
      deallocate(remaps, stat=status)
      if (status /= 0) call mct_perr_die(subName,':: deallocate remaps',status)

   else
   
      call oasis_mpi_recv(reclen, 0, 4000, mpicom, subName//" MPI in reclen recv")
      ! Check we haven't now had the last data
      do while (reclen.ne.-1) 

         !--- recv S, row, col on all other pes
         !write(nulprt,*) subname, 'Receiving [global-id, datalen] ', mpi_rank_global, reclen
         call MPI_Recv(Sbuf, reclen*nwgts, MPI_REAL8, 0, 1000, mpicom, MPI_STATUS_IGNORE, ierr)
         call MPI_Recv(Rbuf, reclen, MPI_INTEGER, 0, 2000, mpicom, MPI_STATUS_IGNORE, ierr)
         call MPI_Recv(Cbuf, reclen, MPI_INTEGER, 0, MPI_ANY_TAG, mpicom, mpistatus, ierr)

         call augment_arrays(cnt, reclen, bsize, nwgts)

         !--- now each pe keeps what it has been sent
         Snew(1:nwgts,cnt:cnt+reclen-1) = Sbuf(1:nwgts,1:reclen)
         Rnew(cnt:cnt+reclen-1) = Rbuf(1:reclen)
         Cnew(cnt:cnt+reclen-1) = Cbuf(1:reclen)
         cnt = cnt + reclen

         call oasis_mpi_recv(reclen, 0, 4000, mpicom, subName//" MPI in reclen recv")
      end do

   endif

   ! Fix cnt to be the length of the array
   cnt = cnt-1

!   write(fname,'(A,I3.3)') '/tmp/cegtest_', mpi_rank_global 
!   open (unit=63, file=fname, status='unknown')
!   do n=1,cnt
!      do m=1,nwgts
!         write(63,'(F16.10)') Snew(m,n)
!      end do
!      write(63,'(I7,2X,I7)') Rnew(n), Cnew(n)
!   end do
!   close(63)

   deallocate(Sbuf,Rbuf,Cbuf, stat=status)
   if (status /= 0) call mct_perr_die(subName,':: deallocate Sbuf',status)

   !----------------------------------------------------------------------------
   ! init the mct sMat data type
   !----------------------------------------------------------------------------
   ! mct_sMat_init must be given the number of rows and columns that
   ! would be in the full matrix.  Nrows= size of output vector=nb.
   ! Ncols = size of input vector = na.
   do n = 1,nwgts
      call mct_sMat_init(sMat(n), nb, na, cnt)
   enddo

   igrow = mct_sMat_indexIA(sMat(1),'grow')
   igcol = mct_sMat_indexIA(sMat(1),'gcol')
   iwgt  = mct_sMat_indexRA(sMat(1),'weight')

   if (cnt /= 0) then
   do n = 1,nwgts
      sMat(n)%data%rAttr(iwgt ,1:cnt) = Snew(n,1:cnt)
      sMat(n)%data%iAttr(igrow,1:cnt) = Rnew(1:cnt)
      sMat(n)%data%iAttr(igcol,1:cnt) = Cnew(1:cnt)
   enddo
   endif
   deallocate(Snew,Rnew,Cnew, stat=status)
   if (status /= 0) call mct_perr_die(subName,':: deallocate new',status)

   if (mytask == 0) then
      status = nf90_close(fid)
      IF (OASIS_debug >= 2) THEN
          WRITE(nulprt,*) subname," ... done reading file"
          CALL oasis_flush(nulprt)
      ENDIF
   endif

   call oasis_debug_exit(subname)

!  write(nulprt,*) subname,mpi_rank_local,'done reading'

end subroutine ceg_coupler_sMatReaddnc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!  Extend array and store data  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine augment_arrays(cnt, reclen, bsize, nwgts)

! !USES:

   integer, intent(inout) :: cnt
   integer, intent(in)    :: reclen
   integer, intent(inout) :: bsize
   integer, intent(in)    :: nwgts

   integer           :: status   ! netCDF routine return code
   !--- formats ---
   character(*),parameter :: subName = '(ceg_coupler_augment_arrays)'


   !--- ?new arrays need to be bigger?
   if (cnt+reclen > bsize) then
  
!      write(nulprt,*) subname,mpi_rank_global, 'EXTEND'

      !--- allocate old arrays and copy new into old
      allocate(Sold(1:nwgts,cnt),Rold(cnt),Cold(cnt),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate old',status)
      Sold(1:nwgts,1:cnt-1) = Snew(1:nwgts,1:cnt-1)
      Rold(1:cnt-1) = Rnew(1:cnt-1)
      Cold(1:cnt-1) = Cnew(1:cnt-1)

      !--- reallocate new to bigger size, increase buffer by 50% (arbitrary)
      deallocate(Snew,Rnew,Cnew,stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate new',status)
      bsize = 1.5 * (cnt+reclen)
      if (OASIS_debug > 15) write(nulprt,*) subname,' reallocate bsize to ',bsize
      allocate(Snew(nwgts,bsize),Rnew(bsize),Cnew(bsize),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate old',status)

      !--- copy data back into new
      Snew(1:nwgts,1:cnt-1) = Sold(1:nwgts,1:cnt-1)
      Rnew(1:cnt-1) = Rold(1:cnt-1)
      Cnew(1:cnt-1) = Cold(1:cnt-1)
      deallocate(Sold,Rold,Cold,stat=status)
      if (status /= 0) call mct_perr_die(subName,':: deallocate old',status)
   endif

end subroutine augment_arrays

!------------------------------------------------------------
! !BOP ===========================================================================
!
! !IROUTINE:  get_cegindex - binary search for index in list
!
! !DESCRIPTION: 
!     Do a binary search to see if a value is contained in a list of
!     values.  return value of processor that owns it
!     starti must be monotonically
!     increasing, function does NOT check this.
!
! !INTERFACE:  -----------------------------------------------------------------

integer function get_cegindex(index,mygsmap)

! !USES:


   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p

! !INPUT/OUTPUT PARAMETERS:

   type(mct_gsMap),pointer :: mygsmap ! pointer to one of the gsmaps
   integer(IN) :: index       ! is this index in start/count list

! !EOP

   !--- local ---
   integer(IN)    :: nl,nc,nr,ncprev 
   logical        :: stopnow
   integer, save  :: prevnc = 1  ! Store previous found segment -- guess at 0 for forst call

   !--- formats ---
   character(*),parameter :: subName = '(get_cegindex) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

! Need to search my version of GlobalSegMap to find which process owns this point

  if (prevnc .gt. size(mygsmap%start)) then
      prevnc=1
  endif

  ! First test if last time was correct
  if (index .ge.mygsmap%start(prevnc) .and. index.le.mygsmap%start(prevnc) + mygsmap%length(prevnc)-1) then
     get_cegindex = mygsmap%pe_loc(prevnc)
     if ( OASIS_debug >= 40 ) then
        write(nulprt,*) subname,'prevnc=',index,prevnc,mygsmap%start(prevnc),mygsmap%start(prevnc) + mygsmap%length(prevnc)-1
        call oasis_flush(nulprt)
     endif
     return
  endif


! Can still use binary search

   nl = 0                   ! Note use 0 and +1 in these two lines to save test of division by 2 later on
   nr = mygsmap%ngseg+1     !
   nc = (nl+nr)/2
   stopnow = .false.
!   write(nulprt,*) subname,mpi_rank_local, mygsmap%start(:)
!   write(nulprt,*) subname,mpi_rank_local, mygsmap%length(:)
   get_cegindex = -1
   do while (.not.stopnow)
      if ( OASIS_debug >= 40 ) then
         write(nulprt,'(a,7i7)') subname,mpi_rank_local, nl,nc,nr,index,mygsmap%start(nc), mygsmap%length(nc)
      endif
      if (index < mygsmap%start(nc)) then
         nr = nc
      elseif (index > (mygsmap%start(nc) + mygsmap%length(nc) - 1)) then
         nl = nc
      else
         get_cegindex = mygsmap%pe_loc(nc)
!        call oasis_debug_exit(subname)
!      write(nulprt,*) subname,mpi_rank_local, 'Found'
         prevnc = nc
         return
      endif
      ncprev = nc
      nc = (nl + nr)/2
!      write(nulprt,*) subname,mpi_rank_local, 'loop',nc,ncprev, mygsmap%ngseg
      if (nc == ncprev .or. nc < 1 .or. nc > mygsmap%ngseg) stopnow = .true.
   enddo

   if ( OASIS_debug >= 40 ) then
      write(nulprt,'(2a,6i7)') subname,'cegindex=',get_cegindex,mpi_rank_local, nl,nc,nr,index
   endif



end function get_cegindex

!===============================================================================
END MODULE ceg_oasis_coupler


