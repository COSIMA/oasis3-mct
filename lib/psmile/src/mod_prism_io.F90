MODULE mod_prism_io

   USE mod_prism_kinds
   USE mod_prism_data
   USE mod_prism_parameters
   USE mod_prism_sys
   USE mod_prism_ioshr
   USE mct_mod
   USE netcdf

   implicit none

   private

   !--- interfaces ---
   public :: prism_io_read_avfld
   public :: prism_io_read_avfile
   public :: prism_io_write_avfile
   public :: prism_io_write_avfbf
   public :: prism_io_read_avfbf
   public :: prism_io_write_2dgridint_fromroot
   public :: prism_io_write_2dgridfld_fromroot
   public :: prism_io_write_3dgridfld_fromroot

!===========================================================================
CONTAINS
!===========================================================================

!===============================================================================

subroutine prism_io_read_avfld(filename,av,gsmap,avfld,filefld,fldtype)

   ! ---------------------------------------
   ! Reads single field from file to av
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: filename   ! filename
   type(mct_aVect) , intent(inout) :: av      ! avect
   type(mct_gsmap) , intent(in) :: gsmap      ! gsmap
   character(len=*), intent(in) :: avfld      ! av field name
   character(len=*), intent(in) :: filefld    ! file field name
   character(len=*), intent(in),optional :: fldtype       ! int or real

   !--- local ---
   integer(ip_i4_p)    :: n,n1,i,j,fk,fk1    ! index
   integer(ip_i4_p)    :: nx          ! 2d global size nx
   integer(ip_i4_p)    :: ny          ! 2d global size ny
   type(mct_aVect)     :: av_g        ! avect global data
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid2(2),varid ! netcdf info
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance
   real(ip_double_p),allocatable :: array2(:,:)
   integer(ip_i4_p) ,allocatable :: array2i(:,:)
   integer(ip_i4_p)    :: ifldtype     ! field type int (1) or real (2)

   character(len=*),parameter :: subname = 'prism_io_read_avfld'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   ! empty filename, just return

   if (len_trim(filename) < 1) return

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

   ifldtype = 2   ! real default
   if (present(fldtype)) then
      ifldtype = 0
      if (trim(fldtype) == 'int')  ifldtype = 1
      if (trim(fldtype) == 'real') ifldtype = 2
      if (ifldtype == 0) then
         call prism_sys_abort(compid,subname,' ERROR in fldtype argument')
      endif
   endif

   call mct_aVect_gather(av,av_g,gsmap,master_task,mpicom)

   if (iam == master_task) then

      inquire(file=trim(filename),exist=exists)
      if (exists) then
         status = nf90_open(trim(filename),NF90_NOWRITE,ncid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
      else
         write(nulprt,*) subname,' ERROR: file missing ',trim(filename)
         call prism_sys_abort(compid,subname,'ERROR: file missing')
      endif

      status = nf90_inq_varid(ncid,trim(filefld),varid)
      if (status /= nf90_noerr) then
         write(nulprt,*) subname,':',trim(nf90_strerror(status))
         call prism_sys_abort(compid,subname,' ERROR: filefld variable not found '//trim(filefld))
      endif
      status = nf90_inquire_variable(ncid,varid,ndims=dlen,dimids=dimid2)
      if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
      if (dlen /= 2) then
         write(nulprt,*) subname,' ERROR: variable ndims ne 2 ',trim(filefld),dlen
         call prism_sys_abort(compid,subname,' ERROR: variable ndims ne 2')
      endif
      status = nf90_inquire_dimension(ncid,dimid2(1),len=nx)
      if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
      status = nf90_inquire_dimension(ncid,dimid2(2),len=ny)
      if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

      if (size(av_g%rAttr,dim=2) /= nx*ny) then
         write(nulprt,*) subname,' ERROR: av gsize nx ny mismatch ',size(av_g%rAttr,dim=2),nx,ny
         call prism_sys_abort(compid,subname,'ERROR: av_g gsize nx ny mismatch')
      endif

      if (ifldtype == 1) then
         allocate(array2i(nx,ny))
         status = nf90_get_var(ncid,varid,array2i)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

         n = mct_avect_indexIA(av_g,trim(avfld))
         n1 = 0
         do j = 1,ny
         do i = 1,nx
            n1 = n1 + 1
            av_g%iAttr(n,n1) = array2i(i,j)
         enddo
         enddo
         deallocate(array2i)
      else
         allocate(array2(nx,ny))
         status = nf90_get_var(ncid,varid,array2)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

         n = mct_avect_indexRA(av_g,trim(avfld))
         n1 = 0
         do j = 1,ny
         do i = 1,nx
            n1 = n1 + 1
            av_g%rAttr(n,n1) = array2(i,j)
         enddo
         enddo
         deallocate(array2)
      endif

      status = nf90_close(ncid)
      if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

   endif

   call mct_aVect_scatter(av_g,av,gsmap,master_task,mpicom)
   if (iam == master_task) then
      call mct_aVect_clean(av_g)
   endif

end subroutine prism_io_read_avfld

!===============================================================================

subroutine prism_io_write_avfile(rstfile,av,gsmap,nx,ny)

   ! ---------------------------------------
   ! Writes all fields from av to file
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: rstfile    ! restart filename
   type(mct_aVect) , intent(in) :: av         ! avect
   type(mct_gsmap) , intent(in) :: gsmap      ! gsmap
   integer(ip_i4_p), intent(in) :: nx         ! 2d global size nx
   integer(ip_i4_p), intent(in) :: ny         ! 2d global size ny

   !--- local ---
   integer(ip_i4_p)    :: n,n1,i,j,fk,fk1    ! index
   type(mct_aVect)     :: av_g        ! avect global data
   type(mct_string)    :: mstring     ! mct char type
   character(ic_med)   :: itemc       ! string converted to char
   character(ic_med)   :: lstring     ! local filename
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid2(2),varid ! netcdf info
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance
   real(ip_double_p),allocatable :: array2(:,:)

   character(len=*),parameter :: subname = 'prism_io_write_avfile'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   ! empty filename, just return

   if (len_trim(rstfile) < 1) return

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

   call mct_aVect_gather(av,av_g,gsmap,master_task,mpicom)

   if (iam == master_task) then
      if (size(av_g%rAttr,dim=2) /= nx*ny) then
         write(nulprt,*) subname,' ERROR: av gsize nx ny mismatch ',size(av_g%rAttr,dim=2),nx,ny
         call prism_sys_abort(compid,subname,'ERROR: av_g gsize nx ny mismatch')
      endif

      inquire(file=trim(rstfile),exist=exists)
      if (exists) then
         status = nf90_open(trim(rstfile),NF90_WRITE,ncid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_redef(ncid)
      else
         status = nf90_create(trim(rstfile),NF90_CLOBBER,ncid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
      endif

      do n = 1,mct_aVect_nRAttr(av_g)
         call mct_aVect_getRList(mstring,n,av_g)
         itemc = mct_string_toChar(mstring)
         call mct_string_clean(mstring)

         status = nf90_inq_dimid(ncid,trim(itemc)//'_nx',dimid2(1))
         if (status /= nf90_noerr) then
            status = nf90_def_dim(ncid,trim(itemc)//'_nx',nx,dimid2(1))
         endif

         status = nf90_inq_dimid(ncid,trim(itemc)//'_ny',dimid2(2))
         if (status /= nf90_noerr) then
            status = nf90_def_dim(ncid,trim(itemc)//'_ny',ny,dimid2(2))
         endif

         status = nf90_inquire_dimension(ncid,dimid2(1),len=dlen)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         if (dlen /= nx) then
            write(nulprt,*) subname,' ERROR: dlen ne nx ',dlen,nx
            call prism_sys_abort(compid,subname,' ERROR: dlen ne nx')
         endif

         status = nf90_inquire_dimension(ncid,dimid2(2),len=dlen)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         if (dlen /= ny) then
            write(nulprt,*) subname,' ERROR: dlen ne ny ',dlen,ny
            call prism_sys_abort(compid,subname,' ERROR: dlen ne ny')
         endif

         status = nf90_inq_varid(ncid,trim(itemc),varid)
         if (status /= nf90_noerr) then
            status = nf90_def_var(ncid,trim(itemc),NF90_DOUBLE,dimid2,varid)
            if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         endif

      enddo

      status = nf90_enddef(ncid)
      if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

      allocate(array2(nx,ny))
      do n = 1,mct_aVect_nRAttr(av_g)
         call mct_aVect_getRList(mstring,n,av_g)
         itemc = mct_string_toChar(mstring)
         call mct_string_clean(mstring)
         n1 = 0
         do j = 1,ny
         do i = 1,nx
            n1 = n1 + 1
            array2(i,j) = av_g%rAttr(n,n1)
         enddo
         enddo

         status = nf90_inq_varid(ncid,trim(itemc),varid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_put_var(ncid,varid,array2)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
      enddo
      deallocate(array2)
      call mct_aVect_clean(av_g)

      status = nf90_close(ncid)
      if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

   endif

end subroutine prism_io_write_avfile

!===============================================================================

subroutine prism_io_read_avfile(rstfile,av,gsmap)

   ! ---------------------------------------
   ! Reads all fields for av from file
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: rstfile    ! restart filename
   type(mct_aVect) , intent(inout) :: av      ! avect
   type(mct_gsmap) , intent(in) :: gsmap      ! gsmap

   !--- local ---
   integer(ip_i4_p)    :: n,n1,i,j,fk,fk1    ! index
   integer(ip_i4_p)    :: nx          ! 2d global size nx
   integer(ip_i4_p)    :: ny          ! 2d global size ny
   type(mct_aVect)     :: av_g        ! avect global data
   type(mct_string)    :: mstring     ! mct char type
   character(ic_med)   :: itemc       ! string converted to char
   character(ic_med)   :: lstring     ! local filename
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid2(2),varid ! netcdf info
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance
   real(ip_double_p),allocatable :: array2(:,:)

   character(len=*),parameter :: subname = 'prism_io_read_avfile'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   ! empty filename, just return

   if (len_trim(rstfile) < 1) return

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

   call mct_aVect_gather(av,av_g,gsmap,master_task,mpicom)

   if (iam == master_task) then

      inquire(file=trim(rstfile),exist=exists)
      if (exists) then
         status = nf90_open(trim(rstfile),NF90_NOWRITE,ncid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
      else
         write(nulprt,*) subname,' ERROR: file missing ',trim(rstfile)
         call prism_sys_abort(compid,subname,'ERROR: file missing')
      endif

      do n = 1,mct_aVect_nRAttr(av_g)
         call mct_aVect_getRList(mstring,n,av_g)
         itemc = mct_string_toChar(mstring)
         call mct_string_clean(mstring)

         status = nf90_inq_varid(ncid,trim(itemc),varid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_inquire_variable(ncid,varid,ndims=dlen,dimids=dimid2)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         if (dlen /= 2) then
            write(nulprt,*) subname,' ERROR: variable ndims ne 2 ',trim(itemc),dlen
            call prism_sys_abort(compid,subname,' ERROR: variable ndims ne 2')
         endif
         status = nf90_inquire_dimension(ncid,dimid2(1),len=nx)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_inquire_dimension(ncid,dimid2(2),len=ny)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

         if (size(av_g%rAttr,dim=2) /= nx*ny) then
            write(nulprt,*) subname,' ERROR: av gsize nx ny mismatch ',size(av_g%rAttr,dim=2),nx,ny
            call prism_sys_abort(compid,subname,'ERROR: av_g gsize nx ny mismatch')
         endif

         allocate(array2(nx,ny))

         status = nf90_get_var(ncid,varid,array2)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

         n1 = 0
         do j = 1,ny
         do i = 1,nx
            n1 = n1 + 1
            av_g%rAttr(n,n1) = array2(i,j)
         enddo
         enddo

         deallocate(array2)
      enddo

      status = nf90_close(ncid)
      if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

   endif

   call mct_aVect_scatter(av_g,av,gsmap,master_task,mpicom)
   if (iam == master_task) then
      call mct_aVect_clean(av_g)
   endif

end subroutine prism_io_read_avfile

!===============================================================================

subroutine prism_io_write_avfbf(av,gsmap,nx,ny,msec,string,filename)

   ! ---------------------------------------
   ! Write all fields from av to individual field files
   ! This works only for a single av to a file
   ! Optionally can specify time info, and filename info
   ! ---------------------------------------

   implicit none

   type(mct_aVect) , intent(in) :: av         ! avect
   type(mct_gsmap) , intent(in) :: gsmap      ! gsmap
   integer(ip_i4_p), intent(in) :: nx         ! 2d global size nx
   integer(ip_i4_p), intent(in) :: ny         ! 2d global size ny
   integer(ip_i4_p), intent(in),optional :: msec    ! time info
   character(len=*), intent(in),optional :: string  ! optional string to append to filename
   character(len=*), intent(in),optional :: filename   ! optional output filename

   !--- local ---
   integer(ip_i4_p)    :: n,n1,i,j,fk,fk1    ! index
   type(mct_aVect)     :: av_g        ! avect global data
   type(mct_string)    :: mstring     ! mct char type
   character(ic_med)   :: itemc       ! string converted to char
   character(ic_med)   :: lfn         ! local filename
   character(ic_med)   :: lstring     ! local filename
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid3(3),varid ! netcdf info
   integer(ip_i4_p)    :: start3(3),count3(3)        ! netcdf info
   integer(ip_i4_p)    :: start1(1),count1(1)        ! netcdf info
   integer(ip_i4_p)    :: lmsec(1)    ! local msec value
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance
   logical             :: whead,wdata ! for writing restart/history cdf files
   real(ip_double_p),allocatable :: array3(:,:,:)
   real(ip_double_p)   :: tbnds(2)

   character(len=*),parameter :: subname = 'prism_io_write_avfbf'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   lmsec = 0
   if (present(msec)) then
      lmsec = msec
   endif

   lstring = " "
   if (present(string)) then
      lstring = trim(string)
   endif

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

#if (PIO_DEFINED)
! tcraig, not working as of Oct 2011
   call prism_ioshr_wopen(lfn,clobber=.true.,cdf64=.true.)

   do fk = fk1,2
      if (fk == 1) then
         whead = .true.
         wdata = .false.
      elseif (fk == 2) then
         whead = .false.
         wdata = .true.
      else
         call prism_sys_abort(compid,subname,'ERROR: fk illegal')
      end if

      call prism_ioshr_write(lfn,&
               time_units='seconds',time_cal='seconds',time_val=real(lmsec,ip_double_p),&
               nt=1,whead=whead,wdata=wdata)

      call prism_ioshr_write(lfn, gsmap, av, 'pout', &
               whead=whead,wdata=wdata,nx=nx,ny=ny,nt=1, &
               use_float=.false.)
   
      if (fk == 1) call prism_ioshr_enddef(lfn)
   enddo

   call prism_ioshr_close(lfn)
#else

   call mct_aVect_gather(av,av_g,gsmap,master_task,mpicom)
   if (iam == master_task) then
      if (size(av_g%rAttr,dim=2) /= nx*ny) then
         write(nulprt,*) subname,' ERROR: av gsize nx ny mismatch ',size(av_g%rAttr,dim=2),nx,ny
         call prism_sys_abort(compid,subname,'ERROR: av_g gsize nx ny mismatch')
      endif

      allocate(array3(nx,ny,1))
      do n = 1,mct_aVect_nRAttr(av_g)
         call mct_aVect_getRList(mstring,n,av_g)
         itemc = mct_string_toChar(mstring)
         call mct_string_clean(mstring)
         if (present(filename)) then
            lfn = trim(filename)
         else
            lfn = trim(itemc)//trim(lstring)//'.nc'
         endif
         n1 = 0
         do j = 1,ny
         do i = 1,nx
            n1 = n1 + 1
            array3(i,j,1) = av_g%rAttr(n,n1)
         enddo
         enddo

         start1 = 1
         count1 = 1
         start3 = 1
         count3(1) = nx
         count3(2) = ny
         count3(3) = 1

         inquire(file=trim(lfn),exist=exists)
         if (exists) then
            status = nf90_open(lfn,NF90_WRITE,ncid)
            if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
            status = nf90_inq_dimid(ncid,'time',dimid)
            if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
            status = nf90_inquire_dimension(ncid,dimid,len=dlen)
            if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
            start1(1) = dlen + 1
            start3(3) = start1(1)
         else
            status = nf90_create(lfn,NF90_CLOBBER,ncid)
            if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
            status = nf90_def_dim(ncid,'nx',nx,dimid3(1))
            if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
            status = nf90_def_dim(ncid,'ny',ny,dimid3(2))
            if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
            status = nf90_def_dim(ncid,'time',NF90_UNLIMITED,dimid)
            if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
            dimid3(3) = dimid
            status = nf90_def_var(ncid,'time',NF90_INT,dimid,varid)
            if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
            status = nf90_def_var(ncid,trim(itemc),NF90_DOUBLE,dimid3,varid)
            if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
            status = nf90_enddef(ncid)
            if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         endif

         status = nf90_inq_varid(ncid,'time',varid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_put_var(ncid,varid,lmsec,start1,count1)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_inq_varid(ncid,trim(itemc),varid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_put_var(ncid,varid,array3,start3,count3)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_close(ncid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
      enddo
      deallocate(array3)
      call mct_aVect_clean(av_g)
   endif


#endif

end subroutine prism_io_write_avfbf

!===============================================================================

subroutine prism_io_read_avfbf(av,gsmap,msec,string,filename)

   ! ---------------------------------------
   ! Read all fields to av from individual field files
   ! This works only for a single av from a file
   ! Optionally can specify time info, and filename info
   ! ---------------------------------------

   implicit none

   type(mct_aVect) , intent(inout) :: av     ! avect
   type(mct_gsmap) , intent(in) :: gsmap     ! gsmap
   integer(ip_i4_p), intent(in),optional :: msec    ! time info
   character(len=*), intent(in),optional :: string  ! optional string to append to filename
   character(len=*), intent(in),optional :: filename   ! optional input filename

   !--- local ---
   integer(ip_i4_p)    :: n,n1,i,j,fk,fk1    ! index
   integer(ip_i4_p)    :: nx,ny       ! grid size from file
   type(mct_aVect)     :: av_g        ! avect global data
   type(mct_string)    :: mstring     ! mct char type
   character(ic_med)   :: itemc       ! string converted to char
   character(ic_med)   :: lfn         ! local filename
   character(ic_med)   :: lstring     ! local filename
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid3(3),varid ! netcdf info
   integer(ip_i4_p)    :: start3(3),count3(3)        ! netcdf info
   integer(ip_i4_p)    :: lmsec(1)    ! local msec value
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance
   logical             :: whead,wdata ! for writing restart/history cdf files
   real(ip_double_p),allocatable :: array3(:,:,:)
   integer(ip_i4_p) ,allocatable :: time(:)
   real(ip_double_p)   :: tbnds(2)

   character(len=*),parameter :: subname = 'prism_io_read_avfbf'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   lmsec = 0
   if (present(msec)) then
      lmsec = msec
   endif

   lstring = " "
   if (present(string)) then
      lstring = trim(string)
   endif

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

   call mct_aVect_gather(av,av_g,gsmap,master_task,mpicom)
   if (iam == master_task) then
      do n = 1,mct_aVect_nRAttr(av_g)
         call mct_aVect_getRList(mstring,n,av_g)
         itemc = mct_string_toChar(mstring)
         call mct_string_clean(mstring)
         if (present(filename)) then
            lfn = trim(filename)
         else
            lfn = trim(itemc)//trim(lstring)//'.nc'
         endif

         inquire(file=trim(lfn),exist=exists)
         if (.not.exists) then
            write(nulprt,*) subname,' ERROR: file not found ',trim(lfn)
            call prism_sys_abort(compid,subname,'ERROR: file not found')
         endif

         status = nf90_open(lfn,NF90_NOWRITE,ncid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

         status = nf90_inq_dimid(ncid,'time',dimid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_inquire_dimension(ncid,dimid,len=dlen)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         allocate(time(dlen))
         status = nf90_inq_varid(ncid,'time',varid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_get_var(ncid,varid,time)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         n1 = 0
         do j = 1,dlen
            if (time(j) == lmsec(1)) n1 = j
         enddo
         deallocate(time)
         if (n1 < 1) then
            write(nulprt,*) subname,' ERROR: time not found on file ',trim(lfn),lmsec
            call prism_sys_abort(compid,subname,'ERROR: time not found on file')
         endif

         status = nf90_inq_varid(ncid,trim(itemc),varid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_inquire_variable(ncid,varid,dimids=dimid3)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_inquire_dimension(ncid,dimid3(1),len=nx)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_inquire_dimension(ncid,dimid3(2),len=ny)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

         if (size(av_g%rAttr,dim=2) /= nx*ny) then
             write(nulprt,*) subname,' ERROR: av gsize nx ny mismatch ',size(av_g%rAttr,dim=2),nx,ny
             call prism_sys_abort(compid,subname,'ERROR: av_g gsize nx ny mismatch')
         endif

         start3 = 1
         count3(1) = nx
         count3(2) = ny
         count3(3) = 1
         start3(3) = n1
         allocate(array3(nx,ny,1))

         status = nf90_get_var(ncid,varid,array3,start3,count3)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
         status = nf90_close(ncid)
         if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

         n1 = 0
         do j = 1,ny
         do i = 1,nx
            n1 = n1 + 1
            av_g%rAttr(n,n1) = array3(i,j,1)
         enddo
         enddo

         deallocate(array3)

      enddo
   endif

   call mct_aVect_scatter(av_g,av,gsmap,master_task,mpicom)
   if (iam == master_task) then
      call mct_aVect_clean(av_g)
   endif

end subroutine prism_io_read_avfbf

!===============================================================================

subroutine prism_io_write_2dgridfld_fromroot(filename,fldname,fld,nx,ny)

   ! ---------------------------------------
   ! Write real fld on rootpe to file
   ! Designed to work with oasis3 write_grid 
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: fldname
   real(ip_realwp_p), intent(in) :: fld(:,:)
   integer(ip_i4_p), intent(in) :: nx         ! 2d global size nx
   integer(ip_i4_p), intent(in) :: ny         ! 2d global size ny

   !--- local ---
   integer(ip_i4_p)    :: ncid,dimid,dimid2(2),varid  ! cdf info
   integer(ip_i4_p)    :: status      ! error code
   integer(ip_i4_p)    :: ind         ! string index
   logical             :: exists      ! file existance
   character(len=ic_med) :: gridname  ! grid name derived from fldname

   character(len=*),parameter :: subname = 'prism_io_write_2dgridfld_fromroot'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   expects to run only on 1 pe.
!   if (iam == master_task) then

    ind = index(trim(fldname),'.')
    if (ind < 2) then
       write(nulprt,*) subname,' ERROR: in fldname ',trim(fldname)
       call prism_sys_abort(compid,subname,' ERROR in fldname')
    endif
    gridname = fldname(1:ind-1)

    inquire(file=trim(filename),exist=exists)
    if (exists) then
       status = nf90_open(filename,NF90_WRITE,ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
       status = nf90_redef(ncid)
    else
       status = nf90_create(filename,NF90_CLOBBER,ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    ! define x dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'x_'//trim(gridname),dimid2(1))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'x_'//trim(gridname),nx,dimid2(1))
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    ! define y dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'y_'//trim(gridname),dimid2(2))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'y_'//trim(gridname),ny,dimid2(2))
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    ! define var if it doesn't exist
    status = nf90_inq_varid(ncid,trim(fldname),varid)
    if (status /= nf90_noerr) then
       status = nf90_def_var(ncid,trim(fldname),NF90_DOUBLE,dimid2,varid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
       status = nf90_enddef(ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
       status = nf90_put_var(ncid,varid,fld)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    else
       status = nf90_enddef(ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    status = nf90_close(ncid)
    if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

!   endif

end subroutine prism_io_write_2dgridfld_fromroot

!===============================================================================

subroutine prism_io_write_2dgridint_fromroot(filename,fldname,fld,nx,ny)

   ! ---------------------------------------
   ! Write int fld on rootpe to file
   ! Designed to work with oasis3 write_grid 
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: fldname
   integer(ip_i4_p), intent(in) :: fld(:,:)
   integer(ip_i4_p), intent(in) :: nx         ! 2d global size nx
   integer(ip_i4_p), intent(in) :: ny         ! 2d global size ny

   !--- local ---
   integer(ip_i4_p)    :: ncid,dimid,dimid2(2),varid  ! cdf info
   integer(ip_i4_p)    :: status      ! error code
   integer(ip_i4_p)    :: ind         ! string index
   logical             :: exists      ! file existance
   character(len=ic_med) :: gridname  ! grid name derived from fldname

   character(len=*),parameter :: subname = 'prism_io_write_2dgridint_fromroot'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   expects to run only on 1 pe.
!   if (iam == master_task) then

    ind = index(trim(fldname),'.')
    if (ind < 2) then
       write(nulprt,*) subname,' ERROR: in fldname ',trim(fldname)
       call prism_sys_abort(compid,subname,' ERROR in fldname')
    endif
    gridname = fldname(1:ind-1)

    inquire(file=trim(filename),exist=exists)
    if (exists) then
       status = nf90_open(filename,NF90_WRITE,ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
       status = nf90_redef(ncid)
    else
       status = nf90_create(filename,NF90_CLOBBER,ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    ! define x dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'x_'//trim(gridname),dimid2(1))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'x_'//trim(gridname),nx,dimid2(1))
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    ! define y dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'y_'//trim(gridname),dimid2(2))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'y_'//trim(gridname),ny,dimid2(2))
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    ! define var if it doesn't exist
    status = nf90_inq_varid(ncid,trim(fldname),varid)
    if (status /= nf90_noerr) then
       status = nf90_def_var(ncid,trim(fldname),NF90_INT,dimid2,varid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
       status = nf90_enddef(ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
       status = nf90_put_var(ncid,varid,fld)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    else
       status = nf90_enddef(ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    status = nf90_close(ncid)
    if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

!   endif

end subroutine prism_io_write_2dgridint_fromroot

!===============================================================================

subroutine prism_io_write_3dgridfld_fromroot(filename,fldname,fld,nx,ny,nc)
 
   ! ---------------------------------------
   ! Write real 3d fld on rootpe to file
   ! Designed to work with oasis3 write_grid (corners)
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: fldname
   real(ip_realwp_p), intent(in) :: fld(:,:,:)
   integer(ip_i4_p), intent(in) :: nx         ! 3d global size nx
   integer(ip_i4_p), intent(in) :: ny         ! 3d global size ny
   integer(ip_i4_p), intent(in) :: nc         ! 3d global size nc ncorners

   !--- local ---
   integer(ip_i4_p)    :: ncid,dimid,dimid3(3),varid  ! cdf info
   integer(ip_i4_p)    :: status      ! error code
   integer(ip_i4_p)    :: ind         ! string index
   logical             :: exists      ! file existance
   character(len=ic_med) :: gridname  ! grid name derived from fldname

   character(len=*),parameter :: subname = 'prism_io_write_3dgridfld_fromroot'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   expects to run only on 1 pe.
!   if (iam == master_task) then

    ind = index(trim(fldname),'.')
    if (ind < 2) then
       write(nulprt,*) subname,' ERROR: in fldname ',trim(fldname)
       call prism_sys_abort(compid,subname,' ERROR in fldname')
    endif
    gridname = fldname(1:ind-1)

    inquire(file=trim(filename),exist=exists)
    if (exists) then
       status = nf90_open(filename,NF90_WRITE,ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
       status = nf90_redef(ncid)
    else
       status = nf90_create(filename,NF90_CLOBBER,ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    ! define x dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'x_'//trim(gridname),dimid3(1))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'x_'//trim(gridname),nx,dimid3(1))
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    ! define y dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'y_'//trim(gridname),dimid3(2))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'y_'//trim(gridname),ny,dimid3(2))
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    ! define crn dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'crn_'//trim(gridname),dimid3(3))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'crn_'//trim(gridname),nc,dimid3(3))
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    ! define var if it doesn't exist
    status = nf90_inq_varid(ncid,trim(fldname),varid)
    if (status /= nf90_noerr) then
       status = nf90_def_var(ncid,trim(fldname),NF90_DOUBLE,dimid3,varid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
       status = nf90_enddef(ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
       status = nf90_put_var(ncid,varid,fld)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    else
       status = nf90_enddef(ncid)
       if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))
    endif

    status = nf90_close(ncid)
    if (status /= nf90_noerr) write(nulprt,*) subname,':',trim(nf90_strerror(status))

!   endif

end subroutine prism_io_write_3dgridfld_fromroot

!-------------------------------------------------------------------

END MODULE mod_prism_io
