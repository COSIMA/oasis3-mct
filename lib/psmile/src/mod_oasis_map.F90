
!> OASIS map (interpolation) data and methods


MODULE mod_oasis_map

  USE mod_oasis_kinds
  USE mod_oasis_data
  USE mod_oasis_parameters
  USE mod_oasis_namcouple
  USE mod_oasis_sys
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

  private

  public oasis_map_genmap
  public oasis_map_sMatReaddnc_orig
  public oasis_map_sMatReaddnc_ceg

  !--- Type of data

  public prism_mapper_type

  !> Mapper data for interpolating data between grids
  type prism_mapper_type
     !--- fixed at initialization ---
     type(mct_sMatP),pointer :: sMatP(:)  !< stores mapping data such as weights
     integer(kind=ip_i4_p) :: nwgts       !< number of weights in weights file
     character(len=ic_long):: file        !< file to read/write
     character(len=ic_med) :: loc         !< location setting, src or dst model
     character(len=ic_med) :: opt         !< optimization setting, bfb, sum, or opt
     character(len=ic_med) :: optval      !< mct map setting, src or dst, derived from opt
     logical               :: init        !< flag indicating initialization complete
     integer(kind=ip_i4_p) :: spart       !< src partition
     integer(kind=ip_i4_p) :: dpart       !< dst partition
     character(len=ic_med) :: srcgrid     !< source grid name
     character(len=ic_med) :: dstgrid     !< target grid name
     logical               :: AVred       !< flag indicating AV_ms, AV_md data has been read
     type(mct_aVect)       :: AV_ms       !< stores data for CONSERV for src such as mask and area
     type(mct_aVect)       :: AV_md       !< stores data for CONSERV for dst such as mask and area
  end type prism_mapper_type

  integer(kind=ip_i4_p)   ,public :: prism_mmapper   !< max mappers
  integer(kind=ip_i4_p)   ,public :: prism_nmapper = 0  !< mapper counter
  type(prism_mapper_type) ,public, pointer :: prism_mapper(:)  !< list of defined mappers

  !--- local kinds ---

  integer,private,parameter :: R8 = ip_double_p
  integer,private,parameter :: IN = ip_i4_p

  !--- variable assocaited with local data buffers and reallocation

  real(R8),private,allocatable :: Snew(:,:),Sold(:,:)  ! reals
  integer,private,allocatable :: Rnew(:),Rold(:)  ! ints
  integer,private,allocatable :: Cnew(:),Cold(:)  ! ints

!------------------------------------------------------------
CONTAINS
!------------------------------------------------------------

!------------------------------------------------------------

!> Routine to generate mapping weights data via a direct SCRIP call

!> This routine reads in grid data from files and passes that data
!> to SCRIP.  Mapping weights are generated and written to a file.
!> This entire operation is done on a single task.

  SUBROUTINE oasis_map_genmap(mapid,namid)

  IMPLICIT NONE

  integer(ip_i4_p), intent(in) :: mapid  !< map id
  integer(ip_i4_p), intent(in) :: namid  !< namcouple id
  !----------------------------------------------------------

  integer(ip_i4_p)              :: src_size,src_rank, ncrn_src
  integer(ip_i4_p) ,allocatable :: src_dims(:),src_mask(:)
  real(ip_double_p),allocatable :: src_lon(:),src_lat(:)
  real(ip_double_p),allocatable :: src_corner_lon(:,:),src_corner_lat(:,:)
  integer(ip_i4_p)              :: dst_size,dst_rank, ncrn_dst
  integer(ip_i4_p) ,allocatable :: dst_dims(:),dst_mask(:)
  real(ip_double_p),allocatable :: dst_lon(:),dst_lat(:)
  real(ip_double_p),allocatable :: dst_corner_lon(:,:),dst_corner_lat(:,:)
  integer(ip_i4_p) ,allocatable :: ifld2(:,:)
  real(ip_double_p),allocatable :: fld2(:,:),fld3(:,:,:)
  integer(ip_i4_p) :: i,j,k,icnt,nx,ny,nc
  logical :: lextrapdone
  logical :: do_corners
  character(len=ic_med) :: filename
  character(len=ic_med) :: fldname
  character(len=*),parameter :: subname = '(oasis_map_genmap)'

  call oasis_debug_enter(subname)

  lextrapdone = .false.
  nx = -1  ! must be read
  ny = -1  ! must be read
  nc =  1  ! might not be read, set to something reasonable

  !--- checks first ---

  if (trim(namscrtyp(namID)) /= 'SCALAR') then
     write(nulprt,*) subname,estr,'only scrip type SCALAR mapping supported'
     CALL oasis_abort()
  endif

  
  do_corners = .false.
  if (trim(namscrmet(namID)) == 'CONSERV') then
     do_corners = .true.
  endif

  !--- src data ---

  filename = 'grids.nc'
  if (do_corners) then
     fldname = trim(namsrcgrd(namID))//'.clo'
     call oasis_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny,nz=nc)
  else
     fldname = trim(namsrcgrd(namID))//'.lon'
     call oasis_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny)
  endif
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',&
                                         trim(fldname),nx,ny,nc,do_corners
  src_rank = 2
  src_size = nx*ny
  allocate(src_dims(src_rank))
  src_dims(1) = nx
  src_dims(2) = ny
  ncrn_src = nc
  allocate(src_mask(src_size))
  allocate(src_lon (src_size))
  allocate(src_lat (src_size))
  allocate(src_corner_lon(ncrn_src,src_size))
  allocate(src_corner_lat(ncrn_src,src_size))

  allocate(ifld2(nx,ny))
  filename = 'masks.nc'
  fldname = trim(namsrcgrd(namID))//'.msk'
  call oasis_io_read_field_fromroot(filename,fldname,ifld2=ifld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     src_mask(icnt) = ifld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(src_mask),maxval(src_mask)
  deallocate(ifld2)

  allocate(fld2(nx,ny))
  filename = 'grids.nc'
  fldname = trim(namsrcgrd(namID))//'.lon'
  call oasis_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     src_lon(icnt) = fld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(src_lon),maxval(src_lon)
  fldname = trim(namsrcgrd(namID))//'.lat'
  call oasis_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     src_lat(icnt) = fld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(src_lat),maxval(src_lat)
  deallocate(fld2)

  if (do_corners) then
     allocate(fld3(nx,ny,nc))
     filename = 'grids.nc'
     fldname = trim(namsrcgrd(namID))//'.clo'
     call oasis_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           src_corner_lon(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
        minval(src_corner_lon),maxval(src_corner_lon)
     fldname = trim(namsrcgrd(namID))//'.cla'
     call oasis_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           src_corner_lat(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
        minval(src_corner_lat),maxval(src_corner_lat)
     deallocate(fld3)
  else
     src_corner_lon = -9999.
     src_corner_lat = -9999.
  endif

  !--- dst data ---

  filename = 'grids.nc'
  if (do_corners) then
     fldname = trim(namdstgrd(namID))//'.clo'
     call oasis_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny,nz=nc)
  else
     fldname = trim(namdstgrd(namID))//'.lon'
     call oasis_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny)
  endif
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname),nx,ny,nc
  dst_rank = 2
  dst_size = nx*ny
  allocate(dst_dims(dst_rank))
  dst_dims(1) = nx
  dst_dims(2) = ny
  ncrn_dst = nc
  allocate(dst_mask(dst_size))
  allocate(dst_lon (dst_size))
  allocate(dst_lat (dst_size))
  allocate(dst_corner_lon(ncrn_dst,dst_size))
  allocate(dst_corner_lat(ncrn_dst,dst_size))

  allocate(ifld2(nx,ny))
  filename = 'masks.nc'
  fldname = trim(namdstgrd(namID))//'.msk'
  call oasis_io_read_field_fromroot(filename,fldname,ifld2=ifld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     dst_mask(icnt) = ifld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(dst_mask),maxval(dst_mask)
  deallocate(ifld2)

  allocate(fld2(nx,ny))
  filename = 'grids.nc'
  fldname = trim(namdstgrd(namID))//'.lon'
  call oasis_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     dst_lon(icnt) = fld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(dst_lon),maxval(dst_lon)
  fldname = trim(namdstgrd(namID))//'.lat'
  call oasis_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     dst_lat(icnt) = fld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(dst_lat),maxval(dst_lat)
  deallocate(fld2)

  if (do_corners) then
     allocate(fld3(nx,ny,nc))
     filename = 'grids.nc'
     fldname = trim(namdstgrd(namID))//'.clo'
     call oasis_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           dst_corner_lon(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
        minval(dst_corner_lon),maxval(dst_corner_lon)
     fldname = trim(namdstgrd(namID))//'.cla'
     call oasis_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           dst_corner_lat(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
        minval(dst_corner_lat),maxval(dst_corner_lat)
     deallocate(fld3)
  else
     dst_corner_lon = -9999.
     dst_corner_lat = -9999.
  endif

  IF (OASIS_debug >= 15) THEN
      WRITE(nulprt,*) subname,' call grid_init '
      CALL oasis_flush(nulprt)
  ENDIF

  !--- 0/1 mask convention opposite in scrip vs oasis
  src_mask = 1 - src_mask
  dst_mask = 1 - dst_mask
  call grid_init(namscrmet(namID),namscrres(namID),namscrbin(namID),  &
       src_size, dst_size, src_dims, dst_dims, &
       src_rank, dst_rank, ncrn_src, ncrn_dst, &
       src_mask, dst_mask, namsrcgrd(namID), namdstgrd(namID), &
       src_lat,  src_lon,  dst_lat,  dst_lon, &
       src_corner_lat, src_corner_lon, &
       dst_corner_lat, dst_corner_lon, &
       logunit=nulprt)
  if (OASIS_debug >= 15) then
      WRITE(nulprt,*) subname,' done grid_init '
      CALL oasis_flush(nulprt)
  ENDIF

  IF (OASIS_debug >= 15) THEN
      WRITE(nulprt,*) subname,' call scrip '
      CALL oasis_flush(nulprt)
  ENDIF
  call scrip(prism_mapper(mapid)%file,prism_mapper(mapid)%file,namscrmet(namID), &
             namscrnor(namID),lextrapdone,namscrvam(namID),namscrnbr(namID))
  IF (OASIS_debug >= 15) THEN
      WRITE(nulprt,*) subname,' done scrip '
      CALL oasis_flush(nulprt)
  ENDIF

  deallocate(src_dims, dst_dims)
  deallocate(src_mask)
  deallocate(src_lon)
  deallocate(src_lat)
  deallocate(src_corner_lon)
  deallocate(src_corner_lat)
  deallocate(dst_mask)
  deallocate(dst_lon)
  deallocate(dst_lat)
  deallocate(dst_corner_lon)
  deallocate(dst_corner_lat)


  call oasis_debug_exit(subname)

  END SUBROUTINE oasis_map_genmap

!------------------------------------------------------------
!BOP ===========================================================================
!> Read in mapping matrix data from a SCRIP netCDF weights file
!
! !IROUTINE:  oasis_map_sMatReaddnc_orig - Do a distributed read of a NetCDF SCRIP file and
!                                return weights in a distributed SparseMatrix
!
! !DESCRIPTION: 
!>     Read in mapping matrix data from a SCRIP netCDF data file using
!>     a low memory method and then scatter to all pes.  Based on 
!>     the sMatReaddnc method from CESM1.0.3.
!>
! !REMARKS:
!>   This routine leverages gsmaps to determine scatter pattern.
!>
!>   The scatter is implemented as a broadcast of all weights then a local
!>     computation on each pe to determine with weights to keep based
!>     on gsmap information.
!>
!>   The algorithm to determine whether a weight belongs on a pe involves
!>     creating a couple local arrays (lsstart and lscount) which are
!>     the local values of start and length from the gsmap.  These are
!>     sorted via a bubble sort and then searched via a binary search
!>     to check whether a global index is on the local pe.
!>
!>   The local buffer sizes are estimated up front based on ngridcell/npes
!>     plus 20% (search for 1.2 below).  If the local buffer size fills up, then
!>     the buffer is reallocated 50% larger (search for 1.5 below) and the fill
!>     continues.  The idea is to trade off memory reallocation and copy
!>     with memory usage.  1.2 and 1.5 are arbitary, other values may
!>     result in better performance.
!>
!>   Once all the matrix weights have been read, the sMat is initialized,
!>     the values from the buffers are copied in, and everything is deallocated.
!
! !INTERFACE:  -----------------------------------------------------------------

subroutine oasis_map_sMatReaddnc_orig(sMat,SgsMap,DgsMap,newdom, &
                            fileName,mytask,mpicom,nwgts, &
                            areasrc,areadst,ni_i,nj_i,ni_o,nj_o )

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p

! !INPUT/OUTPUT PARAMETERS:

   type(mct_sMat)  ,intent(out),pointer   :: sMat(:) !< mapping data
   type(mct_gsMap) ,intent(in) ,target    :: SgsMap  !< src gsmap
   type(mct_gSMap) ,intent(in) ,target    :: DgsMap  !< dst gsmap
   character(*)    ,intent(in)            :: newdom  !< type of sMat (src or dst)
        !< src = rearrange and map (bfb), dst = map and rearrange (partial sums)
   character(*)    ,intent(in)            :: filename!< netCDF file to read
   integer(IN)     ,intent(in)            :: mytask  !< processor id
   integer(IN)     ,intent(in)            :: mpicom  !< mpi communicator
   integer(IN)     ,intent(out)           :: nwgts   !< number of weights 
   type(mct_Avect) ,intent(out), optional :: areasrc !< area of src grid from mapping file
   type(mct_Avect) ,intent(out), optional :: areadst !< area of dst grid from mapping file
   integer(IN)     ,intent(out), optional :: ni_i    !< number of lons on input grid   
   integer(IN)     ,intent(out), optional :: nj_i    !< number of lats on input grid   
   integer(IN)     ,intent(out), optional :: ni_o    !< number of lons on output grid   
   integer(IN)     ,intent(out), optional :: nj_o    !< number of lats on output grid   

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
   integer           :: cntold  ! local num of wgts, previous read
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
   integer,allocatable :: Rbuf(:)  ! ints rows
   integer,allocatable :: Cbuf(:)  ! ints cols

   !--- variables associated with local computation of global indices
   integer             :: lsize     ! size of local seg map
   integer             :: commsize  ! size of local communicator
   integer,allocatable :: lsstart(:) ! local seg map info
   integer,allocatable :: lscount(:) ! local seg map info
   type(mct_gsMap),pointer :: mygsmap ! pointer to one of the gsmaps
   integer             :: l1,l2     ! generice indices for sort
   logical             :: found     ! for sort

   !--- variable assocaited with local data buffers and reallocation
   real(R8)   ,allocatable :: Snew(:,:),Sold(:,:)  ! reals
   integer,allocatable :: Rnew(:),Rold(:)  ! ints
   integer,allocatable :: Cnew(:),Cold(:)  ! ints

   character,allocatable :: str(:)  ! variable length char string
   character(len=ic_long):: attstr  ! netCDF attribute name string
   integer           :: status   ! netCDF routine return code
   integer           :: fid     ! netCDF file      ID
   integer           :: vid     ! netCDF variable  ID
   integer           :: did     ! netCDF dimension ID
   !--- arbitrary size of read buffer, this is the chunk size weights reading
   integer,parameter :: rbuf_size = 100000

   !--- global source and destination areas ---
   type(mct_Avect) :: areasrc0   ! area of src grid from mapping file
   type(mct_Avect) :: areadst0   ! area of src grid from mapping file

   character(*),parameter :: areaAV_field = 'aream'

   !--- formats ---
   character(*),parameter :: subName = '(oasis_map_sMatReaddnc_orig)'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

 call oasis_debug_enter(subname)
 call oasis_mpi_commsize(mpicom,commsize)
 nwgts = -1
 if (mytask == 0) then
   if (OASIS_debug >= 2) write(nulprt,*) subname," reading mapping matrix data decomposed..."

   !----------------------------------------------------------------------------
   !> * Open and read the file SCRIP weights size on the root task
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
 
   !----------------------------------------------------------------------------
   !> * Read and load area_a on root task
   !----------------------------------------------------------------------------
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
!            write(nulprt,*) subName,'Size of src ',mct_aVect_lSize(areasrc0)
!            write(nulprt,*) subName,'min/max src ',minval(areasrc0%rAttr(1,:)),maxval(areasrc0%rAttr(1,:))
!         endif
!      end if
      call mct_aVect_clean(areasrc0)
   end if
   end if

   !----------------------------------------------------------------------------
   !> * Read and load area_b on root task
   !----------------------------------------------------------------------------
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
!            write(nulprt,*) subName,'Size of dst ',mct_aVect_lSize(areadst0)
!            write(nulprt,*) subName,'min/max dst ',minval(areadst0%rAttr(1,:)),maxval(areadst0%rAttr(1,:))
!         endif
!      end if
      call mct_aVect_clean(areadst0)
   endif
   endif

   !----------------------------------------------------------------------------
   !> * Broadcast ni and nj if requested
   !----------------------------------------------------------------------------
   if (present(ni_i) .and. present(nj_i) .and. present(ni_o) .and. present(nj_o)) then
      call oasis_mpi_bcast(ni_i,mpicom,subName//" MPI in ni_i bcast")
      call oasis_mpi_bcast(nj_i,mpicom,subName//" MPI in nj_i bcast")
      call oasis_mpi_bcast(ni_o,mpicom,subName//" MPI in ni_o bcast")
      call oasis_mpi_bcast(nj_o,mpicom,subName//" MPI in nj_o bcast")
   end if

   !----------------------------------------------------------------------------
   !> * Broadcast array sizes and allocate arrays for local storage
   !----------------------------------------------------------------------------
   call oasis_mpi_bcast(ns,mpicom,subName//" MPI in ns bcast")
   call oasis_mpi_bcast(na,mpicom,subName//" MPI in na bcast")
   call oasis_mpi_bcast(nb,mpicom,subName//" MPI in nb bcast")
   call oasis_mpi_bcast(nwgts,mpicom,subName//" MPI in nwgts bcast")

   !--- setup local seg map, sorted
   if (newdom == 'src') then
      mygsmap => DgsMap
   elseif (newdom == 'dst') then
      mygsmap => SgsMap
   else
      write(nulprt,*) subname,estr,'invalid newdom value, expect src or dst = ',newdom
      call oasis_abort()
   endif
   lsize = 0
   do n = 1,size(mygsmap%start)
      if (mygsmap%pe_loc(n) == mytask) then
         lsize=lsize+1
      endif
   enddo
   allocate(lsstart(lsize),lscount(lsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Lsstart',status)

   !----------------------------------------------------------------------------
   !> * Initialize lsstart and lscount, the sorted list of local indices
   !----------------------------------------------------------------------------
   lsize = 0
   do n = 1,size(mygsmap%start)
      if (mygsmap%pe_loc(n) == mytask) then  ! on my pe
         lsize=lsize+1
         found = .false.
         l1 = 1
         do while (.not.found .and. l1 < lsize)         ! bubble sort copy
            if (mygsmap%start(n) < lsstart(l1)) then
               do l2 = lsize, l1+1, -1
                  lsstart(l2) = lsstart(l2-1)
                  lscount(l2) = lscount(l2-1)
               enddo
               found = .true.
            else
               l1 = l1 + 1
            endif
         enddo
         lsstart(l1) = mygsmap%start(n)
         lscount(l1) = mygsmap%length(n)
      endif
   enddo
   do n = 1,lsize-1
      if (lsstart(n) > lsstart(n+1)) then
         write(nulprt,*) subname,estr,'lsstart not properly sorted'
         call oasis_abort()
      endif
   enddo

   !----------------------------------------------------------------------------
   !> * Compute the number of chunks to read, read size is rbuf_size
   !----------------------------------------------------------------------------
   rsize = min(rbuf_size,ns)                     ! size of i/o chunks
   bsize = ((ns/commsize) + 1 ) * 1.2   ! local temporary buffer size
   if (ns == 0) then
      nread = 0
   else
      nread = (ns-1)/rsize + 1                      ! num of reads to do
   endif

   !----------------------------------------------------------------------------
   !> * Allocate arrays for local weights plus row and column indices
   !----------------------------------------------------------------------------
   if (mytask == 0) then
      allocate(remaps(nwgts,rsize),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate remaps',status)
   endif
   allocate(Smat(nwgts),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Smat',status)
   allocate(Sbuf(nwgts,rsize),Rbuf(rsize),Cbuf(rsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Sbuf',status)
   allocate(Snew(nwgts,bsize),Cnew(bsize),Rnew(bsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Snew1',status)

   !----------------------------------------------------------------------------
   !> * Loop over the chunks of weights data
   !----------------------------------------------------------------------------
   cnt = 0
   do n = 1,nread
      start(1) = (n-1)*rsize + 1
      count(1) = min(rsize,ns-start(1)+1)
      start2(1) = 1
      count2(1) = nwgts
      start2(2) = start(1)
      count2(2) = count(1)

      !----------------------------------------------------------------------------
      !>   * Read chunk of data on root pe
      !----------------------------------------------------------------------------
      if (mytask== 0) then
!        status = nf90_inq_varid      (fid,'S'  ,vid)
         status = nf90_inq_varid      (fid,'remap_matrix'  ,vid)
!        status = nf90_get_var(fid,vid,start,count,Sbuf)
         status = nf90_get_var(fid,vid,remaps,start2,count2)
         Sbuf(:,:) = remaps(:,:)
         IF (status /= NF90_NOERR) THEN
             WRITE(nulprt,*) subname,' nf90_strerror = ',TRIM(nf90_strerror(status))
             WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
         ENDIF

!        status = nf90_inq_varid      (fid,'row',vid)
         status = nf90_inq_varid      (fid,'dst_address',vid)
         status = nf90_get_var   (fid,vid,Rbuf,start,count)
         IF (status /= NF90_NOERR) THEN
             WRITE(nulprt,*) subname,' nf90_strerror = ',TRIM(nf90_strerror(status))
             WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
         ENDIF

!        status = nf90_inq_varid      (fid,'col',vid)
         status = nf90_inq_varid      (fid,'src_address',vid)
         status = nf90_get_var   (fid,vid,Cbuf,start,count)
         IF (status /= NF90_NOERR) THEN
             WRITE(nulprt,*) subname,' nf90_strerror = ',TRIM(nf90_strerror(status))
             WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
         ENDIF
      endif
 
      !----------------------------------------------------------------------------
      !>   * Broadcast S, row, col to all tasks
      !----------------------------------------------------------------------------

      call oasis_mpi_bcast(Sbuf,mpicom,subName//" MPI in Sbuf bcast")
      call oasis_mpi_bcast(Rbuf,mpicom,subName//" MPI in Rbuf bcast")
      call oasis_mpi_bcast(Cbuf,mpicom,subName//" MPI in Cbuf bcast")

      !----------------------------------------------------------------------------
      !>   * Each task keeps only the data required
      !----------------------------------------------------------------------------
      do m = 1,count(1)
         !--- should this weight be on my pe
         if (newdom == 'src') then
            mywt = check_myindex(Rbuf(m),lsstart,lscount)
         elseif (newdom == 'dst') then
            mywt = check_myindex(Cbuf(m),lsstart,lscount)
         endif

         if (mywt) then
            cntold = cnt
            cnt = cnt + 1

            !----------------------------------------------------------------------------
            !>   * Reallocate local weights arrays if they need to be bigger
            !----------------------------------------------------------------------------
            if (cnt > bsize) then
               !--- allocate old arrays and copy new into old
               allocate(Sold(1:nwgts,cntold),Rold(cntold),Cold(cntold),stat=status)
               if (status /= 0) call mct_perr_die(subName,':: allocate old',status)
               Sold(1:nwgts,1:cntold) = Snew(1:nwgts,1:cntold)
               Rold(1:cntold) = Rnew(1:cntold)
               Cold(1:cntold) = Cnew(1:cntold)

               !--- reallocate new to bigger size, increase buffer by 50% (arbitrary)
               deallocate(Snew,Rnew,Cnew,stat=status)
               if (status /= 0) call mct_perr_die(subName,':: allocate new',status)
               bsize = 1.5 * bsize
               if (OASIS_debug > 15) write(nulprt,*) subname,' reallocate bsize to ',bsize
               allocate(Snew(nwgts,bsize),Rnew(bsize),Cnew(bsize),stat=status)
               if (status /= 0) call mct_perr_die(subName,':: allocate old',status)

               !--- copy data back into new
               Snew(1:nwgts,1:cntold) = Sold(1:nwgts,1:cntold)
               Rnew(1:cntold) = Rold(1:cntold)
               Cnew(1:cntold) = Cold(1:cntold)
               deallocate(Sold,Rold,Cold,stat=status)
               if (status /= 0) call mct_perr_die(subName,':: deallocate old',status)
            endif

            Snew(1:nwgts,cnt) = Sbuf(1:nwgts,m)
            Rnew(cnt) = Rbuf(m)
            Cnew(cnt) = Cbuf(m)
         endif
      enddo  ! count
   enddo   ! nread

   !----------------------------------------------------------------------------
   !> * Clean up arrays
   !----------------------------------------------------------------------------
   if (mytask == 0) then
      deallocate(remaps, stat=status)
      if (status /= 0) call mct_perr_die(subName,':: deallocate remaps',status)
   endif
   deallocate(Sbuf,Rbuf,Cbuf, stat=status)
   if (status /= 0) call mct_perr_die(subName,':: deallocate Sbuf',status)

   !----------------------------------------------------------------------------
   !> * Initialize the mct sMat data type
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

   !----------------------------------------------------------------------------
   !> * More clean up
   !----------------------------------------------------------------------------
   deallocate(Snew,Rnew,Cnew, stat=status)
   deallocate(lsstart,lscount,stat=status)
   if (status /= 0) call mct_perr_die(subName,':: deallocate new',status)

   if (mytask == 0) then
      status = nf90_close(fid)
      IF (OASIS_debug >= 2) THEN
          WRITE(nulprt,*) subname," ... done reading file"
          CALL oasis_flush(nulprt)
      ENDIF
   endif

  call oasis_debug_exit(subname)

end subroutine oasis_map_sMatReaddnc_orig

!------------------------------------------------------------
! !BOP ===========================================================================
!
!> Function that checks whether an index is part of a start and count list
!
!> Does a binary search on a sorted start and count list to determine
!> whether index is a value in the list.  The list values consist of
!> the values start(i):start(i)+count(i)-1 for all i.
!
! !DESCRIPTION: 
!     Do a binary search to see if a value is contained in a list of
!     values.  return true or false.  starti must be monotonically
!     increasing, function does NOT check this.
!
! !INTERFACE:  -----------------------------------------------------------------

logical function check_myindex(index,starti,counti)

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p

! !INPUT/OUTPUT PARAMETERS:

   integer(IN) :: index       !< index to search
   integer(IN) :: starti(:)   !< start list
   integer(IN) :: counti(:)   !< count list

! !EOP

   !--- local ---
   integer(IN)    :: nl,nc,nr,ncprev 
   integer(IN)    :: lsize
   logical        :: stopnow

   !--- formats ---
   character(*),parameter :: subName = '(check_myindex) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   call oasis_debug_enter(subname)
   check_myindex = .false.

   lsize = size(starti)
   if (lsize < 1) then
!     call oasis_debug_exit(subname)
      return
   endif

   nl = 0
   nr = lsize + 1
   nc = (nl+nr)/2
   stopnow = .false.
   do while (.not.stopnow)
      if (index < starti(nc)) then
         nr = nc
      elseif (index > (starti(nc) + counti(nc) - 1)) then
         nl = nc
      else
         check_myindex = .true.
!        call oasis_debug_exit(subname)
         return
      endif
      ncprev = nc
      nc = (nl + nr)/2
      if (nc == ncprev .or. nc < 1 .or. nc > lsize) stopnow = .true.
   enddo

   check_myindex = .false.

!   call oasis_debug_exit(subname)

end function check_myindex

!===============================================================================
!BOP ===========================================================================
!> Read in mapping matrix data from a SCRIP netCDF file using smart scatter (ceg)
!
! !IROUTINE:  oasis_map_sMatReaddnc_ceg - Do a distributed read of a NetCDF SCRIP file and
!                                return weights in a distributed SparseMatrix
!
! !DESCRIPTION: 
!>     Read in mapping matrix data from a SCRIP netCDF data file using
!>     a low memory method and then scatter to all pes using a smart method
!>     where only select data is sent to tasks.  Based on the sMatReaddnc method 
!>    from CESM1.0.3
! !REMARKS:
!>   This routine leverages gsmaps to determine scatter pattern
!>
!>   The scatter is implemented via the root task reading the data and
!>     then determining which task gets which weights from the gsmap.
!>     The root the sends specific data to each task.
!>
!>   The algorithm to determine which task a weigth belongs to involves
!>     checking the task ownership for a given global index.
!>
!>   The local buffer sizes are estimated up front based on ngridcell/npes
!>     plus 20% (see 1.2 below).  If the local buffer size fills up, then
!>     the buffer is reallocated 50% larger (see 1.5 below) and the fill
!>     continues.  The idea is to trade off memory reallocation and copy
!>     with memory usage.  1.2 and 1.5 are arbitary, other values may
!>     result in better performance.
!>
!>   Once all the matrix weights have been read, the sMat is initialized,
!>     the values from the buffers are copied in, and everything is deallocated.
!
! !INTERFACE:  -----------------------------------------------------------------

subroutine oasis_map_sMatReaddnc_ceg(sMat,SgsMap,DgsMap,newdom, &
                            fileName,mytask,mpicom,nwgts, &
                            areasrc,areadst,ni_i,nj_i,ni_o,nj_o )

! !USES:

! !INPUT/OUTPUT PARAMETERS:

   type(mct_sMat)  ,intent(out),pointer   :: sMat(:) !< mapping data
   type(mct_gsMap) ,intent(in) ,target    :: SgsMap  !< src gsmap
   type(mct_gSMap) ,intent(in) ,target    :: DgsMap  !< dst gsmap
   character(*)    ,intent(in)            :: newdom  !< type of sMat (src or dst)
        !< src = rearrange and map (bfb), dst = map and rearrange (partial sums)
   character(*)    ,intent(in)            :: filename!< netCDF file to read
   integer(IN)     ,intent(in)            :: mytask  !< processor id
   integer(IN)     ,intent(in)            :: mpicom  !< mpi communicator
   integer(IN)     ,intent(out)           :: nwgts   !< number of weights 
   type(mct_Avect) ,intent(out), optional :: areasrc !< area of src grid from mapping file
   type(mct_Avect) ,intent(out), optional :: areadst !< area of dst grid from mapping file
   integer(IN)     ,intent(out), optional :: ni_i    !< number of lons on input grid   
   integer(IN)     ,intent(out), optional :: nj_i    !< number of lats on input grid   
   integer(IN)     ,intent(out), optional :: ni_o    !< number of lons on output grid   
   integer(IN)     ,intent(out), optional :: nj_o    !< number of lats on output grid   

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
   integer             :: l1,l2,lsize ! generice indices for sort
   integer,allocatable :: lsstart(:)  ! local seg map info
   integer,allocatable :: lscount(:)  ! local seg map info
   integer,allocatable :: lspeloc(:)  ! local seg map info
   logical             :: found       ! for sort
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
   character(*),parameter :: subName = '(oasis_map_sMatReaddnc_ceg)'
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
   !> * Open and read the file SCRIP weights size on the root task
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
 
   !----------------------------------------------------------------------------
   !> * Read and load area_a on root task
   !----------------------------------------------------------------------------
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

   !----------------------------------------------------------------------------
   !> * Read and load area_b on root task
   !----------------------------------------------------------------------------
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

   !----------------------------------------------------------------------------
   !> * Broadcast ni and nj if requested
   !> * Broadcast array sizes and allocate arrays for local storage
   ! Replace 8 MPI_Bcasts with just one
   !----------------------------------------------------------------------------
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

   !----------------------------------------------------------------------------
   !> * Compute the number of chunks to read, read size is rbuf_size
   !----------------------------------------------------------------------------
   rsize = min(rbuf_size,ns)                     ! size of i/o chunks
   bsize = ((ns/commsize) + 1 ) * 1.2   ! local temporary buffer size
   if (ns == 0) then
      nread = 0
   else
      nread = (ns-1)/rsize + 1                      ! num of reads to do
   endif

   !----------------------------------------------------------------------------
   !> * Allocate arrays for local weights plus row and column indices
   !----------------------------------------------------------------------------
   allocate(Smat(nwgts),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Smat',status)
   allocate(Sbuf(nwgts,rsize),Rbuf(rsize),Cbuf(rsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Sbuf',status)
   allocate(Snew(nwgts,bsize),Cnew(bsize),Rnew(bsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Snew1',status)
!   write(nulprt,*) subname,mpi_rank_local,rsize,rsize*nwgts

   cnt = 1

   !----------------------------------------------------------------------------
   !> * On the root task
   !----------------------------------------------------------------------------
   if (mytask == 0) then

      !----------------------------------------------------------------------------
      !>   * Initialize lsstart and lscount, the sorted list of local indices
      !----------------------------------------------------------------------------
      lsize = size(mygsmap%start)
      allocate(lsstart(lsize),lscount(lsize),lspeloc(lsize),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate Lsstart',status)

      do n = 1,size(mygsmap%start)
         found = .false.
         l1 = 1
         do while (.not.found .and. l1 < n)         ! bubble sort copy
            if (mygsmap%start(n) < lsstart(l1)) then
               do l2 = n, l1+1, -1
                  lsstart(l2) = lsstart(l2-1)
                  lscount(l2) = lscount(l2-1)
                  lspeloc(l2) = lspeloc(l2-1)
               enddo
               found = .true.
            else
               l1 = l1 + 1
            endif
         enddo
         lsstart(l1) = mygsmap%start(n)
         lscount(l1) = mygsmap%length(n)
         lspeloc(l1) = mygsmap%pe_loc(n)
      enddo
      do n = 1,size(mygsmap%start)-1
         if (lsstart(n) > lsstart(n+1)) then
            write(nulprt,*) subname,estr,'lsstart not properly sorted'
            call oasis_abort()
         endif
      enddo

      !----------------------------------------------------------------------------
      !>   * Allocate arrays for reading data on the root
      !----------------------------------------------------------------------------
      allocate(remaps(nwgts,rsize),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate remaps',status)
      allocate(SReadData(nwgts,rsize),RReadData(rsize),CReadData(rsize),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate SReadData',status)
      allocate(pesave(rsize),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate pesave',status)
      allocate(SDistData(nwgts,rsize),RDistData(rsize),CDistData(rsize), stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate SDistData',status)
      allocate(cntrs(0:mpi_size_local), stat=status) ! Purposefully allocating one extra
      if (status /= 0) call mct_perr_die(subName,':: allocate cntrs',status)

      !----------------------------------------------------------------------------
      !>   * Loop over the chunks of weights data
      !----------------------------------------------------------------------------
      do n = 1,nread
         start(1) = (n-1)*rsize + 1
         count(1) = min(rsize,ns-start(1)+1)
         start2(1) = 1
         count2(1) = nwgts
         start2(2) = start(1)
         count2(2) = count(1)

         !----------------------------------------------------------------------------
         !>   * Read chunk of data
         !----------------------------------------------------------------------------
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

         ! Two stage process
         !   1. Count how many to send to each process
         !   2. Build the sending array up in process receiving order
        
         ! Initialize cntrs array for number of accumulations per process
         cntrs  =0
         pesave = -99

         !----------------------------------------------------------------------------
         !>   * Determine which process owns each weight and count them
         !>   * Determine offsets in the array
         !----------------------------------------------------------------------------
         do m = 1,count(1)
            !--- which process owns this point?
            if (newdom == 'src') then
               pe = get_cegindex(RReadData(m),lsstart,lscount,lspeloc)
            else if (newdom == 'dst') then
               pe = get_cegindex(CReadData(m),lsstart,lscount,lspeloc)
            endif
            pesave(m) = pe
            
            ! Copy data, incrementing index array, pe = 1 to mpi_size_local
            if (pe+1 < 1 .or. pe+1  > mpi_size_local) then
               write(nulprt,*) subname,wstr,'get_cegindex search error', m,count(1),CReadData(m),RReadData(m),SReadData(:,m)
            else
               cntrs(pe+1) = cntrs(pe+1) + 1  ! Note incrementing 1->noprocs rather than 0->noprocs-1
            endif

         end do
         
         cntrs(0) = 1
         do pe = 1, mpi_size_local
            cntrs(pe) = cntrs(pe-1) + cntrs(pe)
         end do

         !----------------------------------------------------------------------------
         !>   * Determine which process owns each weight and fill arrays
         !----------------------------------------------------------------------------
         do m = 1,count(1)
            !--- which process owns this point?
            if (newdom == 'src') then
               pe = get_cegindex(RReadData(m),lsstart,lscount,lspeloc)
            else if (newdom == 'dst') then
               pe = get_cegindex(CReadData(m),lsstart,lscount,lspeloc)
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

         !----------------------------------------------------------------------------
         !>   * Send select data from root to other processes
         ! Copy root process data into local array
         ! Send data from root to other processes
         !----------------------------------------------------------------------------
         if (cntrs(0).gt.1) then ! Need to do it differently if it is for me!
            reclen = cntrs(0)-1
            ! Reallocate local weights arrays if they need to be bigger
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

      !----------------------------------------------------------------------------
      !>   * Deallocate memory on root process
      !----------------------------------------------------------------------------
      m = -1
      do pe = 1, mpi_size_local-1
         !write(nulprt,*) subname, 'Final sending [to, datalen] ', m, cntrs(m)
         call MPI_Send(m, 1, MPI_INTEGER, pe, 4000, mpicom, ierr)
      end do

      ! Free memory used during the distribution
      deallocate(lsstart,lscount,lspeloc, stat=status)
      if (status /= 0) call mct_perr_die(subName,':: deallocate lsstart',status)
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

   !----------------------------------------------------------------------------
   !> * On non-root processes
   !----------------------------------------------------------------------------

   else   ! mytask == 0
   
      !----------------------------------------------------------------------------
      !>   * Receive data from root
      !----------------------------------------------------------------------------
      call oasis_mpi_recv(reclen, 0, 4000, mpicom, subName//" MPI in reclen recv")
      ! Check we haven't now had the last data
      do while (reclen.ne.-1) 

         !--- recv S, row, col on all other pes
         !write(nulprt,*) subname, 'Receiving [global-id, datalen] ', mpi_rank_global, reclen
         call MPI_Recv(Sbuf, reclen*nwgts, MPI_REAL8, 0, 1000, mpicom, MPI_STATUS_IGNORE, ierr)
         call MPI_Recv(Rbuf, reclen, MPI_INTEGER, 0, 2000, mpicom, MPI_STATUS_IGNORE, ierr)
         call MPI_Recv(Cbuf, reclen, MPI_INTEGER, 0, MPI_ANY_TAG, mpicom, mpistatus, ierr)

         !----------------------------------------------------------------------------
         !>   * Reallocate local weights arrays if they need to be bigger
         !----------------------------------------------------------------------------
         call augment_arrays(cnt, reclen, bsize, nwgts)

         !--- now each pe keeps what it has been sent
         Snew(1:nwgts,cnt:cnt+reclen-1) = Sbuf(1:nwgts,1:reclen)
         Rnew(cnt:cnt+reclen-1) = Rbuf(1:reclen)
         Cnew(cnt:cnt+reclen-1) = Cbuf(1:reclen)
         cnt = cnt + reclen

         call oasis_mpi_recv(reclen, 0, 4000, mpicom, subName//" MPI in reclen recv")
      end do

   endif   ! mytask == 0

   ! Fix cnt to be the length of the array
   cnt = cnt-1

   !----------------------------------------------------------------------------
   !> * Clean up arrays
   !----------------------------------------------------------------------------
   deallocate(Sbuf,Rbuf,Cbuf, stat=status)
   if (status /= 0) call mct_perr_die(subName,':: deallocate Sbuf',status)

   !----------------------------------------------------------------------------
   !> * Initialize the mct sMat data type
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

   !----------------------------------------------------------------------------
   !> * More clean up
   !----------------------------------------------------------------------------
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

end subroutine oasis_map_sMatReaddnc_ceg

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

integer function get_cegindex(index,starti,counti,peloci)

! !USES:


   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p

! !INPUT/OUTPUT PARAMETERS:

   integer(IN) :: index       !< index to search
   integer(IN) :: starti(:)   !< start list
   integer(IN) :: counti(:)   !< count list
   integer(IN) :: peloci(:)   !< pe list

! !EOP


   !--- local ---
   integer(IN)    :: nl,nc,nr,ncprev
   integer(IN)    :: lsize
   logical        :: stopnow

   !--- formats ---
   character(*),parameter :: subName = '(get_cegindex) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!   call oasis_debug_enter(subname)

   get_cegindex = -99
   lsize = size(starti)
   if (lsize < 1) then
!     call oasis_debug_exit(subname)
      return
   endif

   nl = 0
   nr = lsize + 1
   nc = (nl+nr)/2
   stopnow = .false.
   do while (.not.stopnow)
      if (index < starti(nc)) then
         nr = nc
      elseif (index > (starti(nc) + counti(nc) - 1)) then
         nl = nc
      else
         get_cegindex = peloci(nc)
!        call oasis_debug_exit(subname)
         return
      endif
      ncprev = nc
      nc = (nl + nr)/2
      if (nc == ncprev .or. nc < 1 .or. nc > lsize) stopnow = .true.
   enddo


!   call oasis_debug_exit(subname)


end function get_cegindex

!===============================================================================
END MODULE mod_oasis_map


