module mod_prism_grid
!-----------------------------------------------------------------------
! BOP
!
! !MODULE:  mod_prism_grid
! !REMARKS: 
!
! **********************
! THIS SHOULD BE CALLED BY A SINGLE PE ACCORDING TO THE OASIS3 
! STANDARD.  THE DATA IS GLOBAL.
! **********************
!
! !REVISION HISTORY:
! 
!
! !PUBLIC MEMBER FUNCTIONS:
! 
!      subroutine prism_grid_start_grids_writing(iwrite)
!             This subroutine initializes grid writing by receiving a 
!             starting command from OASIS.
!
!      subroutine prism_grid_write_grid(cgrid, nx, ny, lon, lat)
!	      This subroutine writes longitudes and latitudes for a model
!             grid.
!
!      subroutine prism_grid_write_corner(cgrid, nx, ny, nc, clon, clat)
!	      This subroutine writes the longitudes and latitudes of the
!             grid cell corners.
!
!      subroutine prism_grid_write_mask(cgrid, nx, ny, mask)
!	      This subroutine writes the mask for a model grid
!
!      subroutine prism_grid_write_area(cgrid, nx, ny, area)
!	      This subroutine writes the grid cell areas for a model grid.
!
!      subroutine prism_grid_terminate_grids_writing()
!             This subroutine terminates grid writing by sending a flag
!             to OASIS, stating the all needed grid information was written.
!       

! !USES:
  use mod_prism_data
  use mod_oasis_print
  use mod_prism_io
  use mod_prism_sys
  
  implicit none

  private

  public prism_grid_start_grids_writing
  public prism_grid_write_grid
  public prism_grid_write_corner
  public prism_grid_write_mask
  public prism_grid_write_area
  public prism_grid_terminate_grids_writing
  public prism_grid_write2files

  !--- datatypes ---
  public :: prism_grid_type

  integer(kind=ip_intwp_p),parameter :: mgrid = 100

  type prism_grid_type
     character(len=ic_med)  :: gridname
     integer(kind=ip_i4_p)  :: nx
     integer(kind=ip_i4_p)  :: ny
     integer(kind=ip_i4_p)  :: nc
     logical                :: grid_set
     logical                :: corner_set
     logical                :: angle_set
     logical                :: area_set
     logical                :: mask_set
     logical                :: written
     logical                :: terminated
     real(kind=ip_realwp_p),allocatable :: lon(:,:)     ! longitudes
     real(kind=ip_realwp_p),allocatable :: lat(:,:)     ! latitudes
     real(kind=ip_realwp_p),allocatable :: clon(:,:,:)  ! corner longitudes
     real(kind=ip_realwp_p),allocatable :: clat(:,:,:)  ! corner latitudes
     real(kind=ip_realwp_p),allocatable :: angle(:,:)   ! angle
     real(kind=ip_realwp_p),allocatable :: area(:,:)    ! area
     integer(kind=ip_i4_p) ,allocatable :: mask(:,:)    ! mask
  end type prism_grid_type

  integer(kind=ip_intwp_p),public,save :: prism_ngrid = 0
  type(prism_grid_type),public,save :: prism_grid(mgrid)


#ifdef use_netCDF
#include <netcdf.inc>
#endif

!---------------------------------------------------------------------------

CONTAINS

!--------------------------------------------------------------------------
    subroutine prism_grid_start_grids_writing(iwrite)

    !-------------------------------------------------
    ! Routine to start the grids writing. To syncronize access to the
    ! grids file all component models have to wait for the starting 
    ! message from OASIS (via MPI; see prism_init_comp_proto)
    !-------------------------------------------------

    implicit none
  
    integer(kind=ip_intwp_p), intent (OUT) :: iwrite ! flag to state whether
                                            ! grids file needs to be written
    !-------------------------------------------------
    character(len=*),parameter :: subname = 'prism_grid_start_grids_writing'
    !-------------------------------------------------

    call prism_sys_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
       CALL oasis_pprintc(subname,2,' error :',char1='subroutine call by non root processor')
       CALL prism_sys_abort()
    endif

    if (prism_ngrid == 0) then  ! first call
       prism_grid(:)%grid_set   = .false.
       prism_grid(:)%corner_set = .false.
       prism_grid(:)%angle_set  = .false.
       prism_grid(:)%area_set   = .false.
       prism_grid(:)%mask_set   = .false.
       prism_grid(:)%written    = .false.
    endif
    iwrite = 1   ! just set grids are needed always

    call prism_sys_debug_exit(subname)

    end subroutine prism_grid_start_grids_writing

!--------------------------------------------------------------------------

    subroutine prism_grid_write_grid(cgrid, nx, ny, lon, lat)

    !-------------------------------------------------
    ! Routine to create a new grids file or to add a grid description to an
    ! existing grids file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid      ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx         ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny         ! number of latitudes
    real(kind=ip_realwp_p),   intent (in) :: lon(nx,ny) ! longitudes
    real(kind=ip_realwp_p),   intent (in) :: lat(nx,ny) ! latitudes
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'prism_grid_write_grid'
    !-------------------------------------------------

    call prism_sys_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
       CALL oasis_pprintc(subname,2,' error :',char1='subroutine call by non root processor')
       call prism_sys_abort()
    endif

    call prism_grid_findgrid(cgrid,nx,ny,gridID)

    allocate(prism_grid(gridID)%lon(nx,ny),stat=ierror)
    if (ierror /= 0) CALL oasis_pprintc(subname,2,' : ',char1=' WARNING lon alloc')
    allocate(prism_grid(gridID)%lat(nx,ny),stat=ierror)
    if (ierror /= 0) CALL oasis_pprintc(subname,2,' : ',char1=' WARNING lat alloc')
    prism_grid(gridID)%lon = lon
    prism_grid(gridID)%lat = lat
    prism_grid(gridID)%grid_set = .true.

    call prism_sys_debug_exit(subname)

    end subroutine prism_grid_write_grid

!--------------------------------------------------------------------------
    subroutine prism_grid_write_angle(cgrid, nx, ny, angle)

    !-------------------------------------------------
    ! Routine to add angles to an existing grid file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    real(kind=ip_realwp_p),   intent (in) :: angle(nx,ny) ! angles
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'prism_grid_write_angle'
    !-------------------------------------------------

    call prism_sys_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
       CALL oasis_pprintc(subname,2,' error :',char1='subroutine call by non root processor')
       call prism_sys_abort()
    endif

    call prism_grid_findgrid(cgrid,nx,ny,gridID)

    allocate(prism_grid(gridID)%angle(nx,ny),stat=ierror)
    if (ierror /= 0) CALL oasis_pprintc(subname,2,' : ',char1=' WARNING angle alloc')
    prism_grid(gridID)%angle = angle
    prism_grid(gridID)%angle_set = .true.

    call prism_sys_debug_exit(subname)

    end subroutine prism_grid_write_angle

!--------------------------------------------------------------------------
    subroutine prism_grid_write_corner(cgrid, nx, ny, nc, clon, clat)

    !-------------------------------------------------
    ! Routine to add longitudes and latitudes of grid cell corners to an
    ! existing grids file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid  ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx     ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny     ! number of latitudes
    integer(kind=ip_intwp_p), intent (in) :: nc     ! number of corners per cell
    real(kind=ip_realwp_p),   intent (in) :: clon(nx,ny,nc) ! longitudes
    real(kind=ip_realwp_p),   intent (in) :: clat(nx,ny,nc) ! latitudes
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'prism_grid_write_corner'
    !-------------------------------------------------

    call prism_sys_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
        CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
        CALL oasis_pprintc(subname,2,' error :',char1='subroutine call by non root processor')
        CALL prism_sys_abort()
    endif

    call prism_grid_findgrid(cgrid,nx,ny,gridID)

    allocate(prism_grid(gridID)%clon(nx,ny,nc),stat=ierror)
    if (ierror /= 0) CALL oasis_pprintc(subname,2,' : ',char1=' WARNING clon alloc')
    allocate(prism_grid(gridID)%clat(nx,ny,nc),stat=ierror)
    if (ierror /= 0) CALL oasis_pprintc(subname,2,' : ',char1=' WARNING clat alloc')
    prism_grid(gridID)%nc = nc
    prism_grid(gridID)%clon = clon
    prism_grid(gridID)%clat = clat
    prism_grid(gridID)%corner_set = .true.

    call prism_sys_debug_exit(subname)

    end subroutine prism_grid_write_corner

!--------------------------------------------------------------------------
    subroutine prism_grid_write_mask(cgrid, nx, ny, mask)

    !-------------------------------------------------
    ! Routine to create a new masks file or to add a land see mask to an
    ! existing masks file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    integer(kind=ip_intwp_p), intent (in) :: mask(nx,ny) ! mask
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'prism_grid_write_mask'
    !-------------------------------------------------

    call prism_sys_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
       CALL oasis_pprintc(subname,2,' error :',char1='subroutine call by non root processor')
       call prism_sys_abort()
    endif

    call prism_grid_findgrid(cgrid,nx,ny,gridID)

    allocate(prism_grid(gridID)%mask(nx,ny),stat=ierror)
    if (ierror /= 0) CALL oasis_pprintc(subname,2,' : ',char1=' WARNING mask alloc')
    prism_grid(gridID)%mask = mask
    prism_grid(gridID)%mask_set = .true.

    call prism_sys_debug_exit(subname)

    end subroutine prism_grid_write_mask

!--------------------------------------------------------------------------
    subroutine prism_grid_write_area(cgrid, nx, ny, area)

    !-------------------------------------------------
    ! Routine to create a new areas file or to add areas of a grid to an
    ! existing areas file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    real(kind=ip_realwp_p),   intent (in) :: area(nx,ny) ! areas
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'prism_grid_write_area'
    !-------------------------------------------------

    call prism_sys_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
       CALL oasis_pprintc(subname,2,' error :',char1='subroutine call by non root processor')
       call prism_sys_abort()
    endif

    call prism_grid_findgrid(cgrid,nx,ny,gridID)

    allocate(prism_grid(gridID)%area(nx,ny),stat=ierror)
    if (ierror /= 0) CALL oasis_pprintc(subname,2,' : ',char1=' WARNING area alloc')
    prism_grid(gridID)%area = area
    prism_grid(gridID)%area_set = .true.

    call prism_sys_debug_exit(subname)

    end subroutine prism_grid_write_area

!--------------------------------------------------------------------------
    subroutine prism_grid_terminate_grids_writing()
    !-------------------------------------------------
    ! Routine to terminate the grids writing.
    !-------------------------------------------------

    implicit none
    integer(kind=ip_i4_p) :: n
    character(len=*),parameter :: subname = 'prism_grid_terminate_grids_writing'

    call prism_sys_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
       CALL oasis_pprintc(subname,2,' error :',char1='subroutine call by non root processor')
       call prism_sys_abort()
    endif

    do n = 1,prism_ngrid
       prism_grid(n)%terminated = .true.
    enddo

! moved to prism_method_enddef for synchronization
!    call prism_grid_write2files()

    call prism_sys_debug_exit(subname)

    end subroutine prism_grid_terminate_grids_writing

!--------------------------------------------------------------------------
    subroutine prism_grid_write2files()

    !-------------------------------------------------
    ! Write fields to grid files.
    ! Only write fields that have been buffered and
    ! if prism_grid_terminate_grids_writing has been called
    !-------------------------------------------------

    implicit none

    !-------------------------------------------------
    character(len=ic_med) :: filename  ! grid filename
    character(len=ic_med) :: fldname   ! full field name
    character(len=ic_med) :: cgrid     ! grid name
    logical :: exists                  ! check if file exists
    integer(kind=ip_i4_p) :: n         ! counter
    integer(kind=ip_i4_p) :: nx,ny,nc  ! grid size
    character(len=*),parameter :: subname = 'prism_grid_write2files'
    !-------------------------------------------------

    call prism_sys_debug_enter(subname)

    do n = 1,prism_ngrid
    if (prism_grid(n)%terminated) then
       cgrid = trim(prism_grid(n)%gridname)
       prism_grid(n)%written = .true.

       nx = prism_grid(n)%nx
       ny = prism_grid(n)%ny
       nc = prism_grid(n)%nc

       if (prism_grid(n)%grid_set) then
          filename = 'grids.nc'
          fldname  = trim(cgrid)//'.lon'
          call prism_io_write_2dgridfld_fromroot(filename,fldname,prism_grid(n)%lon,nx,ny)
          fldname  = trim(cgrid)//'.lat'
          call prism_io_write_2dgridfld_fromroot(filename,fldname,prism_grid(n)%lat,nx,ny)
       endif

       if (prism_grid(n)%corner_set) then
          filename = 'grids.nc'
          fldname  = trim(cgrid)//'.clo'
          call prism_io_write_3dgridfld_fromroot(filename,fldname,prism_grid(n)%clon,nx,ny,nc)
          fldname  = trim(cgrid)//'.cla'
          call prism_io_write_3dgridfld_fromroot(filename,fldname,prism_grid(n)%clat,nx,ny,nc)
       endif

       if (prism_grid(n)%area_set) then
          filename = 'areas.nc'
          fldname  = trim(cgrid)//'.srf'
          call prism_io_write_2dgridfld_fromroot(filename,fldname,prism_grid(n)%area,nx,ny)
       endif

       if (prism_grid(n)%angle_set) then
          filename = 'grids.nc'
          fldname  = trim(cgrid)//'.ang'
          call prism_io_write_2dgridfld_fromroot(filename,fldname,prism_grid(n)%angle,nx,ny)
       endif

       if (prism_grid(n)%mask_set) then
          filename = 'masks.nc'
          fldname  = trim(cgrid)//'.msk'
          call prism_io_write_2dgridint_fromroot(filename,fldname,prism_grid(n)%mask,nx,ny)
       endif

    endif  ! terminated
    enddo

    call prism_sys_debug_exit(subname)

    end subroutine prism_grid_write2files
!--------------------------------------------------------------------------

    subroutine prism_grid_findgrid(cgrid,nx,ny,gridID)
    !-------------------------------------------------
    ! Routine that sets gridID, identifies existing
    ! grid with cgrid name or starts a new one
    !-------------------------------------------------
    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    integer(kind=ip_intwp_p), intent(out) :: gridID      ! gridID matching cgrid
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: n
    character(len=*),parameter :: subname = 'prism_grid_findgrid'
    !-------------------------------------------------

    call prism_sys_debug_enter(subname)

    gridID = -1
    do n = 1,prism_ngrid
       if (trim(cgrid) == trim(prism_grid(n)%gridname)) then
          gridID = n
          ! since grid is defined before, make sure nx,ny match
          if (nx /= prism_grid(gridID)%nx .or. ny /= prism_grid(gridID)%ny) then
             CALL oasis_pprinti(subname,2,' ERROR in predefined grid size nx,ny : ',int1=nx,int2=ny)
             CALL oasis_pprinti(subname,2,' ERROR in predefined grid size : ',int1=prism_grid(gridID)%nx,&
                                           int2=prism_grid(gridID)%ny)
             CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
             CALL oasis_pprintc(subname,2,' error :',char1='prism grid sizes')
             call prism_sys_abort()
          endif
       endif
    enddo

    if (gridID < 1) then
       prism_ngrid = prism_ngrid+1
       gridID = prism_ngrid
    endif

    prism_grid(gridID)%gridname = trim(cgrid)
    prism_grid(gridID)%nx = nx
    prism_grid(gridID)%ny = ny

    call prism_sys_debug_exit(subname)

    end subroutine prism_grid_findgrid
!--------------------------------------------------------------------------
end module mod_prism_grid
!--------------------------------------------------------------------------


     
