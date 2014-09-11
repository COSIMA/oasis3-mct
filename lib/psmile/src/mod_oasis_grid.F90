MODULE mod_oasis_grid
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
!      subroutine oasis_start_grids_writing(iwrite)
!             This subroutine initializes grid writing by receiving a 
!             starting command from OASIS.
!
!      subroutine oasis_write_grid(cgrid, nx, ny, lon, lat, part_id)
!	      This subroutine writes longitudes and latitudes for a model
!             grid.  part_id is optional and indicates decomposed data is provided
!
!      subroutine oasis_write_corner(cgrid, nx, ny, nc, clon, clat, part_id)
!	      This subroutine writes the longitudes and latitudes of the
!             grid cell corners.  part_id is optional and indicates decomposed data 
!             is provided
!
!      subroutine oasis_write_mask(cgrid, nx, ny, mask, part_id)
!	      This subroutine writes the mask for a model grid.  part_id is optional 
!             and indicates decomposed data is provided
!
!      subroutine oasis_write_area(cgrid, nx, ny, area, part_id)
!	      This subroutine writes the grid cell areas for a model grid.  part_id 
!             is optional and indicates decomposed data is provided
!
!      subroutine oasis_terminate_grids_writing()
!             This subroutine terminates grid writing by sending a flag
!             to OASIS, stating the all needed grid information was written.
!       

! !USES:
  USE mod_oasis_data
  USE mod_oasis_io
  USE mod_oasis_sys
  USE mod_oasis_part
  USE mod_oasis_mpi, only: oasis_mpi_min, oasis_mpi_max, oasis_mpi_bcast, oasis_mpi_barrier
  USE mod_oasis_timer
  USE mct_mod
  
  implicit none

  private

  public oasis_start_grids_writing
  public oasis_write_grid
  public oasis_write_angle
  public oasis_write_corner
  public oasis_write_mask
  public oasis_write_area   
  public oasis_terminate_grids_writing 
  public oasis_write2files
  public oasis_print_grid_data

  interface oasis_write_grid
#ifndef __NO_4BYTE_REALS
     module procedure oasis_write_grid_r4
#endif
     module procedure oasis_write_grid_r8
  end interface

  interface oasis_write_angle
#ifndef __NO_4BYTE_REALS
     module procedure oasis_write_angle_r4
#endif
     module procedure oasis_write_angle_r8
  end interface

  interface oasis_write_corner
#ifndef __NO_4BYTE_REALS
     module procedure oasis_write_corner_r4
#endif
     module procedure oasis_write_corner_r8
  end interface

  interface oasis_write_area
#ifndef __NO_4BYTE_REALS
     module procedure oasis_write_area_r4
#endif
     module procedure oasis_write_area_r8
  end interface

  !--- datatypes ---
  public :: prism_grid_type

  integer(kind=ip_intwp_p),parameter :: mgrid = 100

  type prism_grid_type
     character(len=ic_med)  :: gridname
     integer(kind=ip_i4_p)  :: partid
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
    SUBROUTINE oasis_print_grid_data()

    !-------------------------------------------------
    ! Routine to start the grids writing. To syncronize access to the
    ! grids file all component models have to wait for the starting 
    ! message from OASIS (via MPI; see prism_init_comp_proto)
    !-------------------------------------------------

    implicit none
  
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: n
    character(len=*),parameter :: subname = '(oasis_print_grid_data)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    do n = 1,prism_ngrid
       write(nulprt,*) ' '
       write(nulprt,*) subname,trim(prism_grid(n)%gridname),' size',prism_grid(n)%nx,prism_grid(n)%ny
       write(nulprt,*) subname,trim(prism_grid(n)%gridname),' set ',prism_grid(n)%grid_set, &
                       prism_grid(n)%corner_set, prism_grid(n)%angle_set, prism_grid(n)%area_set, prism_grid(n)%mask_set
       if (prism_grid(n)%partid > 0 .and. prism_grid(n)%partid < prism_npart) then
          write(nulprt,*) subname,'partid ',trim(prism_grid(n)%gridname),prism_grid(n)%partid, &
                       trim(prism_part(prism_grid(n)%partid)%partname)
       else
          write(nulprt,*) subname,'partid ',trim(prism_grid(n)%gridname),prism_grid(n)%partid
       endif
       if (prism_grid(n)%grid_set) write(nulprt,*) subname,trim(prism_grid(n)%gridname),' lon  ', &
                                   size(prism_grid(n)%lon,dim=1),size(prism_grid(n)%lon,dim=2), &
                                   minval(prism_grid(n)%lon),maxval(prism_grid(n)%lon)
       if (prism_grid(n)%grid_set) write(nulprt,*) subname,trim(prism_grid(n)%gridname),' lat  ', &
                                   size(prism_grid(n)%lat,dim=1),size(prism_grid(n)%lat,dim=2), &
                                   minval(prism_grid(n)%lat),maxval(prism_grid(n)%lat)
       if (prism_grid(n)%corner_set) write(nulprt,*) subname,trim(prism_grid(n)%gridname),' clon ', &
                                   size(prism_grid(n)%clon,dim=1),size(prism_grid(n)%clon,dim=2), &
                                   minval(prism_grid(n)%clon),maxval(prism_grid(n)%clon)
       if (prism_grid(n)%corner_set) write(nulprt,*) subname,trim(prism_grid(n)%gridname),' clat ', &
                                   size(prism_grid(n)%clat,dim=1),size(prism_grid(n)%clat,dim=2), &
                                   minval(prism_grid(n)%clat),maxval(prism_grid(n)%clat)
       if (prism_grid(n)%angle_set) write(nulprt,*) subname,trim(prism_grid(n)%gridname),' angl ', &
                                   size(prism_grid(n)%angle,dim=1),size(prism_grid(n)%angle,dim=2), &
                                   minval(prism_grid(n)%angle),maxval(prism_grid(n)%angle)
       if (prism_grid(n)%mask_set) write(nulprt,*) subname,trim(prism_grid(n)%gridname),' mask ', &
                                   size(prism_grid(n)%mask,dim=1),size(prism_grid(n)%mask,dim=2), &
                                   minval(prism_grid(n)%mask),maxval(prism_grid(n)%mask)
       if (prism_grid(n)%area_set) write(nulprt,*) subname,trim(prism_grid(n)%gridname),' area ', &
                                   size(prism_grid(n)%area,dim=1),size(prism_grid(n)%area,dim=2), &
                                   minval(prism_grid(n)%area),maxval(prism_grid(n)%area)
    enddo

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_print_grid_data

!--------------------------------------------------------------------------
    SUBROUTINE oasis_start_grids_writing(iwrite)

    !-------------------------------------------------
    ! Routine to start the grids writing. To syncronize access to the
    ! grids file all component models have to wait for the starting 
    ! message from OASIS (via MPI; see prism_init_comp_proto)
    !-------------------------------------------------

    implicit none
  
    integer(kind=ip_intwp_p), intent (OUT) :: iwrite ! flag to state whether
                                            ! grids file needs to be written
    !-------------------------------------------------
    character(len=*),parameter :: subname = '(oasis_start_grids_writing)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' prism_ngrid = ',prism_ngrid
    endif

    if (prism_ngrid == 0) then  ! first call
       prism_grid(:)%gridname   = 'unSet'
       prism_grid(:)%nx         = -1
       prism_grid(:)%ny         = -1
       prism_grid(:)%grid_set   = .false.
       prism_grid(:)%corner_set = .false.
       prism_grid(:)%angle_set  = .false.
       prism_grid(:)%area_set   = .false.
       prism_grid(:)%mask_set   = .false.
       prism_grid(:)%written    = .false.
       prism_grid(:)%partid     = -1
    endif
    iwrite = 1   ! just set grids are needed always

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_start_grids_writing

!--------------------------------------------------------------------------

    SUBROUTINE oasis_write_grid_r8(cgrid, nx, ny, lon, lat, partid)

    !-------------------------------------------------
    ! Routine to create a new grids file or to add a grid description to an
    ! existing grids file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid      ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx         ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny         ! number of latitudes
    real(kind=ip_double_p),   intent (in) :: lon(:,:)   ! longitudes
    real(kind=ip_double_p),   intent (in) :: lat(:,:)   ! latitudes
    integer(kind=ip_intwp_p), intent (in),optional :: partid  ! partid if nonglobal
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    integer(kind=ip_intwp_p) :: lnx,lny
    character(len=*),parameter :: subname = '(oasis_write_grid_r8)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' size = ',trim(cgrid),nx,ny
    endif

    call oasis_findgrid(cgrid,nx,ny,gridID)

    lnx = size(lon,dim=1)
    lny = size(lon,dim=2)

    allocate(prism_grid(gridID)%lon(lnx,lny),stat=ierror)
    IF (ierror /= 0) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING lon alloc'

    lnx = size(lat,dim=1)
    lny = size(lat,dim=2)

    allocate(prism_grid(gridID)%lat(lnx,lny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING lat alloc'

    prism_grid(gridID)%lon = lon
    prism_grid(gridID)%lat = lat
    prism_grid(gridID)%grid_set = .true.

    if (present(partid)) then
       if (prism_grid(gridID)%partid > 0 .and. prism_grid(gridID)%partid /= partid) then
          write(nulprt,*) subname,estr,'partid inconsistency',gridID,prism_grid(gridID)%partid,partid
          call oasis_abort()
       endif
       prism_grid(gridID)%partid = partid
       if (OASIS_debug >= 15) then
          write(nulprt,*) subname,' partid = ',trim(cgrid),partid
       endif
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_grid_r8

!--------------------------------------------------------------------------

    SUBROUTINE oasis_write_grid_r4(cgrid, nx, ny, lon, lat, partid)

    !-------------------------------------------------
    ! Routine to create a new grids file or to add a grid description to an
    ! existing grids file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid      ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx         ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny         ! number of latitudes
    real(kind=ip_single_p),   intent (in) :: lon(:,:)   ! longitudes
    real(kind=ip_single_p),   intent (in) :: lat(:,:)   ! latitudes
    integer(kind=ip_intwp_p), intent (in),optional :: partid  ! partid if nonglobal
    !-------------------------------------------------
    real(kind=ip_double_p), allocatable :: lon8(:,:)
    real(kind=ip_double_p), allocatable :: lat8(:,:)
    integer(kind=ip_intwp_p) :: ierror
    integer(kind=ip_intwp_p) :: lpartid
    integer(kind=ip_intwp_p) :: lnx,lny
    character(len=*),parameter :: subname = '(oasis_write_grid_r4)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' size = ',trim(cgrid),nx,ny
    endif

    lpartid = -1
    if (present(partid)) then
       lpartid = partid
    endif
    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' partid = ',trim(cgrid),lpartid
    endif

    lnx = size(lon,dim=1)
    lny = size(lon,dim=2)

    allocate(lon8(lnx,lny),stat=ierror)
    IF (ierror /= 0) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING lon alloc'

    lnx = size(lat,dim=1)
    lny = size(lat,dim=2)

    allocate(lat8(lnx,lny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING lat alloc'

    lon8 = lon
    lat8 = lat
    call oasis_write_grid_r8(cgrid,nx,ny,lon8,lat8,partid=lpartid)
    deallocate(lon8)
    deallocate(lat8)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_grid_r4

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_angle_r8(cgrid, nx, ny, angle, partid)

    !-------------------------------------------------
    ! Routine to add angles to an existing grid file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    real(kind=ip_double_p),   intent (in) :: angle(:,:)  ! angles
    integer(kind=ip_intwp_p), intent (in),optional :: partid  ! partid if nonglobal
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    integer(kind=ip_intwp_p) :: lnx,lny
    character(len=*),parameter :: subname = '(oasis_write_angle_r8)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' size = ',trim(cgrid),nx,ny
    endif

    call oasis_findgrid(cgrid,nx,ny,gridID)

    lnx = size(angle,dim=1)
    lny = size(angle,dim=2)

    allocate(prism_grid(gridID)%angle(lnx,lny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING angle alloc'

    prism_grid(gridID)%angle = angle
    prism_grid(gridID)%angle_set = .true.
    if (present(partid)) then
       if (prism_grid(gridID)%partid > 0 .and. prism_grid(gridID)%partid /= partid) then
          write(nulprt,*) subname,estr,'partid inconsistency',gridID,prism_grid(gridID)%partid,partid
          call oasis_abort()
       endif
       prism_grid(gridID)%partid = partid
       if (OASIS_debug >= 15) then
          write(nulprt,*) subname,' partid = ',trim(cgrid),partid
       endif
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_angle_r8

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_angle_r4(cgrid, nx, ny, angle, partid)

    !-------------------------------------------------
    ! Routine to add angles to an existing grid file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    real(kind=ip_single_p),   intent (in) :: angle(:,:)  ! angles
    integer(kind=ip_intwp_p), intent (in),optional :: partid  ! partid if nonglobal
    !-------------------------------------------------
    real(kind=ip_double_p),allocatable :: angle8(:,:)
    integer(kind=ip_intwp_p) :: ierror
    integer(kind=ip_intwp_p) :: lpartid
    integer(kind=ip_intwp_p) :: lnx,lny
    character(len=*),parameter :: subname = '(oasis_write_angle_r4)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' size = ',trim(cgrid),nx,ny
    endif

    lpartid = -1
    if (present(partid)) then
       lpartid = partid
    endif
    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' partid = ',trim(cgrid),lpartid
    endif

    lnx = size(angle,dim=1)
    lny = size(angle,dim=2)

    allocate(angle8(lnx,lny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING angle8 alloc'

    angle8 = angle
    call oasis_write_angle_r8(cgrid,nx,ny,angle8,partid=lpartid)

    deallocate(angle8)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_angle_r4

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_corner_r8(cgrid, nx, ny, nc, clon, clat, partid)

    !-------------------------------------------------
    ! Routine to add longitudes and latitudes of grid cell corners to an
    ! existing grids file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid  ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx     ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny     ! number of latitudes
    integer(kind=ip_intwp_p), intent (in) :: nc     ! number of corners per cell
    real(kind=ip_double_p),   intent (in) :: clon(:,:,:) ! longitudes
    real(kind=ip_double_p),   intent (in) :: clat(:,:,:) ! latitudes
    integer(kind=ip_intwp_p), intent (in),optional :: partid  ! partid if nonglobal
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    integer(kind=ip_intwp_p) :: lnx,lny
    character(len=*),parameter :: subname = '(oasis_write_corner_r8)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' size = ',trim(cgrid),nx,ny
    endif

    call oasis_findgrid(cgrid,nx,ny,gridID)

    lnx = size(clon,dim=1)
    lny = size(clon,dim=2)

    allocate(prism_grid(gridID)%clon(lnx,lny,nc),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING clon alloc'

    lnx = size(clat,dim=1)
    lny = size(clat,dim=2)

    allocate(prism_grid(gridID)%clat(lnx,lny,nc),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING clat alloc'

    prism_grid(gridID)%nc = nc
    prism_grid(gridID)%clon = clon
    prism_grid(gridID)%clat = clat
    prism_grid(gridID)%corner_set = .true.
    if (present(partid)) then
       if (prism_grid(gridID)%partid > 0 .and. prism_grid(gridID)%partid /= partid) then
          write(nulprt,*) subname,estr,'partid inconsistency',gridID,prism_grid(gridID)%partid,partid
          call oasis_abort()
       endif
       prism_grid(gridID)%partid = partid
       if (OASIS_debug >= 15) then
          write(nulprt,*) subname,' partid = ',trim(cgrid),partid
       endif
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_corner_r8

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_corner_r4(cgrid, nx, ny, nc, clon, clat, partid)

    !-------------------------------------------------
    ! Routine to add longitudes and latitudes of grid cell corners to an
    ! existing grids file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid  ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx     ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny     ! number of latitudes
    integer(kind=ip_intwp_p), intent (in) :: nc     ! number of corners per cell
    real(kind=ip_single_p),   intent (in) :: clon(:,:,:) ! longitudes
    real(kind=ip_single_p),   intent (in) :: clat(:,:,:) ! latitudes
    integer(kind=ip_intwp_p), intent (in),optional :: partid  ! partid if nonglobal
    !-------------------------------------------------
    real(kind=ip_double_p), allocatable :: clon8(:,:,:),clat8(:,:,:)
    integer(kind=ip_intwp_p) :: ierror
    integer(kind=ip_intwp_p) :: lpartid
    integer(kind=ip_intwp_p) :: lnx,lny
    character(len=*),parameter :: subname = '(oasis_write_corner_r4)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' size = ',trim(cgrid),nx,ny
    endif

    lpartid = -1
    if (present(partid)) then
       lpartid = partid
    endif
    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' partid = ',trim(cgrid),lpartid
    endif

    lnx = size(clon,dim=1)
    lny = size(clon,dim=2)

    allocate(clon8(lnx,lny,nc),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING clon8 alloc'

    lnx = size(clat,dim=1)
    lny = size(clat,dim=2)

    allocate(clat8(lnx,lny,nc),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING clat8 alloc'

    clon8 = clon
    clat8 = clat
    call oasis_write_corner_r8(cgrid,nx,ny,nc,clon8,clat8,partid=lpartid)

    deallocate(clon8)
    deallocate(clat8)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_corner_r4

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_mask(cgrid, nx, ny, mask, partid)

    !-------------------------------------------------
    ! Routine to create a new masks file or to add a land see mask to an
    ! existing masks file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    integer(kind=ip_intwp_p), intent (in) :: mask(:,:)   ! mask
    integer(kind=ip_intwp_p), intent (in),optional :: partid  ! partid if nonglobal
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    integer(kind=ip_intwp_p) :: lnx,lny
    character(len=*),parameter :: subname = '(oasis_write_mask)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' size = ',trim(cgrid),nx,ny
    endif

    call oasis_findgrid(cgrid,nx,ny,gridID)

    lnx = size(mask,dim=1)
    lny = size(mask,dim=2)

    allocate(prism_grid(gridID)%mask(lnx,lny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING mask alloc'

    prism_grid(gridID)%mask = mask
    prism_grid(gridID)%mask_set = .true.
    if (present(partid)) then
       if (prism_grid(gridID)%partid > 0 .and. prism_grid(gridID)%partid /= partid) then
          write(nulprt,*) subname,estr,'partid inconsistency',gridID,prism_grid(gridID)%partid,partid
          call oasis_abort()
       endif
       prism_grid(gridID)%partid = partid
       if (OASIS_debug >= 15) then
          write(nulprt,*) subname,' partid = ',trim(cgrid),partid
       endif
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_mask

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_area_r8(cgrid, nx, ny, area, partid)

    !-------------------------------------------------
    ! Routine to create a new areas file or to add areas of a grid to an
    ! existing areas file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    real(kind=ip_double_p),   intent (in) :: area(:,:)   ! areas
    integer(kind=ip_intwp_p), intent (in),optional :: partid  ! partid if nonglobal
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    integer(kind=ip_intwp_p) :: lnx,lny
    character(len=*),parameter :: subname = '(oasis_write_area_r8)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' size = ',trim(cgrid),nx,ny
    endif

    call oasis_findgrid(cgrid,nx,ny,gridID)

    lnx = size(area,dim=1)
    lny = size(area,dim=2)

    allocate(prism_grid(gridID)%area(lnx,lny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING area alloc'

    prism_grid(gridID)%area = area
    prism_grid(gridID)%area_set = .true.
    if (present(partid)) then
       if (prism_grid(gridID)%partid > 0 .and. prism_grid(gridID)%partid /= partid) then
          write(nulprt,*) subname,estr,'partid inconsistency',gridID,prism_grid(gridID)%partid,partid
          call oasis_abort()
       endif
       prism_grid(gridID)%partid = partid
       if (OASIS_debug >= 15) then
          write(nulprt,*) subname,' partid = ',trim(cgrid),partid
       endif
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_area_r8

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_area_r4(cgrid, nx, ny, area, partid)

    !-------------------------------------------------
    ! Routine to create a new areas file or to add areas of a grid to an
    ! existing areas file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    real(kind=ip_single_p),   intent (in) :: area(:,:)   ! areas
    integer(kind=ip_intwp_p), intent (in),optional :: partid  ! partid if nonglobal
    !-------------------------------------------------
    real(kind=ip_double_p), allocatable :: area8(:,:)
    integer(kind=ip_intwp_p) :: ierror
    integer(kind=ip_intwp_p) :: lpartid
    integer(kind=ip_intwp_p) :: lnx,lny
    character(len=*),parameter :: subname = '(oasis_write_area_r4)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' size = ',trim(cgrid),nx,ny
    endif

    lpartid = -1
    if (present(partid)) then
       lpartid = partid
    endif
    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' partid = ',trim(cgrid),lpartid
    endif

    lnx = size(area,dim=1)
    lny = size(area,dim=2)

    allocate(area8(lnx,lny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING area8 alloc'

    area8 = area
    call oasis_write_area_r8(cgrid,nx,ny,area8,partid=lpartid)

    deallocate(area8)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_area_r4

!--------------------------------------------------------------------------
    SUBROUTINE oasis_terminate_grids_writing()
    !-------------------------------------------------
    ! Routine to terminate the grids writing.
    !-------------------------------------------------

    implicit none
    integer(kind=ip_i4_p) :: n
    character(len=*),parameter :: subname = '(oasis_terminate_grids_writing)'

    call oasis_debug_enter(subname)

    if (OASIS_debug >= 15) then
       write(nulprt,*) subname,' prism_ngrid = ',prism_ngrid
    endif

    do n = 1,prism_ngrid
       prism_grid(n)%terminated = .true.
    enddo

    call oasis_print_grid_data()
! moved to prism_method_enddef for synchronization
!    call oasis_write2files()

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_terminate_grids_writing

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write2files()

    !-------------------------------------------------
    ! Write fields to grid files.
    ! Only write fields that have been buffered and
    ! if prism_grid_terminate_grids_writing has been called.
    ! this is called by all tasks
    !-------------------------------------------------

    implicit none

    !-------------------------------------------------
    character(len=ic_med) :: filename  ! grid filename
    character(len=ic_med) :: fldname   ! full field name
    character(len=ic_med) :: cgrid     ! grid name
    logical :: exists                  ! check if file exists
    integer(kind=ip_i4_p) :: n,n1,g,p  ! counter
    integer(kind=ip_i4_p) :: partid    ! part id
    integer(kind=ip_i4_p) :: taskid    ! task id for writing
    integer(kind=ip_i4_p) :: nx,ny,nc  ! grid size
    integer(kind=ip_i4_p) :: tnx,tny   ! temporary grid size
    logical :: partid_grid             ! global or local grid
    logical :: active_task             ! determine which tasks are involved
    logical :: write_task              ! task for writing
    real(kind=ip_realwp_p),allocatable :: rloc(:,:) ! local array
    real(kind=ip_realwp_p),allocatable :: rglo(:,:) ! global array
    real(kind=ip_realwp_p),allocatable :: r3glo(:,:,:) ! global array
    integer(kind=ip_i4_p) ,allocatable :: iglo(:,:) ! global array
    integer(kind=ip_intwp_p) :: gcnt, tot_gnum, ierr
    logical                  :: found
    character(len=ic_med)   ,pointer :: loc_gname(:),gname0(:),gname(:)
    character(len=ic_lvar2) ,pointer :: loc_pname(:),pname0(:),pname(:)
    integer(kind=ip_intwp_p),pointer :: gnum(:),rcnts(:),displ(:)
    logical, parameter :: local_timers_on = .true.
    character(len=*),parameter :: undefined_partname = '(UnDeFiNeD_PArtnaME)'
    character(len=*),parameter :: subname = '(oasis_write2files)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (local_timers_on) call oasis_timer_start('grid_write_gather')
    allocate(gnum(mpi_size_local))
    gnum = 0
    call MPI_GATHER(prism_ngrid, 1, MPI_INTEGER, gnum, 1, MPI_INTEGER, 0, mpi_comm_local, ierr)

    !--- gname

    if (mpi_rank_local == 0) then
       tot_gnum = sum(gnum)
       allocate(gname0(tot_gnum))
       allocate(rcnts(mpi_size_local),displ(mpi_size_local))
       do n = 1,mpi_size_local
          rcnts(n) = gnum(n) * ic_med
          if (n == 1) then
             displ(n) = 0
          else
             displ(n) = displ(n-1) + rcnts(n-1)
          endif
       enddo
    else
       allocate(gname0(1),rcnts(1),displ(1))
    endif

    allocate(loc_gname(prism_ngrid))
    do n = 1,prism_ngrid
       loc_gname(n) = prism_grid(n)%gridname
    enddo
    call MPI_GATHERV(loc_gname, prism_ngrid*ic_med, MPI_CHARACTER, gname0, rcnts, displ, MPI_CHARACTER, 0, mpi_comm_local, ierr) 
    deallocate(loc_gname)
    deallocate(rcnts, displ)

    !--- pname

    if (mpi_rank_local == 0) then
       tot_gnum = sum(gnum)
       allocate(pname0(tot_gnum))
       allocate(rcnts(mpi_size_local),displ(mpi_size_local))
       do n = 1,mpi_size_local
          rcnts(n) = gnum(n) * ic_lvar2
          if (n == 1) then
             displ(n) = 0
          else
             displ(n) = displ(n-1) + rcnts(n-1)
          endif
       enddo
    else
       allocate(pname0(1),rcnts(1),displ(1))
    endif

    allocate(loc_pname(prism_ngrid))
    do n = 1,prism_ngrid
       if (prism_grid(n)%partid > 0 .and. prism_grid(n)%partid <= prism_npart) then
          loc_pname(n) = prism_part(prism_grid(n)%partid)%partname
       elseif (prism_grid(n)%partid == -1) then
          loc_pname(n) = undefined_partname
       else
          write(nulprt,*) subname,estr,'illegal partition id for grid ',trim(prism_grid(n)%gridname),prism_grid(n)%partid
       endif
    enddo
    call MPI_GATHERV(loc_pname, prism_ngrid*ic_lvar2, MPI_CHARACTER, pname0, rcnts, displ, MPI_CHARACTER, 0, mpi_comm_local, ierr) 
    deallocate(loc_pname)
    if (local_timers_on) call oasis_timer_stop ('grid_write_gather')
 
    !--- determine unique var names on root

    if (local_timers_on) call oasis_timer_start('grid_write_rootsrch')
    if (mpi_rank_local == 0) then
       gcnt = 0
       do n = 1,tot_gnum
          if (OASIS_Debug >= 15) &
             write(nulprt,*) subname,' check gname0 ',n,trim(gname0(n))
          g = 0
          found = .false.
          do while (g < gcnt .and. .not.found)
             g = g + 1
             if (gname0(n) == gname0(g)) then
                found = .true.
                !--- use something other than undefined_partname if it exists
                if (pname0(g) == undefined_partname) then
                   pname0(g) = pname0(n)
                elseif (pname0(n) /= undefined_partname .and. pname0(g) /= pname0(n)) then
                   write(nulprt,*) subname,estr,'inconsistent grid and part name: ',trim(gname0(n)),' ',trim(pname0(n)),' ',trim(pname0(g))
                   call oasis_abort()
                endif
             endif
          enddo
          if (.not.found) then
             gcnt = gcnt + 1
             gname0(gcnt) = gname0(n)
             pname0(gcnt) = pname0(n)
          endif
       enddo
    endif
    if (local_timers_on) call oasis_timer_stop ('grid_write_rootsrch')

    if (local_timers_on) call oasis_timer_start('grid_write_bcast')
    call oasis_mpi_bcast(gcnt,mpi_comm_local,subname//' pcnt')
    allocate(gname(gcnt))
    allocate(pname(gcnt))
    if (mpi_rank_local == 0) then
       gname(1:gcnt) = gname0(1:gcnt)
       pname(1:gcnt) = pname0(1:gcnt)
    endif
    deallocate(gname0)
    deallocate(pname0)
    call oasis_mpi_bcast(gname,mpi_comm_local,subname//' gname')
    call oasis_mpi_bcast(pname,mpi_comm_local,subname//' pname')

    !--- document

    if (OASIS_debug >= 15) then
       do n = 1,gcnt
          write(nulprt,*) subname,' grid writing, n,grid:partition = ',n,trim(gname(n)),':',trim(pname(n))
       enddo
       call oasis_flush(nulprt)
    endif
    if (local_timers_on) call oasis_timer_stop ('grid_write_bcast')

    !--- check grid defined on a partitition is defined on all tasks on that partition

    do n = 1,gcnt
       if (pname(n) /= undefined_partname) then
          do p = 1,prism_npart
             if (pname(n) == prism_part(p)%partname .and. prism_part(p)%mpicom /= MPI_COMM_NULL) then
                found = .false.
                do g = 1,prism_ngrid
                   if (prism_grid(g)%gridname == gname(n)) found = .true.
                enddo
                if (.not. found) then
                   write(nulprt,*) subname,estr,'grid with partition not defined on all partition tasks: ',trim(gname(n))
                   call oasis_abort()
                endif
             endif
          enddo
       endif
    enddo

    if (local_timers_on) call oasis_timer_start('grid_write_writefiles')

    do g = 1,gcnt
    do n = 1,prism_ngrid
    if (prism_grid(n)%terminated) then
    if (prism_grid(n)%gridname == gname(g)) then

       cgrid  = gname(g)
       partid = prism_grid(n)%partid
       prism_grid(n)%written = .true.
       tnx = -1
       tny = -1

       active_task = .false.
       write_task = .false.
       if (pname(g) == undefined_partname) then
          partid_grid = .false.
          if (mpi_rank_local == 0) active_task = .true.
          if (mpi_rank_local == 0) write_task = .true.
       else
          partid_grid = .true.
          if (partid > 0 .and. partid <= prism_npart) then
             taskid = 0
             if (prism_part(partid)%mpicom /= MPI_COMM_NULL) active_task = .true.
             if (prism_part(partid)%rank == taskid) write_task = .true.
          elseif (partid == -1) then
             active_task = .false.
             write_task = .false.
          else
             write(nulprt,*) subname,estr,'illegal partid for grid:',trim(gname(g)),trim(pname(g)),partid
             call oasis_abort()
          endif
       endif

       if (OASIS_debug >= 15) then
          write(nulprt,*) subname,' ',trim(gname(g)),':',trim(pname(g)),': partid_grid=',partid_grid,'active_task=',active_task,'write_task=',write_task
       endif
       !--------------

       if (active_task) then

         nx = prism_grid(n)%nx
         ny = prism_grid(n)%ny
         nc = prism_grid(n)%nc

         allocate(rglo(nx,ny))

         if (prism_grid(n)%grid_set) then
           if (tnx <= 0 .or. tny <= 0) then
             tnx = size(prism_grid(n)%lon,dim=1)
             tny = size(prism_grid(n)%lon,dim=2)
           endif
           if (size(prism_grid(n)%lon,dim=1) /= tnx .or. &
               size(prism_grid(n)%lon,dim=2) /= tny .or. &
               size(prism_grid(n)%lat,dim=1) /= tnx .or. &
               size(prism_grid(n)%lat,dim=2) /= tny ) then
             write(nulprt,*) subname,estr,'inconsistent array size lon/lat ',tnx,tny, &
               size(prism_grid(n)%lon,dim=1),size(prism_grid(n)%lon,dim=2),  &
               size(prism_grid(n)%lat,dim=1),size(prism_grid(n)%lat,dim=2)
             call oasis_abort()
           endif

           filename = 'grids.nc'
           fldname  = trim(cgrid)//'.lon'
           if (partid_grid) then
             call oasis_grid_loc2glo(prism_grid(n)%lon,rglo,partid,0)
           else
             rglo = prism_grid(n)%lon
           endif
           if (write_task) call oasis_io_write_2dgridfld_fromroot(filename,fldname,rglo,nx,ny)

           filename = 'grids.nc'
           fldname  = trim(cgrid)//'.lat'
           if (partid_grid) then
             call oasis_grid_loc2glo(prism_grid(n)%lat,rglo,partid,0)
           else
             rglo = prism_grid(n)%lat
           endif
           if (write_task) call oasis_io_write_2dgridfld_fromroot(filename,fldname,rglo,nx,ny)
         endif  ! grid_set

         if (prism_grid(n)%corner_set) then
           if (tnx <= 0 .or. tny <= 0) then
             tnx = size(prism_grid(n)%clon,dim=1)
             tny = size(prism_grid(n)%clon,dim=2)
           endif
           if (size(prism_grid(n)%clon,dim=1) /= tnx .or. &
               size(prism_grid(n)%clon,dim=2) /= tny .or. &
               size(prism_grid(n)%clat,dim=1) /= tnx .or. &
               size(prism_grid(n)%clat,dim=2) /= tny ) then
             write(nulprt,*) subname,estr,'inconsistent array size clon/clat ',tnx,tny, &
               size(prism_grid(n)%clon,dim=1),size(prism_grid(n)%clon,dim=2),  &
               size(prism_grid(n)%clat,dim=1),size(prism_grid(n)%clat,dim=2)
             call oasis_abort()
           endif

           allocate(r3glo(nx,ny,nc))
           filename = 'grids.nc'
           fldname  = trim(cgrid)//'.clo'
           if (partid_grid) then
             allocate(rloc(tnx,tny))
             do n1 = 1,nc
               rloc(:,:) = prism_grid(n)%clon(:,:,n1)
               call oasis_grid_loc2glo(rloc,rglo,partid,0)
               r3glo(:,:,n1) = rglo(:,:)
             enddo
             deallocate(rloc)
           else
             r3glo = prism_grid(n)%clon
           endif
           if (write_task) call oasis_io_write_3dgridfld_fromroot(filename,fldname,r3glo,nx,ny,nc)

           filename = 'grids.nc'
           fldname  = trim(cgrid)//'.cla'
           if (partid_grid) then
             allocate(rloc(tnx,tny))
             do n1 = 1,nc
               rloc(:,:) = prism_grid(n)%clat(:,:,n1)
               call oasis_grid_loc2glo(rloc,rglo,partid,0)
               r3glo(:,:,n1) = rglo(:,:)
             enddo
             deallocate(rloc)
           else
             r3glo = prism_grid(n)%clat
           endif
           if (write_task) call oasis_io_write_3dgridfld_fromroot(filename,fldname,r3glo,nx,ny,nc)
           deallocate(r3glo)
         endif  ! corner_set

         if (prism_grid(n)%area_set) then
           if (tnx <= 0 .or. tny <= 0) then
             tnx = size(prism_grid(n)%area,dim=1)
             tny = size(prism_grid(n)%area,dim=2)
           endif
           if (size(prism_grid(n)%area,dim=1) /= tnx .or. &
               size(prism_grid(n)%area,dim=2) /= tny ) then
             write(nulprt,*) subname,estr,'inconsistent array size area ',tnx,tny, &
               size(prism_grid(n)%area,dim=1),size(prism_grid(n)%area,dim=2)
             call oasis_abort()
           endif

           filename = 'areas.nc'
           fldname  = trim(cgrid)//'.srf'
           if (partid_grid) then
             call oasis_grid_loc2glo(prism_grid(n)%area,rglo,partid,0)
           else
             rglo = prism_grid(n)%area
           endif
           if (write_task) call oasis_io_write_2dgridfld_fromroot(filename,fldname,rglo,nx,ny)
         endif  ! area_set

         if (prism_grid(n)%angle_set) then
           if (tnx <= 0 .or. tny <= 0) then
             tnx = size(prism_grid(n)%angle,dim=1)
             tny = size(prism_grid(n)%angle,dim=2)
           endif
           if (size(prism_grid(n)%angle,dim=1) /= tnx .or. &
               size(prism_grid(n)%angle,dim=2) /= tny ) then
             write(nulprt,*) subname,estr,'inconsistent array size angle ',tnx,tny, &
                size(prism_grid(n)%angle,dim=1),size(prism_grid(n)%angle,dim=2)
             call oasis_abort()
           endif

           filename = 'grids.nc'
           fldname  = trim(cgrid)//'.ang'
           if (partid_grid) then
             call oasis_grid_loc2glo(prism_grid(n)%lon,rglo,partid,0)
           else
             rglo = prism_grid(n)%angle
           endif
           if (write_task) call oasis_io_write_2dgridfld_fromroot(filename,fldname,rglo,nx,ny)
         endif  ! angle_set

         if (prism_grid(n)%mask_set) then
           if (tnx <= 0 .or. tny <= 0) then
             tnx = size(prism_grid(n)%mask,dim=1)
             tny = size(prism_grid(n)%mask,dim=2)
           endif
           if (size(prism_grid(n)%mask,dim=1) /= tnx .or. &
               size(prism_grid(n)%mask,dim=2) /= tny ) then
             write(nulprt,*) subname,estr,'inconsistent array size mask ',tnx,tny, &
                size(prism_grid(n)%mask,dim=1),size(prism_grid(n)%mask,dim=2)
             call oasis_abort()
           endif

           allocate(iglo(nx,ny))
           filename = 'masks.nc'
           fldname  = trim(cgrid)//'.msk'
           if (partid_grid) then
             allocate(rloc(tnx,tny))
             rloc = prism_grid(n)%mask
             call oasis_grid_loc2glo(rloc,rglo,partid,0)
             iglo = nint(rglo)
             deallocate(rloc)
           else
             iglo = prism_grid(n)%mask
           endif
           if (write_task) call oasis_io_write_2dgridint_fromroot(filename,fldname,iglo,nx,ny)
           deallocate(iglo)
         endif ! mask_set

         deallocate(rglo)

       endif  ! active_task

    endif  ! grid_name = gname
    endif  ! terminated
    enddo  ! n = 1,prism_ngrid
    enddo  ! g = 1,gcnt

    if (local_timers_on) call oasis_timer_stop('grid_write_writefiles')
    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write2files
!--------------------------------------------------------------------------

    SUBROUTINE oasis_findgrid(cgrid,nx,ny,gridID)
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
    character(len=*),parameter :: subname = '(oasis_findgrid)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    gridID = -1
    do n = 1,prism_ngrid
       if (trim(cgrid) == trim(prism_grid(n)%gridname)) then
          gridID = n
          ! since grid is defined before, make sure nx,ny match
          if (nx /= prism_grid(gridID)%nx .or. ny /= prism_grid(gridID)%ny) then
             write(nulprt,*) subname,estr,'in predefined grid size = ',nx,ny, &
                prism_grid(gridID)%nx,prism_grid(gridID)%ny
             call oasis_abort()
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

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_findgrid

!--------------------------------------------------------------------------

  SUBROUTINE oasis_grid_loc2glo(aloc,aglo,partid,taskid)
    !-------------------------------------------------
    ! Routine that gathers local array using partition
    !-------------------------------------------------
    implicit none

    real(kind=ip_realwp_p),intent(in)    :: aloc(:,:)
    real(kind=ip_realwp_p),intent(inout) :: aglo(:,:)
    integer(kind=ip_i4_p) ,intent(in)    :: partid
    integer(kind=ip_i4_p) ,intent(in)    :: taskid
    !-------------------------------------------------
    type(mct_aVect) :: avloc,avglo
    integer(kind=ip_i4_p) :: i,j,n
    integer(kind=ip_i4_p) :: lnx,lny,gnx,gny
    character(len=*),parameter :: subname = '(oasis_grid_loc2glo)'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (prism_part(partid)%mpicom /= MPI_COMM_NULL) then

       lnx = size(aloc,dim=1)
       lny = size(aloc,dim=2)
       gnx = size(aglo,dim=1)
       gny = size(aglo,dim=2)
       call mct_avect_init(avloc,rList='field',lsize=lnx*lny)

       n = 0
       do j = 1,lny
       do i = 1,lnx
          n = n + 1
          avloc%rattr(1,n) = aloc(i,j)
       enddo
       enddo

       call mct_aVect_gather(avloc,avglo,prism_part(partid)%pgsmap,taskid,prism_part(partid)%mpicom)

       if (prism_part(partid)%rank == taskid) then
          n = 0
          do j = 1,gny
          do i = 1,gnx
             n = n + 1
             aglo(i,j) = avglo%rattr(1,n)
          enddo
          enddo
          call mct_avect_clean(avglo)
       endif

       call mct_avect_clean(avloc)

    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_grid_loc2glo

!--------------------------------------------------------------------------
END MODULE mod_oasis_grid
!--------------------------------------------------------------------------


     
