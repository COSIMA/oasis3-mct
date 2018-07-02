!****
!                    ************************
!                    *     OASIS MODULE     *
!                    *     ------------     *
!                    ************************
!****
!***********************************************************************
!     This module belongs to the SCRIP library. It is modified to run
!     within OASIS. 
!     Modifications:
!       - routine does not read SCRIP grid description files, but gets 
!         arrays from the calling routine
!       - unit conversion : only from degrees (OASIS unit) to radians
!       - some allocated array will be freed in the end to allow multiple
!         calls of SCRIP
!       - map-methods and restriction-types are written in capital letters
!       - added bin definition for reduced grid
!
!     Modified by            V. Gayler,  M&D                  20.09.2001
!     Modified by            D. Declat,  CERFACS              27.06.2002
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This module reads in and initializes two grids for remapping.
!     NOTE: grid1 must be the master grid -- the grid that determines
!           which cells participate (e.g. land mask) and the fractional
!           area of grid2 cells that participate in the remapping.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: grids.f 2826 2010-12-10 11:14:21Z valcke $
!
!     Copyright (c) 1997, 1998 the Regents of the University of 
!       California.
!
!     This software and ancillary information (herein called software) 
!     called SCRIP is made available under the terms described here.  
!     The software has been approved for release with associated 
!     LA-CC Number 98-45.
!
!     Unless otherwise indicated, this software has been authored
!     by an employee or employees of the University of California,
!     operator of the Los Alamos National Laboratory under Contract
!     No. W-7405-ENG-36 with the U.S. Department of Energy.  The U.S.
!     Government has rights to use, reproduce, and distribute this
!     software.  The public may copy and use this software without
!     charge, provided that this Notice and any statement of authorship
!     are reproduced on all copies.  Neither the Government nor the
!     University makes any warranty, express or implied, or assumes
!     any liability or responsibility for the use of this software.
!
!     If software is modified to produce derivative works, such modified
!     software should be clearly marked, so as not to confuse it with 
!     the version available from Los Alamos National Laboratory.
!
!***********************************************************************

      module grids

!-----------------------------------------------------------------------

      use kinds_mod    ! defines data types
      use constants    ! common constants
      use iounits      ! I/O unit manager
      USE mod_oasis_flush

      implicit none

!-----------------------------------------------------------------------
!
!     variables that describe each grid
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), save :: grid1_size, grid2_size,    & ! total points on each grid
                                       grid1_rank, grid2_rank,    & ! rank of each grid
                                       grid1_corners, grid2_corners ! number of corners
                                                                    ! for each grid cell

      integer (kind=int_kind), dimension(:), allocatable, save :: grid1_dims, grid2_dims  ! size of each grid dimension

      character(char_len), save :: grid1_name, grid2_name  ! name for each grid

      character (char_len), save :: grid1_units, & ! units for grid coords (degs/radians)
                                    grid2_units    ! units for grid coords

      real (kind=dbl_kind), parameter :: deg2rad = pi/180.   ! conversion for deg to rads

!-----------------------------------------------------------------------
!
!     grid coordinates and masks
!
!-----------------------------------------------------------------------

      logical (kind=log_kind), dimension(:), allocatable, save :: grid1_mask, & ! flag which cells participate
                                                                  grid2_mask    ! flag which cells participate

      real (kind=dbl_kind), dimension(:), allocatable, save :: grid1_center_lat, & ! lat/lon coordinates for
                                                               grid1_center_lon, & ! each grid center in radians
                                                               grid2_center_lat, & 
                                                               grid2_center_lon, &
                                                               grid1_area,       & ! tot area of each grid1 cell
                                                               grid2_area,       & ! tot area of each grid2 cell
                                                               grid1_area_in,    & ! area of grid1 cell from file
                                                               grid2_area_in,    & ! area of grid2 cell from file
                                                               grid1_frac,       & ! fractional area of grid cells
                                                               grid2_frac          ! participating in remapping

      real (kind=dbl_kind), dimension(:,:), allocatable, save :: grid1_corner_lat, & ! lat/lon coordinates for
                                                                 grid1_corner_lon, & ! each grid corner in radians
                                                                 grid2_corner_lat, &
                                                                 grid2_corner_lon

      logical (kind=log_kind), save :: luse_grid_centers, & ! use centers for bounding boxes
                                       luse_grid1_area,   & ! use area from grid file
                                       luse_grid2_area      ! use area from grid file

      real (kind=dbl_kind), dimension(:,:), allocatable, save :: grid1_bound_box, & ! lat/lon bounding box for use
                                                                 grid2_bound_box    ! in restricting grid searches

      integer, dimension(:), allocatable, save :: grid1_bbox_per, & ! bbox position wrt [0,2pi]
                                                  grid2_bbox_per

!-----------------------------------------------------------------------
!
!     bins for restricting searches
!
!-----------------------------------------------------------------------

      character (char_len), save :: restrict_type  ! type of bins to use

      integer (kind=int_kind), save :: num_srch_bins, & ! num of bins for restricted srch
                                       num_srch_red    ! num of bins for reduced case

      integer (kind=int_kind), dimension(:,:), allocatable, save :: bin_addr1, & ! min,max adds for grid1 cells in this lat bin
                                                                    bin_addr2    ! min,max adds for grid2 cells in this lat bin
      integer (kind=int_kind), dimension(:,:), allocatable, save :: bin_addr1_r  ! min,max adds for red grid1 cells

      real(kind=dbl_kind), dimension(:,:), allocatable, save :: bin_lats, &   ! min,max latitude for each search bin
                                                                bin_lons      ! min,max longitude for each search bin
      real(kind=dbl_kind), dimension(:,:), allocatable, save :: bin_lats_r    ! min,max lat for each search bin for red grid

!***********************************************************************

      contains

!***********************************************************************

      subroutine grid_init(m_method, rst_type, n_srch_bins,         &
                           src_size, dst_size, src_dims, dst_dims,  &
                           src_rank, dst_rank, ncrn_src, ncrn_dst,  &
                           src_mask, dst_mask, src_name, dst_name,  &
                           src_lat,  src_lon,  dst_lat,  dst_lon,   &
                           src_corner_lat, src_corner_lon,          &
                           dst_corner_lat, dst_corner_lon,          &
                           ilogunit, ilogprt)

!-----------------------------------------------------------------------
!
!     this routine gets grid info from routine scriprmp and makes any
!     necessary changes (e.g. for 0,2pi longitude range)
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) :: n_srch_bins,       & ! num of bins for restricted srch
                                             src_size,          & ! source grid size
                                             dst_size,          & ! target grid size
                                             src_rank,          & ! source grid rank
                                             dst_rank,          & ! target grid rank
                                             src_dims(src_rank),& ! source grid dimensions
                                             dst_dims(dst_rank),& ! target grid dimensions
                                             ncrn_src,          & ! number of corners of a source grid cell
                                             ncrn_dst,          & ! number of corners of a target grid cell
                                             src_mask(src_size),& ! source grid mask (master mask)
                                             dst_mask(dst_size)   ! target grid mask 

      integer(kind=int_kind), intent(in), optional :: ilogunit
      integer(kind=int_kind), intent(in), optional :: ilogprt

      character*8, intent(in) :: m_method, &          ! remapping method
                                 rst_type, &          ! restriction type
                                 src_name, &          ! source grid name
                                 dst_name             ! target grid name

      real (kind=real_kind), intent (in) :: src_lat(src_size), & ! source grid latitudes
                                            src_lon(src_size), & ! sourde grid longitudes
                                            dst_lat(dst_size), & ! target grid latitudes
                                            dst_lon(dst_size), & ! target grid longitudes
                                            src_corner_lat(ncrn_src,src_size), &
                                            src_corner_lon(ncrn_src,src_size), &
                                            dst_corner_lat(ncrn_dst,dst_size), &
                                            dst_corner_lon(ncrn_dst,dst_size) 

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n,    & ! loop counter
                                 next_n, nnext_n, &
                                 nele, & ! element loop counter
                                 i,j,  & ! logical 2d addresses
                                 ip1,jp1, n_add, e_add, ne_add, nx, ny

      real (kind=dbl_kind) :: dlat, dlon, &          ! lat/lon intervals for search bins
         vec1_lat, vec1_lon, & ! vectors and cross products used
         vec2_lat, vec2_lon, & ! during grid check
         vecc_lat, vecc_lon, & ! during grid check
         cross_product, cross_product_ctr

      logical (kind=log_kind) :: ll_error

      real (kind=dbl_kind), dimension(4) ::tmp_lats, tmp_lons  ! temps for computing bounding boxes
      character(len=*),parameter :: subname = 'scrip:grid_init '

      logical (kind=log_kind) :: lp_debug_grid_corners = .false.

!
!-----------------------------------------------------------------------
!
      if (present(ilogunit)) then
         nulou = ilogunit
      endif

      if (present(ilogprt)) then
         nlogprt = ilogprt
      endif

      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine grid_init'
         WRITE (UNIT = nulou,FMT = *)' '
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     allocate grid coordinates/masks and read data
!
!-----------------------------------------------------------------------

!      write(nulou,*) subname,trim(m_method)
!      write(nulou,*) subname,src_size,dst_size,src_rank,dst_rank

      select case(m_method)
      case ('CONSERV')
        luse_grid_centers = .false.
      case ('BILINEAR')
        luse_grid_centers = .true.
      case ('BICUBIC')
        luse_grid_centers = .true.
      case ('DISTWGT')
        luse_grid_centers = .true.
      case ('GAUSWGT')
        luse_grid_centers = .true.
      case default
        stop 'unknown mapping method'
      end select

      allocate( grid1_mask      (src_size), &
                grid2_mask      (dst_size), &
                grid1_center_lat(src_size), &
                grid1_center_lon(src_size), &
                grid2_center_lat(dst_size), &
                grid2_center_lon(dst_size), &
                grid1_area      (src_size), &
                grid2_area      (dst_size), &
                grid1_frac      (src_size), &
                grid2_frac      (dst_size), &
                grid1_dims      (src_rank), &
                grid2_dims      (dst_rank), &
                grid1_bound_box (4       , src_size), &
                grid2_bound_box (4       , dst_size), &
                grid1_bbox_per  (src_size), &
                grid2_bbox_per  (dst_size))

      if (.not. luse_grid_centers) then
        allocate( grid1_corner_lat(ncrn_src, src_size), &
                  grid1_corner_lon(ncrn_src, src_size), &
                  grid2_corner_lat(ncrn_dst, dst_size), &
                  grid2_corner_lon(ncrn_dst, dst_size)) 
      endif

!-----------------------------------------------------------------------
!
!     copy input data to module data
!
!-----------------------------------------------------------------------

      restrict_type    = rst_type
      num_srch_bins    = n_srch_bins
      grid1_size       = src_size
      grid2_size       = dst_size
      grid1_dims       = src_dims
      grid2_dims       = dst_dims 
      grid1_rank       = src_rank
      grid2_rank       = dst_rank
      grid1_corners    = ncrn_src
      grid2_corners    = ncrn_dst
      grid1_name       = src_name
      grid2_name       = dst_name
      grid1_center_lat = src_lat
      grid1_center_lon = src_lon
      grid2_center_lat = dst_lat
      grid2_center_lon = dst_lon

      if (.not. luse_grid_centers) then
        grid1_corner_lat = src_corner_lat
        grid1_corner_lon = src_corner_lon
        grid2_corner_lat = dst_corner_lat
        grid2_corner_lon = dst_corner_lon
      endif

!      if (luse_grid1_area) then
!        grid1_area_in
!      endif
!      if (luse_grid2_area) then
!        grid2_area_in
!      endif

      grid1_area = zero
      grid1_frac = zero
      grid2_area = zero
      grid2_frac = zero

!-----------------------------------------------------------------------
!
!     initialize logical mask and convert lat/lon units if required
!
!-----------------------------------------------------------------------
      where (src_mask == 1)
        grid1_mask = .true.
      elsewhere
        grid1_mask = .false.
      endwhere

      where (dst_mask == 1)
        grid2_mask = .true.
      elsewhere
        grid2_mask = .false.
      endwhere

!
!* -- convert unit from degrees (OASIS unit) to radians
!
      grid1_center_lat = grid1_center_lat*deg2rad
      grid1_center_lon = grid1_center_lon*deg2rad
      grid2_center_lat = grid2_center_lat*deg2rad
      grid2_center_lon = grid2_center_lon*deg2rad

      if (.not. luse_grid_centers) then
        grid1_corner_lat = grid1_corner_lat*deg2rad
        grid1_corner_lon = grid1_corner_lon*deg2rad
        grid2_corner_lat = grid2_corner_lat*deg2rad
        grid2_corner_lon = grid2_corner_lon*deg2rad
      endif

      grid1_units='radians'
      grid2_units='radians'

!-----------------------------------------------------------------------
!
!     change corners periodicity according to center position
!
!-----------------------------------------------------------------------

      if (.not. luse_grid_centers) then

         do nele = 1, grid1_size
            if (maxval(grid1_corner_lon(:,nele)) - &
               &minval(grid1_corner_lon(:,nele)) > pi) then
!AP               write(nulou,*) 'WARNING fixing wrong corners periodicity for grid1, nele =',nele
               do n = 1,ncrn_src
                  if (grid1_corner_lon(n,nele) < grid1_center_lon(nele) - pi) then
                     grid1_corner_lon(n,nele) = grid1_corner_lon(n,nele) + pi2
                  else
                     if (grid1_corner_lon(n,nele) > grid1_center_lon(nele) + pi) then
                        grid1_corner_lon(n,nele) = grid1_corner_lon(n,nele) - pi2
                     end if
                  end if
               end do
            end if
         end do

         do nele = 1, grid2_size
            if (maxval(grid2_corner_lon(:,nele)) - &
               &minval(grid2_corner_lon(:,nele)) > pi) then
!AP               write(nulou,*) 'WARNING fixing wrong corners periodicity for grid2, nele =',nele
               do n = 1,ncrn_dst
                  if (grid2_corner_lon(n,nele) < grid2_center_lon(nele) - pi) then
                     grid2_corner_lon(n,nele) = grid2_corner_lon(n,nele) + pi2
                  else
                     if (grid2_corner_lon(n,nele) > grid2_center_lon(nele) + pi) then
                        grid2_corner_lon(n,nele) = grid2_corner_lon(n,nele) - pi2
                     end if
                  end if
               end do
            end if
         end do

      end if

!-----------------------------------------------------------------------
!
!     convert center longitudes to 0,2pi interval preserving corners
!
!-----------------------------------------------------------------------

      do n = 1, grid1_size
         if (grid1_center_lon(n) .gt. pi2) then
            grid1_center_lon(n) = grid1_center_lon(n) - pi2
            if (.not. luse_grid_centers) &
               & grid1_corner_lon(:,n) = grid1_corner_lon(:,n) - pi2
         end if
      end do

      do n = 1, grid1_size
         if  (grid1_center_lon(n) .lt. zero) then
            grid1_center_lon(n) = grid1_center_lon(n) + pi2
            if (.not. luse_grid_centers) &
               & grid1_corner_lon(:,n) = grid1_corner_lon(:,n) + pi2
         end if
      end do

      do n = 1, grid2_size
         if  (grid2_center_lon(n) .gt. pi2) then
            grid2_center_lon(n) = grid2_center_lon(n) - pi2
            if (.not. luse_grid_centers) &
               & grid2_corner_lon(:,n) = grid2_corner_lon(:,n) - pi2
         end if
      end do

      do n = 1, grid2_size
         if (grid2_center_lon(n) .lt. zero) then
            grid2_center_lon(n) = grid2_center_lon(n) + pi2
            if (.not. luse_grid_centers) &
               & grid2_corner_lon(:,n) = grid2_corner_lon(:,n) + pi2
         end if
      end do

!-----------------------------------------------------------------------
!
!     make sure input latitude range is within the machine values
!     for +/- pi/2 
!
!-----------------------------------------------------------------------

      where (grid1_center_lat >  pih) grid1_center_lat =  pih
      where (grid1_center_lat < -pih) grid1_center_lat = -pih
      where (grid2_center_lat >  pih) grid2_center_lat =  pih
      where (grid2_center_lat < -pih) grid2_center_lat = -pih

      if (.not. luse_grid_centers) then
        where (grid1_corner_lat >  pih) grid1_corner_lat =  pih
        where (grid1_corner_lat < -pih) grid1_corner_lat = -pih
        where (grid2_corner_lat >  pih) grid2_corner_lat =  pih
        where (grid2_corner_lat < -pih) grid2_corner_lat = -pih
      endif

!-----------------------------------------------------------------------
!
!     check corner counterclockwise numbering and consistency
!
!-----------------------------------------------------------------------

      if (lp_debug_grid_corners .and. .not. luse_grid_centers) then

         ll_error = .false.
         do nele = 1, grid1_size
            corner_loop1: do n=1,ncrn_src
               next_n  = mod(n,ncrn_src) + 1
               nnext_n = mod(next_n,ncrn_src) + 1
               vec1_lat = grid1_corner_lat(next_n, nele) - grid1_corner_lat(n,nele)
               vec1_lon = grid1_corner_lon(next_n, nele) - grid1_corner_lon(n,nele)
               vec2_lat = grid1_corner_lat(nnext_n,nele) - grid1_corner_lat(n,nele)
               vec2_lon = grid1_corner_lon(nnext_n,nele) - grid1_corner_lon(n,nele)
               vecc_lat = grid1_center_lat(nele) - grid1_corner_lat(n,nele)
               vecc_lon = grid1_center_lon(nele) - grid1_corner_lon(n,nele)

               cross_product     = vec1_lon*vec2_lat - vec2_lon*vec1_lat
               cross_product_ctr = vec1_lon*vecc_lat - vecc_lon*vec1_lat
               !***
               !*** if cross product is less than zero, this cell
               !*** doesn't work
               !***
               if (cross_product < zero) then
                  if (grid1_mask(nele)) then
                     write(nulou,'(A,I7,A)') ' ERROR   : valid  cell ', nele,'&
                        & from src grid '//trim(grid1_name)//' does not respect corner order '
                     ll_error = .true.
                  else
                     write(nulou,'(A,I7,A)') ' WARNING : masked cell ', nele,'&
                        & from src grid '//trim(grid1_name)//' does not respect corner order '
                  end if
                  write(nulou,'(A,2F9.2)') '         grid_center lon and lat (deg) ',&
                     &                     grid1_center_lon(nele)/deg2rad,&
                     &                     grid1_center_lat(nele)/deg2rad
                  do next_n = 1,ncrn_src
                     write(nulou,'(A,I3,A,2F9.2)') '         grid_corner ',next_n,' lon and lat (deg) ',&
                        &                          grid1_corner_lon(next_n,nele)/deg2rad,&
                        &                          grid1_corner_lat(next_n,nele)/deg2rad
                  end do
                  exit corner_loop1
               end if

               if (cross_product_ctr < zero) then
                  if (abs(grid1_center_lat(nele)) .ge. 1.48) then
                     if (grid1_mask(nele)) then
                        write(nulou,'(A,I7,A)') ' WARNING : valid  polar cell ', nele,'&
                           & from src grid '//trim(grid1_name)//' could lead to'
                     else
                        write(nulou,'(A,I7,A)') ' WARNING : masked polar cell ', nele,'&
                           & from src grid '//trim(grid1_name)//' could lead to'
                     end if
                     write(nulou,'(2A)') '           a loss in precision if',&
                        & ' a polar projection is not active in remap_conserv'
                  else
                     if (grid1_mask(nele)) then
                        write(nulou,'(A,I7,A)') ' WARNING : valid  cell ', nele,'&
                           & from src grid '//trim(grid1_name)//' does not contain its center'
                     else
                        write(nulou,'(A,I7,A)') ' WARNING : masked cell ', nele,'&
                           & from src grid '//trim(grid1_name)//' does not contain its center'
                     end if
                  end if
                  write(nulou,'(A,2F9.2)') '         grid_center lon and lat (deg) ',&
                     &                     grid1_center_lon(nele)/deg2rad,&
                     &                     grid1_center_lat(nele)/deg2rad
                  do next_n = 1,ncrn_src
                     write(nulou,'(A,I3,A,2F9.2)') '         grid_corner ',next_n,' lon and lat (deg) ',&
                        &                          grid1_corner_lon(next_n,nele)/deg2rad,&
                        &                          grid1_corner_lat(next_n,nele)/deg2rad
                  end do
                  exit corner_loop1
               end if

            end do corner_loop1

         end do

         do nele = 1, grid2_size
            corner_loop2: do n=1,ncrn_dst
               next_n  = mod(n,ncrn_dst) + 1
               nnext_n = mod(next_n,ncrn_dst) + 1
               vec1_lat = grid2_corner_lat(next_n, nele) - grid2_corner_lat(n,nele)
               vec1_lon = grid2_corner_lon(next_n, nele) - grid2_corner_lon(n,nele)
               vec2_lat = grid2_corner_lat(nnext_n,nele) - grid2_corner_lat(n,nele)
               vec2_lon = grid2_corner_lon(nnext_n,nele) - grid2_corner_lon(n,nele)
               vecc_lat = grid2_center_lat(nele) - grid2_corner_lat(n,nele)
               vecc_lon = grid2_center_lon(nele) - grid2_corner_lon(n,nele)

               cross_product = vec1_lon*vec2_lat - vec2_lon*vec1_lat
               cross_product_ctr = vec1_lon*vecc_lat - vecc_lon*vec1_lat
               !***
               !*** if cross product is less than zero, this cell
               !*** doesn't work
               !***
               if (cross_product < zero) then
                  if (grid2_mask(nele)) then
                     write(nulou,'(A,I7,A)') ' ERROR   : valid  cell ', nele,&
                        & ' from dst grid '//trim(grid2_name)//' does not respect corner order '
                     ll_error = .true.
                  else
                     write(nulou,'(A,I7,A)') ' WARNING : masked cell ', nele,&
                        & ' from dst grid '//trim(grid2_name)//' does not respect corner order '
                  end if
                  write(nulou,'(A,2F9.2)') '         grid_center lon and lat (deg) ',&
                     &                     grid2_center_lon(nele)/deg2rad,&
                     &                     grid2_center_lat(nele)/deg2rad
                  do next_n = 1,ncrn_dst
                     write(nulou,'(A,I3,A,2F9.2)') '         grid_corner ',next_n,' lon and lat (deg) ',&
                        &                          grid2_corner_lon(next_n,nele)/deg2rad,&
                        &                          grid2_corner_lat(next_n,nele)/deg2rad
                  end do
                  exit corner_loop2
               end if

               if (cross_product_ctr < zero) then
                  if (abs(grid2_center_lat(nele)) .ge. 1.48) then
                     if (grid2_mask(nele)) then
                        write(nulou,'(A,I7,A)') ' WARNING : valid  polar cell ', nele,'&
                           & from dst grid '//trim(grid2_name)//' could lead to'
                     else
                        write(nulou,'(A,I7,A)') ' WARNING : masked polar cell ', nele,'&
                           & from dst grid '//trim(grid2_name)//' could lead to'
                     end if
                     write(nulou,'(2A)') '           a loss in precision if',&
                        & ' a polar projection is not active in remap_conserv'
                  else
                     if (grid2_mask(nele)) then
                        write(nulou,'(A,I7,A)') ' WARNING : valid  cell ', nele,'&
                           & from dst grid '//trim(grid2_name)//' does not contain its center'
                     else
                        write(nulou,'(A,I7,A)') ' WARNING : masked cell ', nele,'&
                           & from dst grid '//trim(grid2_name)//' does not contain its center'
                     end if
                  end if
                  write(nulou,'(A,2F9.2)') '         grid_center lon and lat (deg) ',&
                     &                     grid2_center_lon(nele)/deg2rad,&
                     &                     grid2_center_lat(nele)/deg2rad
                  do next_n = 1,ncrn_dst
                     write(nulou,'(A,I3,A,2F9.2)') '         grid_corner ',next_n,' lon and lat (deg) ',&
                        &                          grid2_corner_lon(next_n,nele)/deg2rad,&
                        &                          grid2_corner_lat(next_n,nele)/deg2rad
                  end do
                  exit corner_loop2
               end if

            end do corner_loop2

         end do

         if (ll_error) stop('Wrong corners order')

      end if

!-----------------------------------------------------------------------
!
!     compute bounding boxes for restricting future grid searches
!
!-----------------------------------------------------------------------

      grid1_bbox_per(:) = 0
      grid2_bbox_per(:) = 0

      if (.not. luse_grid_centers) then

        grid1_bound_box(1,:) = minval(grid1_corner_lat, DIM=1)
        grid1_bound_box(2,:) = maxval(grid1_corner_lat, DIM=1)
        grid1_bound_box(3,:) = minval(grid1_corner_lon, DIM=1)
        grid1_bound_box(4,:) = maxval(grid1_corner_lon, DIM=1)

        grid2_bound_box(1,:) = minval(grid2_corner_lat, DIM=1)
        grid2_bound_box(2,:) = maxval(grid2_corner_lat, DIM=1)
        grid2_bound_box(3,:) = minval(grid2_corner_lon, DIM=1)
        grid2_bound_box(4,:) = maxval(grid2_corner_lon, DIM=1)
      else

        nx = grid1_dims(1)
        ny = grid1_dims(2)

        do n=1,grid1_size

          !*** find N,S and NE points to this grid point

          j = (n - 1)/nx +1
          i = n - (j-1)*nx

          if (i < nx) then
            ip1 = i + 1
          else
            !*** assume cyclic
            ip1 = 1
            !*** but if it is not, correct
            e_add = (j - 1)*nx + ip1
            if (abs(grid1_center_lat(e_add) -  &
                    grid1_center_lat(n   )) > pih) then
              ip1 = i
            endif
          endif

          if (j < ny) then
            jp1 = j+1
          else
            !*** assume cyclic
            jp1 = 1
            !*** but if it is not, correct
            n_add = (jp1 - 1)*nx + i
            if (abs(grid1_center_lat(n_add) -  &
                    grid1_center_lat(n   )) > pih) then
              jp1 = j
            endif
          endif

          n_add = (jp1 - 1)*nx + i
          e_add = (j - 1)*nx + ip1
          ne_add = (jp1 - 1)*nx + ip1

          !*** find N,S and NE lat/lon coords and check bounding box

          tmp_lats(1) = grid1_center_lat(n)
          tmp_lats(2) = grid1_center_lat(e_add)
          tmp_lats(3) = grid1_center_lat(ne_add)
          tmp_lats(4) = grid1_center_lat(n_add)

          tmp_lons(1) = grid1_center_lon(n)
          tmp_lons(2) = grid1_center_lon(e_add)
          tmp_lons(3) = grid1_center_lon(ne_add)
          tmp_lons(4) = grid1_center_lon(n_add)

          grid1_bound_box(1,n) = minval(tmp_lats)
          grid1_bound_box(2,n) = maxval(tmp_lats)
          grid1_bound_box(3,n) = minval(tmp_lons)
          grid1_bound_box(4,n) = maxval(tmp_lons)
          if (abs(grid1_bound_box(4,n) - grid1_bound_box(3,n)) > pi - 20*epsilon(1.)) then
             grid1_bound_box(3,n) = maxval(tmp_lons)
             grid1_bound_box(4,n) = minval(tmp_lons)
             grid1_bbox_per(n) = 1
          end if
        end do

        nx = grid2_dims(1)
        ny = grid2_dims(2)

        do n=1,grid2_size

          !*** find N,S and NE points to this grid point

          j = (n - 1)/nx +1
          i = n - (j-1)*nx

          if (i < nx) then
            ip1 = i + 1
          else
            !*** assume cyclic
            ip1 = 1
            !*** but if it is not, correct
            e_add = (j - 1)*nx + ip1
            if (abs(grid2_center_lat(e_add) - &
                    grid2_center_lat(n   )) > pih) then
              ip1 = i
            endif
          endif

          if (j < ny) then
            jp1 = j+1
          else
            !*** assume cyclic
            jp1 = 1
            !*** but if it is not, correct
            n_add = (jp1 - 1)*nx + i
            if (abs(grid2_center_lat(n_add) -  &
                    grid2_center_lat(n   )) > pih) then
              jp1 = j
            endif
          endif

          n_add = (jp1 - 1)*nx + i
          e_add = (j - 1)*nx + ip1
          ne_add = (jp1 - 1)*nx + ip1

          !*** find N,S and NE lat/lon coords and check bounding box

          tmp_lats(1) = grid2_center_lat(n)
          tmp_lats(2) = grid2_center_lat(e_add)
          tmp_lats(3) = grid2_center_lat(ne_add)
          tmp_lats(4) = grid2_center_lat(n_add)

          tmp_lons(1) = grid2_center_lon(n)
          tmp_lons(2) = grid2_center_lon(e_add)
          tmp_lons(3) = grid2_center_lon(ne_add)
          tmp_lons(4) = grid2_center_lon(n_add)

          grid2_bound_box(1,n) = minval(tmp_lats)
          grid2_bound_box(2,n) = maxval(tmp_lats)
          grid2_bound_box(3,n) = minval(tmp_lons)
          grid2_bound_box(4,n) = maxval(tmp_lons)
          if (abs(grid2_bound_box(4,n) - grid2_bound_box(3,n)) > pi - 20*epsilon(1.)) then
             grid2_bound_box(3,n) = maxval(tmp_lons)
             grid2_bound_box(4,n) = minval(tmp_lons)
             grid2_bbox_per(n) = 1
          end if
        end do

      endif

      do n = 1,grid1_size
         if (grid1_bbox_per(n) == 0 .and. &
            & abs(grid1_bound_box(4,n) - grid1_bound_box(3,n)) > pi - 20*epsilon(1.)) then
            grid1_bound_box(3,n) = zero
            grid1_bound_box(4,n) = pi2
            if (grid1_center_lat(n) > pi/3.)   grid1_bound_box(2,n) = pih
            if (grid1_center_lat(n) < - pi/3.) grid1_bound_box(1,n) = -pih
         end if
      end do

      do n = 1,grid2_size
         if (grid2_bbox_per(n) == 0 .and. &
            & abs(grid2_bound_box(4,n) - grid2_bound_box(3,n)) > pi - 20*epsilon(1.)) then
            grid2_bound_box(3,n) = zero
            grid2_bound_box(4,n) = pi2
            if (grid2_center_lat(n) > pi/3.)   grid2_bound_box(2,n) = pih
            if (grid2_center_lat(n) < - pi/3.) grid2_bound_box(1,n) = -pih
         end if
      end do

      where (grid1_bound_box(3,:) < 0)   grid1_bbox_per(:) = -1
      where (grid1_bound_box(4,:) > pi2) grid1_bbox_per(:) =  1
      where (grid2_bound_box(3,:) < 0)   grid2_bbox_per(:) = -1
      where (grid2_bound_box(4,:) > pi2) grid2_bbox_per(:) =  1

      !***
      !*** try to check for cells that overlap poles or do not encompass centers
      !***

      do n = 1, grid1_size
         if (grid1_center_lat(n) > grid1_bound_box(2,n)) then
            if (grid1_center_lat(n) > pi/3.) then
               grid1_bound_box(2,n) = pih
            else
               grid1_bound_box(2,n) = grid1_center_lat(n)
            end if
         end if
      end do

      do n = 1, grid1_size
         if (grid1_center_lat(n) < grid1_bound_box(1,n)) then
            if (grid1_center_lat(n) < - pi/3.) then
               grid1_bound_box(1,n) = -pih
            else
               grid1_bound_box(1,n) = grid1_center_lat(n)
            end if
         end if

      end do

      do n = 1, grid2_size
         if (grid2_center_lat(n) > grid2_bound_box(2,n)) then
            if (grid2_center_lat(n) > pi/3.) then
               grid2_bound_box(2,n) = pih
            else
               grid2_bound_box(2,n) = grid2_center_lat(n)
            end if
         end if
      end do

      do n = 1, grid2_size
         if (grid2_center_lat(n) < grid2_bound_box(1,n)) then
            IF (grid2_center_lat(n) < - pi/3.) THEN
               grid2_bound_box(1,n) = -pih
            else
               grid2_bound_box(1,n) = grid2_center_lat(n)
            end if
         end if
      end do

!-----------------------------------------------------------------------
!
!     convert corner longitudes to 0,2pi interval (to be compliant with
!     old grids.f90 arithmetics. This section could be dropped.
!
!-----------------------------------------------------------------------

     if (.not. luse_grid_centers) then
        where (grid1_corner_lon .gt. pi2)  grid1_corner_lon = grid1_corner_lon - pi2
        where (grid1_corner_lon .lt. zero) grid1_corner_lon = grid1_corner_lon + pi2
        where (grid2_corner_lon .gt. pi2)  grid2_corner_lon = grid2_corner_lon - pi2
        where (grid2_corner_lon .lt. zero) grid2_corner_lon = grid2_corner_lon + pi2
     endif

!-----------------------------------------------------------------------
!
!     set up and assign address ranges to search bins in order to 
!     further restrict later searches
!
!-----------------------------------------------------------------------

      select case (restrict_type)

      case ('LATITUDE')
        IF (nlogprt .GE. 2) &
          write(nulou,*) 'Using latitude bins to restrict search.'

        allocate(bin_addr1(2,num_srch_bins))
        allocate(bin_addr2(2,num_srch_bins))
        allocate(bin_lats (2,num_srch_bins))
        allocate(bin_lons (2,num_srch_bins))

        dlat = pi/num_srch_bins

        do n=1,num_srch_bins
          bin_lats(1,n) = (n-1)*dlat - pih
          bin_lats(2,n) =     n*dlat - pih
          bin_lons(1,n) = zero
          bin_lons(2,n) = pi2
          bin_addr1(1,n) = grid1_size + 1
          bin_addr1(2,n) = 0
          bin_addr2(1,n) = grid2_size + 1
          bin_addr2(2,n) = 0
        end do

        do nele=1,grid1_size
          do n=1,num_srch_bins
            if (grid1_bound_box(1,nele) <= bin_lats(2,n) .and.  &
                grid1_bound_box(2,nele) >= bin_lats(1,n)) then
              bin_addr1(1,n) = min(nele,bin_addr1(1,n))
              bin_addr1(2,n) = max(nele,bin_addr1(2,n))
            endif
          end do
        end do

        do nele=1,grid2_size
          do n=1,num_srch_bins
            if (grid2_bound_box(1,nele) <= bin_lats(2,n) .and.  &
                grid2_bound_box(2,nele) >= bin_lats(1,n)) then
              bin_addr2(1,n) = min(nele,bin_addr2(1,n))
              bin_addr2(2,n) = max(nele,bin_addr2(2,n))
            endif
          end do
        end do

      case ('LATLON')
        IF (nlogprt .GE. 2) &
          write(nulou,*) 'Using lat/lon boxes to restrict search.'

        dlat = pi /num_srch_bins
        dlon = pi2/num_srch_bins

        allocate(bin_addr1(2,num_srch_bins*num_srch_bins))
        allocate(bin_addr2(2,num_srch_bins*num_srch_bins))
        allocate(bin_lats (2,num_srch_bins*num_srch_bins))
        allocate(bin_lons (2,num_srch_bins*num_srch_bins))

        n = 0
        do j=1,num_srch_bins
          do i=1,num_srch_bins
            n = n + 1
            
            bin_lats(1,n) = (j-1)*dlat - pih
            bin_lats(2,n) =     j*dlat - pih
            bin_lons(1,n) = (i-1)*dlon
            bin_lons(2,n) =     i*dlon
            bin_addr1(1,n) = grid1_size + 1
            bin_addr1(2,n) = 0
            bin_addr2(1,n) = grid2_size + 1
            bin_addr2(2,n) = 0
          end do
        end do
        
        num_srch_bins = num_srch_bins**2

        do nele=1,grid1_size
          do n=1,num_srch_bins
            if (grid1_bound_box(1,nele) <= bin_lats(2,n) .and. &
                grid1_bound_box(2,nele) >= bin_lats(1,n) .and. &
                grid1_bound_box(3,nele) <= bin_lons(2,n) .and. &
                grid1_bound_box(4,nele) >= bin_lons(1,n)) then
                bin_addr1(1,n) = min(nele,bin_addr1(1,n))
                bin_addr1(2,n) = max(nele,bin_addr1(2,n))
            endif
          end do
        end do

        do nele=1,grid2_size
          do n=1,num_srch_bins
            if (grid2_bound_box(1,nele) <= bin_lats(2,n) .and. &
                grid2_bound_box(2,nele) >= bin_lats(1,n) .and. &
                grid2_bound_box(3,nele) <= bin_lons(2,n) .and. &
                grid2_bound_box(4,nele) >= bin_lons(1,n)) then
                bin_addr2(1,n) = min(nele,bin_addr2(1,n))
                bin_addr2(2,n) = max(nele,bin_addr2(2,n))
            endif
          end do
        end do

      case ('REDUCED')
!EM        write(nulou,*) '|  Using reduced bins to restrict search. Reduced grids with'
!EM        write(nulou,*) '| a maximum of 500*NBRBINS latitude circles can be treated'

        allocate(bin_addr2(2,num_srch_bins))
        allocate(bin_lats (2,num_srch_bins))
        allocate(bin_lons (2,num_srch_bins))

        dlat = pi/num_srch_bins

        do n=1,num_srch_bins
          bin_lats(1,n) = (n-1)*dlat - pih
          bin_lats(2,n) =     n*dlat - pih
          bin_lons(1,n) = zero
          bin_lons(2,n) = pi2
          bin_addr2(1,n) = grid2_size + 1
          bin_addr2(2,n) = 0
        end do

        do nele=1,grid2_size
          do n=1,num_srch_bins
            if (grid2_bound_box(1,nele) <= bin_lats(2,n) .and. &
                grid2_bound_box(2,nele) >= bin_lats(1,n)) then
              bin_addr2(1,n) = min(nele,bin_addr2(1,n))
              bin_addr2(2,n) = max(nele,bin_addr2(2,n))
            endif
          end do
        end DO

        ! Count number of search bins
        num_srch_red = 1
        do nele=1,grid1_size-1
          IF (grid1_center_lat(nele+1) /=  grid1_center_lat(nele)) &
            num_srch_red = num_srch_red+1
        end DO

        IF (nlogprt .GE. 2) &
          write(nulou,*) 'Number of automatic search bins : ', num_srch_red

        allocate(bin_addr1_r(4,num_srch_red))
        allocate(bin_lats_r (2,num_srch_red))


        bin_addr1_r(1,1) = 0
        bin_lats_r(1,1) = grid1_center_lat(1)
        num_srch_red = 1
        do nele=1,grid1_size-1
          if (grid1_center_lat(nele+1) ==  grid1_center_lat(nele)) THEN
              bin_addr1_r(2,num_srch_red) = nele+1
          !EM why not simply : else ?
          else IF (grid1_center_lat(nele+1) /=  grid1_center_lat(nele)) THEN 
              bin_addr1_r(1,num_srch_red+1) = nele+1
              bin_lats_r(2,num_srch_red) = grid1_center_lat(nele+1)
              bin_lats_r(1,num_srch_red+1) = bin_lats_r(2,num_srch_red)
              num_srch_red = num_srch_red+1
          ENDIF
        end DO
        
        DO nele = 1,num_srch_red-1
          bin_addr1_r(3,nele)=bin_addr1_r(1,nele+1)
          bin_addr1_r(4,nele)=bin_addr1_r(2,nele+1)
        enddo
        bin_addr1_r(3,num_srch_red) = bin_addr1_r(1,num_srch_red-1)
        bin_addr1_r(4,num_srch_red) = bin_addr1_r(2,num_srch_red-1)

      case default
        stop 'unknown search restriction method'
      end select
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine grid_init'
         WRITE (UNIT = nulou,FMT = *)' '
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
! 
!-----------------------------------------------------------------------

      end subroutine grid_init

!***********************************************************************

      subroutine free_grids

!-----------------------------------------------------------------------
!
!     subroutine to deallocate allocated arrays
!
!----------------------------------------------------------------------- 
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine free_grid'
         WRITE (UNIT = nulou,FMT = *)' '
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
! 
      deallocate(grid1_mask, grid2_mask,              &
                 grid1_center_lat, grid1_center_lon,  &
                 grid2_center_lat, grid2_center_lon,  &
                 grid1_area, grid2_area,              &
                 grid1_frac, grid2_frac,              &
                 grid1_dims, grid2_dims)

      IF (restrict_TYPE == 'REDUCED') then
          deallocate( grid1_bound_box, grid2_bound_box,         &
                      grid1_bbox_per,  grid2_bbox_per,          &
                      bin_addr1_r, bin_addr2,                   &
                      bin_lons, bin_lats,                       &
                      bin_lats_r)
      else
          deallocate( grid1_bound_box, grid2_bound_box,         &
                      grid1_bbox_per,  grid2_bbox_per,          &
                      bin_addr1, bin_addr2,                     &
                      bin_lats, bin_lons)
      endif

      if (.not. luse_grid_centers) then
        deallocate(grid1_corner_lat, grid1_corner_lon, &
                   grid2_corner_lat, grid2_corner_lon)
      ENDIF
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine free_grid'
         WRITE (UNIT = nulou,FMT = *)' '
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!-----------------------------------------------------------------------

      end subroutine free_grids

!***********************************************************************

      end module grids

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

