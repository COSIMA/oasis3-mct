!****
!                    ************************
!                    *     OASIS MODULE     *
!                    *     ------------     *
!                    ************************
!**** 
!***********************************************************************
!     This module belongs to the SCRIP library. It is modified to run
!     within OASIS. 
!     Main modifications:
!       - Some allocated array will be freed in the end to allow multiple
!         calls of SCRIP
!       - Introduction of a logical flag to distinguish between the first
!         and following calls of scrip conservative remapping
!       - Masking of overlapping grid points for source and target grid
!         For these points, links and weights of overlapped point are used
!
!     Modified by            V. Gayler,  M&D                  20.09.2001
!     Modified by            D. Declat,  CERFACS              27.06.2002
!     Modified by            A. Piacentini, CERFACS           24.01.2018
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     this module contains necessary routines for computing addresses
!     and weights for a conservative interpolation  between any two 
!     grids on a sphere.  the weights are computed by performing line 
!     integrals around all overlap regions of the two grids.  see 
!     Dukowicz and Kodis, SIAM J. Sci. Stat. Comput. 8, 305 (1987) and
!     Jones, P.W. Monthly Weather Review (submitted).
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: remap_conserv.f 1811 2008-12-19 08:41:41Z valcke $
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

module remap_conservative

   !-----------------------------------------------------------------------

   use kinds_mod    ! defines common data types
   use constants    ! defines common constants
   use timers       ! module for timing
   use grids        ! module containing grid information
   use remap_vars   ! module containing remap information
   use mod_oasis_flush
!$ use omp_lib

   implicit none

   !-----------------------------------------------------------------------
   !
   !     module variables
   !
   !-----------------------------------------------------------------------

   real (kind=dbl_kind), parameter :: north_thresh = 2.00_dbl_kind, & ! threshold for coord transf.
      south_thresh =-2.00_dbl_kind  ! threshold for coord transf.

   integer (kind=int_kind) :: il_nbthreads = 1

   !***********************************************************************

contains

   !***********************************************************************

   subroutine remap_conserv(mpi_comm_map, mpi_size_map, mpi_rank_map, mpi_root_map)

      !-----------------------------------------------------------------------
      !
      !     this routine traces the perimeters of every grid cell on each
      !     grid checking for intersections with the other grid and computing
      !     line integrals for each subsegment.
      !
      !-----------------------------------------------------------------------

      !-----------------------------------------------------------------------
      !
      !     input variables
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind) :: mpi_comm_map, mpi_rank_map, mpi_size_map, mpi_root_map

      !-----------------------------------------------------------------------
      !
      !     local variables
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind), parameter :: max_subseg = 10000 ! max number of subsegments per segment
      ! to prevent infinite loop

      integer (kind=int_kind) :: num_srch_cells ! num cells in restricted search arrays

      integer (kind=int_kind), dimension(:), allocatable :: srch_add  ! global address of cells in srch arrays

      integer (kind=int_kind) :: grid1_add,  & ! current linear address for grid1 cell
         grid2_add,  & ! current linear address for grid2 cell
         min_add,    & ! addresses for restricting search of
         max_add,    & !   destination grid
         min_cell,    & ! addresses for restricting search of
         max_cell,    & !   destination grid
         n, nwgt,    & ! generic counters
         corner,     & ! corner of cell that segment starts from
         next_corn,  & ! corner of cell that segment ends on
         num_subseg    ! number of subsegments 

      logical (kind=log_kind) :: lcoinc,  & ! flag for coincident segments
         lrevers, & ! flag for reversing direction of segment
         lbegin,  & ! flag for first integration of a segment
         ltake,   & ! flag for longitude intersection
         full, ll_debug ! for debug outputs


      real (kind=dbl_kind) ::  &
         intrsct_lat, intrsct_lon,       & ! lat/lon of next intersect
         beglat, endlat, beglon, endlon, & ! endpoints of current seg.
         norm_factor,                    & ! factor for normalizing wts
         delta,                          & ! precision
         r2d

      real (kind=dbl_kind) :: dgbb1, dgbb2, dgbb3, dgbb4
      integer (kind=int_kind) :: dgbbp
 
      integer (kind=int_kind) :: num_links_map1_sweep1

      real (kind=dbl_kind), dimension(:), allocatable :: grid2_centroid_lat, grid2_centroid_lon, & ! centroid coords
         grid1_centroid_lat, grid1_centroid_lon    ! on each grid

      real (kind=dbl_kind), dimension(2) :: begseg ! begin lat/lon for
      ! full segment

      real (kind=dbl_kind), dimension(6) :: weights ! local wgt array
      real (kind=dbl_kind) :: intrsct_lat_off, intrsct_lon_off ! lat/lon coords offset 

      logical :: ll_timing=.true.

      integer (kind=int_kind) :: il_splitsize
      integer (kind=int_kind) :: ib_proc
      integer (kind=int_kind) :: ib_thread
      integer (kind=int_kind) :: buff_base
      integer (kind=int_kind), dimension(:), allocatable :: ila_mpi_sz
      integer (kind=int_kind), dimension(:), allocatable :: ila_mpi_mn
      integer (kind=int_kind), dimension(:), allocatable :: ila_mpi_mx
      integer (kind=int_kind), dimension(:), allocatable :: ila_thr_sz
      integer (kind=int_kind), dimension(:), allocatable :: ila_thr_mn
      integer (kind=int_kind), dimension(:), allocatable :: ila_thr_mx
      integer (kind=int_kind), dimension(:), allocatable :: ila_num_links_mpi
      integer (kind=int_kind), dimension(:,:), allocatable :: ila_req_mpi
      integer (kind=int_kind), dimension(:,:,:), allocatable :: ila_sta_mpi
      character (LEN=14) :: cl_envvar
      integer (kind=int_kind) :: il_envthreads, il_err, est_num_neighbors

#ifdef TREAT_OVERLAY
      integer (kind=int_kind) :: cell
      integer (kind=int_kind), dimension(:), allocatable :: ila_idx
      integer (kind=int_kind), dimension(:), allocatable :: grid2_overlap ! overlapping points
#endif

      !-----------------------------------------------------------------------
      !
      if (nlogprt .ge. 2) then
         write (UNIT = nulou,FMT = *)' '
         write (UNIT = nulou,FMT = *)'Entering routine remap_conserv'
         call OASIS_FLUSH_SCRIP(nulou)
      endif

      weights = 0.

      est_num_neighbors = 4
      !
      !-----------------------------------------------------------------------
      !
      !     initialize centroid arrays
      !
      !-----------------------------------------------------------------------

      allocate( grid1_centroid_lat(grid1_size), grid1_centroid_lon(grid1_size), &
         grid2_centroid_lat(grid2_size), grid2_centroid_lon(grid2_size))

      grid1_centroid_lat = zero
      grid1_centroid_lon = zero
      grid2_centroid_lat = zero
      grid2_centroid_lon = zero

      !-----------------------------------------------------------------------
      !
      !     integrate around each cell on grid1
      !
      !-----------------------------------------------------------------------

#ifdef TREAT_OVERLAY

      if (ll_timing) call timer_start(2,'remap_conserv overlay')

      ! Coordinate equality tolerance

      delta = epsilon(1.)

      ! Check overlapping point of the source grid
      
      if (nlogprt .ge. 2) then      
         write(nulou,*) 'Check overlapping point of the source grid'
         call OASIS_FLUSH_SCRIP(nulou)
      endif

      ! Initialise array that contains addresses of overlap grid point

      do grid1_add = 1,grid1_size
         grid1_add_repl1(grid1_add)=grid1_add
      end do

      allocate(ila_idx(grid1_size))

      ila_idx(:) = [(n,n=1,grid1_size)]

      ! Sort the source grid indexes by lon (varying first) and lat

      call hpsort_eps (grid1_size, grid1_center_lon, grid1_center_lat, ila_idx, delta)

      ! Check neighbours equality (with tolerance)

      do cell = 1, grid1_size-1
         n = max(ila_idx(cell),ila_idx(cell+1))
         grid1_add = min(ila_idx(cell),ila_idx(cell+1))
         if ( (abs(grid1_center_lon(grid1_add)- grid1_center_lon(n))<delta).and. &
            (abs(grid1_center_lat(grid1_add)- grid1_center_lat(n))<delta)) then
            if (grid1_mask(n) .or. grid1_mask(grid1_add)) then
               if (grid1_mask(n)) then
                  grid1_mask(n) = .false.
                  grid1_mask(grid1_add) = .true.
                  grid1_add_repl1(grid1_add) = n
               endif
            endif
         end if
      end do

      deallocate(ila_idx)

      ! Check overlapping point of the target grid
      
      if (nlogprt .ge. 2) then      
         write(nulou,*) 'Check overlapping point of the target grid'
         call OASIS_FLUSH_SCRIP(nulou)
      endif

      ! Initialise array that contains addresses of overlap grid point

      allocate(grid2_overlap(grid2_size))
      grid2_overlap = -1

      allocate(ila_idx(grid2_size))

      ila_idx(:) = [(n,n=1,grid2_size)]

      ! Sort the destination grid indexes by lon (varying first) and lat

      call hpsort_eps (grid2_size, grid2_center_lon, grid2_center_lat, ila_idx, delta)

      ! Check neighbours equality (with tolerance)

      do cell = 1, grid2_size-1
         n = max(ila_idx(cell),ila_idx(cell+1))
         grid2_add = min(ila_idx(cell),ila_idx(cell+1))
         if ( (abs(grid2_center_lon(grid2_add)- grid2_center_lon(n))<delta).and.  &
            (abs(grid2_center_lat(grid2_add)- grid2_center_lat(n))<delta)) then
            grid2_overlap(grid2_add) = n
         end if
      end do

      deallocate(ila_idx)

      if (ll_timing) call timer_stop(2)

#endif TREAT_OVERLAY

      allocate(ila_mpi_mn(mpi_size_map), ila_mpi_mx(mpi_size_map) )

      if (mpi_size_map .gt. 1) then

         allocate(ila_mpi_sz(mpi_size_map))
         il_splitsize = grid1_size
         ila_mpi_sz(:) = floor(real(il_splitsize)/mpi_size_map)
         ila_mpi_sz(1:il_splitsize-sum(ila_mpi_sz)) = ila_mpi_sz(1:il_splitsize-sum(ila_mpi_sz)) + 1

         ila_mpi_mn(1) = 1
         ila_mpi_mx(1) = ila_mpi_sz(1)

         do ib_proc = 2, mpi_size_map
            ila_mpi_mn(ib_proc) = sum(ila_mpi_sz(1:ib_proc-1)) + 1
            ila_mpi_mx(ib_proc) = sum(ila_mpi_sz(1:ib_proc))
         end do

         deallocate(ila_mpi_sz)

      else

         ila_mpi_mn(1) = 1
         ila_mpi_mx(1) = grid1_size

      endif


      call get_environment_variable(name='OASIS_OMP_NUM_THREADS', value=cl_envvar, status=il_err)

      if ( il_err .ne. 0) then
         il_envthreads = 0
      else
         read(cl_envvar,*) il_envthreads
      end if

      if (ll_timing) call timer_start(3,'remap_conserv grid1 sweep')

!$OMP PARALLEL NUM_THREADS(il_envthreads) DEFAULT(NONE) &
!$OMP SHARED(il_envthreads) &
!$OMP SHARED(num_wts) &
#ifdef TREAT_OVERLAY
!$OMP SHARED(grid2_overlap) &
#endif

!$OMP SHARED(grid1_corners,grid2_corners) &
!$OMP SHARED(grid1_bound_box,grid2_bound_box) &
!$OMP SHARED(grid1_bbox_per,grid2_bbox_per) &
!$OMP SHARED(grid2_size,bin_addr1,bin_addr2) &
!$OMP SHARED(est_num_neighbors,num_srch_bins) &
!$OMP SHARED(il_nbthreads) &
!$OMP SHARED(nulou,sga_remap) &
!$OMP SHARED(grid2_center_lat) &
!$OMP SHARED(grid1_center_lat,grid1_center_lon) &
!$OMP SHARED(grid2_center_lon) &
!$OMP SHARED(grid1_corner_lat,grid1_corner_lon) &
!$OMP SHARED(grid2_corner_lat,grid2_corner_lon) &
!$OMP SHARED(grid1_mask) &
!$OMP SHARED(mpi_rank_map,mpi_root_map,ila_mpi_mn,ila_mpi_mx) &
!$OMP SHARED(ila_thr_sz,ila_thr_mn,ila_thr_mx) &
!$OMP REDUCTION(+:grid2_frac) &
!!$OMP SHARED(grid2_frac) &
!$OMP SHARED(grid1_area) &
!$OMP SHARED(grid1_centroid_lat) &
!$OMP SHARED(grid1_centroid_lon) &
!$OMP PRIVATE(nlogprt) &
!$OMP PRIVATE(grid1_add,n,srch_add,corner) &
!$OMP PRIVATE(dgbb1,dgbb2,dgbb3,dgbb4,dgbbp) &
!$OMP PRIVATE(min_add,max_add) &
!$OMP PRIVATE(min_cell,max_cell,grid2_add,num_srch_cells) &
!$OMP PRIVATE(next_corn,beglat,beglon,endlat,endlon,lrevers) &
!$OMP PRIVATE(begseg,lbegin,num_subseg) &
!$OMP PRIVATE(intrsct_lat,intrsct_lon,lcoinc,weights,ll_debug) &
!$OMP PRIVATE(intrsct_lat_off,intrsct_lon_off) &
!$OMP PRIVATE(ib_thread,il_splitsize)

!$OMP SINGLE

      il_nbthreads = 1
!$    il_nbthreads = OMP_GET_NUM_THREADS ()

      allocate(ila_thr_mn(il_nbthreads))
      allocate(ila_thr_mx(il_nbthreads))

      if (il_nbthreads .gt. 1) then

         nlogprt = 0

         allocate(ila_thr_sz(il_nbthreads))
         il_splitsize = ila_mpi_mx(mpi_rank_map+1)-ila_mpi_mn(mpi_rank_map+1)+1
         ila_thr_sz(:) = floor(real(il_splitsize)/il_nbthreads)
         ila_thr_sz(1:il_splitsize-sum(ila_thr_sz)) = ila_thr_sz(1:il_splitsize-sum(ila_thr_sz)) + 1

         ila_thr_mn(1) = ila_mpi_mn(mpi_rank_map+1)
         ila_thr_mx(1) = ila_thr_mn(1) + ila_thr_sz(1) - 1

         do ib_thread = 2, il_nbthreads
            ila_thr_mn(ib_thread) = ila_mpi_mn(mpi_rank_map+1) + sum(ila_thr_sz(1:ib_thread-1))
            ila_thr_mx(ib_thread) = ila_thr_mn(ib_thread) + ila_thr_sz(ib_thread) - 1
         end do

         allocate(sga_remap(il_nbthreads))

         do ib_thread = 1, il_nbthreads
            il_splitsize = est_num_neighbors*ila_thr_sz(ib_thread)
            sga_remap(ib_thread)%max_links = il_splitsize
            sga_remap(ib_thread)%num_links = 0
            allocate(sga_remap(ib_thread)%grid1_add(il_splitsize))
            allocate(sga_remap(ib_thread)%grid2_add(il_splitsize))
            allocate(sga_remap(ib_thread)%wts(num_wts,il_splitsize))
         end do

         deallocate(ila_thr_sz)

      else

         ila_thr_mn(1) = ila_mpi_mn(mpi_rank_map+1)
         ila_thr_mx(1) = ila_mpi_mx(mpi_rank_map+1)

      end if
!$OMP END SINGLE

      ! Sweeps
      

      if (nlogprt .ge. 2) then
         write (nulou,*) 'grid1 sweep '
         call OASIS_FLUSH_SCRIP(nulou)
      endif

      !-----------------------------------------------------------------------
      !
      !     integrate around each cell on grid1
      !
      !-----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC,1)
      thread_loop1: do ib_thread = 1, il_nbthreads

       ll_debug = .false.

       grid_loop1: do grid1_add = ila_thr_mn(ib_thread), ila_thr_mx(ib_thread)

         if (ll_debug) then 
            write(nulou,*) 'grid1_add=', grid1_add
            call OASIS_FLUSH_SCRIP(nulou)
         endif

         !***
         !*** destination grid bounding box (preloaded)
         !***

         dgbb1 = grid1_bound_box(1,grid1_add)
         dgbb2 = grid1_bound_box(2,grid1_add)
         dgbb3 = grid1_bound_box(3,grid1_add)
         dgbb4 = grid1_bound_box(4,grid1_add)
         dgbbp = grid1_bbox_per(grid1_add)

         !***
         !*** restrict searches first using search bins
         !***

         min_add = grid2_size
         max_add = 1
         do n=1,num_srch_bins
            if (grid1_add >= bin_addr1(1,n) .and. grid1_add <= bin_addr1(2,n)) then
               min_add = min(min_add, bin_addr2(1,n))
               max_add = max(max_add, bin_addr2(2,n))
            endif
         end do

         !***
         !*** select the search cells
         !***

         num_srch_cells = 0
         min_cell = max_add
         max_cell = min_add

         gather1: DO grid2_add = min_add, max_add

            !***
            !*** further restrict searches using bounding boxes
            !***

            if (          grid2_bound_box(1,grid2_add) <= dgbb2 ) then
               if (       grid2_bound_box(2,grid2_add) >= dgbb1 ) then
                  if (    lf_loncross(dgbbp,dgbb3,dgbb4,grid2_add,&
                                      grid2_bbox_per,grid2_bound_box) ) then
                     
                     num_srch_cells = num_srch_cells+1
                     min_cell = MIN(min_cell, grid2_add)
                     max_cell = MAX(max_cell, grid2_add)

                  end if
               end if
            end if

         end do gather1

         !***
         !*** store the search cells array
         !***

         if (num_srch_cells .ne. 0) then

            allocate(srch_add(num_srch_cells))

            n=0
            pick1: DO grid2_add = min_cell,max_cell

            if (          grid2_bound_box(1,grid2_add) <= dgbb2 ) then
               if (       grid2_bound_box(2,grid2_add) >= dgbb1 ) then
                  if (    lf_loncross(dgbbp,dgbb3,dgbb4,grid2_add,&
                                      grid2_bbox_per,grid2_bound_box) ) then

                     n = n+1
                     srch_add(n) = grid2_add

                     end if
                  end if
               end if

            end do pick1

         end if

         if (ll_debug) then 
            write(nulou,*)'    '
            write(nulou,*)'  ** Grid1 cell and associated search cells **'
            write(nulou,1110) grid1_add, grid1_center_lon(grid1_add), grid1_center_lat(grid1_add)
            DO corner = 1,grid1_corners 
               write(nulou,1111) corner, grid1_corner_lon(corner,grid1_add), grid1_corner_lat(corner,grid1_add)
            enddo
            write(nulou,1969) grid1_add, grid1_bound_box(1,grid1_add), grid1_bound_box(2,grid1_add)
            write(nulou,1971) grid1_add, grid1_bound_box(3,grid1_add), grid1_bound_box(4,grid1_add)
            write(nulou,*) '  num_srch_cells=', num_srch_cells
            write(nulou,*) '    '
            if (num_srch_cells .ne. 0) then
               write(nulou,*) '  srch_add(:)=', srch_add(1:num_srch_cells)
               do n=1, num_srch_cells
                  do corner = 1,grid2_corners
                     write(nulou,1112) n, grid2_corner_lon(corner,srch_add(n)), grid2_corner_lat(corner,srch_add(n))
                  end do
               end do
            end if
            write(nulou,*)'    ***************************************'
            write(nulou,*)'    '
            call OASIS_FLUSH_SCRIP(nulou)
         endif
1110     format ('   grid1 center, lon, lat = ', I8, 2X, F12.4, 2X, F12.4)
1111     format ('   grid1 corner, lon, lat = ', I8, 2X, F12.4, 2X, F12.4)
1112     format ('     srch cell, lon, lat = ', I8, 2X, F12.4, 2X, F12.4)       
1969     format ('   grid1 index,  bbox lat = ', I8, 2X, F12.4, 2X, F12.4)       
1971     format ('   grid1 index,  bbox lon = ', I8, 2X, F12.4, 2X, F12.4)       

         if (num_srch_cells .ne. 0) then

            !***
            !*** integrate around this cell
            !***

            do corner = 1,grid1_corners
               next_corn = mod(corner,grid1_corners) + 1

               !***
               !*** define endpoints of the current segment
               !***

               beglat = grid1_corner_lat(corner,grid1_add)
               beglon = grid1_corner_lon(corner,grid1_add)
               endlat = grid1_corner_lat(next_corn,grid1_add)
               endlon = grid1_corner_lon(next_corn,grid1_add)
               lrevers = .false.

               !***
               !*** to ensure exact path taken during both
               !*** sweeps, always integrate segments in the same 
               !*** direction (SW to NE).
               !***

               if ((endlat < beglat) .or. &
                  (endlat == beglat .and. endlon < beglon)) then 
                  beglat = grid1_corner_lat(next_corn,grid1_add)
                  beglon = grid1_corner_lon(next_corn,grid1_add)
                  endlat = grid1_corner_lat(corner,grid1_add)
                  endlon = grid1_corner_lon(corner,grid1_add)
                  lrevers = .true.
                  if (ll_debug) write(nulou, *) ' sweep1 LREVERS TRUE'
               endif
               begseg(1) = beglat
               begseg(2) = beglon
               lbegin = .true.
               num_subseg = 0

               !***
               !*** if this is a constant-longitude segment, skip the rest 
               !*** since the line integral contribution will be zero.
               !***
               if (ll_debug) then
                  if (endlon .eq. beglon) then
                     write(nulou,1113) beglon, beglat 
                     write(nulou,1114) endlon, endlat
                     write(nulou, *)'  sweep1 endlon == beglon; skip segment'
                     write(nulou,*) '             '
                  endif
               endif
1113           format ('   endlon == beglon;  beglon, beglat = ', 2X, F12.4, 2X, F12.4)
1114           format ('   endlon == beglon;  endlon, endlat = ', 2X, F12.4, 2X, F12.4)

               if (endlon /= beglon) then
                  !***
                  !*** integrate along this segment, detecting intersections 
                  !*** and computing the line integral for each sub-segment
                  !***

                  do while (beglat /= endlat .or. beglon /= endlon)
                     !***
                     !*** prevent infinite loops if integration gets stuck
                     !*** near cell or threshold boundary
                     !***

                     num_subseg = num_subseg + 1
                     if (num_subseg > max_subseg) then
                        write(nulou,*) 'ERROR=>integration stalled:' 
                        write(nulou,*) 'num_subseg exceeded limit'
                        write(nulou,*) '=>Verify corners in grids.nc, especially'
                        write(nulou,*) 'if calculated by OASIS routine corners' 
                        write(nulou,*) 'integration stalled: num_subseg exceeded limit'
                        call OASIS_FLUSH_SCRIP(nulou)
                        stop
                     endif

                     !***
                     !*** find next intersection of this segment with a grid
                     !*** line on grid 2.
                     !***

                     if (ll_debug) then
                        write(nulou,*) '             '
                        write(nulou,1115) beglon, beglat 
                        write(nulou,1116) endlon, endlat
                        write(nulou,*) '             '
                        call OASIS_FLUSH_SCRIP(nulou)
                     endif
1115                 format ('   avant intersection;  beglon, beglat = ', 2X, F12.4, 2X, F12.4)
1116                 format ('   avant intersection;  endlon, endlat = ', 2X, F12.4, 2X, F12.4)

                     call intersection(grid2_add,intrsct_lat,intrsct_lon,               &
                        lcoinc, beglat, beglon, endlat, endlon, begseg,  &
                        lbegin, lrevers,                                 &
                        grid2_corners, num_srch_cells,                   &
                        srch_add, grid2_corner_lon, grid2_corner_lat, &
                        intrsct_lat_off,intrsct_lon_off)

                     if (ll_debug) then
                        write(nulou,*) ' After call intersection, grid2_add', grid2_add
                        write(nulou,1117) beglon, beglat 
                        write(nulou,1118) endlon, endlat
                        write(nulou,1119) intrsct_lon, intrsct_lat
                        write(nulou,*) '   '
                        call OASIS_FLUSH_SCRIP(nulou)
                     endif
1117                 format('   après intersection;  beglon, beglat =           ', 2X, F12.4, 2X, F12.4)
1118                 format ('   après intersection;  endlon, endlat =          ', 2X, F12.4, 2X, F12.4)
1119                 format ('   après intersection; intrsct_lon, intrsct_lat = ', 2X, F12.4, 2X, F12.4)

                     lbegin = .false.

                     !***
                     !*** compute line integral for this subsegment.
                     !***

                     if (grid2_add /= 0) then
                        call line_integral(weights, num_wts,                         &
                           beglon, intrsct_lon, beglat, intrsct_lat, &
                           grid1_center_lon(grid1_add),              &
                           grid2_center_lon(grid2_add))

                        if (ll_debug) then
                           write(nulou,*) '  A1) WEIGHTS for this subsegment =', weights(1)
                           write(nulou,*) '     '
                           call OASIS_FLUSH_SCRIP(nulou)
                        endif

                     else
                        call line_integral(weights, num_wts,                         &
                           beglon, intrsct_lon, beglat, intrsct_lat, &
                           grid1_center_lon(grid1_add),              &
                           grid1_center_lon(grid1_add))

                        if (ll_debug) then
                           write(nulou,*) '  B1) WEIGHTS for this subsegment =', weights(1)
                           write(nulou,*) '     '
                           call OASIS_FLUSH_SCRIP(nulou)
                        endif

                     endif

                     !***
                     !*** if integrating in reverse order, change
                     !*** sign of weights
                     !***

                     if (lrevers) then
                        weights = -weights
                        if (ll_debug) then
                           write(nulou,*) '  LREVERS; WEIGHTS for this subsegment =', weights(1)
                           write(nulou,*) '     '
                        endif
                     endif


                     !***
                     !*** store the appropriate addresses and weights. 
                     !*** also add contributions to cell areas and centroids.
                     !***
1120                 FORMAT ('      STORE add1,add2,blon,blat,ilon,ilat,WEIGHTS=', 1X,I8,1X,I8,1X,F12.8,1X,F12.8,1X,F12.8,1X,F12.8,1X, E16.8)
1121                 FORMAT ('      overlap STORE grid1_add, grid2_add, WEIGHTS=', 1X,I8,1X,I8,1X,F12.8,1X,F12.8,1X,F12.8,1X,F12.8,1X, E16.8)
1122                 FORMAT ('      lfracnei STORE grid1_add, grid2_add, WEIGHTS=', 1X,I8,1X,I8,1X,F12.8,1X,F12.8,1X,F12.8,1X,F12.8,1X, E16.8)
                     if (grid2_add /= 0) then
                        if (grid1_mask(grid1_add)) then
                           call store_link_cnsrv(grid1_add, grid2_add, weights, ib_thread)

                           if (ll_debug) then
                              write(nulou,*) '      after store_link_cnsrv norm1'
                              write(nulou,1120) grid1_add, grid2_add,beglon, beglat, intrsct_lon,intrsct_lat,weights(1)
                           endif
#ifdef TREAT_OVERLAY
                           if (grid2_overlap(grid2_add)/=-1) then
                              call store_link_cnsrv(grid1_add, grid2_overlap(grid2_add), weights, ib_thread)

                              if (ll_debug) then
                                 write(nulou,*) '      after store_link_cnsrv overlap1'
                                 write(nulou,1121) grid1_add, grid2_add,beglon, beglat, intrsct_lon,intrsct_lat,weights(1)
                              endif
                           endif
#endif

!EM                           grid1_frac(grid1_add) = grid1_frac(grid1_add) + weights(1)
                           grid2_frac(grid2_add) = grid2_frac(grid2_add) + weights(num_wts+1)
#ifdef TREAT_OVERLAY
                           if (grid2_overlap(grid2_add)/=-1)            &
                              grid2_frac(grid2_overlap(grid2_add)) =   &
                              grid2_frac(grid2_overlap(grid2_add)) + weights(num_wts+1)
#endif
                        endif
                     endif

                     grid1_area(grid1_add) = grid1_area(grid1_add) + weights(1)
                     grid1_centroid_lat(grid1_add) = grid1_centroid_lat(grid1_add) + weights(2)
                     grid1_centroid_lon(grid1_add) = grid1_centroid_lon(grid1_add) + weights(3)

                     !***
                     !*** reset beglat and beglon for next subsegment.
                     !***

                     beglat = intrsct_lat
                     beglon = intrsct_lon

                  end do

               endif

               !***
               !*** end of segment
               !***

            end do

            !***
            !*** finished with this cell: deallocate search array and
            !*** start on next cell

            deallocate(srch_add)

         end if ! num_srch_cells .NE. 0

      end do grid_loop1

      end do thread_loop1
!$OMP END DO

!$OMP END PARALLEL

      if (il_nbthreads .gt. 1) then

         sga_remap(1)%start_pos = 1
         il_splitsize = sga_remap(1)%num_links
         do ib_thread = 2, il_nbthreads
            il_splitsize = il_splitsize + sga_remap(ib_thread)%num_links
            sga_remap(ib_thread)%start_pos = sga_remap(ib_thread-1)%start_pos + &
               sga_remap(ib_thread-1)%num_links
         end do

         num_links_map1 = il_splitsize
         if (num_links_map1 > max_links_map1) &
              call resize_remap_vars(1,num_links_map1-max_links_map1)
         
         do ib_thread = 1, il_nbthreads
            grid1_add_map1(sga_remap(ib_thread)%start_pos: &
               sga_remap(ib_thread)%start_pos+             &
               sga_remap(ib_thread)%num_links-1) =         &
               sga_remap(ib_thread)%grid1_add(1:sga_remap(ib_thread)%num_links)
            grid2_add_map1(sga_remap(ib_thread)%start_pos: &
               sga_remap(ib_thread)%start_pos+             &
               sga_remap(ib_thread)%num_links-1) =         &
               sga_remap(ib_thread)%grid2_add(1:sga_remap(ib_thread)%num_links)
            wts_map1     (:,sga_remap(ib_thread)%start_pos: &
               sga_remap(ib_thread)%start_pos+            &
               sga_remap(ib_thread)%num_links-1) =        &
               sga_remap(ib_thread)%wts(:,1:sga_remap(ib_thread)%num_links)

         end do

         if (nlogprt.ge.2) then

            do ib_thread = 1, il_nbthreads
               if (sga_remap(ib_thread)%nb_resize.gt.0) then
                  write(nulou,*) ' Number of thread_resize_remap_vars on thread ',&
                     ib_thread, ' = ', sga_remap(ib_thread)%nb_resize
               end if
            end do

         end if

      end if

      ! Gather the complete results on master proc

      if (mpi_size_map .gt. 1) then

        IF (mpi_rank_map == mpi_root_map) THEN
          ALLOCATE(ila_num_links_mpi(mpi_size_map))
        ELSE
          ALLOCATE(ila_num_links_mpi(1))
        END IF

        CALL MPI_Gather (num_links_map1,   1,MPI_INT,&
           &             ila_num_links_mpi,1,MPI_INT,&
           &             mpi_root_map,mpi_comm_map,il_err)

        CALL MPI_Allreduce(MPI_IN_PLACE, grid2_frac, grid2_size, MPI_DOUBLE, MPI_SUM, &
                           mpi_comm_map,il_err)

        IF (mpi_rank_map == mpi_root_map) THEN
          num_links_map1 = SUM(ila_num_links_mpi)
          if (num_links_map1 > max_links_map1) &
             call resize_remap_vars(1,num_links_map1-max_links_map1)

          ALLOCATE(ila_req_mpi(6,mpi_size_map-1))
          ALLOCATE(ila_sta_mpi(MPI_STATUS_SIZE,6,mpi_size_map-1))

          DO n = 1, mpi_size_map-1
            buff_base = SUM(ila_num_links_mpi(1:n))+1
            CALL MPI_IRecv(grid1_add_map1(buff_base),&
                  & ila_num_links_mpi(n+1),MPI_INT,n,1,mpi_comm_map,&
                  & ila_req_mpi(1,n),il_err)

            CALL MPI_IRecv(grid2_add_map1(buff_base),&
                  & ila_num_links_mpi(n+1),MPI_INT,n,2,mpi_comm_map,&
                  & ila_req_mpi(2,n),il_err)

            CALL MPI_IRecv(wts_map1(:,buff_base),&
                  & num_wts*ila_num_links_mpi(n+1),MPI_DOUBLE,n,3,mpi_comm_map,&
                  & ila_req_mpi(3,n),il_err)

            CALL MPI_IRecv(grid1_area(ila_mpi_mn(n+1)),&
                  & ila_mpi_mx(n+1)-ila_mpi_mn(n+1)+1,MPI_DOUBLE,n,4,mpi_comm_map,&
                  & ila_req_mpi(4,n),il_err)

            CALL MPI_IRecv(grid1_centroid_lat(ila_mpi_mn(n+1)),&
                  & ila_mpi_mx(n+1)-ila_mpi_mn(n+1)+1,MPI_DOUBLE,n,5,mpi_comm_map,&
                  & ila_req_mpi(5,n),il_err)

            CALL MPI_IRecv(grid1_centroid_lon(ila_mpi_mn(n+1)),&
                  & ila_mpi_mx(n+1)-ila_mpi_mn(n+1)+1,MPI_DOUBLE,n,6,mpi_comm_map,&
                  & ila_req_mpi(6,n),il_err)

          END DO

          DO n=1,6
            CALL MPI_Waitall(mpi_size_map-1,ila_req_mpi(n,:),ila_sta_mpi(1,n,1),il_err)
          END DO

          DEALLOCATE(ila_req_mpi)
          DEALLOCATE(ila_sta_mpi)

        ELSE

          CALL MPI_Send(grid1_add_map1,num_links_map1,MPI_INT,&
               & mpi_root_map,1,mpi_comm_map,il_err)

          CALL MPI_Send(grid2_add_map1,num_links_map1,MPI_INT,&
               & mpi_root_map,2,mpi_comm_map,il_err)

          CALL MPI_Send(wts_map1,num_wts*num_links_map1,MPI_DOUBLE,&
               & mpi_root_map,3,mpi_comm_map,il_err)

          CALL MPI_Send(grid1_area(ila_mpi_mn(mpi_rank_map+1)),&
               & ila_mpi_mx(mpi_rank_map+1)-ila_mpi_mn(mpi_rank_map+1)+1,MPI_DOUBLE,&
               & mpi_root_map,4,mpi_comm_map,il_err)

          CALL MPI_Send(grid1_centroid_lat(ila_mpi_mn(mpi_rank_map+1)),&
               & ila_mpi_mx(mpi_rank_map+1)-ila_mpi_mn(mpi_rank_map+1)+1,MPI_DOUBLE,&
               & mpi_root_map,5,mpi_comm_map,il_err)

          CALL MPI_Send(grid1_centroid_lon(ila_mpi_mn(mpi_rank_map+1)),&
               & ila_mpi_mx(mpi_rank_map+1)-ila_mpi_mn(mpi_rank_map+1)+1,MPI_DOUBLE,&
               & mpi_root_map,6,mpi_comm_map,il_err)

        END IF

        deallocate(ila_num_links_mpi)

      end if

      num_links_map1_sweep1 = num_links_map1

      if (nlogprt .ge. 2) &
        write(6,*) ' num_links_map1 after sweep 1', num_links_map1_sweep1

      deallocate(ila_thr_mn)
      deallocate(ila_thr_mx)
      if (il_nbthreads .gt. 1) then
         do ib_thread = 1, il_nbthreads
            deallocate(sga_remap(ib_thread)%grid1_add)
            deallocate(sga_remap(ib_thread)%grid2_add)
            deallocate(sga_remap(ib_thread)%wts)
         end do
         deallocate(sga_remap)
      end if
!
#ifdef TREAT_OVERLAY
      deallocate(grid2_overlap)
#endif

      if (nlogprt .ge. 2) then 
         write(nulou,*) 'grid1 end sweep '
         call OASIS_FLUSH_SCRIP(nulou)
      endif

      if (ll_timing) call timer_stop(3)
      
      !-----------------------------------------------------------------------
      !
      !     integrate around each cell on grid2
      !
      !-----------------------------------------------------------------------

      if (ll_timing) call timer_start(4,'remap_conserv grid2 sweep')

      if (nlogprt .ge. 2) then 
         write(nulou,*) 'grid2 sweep '
         call OASIS_FLUSH_SCRIP(nulou)
      endif

      if (mpi_size_map .gt. 1) then

         allocate(ila_mpi_sz(mpi_size_map))
         il_splitsize = grid2_size
         ila_mpi_sz(:) = floor(real(il_splitsize)/mpi_size_map)
         ila_mpi_sz(1:il_splitsize-sum(ila_mpi_sz)) = ila_mpi_sz(1:il_splitsize-sum(ila_mpi_sz)) + 1

         ila_mpi_mn(1) = 1
         ila_mpi_mx(1) = ila_mpi_sz(1)

         do ib_proc = 2, mpi_size_map
            ila_mpi_mn(ib_proc) = sum(ila_mpi_sz(1:ib_proc-1)) + 1
            ila_mpi_mx(ib_proc) = sum(ila_mpi_sz(1:ib_proc))
         end do

         deallocate(ila_mpi_sz)

      else

         ila_mpi_mn(1) = 1
         ila_mpi_mx(1) = grid2_size

      endif

!$OMP PARALLEL NUM_THREADS(il_envthreads) DEFAULT(NONE) &
!$OMP SHARED(num_wts) &
!$OMP SHARED(il_envthreads) &
!$OMP SHARED(grid1_corners,grid2_corners) &
!$OMP SHARED(grid1_bound_box,grid2_bound_box) &
!$OMP SHARED(grid1_bbox_per,grid2_bbox_per) &
!$OMP SHARED(grid1_size,bin_addr1,bin_addr2) &
!$OMP SHARED(est_num_neighbors,num_srch_bins) &
!$OMP SHARED(il_nbthreads) &
!$OMP SHARED(nulou,sga_remap) &
!$OMP SHARED(grid2_center_lat) &
!$OMP SHARED(grid1_center_lon) &
!$OMP SHARED(grid2_center_lon) &
!$OMP SHARED(grid1_corner_lat,grid1_corner_lon) &
!$OMP SHARED(grid2_corner_lat,grid2_corner_lon) &
!$OMP SHARED(grid1_mask) &
!$OMP SHARED(mpi_rank_map,mpi_root_map,ila_mpi_mn,ila_mpi_mx) &
!$OMP SHARED(ila_thr_sz,ila_thr_mn,ila_thr_mx) &
!$OMP SHARED(grid2_frac) &
!$OMP SHARED(grid2_area) &
!$OMP SHARED(grid2_centroid_lat) &
!$OMP SHARED(grid2_centroid_lon) &
!$OMP PRIVATE(nlogprt) &
!$OMP PRIVATE(grid1_add,n,srch_add,corner) &
!$OMP PRIVATE(dgbb1,dgbb2,dgbb3,dgbb4,dgbbp) &
!$OMP PRIVATE(min_add,max_add) &
!$OMP PRIVATE(min_cell,max_cell,grid2_add,num_srch_cells) &
!$OMP PRIVATE(next_corn,beglat,beglon,endlat,endlon,lrevers) &
!$OMP PRIVATE(begseg,lbegin,num_subseg) &
!$OMP PRIVATE(intrsct_lat,intrsct_lon,lcoinc,weights,ll_debug) &
!$OMP PRIVATE(intrsct_lat_off,intrsct_lon_off) &
!$OMP PRIVATE(ib_thread,il_splitsize)

!$OMP SINGLE

      il_nbthreads = 1
!$    il_nbthreads = OMP_GET_NUM_THREADS ()

      allocate(ila_thr_mn(il_nbthreads))
      allocate(ila_thr_mx(il_nbthreads))

      if (il_nbthreads .gt. 1) then

         nlogprt = 0

         allocate(ila_thr_sz(il_nbthreads))
         il_splitsize = ila_mpi_mx(mpi_rank_map+1)-ila_mpi_mn(mpi_rank_map+1)+1
         ila_thr_sz(:) = floor(real(il_splitsize)/il_nbthreads)
         ila_thr_sz(1:il_splitsize-sum(ila_thr_sz)) = ila_thr_sz(1:il_splitsize-sum(ila_thr_sz)) + 1

         ila_thr_mn(1) = ila_mpi_mn(mpi_rank_map+1)
         ila_thr_mx(1) = ila_thr_mn(1) + ila_thr_sz(1) - 1

         do ib_thread = 2, il_nbthreads
            ila_thr_mn(ib_thread) = ila_mpi_mn(mpi_rank_map+1) + sum(ila_thr_sz(1:ib_thread-1))
            ila_thr_mx(ib_thread) = ila_thr_mn(ib_thread) + ila_thr_sz(ib_thread) - 1
         end do

         allocate(sga_remap(il_nbthreads))

         do ib_thread = 1, il_nbthreads
            il_splitsize = est_num_neighbors*ila_thr_sz(ib_thread)
            sga_remap(ib_thread)%max_links = il_splitsize
            sga_remap(ib_thread)%num_links = 0
            allocate(sga_remap(ib_thread)%grid1_add(il_splitsize))
            allocate(sga_remap(ib_thread)%grid2_add(il_splitsize))
            allocate(sga_remap(ib_thread)%wts(num_wts,il_splitsize))
         end do

         deallocate(ila_thr_sz)

      else

         ila_thr_mn(1) = ila_mpi_mn(mpi_rank_map+1)
         ila_thr_mx(1) = ila_mpi_mx(mpi_rank_map+1)

      end if
!$OMP END SINGLE

!$OMP DO SCHEDULE(STATIC,1)
      thread_loop2: do ib_thread = 1, il_nbthreads

      ll_debug = .false.

      grid_loop2: do grid2_add = ila_thr_mn(ib_thread), ila_thr_mx(ib_thread)

         if (ll_debug) then 
            write(nulou,*) 'grid2_add=', grid2_add
            call OASIS_FLUSH_SCRIP(nulou)
         endif

         !***
         !*** destination grid bounding box (preloaded)
         !***

         dgbb1 = grid2_bound_box(1,grid2_add)
         dgbb2 = grid2_bound_box(2,grid2_add)
         dgbb3 = grid2_bound_box(3,grid2_add)
         dgbb4 = grid2_bound_box(4,grid2_add)
         dgbbp = grid2_bbox_per(grid2_add)

         !***
         !*** restrict searches first using search bins
         !***

         min_add = grid1_size
         max_add = 1
         do n=1,num_srch_bins
            if (grid2_add >= bin_addr2(1,n) .and. grid2_add <= bin_addr2(2,n)) then
               min_add = min(min_add, bin_addr1(1,n))
               max_add = max(max_add, bin_addr1(2,n))
            endif
         end do

         !***
         !*** select the search cells
         !***

         num_srch_cells = 0
         min_cell = max_add
         max_cell = min_add

         gather2: do grid1_add = min_add, max_add

            !***
            !*** further restrict searches using bounding boxes
            !***

            if (          grid1_bound_box(1,grid1_add) <= dgbb2 ) then
               if (       grid1_bound_box(2,grid1_add) >= dgbb1 ) then
                  if (    lf_loncross(dgbbp,dgbb3,dgbb4,grid1_add,&
                                      grid1_bbox_per,grid1_bound_box) ) then

                     num_srch_cells = num_srch_cells+1
                     min_cell = min(min_cell, grid1_add)
                     max_cell = max(max_cell, grid1_add)

                  end if
               end if
            end if

         end do gather2

         !***
         !*** store the search cells array
         !***

         if (num_srch_cells .ne. 0) then

            allocate(srch_add(num_srch_cells))

            n=0
            pick2: do grid1_add = min_cell,max_cell

               if (          grid1_bound_box(1,grid1_add) <= dgbb2 ) then
                  if (       grid1_bound_box(2,grid1_add) >= dgbb1 ) then
                     if (    lf_loncross(dgbbp,dgbb3,dgbb4,grid1_add, &
                                         grid1_bbox_per,grid1_bound_box) ) then
                        
                        n = n+1
                        srch_add(n) = grid1_add
                        
                     end if
                  end if
               end if
               
            end do pick2

         end if

         if (ll_debug) then
            write(nulou,*)'    '
            write(nulou,*)'  ** Grid2 cell and associated search cells **'
            write(nulou,1130) grid2_add, grid2_center_lon(grid2_add), grid2_center_lat(grid2_add)
            do corner = 1,grid2_corners 
               write(nulou,1131) corner, grid2_corner_lon(corner,grid2_add), grid2_corner_lat(corner,grid2_add)
            enddo
            write(nulou,1804) grid2_add, grid2_bound_box(1,grid2_add), grid2_bound_box(2,grid2_add)
            write(nulou,1408) grid2_add, grid2_bound_box(3,grid2_add), grid2_bound_box(4,grid2_add)
            write(nulou,*) '  num_srch_cells=', num_srch_cells
            write(nulou,*) '    '
            if (num_srch_cells .ne. 0) then
               write(nulou,*) '  srch_add(:)=', srch_add(1:num_srch_cells)
               write(nulou,*) '  msk_srch_add(:)=', (.true.,n=1,num_srch_cells)
               do n=1, num_srch_cells
                  do corner = 1,grid2_corners
                     write(nulou,1132) n, grid1_corner_lon(corner,srch_add(n)), grid1_corner_lat(corner,srch_add(n))
                  end do
               end do
            end if
            write(nulou,*)'    ***************************************'
            write(nulou,*)'    '
         endif
1130     format ('   grid2 center, lon, lat = ', I8, 2X, F12.4, 2X, F12.4)
1131     format ('   grid2 corner, lon, lat = ', I8, 2X, F12.4, 2X, F12.4)
1132     format ('     srch cell, lon, lat = ', I8, 2X, F12.4, 2X, F12.4)  
1804     format ('   grid2 index,  bbox lat = ', I8, 2X, F12.4, 2X, F12.4)       
1408     format ('   grid2 index,  bbox lon = ', I8, 2X, F12.4, 2X, F12.4)       

         if (num_srch_cells .ne. 0) then


            !***
            !*** integrate around this cell
            !***

            !        full = .false.
            !        do grid1_add = min_add,max_add
            !          if (grid1_mask(grid1_add)) full = .true.
            !        end do
            !        if (full) then

            do corner = 1,grid2_corners
               next_corn = mod(corner,grid2_corners) + 1

               beglat = grid2_corner_lat(corner,grid2_add)
               beglon = grid2_corner_lon(corner,grid2_add)
               endlat = grid2_corner_lat(next_corn,grid2_add)
               endlon = grid2_corner_lon(next_corn,grid2_add)
               lrevers = .false.

               !***
               !*** to ensure exact path taken during both
               !*** sweeps, always integrate in the same direction
               !***

               if ((endlat < beglat) .or. &
                  (endlat == beglat .and. endlon < beglon)) then 
                  beglat = grid2_corner_lat(next_corn,grid2_add)
                  beglon = grid2_corner_lon(next_corn,grid2_add)
                  endlat = grid2_corner_lat(corner,grid2_add)
                  endlon = grid2_corner_lon(corner,grid2_add)
                  lrevers = .true.
                  if (ll_debug) &
                     write(nulou, *) ' sweep2 LREVERS TRUE'
               endif
               begseg(1) = beglat
               begseg(2) = beglon
               lbegin = .true.

               !***
               !*** if this is a constant-longitude segment, skip the rest 
               !*** since the line integral contribution will be zero.
               !***

               if (ll_debug) then
                  if (endlon .eq. beglon) then
                     write(nulou,1113) beglon, beglat 
                     write(nulou,1114) endlon, endlat
                     write(nulou, *)'  sweep2 endlon == beglon; skip segment'
                     write(nulou,*) '             '
                  endif
               endif
               if (endlon /= beglon) then
                  num_subseg = 0

                  !***
                  !*** integrate along this segment, detecting intersections 
                  !*** and computing the line integral for each sub-segment
                  !***

                  do while (beglat /= endlat .or. beglon /= endlon)

                     !***
                     !*** prevent infinite loops if integration gets stuck
                     !*** near cell or threshold boundary
                     !***

                     num_subseg = num_subseg + 1
                     if (num_subseg > max_subseg) then
                        write(nulou,*) 'ERROR=>integration stalled:' 
                        write(nulou,*) 'num_subseg exceeded limit'
                        write(nulou,*) 'Verify corners in grids.nc, especially'
                        write(nulou,*) 'if calculated by OASIS routine corners'
                        write(nulou,*) 'integration stalled: num_subseg exceeded limit'
                        call OASIS_FLUSH_SCRIP(nulou)
                        stop
                     endif

                     !***
                     !*** find next intersection of this segment with a line 
                     !*** on grid 1.
                     !***

                     if (ll_debug) then
                        write(nulou,1115) beglon, beglat 
                        write(nulou,1116) endlon, endlat
                        write(nulou,*) '             '
                     endif

                     call intersection(grid1_add,intrsct_lat,intrsct_lon,lcoinc, &
                        beglat, beglon, endlat, endlon, begseg,   &
                        lbegin, lrevers,                          &
                        grid1_corners, num_srch_cells,            &
                        srch_add, grid1_corner_lon, grid1_corner_lat, &
                        intrsct_lat_off,intrsct_lon_off)

                     if (ll_debug) then
                        write(nulou,*) ' After call intersection, grid1_add', grid1_add
                        write(nulou,1117) beglon, beglat 
                        write(nulou,1118) endlon, endlat
                        write(nulou,1119) intrsct_lon, intrsct_lat
                        write(nulou,*) '   '
                     endif

                     lbegin = .false.

                     !***
                     !*** compute line integral for this subsegment.
                     !***

                     if (grid1_add /= 0) then
                        call line_integral(weights, num_wts,                        &
                           beglon, intrsct_lon, beglat, intrsct_lat,&
                           grid1_center_lon(grid1_add),             &
                           grid2_center_lon(grid2_add))

                        if (ll_debug) then
                           write(nulou,*) '  A2) WEIGHTS for this subsegment =', weights(1)
                           write(nulou,*) '     ' 
                        endif
                     else
                        call line_integral(weights, num_wts,                        &
                           beglon, intrsct_lon, beglat, intrsct_lat,&
                           grid2_center_lon(grid2_add),             &
                           grid2_center_lon(grid2_add))

                        if (ll_debug) then
                           write(nulou,*) '  B2) WEIGHTS for this subsegment =', weights(1)
                           write(nulou,*) '     ' 
                        endif
                     endif

                     if (lrevers) then
                        weights = -weights
                        if (ll_debug) then
                           write(nulou,*) '  LREVERS; WEIGHTS for this subsegment =', weights(1)
                           write(nulou,*) '     '
                        endif
                     endif
                     !***
                     !*** store the appropriate addresses and weights. 
                     !*** also add contributions to cell areas and centroids.
                     !*** if there is a coincidence, do not store weights
                     !*** because they have been captured in the previous loop.
                     !*** the grid1 mask is the master mask
                     !***

                     if (ll_debug .and. lcoinc) &
                        write(nulou,*) '  LCOINC is TRUE; weight not stored'

                     if (.not. lcoinc .and. grid1_add /= 0) then
                        if (grid1_mask(grid1_add)) then
                           call store_link_cnsrv(grid1_add, grid2_add, weights, ib_thread)

                           if (ll_debug) then
                              write(nulou,*) '      after store_link_cnsrv norm2'
                              write(nulou,1120) grid1_add, grid2_add,beglon, beglat, intrsct_lon,intrsct_lat,weights(1)
                           endif
!EM                           grid1_frac(grid1_add) = grid1_frac(grid1_add) + weights(1)
                           grid2_frac(grid2_add) = grid2_frac(grid2_add) + weights(num_wts+1)
                        endif

                     endif

                     grid2_area(grid2_add) = grid2_area(grid2_add) + weights(num_wts+1)
                     grid2_centroid_lat(grid2_add) = grid2_centroid_lat(grid2_add) + weights(num_wts+2)
                     grid2_centroid_lon(grid2_add) = grid2_centroid_lon(grid2_add) + weights(num_wts+3)

                     !***
                     !*** reset beglat and beglon for next subsegment.
                     !***

                     beglat = intrsct_lat
                     beglon = intrsct_lon

                  end do

               end if

               !***
               !*** end of segment
               !***

            end do

            !***
            !*** finished with this cell: deallocate search array and
            !*** start on next cell

            deallocate(srch_add)

         end if ! num_srch_cells .NE. 0

      end do grid_loop2

      end do thread_loop2
!$OMP END DO

!$OMP END PARALLEL

      if (il_nbthreads .gt. 1) then

         sga_remap(1)%start_pos = num_links_map1_sweep1 + 1

         il_splitsize = sga_remap(1)%num_links
         do ib_thread = 2, il_nbthreads
            il_splitsize = il_splitsize + sga_remap(ib_thread)%num_links
            sga_remap(ib_thread)%start_pos = sga_remap(ib_thread-1)%start_pos + &
               sga_remap(ib_thread-1)%num_links
         end do

         num_links_map1 = num_links_map1_sweep1 + il_splitsize 
         if (num_links_map1 > max_links_map1) &
            call resize_remap_vars(1,num_links_map1-max_links_map1)

         do ib_thread = 1, il_nbthreads
            grid1_add_map1(sga_remap(ib_thread)%start_pos: &
               sga_remap(ib_thread)%start_pos+             &
               sga_remap(ib_thread)%num_links-1) =         &
               sga_remap(ib_thread)%grid1_add(1:sga_remap(ib_thread)%num_links)
            grid2_add_map1(sga_remap(ib_thread)%start_pos: &
               sga_remap(ib_thread)%start_pos+             &
               sga_remap(ib_thread)%num_links-1) =         &
               sga_remap(ib_thread)%grid2_add(1:sga_remap(ib_thread)%num_links)
            wts_map1     (:,sga_remap(ib_thread)%start_pos: &
               sga_remap(ib_thread)%start_pos+            &
               sga_remap(ib_thread)%num_links-1) =        &
               sga_remap(ib_thread)%wts(:,1:sga_remap(ib_thread)%num_links)

         end do

         if (nlogprt.ge.2) then

            do ib_thread = 1, il_nbthreads
               if (sga_remap(ib_thread)%nb_resize.gt.0) then
                  write(nulou,*) ' Number of thread_resize_remap_vars on thread ',&
                     ib_thread, ' = ', sga_remap(ib_thread)%nb_resize
               end if
            end do

         end if

      end if

      ! Gather the complete results on master proc

      if (mpi_size_map .gt. 1) then

        IF (mpi_rank_map == mpi_root_map) THEN
          ALLOCATE(ila_num_links_mpi(mpi_size_map))
        ELSE
          ALLOCATE(ila_num_links_mpi(1))
        END IF

        CALL MPI_Gather (num_links_map1-num_links_map1_sweep1,   1,MPI_INT,&
           &             ila_num_links_mpi,1,MPI_INT,&
           &             mpi_root_map,mpi_comm_map,il_err)

        IF (mpi_rank_map == mpi_root_map) THEN
          num_links_map1 = num_links_map1_sweep1 + SUM(ila_num_links_mpi)
          if (num_links_map1 > max_links_map1) &
             call resize_remap_vars(1,num_links_map1-max_links_map1)

          ALLOCATE(ila_req_mpi(7,mpi_size_map-1))
          ALLOCATE(ila_sta_mpi(MPI_STATUS_SIZE,7,mpi_size_map-1))

          DO n = 1, mpi_size_map-1
            buff_base = num_links_map1_sweep1+SUM(ila_num_links_mpi(1:n))+1
                        
            CALL MPI_IRecv(grid1_add_map1(buff_base),&
                  & ila_num_links_mpi(n+1),MPI_INT,n,1,mpi_comm_map,&
                  & ila_req_mpi(1,n),il_err)

            CALL MPI_IRecv(grid2_add_map1(buff_base),&
                  & ila_num_links_mpi(n+1),MPI_INT,n,2,mpi_comm_map,&
                  & ila_req_mpi(2,n),il_err)

            CALL MPI_IRecv(wts_map1(:,buff_base),&
                  & num_wts*ila_num_links_mpi(n+1),MPI_DOUBLE,n,3,mpi_comm_map,&
                  & ila_req_mpi(3,n),il_err)

            CALL MPI_IRecv(grid2_area(ila_mpi_mn(n+1)),&
                  & ila_mpi_mx(n+1)-ila_mpi_mn(n+1)+1,MPI_DOUBLE,n,4,mpi_comm_map,&
                  & ila_req_mpi(4,n),il_err)

            CALL MPI_IRecv(grid2_centroid_lat(ila_mpi_mn(n+1)),&
                  & ila_mpi_mx(n+1)-ila_mpi_mn(n+1)+1,MPI_DOUBLE,n,5,mpi_comm_map,&
                  & ila_req_mpi(5,n),il_err)

            CALL MPI_IRecv(grid2_centroid_lon(ila_mpi_mn(n+1)),&
                  & ila_mpi_mx(n+1)-ila_mpi_mn(n+1)+1,MPI_DOUBLE,n,6,mpi_comm_map,&
                  & ila_req_mpi(6,n),il_err)

            CALL MPI_IRecv(grid2_frac(ila_mpi_mn(n+1)),&
                  & ila_mpi_mx(n+1)-ila_mpi_mn(n+1)+1,MPI_DOUBLE,n,7,mpi_comm_map,&
                  & ila_req_mpi(7,n),il_err)

          END DO

          DO n=1,7
            CALL MPI_Waitall(mpi_size_map-1,ila_req_mpi(n,:),ila_sta_mpi(1,n,1),il_err)
          END DO

          DEALLOCATE(ila_req_mpi)
          DEALLOCATE(ila_sta_mpi)

        ELSE

          CALL MPI_Send(grid1_add_map1(num_links_map1_sweep1+1),num_links_map1-num_links_map1_sweep1,MPI_INT,&
               & mpi_root_map,1,mpi_comm_map,il_err)

          CALL MPI_Send(grid2_add_map1(num_links_map1_sweep1+1),num_links_map1-num_links_map1_sweep1,MPI_INT,&
               & mpi_root_map,2,mpi_comm_map,il_err)

          CALL MPI_Send(wts_map1(1,num_links_map1_sweep1+1),num_wts*(num_links_map1-num_links_map1_sweep1),MPI_DOUBLE,&
               & mpi_root_map,3,mpi_comm_map,il_err)

          CALL MPI_Send(grid2_area(ila_mpi_mn(mpi_rank_map+1)),&
               & ila_mpi_mx(mpi_rank_map+1)-ila_mpi_mn(mpi_rank_map+1)+1,MPI_DOUBLE,&
               & mpi_root_map,4,mpi_comm_map,il_err)

          CALL MPI_Send(grid2_centroid_lat(ila_mpi_mn(mpi_rank_map+1)),&
               & ila_mpi_mx(mpi_rank_map+1)-ila_mpi_mn(mpi_rank_map+1)+1,MPI_DOUBLE,&
               & mpi_root_map,5,mpi_comm_map,il_err)

          CALL MPI_Send(grid2_centroid_lon(ila_mpi_mn(mpi_rank_map+1)),&
               & ila_mpi_mx(mpi_rank_map+1)-ila_mpi_mn(mpi_rank_map+1)+1,MPI_DOUBLE,&
               & mpi_root_map,6,mpi_comm_map,il_err)

          CALL MPI_Send(grid2_frac(ila_mpi_mn(mpi_rank_map+1)),&
               & ila_mpi_mx(mpi_rank_map+1)-ila_mpi_mn(mpi_rank_map+1)+1,MPI_DOUBLE,&
               & mpi_root_map,7,mpi_comm_map,il_err)

        END IF

        deallocate(ila_num_links_mpi)

      end if

      if (nlogprt .ge. 2) &
        write(6,*) ' num_links_map1 after sweep 2', num_links_map1

      deallocate(ila_thr_mn)
      deallocate(ila_thr_mx)

      deallocate(ila_mpi_mn, ila_mpi_mx)

      if (il_nbthreads .gt. 1) then
         do ib_thread = 1, il_nbthreads
            deallocate(sga_remap(ib_thread)%grid1_add)
            deallocate(sga_remap(ib_thread)%grid2_add)
            deallocate(sga_remap(ib_thread)%wts)
         end do
         deallocate(sga_remap)
      end if

      if (nlogprt .ge. 2) then
         write(nulou,*) 'grid2 end sweep '
         call OASIS_FLUSH_SCRIP(nulou)
      endif
      
      ll_debug = .false.
      
      if (ll_debug) then
         do n=1,num_links_map1
            write(nulou,*) 'grid1, grid2, weight= ', grid1_add_map1(n), grid2_add_map1(n), wts_map1(1,n)
         end do
      endif

      if (ll_timing) call timer_stop(4)

      !-----------------------------------------------------------------------
      !  WARNING 
      !  After this line, calculations MUST be single threaded
      !-----------------------------------------------------------------------
      il_nbthreads = 1

      !-----------------------------------------------------------------------
      !
      !     correct for situations where N/S pole not explicitly included in
      !     grid (i.e. as a grid corner point). if pole is missing from only
      !     one grid, need to correct only the area and centroid of that 
      !     grid.  if missing from both, do complete weight calculation.
      !
      !-----------------------------------------------------------------------

      !*** North Pole
      weights(1) =  pi2
      weights(2) =  pi*pi
      weights(3) =  zero
      weights(4) =  pi2
      weights(5) =  pi*pi
      weights(6) =  zero

      grid1_add = 0
      pole_loop1: do n=1,grid1_size
         if (grid1_area(n) < -three*pih .and. grid1_center_lat(n) > zero) then
            grid1_add = n
            exit pole_loop1
         endif
      end do pole_loop1

      grid2_add = 0
      pole_loop2: do n=1,grid2_size
         if (grid2_area(n) < -three*pih .and. grid2_center_lat(n) > zero) then
            grid2_add = n
            exit pole_loop2
         endif
      end do pole_loop2

      if (grid1_add /=0) then
         grid1_area(grid1_add) = grid1_area(grid1_add) + weights(1)
         grid1_centroid_lat(grid1_add) = grid1_centroid_lat(grid1_add) + weights(2)
         grid1_centroid_lon(grid1_add) = grid1_centroid_lon(grid1_add) + weights(3)
      endif

      if (grid2_add /=0) then
         grid2_area(grid2_add) = grid2_area(grid2_add) + weights(num_wts+1)
         grid2_centroid_lat(grid2_add) = grid2_centroid_lat(grid2_add) + weights(num_wts+2)
         grid2_centroid_lon(grid2_add) = grid2_centroid_lon(grid2_add) + weights(num_wts+3)
      endif

      if (grid1_add /= 0 .and. grid2_add /=0) then
         call store_link_cnsrv(grid1_add, grid2_add, weights, 1)
!EM         grid1_frac(grid1_add) = grid1_frac(grid1_add) + weights(1)
         grid2_frac(grid2_add) = grid2_frac(grid2_add) + weights(num_wts+1)
      endif

      !*** South Pole
      weights(1) =  pi2
      weights(2) = -pi*pi
      weights(3) =  zero
      weights(4) =  pi2
      weights(5) = -pi*pi
      weights(6) =  zero

      grid1_add = 0
      pole_loop3: do n=1,grid1_size
         if (grid1_area(n) < -three*pih .and. grid1_center_lat(n) < zero) then
            grid1_add = n
            exit pole_loop3
         endif
      end do pole_loop3

      grid2_add = 0
      pole_loop4: do n=1,grid2_size
         if (grid2_area(n) < -three*pih .and. grid2_center_lat(n) < zero) then
            grid2_add = n
            exit pole_loop4
         endif
      end do pole_loop4

      if (grid1_add /=0) then
         grid1_area(grid1_add) = grid1_area(grid1_add) + weights(1)
         grid1_centroid_lat(grid1_add) = grid1_centroid_lat(grid1_add) + weights(2)
         grid1_centroid_lon(grid1_add) = grid1_centroid_lon(grid1_add) + weights(3)
      endif

      if (grid2_add /=0) then
         grid2_area(grid2_add) = grid2_area(grid2_add) + weights(num_wts+1)
         grid2_centroid_lat(grid2_add) = grid2_centroid_lat(grid2_add) + weights(num_wts+2)
         grid2_centroid_lon(grid2_add) = grid2_centroid_lon(grid2_add) + weights(num_wts+3)
      endif

      if (grid1_add /= 0 .and. grid2_add /=0) then
         call store_link_cnsrv(grid1_add, grid2_add, weights, 1)

!EM         grid1_frac(grid1_add) = grid1_frac(grid1_add) + weights(1)
         grid2_frac(grid2_add) = grid2_frac(grid2_add) + weights(num_wts+1)
      endif

      !-----------------------------------------------------------------------
      !
      !     finish centroid computation
      !
      !-----------------------------------------------------------------------

      where (grid1_area /= zero)
         grid1_centroid_lat = grid1_centroid_lat/grid1_area
         grid1_centroid_lon = grid1_centroid_lon/grid1_area
      end where

      where (grid2_area /= zero)
         grid2_centroid_lat = grid2_centroid_lat/grid2_area
         grid2_centroid_lon = grid2_centroid_lon/grid2_area
      end where

      !-----------------------------------------------------------------------
      !
      !     include centroids in weights and normalize using destination
      !     area if requested
      !
      !-----------------------------------------------------------------------


      do n=1,num_links_map1

         grid1_add = grid1_add_map1(n)
         grid2_add = grid2_add_map1(n)
         do nwgt=1,num_wts
            weights(        nwgt) = wts_map1(nwgt,n)
            !          if (num_maps > 1) then
            !            weights(num_wts+nwgt) = wts_map2(nwgt,n)
            !          endif
         end do

         select case(norm_opt)
         case (norm_opt_dstarea)
            if (grid2_area(grid2_add) /= zero) then
               if (luse_grid2_area) then
                  norm_factor = one/grid2_area_in(grid2_add)
               else
                  norm_factor = one/grid2_area(grid2_add)
               endif
            else
               norm_factor = zero
            endif
         case (norm_opt_frcarea)
            if (grid2_frac(grid2_add) /= zero) then
               if (luse_grid2_area) then
                  norm_factor = grid2_area(grid2_add)/ &
                     (grid2_frac(grid2_add)*grid2_area_in(grid2_add))
               else
                  norm_factor = one/grid2_frac(grid2_add)
               endif
            else
               norm_factor = zero
            endif
         case (norm_opt_none)
            norm_factor = one
         end select

         wts_map1(1,n) =  weights(1)*norm_factor
         wts_map1(2,n) = (weights(2) - weights(1)*grid1_centroid_lat(grid1_add))*norm_factor
         wts_map1(3,n) = (weights(3) - weights(1)*grid1_centroid_lon(grid1_add))*norm_factor

         !        if (num_maps > 1) then
         !          select case(norm_opt)
         !          case (norm_opt_dstarea)
         !            if (grid1_area(grid1_add) /= zero) then
         !              if (luse_grid1_area) then
         !                norm_factor = one/grid1_area_in(grid1_add)
         !              else
         !                norm_factor = one/grid1_area(grid1_add)
         !              endif
         !            else
         !              norm_factor = zero
         !            endif
         !          case (norm_opt_frcarea)
         !            if (grid1_frac(grid1_add) /= zero) then
         !              if (luse_grid1_area) then
         !                norm_factor = grid1_area(grid1_add)/
         !     &                       (grid1_frac(grid1_add)*
         !     &                        grid1_area_in(grid1_add))
         !              else
         !                norm_factor = one/grid1_frac(grid1_add)
         !              endif
         !            else
         !              norm_factor = zero
         !            endif
         !          case (norm_opt_none)
         !            norm_factor = one
         !          end select
         !
         !          wts_map2(1,n) =  weights(num_wts+1)*norm_factor
         !          wts_map2(2,n) = (weights(num_wts+2) - weights(num_wts+1)*
         !     &                                grid2_centroid_lat(grid2_add))*
         !     &                                norm_factor
         !          wts_map2(3,n) = (weights(num_wts+3) - weights(num_wts+1)*
         !     &                                grid2_centroid_lon(grid2_add))*
         !     &                                norm_factor
         !      endif

      end do

      if (nlogprt .ge. 2) then
         write(nulou,*) 'Total number of links = ',num_links_map1
         call OASIS_FLUSH_SCRIP(nulou)
      endif

!EM      where (grid1_area /= zero) grid1_frac = grid1_frac/grid1_area
      where (grid2_area /= zero) grid2_frac = grid2_frac/grid2_area

      !-----------------------------------------------------------------------
      !
      !     perform some error checking on final weights
      !
      !-----------------------------------------------------------------------

      grid2_centroid_lat = zero
      grid2_centroid_lon = zero

      do n=1,grid1_size
         if (nlogprt .ge. 3) then
            if (grid1_area(n) < -.01) then
               write(nulou,*) 'Grid 1 area error: n, area, mask =',n,grid1_area(n), grid1_mask(n)
            endif
            if (grid1_centroid_lat(n) < -pih-.01 .or.grid1_centroid_lat(n) >  pih+.01) then
               write(nulou,*)'Grid1 centroid lat error: n, centroid_lat, mask=' &
                  ,n,grid1_centroid_lat(n), grid1_mask(n)
            endif
            call OASIS_FLUSH_SCRIP(nulou)
         endif
         grid1_centroid_lat(n) = zero
         grid1_centroid_lon(n) = zero
      end do

      do n=1,grid2_size
         if (nlogprt .ge. 3) then
            if (grid2_area(n) < -.01) then
               write(nulou,*) 'Grid 2 area error:  n, area, mask =' ,n,grid2_area(n), grid2_mask(n)
            endif
            if (grid2_centroid_lat(n) < -pih-.01 .or. grid2_centroid_lat(n) >  pih+.01) then
               write(nulou,*) 'Grid 2 centroid lat error: n, centroid_lat, mask=' &
                  ,n,grid2_centroid_lat(n), grid2_mask(n)
            endif
            call OASIS_FLUSH_SCRIP(nulou)
         endif
         grid2_centroid_lat(n) = zero
         grid2_centroid_lon(n) = zero
      end do

      do n=1,num_links_map1
         grid2_add = grid2_add_map1(n)
         grid2_centroid_lat(grid2_add) = grid2_centroid_lat(grid2_add) + wts_map1(1,n)

         !        if (num_maps > 1) then
         !          if (wts_map2(1,n) < -.01) then
         !            print *,'Map 2 weight < 0 '
         !            PRINT *,
         !     &          'grid1_add,grid2_add, wts_map2, grid1_mask, grid2_mask',
         !     &          grid1_add, grid2_add, wts_map2(1,n), 
         !     &          grid1_mask(grid1_add), grid2_mask(grid2_add) 
         !          endif
         !          if (norm_opt /= norm_opt_none .and. wts_map2(1,n) > 1.01) then
         !            print *,'Map 2 weight < 0 '
         !            PRINT *,
         !     &          'grid1_add,grid2_add,wts_map2, grid1_mask,grid2_mask',
         !     &          grid1_add, grid2_add, wts_map2(1,n), 
         !     &          grid1_mask(grid1_add), grid2_mask(grid2_add) 
         !          endif
         !          grid1_centroid_lat(grid1_add) = 
         !     &    grid1_centroid_lat(grid1_add) + wts_map2(1,n)
         !      endif
      end do

      do n=1,grid2_size
         select case(norm_opt)
         case (norm_opt_dstarea)
            norm_factor = grid2_frac(n)
         case (norm_opt_frcarea)
            norm_factor = one
         case (norm_opt_none)
            if (luse_grid2_area) then
               norm_factor = grid2_area_in(n)
            else
               norm_factor = grid2_area(n)
            endif
         end select
         if (nlogprt .ge. 3) then
         if (abs(grid2_centroid_lat(n)-norm_factor) > .01) then
            print *, 'Error sum wts map1:grid2_add,grid2_centroid_lat,norm_factor=', &
               n,grid2_centroid_lat(n),norm_factor,grid2_mask(n)
         endif
         endif
      end do

      !      if (num_maps > 1) then
      !        do n=1,grid1_size
      !          select case(norm_opt)
      !          case (norm_opt_dstarea)
      !            norm_factor = grid1_frac(n)
      !          case (norm_opt_frcarea)
      !            norm_factor = one
      !          case (norm_opt_none)
      !            if (luse_grid1_area) then
      !              norm_factor = grid1_area_in(n)
      !            else
      !              norm_factor = grid1_area(n)
      !            endif
      !          end select
      !          if (abs(grid1_centroid_lat(n)-norm_factor) > .01) then
      !            print *,
      !     &'Error sum wts map2:grid1_add,grid1_centroid_lat,norm_factor='
      !     &,n,grid1_centroid_lat(n),
      !     &norm_factor,grid1_mask(n)
      !          endif
      !        end do
      !      endif

      !-----------------------------------------------------------------------
      !
      !     deallocate allocated arrays
      !
      !----------------------------------------------------------------------- 

      deallocate (grid1_centroid_lat, grid1_centroid_lon, grid2_centroid_lat, grid2_centroid_lon)

      if (nlogprt .ge. 2) then
         write (UNIT = nulou,FMT = *)' '
         write (UNIT = nulou,FMT = *)'Leaving routine remap_conserv'
         call OASIS_FLUSH_SCRIP(nulou)
      endif

      !-----------------------------------------------------------------------

   CONTAINS

      logical (kind=log_kind) function lf_loncross(dgbbp,dgbb3,dgbb4,grid_add,&
                                                   bbox_per, bound_box)

      !-----------------------------------------------------------------------
      !
      !     this function tests wether the bounding box
      !     of the currently checked cell at address grid2_add in grid2
      !     (grid1_add in grid1) intersects in longitude the destination cell
      !     on grid1 (grid2).
      !     Periodicity is taken into account
      !
      !-----------------------------------------------------------------------

         integer (kind=int_kind), INTENT(IN) :: dgbbp, grid_add
         real (kind=dbl_kind) , INTENT(IN) :: dgbb3, dgbb4
         integer (kind=int_kind), dimension(:) , INTENT(IN) :: bbox_per
         real (kind=dbl_kind), dimension(:,:) , INTENT(IN) :: bound_box

         !--------------------------------------------------------------------
         !
         ! Work variables (argument)
         !
         ! dgbpp : prefetched periodicity class of the destination grid cell
         !         -1 : the west boundary is < 0
         !          0 : both boundaries are in [0,2pi]
         !          1 : the east boundary is > 2pi
         !
         ! dgbb3, dgbb4 : prefetched west and east longitudes of destination
         !                cell
         !
         ! grid_add : address of the source grid
         !
         ! grid_bbox_per : periodicity class of the source grid cells
         !
         ! grid_bound_box : coordinates of the source grid bounding boxes
         !
         !--------------------------------------------------------------------

         !--------------------------------------------------------------------
         !
         ! Principle of the test
         !
         ! A source cell with longitude boundaries [w_s, e_s] intersects
         ! a destination cell with boundaries [w_d, e_d] if simultaneously
         ! w_s is west of e_d and e_s is east of w_d
         ! If one (and only one) of the cells crosses the periodicity
         ! threshold the test checks if the other cell crosses its boundary
         ! inside [0,2pi] (with the standard test) or the boundary stored
         ! with periodicity (by a reverted test on the modulo image of the
         ! boundary).
         !
         !--------------------------------------------------------------------

        lf_loncross = .true.

         select case(dgbbp)
         case(-1)
            
            ! The destination cell crosses 0
            ! It necessarily intersects source cells crossing 0 or 2pi.
            ! It is west of any cell in [0,2pi]: only the w_s is west of e_d
            ! test is needed (periodicity accounted for)

            if (bbox_per(grid_add).eq.0) then
               lf_loncross = bound_box(3,grid_add) <= dgbb4 .or. &
                  &          bound_box(4,grid_add) >= dgbb3 + pi2
            end if
         case(0)

            ! The destination cell is in [0,2pi]
            
            select case(bbox_per(grid_add))
            case(-1)

               ! The source cell crosses 0
               ! Only the e_s is east of w_d test is needed 
               ! (periodicity accounted for)

               lf_loncross = bound_box(4,grid_add) >= dgbb3 .or. &
                  &          bound_box(3,grid_add) + pi2 <= dgbb4
            case(0)

               ! The source cell is in [0,2pi]
               ! Standard test

               lf_loncross = bound_box(3,grid_add) <= dgbb4 ! w_s is west of e_d
               if (lf_loncross) &
                  & lf_loncross = bound_box(4,grid_add) >= dgbb3 ! e_s is east of w_d 
            case(1)

               ! The source cell crosses 2pi
               ! Only the w_s is west of e_d test is needed 
               ! (periodicity accounted for)

               lf_loncross = bound_box(3,grid_add) <= dgbb4 .or. &
                  &          bound_box(4,grid_add) - pi2 >= dgbb3
            end select
         case(1)
            
            ! The destination cell crosses 2pi
            ! It necessarily intersects source cells crossing 0 or 2pi.
            ! It is east of any cell in [0,2pi]: only the e_s is east of w_d
            ! test is needed (periodicity accounted for)

            if (bbox_per(grid_add).eq.0) then
               lf_loncross = bound_box(4,grid_add) >= dgbb3 .or. &
                  &          bound_box(3,grid_add) <= dgbb4 - pi2  
            end if
         end select

      end function lf_loncross

   end subroutine remap_conserv

   !***********************************************************************

   subroutine intersection(location,intrsct_lat,intrsct_lon,lcoinc, &
      beglat, beglon, endlat, endlon, begseg,  &
      lbegin, lrevers,                         &
      srch_corners, num_srch_cells,            &
      srch_add, grid_corner_lon, grid_corner_lat, &
      intrsct_lat_off, intrsct_lon_off)

      !-----------------------------------------------------------------------
      !
      !     this routine finds the next intersection of a destination grid 
      !     line with the line segment given by beglon, endlon, etc.
      !     a coincidence flag is returned if the segment is entirely 
      !     coincident with an ocean grid line.  the cells in which to search
      !     for an intersection must have already been restricted in the
      !     calling routine.
      !
      !-----------------------------------------------------------------------

      !-----------------------------------------------------------------------
      !
      !     intent(in): 
      !
      !-----------------------------------------------------------------------

      logical (kind=log_kind), intent(in) :: lbegin, & ! flag for first integration along this segment
         lrevers   ! flag whether segment integrated in reverse

      real (kind=dbl_kind), intent(in) :: beglat, beglon,  & ! beginning lat/lon endpoints for segment
         endlat, endlon     ! ending    lat/lon endpoints for segment

      real (kind=dbl_kind), dimension(2), intent(inout) :: begseg ! begin lat/lon of full segment
      integer (kind=int_kind), intent(in) :: srch_corners ! Nb of corners in sarched grid

      integer (kind=int_kind), intent(in) :: num_srch_cells ! Nb of preselected search cells

      integer (kind=int_kind), dimension(:), intent(in) :: srch_add ! Address of search cells

      real (kind=dbl_kind), dimension(:,:), intent(in) :: grid_corner_lat,&
         & grid_corner_lon ! lat and lon of each corner of srch grids

      !-----------------------------------------------------------------------
      !
      !     intent(out): 
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind), intent(out) :: location  ! address in destination array containing this
      ! segment

      logical (kind=log_kind), intent(out) :: lcoinc    ! flag segments which are entirely coincident
      ! with a grid line

      real (kind=dbl_kind), intent(out) :: intrsct_lat, intrsct_lon ! lat/lon coords of next intersect.
      real (kind=dbl_kind), intent(inout) :: intrsct_lat_off, intrsct_lon_off ! lat/lon coords offset 

      !-----------------------------------------------------------------------
      !
      !     local variables
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind) :: n, next_n, cell, gcell
      ! for test of non-convexe cell
      integer (kind=int_kind) :: next2_n, neg, pos  

      integer (kind=int_kind), save :: last_loc  ! save location when crossing threshold
!$OMP THREADPRIVATE(last_loc)

      logical (kind=log_kind) :: loutside  ! flags points outside grid

      logical (kind=log_kind), save :: lthresh = .false.  ! flags segments crossing threshold bndy
!$OMP THREADPRIVATE(lthresh)

      real (kind=dbl_kind) :: lon1, lon2,         & ! local longitude variables for segment
         lat1, lat2,         & ! local latitude  variables for segment
         grdlon1, grdlon2,   & ! local longitude variables for grid cell
         grdlat1, grdlat2,   & ! local latitude  variables for grid cell
         vec1_lat, vec1_lon, & ! vectors and cross products used
         vec2_lat, vec2_lon, & ! during grid search
         cross_product,      &  
         eps, offset,        & ! small offset away from intersect
         s1, s2, determ,     & ! variables used for linear solve to
         mat1, mat2, mat3, mat4, rhs1, rhs2, & ! find intersection
         rl_halfpi, rl_v2lonmpi2, rl_v2lonppi2

      ! for next search

      !-----------------------------------------------------------------------
      !
      !     initialize defaults, flags, etc.
      !
      !-----------------------------------------------------------------------

      location = 0
      lcoinc = .false.
      intrsct_lat = endlat
      intrsct_lon = endlon

      if (num_srch_cells == 0) return

      if (beglat > north_thresh .or. beglat < south_thresh) then

         if (lthresh) location = last_loc
         call pole_intersection(location,                                        &
            intrsct_lat,intrsct_lon,lcoinc,lthresh,          &
            beglat, beglon, endlat, endlon, begseg, lrevers, &
            srch_corners, num_srch_cells,                    &
            srch_add, grid_corner_lon, grid_corner_lat)
         if (lthresh) then
            last_loc = location
            intrsct_lat_off = intrsct_lat
            intrsct_lon_off = intrsct_lon
         endif
         return

      endif

      loutside = .false.
      if (lbegin) then
         lat1 = beglat
         lon1 = beglon
      else
         lat1 = intrsct_lat_off
         lon1 = intrsct_lon_off
      endif
      lat2 = endlat
      lon2 = endlon
      if ((lon2-lon1) > three*pih) then
         lon2 = lon2 - pi2
      else if ((lon2-lon1) < -three*pih) then
         lon2 = lon2 + pi2
      endif
      s1 = zero

      !-----------------------------------------------------------------------
      !
      !     search for location of this segment in ocean grid using cross
      !     product method to determine whether a point is enclosed by a cell
      !
      !-----------------------------------------------------------------------


      srch_loop: do

         !***
         !*** if last segment crossed threshold, use that location
         !***

         if (lthresh) then

            do cell=1,num_srch_cells

               if (srch_add(cell) == last_loc) then
                  location = last_loc
                  eps = tiny
                  exit srch_loop
               endif
            end do
         endif

         !***
         !*** otherwise normal search algorithm
         !***

         cell_loop: do cell=1,num_srch_cells
            gcell = srch_add(cell)
            corner_loop: do n=1,srch_corners
               next_n = mod(n,srch_corners) + 1

               !***
               !*** here we take the cross product of the vector making 
               !*** up each cell side with the vector formed by the vertex
               !*** and search point.  if all the cross products are 
               !*** positive, the point is contained in the cell.
               !***

               vec1_lat = grid_corner_lat(next_n,gcell) - grid_corner_lat(n     ,gcell)
               vec1_lon = grid_corner_lon(next_n,gcell) - grid_corner_lon(n     ,gcell)
               vec2_lat = lat1 - grid_corner_lat(n,gcell)
               vec2_lon = lon1 - grid_corner_lon(n,gcell)

               !***
               !*** if endpoint coincident with vertex, offset
               !*** the endpoint
               !***

               if (vec2_lat == 0 .and. vec2_lon == 0) then
                  lat1 = lat1 + 1.d-10*(lat2-lat1)
                  lon1 = lon1 + 1.d-10*(lon2-lon1)
                  vec2_lat = lat1 - grid_corner_lat(n,gcell)
                  vec2_lon = lon1 - grid_corner_lon(n,gcell)
               endif

               !***
               !*** check for 0,2pi crossings
               !***

               if (vec1_lon >  pi) then
                  vec1_lon = vec1_lon - pi2
               else if (vec1_lon < -pi) then
                  vec1_lon = vec1_lon + pi2
               endif
               if (vec2_lon >  pi) then
                  vec2_lon = vec2_lon - pi2
               else if (vec2_lon < -pi) then
                  vec2_lon = vec2_lon + pi2
               endif

               cross_product = vec1_lon*vec2_lat - vec2_lon*vec1_lat
               !***
               !*** if the cross product for a side is zero, the point 
               !***   lies exactly on the side or the side is degenerate
               !***   (zero length).  if degenerate, set the cross 
               !***   product to a positive number.  otherwise perform 
               !***   another cross product between the side and the 
               !***   segment itself. 
               !*** if this cross product is also zero, the line is 
               !***   coincident with the cell boundary - perform the 
               !***   dot product and only choose the cell if the dot 
               !***   product is positive (parallel vs anti-parallel).
               !***

               if (cross_product == zero) then
                  if (vec1_lat /= zero .or. vec1_lon /= zero) then
                     vec2_lat = lat2 - lat1
                     vec2_lon = lon2 - lon1

                     if (vec2_lon >  pi) then
                        vec2_lon = vec2_lon - pi2
                     else if (vec2_lon < -pi) then
                        vec2_lon = vec2_lon + pi2
                     endif

                     cross_product = vec1_lon*vec2_lat - vec2_lon*vec1_lat
                  else
                     cross_product = one
                  endif

                  if (cross_product == zero) then
                     lcoinc = .true.
                     cross_product = vec1_lon*vec2_lon + vec1_lat*vec2_lat
                     if (lrevers) cross_product = -cross_product
                  endif
               endif

               !***
               !*** if cross product is less than zero, this cell
               !*** doesn't work
               !***

               if (cross_product < zero) exit corner_loop

            end do corner_loop

            !***
            !*** if cross products all positive, we found the location
            !***

            if (n > srch_corners) then
               location = srch_add(cell)

               !***
               !*** if the beginning of this segment was outside the
               !*** grid, invert the segment so the intersection found
               !*** will be the first intersection with the grid
               !***
               if (loutside) then
                  !*** do a test to see if the cell really is outside 
                  !*** or if it is a non-convexe cell 
                  neg=0
                  pos=0
                  do n=1,srch_corners
                     next_n = mod(n,srch_corners) + 1
                     next2_n = mod(next_n,srch_corners) + 1

                     vec1_lat = grid_corner_lat(next_n,gcell) - grid_corner_lat(n,gcell)
                     vec1_lon = grid_corner_lon(next_n,gcell) - grid_corner_lon(n,gcell)
                     vec2_lat = grid_corner_lat(next2_n,gcell) - grid_corner_lat(next_n,gcell)
                     vec2_lon =  grid_corner_lon(next2_n,gcell) - grid_corner_lon(next_n,gcell)

                     if (vec1_lon > three*pih) then
                        vec1_lon = vec1_lon - pi2
                     else if (vec1_lon < -three*pih) then
                        vec1_lon = vec1_lon + pi2
                     endif

                     if (vec2_lon > three*pih) then
                        vec2_lon = vec2_lon - pi2
                     else if (vec2_lon < -three*pih) then
                        vec2_lon = vec2_lon + pi2
                     endif

                     cross_product = vec1_lat*vec2_lon - vec2_lat*vec1_lon

                     if (cross_product < zero) then
                        neg=neg+1
                     else if (cross_product > zero) then
                        pos=pos+1
                     endif
                  enddo
                  !*** the cell is non-convexe if not all cross_products 
                  !*** have the same signe
                  if (neg/=0 .and. pos/=0) then
                     loutside=.false.
                     if (nlogprt .ge. 2) then
                        write(nulou,*) 'The mesh ',gcell,' is non-convex'
                        write(nulou,*) 'srch_corner_lat=',grid_corner_lat(:,gcell)
                        write(nulou,*) 'srch_corner_lon=',grid_corner_lon(:,gcell)
                        call OASIS_FLUSH_SCRIP(nulou)
                     endif
                  endif
               endif
               if (loutside) then
                  lat2 = beglat
                  lon2 = beglon
                  location = 0
                  eps  = -tiny
               else
                  eps  = tiny
               endif

               exit srch_loop
            endif

            !***
            !*** otherwise move on to next cell
            !***

         end do cell_loop

         !***
         !*** if still no cell found, the point lies outside the grid.
         !***   take some baby steps along the segment to see if any
         !***   part of the segment lies inside the grid.  
         !***

         loutside = .true.
         s1 = s1 + 0.001_dbl_kind
         lat1 = beglat + s1*(endlat - beglat)
         lon1 = beglon + s1*(lon2   - beglon)

         !***
         !*** reached the end of the segment and still outside the grid
         !*** return no intersection
         !***

         if (s1 >= one) return

      end do srch_loop

      !-----------------------------------------------------------------------
      !
      !     now that a cell is found, search for the next intersection.
      !     loop over sides of the cell to find intersection with side
      !     must check all sides for coincidences or intersections
      !
      !-----------------------------------------------------------------------

      intrsct_loop: do n=1,srch_corners
         next_n = mod(n,srch_corners) + 1

         grdlon1 = grid_corner_lon(n     ,gcell)
         grdlon2 = grid_corner_lon(next_n,gcell)
         grdlat1 = grid_corner_lat(n     ,gcell)
         grdlat2 = grid_corner_lat(next_n,gcell)

         !***
         !*** set up linear system to solve for intersection
         !***

         mat1 = lat2 - lat1
         mat2 = grdlat1 - grdlat2
         mat3 = lon2 - lon1
         mat4 = grdlon1 - grdlon2
         rhs1 = grdlat1 - lat1
         rhs2 = grdlon1 - lon1

         if (mat3 >  pi) then
            mat3 = mat3 - pi2
         else if (mat3 < -pi) then
            mat3 = mat3 + pi2
         endif
         if (mat4 >  pi) then
            mat4 = mat4 - pi2
         else if (mat4 < -pi) then
            mat4 = mat4 + pi2
         endif
         if (rhs2 >  pi) then
            rhs2 = rhs2 - pi2
         else if (rhs2 < -pi) then
            rhs2 = rhs2 + pi2
         endif

         determ = mat1*mat4 - mat2*mat3

         !***
         !*** if the determinant is zero, the segments are either 
         !***   parallel or coincident.  coincidences were detected 
         !***   above so do nothing.
         !*** if the determinant is non-zero, solve for the linear 
         !***   parameters s for the intersection point on each line 
         !***   segment.
         !*** if 0<s1,s2<1 then the segment intersects with this side.
         !***   return the point of intersection (adding a small
         !***   number so the intersection is off the grid line).
         !***

         if (abs(determ) > 1.e-30) then

            s1 = (rhs1*mat4 - mat2*rhs2)/determ
            s2 = (mat1*rhs2 - rhs1*mat3)/determ

            !EM bug F77:
            !          if (s2 >= zero .and. s2 <= one .and.
            !     &        s1 >  zero. and. s1 <= one) then

            if (s2 >= zero .and. s2 <= one .and. &
               s1 >  zero .and. s1 <= one) then

               !***
               !*** recompute intersection based on full segment
               !*** so intersections are consistent for both sweeps
               !***

               if (.not. loutside) then
                  mat1 = lat2 - begseg(1)
                  mat3 = lon2 - begseg(2)
                  rhs1 = grdlat1 - begseg(1)
                  rhs2 = grdlon1 - begseg(2)
               else
                  mat1 = begseg(1) - endlat
                  mat3 = begseg(2) - endlon
                  rhs1 = grdlat1 - endlat
                  rhs2 = grdlon1 - endlon
               endif

               if (mat3 >  pi) then
                  mat3 = mat3 - pi2
               else if (mat3 < -pi) then
                  mat3 = mat3 + pi2
               endif
               if (rhs2 >  pi) then
                  rhs2 = rhs2 - pi2
               else if (rhs2 < -pi) then
                  rhs2 = rhs2 + pi2
               endif

               determ = mat1*mat4 - mat2*mat3

               !***
               !*** sometimes due to roundoff, the previous 
               !*** determinant is non-zero, but the lines
               !*** are actually coincident.  if this is the
               !*** case, skip the rest.
               !***

               if (determ /= zero) then
                  s1 = (rhs1*mat4 - mat2*rhs2)/determ
                  s2 = (mat1*rhs2 - rhs1*mat3)/determ

                  offset = s1 + eps/determ
                  if (offset > one) offset = one

                  if (.not. loutside) then
                     intrsct_lat = begseg(1) + mat1*s1
                     intrsct_lon = begseg(2) + mat3*s1
                     intrsct_lat_off = begseg(1) + mat1*offset
                     intrsct_lon_off = begseg(2) + mat3*offset
                  else
                     intrsct_lat = endlat + mat1*s1
                     intrsct_lon = endlon + mat3*s1
                     intrsct_lat_off = endlat + mat1*offset
                     intrsct_lon_off = endlon + mat3*offset
                  endif
                  exit intrsct_loop
               endif

            endif
         endif

         !***
         !*** no intersection this side, move on to next side
         !***

      end do intrsct_loop

      !-----------------------------------------------------------------------
      !
      !     if the segment crosses a pole threshold, reset the intersection
      !     to be the threshold latitude.  only check if this was not a
      !     threshold segment since sometimes coordinate transform can end
      !     up on other side of threshold again.
      !
      !-----------------------------------------------------------------------

      if (lthresh) then
         if (intrsct_lat < north_thresh .or. intrsct_lat > south_thresh) &
            lthresh = .false.
      else if (lat1 > zero .and. intrsct_lat > north_thresh) then
         intrsct_lat = north_thresh + tiny
         intrsct_lat_off = north_thresh + eps*mat1
         s1 = (intrsct_lat - begseg(1))/mat1
         intrsct_lon     = begseg(2) + s1*mat3
         intrsct_lon_off = begseg(2) + (s1+eps)*mat3
         last_loc = location
         lthresh = .true.
      else if (lat1 < zero .and. intrsct_lat < south_thresh) then
         intrsct_lat = south_thresh - tiny
         intrsct_lat_off = south_thresh + eps*mat1
         s1 = (intrsct_lat - begseg(1))/mat1
         intrsct_lon     = begseg(2) + s1*mat3
         intrsct_lon_off = begseg(2) + (s1+eps)*mat3
         last_loc = location
         lthresh = .true.
      endif

      !-----------------------------------------------------------------------

   end subroutine intersection

   !***********************************************************************

   subroutine pole_intersection(location,                                        &
      intrsct_lat,intrsct_lon,lcoinc,lthresh,          &
      beglat, beglon, endlat, endlon, begseg, lrevers, &
      srch_corners, num_srch_cells,                    &
      srch_add, grid_corner_lon, grid_corner_lat)

      !-----------------------------------------------------------------------
      !
      !     this routine is identical to the intersection routine except
      !     that a coordinate transformation (using a Lambert azimuthal
      !     equivalent projection) is performed to treat polar cells more
      !     accurately.
      !
      !-----------------------------------------------------------------------

      !-----------------------------------------------------------------------
      !
      !     intent(in): 
      !
      !-----------------------------------------------------------------------

      real (kind=dbl_kind), intent(in) :: beglat, beglon,  & ! beginning lat/lon endpoints for segment
         endlat, endlon     ! ending    lat/lon endpoints for segment

      real (kind=dbl_kind), dimension(2), intent(inout) :: begseg ! begin lat/lon of full segment

      logical (kind=log_kind), intent(in) :: lrevers   ! flag true if segment integrated in reverse

      integer (kind=int_kind), intent(in) :: srch_corners ! Nb of corners in sarched grid

      integer (kind=int_kind), intent(in) :: num_srch_cells   ! Nb of preselected search cells

      integer (kind=int_kind), dimension(:), intent(in) :: srch_add ! Address of search cells

      real (kind=dbl_kind), dimension(:,:), intent(in) :: grid_corner_lat,&
         & grid_corner_lon ! lat and lon of each corner of srch grids

      !-----------------------------------------------------------------------
      !
      !     intent(out): 
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind), intent(inout) :: location  ! address in destination array containing this
      ! segment -- also may contain last location on
      ! entry

      logical (kind=log_kind), intent(out) :: lcoinc    ! flag segment coincident with grid line

      logical (kind=log_kind), intent(inout) :: lthresh   ! flag segment crossing threshold boundary

      real (kind=dbl_kind), intent(out) :: intrsct_lat, intrsct_lon ! lat/lon coords of next intersect.

      !-----------------------------------------------------------------------
      !
      !     local variables
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind) :: n, next_n, cell, gcell

      logical (kind=log_kind) :: loutside ! flags points outside grid

      real (kind=dbl_kind) :: pi4, rns,           & ! north/south conversion
         x1, x2,             & ! local x variables for segment
         y1, y2,             & ! local y variables for segment
         begx, begy,         & ! beginning x,y variables for segment
         endx, endy,         & ! beginning x,y variables for segment
         begsegx, begsegy,   & ! beginning x,y variables for segment
         grdx1, grdx2,       & ! local x variables for grid cell
         grdy1, grdy2,       & ! local y variables for grid cell
         vec1_y, vec1_x,     & ! vectors and cross products used
         vec2_y, vec2_x,     & ! during grid search
         cross_product, eps, & ! eps=small offset away from intersect
         s1, s2, determ,     & ! variables used for linear solve to
         mat1, mat2, mat3, mat4, rhs1, rhs2  ! find intersection

      real (kind=dbl_kind), dimension(srch_corners) :: srch_corner_x,  & ! x of each corner of srch cells
         srch_corner_y   ! y of each corner of srch cells

      !***
      !*** save last intersection to avoid roundoff during coord
      !*** transformation
      !***

      logical (kind=log_kind), save :: luse_last = .false.

      real (kind=dbl_kind), save :: intrsct_x, intrsct_y  ! x,y for intersection

      !***
      !*** variables necessary if segment manages to hit pole
      !***

      integer (kind=int_kind), save :: avoid_pole_count = 0  ! count attempts to avoid pole

      real (kind=dbl_kind), save :: avoid_pole_offset = tiny  ! endpoint offset to avoid pole
!$OMP THREADPRIVATE(luse_last,intrsct_x,intrsct_y,avoid_pole_count,avoid_pole_offset)

      !-----------------------------------------------------------------------
      !
      !     initialize defaults, flags, etc.
      !
      !-----------------------------------------------------------------------

      if (.not. lthresh) location = 0
      lcoinc = .false.
      intrsct_lat = endlat
      intrsct_lon = endlon

      loutside = .false.
      s1 = zero

      !-----------------------------------------------------------------------
      !
      !     convert coordinates
      !
      !-----------------------------------------------------------------------

      if (beglat > zero) then
         pi4 = quart*pi
         rns = one
      else
         pi4 = -quart*pi
         rns = -one
      endif

      if (luse_last) then
         x1 = intrsct_x
         y1 = intrsct_y
      else
         x1 = rns*two*sin(pi4 - half*beglat)*cos(beglon)
         y1 =     two*sin(pi4 - half*beglat)*sin(beglon)
         luse_last = .true.
      endif
      x2 = rns*two*sin(pi4 - half*endlat)*cos(endlon)
      y2 =     two*sin(pi4 - half*endlat)*sin(endlon)

      begx = x1
      begy = y1
      endx = x2
      endy = y2
      begsegx = rns*two*sin(pi4 - half*begseg(1))*cos(begseg(2))
      begsegy =     two*sin(pi4 - half*begseg(1))*sin(begseg(2))
      intrsct_x = endx
      intrsct_y = endy

      !-----------------------------------------------------------------------
      !
      !     search for location of this segment in ocean grid using cross
      !     product method to determine whether a point is enclosed by a cell
      !
      !-----------------------------------------------------------------------

      srch_loop: do

         !***
         !*** if last segment crossed threshold, use that location
         !***

         if (lthresh) then
            do cell=1,num_srch_cells
               if (srch_add(cell) == location) then
                  eps = tiny
                  exit srch_loop
               endif
            end do
         endif

         !***
         !*** otherwise normal search algorithm
         !***

         cell_loop: do cell=1,num_srch_cells
            gcell = srch_add(cell)
            srch_corner_x(:) = rns*two*sin(pi4 - half*grid_corner_lat(:,gcell)) * cos(grid_corner_lon(:,gcell))
            srch_corner_y(:) =     two*sin(pi4 - half*grid_corner_lat(:,gcell)) * sin(grid_corner_lon(:,gcell))
            corner_loop: do n=1,srch_corners
               next_n = mod(n,srch_corners) + 1

               !***
               !*** here we take the cross product of the vector making 
               !*** up each cell side with the vector formed by the vertex
               !*** and search point.  if all the cross products are 
               !*** positive, the point is contained in the cell.
               !***

               vec1_x = srch_corner_x(next_n) - srch_corner_x(n)
               vec1_y = srch_corner_y(next_n) - srch_corner_y(n)
               vec2_x = x1 - srch_corner_x(n)
               vec2_y = y1 - srch_corner_y(n)

               !***
               !*** if endpoint coincident with vertex, offset
               !*** the endpoint
               !***

               if (vec2_x == 0 .and. vec2_y == 0) then
                  x1 = x1 + 1.d-10*(x2-x1)
                  y1 = y1 + 1.d-10*(y2-y1)
                  vec2_x = x1 - srch_corner_x(n)
                  vec2_y = y1 - srch_corner_y(n)
               endif

               cross_product = vec1_x*vec2_y - vec2_x*vec1_y

               !***
               !*** if the cross product for a side is zero, the point 
               !***   lies exactly on the side or the length of a side
               !***   is zero.  if the length is zero set det > 0.
               !***   otherwise, perform another cross 
               !***   product between the side and the segment itself. 
               !*** if this cross product is also zero, the line is 
               !***   coincident with the cell boundary - perform the 
               !***   dot product and only choose the cell if the dot 
               !***   product is positive (parallel vs anti-parallel).
               !***

               if (cross_product == zero) then
                  if (vec1_x /= zero .or. vec1_y /= 0) then
                     vec2_x = x2 - x1
                     vec2_y = y2 - y1
                     cross_product = vec1_x*vec2_y - vec2_x*vec1_y
                  else
                     cross_product = one
                  endif

                  if (cross_product == zero) then
                     lcoinc = .true.
                     cross_product = vec1_x*vec2_x + vec1_y*vec2_y
                     if (lrevers) cross_product = -cross_product
                  endif
               endif

               !***
               !*** if cross product is less than zero, this cell
               !*** doesn't work
               !***

               if (cross_product < zero) exit corner_loop

            end do corner_loop

            !***
            !*** if cross products all positive, we found the location
            !***

            if (n > srch_corners) then
               location = srch_add(cell)

               !***
               !*** if the beginning of this segment was outside the
               !*** grid, invert the segment so the intersection found
               !*** will be the first intersection with the grid
               !***

               if (loutside) then
                  x2 = begx
                  y2 = begy
                  location = 0
                  eps  = -tiny
               else
                  eps  = tiny
               endif

               exit srch_loop
            endif

            !***
            !*** otherwise move on to next cell
            !***

         end do cell_loop

         !***
         !*** if no cell found, the point lies outside the grid.
         !***   take some baby steps along the segment to see if any
         !***   part of the segment lies inside the grid.  
         !***

         loutside = .true.
         s1 = s1 + 0.001_dbl_kind
         x1 = begx + s1*(x2 - begx)
         y1 = begy + s1*(y2 - begy)

         !***
         !*** reached the end of the segment and still outside the grid
         !*** return no intersection
         !***

         if (s1 >= one) then
            luse_last = .false.
            return
         endif

      end do srch_loop

      !-----------------------------------------------------------------------
      !
      !     now that a cell is found, search for the next intersection.
      !     loop over sides of the cell to find intersection with side
      !     must check all sides for coincidences or intersections
      !
      !-----------------------------------------------------------------------

      gcell = srch_add(cell)
      srch_corner_x(:) = rns*two*sin(pi4 - half*grid_corner_lat(:,gcell)) * cos(grid_corner_lon(:,gcell))
      srch_corner_y(:) =     two*sin(pi4 - half*grid_corner_lat(:,gcell)) * sin(grid_corner_lon(:,gcell))

      intrsct_loop: do n=1,srch_corners
         next_n = mod(n,srch_corners) + 1

         grdy1 = srch_corner_y(n     )
         grdy2 = srch_corner_y(next_n)
         grdx1 = srch_corner_x(n     )
         grdx2 = srch_corner_x(next_n)

         !***
         !*** set up linear system to solve for intersection
         !***

         mat1 = x2 - x1
         mat2 = grdx1 - grdx2
         mat3 = y2 - y1
         mat4 = grdy1 - grdy2
         rhs1 = grdx1 - x1
         rhs2 = grdy1 - y1

         determ = mat1*mat4 - mat2*mat3

         !***
         !*** if the determinant is zero, the segments are either 
         !***   parallel or coincident or one segment has zero length.  
         !***   coincidences were detected above so do nothing.
         !*** if the determinant is non-zero, solve for the linear 
         !***   parameters s for the intersection point on each line 
         !***   segment.
         !*** if 0<s1,s2<1 then the segment intersects with this side.
         !***   return the point of intersection (adding a small
         !***   number so the intersection is off the grid line).
         !***

         if (abs(determ) > 1.e-30) then

            s1 = (rhs1*mat4 - mat2*rhs2)/determ
            s2 = (mat1*rhs2 - rhs1*mat3)/determ

            !EM bug F77
            !          if (s2 >= zero .and. s2 <= one .and. &
            !    &         s1 >  zero. and. s1 <= one) then
            if (s2 >= zero .and. s2 <= one .and. &
               s1 >  zero .and. s1 <= one) then

               !***
               !*** recompute intersection using entire segment
               !*** for consistency between sweeps
               !***

               if (.not. loutside) then
                  mat1 = x2 - begsegx
                  mat3 = y2 - begsegy
                  rhs1 = grdx1 - begsegx
                  rhs2 = grdy1 - begsegy
               else 
                  mat1 = x2 - endx
                  mat3 = y2 - endy
                  rhs1 = grdx1 - endx
                  rhs2 = grdy1 - endy
               endif

               determ = mat1*mat4 - mat2*mat3

               !***
               !*** sometimes due to roundoff, the previous 
               !*** determinant is non-zero, but the lines
               !*** are actually coincident.  if this is the
               !*** case, skip the rest.
               !***

               if (determ /= zero) then
                  s1 = (rhs1*mat4 - mat2*rhs2)/determ
                  s2 = (mat1*rhs2 - rhs1*mat3)/determ

                  if (.not. loutside) then
                     intrsct_x = begsegx + s1*mat1
                     intrsct_y = begsegy + s1*mat3
                  else 
                     intrsct_x = endx + s1*mat1
                     intrsct_y = endy + s1*mat3
                  endif

                  !***
                  !*** convert back to lat/lon coordinates
                  !***

                  intrsct_lon = rns*atan2(intrsct_y,intrsct_x)
                  if (intrsct_lon < zero)  &
                     intrsct_lon = intrsct_lon + pi2

                  if (abs(intrsct_x) > 1.d-10) then
                     intrsct_lat = (pi4 - asin(rns*half*intrsct_x/cos(intrsct_lon)))*two
                  else if (abs(intrsct_y) > 1.d-10) then
                     intrsct_lat = (pi4 - asin(half*intrsct_y/sin(intrsct_lon)))*two
                  else
                     intrsct_lat = two*pi4
                  endif

                  !***
                  !*** add offset in transformed space for next pass.
                  !***

                  if (s1 - eps/determ < one) then
                     intrsct_x = intrsct_x - mat1*(eps/determ)
                     intrsct_y = intrsct_y - mat3*(eps/determ)
                  else
                     if (.not. loutside) then
                        intrsct_x = endx
                        intrsct_y = endy
                        intrsct_lat = endlat
                        intrsct_lon = endlon
                     else 
                        intrsct_x = begsegx
                        intrsct_y = begsegy
                        intrsct_lat = begseg(1)
                        intrsct_lon = begseg(2)
                     endif
                  endif

                  exit intrsct_loop
               endif
            endif
         endif

         !***
         !*** no intersection this side, move on to next side
         !***

      end do intrsct_loop

      !-----------------------------------------------------------------------
      !
      !     if segment manages to cross over pole, shift the beginning 
      !     endpoint in order to avoid hitting pole directly
      !     (it is ok for endpoint to be pole point)
      !
      !-----------------------------------------------------------------------

      if (abs(intrsct_x) < 1.e-10 .and. abs(intrsct_y) < 1.e-10 .and. &
         (endx /= zero .and. endy /=0)) then
         if (avoid_pole_count > 2) then
            avoid_pole_count = 0
            avoid_pole_offset = 10.*avoid_pole_offset
         endif

         cross_product = begsegx*(endy-begsegy) - begsegy*(endx-begsegx)
         intrsct_lat = begseg(1)
         if (cross_product*intrsct_lat > zero) then
            intrsct_lon = beglon    + avoid_pole_offset
            begseg(2)   = begseg(2) + avoid_pole_offset
         else
            intrsct_lon = beglon    - avoid_pole_offset
            begseg(2)   = begseg(2) - avoid_pole_offset
         endif

         avoid_pole_count = avoid_pole_count + 1
         luse_last = .false.
      else
         avoid_pole_count = 0
         avoid_pole_offset = tiny
      endif

      !-----------------------------------------------------------------------
      !
      !     if the segment crosses a pole threshold, reset the intersection
      !     to be the threshold latitude and do not reuse x,y intersect
      !     on next entry.  only check if did not cross threshold last
      !     time - sometimes the coordinate transformation can place a
      !     segment on the other side of the threshold again
      !
      !-----------------------------------------------------------------------

      if (lthresh) then
         if (intrsct_lat > north_thresh .or. intrsct_lat < south_thresh) &
            lthresh = .false.
      else if (beglat > zero .and. intrsct_lat < north_thresh) then
         mat4 = endlat - begseg(1)
         mat3 = endlon - begseg(2)
         if (mat3 >  pi) mat3 = mat3 - pi2
         if (mat3 < -pi) mat3 = mat3 + pi2
         intrsct_lat = north_thresh - tiny
         s1 = (north_thresh - begseg(1))/mat4
         intrsct_lon = begseg(2) + s1*mat3
         luse_last = .false.
         lthresh = .true.
      else if (beglat < zero .and. intrsct_lat > south_thresh) then
         mat4 = endlat - begseg(1)
         mat3 = endlon - begseg(2)
         if (mat3 >  pi) mat3 = mat3 - pi2
         if (mat3 < -pi) mat3 = mat3 + pi2
         intrsct_lat = south_thresh + tiny
         s1 = (south_thresh - begseg(1))/mat4
         intrsct_lon = begseg(2) + s1*mat3
         luse_last = .false.
         lthresh = .true.
      endif

      !***
      !*** if reached end of segment, do not use x,y intersect 
      !*** on next entry
      !***

      if (intrsct_lat == endlat .and. intrsct_lon == endlon) then
         luse_last = .false.
      endif

      !-----------------------------------------------------------------------

   end subroutine pole_intersection

   !***********************************************************************

   subroutine line_integral(weights, num_wts,                 &
      in_phi1, in_phi2, theta1, theta2, &
      grid1_lon, grid2_lon)

      !-----------------------------------------------------------------------
      !
      !     this routine computes the line integral of the flux function 
      !     that results in the interpolation weights.  the line is defined
      !     by the input lat/lon of the endpoints.
      !
      !-----------------------------------------------------------------------

      !-----------------------------------------------------------------------
      !
      !     intent(in):
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) :: num_wts  ! number of weights to compute

      real (kind=dbl_kind), intent(in) :: in_phi1, in_phi2,  &   ! longitude endpoints for the segment
         theta1, theta2,    &   ! latitude  endpoints for the segment
         grid1_lon,         &   ! reference coordinates for each
         grid2_lon              ! grid (to ensure correct 0,2pi interv.

      !-----------------------------------------------------------------------
      !
      !     intent(out):
      !
      !-----------------------------------------------------------------------

      real (kind=dbl_kind), dimension(2*num_wts), intent(out) :: weights   ! line integral contribution to weights

      !-----------------------------------------------------------------------
      !
      !     local variables
      !
      !-----------------------------------------------------------------------

      real (kind=dbl_kind) :: dphi, sinth1, sinth2, costh1, costh2, fac, phi1, phi2, phidiff1, phidiff2
      real (kind=dbl_kind) :: f1, f2, fint

      !-----------------------------------------------------------------------
      !
      !     weights for the general case based on a trapezoidal approx to
      !     the integrals.
      !
      !-----------------------------------------------------------------------

      sinth1 = sin(theta1)
      sinth2 = sin(theta2)
      costh1 = cos(theta1)
      costh2 = cos(theta2)

      dphi = in_phi1 - in_phi2
      if (dphi >  pi) then
         dphi = dphi - pi2
      else if (dphi < -pi) then
         dphi = dphi + pi2
      endif
      dphi = half*dphi

      !-----------------------------------------------------------------------
      !
      !     the first weight is the area overlap integral. the second and
      !     fourth are second-order latitude gradient weights.
      !
      !-----------------------------------------------------------------------

      weights(        1) = dphi*(sinth1 + sinth2)
      weights(num_wts+1) = dphi*(sinth1 + sinth2)
      weights(        2) = dphi*(costh1 + costh2 + (theta1*sinth1 + theta2*sinth2))
      weights(num_wts+2) = dphi*(costh1 + costh2 + (theta1*sinth1 + theta2*sinth2))

      !-----------------------------------------------------------------------
      !
      !     the third and fifth weights are for the second-order phi gradient
      !     component.  must be careful of longitude range.
      !
      !-----------------------------------------------------------------------

      f1 = half*(costh1*sinth1 + theta1)
      f2 = half*(costh2*sinth2 + theta2)

      phi1 = in_phi1 - grid1_lon
      if (phi1 >  pi) then
         phi1 = phi1 - pi2
      else if (phi1 < -pi) then
         phi1 = phi1 + pi2
      endif

      phi2 = in_phi2 - grid1_lon
      if (phi2 >  pi) then
         phi2 = phi2 - pi2
      else if (phi2 < -pi) then
         phi2 = phi2 + pi2
      endif

      if ((phi2-phi1) <  pi .and. (phi2-phi1) > -pi) then
         weights(3) = dphi*(phi1*f1 + phi2*f2)
      else
         if (phi1 > zero) then
            fac = pi
         else
            fac = -pi
         endif
         fint = f1 + (f2-f1)*(fac-phi1)/abs(dphi)
         weights(3) = half*phi1*(phi1-fac)*f1 - &
            half*phi2*(phi2+fac)*f2 + &
            half*fac*(phi1+phi2)*fint
      endif

      phi1 = in_phi1 - grid2_lon
      if (phi1 >  pi) then
         phi1 = phi1 - pi2
      else if (phi1 < -pi) then
         phi1 = phi1 + pi2
      endif

      phi2 = in_phi2 - grid2_lon
      if (phi2 >  pi) then
         phi2 = phi2 - pi2
      else if (phi2 < -pi) then
         phi2 = phi2 + pi2
      endif

      if ((phi2-phi1) <  pi .and. (phi2-phi1) > -pi) then
         weights(num_wts+3) = dphi*(phi1*f1 + phi2*f2)
      else
         if (phi1 > zero) then
            fac = pi
         else
            fac = -pi
         endif
         fint = f1 + (f2-f1)*(fac-phi1)/abs(dphi)
         weights(num_wts+3) = half*phi1*(phi1-fac)*f1 - &
            half*phi2*(phi2+fac)*f2 + &
            half*fac*(phi1+phi2)*fint
      endif

      !-----------------------------------------------------------------------

   end subroutine line_integral

   !***********************************************************************

   subroutine store_link_cnsrv(add1, add2, weights, id_thread)

      !-----------------------------------------------------------------------
      !
      !     this routine stores the address and weight for this link in
      !     the appropriate address and weight arrays and resizes those
      !     arrays if necessary.
      !
      !-----------------------------------------------------------------------

      !-----------------------------------------------------------------------
      !
      !     input variables
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) :: add1,  &  ! address on grid1
                                             add2,  &  ! address on grid2
                                             id_thread ! local threaded task nb

      real (kind=dbl_kind), dimension(:), intent(in) :: weights ! array of remapping weights for this link

      !-----------------------------------------------------------------------
      !
      !     local variables
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind) :: nlink, min_link, max_link ! link index

      integer (kind=int_kind), dimension(:,:), allocatable, save :: link_add1,  & ! min,max link add to restrict search
         link_add2     ! min,max link add to restrict search

      !-----------------------------------------------------------------------
      !
      !     if all weights are zero, do not bother storing the link
      !
      !-----------------------------------------------------------------------

      !SV      if (all(weights == zero)) return

      !-----------------------------------------------------------------------
      !
      !     if the link does not yet exist, increment number of links and 
      !     check to see if remap arrays need to be increased to accomodate 
      !     the new link.  then store the link.
      !
      !-----------------------------------------------------------------------

      if (il_nbthreads .eq. 1) then
        num_links_map1  = num_links_map1 + 1
        if (num_links_map1 > max_links_map1)  &
           call resize_remap_vars(1,resize_increment)

        grid1_add_map1(num_links_map1) = add1
        grid2_add_map1(num_links_map1) = add2
        wts_map1    (:,num_links_map1) = weights(1:num_wts)

      else
        sga_remap(id_thread)%num_links = sga_remap(id_thread)%num_links + 1

         if (sga_remap(id_thread)%num_links > sga_remap(id_thread)%max_links) &
            call sga_remap(id_thread)%resize(int(0.2*real(sga_remap(id_thread)%max_links)))

         sga_remap(id_thread)%grid1_add(sga_remap(id_thread)%num_links) = add1
         sga_remap(id_thread)%grid2_add(sga_remap(id_thread)%num_links) = add2
         sga_remap(id_thread)%wts(1:num_wts,sga_remap(id_thread)%num_links) = weights(1:num_wts)

      endif


      !-----------------------------------------------------------------------

   end subroutine store_link_cnsrv

   !***********************************************************************

#ifdef TREAT_OVERLAY
   !
   ! Modified by A. Piacentini for OASIS in order to compute only the permutation
   ! without modifying the input arrays and to take two arrays (lon and lat) as input
   ! The comparison is made on the lon*(lat-pi) value which is well defined because
   ! SCRIP has forced lon in [0,2pi] and lat in [-pih,pih]
   !
   ! Copyright (C) 2010-2016 Samuel Ponce', Roxana Margine, Carla Verdi, Feliciano Giustino 
   ! Copyright (C) 2007-2009 Jesse Noffsinger, Brad Malone, Feliciano Giustino  
   ! Copyright (C) 2001 PWSCF group
   !                                                                            
   ! This file is distributed under the terms of the GNU General Public         
   ! License. See the file `LICENSE' in the root directory of the               
   ! present distribution, or http://www.gnu.org/copyleft.gpl.txt .             
   !                                                                            
   ! Adapted from flib/hpsort_eps
   !---------------------------------------------------------------------
   subroutine hpsort_eps (n, rlon, rlat, ind, eps)
      !---------------------------------------------------------------------
      ! sort an array ra(1:n) into ascending order using heapsort algorithm,
      ! and considering two elements being equal if their values differ
      ! for less than "eps".
      ! n is input, ra is replaced on output by its sorted rearrangement.
      ! create an index table (ind) by making an exchange in the index array
      ! whenever an exchange is made on the sorted data array (ra).
      ! in case of equal values in the data array (ra) the values in the
      ! index array (ind) are used to order the entries.
      ! if on input ind(1)  = 0 then indices are initialized in the routine,
      ! if on input ind(1) != 0 then indices are assumed to have been
      !                initialized before entering the routine and these
      !                indices are carried around during the sorting process
      !
      ! no work space needed !
      ! free us from machine-dependent sorting-routines !
      !
      ! adapted from Numerical Recipes pg. 329 (new edition)
      !
      use kinds_mod
      implicit none  
      !-input/output variables
      integer, intent(in)   :: n  
      real(kind=dbl_kind), intent(in)  :: eps
      integer :: ind (n)  
      real(kind=dbl_kind) :: rlon (n)
      real(kind=dbl_kind) :: rlat (n)
      !-local variables
      integer :: i, ir, j, l, iind  
      real(kind=dbl_kind) :: rrlon  
      real(kind=dbl_kind) :: rrlat  
      !
      ! initialize index array
      if (ind (1) .eq.0) then  
         do i = 1, n  
            ind (i) = i  
         enddo
      endif
      ! nothing to order
      if (n.lt.2) return  
      ! initialize indices for hiring and retirement-promotion phase
      l = n / 2 + 1  

      ir = n  

      sorting: do 

         ! still in hiring phase
         if ( l .gt. 1 ) then  
            l    = l - 1  
            iind = ind (l)  
            rrlon  = rlon (iind)  
            rrlat  = rlat (iind)  
            ! in retirement-promotion phase.
         else  
            ! clear a space at the end of the array
            iind = ind (ir)  
            rrlon  = rlon (iind)  
            rrlat  = rlat (iind)  
            !
            ! retire the top of the heap into it
            !
            ind (ir) = ind (1)  
            ! decrease the size of the corporation
            ir = ir - 1  
            ! done with the last promotion
            if ( ir .eq. 1 ) then  
               ! the least competent worker at all !
               !
               ind (1) = iind  
               exit sorting  
            endif
         endif
         ! wheter in hiring or promotion phase, we
         i = l  
         ! set up to place rra in its proper level
         j = l + l  
         !
         do while ( j .le. ir )  
            if ( j .lt. ir ) then  
               ! compare to better underling
               if ( hslt( rlon (ind(j)),  rlat (ind(j)),  &
                  &       rlon (ind(j + 1)),  rlat (ind(j + 1)), eps ) ) then  
                  j = j + 1  
               endif
            endif
            ! demote rra
            if ( hslt( rrlon, rrlat, rlon (ind(j)), rlat (ind(j)), eps ) ) then  
               ind (i) = ind (j)  
               i = j  
               j = j + j  
               ! this is the right place for rra
            else
               ! set j to terminate do-while loop
               j = ir + 1  
            endif
         enddo
         ind (i) = iind  

      end do sorting
   end subroutine hpsort_eps

   !  internal function 
   !  compare two coordinates and return the result

   logical function hslt( lo1, la1, lo2, la2, eps )
      real(kind=dbl_kind), intent(in) :: lo1, la1, lo2, la2
      real(kind=dbl_kind), intent(in)  :: eps

      if (abs(la1-la2)<eps) then
         if (abs(lo1-lo2)<eps) then
            hslt = .false.
         else
            hslt = (lo1 < lo2)
         end if
      else
         hslt = (la1 < la2)
      end if
   end function hslt

   !

#endif
end module remap_conservative

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
