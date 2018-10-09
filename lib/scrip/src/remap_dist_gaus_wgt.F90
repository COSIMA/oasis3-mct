!****
!                    ************************
!                    *     OASIS MODULE     *
!                    *     ------------     *
!                    ************************
!**** 
!**********************************************************************
!     This module belongs to the SCRIP library. It is modified to run
!     within OASIS. 
!     Modifications:
!       - restrict types are written in capital letters
!       - bug line 386: bin_lons(3,n) instead of bin_lons(2,n)
!
!     Modified by         V. Gayler,      M&D              20.09.2001
!     Modified by         D. Declat,      CERFACS          27.06.2002
!     Modified by         E. Maisonnave,  CERFACS          21.12.2017
!     Modified by         A. Piacentini,  CERFACS          21.12.2017
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     this module contains necessary routines for performing an 
!     interpolation using a distance-weighted average of n nearest
!     neighbors.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: remap_dist_gaus_wgt.F 2826 2010-12-10 11:14:21Z valcke $
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

module remap_distance_gaussian_weight 

   !-----------------------------------------------------------------------

   use kinds_mod     ! defines common data types
   use constants     ! defines common constants
   use grids         ! module containing grid info
   use remap_vars    ! module containing remap info
   use timers
   use mod_oasis_flush
!$ use omp_lib

   implicit none

   !-----------------------------------------------------------------------
   !
   !     module variables
   !
   !-----------------------------------------------------------------------

   real (kind=dbl_kind), dimension(:), allocatable, save ::  &
      coslat, sinlat,  & ! cosine, sine of grid lats (for distance) 
      coslon, sinlon,  & ! cosine, sine of grid lons (for distance) 
      wgtstmp            ! an array to hold the link weight

   logical, parameter :: ll_nnei=.true.

   integer (kind=int_kind) :: il_nbthreads = 1

   !***********************************************************************

contains

   !***********************************************************************

   subroutine remap_dist_gaus_wgt (lextrapdone, num_neighbors, &
                                   mpi_comm_map, mpi_size_map, mpi_rank_map, mpi_root_map, &
                                   r_varmul )

      !-----------------------------------------------------------------------
      !
      !     this routine computes the inverse-distance weights for a
      !     nearest-neighbor interpolation.
      !
      !-----------------------------------------------------------------------
      !-----------------------------------------------------------------------
      !
      !     input variables
      !
      !-----------------------------------------------------------------------

      logical :: lextrapdone   ! logical, true if EXTRAP done on field

      real (kind=dbl_kind),optional :: r_varmul          ! Gaussian variance

      integer (kind=int_kind) :: num_neighbors     ! number of neighbours

      integer (kind=int_kind) :: mpi_comm_map, mpi_rank_map, mpi_size_map, mpi_root_map

      !-----------------------------------------------------------------------
      !
      !     local variables
      !
      !-----------------------------------------------------------------------

      logical (kind=log_kind), dimension(num_neighbors) :: nbr_mask ! mask at nearest neighbors

      logical (kind=log_kind) :: ll_debug  ! for debug outputs

      integer (kind=int_kind) :: n,            &
                                 dst_add,      &  ! destination address
                                 nmap,         &  ! index of current map being computed
                                 icount           ! number of non masked nearest neighbour

      integer (kind=int_kind), dimension(num_neighbors) :: nbr_add  ! source address at nearest neighbors

      real (kind=dbl_kind), dimension(num_neighbors) ::   nbr_dist  ! angular distance four nearest neighbors

      real (kind=dbl_kind) :: coslat_dst,   &  ! cos(lat) of destination grid point
                              coslon_dst,   &  ! cos(lon) of destination grid point
                              sinlat_dst,   &  ! sin(lat) of destination grid point
                              sinlon_dst,   &  ! sin(lon) of destination grid point
                              dist_tot,     &  ! sum of neighbor distances (for normalizing)
                              dist_average     ! average distance between the source points

      real (kind=dbl_kind) :: dl_test

      real (kind=dbl_kind) :: distance, plat, plon, src_latsnn, dl_dist   !  angular distance
      real (kind=dbl_kind), dimension (1) :: wgts_new
      real (kind=dbl_kind) :: rl_varmul                                   ! local Gaussian variance
      real (kind=dbl_kind) :: rl_mean_variance                            ! local mean variance


      integer (kind=int_kind) :: src_addnn, il_debug_add

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
      integer (kind=int_kind) :: il_envthreads, il_err
      logical :: ll_gaussian_weights
#ifdef REMAP_TIMING
      logical :: ll_timing = .true.
      real    (kind=8), dimension(:,:), allocatable :: dla_timer
      real    (kind=8)        :: dl_tstart, dl_tstop
      integer (kind=int_kind) :: il_mythread
#endif

      !-----------------------------------------------------------------------
      !
      if (nlogprt .ge. 2) then
         write (UNIT = nulou,FMT = *)' '
         write (UNIT = nulou,FMT = *)'Entering routine remap_dist_gaus_wgt'
         call OASIS_FLUSH_SCRIP(nulou)
      endif
      !
      !-----------------------------------------------------------------------
      !
      !     compute mappings from grid1 to grid2
      !
      !-----------------------------------------------------------------------
#ifdef REMAP_TIMING
      if (ll_timing) call timer_start(2,'remap_dist_gaus_wgt alloc-res_ave')
#endif

      if (PRESENT(r_varmul )) then
        ll_gaussian_weights = .true.
        ! Check that variance is not zero
        IF ( r_varmul < epsilon(1.) ) then
          write (UNIT = nulou,FMT = *) 'Namcouple GAUSWGT variance $VAR cannot be zero'
          call OASIS_FLUSH_SCRIP(nulou)
          stop
        ENDIF
        rl_varmul = r_varmul
      else
        ll_gaussian_weights = .false.
        rl_varmul = 0.
      endif

      dl_test = 0.0
      nmap = 1

      !***
      !*** allocate wgtstmp to be consistent with store_link interface
      !***

      allocate (wgtstmp(num_wts))

      !***
      !*** compute cos, sin of lat/lon on source grid for distance
      !*** calculations
      !***

      allocate (coslat(grid1_size), coslon(grid1_size), sinlat(grid1_size), sinlon(grid1_size))
      coslat = cos(grid1_center_lat)
      coslon = cos(grid1_center_lon)
      sinlat = sin(grid1_center_lat)
      sinlon = sin(grid1_center_lon)

      if ( ll_gaussian_weights ) then
      ! EM: dist_average wrong if U
      ! EM: NOT PARALLEL FOR THE MOMENT
      ! EM: TO BE INVESTIGATED LATER
        call grid_dist_average(grid1_size, coslat, coslon, sinlat, sinlon, dist_average)
        rl_mean_variance = rl_varmul * dist_average * dist_average
      endif

#ifdef REMAP_TIMING
      if (ll_timing) call timer_stop(2)
#endif

      !***
      !*** precompute best scheduling of target grid points
      !***

      allocate(ila_mpi_mn(mpi_size_map))
      allocate(ila_mpi_mx(mpi_size_map))

      if (mpi_size_map .gt. 1) then

         allocate(ila_mpi_sz(mpi_size_map))
         il_splitsize = count(grid2_mask)
         ila_mpi_sz(:) = floor(real(il_splitsize)/mpi_size_map)
         ila_mpi_sz(1:il_splitsize-sum(ila_mpi_sz)) = ila_mpi_sz(1:il_splitsize-sum(ila_mpi_sz)) + 1

         ila_mpi_mn(:) = 0
         ib_proc = 1
         il_splitsize = 0
         do dst_add = 1, grid2_size
            if (grid2_mask(dst_add)) then
               if (ila_mpi_mn(ib_proc).eq.0) &
                  ila_mpi_mn(ib_proc) = dst_add 
               il_splitsize = il_splitsize + 1
               if (il_splitsize .eq. ila_mpi_sz(ib_proc)) then
                  il_splitsize = 0
                  ila_mpi_mx(ib_proc) = dst_add
                  ib_proc = ib_proc + 1
               end if
            end if
         end do

         deallocate(ila_mpi_sz)

      else

         ila_mpi_mn(1) = 1
         ila_mpi_mx(1) = grid2_size

      endif


      call get_environment_variable(name='OASIS_OMP_NUM_THREADS', &
         & value=cl_envvar, status=il_err)
      if ( il_err .ne. 0) then
         il_envthreads = 0
      else
         read(cl_envvar,*) il_envthreads
      end if

!$OMP PARALLEL NUM_THREADS(il_envthreads) DEFAULT(NONE) &
!$OMP SHARED(il_envthreads) &
!$OMP SHARED(lextrapdone,num_neighbors) &
!$OMP SHARED(grid2_mask,grid2_frac) &
!$OMP SHARED(grid2_center_lat,grid2_center_lon) &
!$OMP SHARED(grid1_mask) &
!$OMP SHARED(bin_addr1,nulou,sga_remap,num_wts) &
!$OMP SHARED(nmap,dl_test) &
!$OMP PRIVATE(nlogprt) &
!$OMP SHARED(coslat,coslon,sinlat,sinlon) &
!$OMP PRIVATE(nbr_mask,wgtstmp,dst_add,icount,nbr_add,nbr_dist) &
!$OMP PRIVATE(coslat_dst,coslon_dst,sinlat_dst,sinlon_dst,dist_tot) &
!$OMP PRIVATE(ll_debug,src_addnn, il_debug_add) &
!$OMP SHARED(il_nbthreads) &
!$OMP SHARED(mpi_rank_map,mpi_root_map,ila_mpi_mn,ila_mpi_mx) &
!$OMP SHARED(ila_thr_sz,ila_thr_mn,ila_thr_mx) &
!$OMP SHARED(ll_gaussian_weights,rl_mean_variance) &
#ifdef REMAP_TIMING
!$OMP PRIVATE(ib_thread,il_splitsize) &
!$OMP SHARED(ll_timing,dla_timer) &
!$OMP PRIVATE(il_mythread,dl_tstart,dl_tstop)

!$    il_mythread = OMP_GET_THREAD_NUM () + 1
#else
!$OMP PRIVATE(ib_thread,il_splitsize)
#endif

!$OMP SINGLE

      il_nbthreads = 1
!$    il_nbthreads = OMP_GET_NUM_THREADS ()

#ifdef REMAP_TIMING
      if (ll_timing) then
         if (il_nbthreads.gt.1) then
!$          dl_tstart = OMP_GET_WTIME()
         else
            call timer_start(3,'remap_dist_gaus_wgt distr')
         end if
      end if
#endif
      allocate(ila_thr_mn(il_nbthreads))
      allocate(ila_thr_mx(il_nbthreads))

      if (il_nbthreads .gt. 1) then

#ifdef REMAP_TIMING
         if (ll_timing) then
            allocate(dla_timer(il_nbthreads,4))
            dla_timer(:,:) = 0.0
         end if
#endif
         nlogprt = 0

         allocate(ila_thr_sz(il_nbthreads))
         il_splitsize = COUNT(grid2_mask(ila_mpi_mn(mpi_rank_map+1):&
            & ila_mpi_mx(mpi_rank_map+1)))
         ila_thr_sz(:) = floor(real(il_splitsize)/il_nbthreads)
         ila_thr_sz(1:il_splitsize-sum(ila_thr_sz)) = ila_thr_sz(1:il_splitsize-sum(ila_thr_sz)) + 1

         ila_thr_mn(:) = 0
         ib_thread = 1
         il_splitsize = 0
         DO dst_add = ila_mpi_mn(mpi_rank_map+1), ila_mpi_mx(mpi_rank_map+1)
            if (grid2_mask(dst_add)) then
               if (ila_thr_mn(ib_thread).eq.0) &
                  ila_thr_mn(ib_thread) = dst_add 
               il_splitsize = il_splitsize + 1
               if (il_splitsize .eq. ila_thr_sz(ib_thread)) then
                  il_splitsize = 0
                  ila_thr_mx(ib_thread) = dst_add
                  ib_thread = ib_thread + 1
               end if
            end if
         end do

         allocate(sga_remap(il_nbthreads))

         do ib_thread = 1, il_nbthreads
            il_splitsize = num_neighbors*ila_thr_sz(ib_thread)
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
#ifdef REMAP_TIMING
      if (ll_timing) then
         if (il_nbthreads.gt.1) then
!$          dl_tstop = OMP_GET_WTIME() 
            dla_timer(il_mythread,1)=dla_timer(il_mythread,1) + dl_tstop - dl_tstart
         else
            call timer_stop(3)
         end if
      end if
#endif
!$OMP END SINGLE


      !***
      !*** loop over destination grid 
      !***
!$OMP DO SCHEDULE(STATIC,1)
      thread_loop: do ib_thread = 1, il_nbthreads

         grid_loop1: do dst_add =ila_thr_mn(ib_thread), ila_thr_mx(ib_thread)

            if (.not. grid2_mask(dst_add)) cycle grid_loop1

#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstart = OMP_GET_WTIME()
               else
                  call timer_start(4,'remap_dist_gaus_wgt search')
               end if
            end if
#endif
            coslat_dst = cos(grid2_center_lat(dst_add))
            coslon_dst = cos(grid2_center_lon(dst_add))
            sinlat_dst = sin(grid2_center_lat(dst_add))
            sinlon_dst = sin(grid2_center_lon(dst_add))

            !***
            !*** find nearest grid points on source grid
            !*** or non masked nearest neighbour
            !*** and distances to each point
            !***

            call grid_search_nbr(src_addnn, nbr_add, nbr_dist,                   &
                                 grid2_center_lat(dst_add),                      &
                                 grid2_center_lon(dst_add),                      &
                                 coslat_dst, coslon_dst, sinlat_dst, sinlon_dst, &
                                 bin_addr1, num_neighbors, lextrapdone, dst_add)

            ll_debug = .false.
            if (ll_debug) then
               il_debug_add = 15248
               if (dst_add == il_debug_add) then 
                  write(nulou,*) 'nbr_add = ', nbr_add(:)
                  write(nulou,*) 'nbr_dist = ', nbr_dist(:)
                  call OASIS_FLUSH_SCRIP(nulou)
               endif
            endif

#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstop = OMP_GET_WTIME() 
                  dla_timer(ib_thread,2) = dla_timer(ib_thread,2) + dl_tstop - dl_tstart
               else
                  call timer_stop(4)
               end if
            end if
#endif
            !***
            !*** compute weights based on inverse distance
            !*** if mask is false, eliminate those points
            !***

#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstart = OMP_GET_WTIME()
               else
                  call timer_start(5,'remap_dist_gaus_wgt weights')
               end if
            end if
#endif
            dist_tot = zero
            do n=1,num_neighbors
               if (grid1_mask(nbr_add(n)) .or. lextrapdone) then
                 !
                 ! Change distance if gausswgt option
                 if ( ll_gaussian_weights  ) &
                   nbr_dist(n) = exp(.5*nbr_dist(n)*nbr_dist(n)/rl_mean_variance)
                 nbr_dist(n) = one/(nbr_dist(n) + epsilon(dl_test))
                 dist_tot = dist_tot + nbr_dist(n)
                 nbr_mask(n) = .true.
               else
                 nbr_mask(n) = .false.
               endif
            end do

            if (ll_debug) then
               if (dst_add == il_debug_add) then 
                  write(nulou,*) 'nbr_mask = ', nbr_mask(:)
                  write(nulou,*) 'nbr_dist = ', nbr_dist(:)
                  write(nulou,*) 'dist_tot = ', dist_tot
               endif
            endif

#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstop = OMP_GET_WTIME() 
                  dla_timer(ib_thread,3) = dla_timer(ib_thread,3) + dl_tstop - dl_tstart
               else
                  call timer_stop(5)
               end if
            end if
#endif
            !***
            !*** normalize weights and store the link
            !***

#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstart = OMP_GET_WTIME()
               else
                  call timer_start(6,'remap_dist_gaus_wgt store_link')
               end if
            end if
#endif
            icount = 0
            do n=1,num_neighbors
               if (nbr_mask(n)) then
                  wgtstmp(1) = nbr_dist(n)/dist_tot
                  if (ll_debug) then       
                     if (dst_add == il_debug_add) then 
                        write(nulou,*) 'wgtstmp = ', wgtstmp(1)
                        write(nulou,*) 'nbr_dist = ', nbr_dist(:)
                     endif
                  endif
                  call store_link_nbr(nbr_add(n), dst_add, wgtstmp, nmap, ib_thread)
                  grid2_frac(dst_add) = one
                  icount = icount + 1
               endif
            end do
            if (ll_nnei) then
               if (icount == 0) then
                  if (nlogprt .ge. 2) then
                     write(nulou,*) '    '
                     write(nulou,*) 'Using the nearest non-masked neighbour because all 4 &
                        & surrounding source points are masked for target point ',dst_add
                     call OASIS_FLUSH_SCRIP(nulou)
                  endif
                  wgtstmp(1) = 1.
                  grid2_frac(dst_add) = one
                  call store_link_nbr(src_addnn, dst_add, wgtstmp, nmap, ib_thread)
               endif
            endif
#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstop = OMP_GET_WTIME() 
                  dla_timer(ib_thread,4) = dla_timer(ib_thread,4) + dl_tstop - dl_tstart
               else
                  call timer_stop(6)
               end if
            end if
#endif
         end do grid_loop1

      end do thread_loop
!$OMP END DO

!$OMP END PARALLEL

      if (il_nbthreads .gt. 1) then
#ifdef REMAP_TIMING
         if (ll_timing) call timer_start(3,'remap_dist_gaus_wgt gather_lk')
#endif
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
               sga_remap(ib_thread)%grid1_add
            grid2_add_map1(sga_remap(ib_thread)%start_pos: &
               sga_remap(ib_thread)%start_pos+             &
               sga_remap(ib_thread)%num_links-1) =         &
               sga_remap(ib_thread)%grid2_add
            wts_map1     (:,sga_remap(ib_thread)%start_pos: &
               sga_remap(ib_thread)%start_pos+            &
               sga_remap(ib_thread)%num_links-1) =        &
               sga_remap(ib_thread)%wts

         end do

#ifdef REMAP_TIMING
         if (ll_timing) call timer_stop(3)
#endif
         if (nlogprt.ge.2) then

            do ib_thread = 1, il_nbthreads
               if (sga_remap(ib_thread)%nb_resize.gt.0) then
                  write(nulou,*) ' Number of thread_resize_remap_vars on thread ',&
                     ib_thread, ' = ', sga_remap(ib_thread)%nb_resize
               end if
            end do

         end if

#ifdef REMAP_TIMING
         if (ll_timing.and.nlogprt.ge.2) then
            write(nulou,*) ' On master thread '
            call timer_print(2)
            call timer_clear(2)
            do ib_thread = 1,il_nbthreads
               write(nulou,*) ' On thread ',ib_thread
               write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_dist_gaus_wgt distr     ',dla_timer(ib_thread,1)
               write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_dist_gaus_wgt search    ',dla_timer(ib_thread,2)
               write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_dist_gaus_wgt weights   ',dla_timer(ib_thread,3)
               write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_dist_gaus_wgt store_link',dla_timer(ib_thread,4)
            end do
            deallocate(dla_timer)
            write(nulou,*) ' On master thread '
            call timer_print(3)
            call timer_clear(3)
         end if

      else

         if (ll_timing.and.nlogprt.ge.2) then
            do n = 2, 6
               call timer_print(n)
               call timer_clear(n)
            end do
         end if
#endif
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

         
         IF (mpi_rank_map == mpi_root_map) THEN
            num_links_map1 = SUM(ila_num_links_mpi)
            if (num_links_map1 > max_links_map1) &
                  call resize_remap_vars(1,num_links_map1-max_links_map1)
            
            ALLOCATE(ila_req_mpi(4,mpi_size_map-1))
            ALLOCATE(ila_sta_mpi(MPI_STATUS_SIZE,4,mpi_size_map-1))

            DO n = 1, mpi_size_map-1
               buff_base = SUM(ila_num_links_mpi(1:n))+1
               CALL MPI_IRecv(grid1_add_map1(buff_base),&
                  & ila_num_links_mpi(n+1),MPI_INT,n,1,mpi_comm_map,&
                  & ila_req_mpi(1,n),il_err)

               CALL MPI_IRecv(grid2_add_map1(buff_base),&
                  & ila_num_links_mpi(n+1),MPI_INT,n,2,mpi_comm_map,&
                  & ila_req_mpi(2,n),il_err)

               CALL MPI_IRecv(wts_map1(1,buff_base),&
                  & num_wts*ila_num_links_mpi(n+1),MPI_DOUBLE,n,0,mpi_comm_map,&
                  & ila_req_mpi(3,n),il_err)

               CALL MPI_IRecv(grid2_frac(ila_mpi_mn(n+1)),&
                  & ila_mpi_mx(n+1)-ila_mpi_mn(n+1)+1,MPI_DOUBLE,n,0,mpi_comm_map,&
                  & ila_req_mpi(4,n),il_err)

            END DO

            DO n=1,4
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
               & mpi_root_map,0,mpi_comm_map,il_err)

            CALL MPI_Send(grid2_frac(ila_mpi_mn(mpi_rank_map+1)),&
               & ila_mpi_mx(mpi_rank_map+1)-ila_mpi_mn(mpi_rank_map+1)+1,MPI_DOUBLE,&
               & mpi_root_map,0,mpi_comm_map,il_err)


         END IF




         ! Reduction (SUM) of  num_links_map1 on master proc
         ! Most probably it will be better to gather the single num_links_map1
         ! Because they are useful for storing the gatherv or Irecv results
         !
         ! if (ll_master_remap) then
         !    allocate(ila_num_links_mpi(mpi_size_map))
         !    !! MPI receive the num_links_map1 of every proc in right pos
         !    num_links_map1 = SUM(ila_num_links_mpi)
         !    if (num_links_map1 > max_links_map1) &
         !         call resize_remap_vars(1,num_links_map1-max_links_map1)
         ! else
         !    !! MPI send of num_links_map1
         ! end IF
         !
         !
         ! Gather (ou similar) with positions derived from partial sums
         ! of ila_num_links mpi :
         !grid1_add_map1
         !grid2_add_map1
         !wts_map1
         !
         ! Gather (ou reduction sum) avec attention au decalage d'index de
         !grid2_frac
         !
         ! if (ll_master_remap) deallocate(ila_num_links_mpi)

         deallocate(ila_num_links_mpi)

      end if


      deallocate (coslat, coslon, sinlat, sinlon)
      deallocate(wgtstmp)
      deallocate(ila_mpi_mn)
      deallocate(ila_mpi_mx)
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
      if (nlogprt .ge. 2) then
         write (UNIT = nulou,FMT = *)' '
         write (UNIT = nulou,FMT = *)'Leaving routine remap_dist_gaus_wgt'
         call OASIS_FLUSH_SCRIP(nulou)
      endif
      !
      !-----------------------------------------------------------------------

   end subroutine remap_dist_gaus_wgt

   !***********************************************************************

   subroutine grid_search_nbr(src_addnn, nbr_add, nbr_dist, plat, plon,               &
                              coslat_dst, coslon_dst, sinlat_dst, sinlon_dst,         &
                              src_bin_add, num_neighbors, lextrapdone, dst_add )

      !-----------------------------------------------------------------------
      !
      !     this routine finds the closest num_neighbor points to a search 
      !     point and computes a distance to each of the neighbors.
      !
      !-----------------------------------------------------------------------

      !-----------------------------------------------------------------------
      !
      !     input variables
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind), dimension(:,:), intent(in) :: src_bin_add ! search bins for restricting search

      integer (kind=int_kind), intent(in) :: dst_add

      real (kind=dbl_kind), intent(in) :: plat,        & ! latitude  of the search point
                                          plon,        & ! longitude of the search point
                                          coslat_dst,  & ! cos(lat)  of the search point
                                          coslon_dst,  & ! cos(lon)  of the search point
                                          sinlat_dst,  & ! sin(lat)  of the search point
                                          sinlon_dst     ! sin(lon)  of the search point

      logical, intent(in)              :: lextrapdone     ! logical, true if EXTRAP done on field

      integer (kind=int_kind) ::  num_neighbors     ! number of neighbours

      logical :: ll_debug


      !-----------------------------------------------------------------------
      !
      !     output variables
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind), dimension(num_neighbors), intent(out) :: nbr_add  ! address of each of the closest points

      real (kind=dbl_kind), dimension(num_neighbors), intent(out) :: nbr_dist    ! distance to each of the closest points

      integer (kind=int_kind), intent(out) :: src_addnn

      !-----------------------------------------------------------------------
      !
      !     local variables
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind) :: n, nmax, nadd, nchk, & ! dummy indices
                                 nm1, np1, i, j, ip1, im1, jp1, jm1, &
                                 min_add, max_add,                   &
                                 il_debug_add

      real (kind=dbl_kind) :: distance, rl_dist, src_latsnn      ! angular distance

      !-----------------------------------------------------------------------
      !
      !     loop over source grid and find nearest neighbors
      !
      !-----------------------------------------------------------------------

      !***
      !*** restrict the search using search bins
      !*** expand the bins to catch neighbors
      !***

      select case (restrict_type)
      case('LATITUDE')

         do n=1,num_srch_bins
            if (plat >= bin_lats(1,n) .and. plat <= bin_lats(2,n)) then
               min_add = src_bin_add(1,n)
               max_add = src_bin_add(2,n)

               nm1 = max(n-1,1)
               np1 = min(n+1,num_srch_bins)

               min_add = min(min_add,src_bin_add(1,nm1))
               max_add = max(max_add,src_bin_add(2,nm1))
               min_add = min(min_add,src_bin_add(1,np1))
               max_add = max(max_add,src_bin_add(2,np1))
            endif
         end do

      case('LATLON')

         n = 0
         nmax = nint(sqrt(real(num_srch_bins)))
         do j=1,nmax
            jp1 = min(j+1,nmax)
            jm1 = max(j-1,1)
            do i=1,nmax
               ip1 = min(i+1,nmax)
               im1 = max(i-1,1)

               n = n+1
               if (plat >= bin_lats(1,n) .and. plat <= bin_lats(2,n) .and.  &
                  plon >= bin_lons(1,n) .and. plon <= bin_lons(2,n)) then
                  min_add = src_bin_add(1,n)
                  max_add = src_bin_add(2,n)

                  nm1 = (jm1-1)*nmax + im1
                  np1 = (jp1-1)*nmax + ip1
                  nm1 = max(nm1,1)
                  np1 = min(np1,num_srch_bins)

                  min_add = min(min_add,src_bin_add(1,nm1))
                  max_add = max(max_add,src_bin_add(2,nm1))
                  min_add = min(min_add,src_bin_add(1,np1))
                  max_add = max(max_add,src_bin_add(2,np1))
               endif
            end do
         end do

      end select

      !***
      !*** initialize distance and address arrays
      !***

      nbr_add = 0
      nbr_dist = bignum
      src_latsnn = bignum
      src_addnn = 0

      do nadd=min_add,max_add

         !***
         !*** find distance to this point
         !***
         rl_dist = sinlat_dst*sinlat(nadd) +  &
                   coslat_dst*coslat(nadd)*   &
                   (coslon_dst*coslon(nadd) + &
                    sinlon_dst*sinlon(nadd))
         if (rl_dist < -1.0d0) then
            rl_dist = -1.0d0
         else if (rl_dist > 1.0d0) then
            rl_dist = 1.0d0
         end if
         distance = acos(rl_dist)
         ll_debug = .false.
         if (ll_debug) then
            il_debug_add = 15248
            if (dst_add == il_debug_add) then
               write(nulou,1009) nadd, distance
            endif
         endif

         !*** store the address and distance if this is one of the
         !*** smallest four so far
         !***

         check_loop: do nchk=1,num_neighbors
            if (distance .lt. nbr_dist(nchk)) then
               do n=num_neighbors,nchk+1,-1
                  nbr_add(n) = nbr_add(n-1)
                  nbr_dist(n) = nbr_dist(n-1)
               end do
               nbr_add(nchk) = nadd
               nbr_dist(nchk) = distance
               exit check_loop
            endif
         end do check_loop

         if (ll_debug) then
            if (dst_add == il_debug_add) then
               write(nulou,1010) nadd, distance
            endif
         endif

         if (ll_nnei) then
            !***
            !*** store the non-masked closest neighbour
            !***
            if (distance < src_latsnn) then
               if (grid1_mask(nadd) .or. lextrapdone) then
                  src_addnn = nadd
                  src_latsnn = distance
               endif
               if (ll_debug) then
                  if (dst_add == il_debug_add) then
                     write(nulou,1010) nadd, distance
                  endif
               endif
            endif
         endif
      end do

1009  format (1X, 'nadd0 =', 1X, I6, 2X, 'distance0 =', 1X, F18.16)
1010  format (1X, 'nadd1 =', 1X, I6, 2X, 'distance0 =', 1X, F18.16)
1011  format (1X, 'nadd2 =', 1X, I6, 2X, 'distance2 =', 1X, F18.16)

      !-----------------------------------------------------------------------

   end subroutine grid_search_nbr

   !***********************************************************************

   subroutine store_link_nbr(add1, add2, weights, nmap, id_thread)

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

      integer (kind=int_kind), intent(in) :: add1,  &   ! address on grid1
                                             add2,  &   ! address on grid2
                                             nmap,  &   ! identifies which direction for mapping
                                             id_thread  ! local threaded task

      real (kind=dbl_kind), dimension(:), intent(in) :: weights ! array of remapping weights for this link

      !-----------------------------------------------------------------------
      !
      !     increment number of links and check to see if remap arrays need
      !     to be increased to accomodate the new link.  then store the
      !     link.
      !
      !-----------------------------------------------------------------------

      if (il_nbthreads .eq. 1) then


         select case (nmap)
         case(1)

            num_links_map1  = num_links_map1 + 1

            if (num_links_map1 > max_links_map1)  &
               call resize_remap_vars(1,resize_increment)

            grid1_add_map1(num_links_map1) = add1
            grid2_add_map1(num_links_map1) = add2
            wts_map1    (:,num_links_map1) = weights

         case(2)

            num_links_map2  = num_links_map2 + 1

            if (num_links_map2 > max_links_map2) &
               call resize_remap_vars(2,resize_increment)

            grid1_add_map2(num_links_map2) = add1
            grid2_add_map2(num_links_map2) = add2
            wts_map2    (:,num_links_map2) = weights

         end select

      else

         sga_remap(id_thread)%num_links = sga_remap(id_thread)%num_links + 1

         if (sga_remap(id_thread)%num_links > sga_remap(id_thread)%max_links) &
            call sga_remap(id_thread)%resize(int(0.2*real(sga_remap(id_thread)%max_links)))

         sga_remap(id_thread)%grid1_add(sga_remap(id_thread)%num_links) = add1
         sga_remap(id_thread)%grid2_add(sga_remap(id_thread)%num_links) = add2
         sga_remap(id_thread)%wts(:,sga_remap(id_thread)%num_links)     = weights

      endif

      !-----------------------------------------------------------------------

   end subroutine store_link_nbr

   !***********************************************************************

   subroutine grid_dist_average(grid_size,                 &
                                coslat_grid, coslon_grid,  &
                                sinlat_grid, sinlon_grid,  &
                                dist_average)

!-----------------------------------------------------------------------
!
!     this routine computes the average distance between the points of a
!     grid.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     output variables
!
!-----------------------------------------------------------------------

      real (kind=dbl_kind), intent(out) :: dist_average ! distance to each of the closest points

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) :: grid_size

      real (kind=dbl_kind), dimension(:), intent(in) ::  &
              coslat_grid,   & ! cos(lat)  of the grid points
              coslon_grid,   & ! cos(lon)  of the grid points
              sinlat_grid,   & ! sin(lat)  of the grid points
              sinlon_grid      ! sin(lon)  of the grid points
      REAL (kind=dbl_kind) :: distance

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: i
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *) 'Entering routine remap_dist_average'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     compute the distance over the grid and average
!
!-----------------------------------------------------------------------

      dist_average = 0.0
      distance = sinlat_grid(grid_size)*sinlat_grid(1) +   &
                 coslat_grid(grid_size)*coslat_grid(1)*    &
                 (coslon_grid(grid_size)*coslon_grid(1) +  &
                  sinlon_grid(grid_size)*sinlon_grid(1))
      IF (distance < -1.0d0) THEN
        distance = -1.0d0
      ELSE IF (distance > 1.0d0) THEN
        distance = 1.0d0
      END IF
      dist_average = dist_average + acos(distance)
      !
      DO i = 2, grid_size
        distance = sinlat_grid(i-1)*sinlat_grid(i) +         &
                   coslat_grid(i-1)*coslat_grid(i)*          &
                   (coslon_grid(i-1)*coslon_grid(i) +        &
                   sinlon_grid(i-1)*sinlon_grid(i))
        IF (distance < -1.0d0) THEN
          distance = -1.0d0
        ELSE IF (distance > 1.0d0) THEN
          distance = 1.0d0
        END IF
        dist_average = dist_average + acos(distance)
      END DO
      !
      dist_average = dist_average / grid_size
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *) 'Leaving routine remap_dist_average'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
      END subroutine grid_dist_average

end module remap_distance_gaussian_weight

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



