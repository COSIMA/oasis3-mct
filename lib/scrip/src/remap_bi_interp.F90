!****
!               *****************************
!               * OASIS MODULE  -  LEVEL ? *
!               * -------------     ------- *
!               *****************************
!
!**** remap_biear - calculate bilinear remapping
!
!     Purpose:
!     -------
!     Adaptation of SCRIP 1.4 remap_biear module to calculate 
!     bilinear remapping.
!
!**   Interface:
!     ---------
!       *CALL*  *remap_bi**
!
!     Input:
!     -----
!
!     Output:
!     ------
!
!     History:
!     -------
!       Version   Programmer     Date        Description
!       -------   ----------     ----        -----------  
!       2.5       S. Valcke      2002/09     Treatment for masked point
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     this module contains necessary routines for performing an 
!     bilinear interpolation.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: remap_biear.f 2826 2010-12-10 11:14:21Z valcke $
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

      module remap_bi_interp

!-----------------------------------------------------------------------

      use kinds_mod     ! defines common data types
      use constants     ! defines common constants
      use grids         ! module containing grid info
      use remap_vars    ! module containing remap info
      use timers
      USE mod_oasis_flush
!$ use omp_lib

      implicit NONE

!-----------------------------------------------------------------------

      real (kind=dbl_kind), dimension(:), allocatable, save ::  &
                                        coslat, sinlat,  & ! cosine, sine of grid lats (for distance) 
                                        coslon, sinlon     ! cosine, sine of grid lons (for distance) 

      integer (kind=int_kind) :: il_nbthreads = 1

      integer (kind=int_kind), parameter :: max_iter = 100   ! max iteration count for i,j iteration

      real (kind=dbl_kind), parameter :: converge = 1.e-10_dbl_kind  ! convergence criterion

!***********************************************************************

      contains

!***********************************************************************

      subroutine remap_bi (lextrapdone, &
                           mpi_comm_map, mpi_size_map, mpi_rank_map, mpi_root_map)

!-----------------------------------------------------------------------
!
!     this routine computes the weights for a bilinear interpolation.
!
!-----------------------------------------------------------------------
      !-----------------------------------------------------------------------
      !
      !     input variables
      !
      !-----------------------------------------------------------------------

      integer (kind=int_kind) :: mpi_comm_map, mpi_rank_map, mpi_size_map, mpi_root_map


      LOGICAL :: lextrapdone   ! logical, true if EXTRAP done on field
      LOGICAL :: ll_nnei        ! true (default) if extra search is done

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n,icount, i, k, &
                                 dst_add,        & ! destination address
                                 iter,           & ! iteration counter
                                 nmap              ! index of current map being computed

      integer (kind=int_kind), dimension(4) :: src_add        ! address for the four source points

      real (kind=dbl_kind), dimension(4)  :: src_lats, &      ! latitudes  of four bilinear corners
                                             src_lons         ! longitudes of four bilinear corners

      real (kind=dbl_kind), dimension(num_wts,4) :: wgts      ! bilinear/bicubic weights for four corners
      real (kind=dbl_kind) :: plat, plon,             & ! lat/lon coords of destination point
                              iguess, jguess,         & ! current guess for bilinear coordinate
                              deli, delj,             & ! corrections to i,j
                              dth1, dth2, dth3,       & ! some latitude  differences
                              dph1, dph2, dph3,       & ! some longitude differences
                              dthp, dphp,             & ! difference between point and sw corner
                              mat1, mat2, mat3, mat4, & ! matrix elements
                              determinant,            & ! matrix determinant
                              sum_wgts                  ! sum of weights for normalization

      real (kind=dbl_kind) ::  coslat_dst, sinlat_dst, coslon_dst, sinlon_dst, &
                               dist_min, distance,                             & ! for computing dist-weighted avg
                               src_latsnn, d_dist

      real (kind=dbl_kind) :: dl_test

      integer (kind=int_kind) :: min_add, max_add, srch_add, src_addnn, num_neighbors

      integer (kind=int_kind) :: il_splitsize
      integer (kind=int_kind) :: ib_proc
      integer (kind=int_kind) :: ib_thread
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
      logical :: ll_gaussreduced_grid = .false.
      logical :: ll_bicubic = .false.

#ifdef REMAP_TIMING
      logical :: ll_timing = .true.
      real    (kind=dbl_kind), dimension(:,:), allocatable :: dla_timer
      real    (kind=dbl_kind) :: dl_tstart, dl_tstop
      integer (kind=int_kind) :: il_mythread
#endif

!-----------------------------------------------------------------------
!
!     compute mappings from grid1 to grid2
!
!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine remap_bi'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF

#ifdef REMAP_TIMING
      if (ll_timing) call timer_start(2,'remap_bi alloc')
#endif

      ! num_neighbors = 4 for bilinear or bicubic interpolation
      num_neighbors = 4
!
      ll_nnei = .true.
      nmap = 1

      IF (restrict_TYPE == 'REDUCED') ll_gaussreduced_grid = .TRUE.
      IF (map_type == map_type_bicubic ) ll_bicubic = .TRUE.

      if (grid1_rank /= 2) then
        stop 'Can not do bilinear or bicubic interpolation when grid_rank /= 2'
      endif

      !***
      !*** compute cos, sin of lat/lon on source grid for distance
      !*** calculations
      !***

      allocate (coslat(grid1_size), coslon(grid1_size), sinlat(grid1_size), sinlon(grid1_size))

      coslat = cos(grid1_center_lat)
      coslon = cos(grid1_center_lon)
      sinlat = sin(grid1_center_lat)
      sinlon = sin(grid1_center_lon)

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
!$OMP SHARED(lextrapdone,num_neighbors) &
!$OMP SHARED(grid2_mask,grid2_frac) &
!$OMP SHARED(grid2_center_lat,grid2_center_lon) &
!$OMP SHARED(grid1_center_lat,grid1_center_lon) &
!$OMP SHARED(grid1_bound_box,grid1_bbox_per) &
!$OMP SHARED(grid1_mask) &
!$OMP SHARED(bin_addr1,nulou,sga_remap,num_wts) &
!$OMP SHARED(nmap,dl_test) &
!$OMP PRIVATE(nlogprt) &
!$OMP SHARED(coslat,coslon,sinlat,sinlon) &
!$OMP PRIVATE(wgts,sum_wgts,dst_add,icount) &
!$OMP PRIVATE(coslat_dst,coslon_dst,sinlat_dst,sinlon_dst) &
!$OMP PRIVATE(plat,plon,src_add,src_lats,src_lons,min_add,max_add) &
!$OMP PRIVATE(src_addnn) &
!$OMP PRIVATE(dth1,dth2,dth3,dph1,dph2,dph3,iguess,jguess,iter,dthp,dphp) &
!$OMP PRIVATE(mat1,mat2,mat3,mat4,determinant,deli,delj,d_dist) &
!$OMP PRIVATE(distance,dist_min,n,srch_add,src_latsnn) &
!$OMP SHARED(il_nbthreads) &
!$OMP SHARED(mpi_rank_map,mpi_root_map,ila_mpi_mn,ila_mpi_mx) &
!$OMP SHARED(ila_thr_sz,ila_thr_mn,ila_thr_mx) &
!$OMP SHARED(grid1_dims,grid1_size) &
!$OMP SHARED(ll_nnei,ll_gaussreduced_grid,ll_bicubic) &
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
            call timer_start(3,'remap_bi distr')
         end if
      end if
#endif
      allocate(ila_thr_mn(il_nbthreads))
      allocate(ila_thr_mx(il_nbthreads))

      if (il_nbthreads .gt. 1) then

#ifdef REMAP_TIMING
         if (ll_timing) then
            allocate(dla_timer(il_nbthreads,8))
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

        grid_loop1: do dst_add = ila_thr_mn(ib_thread), ila_thr_mx(ib_thread)

          if (.not. grid2_mask(dst_add)) cycle grid_loop1

#ifdef REMAP_TIMING
          if (ll_timing) then
             if (il_nbthreads.gt.1) then
!$              dl_tstart = OMP_GET_WTIME()
             else
                call timer_start(4,'remap_bi search')
             end if
          end if
#endif

          plat = grid2_center_lat(dst_add)
          plon = grid2_center_lon(dst_add)

          !***
          !*** find nearest square of grid points on source grid
          !***

          IF ( ll_gaussreduced_grid ) then

            call grid_search_bilin_reduced(src_add, src_lats, src_lons,          &
                                           min_add, max_add,                     &
                                           plat, plon, grid1_dims,               &
                                           grid1_center_lat, grid1_center_lon,   &
                                           lextrapdone)
          ELSE
            call grid_search_bi(src_add, src_lats, src_lons,          &
                                min_add, max_add,                     &
                                plat, plon, grid1_dims,               &
                                grid1_center_lat, grid1_center_lon,   &
                                grid1_bound_box, grid1_bbox_per,      &
                                bin_addr1, lextrapdone)
          ENDIF

#ifdef REMAP_TIMING
          if (ll_timing) then
            if (il_nbthreads.gt.1) then
!$            dl_tstop = OMP_GET_WTIME() 
              dla_timer(ib_thread,2) = dla_timer(ib_thread,2) + dl_tstop - dl_tstart
            else
              call timer_stop(4)
            end if
          end if
#endif

          if (src_add(1) > 0) THEN

            !***
            !*** if the 4 surrounding points have been found and are 
            !*** non-masked, do bilinear interpolation
            !***

#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstart = OMP_GET_WTIME()
               else
                  call timer_start(5,'remap_bi weights')
               end if
            end if
#endif


            grid2_frac(dst_add) = one


            dth1 = src_lats(2) - src_lats(1)
            dth2 = src_lats(4) - src_lats(1)
            dth3 = src_lats(3) - src_lats(2) - dth2

            dph1 = src_lons(2) - src_lons(1)
            dph2 = src_lons(4) - src_lons(1)
            dph3 = src_lons(3) - src_lons(2)

            if (dph1 >  three*pih) dph1 = dph1 - pi2
            if (dph2 >  three*pih) dph2 = dph2 - pi2
            if (dph3 >  three*pih) dph3 = dph3 - pi2
            if (dph1 < -three*pih) dph1 = dph1 + pi2
            if (dph2 < -three*pih) dph2 = dph2 + pi2
            if (dph3 < -three*pih) dph3 = dph3 + pi2

            dph3 = dph3 - dph2

            iguess = half
            jguess = half

            iter_loop1: do iter=1,max_iter

              dthp = plat - src_lats(1) - dth1*iguess - dth2*jguess - dth3*iguess*jguess
              dphp = plon - src_lons(1)

              if (dphp >  three*pih) dphp = dphp - pi2
              if (dphp < -three*pih) dphp = dphp + pi2

              dphp = dphp - dph1*iguess - dph2*jguess - dph3*iguess*jguess

              mat1 = dth1 + dth3*jguess
              mat2 = dth2 + dth3*iguess
              mat3 = dph1 + dph3*jguess
              mat4 = dph2 + dph3*iguess

              determinant = mat1*mat4 - mat2*mat3

              deli = (dthp*mat4 - mat2*dphp)/determinant
              delj = (mat1*dphp - dthp*mat3)/determinant

              if (abs(deli) < converge .and. abs(delj) < converge) &
                exit iter_loop1

              iguess = iguess + deli
              jguess = jguess + delj

            end do iter_loop1

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

#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstart = OMP_GET_WTIME()
               else
                  call timer_start(6,'remap_dist_gaus_wgt store_link')
               end if
            end if
#endif

            if (iter <= max_iter) then

              !***
              !*** successfully found i,j - compute weights
              !***

              if ( ll_bicubic ) then
                wgts(1,1) = (one - jguess**2*(three-two*jguess))* (one - iguess**2*(three-two*iguess))
                wgts(1,2) = (one - jguess**2*(three-two*jguess))* iguess**2*(three-two*iguess)
                wgts(1,3) =        jguess**2*(three-two*jguess) * iguess**2*(three-two*iguess)
                wgts(1,4) =        jguess**2*(three-two*jguess) * (one - iguess**2*(three-two*iguess))
                wgts(2,1) = (one - jguess**2*(three-two*jguess))* iguess*(iguess-one)**2
                wgts(2,2) = (one - jguess**2*(three-two*jguess))* iguess**2*(iguess-one)
                wgts(2,3) =        jguess**2*(three-two*jguess) * iguess**2*(iguess-one)
                wgts(2,4) =        jguess**2*(three-two*jguess) * iguess*(iguess-one)**2
                wgts(3,1) =        jguess*(jguess-one)**2       * (one - iguess**2*(three-two*iguess))
                wgts(3,2) =        jguess*(jguess-one)**2       * iguess**2*(three-two*iguess)
                wgts(3,3) =        jguess**2*(jguess-one)       * iguess**2*(three-two*iguess)
                wgts(3,4) =        jguess**2*(jguess-one)       * (one - iguess**2*(three-two*iguess))
                wgts(4,1) =        iguess*(iguess-one)**2       * jguess*(jguess-one)**2
                wgts(4,2) =        iguess**2*(iguess-one)       * jguess*(jguess-one)**2
                wgts(4,3) =        iguess**2*(iguess-one)       * jguess**2*(jguess-one)
                wgts(4,4) =        iguess*(iguess-one)**2       * jguess**2*(jguess-one)
              ! Bilinear and reduced bilinear cases
              else
                wgts(1,1) = (one-iguess)*(one-jguess)
                wgts(1,2) = iguess*(one-jguess)
                wgts(1,3) = iguess*jguess
                wgts(1,4) = (one-iguess)*jguess
              end if

              call store_link_bi(dst_add, src_add, wgts, nmap, ib_thread)

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


          ELSE
            write(nulou,*)'Point coords: ',plat,plon
            do k = 1,size(src_add)
               if (src_add(k) > 0) then
                  write(nulou,*)'Dest grid info: ',src_add(k),src_lats(k),src_lons(k),grid1_mask(src_add(k))
               else
                  write(nulou,*)'Dest grid info: ',src_add(k),src_lats(k),src_lons(k)
               endif
            enddo
            write(nulou,*)'Current i,j : ',iguess, jguess
            write(nulou,*)'Iteration for i,j exceed max iteration count'
            stop 
          endif
        !
        else if (src_add(1) < 0) THEN

          !***
          !*** Search for neighbour search failed or at least one of the 4
          !*** neighbours was masked. Do distance-weighted average using
          !*** the non-masked points among the 4 closest ones. 
          !***

          IF (nlogprt .eq. 2) then
            WRITE(nulou,*) ' '
            WRITE(nulou,*) 'WARNING: Cannot make bilinear interpolation for target point',dst_add
            WRITE(nulou,*) 'Using non-masked points among 4 nearest neighbors.'
            WRITE(nulou,*) ' '
          ENDIF

#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstart = OMP_GET_WTIME()
               else
                  call timer_start(7,'remap_bi nneighbour')
               end if
            end if
#endif
          !***
          !*** Find the 4 closest points
          !***

          coslat_dst = cos(plat)
          sinlat_dst = sin(plat)
          coslon_dst = cos(plon)
          sinlon_dst = sin(plon)
          src_add = 0
          dist_min = bignum
          src_lats = bignum

          if ( ll_gaussreduced_grid ) then
            IF (min_add == 0) min_add = 1
            IF (max_add > grid1_size) max_add = grid1_size
          endif

          do srch_add = min_add,max_add
            d_dist = coslat_dst*coslat(srch_add)*     &
                     (coslon_dst*coslon(srch_add) +   &
                      sinlon_dst*sinlon(srch_add)) +  &
                     sinlat_dst*sinlat(srch_add)
            IF (d_dist < -1.0d0) THEN
              d_dist = -1.0d0
            ELSE IF (d_dist > 1.0d0) THEN
              d_dist = 1.0d0
            END IF
            distance=acos(d_dist)

            if (distance < dist_min) then
              sort_loop: do n=1,4
                if (distance < src_lats(n)) then
                  do i=4,n+1,-1
                    src_add (i) = src_add (i-1)
                    src_lats(i) = src_lats(i-1)
                  end do
                  src_add (n) = srch_add
                  src_lats(n) = distance
                  dist_min = src_lats(4)
                  exit sort_loop
                endif
              end do sort_loop
            endif
          end do

          src_lons = one/(src_lats + tiny)
          distance = sum(src_lons)
          src_lats = src_lons/distance

          !***
          !*** Among 4 closest points, keep only the non-masked ones
          !***

          icount = 0
          do n=1,4
            if (grid1_mask(src_add(n)) .or. &
                (.not. grid1_mask(src_add(n)) .and. lextrapdone)) then
              icount = icount + 1
            else
              src_lats(n) = zero
            endif
          end do

#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstop = OMP_GET_WTIME() 
                  dla_timer(ib_thread,5) = dla_timer(ib_thread,5) + dl_tstop - dl_tstart
               else
                  call timer_stop(7)
               end if
            end if
#endif

          if (icount > 0) then

#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstart = OMP_GET_WTIME()
               else
                  call timer_start(6,'remap_dist_gaus_wgt store_link')
               end if
            end if
#endif

            !*** renormalize weights
            sum_wgts = sum(src_lats)
            wgts(1,1) = src_lats(1)/sum_wgts
            wgts(1,2) = src_lats(2)/sum_wgts
            wgts(1,3) = src_lats(3)/sum_wgts
            wgts(1,4) = src_lats(4)/sum_wgts
            if ( ll_bicubic ) &
              wgts(2:4,:) = 0.

            grid2_frac(dst_add) = one
            call store_link_bi(dst_add, src_add, wgts, nmap, ib_thread)
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

          ELSE

            IF (ll_nnei .eqv. .true. ) then

#ifdef REMAP_TIMING
              if (ll_timing) then
                 if (il_nbthreads.gt.1) then
!$                  dl_tstart = OMP_GET_WTIME()
                 else
                    call timer_start(8,'remap_dist_gaus_wgt store_lk2')
                 end if
              end if
#endif
              IF (nlogprt .ge. 2) THEN
                WRITE(nulou,*) '  '
                WRITE(nulou,*) 'All 4 surrounding source grid points are masked'
                WRITE(nulou,*) 'for target point ',dst_add
                WRITE(nulou,*) 'with longitude and latitude', plon, plat
                WRITE(nulou,*) 'Using the nearest non-masked neighbour.'
                CALL OASIS_FLUSH_SCRIP(nulou)
              ENDIF
!
              src_latsnn = bignum
              src_addnn = 0

!cdir novector
              do srch_add = min_add,max_add
                if (grid1_mask(srch_add) .or. &
                    (.not. grid1_mask(srch_add) .and. lextrapdone)) THEN
                  d_dist = coslat_dst*coslat(srch_add)*   &
                           (coslon_dst*coslon(srch_add) + &
                           sinlon_dst*sinlon(srch_add))+  &
                           sinlat_dst*sinlat(srch_add)
                  IF (d_dist < -1.0d0) THEN
                    d_dist = -1.0d0
                  ELSE IF (d_dist > 1.0d0) THEN
                    d_dist = 1.0d0
                  END IF
                  distance=acos(d_dist)
                  if (distance < src_latsnn) then
                    src_addnn = srch_add
                    src_latsnn = distance
                  endif
                endif
              end do
              ! Special treatment for bilinear reduced 
              ! consequence of different bins definition
              ! in source and target grid
              IF (src_addnn == 0 .AND. ll_gaussreduced_grid) THEN
                src_latsnn = bignum
!cdir novector
                do srch_add = 1, grid1_size
                  if (grid1_mask(srch_add) .or. lextrapdone) THEN
                    d_dist = coslat_dst*coslat(srch_add)*    &
                             (coslon_dst*coslon(srch_add) +  &
                             sinlon_dst*sinlon(srch_add))+   &
                             sinlat_dst*sinlat(srch_add)
                    IF (d_dist < -1.0d0) THEN
                      d_dist = -1.0d0
                    ELSE IF (d_dist > 1.0d0) THEN
                      d_dist = 1.0d0
                    END IF
                    distance=acos(d_dist)
                    if (distance < src_latsnn) then
                      src_addnn = srch_add
                      src_latsnn = distance
                    endif
                  endif
                end DO
              ENDIF
              IF (src_addnn == 0) THEN
                WRITE(nulou,*) 'Problem with target grid point'
                WRITE(nulou,*) 'with address = ',dst_add 
                call abort
              ENDIF 
              IF (nlogprt .ge. 2) THEN
                WRITE(nulou,*) '  '
                WRITE(nulou,*) 'Nearest non masked neighbour is source point ',src_addnn
                WRITE(nulou,*) 'with longitude and latitude',grid1_center_lon(src_addnn), &
                                                             grid1_center_lat(src_addnn) 
              ENDIF

              wgts(1,1) = 1.
              wgts(1,2:4) = 0.
              if ( ll_bicubic ) &
                wgts(2:4,:) = 0.

              src_add(1) = src_addnn
              src_add(2) = 0
              src_add(3) = 0
              src_add(4) = 0

              grid2_frac(dst_add) = one
              call store_link_bi(dst_add, src_add, wgts, nmap, ib_thread)
            endif
#ifdef REMAP_TIMING
            if (ll_timing) then
               if (il_nbthreads.gt.1) then
!$                dl_tstop = OMP_GET_WTIME() 
                  dla_timer(ib_thread,6) = dla_timer(ib_thread,6) + dl_tstop - dl_tstart
               else
                  call timer_stop(8)
               end if
            end if
#endif
          ENDIF
        ENDIF
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
                  IF (nlogprt .GE. 2) &
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
                  & 'remap_bi distr ',dla_timer(ib_thread,1)
               write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_bi search ',dla_timer(ib_thread,2)
               write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_bi weights ',dla_timer(ib_thread,3)
               write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_bi store_link',dla_timer(ib_thread,4)
               write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_bi nneighbour',dla_timer(ib_thread,5)
               write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_bi store_lk2',dla_timer(ib_thread,6)
            end do
            deallocate(dla_timer)
            write(nulou,*) ' On master thread '
            call timer_print(3)
            call timer_clear(3)
         end if

      else

         if (ll_timing.and.nlogprt.ge.2) then
            do n = 2, 8
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
            CALL MPI_IRecv(grid1_add_map1(SUM(ila_num_links_mpi(1:n))+1),&
                  & ila_num_links_mpi(n+1),MPI_INT,n,1,mpi_comm_map,&
                  & ila_req_mpi(1,n),il_err)

            CALL MPI_IRecv(grid2_add_map1(SUM(ila_num_links_mpi(1:n))+1),&
                  & ila_num_links_mpi(n+1),MPI_INT,n,2,mpi_comm_map,&
                  & ila_req_mpi(2,n),il_err)

            CALL MPI_IRecv(wts_map1(:,SUM(ila_num_links_mpi(1:n))+1),&
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

        deallocate(ila_num_links_mpi)

      end if


      deallocate (coslat, coslon, sinlat, sinlon)
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
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine remap_bi'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
      end subroutine remap_bi

!***********************************************************************

      subroutine grid_search_bi(src_add, src_lats, src_lons,     &
                                min_add, max_add,                &
                                plat, plon, src_grid_dims,       &
                                src_center_lat, src_center_lon,  &
                                src_grid_bound_box,              &
                                src_grid_bbox_per,               &
                                src_bin_add, lextrapdone)

!-----------------------------------------------------------------------
!
!     this routine finds the location of the search point plat, plon
!     in the source grid and returns the corners needed for a bilinear
!     interpolation.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     output variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), dimension(4), intent(out) :: src_add  ! address of each corner point enclosing P

      real (kind=dbl_kind), dimension(4), intent(out) :: src_lats,  & ! latitudes  of the four corner points
                                                         src_lons     ! longitudes of the four corner points

      integer (kind=int_kind) :: min_add, max_add

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      real (kind=dbl_kind), intent(in) :: plat, &  ! latitude  of the search point
                                          plon     ! longitude of the search point

      integer (kind=int_kind), dimension(2), intent(in) :: src_grid_dims  ! size of each src grid dimension

      real (kind=dbl_kind), dimension(:), intent(in) :: src_center_lat, & ! latitude  of each src grid center 
                                                        src_center_lon    ! longitude of each src grid center

      real (kind=dbl_kind), dimension(:,:), intent(in) :: src_grid_bound_box ! bound box for source grid

      integer (kind=int_kind), dimension(:), intent(in) :: src_grid_bbox_per ! bound box periodicity for source grid

      integer (kind=int_kind), dimension(:,:), intent(in) :: src_bin_add    ! latitude bins for restricting

      LOGICAL :: lextrapdone   ! logical, true if EXTRAP done on field
!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n, next_n, srch_add, ni, &  ! dummy indices
                                 nx, ny, ntotmask,        &   ! dimensions of src grid
                                 i, j, jp1, ip1, n_add, e_add, ne_add  ! addresses

      real (kind=dbl_kind) :: & ! vectors for cross-product check
           vec1_lat, vec1_lon, vec2_lat, vec2_lon, cross_product, cross_product_last

!-----------------------------------------------------------------------
!
!     restrict search first using bins
!
!-----------------------------------------------------------------------

      src_add = 0

      min_add = size(src_center_lat)
      max_add = 1

      do n=1,num_srch_bins
        if (plat >= bin_lats(1,n) .and. plat <= bin_lats(2,n) .and. &
            plon >= bin_lons(1,n) .and. plon <= bin_lons(2,n)) then
          min_add = min(min_add, src_bin_add(1,n))
          max_add = max(max_add, src_bin_add(2,n))
        endif
      end do
 
!-----------------------------------------------------------------------
!
!     now perform a more detailed search 
!
!-----------------------------------------------------------------------

      nx = src_grid_dims(1)
      ny = src_grid_dims(2)

      srch_loop: do srch_add = min_add,max_add

        !*** first check bounding box

        if (plat <= src_grid_bound_box(2,srch_add) .and.  &
            plat >= src_grid_bound_box(1,srch_add) .and.  &
            lf_lon_in_box (plon,src_grid_bound_box(3,srch_add),&
            &  src_grid_bound_box(4,srch_add),src_grid_bbox_per(srch_add)) ) then

          !***
          !*** we are within bounding box so get really serious
          !***

          !*** determine neighbor addresses

          j = (srch_add - 1)/nx +1
          i = srch_add - (j-1)*nx

          !*** find ip1
          !*** Note: I do not want to take into account the number
          !*** of overlapping grid points, as the underlying cell
          !*** will be found in all cases if the grid overlaps.

          if (i < nx) then
            ip1 = i + 1
          else
            ip1 = 1
          endif

          if (j < ny) then
            jp1 = j+1
          else
            jp1 = 1
          endif

          n_add = (jp1 - 1)*nx + i
          e_add = (j - 1)*nx + ip1
          ne_add = (jp1 - 1)*nx + ip1

          src_lats(1) = src_center_lat(srch_add)
          src_lats(2) = src_center_lat(e_add)
          src_lats(3) = src_center_lat(ne_add)
          src_lats(4) = src_center_lat(n_add)

          src_lons(1) = src_center_lon(srch_add)
          src_lons(2) = src_center_lon(e_add)
          src_lons(3) = src_center_lon(ne_add)
          src_lons(4) = src_center_lon(n_add)

          !***
          !*** for consistency, we must make sure all lons are in
          !*** same 2pi interval
          !***

          vec1_lon = src_lons(1) - plon
          if (vec1_lon >  pi) then
            src_lons(1) = src_lons(1) - pi2
          else if (vec1_lon < -pi) then
            src_lons(1) = src_lons(1) + pi2
          endif
          do n=2,4
            vec1_lon = src_lons(n) - src_lons(1)
            if (vec1_lon >  pi) then
              src_lons(n) = src_lons(n) - pi2
            else if (vec1_lon < -pi) then
              src_lons(n) = src_lons(n) + pi2
            endif
          end do

          corner_loop: do n=1,4
            next_n = MOD(n,4) + 1

            !***
            !*** here we take the cross product of the vector making 
            !*** up each box side with the vector formed by the vertex
            !*** and search point.  if all the cross products are 
            !*** positive, the point is contained in the box.
            !***

            vec1_lat = src_lats(next_n) - src_lats(n)
            vec1_lon = src_lons(next_n) - src_lons(n)
            vec2_lat = plat - src_lats(n)
            vec2_lon = plon - src_lons(n)

            !***
            !*** check for 0,2pi crossings
            !***

            if (vec1_lon >  three*pih) then
              vec1_lon = vec1_lon - pi2
            else if (vec1_lon < -three*pih) then
              vec1_lon = vec1_lon + pi2
            endif
            if (vec2_lon >  three*pih) then
              vec2_lon = vec2_lon - pi2
            else if (vec2_lon < -three*pih) then
              vec2_lon = vec2_lon + pi2
            endif

            cross_product = vec1_lon*vec2_lat - vec2_lon*vec1_lat

            !***
            !*** if cross product is less than zero, this cell
            !*** doesn't work
            !***

            if (n == 1) cross_product_last = cross_product
            if (cross_product*cross_product_last < zero) &
               exit corner_loop
            if (cross_product.ne.0) cross_product_last = cross_product

          end do corner_loop

          !***
          !*** if cross products all same sign, we found the location
          !***

          if (n > 4) then
            src_add(1) = srch_add
            src_add(2) = e_add
            src_add(3) = ne_add
            src_add(4) = n_add

           ! Check if one point is masked; IF so, nearest-neighbour
           ! interpolation will be used  

            ntotmask = 0
            do ni=1,4
              if (.not. grid1_mask(src_add(ni)).and. .not. lextrapdone) &
                 ntotmask = ntotmask + 1 
            end DO
            IF (ntotmask .gt. 0) src_add(1) = -src_add(1)
        
            return
          endif

          !***
          !*** otherwise move on to next cell
          !***

        endif !bounding box check
      end do srch_loop

      !***
      !*** if no cell found, point is likely either in a box that straddles
      !*** either pole or is outside the grid. Put src_add = -1 so that
      !*** distance-weighted average of the 4 non-masked closest points
      !*** is done in calling routine.
  
      src_add = -1

!-----------------------------------------------------------------------

      contains

         logical (kind=log_kind) function lf_lon_in_box(rd_lon,&
            & rd_west,rd_east,id_per)

            real (kind=dbl_kind), intent(in) :: rd_lon
            real (kind=dbl_kind), intent(in) :: rd_west,rd_east
            integer (kind=int_kind), intent(in) :: id_per
            
            select case (id_per)
            case (0)
               lf_lon_in_box = &
                  rd_lon <= rd_east .and. rd_lon >= rd_west

            case (1)
               lf_lon_in_box = &
                  rd_lon <= rd_east .or. rd_lon >= rd_west

            end select

         end function lf_lon_in_box

!-----------------------------------------------------------------------

      end subroutine grid_search_bi 

!***********************************************************************

      subroutine store_link_bi(dst_add, src_add, weights, nmap, id_thread)

!-----------------------------------------------------------------------
!
!     this routine stores the address and weight for four links 
!     associated with one destination point in the appropriate address 
!     and weight arrays and resizes those arrays if necessary.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) :: dst_add, & ! address on destination grid
                                             nmap,    & ! identifies which direction for mapping
                                             id_thread  ! local threaded task

      integer (kind=int_kind), dimension(4), intent(in) :: src_add   ! addresses on source grid

      real (kind=dbl_kind), dimension(num_wts,4), intent(in) :: weights ! array of remapping weights for these links

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n, num_links_old          ! placeholder for old link number

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

          num_links_old  = num_links_map1
          num_links_map1 = num_links_old + 4

          if (num_links_map1 > max_links_map1) &
            call resize_remap_vars(1,resize_increment)

          do n=1,4
            grid1_add_map1(num_links_old+n) = src_add(n)
            grid2_add_map1(num_links_old+n) = dst_add
            wts_map1    (1:num_wts,num_links_old+n) = weights(1:num_wts,n)
          end do

        case(2)

          num_links_old  = num_links_map2
          num_links_map2 = num_links_old + 4

          if (num_links_map2 > max_links_map2) &
            call resize_remap_vars(2,resize_increment)

          do n=1,4
            grid1_add_map2(num_links_old+n) = dst_add
            grid2_add_map2(num_links_old+n) = src_add(n)
            wts_map2    (1:num_wts,num_links_old+n) = weights(1:num_wts,n)
          end do

        end select

      else

         sga_remap(id_thread)%num_links = sga_remap(id_thread)%num_links + 4

         if (sga_remap(id_thread)%num_links > sga_remap(id_thread)%max_links) &
            call sga_remap(id_thread)%resize(int(0.2*real(sga_remap(id_thread)%max_links)))

         do n=1,4
           sga_remap(id_thread)%grid1_add(sga_remap(id_thread)%num_links-4+n) = src_add(n)
           sga_remap(id_thread)%grid2_add(sga_remap(id_thread)%num_links-4+n) = dst_add
           sga_remap(id_thread)%wts(1:num_wts,sga_remap(id_thread)%num_links-4+n) = weights(1:num_wts,n)
         end do

      endif


!-----------------------------------------------------------------------

      end subroutine store_link_bi

!***********************************************************************

      subroutine grid_search_bilin_reduced(src_add, src_lats, src_lons,     &
                                           min_add, max_add,                &
                                           plat, plon, src_grid_dims,       &
                                           src_center_lat, src_center_lon,  &
                                           lextrapdone)

!-----------------------------------------------------------------------
!
!     this routine finds the location of the search point plat, plon
!     in the source grid and returns the corners needed for a bilinear
!     interpolation. The target grid is a reduced grid.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     output variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), dimension(4), intent(out) :: src_add  ! address of each corner point enclosing P

      real (kind=dbl_kind), dimension(4), intent(out) :: src_lats, & ! latitudes  of the four corner points
                                                         src_lons    ! longitudes of the four corner points

      integer (kind=int_kind) :: min_add, max_add

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      real (kind=dbl_kind), intent(in) :: plat,  & ! latitude  of the search point
                                          plon     ! longitude of the search point

      integer (kind=int_kind), dimension(2), intent(in) :: src_grid_dims  ! size of each src grid dimension

      real (kind=dbl_kind), dimension(:), intent(in) :: src_center_lat, & ! latitude  of each src grid center 
                                                        src_center_lon  ! longitude of each src grid center

      LOGICAL :: lextrapdone   ! logical, true if EXTRAP done on field

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n, next_n, srch_add, ni, &
                                 nx, ny, ntotmask,        &   ! dimensions of src grid
                                 inter_add                    ! add for restricting search
!
      integer (kind=int_kind), DIMENSION(4) :: src_bid

!-----------------------------------------------------------------------
!
!     restrict search first using bins
!
!-----------------------------------------------------------------------

      nx = src_grid_dims(1)
      inter_add = 0

      src_add = 0

      min_add = size(src_center_lat) + 1
      max_add = 1
      if (plat >= bin_lats_r(1,1)) then
          min_add = 0
          max_add = bin_addr1_r(4,1)
          inter_add = bin_addr1_r(3,1)
      else if (plat <= bin_lats_r(1,num_srch_red)) then
          max_add = nx + 1
          min_add = bin_addr1_r(1,num_srch_red)
          inter_add = bin_addr1_r(3,num_srch_red)
      else
          srch_loopred: do n=1,num_srch_red
            if (plat <= bin_lats_r(1,n) .and. plat >= bin_lats_r(2,n)) then
                min_add = bin_addr1_r(1,n)
                max_add = bin_addr1_r(4,n)
                inter_add = bin_addr1_r(3,n)
                exit srch_loopred
            endif
          end DO srch_loopred
      ENDIF

!-----------------------------------------------------------------------
!
!     now perform a more detailed search 
!
!-----------------------------------------------------------------------
      if (min_add .ne. 0 .and. max_add .ne. nx+1) THEN
         !*** The concerned bins are not the top north or south ones.
         !*** We should be able to find the four corners
         !*** for the bilinear interpolation.

          IF ( plon <= src_center_lon(min_add) ) THEN
              src_add(1) = inter_add-1
              src_add(2) = min_add
          ELSE IF ( plon > src_center_lon(inter_add-1) ) then
              src_add(1) = inter_add-1
              src_add(2) = min_add
          else
              srch_loop2: do srch_add = min_add, inter_add-2
                 if ( (plon > src_center_lon(srch_add)) .and. &
                      (plon <= src_center_lon(srch_add+1)) )then
                     src_add(1) = srch_add
                     src_add(2) = srch_add+1
                     exit srch_loop2
                 endif
               end do srch_loop2
           ENDIF
           IF ( plon <= src_center_lon(inter_add) ) THEN
               src_add(3) = max_add
               src_add(4) = inter_add
           ELSE IF ( plon >= src_center_lon(max_add) ) then
               src_add(3) = max_add
               src_add(4) = inter_add
           else
               srch_loop3: do srch_add = inter_add, max_add
                 if ( (plon >= src_center_lon(srch_add)) .and. &
                      (plon <= src_center_lon(srch_add+1)) )then
                      src_add(3) = srch_add
                      src_add(4) = srch_add+1
                      exit srch_loop3
                  endif
               enddo srch_loop3
           ENDIF
           src_lats(1) = src_center_lat(src_add(3))
           src_lats(2) = src_center_lat(src_add(4))
           src_lats(3) = src_center_lat(src_add(2))
           src_lats(4) = src_center_lat(src_add(1))
      
           src_lons(1) = src_center_lon(src_add(3))
           src_lons(2) = src_center_lon(src_add(4))
           src_lons(3) = src_center_lon(src_add(2))
           src_lons(4) = src_center_lon(src_add(1))

           src_bid=src_add

           src_add(1) = src_bid(3)
           src_add(2) = src_bid(4)
           src_add(3) = src_bid(2)
           src_add(4) = src_bid(1)
    
           ! Check if one point is masked; IF so, nearest-neighbour
           ! interpolation will be used 

           ntotmask = 0
           do ni=1,4
             if (.not. grid1_mask(src_add(ni)).and. .not. lextrapdone) &
                ntotmask = ntotmask + 1 
           end DO
           IF (ntotmask .gt. 0) src_add(1) = -src_add(1) 
          
       ELSE 

           !*** We are in the first or last bin.  Put src_add = -1 so that
           !*** distance-weighted average of the 4 non-masked closest points
           !*** is done in calling routine. 

           src_add = -1

       ENDIF

!-----------------------------------------------------------------------

      end subroutine grid_search_bilin_reduced

!***********************************************************************

      end module remap_bi_interp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
