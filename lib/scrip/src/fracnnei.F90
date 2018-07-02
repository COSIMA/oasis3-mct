module fracnnei_mod

contains

   subroutine fracnnei()
      !
      !****
      !               *****************************
      !               * OASIS ROUTINE  -  LEVEL 4 *
      !               * -------------     ------- *
      !               *****************************
      !
      !**** *fracnnei* - SCRIP remapping
      !
      !     Purpose:
      !     -------
      !     Treatment of the tricky points in an interpolation
      !
      !     Interface:
      !     ---------
      !       *CALL*  *
      !
      !     Called from:
      !     -----------
      !     scriprmp
      !
      !     History:
      !     -------
      !       Version   Programmer     Date        Description
      !       -------   ----------     ----        -----------  
      !       2.5       D.Declat       2002/08/20  adapted from S. Valcke ptmsq
      !       3.0       S. Valcke      2002/10/30  test and corrections
      !       4.0       A.Piacentini   2018/01/23  F90 and optimisation
      !
      ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      !* ---------------------------- Modules used ----------------------------
      !
      use kinds_mod     ! defines common data types
      use constants     ! defines common constants
      use grids         ! module containing grid information
      use remap_vars    ! module containing remap information
      use mod_oasis_flush
!$ use omp_lib
      !
      !* ---------------------------- Implicit --------------------------------
      !
      implicit none
      !
      !* ---------------------------- Arguments -------------------------------
      !
      !
      !* ---------------------------- Local Variables  ------------------------
      !

      logical (kind=log_kind) :: ll_debug = .false.

      integer (kind=int_kind) ::  num_neigh, num_prev_links
      logical (kind=log_kind), dimension(grid2_size) :: lnnei
      integer (kind=int_kind) :: &
         &    ib_dst,  &           ! INDEX loop for the distance grid
         &    ib_src,  &           ! INDEX loop for the source grid
         &    ib_links             ! INDEX loop for the links
      integer (kind=int_kind) :: &
         &    il_nneiadd, &        ! Nearest-neighbor address
         &    il_lonkind, &
         &    ib_bin,     &
         &    min_add,    &
         &    max_add
      real (kind=dbl_kind) :: &
         &    coslatd,   &          ! cosinus of the latitude for dst
         &    sinlatd,   &          ! sinus of the latitude for dst
         &    coslond,   &          ! cosinus of the longitude for dst
         &    sinlond,   &          ! sinus of the longitude for dst
         &    lonwestd,  &
         &    loneastd,  &
         &    latsouthd, &
         &    latnorthd, &
         &    distance, &
         &    dist_min, &
         &    d_dist

      real (kind=dbl_kind), parameter :: dp_box     = 2.0*deg2rad
      real (kind=dbl_kind), parameter :: dp_pthresh = 85.0*deg2rad

      real (kind=dbl_kind), dimension(grid1_size) :: &
         &    coslats,   &          ! cosinus of the latitude for src
         &    sinlats,   &          ! sinus of the latitude for src
         &    coslons,   &          ! cosinus of the longitude for src
         &    sinlons               ! sinus of the longitude for src

      integer (kind=int_kind) :: ib_vmm, il_add
      integer (kind=int_kind), dimension(:), allocatable :: ila_dst
      integer (kind=int_kind) :: il_envthreads, il_err, il_splitsize, ib_thread
      integer (kind=int_kind) :: il_mythread
      integer (kind=int_kind) :: il_nbthreads = 1
      integer (kind=int_kind), dimension(:), allocatable :: ila_thr_sz
      integer (kind=int_kind), dimension(:), allocatable :: ila_thr_mn
      integer (kind=int_kind), dimension(:), allocatable :: ila_thr_mx

      character (LEN=14) :: cl_envvar
      !
      !* ---------------------------- Poema verses ----------------------------
      !
      ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      !
      !*    1. Initialization
      !        --------------
      !
      if (nlogprt .ge. 2) then
         write (UNIT = nulou,FMT = *) ' '
         write (UNIT = nulou,FMT = *) ' '
         write (UNIT = nulou,FMT = *) '   Entering ROUTINE fracnnei  -  Level 4'
         write (UNIT = nulou,FMT = *) '           ****************     *******'
         write (UNIT = nulou,FMT = *) ' '
         write (UNIT = nulou,FMT = *) ' Treating the tricky points of the remapping'
         write (UNIT = nulou,FMT = *) ' '
         call OASIS_FLUSH_SCRIP(nulou)
      endif
      !
      ! *----------------------------------------------------------------------
      !
      !*    2. Find Vmm points   V
      !        ---------------  m m
      !    A non-masked Valid target point is either with link or without link
      !    If without link, find the non-masked nearest neighbours.
      ! 
      lnnei(:) = grid2_mask(:)
      do ib_links = 1, num_links_map1
         lnnei(grid2_add_map1(ib_links)) = .false.
      end do

      num_neigh = count(lnnei)

      allocate(ila_dst(num_neigh))

      ila_dst=pack([(ib_dst,ib_dst=1,grid2_size)],lnnei)

      if (nlogprt .ge. 2) then
         if (ll_debug) then
            do ib_vmm = 1, num_neigh
               write(nulou,*) '********* Will do FRACNNEI for point',ila_dst(ib_vmm)
            end do
         end if
         write(nulou,*) '************ There are',num_neigh,'Vmm points in all'
      end if

      !
      ! *----------------------------------------------------------------------
      !
      !*    2. Treating Vmm points   V
      !        -------------------  m m
      !     The target point is a non-masked Valid point while the source points 
      !         are all masked points. Use of the non-masked nearest neighbours.
      !

      !     Resize the storage

      num_prev_links = num_links_map1
      num_links_map1 = num_links_map1 + num_neigh

      call resize_remap_vars(1, num_links_map1 - max_links_map1)

      !* -- Precompute src grid spherical coordinates

      coslats(:) = cos(grid1_center_lat(:))
      coslons(:) = cos(grid1_center_lon(:))
      sinlons(:) = sin(grid1_center_lon(:))
      sinlats(:) = sin(grid1_center_lat(:))

      call get_environment_variable(name='OASIS_OMP_NUM_THREADS', &
         & value=cl_envvar, status=il_err)
      if ( il_err .ne. 0) then
         il_envthreads = 0
      else
         read(cl_envvar,*) il_envthreads
      end if

!$OMP PARALLEL NUM_THREADS(il_envthreads) DEFAULT(NONE) &
!$OMP SHARED(sga_remap)&
!$OMP SHARED(il_nbthreads) &
!$OMP SHARED(ila_thr_sz,ila_thr_mn,ila_thr_mx) &
!$OMP SHARED(num_neigh,ll_debug,ila_dst,num_prev_links,nulou) &
!$OMP SHARED(grid1_add_map1,grid2_add_map1,wts_map1) &
!$OMP SHARED(grid1_center_lat,grid1_center_lon) &
!$OMP SHARED(grid2_center_lat,grid2_center_lon) &
!$OMP SHARED(grid1_mask,grid1_size) &
!$OMP SHARED(bin_lats,bin_addr1,num_srch_bins) &
!$OMP SHARED(coslats,coslons,sinlons,sinlats) &
!$OMP PRIVATE(nlogprt,ib_vmm,il_add,ib_dst,ib_src) &
!$OMP PRIVATE(coslatd,sinlatd,coslond,sinlond) &
!$OMP PRIVATE(lonwestd,loneastd,latsouthd,latnorthd) &
!$OMP PRIVATE(dist_min,il_nneiadd,d_dist,distance) &
!$OMP PRIVATE(il_lonkind,ib_bin,min_add,max_add) &
!$OMP PRIVATE(ib_thread,il_splitsize) 

!$OMP SINGLE

      il_nbthreads = 1
!$    il_nbthreads = OMP_GET_NUM_THREADS ()

      allocate(ila_thr_mn(il_nbthreads))
      allocate(ila_thr_mx(il_nbthreads))

      if (il_nbthreads .gt. 1) then

         nlogprt = 0

         allocate(ila_thr_sz(il_nbthreads))
         ila_thr_sz(:) = floor(real(num_neigh)/il_nbthreads)
         ila_thr_sz(1:num_neigh-sum(ila_thr_sz)) = ila_thr_sz(1:num_neigh-sum(ila_thr_sz)) + 1

         ila_thr_mn(1) = 1
         ila_thr_mx(1) = ila_thr_sz(1)

         do ib_thread = 2, il_nbthreads
            ila_thr_mn(ib_thread) = sum(ila_thr_sz(1:ib_thread-1)) + 1
            ila_thr_mx(ib_thread) = sum(ila_thr_sz(1:ib_thread))
         end do

         allocate(sga_remap(il_nbthreads))

         do ib_thread = 1, il_nbthreads
            il_splitsize = ila_thr_sz(ib_thread)
            sga_remap(ib_thread)%max_links = il_splitsize
            sga_remap(ib_thread)%num_links = 0
            allocate(sga_remap(ib_thread)%grid1_add(il_splitsize))
            allocate(sga_remap(ib_thread)%grid2_add(il_splitsize))
!EM            allocate(sga_remap(ib_thread)%wts(num_wts,il_splitsize))
         end do

         deallocate(ila_thr_sz)

      else

         ila_thr_mn(1) = 1
         ila_thr_mx(1) = num_neigh

      end if
!$OMP END SINGLE

      !* -- Find the nearest neighbours and store weights and address 

!$OMP DO SCHEDULE(STATIC,1)
      thread_loop: do ib_thread = 1, il_nbthreads

      grid_loop1: do ib_vmm = ila_thr_mn(ib_thread), ila_thr_mx(ib_thread)
         ib_dst = ila_dst(ib_vmm)
         il_add = num_prev_links+ib_vmm
         if (il_nbthreads .eq. 1) then
           grid2_add_map1(il_add) = ib_dst
         else
           sga_remap(ib_thread)%num_links = sga_remap(ib_thread)%num_links + 1
           sga_remap(ib_thread)%grid2_add(sga_remap(ib_thread)%num_links) = ib_dst
         endif

         if (ll_debug) then
            write(nulou,*) 'ib_dst for true=',ib_dst 
            write(nulou,*) 'counter_Vmm =',ib_vmm
            write(nulou,*) 'num_links+counter_Vmm =', il_add 
            write(nulou,*) 'dst_addr =',grid2_add_map1(il_add)
         endif

         coslatd = cos(grid2_center_lat(ib_dst))
         sinlatd = sin(grid2_center_lat(ib_dst))
         coslond = cos(grid2_center_lon(ib_dst))
         sinlond = sin(grid2_center_lon(ib_dst))

         ! Draw a rough box around the target point (relaxed at poles)

         if (grid2_center_lat(ib_dst) >= dp_pthresh) then

            latsouthd  = grid2_center_lat(ib_dst) - dp_box
            latnorthd  = pih
            lonwestd   = zero
            loneastd   = pi2
            il_lonkind = 0

         else if (grid2_center_lat(ib_dst) <= -dp_pthresh) then

            latsouthd  = -pih
            latnorthd  = grid2_center_lat(ib_dst) + dp_box
            lonwestd   = zero
            loneastd   = pi2
            il_lonkind = 0

         else

            latsouthd = grid2_center_lat(ib_dst) - dp_box
            latnorthd = grid2_center_lat(ib_dst) + dp_box
            lonwestd  = grid2_center_lon(ib_dst) - dp_box/coslatd
            loneastd  = grid2_center_lon(ib_dst) + dp_box/coslatd

            if (lonwestd < zero) then
               il_lonkind = -1
            else if (loneastd > pi2) then
               il_lonkind = 1
            else
               il_lonkind = 0
            end if

         end if

         dist_min = bignum
         il_nneiadd = 0

         ! Restrict the loop by bins

         min_add = grid1_size
         max_add = 1
         do ib_bin=1,num_srch_bins
            if (bin_lats(2,ib_bin).ge.latsouthd .and.&
              & bin_lats(1,ib_bin).le.latnorthd) then
               min_add = min(min_add, bin_addr1(1,ib_bin))
               max_add = max(max_add, bin_addr1(2,ib_bin))
            end if
         end do

         do ib_src = min_add, max_add

            ! Restrict the angular distance computation to a rough box

            if (grid1_mask(ib_src)) then

               if (grid1_center_lat(ib_src).le.latnorthd) then
                  if (grid1_center_lat(ib_src).ge.latsouthd) then
                     if (lf_loncheck(grid1_center_lon(ib_src),&
                        &            lonwestd,loneastd,il_lonkind)) then
                        d_dist = coslatd*coslats(ib_src) * &
                           &  (coslond*coslons(ib_src) + &
                           &   sinlond*sinlons(ib_src)) + &
                           &  sinlatd*sinlats(ib_src)
                        if (d_dist < -1.0d0) then
                           d_dist = -1.0d0
                        else if (d_dist > 1.0d0) then
                           d_dist = 1.0d0
                        end if
                        distance = acos(d_dist)
                        if (distance < dist_min) then
                           il_nneiadd = ib_src
                           dist_min = distance
                        end if
                     end if
                  end if
               end if
            end if
         end do

         ! If the rough box was too small, search the entire grid

         if (il_nneiadd == 0 ) then
            dist_min = bignum
            do ib_src = 1, grid1_size
               if (grid1_mask(ib_src)) then
                  d_dist = coslatd*coslats(ib_src) * &
                     (coslond*coslons(ib_src) + &
                     sinlond*sinlons(ib_src)) + &
                     sinlatd*sinlats(ib_src)
                  if (d_dist < -1.0d0) then
                     d_dist = -1.0d0
                  else if (d_dist > 1.0d0) then
                     d_dist = 1.0d0
                  end if
                  distance = acos(d_dist)
                  if (distance < dist_min) then
                     il_nneiadd = ib_src
                     dist_min = distance
                  end if
               end if
            end do
         end if

         if (il_nbthreads .eq. 1) then
           grid1_add_map1(il_add) = il_nneiadd
           wts_map1(1,il_add) = 1.0
         else
           sga_remap(ib_thread)%grid1_add(sga_remap(ib_thread)%num_links) = il_nneiadd
         endif
         if (ll_debug.and.nlogprt .ge. 2) then
            write(nulou,*) 'src_addr =',grid1_add_map1(il_add)
            write(nulou,*) '*************** Nearest source neighbour is ',il_nneiadd          
         end if
      end do grid_loop1

      end do thread_loop
!$OMP END DO

!$OMP END PARALLEL

      if (il_nbthreads .gt. 1) then
         sga_remap(1)%start_pos = num_prev_links + 1
         il_splitsize = sga_remap(1)%num_links
         do ib_thread = 2, il_nbthreads
            il_splitsize = il_splitsize + sga_remap(ib_thread)%num_links
            sga_remap(ib_thread)%start_pos = sga_remap(ib_thread-1)%start_pos + &
               sga_remap(ib_thread-1)%num_links
         end do

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
               sga_remap(ib_thread)%num_links-1) = 1.0
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

      deallocate(ila_thr_mn)
      deallocate(ila_thr_mx)
      if (il_nbthreads .gt. 1) then
         do ib_thread = 1, il_nbthreads
            deallocate(sga_remap(ib_thread)%grid1_add)
            deallocate(sga_remap(ib_thread)%grid2_add)
      !EM      deallocate(sga_remap(ib_thread)%wts)
         end do
         deallocate(sga_remap)
      end if




      !
      deallocate(ila_dst)
      !
      ! *----------------------------------------------------------------------
      !
      if (nlogprt .ge. 2) then
         write (UNIT = nulou,FMT = *) ' '
         write (UNIT = nulou,FMT = *) '   Leaving ROUTINE fracnnei  -  Level 4'
         write (UNIT = nulou,FMT = *) ' '
         call OASIS_FLUSH_SCRIP(nulou)
      end if

   contains

      logical (kind=log_kind) function lf_loncheck(rd_lon,rd_west,rd_east,id_kind)

         real (kind=dbl_kind), intent(in) :: rd_lon
         real (kind=dbl_kind), intent(in) :: rd_west,rd_east
         integer (kind=int_kind), intent(in) :: id_kind
         
         select case(id_kind)

         case(0)
            lf_loncheck = (rd_lon>=rd_west) .and. (rd_lon<=rd_east)
         case(-1)
            lf_loncheck = (rd_lon-pi2>=rd_west) .or. (rd_lon<=rd_east)
         case(1)
            lf_loncheck = (rd_lon>=rd_west) .or. (rd_lon+pi2<=rd_east)
         end select

      end function lf_loncheck

   end subroutine fracnnei

   !***********************************************************************


end module fracnnei_mod
