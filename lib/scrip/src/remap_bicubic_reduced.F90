MODULE remap_bicubic_reduced

!-----------------------------------------------------------------------
! BOP
!
! !MODULE: remap_bicubic_reduced
! 
! !USES:
  USE kinds_mod             ! defines common data types      
  USE constants             ! defines common constants      
  USE grids                 ! module containing grid info
  USE timers
  USE remap_vars            ! module containing remap info
  USE mod_oasis_flush
!$ use omp_lib

! !PUBLIC TYPES:
  IMPLICIT NONE  
! !PUBLIC MEMBER FUNCTIONS:
!  
! !PUBLIC DATA MEMBERS:

      real (kind=dbl_kind), dimension(:), allocatable, save ::  &
                                        coslat, sinlat,  & ! cosine, sine of grid lats (for distance) 
                                        coslon, sinlon     ! cosine, sine of grid lons (for distance) 
  
      integer (kind=int_kind) :: il_nbthreads = 1

      integer (kind=int_kind), PARAMETER :: num_neighbors = 16
 
! !DESCRIPTION:
!  This routine computes the weights for a bicubic interpolation
!  with a reduced grid. Computes mappings from grid1 to grid2.
!
! !REVISION HISTORY:
!  2002.10.21  J.Ghattas  created
!
! EOP
!-----------------------------------------------------------------------
! $Id: remap_bicubic_reduced.F90 2826 2010-12-10 11:14:21Z valcke $
! $Author: valcke $
!-----------------------------------------------------------------------
  


CONTAINS
  
!***********************************************************************
  
    
!-----------------------------------------------------------------------
! BOP
!
! !IROUTINE:  remap_bicub_reduced
!
! !INTERFACE:

  SUBROUTINE remap_bicub_reduced(ld_extrapdone, &
                                 mpi_comm_map, mpi_size_map, mpi_rank_map, mpi_root_map)

      
! !USES:
    
! !RETURN VALUE:
    
! !PARAMETERS:

    LOGICAL, INTENT(in) :: &
       ld_extrapdone              ! logical, true if EXTRAP done on field

    integer (kind=int_kind), INTENT(in) :: &
       mpi_comm_map, mpi_rank_map, mpi_size_map, mpi_root_map

    LOGICAL :: ll_nnei            ! true (default) if extra search is done 
    
    INTEGER (KIND=int_kind), DIMENSION(4,4) :: &
       ila_src_add                ! address for source points non-masked  
    
    INTEGER (KIND=int_kind), DIMENSION(4) :: &
       ila_nbr_found              ! nrb of points found on each latitude
    
    INTEGER (KIND=int_kind) :: &
       ib_i, &                    ! iter index
       ib_dst_add, &              ! destination address, target point
       il_count, &                ! nbr of latitudes with found points   
       il_min, il_max, bin        ! begin and end for distances calculation
    
    REAL (KIND=dbl_kind), DIMENSION(4,4) :: &
       rla_src_lons, &            ! longitudes for the points 'ila_src_add'
       rla_weight, &              ! bicubic weights for the points 'ila_src_add'
       rla_wght_lon               ! temp. weights
    
    REAL (KIND=dbl_kind), DIMENSION(4) :: &
       rla_src_lats, &            ! latitudes for the points 'ila_src_add'
       rla_lats_temp, &           ! temp. latitudes
       rla_wght_lat, rla_wght_temp! temp. weights
    
    REAL (KIND=dbl_kind) :: &
       rl_plat, rl_plon         ! latitude and longitude for destination address
    
    REAL (KIND=dbl_kind) :: &     ! variables for distances calculation
       rl_coslat_dst, rl_sinlat_dst, &
       rl_coslon_dst, rl_sinlon_dst, &
       rl_distance, d_dist           

    REAL (KIND=dbl_kind), DIMENSION(2) :: &
       rla_dist                   ! lat distances to point cible     

    INTEGER (KIND=int_kind), DIMENSION(4) :: &
       ila_add_dist               ! temporary addr. for distances       

    LOGICAL :: ll_linear          ! flag

    INTEGER (KIND=int_kind) :: il_splitsize
    INTEGER (KIND=int_kind) :: ib_proc
    INTEGER (KIND=int_kind) :: ib_thread
    INTEGER (KIND=int_kind) :: buff_base
    INTEGER (KIND=int_kind), DIMENSION(:), ALLOCATABLE :: ila_mpi_sz
    INTEGER (KIND=int_kind), DIMENSION(:), ALLOCATABLE :: ila_mpi_mn
    INTEGER (KIND=int_kind), DIMENSION(:), ALLOCATABLE :: ila_mpi_mx
    INTEGER (KIND=int_kind), DIMENSION(:), ALLOCATABLE :: ila_thr_sz
    INTEGER (KIND=int_kind), DIMENSION(:), ALLOCATABLE :: ila_thr_mn
    INTEGER (KIND=int_kind), DIMENSION(:), ALLOCATABLE :: ila_thr_mx
    INTEGER (KIND=int_kind), DIMENSION(:), ALLOCATABLE :: ila_num_links_mpi
    INTEGER (KIND=int_kind), DIMENSION(:,:), ALLOCATABLE :: ila_req_mpi
    INTEGER (KIND=int_kind), DIMENSION(:,:,:), ALLOCATABLE :: ila_sta_mpi
    CHARACTER (LEN=14) :: cl_envvar
    INTEGER (KIND=int_kind) :: il_envthreads, il_err


#ifdef REMAP_TIMING
      LOGICAL :: ll_timing = .true.
      REAL    (KIND=dbl_kind), DIMENSION(:,:), ALLOCATABLE :: dla_timer
      REAL    (KIND=dbl_kind) :: dl_tstart, dl_tstop
      INTEGER (KIND=int_kind) :: il_mythread, n
#endif

    
! !DESCRIPTION:
!  This routine computes the weights for a bicubic interpolation
!  with a reduced grid. Computes mappings from grid1 to grid2.     
! 
! !REVISION HISTORY:
!  2002.10.21  J.Ghattas   created
!
! EOP
!-----------------------------------------------------------------------
! $Id: remap_bicubic_reduced.F90 2826 2010-12-10 11:14:21Z valcke $
! $Author: valcke $
!-----------------------------------------------------------------------
!
    IF (nlogprt .GE. 2) THEN
      WRITE (UNIT = nulou,FMT = *)' '
      WRITE (UNIT = nulou,FMT = *) 'Entering routine remap_bicub_reduced'
      CALL OASIS_FLUSH_SCRIP(nulou)
    ENDIF

    ll_nnei = .true.

#ifdef REMAP_TIMING
    if (ll_timing) call timer_start(2,'remap_bicubic_reduced alloc')
#endif

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
      do ib_dst_add = 1, grid2_size
        if (grid2_mask(ib_dst_add)) then
          if (ila_mpi_mn(ib_proc).eq.0) &
            ila_mpi_mn(ib_proc) = ib_dst_add
          il_splitsize = il_splitsize + 1
          if (il_splitsize .eq. ila_mpi_sz(ib_proc)) then
            il_splitsize = 0
            ila_mpi_mx(ib_proc) = ib_dst_add
            ib_proc = ib_proc + 1
          end if
        end if
       end do
!   
       deallocate(ila_mpi_sz)

    else

      ila_mpi_mn(1) = 1
      ila_mpi_mx(1) = grid2_size

    endif


    call get_environment_variable(name='OASIS_OMP_NUM_THREADS', value=cl_envvar, status=il_err)
    if ( il_err .ne. 0) then
      il_envthreads = 0
    else
      read(cl_envvar,*) il_envthreads
    end if

!$OMP PARALLEL NUM_THREADS(il_envthreads) DEFAULT(NONE) &
!$OMP SHARED(ld_extrapdone) &
!$OMP SHARED(grid2_mask) &
!$OMP SHARED(il_envthreads) &
!$OMP SHARED(grid2_center_lat,grid2_center_lon) &
!$OMP SHARED(grid1_center_lat,grid1_center_lon) &
!$OMP SHARED(grid1_mask) &
!$OMP SHARED(bin_addr1_r,num_srch_red,sga_remap,num_wts) &
!$OMP SHARED(nulou) &
!$OMP SHARED(il_nbthreads) &
!$OMP SHARED(mpi_rank_map,mpi_root_map,ila_mpi_mn,ila_mpi_mx) &
!$OMP SHARED(ila_thr_sz,ila_thr_mn,ila_thr_mx) &
!$OMP SHARED(grid1_dims,grid1_size) &
!$OMP PRIVATE(nlogprt) &
!$OMP PRIVATE(ib_dst_add) &
!$OMP PRIVATE(ila_src_add) &
!$OMP PRIVATE(rl_coslat_dst,rl_coslon_dst,rl_sinlat_dst,rl_sinlon_dst) &
!$OMP PRIVATE(rl_plat,rl_plon) &
!$OMP PRIVATE(rla_src_lats,rla_src_lons) &
!$OMP PRIVATE(ila_nbr_found,ila_add_dist,bin,il_min,il_max,ib_i,ll_linear) &
!$OMP PRIVATE(rla_dist,rla_wght_lon,rla_wght_lat,rla_wght_temp,rla_lats_temp) &
!$OMP PRIVATE(d_dist) &
!$OMP PRIVATE(rla_weight,rl_distance,il_count) &
!$OMP SHARED(coslat,coslon,sinlat,sinlon) &
!$OMP SHARED(ll_nnei) &
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
!$  il_nbthreads = OMP_GET_NUM_THREADS ()


#ifdef REMAP_TIMING
    if (ll_timing) then
      if (il_nbthreads.gt.1) then
!$      dl_tstart = OMP_GET_WTIME()
      else
        call timer_start(3,'remap_bicubic_reduced distr')
      end if
    end if
#endif

    allocate(ila_thr_mn(il_nbthreads))
    allocate(ila_thr_mx(il_nbthreads))

    if (il_nbthreads .gt. 1) then

#ifdef REMAP_TIMING
      if (ll_timing) then
        allocate(dla_timer(il_nbthreads,6))
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
      do ib_dst_add = ila_mpi_mn(mpi_rank_map+1), ila_mpi_mx(mpi_rank_map+1)
        if (grid2_mask(ib_dst_add)) then
          if (ila_thr_mn(ib_thread).eq.0) &
            ila_thr_mn(ib_thread) = ib_dst_add
          il_splitsize = il_splitsize + 1
          if (il_splitsize .eq. ila_thr_sz(ib_thread)) then
            il_splitsize = 0
            ila_thr_mx(ib_thread) = ib_dst_add
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
!$      dl_tstop = OMP_GET_WTIME() 
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

      grid_loop1:DO ib_dst_add = ila_thr_mn(ib_thread), ila_thr_mx(ib_thread)

      ll_linear=.false.

      IF (.NOT. grid2_mask(ib_dst_add)) THEN
        CYCLE grid_loop1     ! target point is masked
      END IF

#ifdef REMAP_TIMING
      if (ll_timing) then
        if (il_nbthreads.gt.1) then
!$        dl_tstart = OMP_GET_WTIME()
        else
          call timer_start(4,'remap_bicubic_reduced search')
        end if
      end if
#endif
      
      rl_plat = grid2_center_lat(ib_dst_add)
      rl_plon = grid2_center_lon(ib_dst_add)
      
      !
      !   Search for non-masked points among the closest 16 points 
      !   on source grid (grid1)
      !
      CALL grid_search_16_points(ila_src_add,   rla_src_lats, rla_src_lons,&
                                 ila_nbr_found, bin,          rl_plat, &
                                 rl_plon,       ld_extrapdone)

#ifdef REMAP_TIMING
      if (ll_timing) then
        if (il_nbthreads.gt.1) then
!$        dl_tstop = OMP_GET_WTIME() 
          dla_timer(ib_thread,2) = dla_timer(ib_thread,2) + dl_tstop - dl_tstart
        else
          call timer_stop(4)
        end if
      end if
#endif
      !
      ! If there is no point found, search the nearest 
      ! non-masked point
      !
#ifdef REMAP_TIMING
          if (ll_timing) then
            if (il_nbthreads.gt.1) then
!$            dl_tstart = OMP_GET_WTIME()
            else
              call timer_start(5,'remap_bicubic_reduced partial neighbours')
            end if
          end if
#endif
          
      IF (SUM(ila_nbr_found)==0) THEN
        IF (ll_nnei .EQV. .TRUE. ) THEN
          IF (nlogprt .GE. 2) THEN
              WRITE(nulou,*) '  '
              WRITE(nulou,*) &
                 'All 16 surrounding source grid points are masked'
              WRITE(nulou,*) 'for target point ',ib_dst_add
              WRITE(nulou,*) 'with longitude and latitude', rl_plon, rl_plat
              WRITE(nulou,*) 'Using the nearest non-masked neighbour.' 
              WRITE(nulou,*) ' '
              CALL OASIS_FLUSH_SCRIP(nulou)
          ENDIF

          ! Search the nearest point in bin [il_min:il_max]
          IF (bin==0 .or. bin==1) THEN
            il_min=1
            il_max=bin_addr1_r(2,3)
          ELSE IF (bin==num_srch_red .or. bin==num_srch_red-1) THEN
            il_min=bin_addr1_r(1,num_srch_red-2)
            il_max=bin_addr1_r(2,num_srch_red)
          ELSE
            il_min=bin_addr1_r(1,bin-1)+1
            il_max=bin_addr1_r(2,bin+2)
          END IF
         
          rl_coslat_dst = COS(rl_plat)
          rl_sinlat_dst = SIN(rl_plat)
          rl_coslon_dst = COS(rl_plon)
          rl_sinlon_dst = SIN(rl_plon)
          
          rla_weight(1,1) = bignum
          ila_src_add(1,1) = 0
!cdir novector
          DO ib_i=il_min, il_max                            
            IF (grid1_mask(ib_i) .or. ld_extrapdone) THEN
              d_dist = rl_coslat_dst*coslat(ib_i)* &
                       (rl_coslon_dst*coslon(ib_i) + &
                       rl_sinlon_dst*sinlon(ib_i))+&
                       rl_sinlat_dst*sinlat(ib_i)
              IF (d_dist < -1.0d0) THEN
                d_dist = -1.0d0
              ELSE IF (d_dist > 1.0d0) THEN
                d_dist = 1.0d0
              END IF
              rl_distance = ACOS(d_dist)
              IF (rl_distance < rla_weight(1,1)) THEN
                rla_weight(1,1) = rl_distance
                ila_src_add(1,1) = ib_i
              END IF
            END IF
          END DO

          IF (ila_src_add(1,1) == 0) THEN
            rla_weight(1,1) = bignum
!cdir novector
            DO ib_i=1,grid1_size                    
              IF (grid1_mask(ib_i) .or. ld_extrapdone) THEN
                d_dist = rl_coslat_dst*coslat(ib_i)* &
                         (rl_coslon_dst*coslon(ib_i) + &
                         rl_sinlon_dst*sinlon(ib_i))+&
                         rl_sinlat_dst*sinlat(ib_i)
                IF (d_dist < -1.0d0) THEN
                  d_dist = -1.0d0
                ELSE IF (d_dist > 1.0d0) THEN
                  d_dist = 1.0d0
                END IF
                rl_distance = ACOS(d_dist)
                IF (rl_distance < rla_weight(1,1)) THEN
                  rla_weight(1,1) = rl_distance
                  ila_src_add(1,1) = ib_i
                END IF
               END IF
             END DO
          ENDIF

          if (ila_src_add(1,1) == 0) then
             WRITE(nulou,*) 'Problem with neighbour identification for target grid point'
             WRITE(nulou,*) 'with address = ',ib_dst_add 
             call abort
          endif

          rla_weight(:,:) = 0
          rla_weight(1,1) = 1
          CALL store_link_bicub(ib_dst_add, ila_src_add, rla_weight, ib_thread)
          IF (nlogprt .GE. 2) THEN
            WRITE(nulou,*)  &
               'Nearest non masked neighbour is source point ', &
               ila_src_add(1,1)
            WRITE(nulou,*) 'with longitude and latitude', &
               grid1_center_lon(ila_src_add(1,1)), &
               grid1_center_lat(ila_src_add(1,1)) 
            WRITE(nulou,*) '  '
          ENDIF
#ifdef REMAP_TIMING
          if (ll_timing) then
            if (il_nbthreads.gt.1) then
!$            dl_tstop = OMP_GET_WTIME() 
              dla_timer(ib_thread,3) = dla_timer(ib_thread,3) + dl_tstop - dl_tstart
            else
              call timer_stop(5)
            end if
          end if
#endif
          CYCLE grid_loop1
        ENDIF
      END IF

      rla_weight(:,:) = 0
      ! if there is only one point found, save it
      IF (SUM(ila_nbr_found)==1) THEN   
        DO ib_i=1,4
          IF (ila_nbr_found(ib_i)==1) THEN
            rla_weight(ib_i,1)=1
            EXIT
          END IF
        END DO
        CALL store_link_bicub(ib_dst_add, ila_src_add, rla_weight, ib_thread)
#ifdef REMAP_TIMING
        if (ll_timing) then
          if (il_nbthreads.gt.1) then
!$          dl_tstop = OMP_GET_WTIME() 
            dla_timer(ib_thread,3) = dla_timer(ib_thread,3) + dl_tstop - dl_tstart
          else
            call timer_stop(5)
          end if
        end if
#endif
        CYCLE grid_loop1
      END IF

      ! if there are only 2 points found, distance weighted average 
      IF (SUM(ila_nbr_found)==2) THEN
        rl_coslat_dst = COS(rl_plat)
        rl_sinlat_dst = SIN(rl_plat)
        rl_coslon_dst = COS(rl_plon)
        rl_sinlon_dst = SIN(rl_plon)
                              
        rl_distance=0  ! count of total distance 
        DO ib_i=1,4
          IF (ila_nbr_found(ib_i) > 0) THEN
            d_dist = rl_coslat_dst*COS(rla_src_lats(ib_i))* &
                     (rl_coslon_dst*COS(rla_src_lons(ib_i,1)) + &
                     rl_sinlon_dst*SIN(rla_src_lons(ib_i,1)))+&
                     rl_sinlat_dst*SIN(rla_src_lats(ib_i))
            IF (d_dist < -1.0d0) THEN
              d_dist = -1.0d0
            ELSE IF (d_dist > 1.0d0) THEN
              d_dist = 1.0d0
            END IF
            rla_weight(ib_i,1) = ACOS(d_dist)
            rl_distance = rl_distance+rla_weight(ib_i,1)
            IF (ila_nbr_found(ib_i)==2) THEN
              d_dist = rl_coslat_dst*COS(rla_src_lats(ib_i))* &
                       (rl_coslon_dst*COS(rla_src_lons(ib_i,2)) + &
                       rl_sinlon_dst*SIN(rla_src_lons(ib_i,2)))+&
                       rl_sinlat_dst*SIN(rla_src_lats(ib_i))
              IF (d_dist < -1.0d0) THEN
                d_dist = -1.0d0
              ELSE IF (d_dist > 1.0d0) THEN
                d_dist = 1.0d0
              END IF
              rla_weight(ib_i,2) =  ACOS(d_dist)
              rl_distance = rl_distance+rla_weight(ib_i,2)
            END IF
          END IF
        END DO
        rla_weight=rla_weight/rl_distance

        CALL store_link_bicub(ib_dst_add, ila_src_add, rla_weight, ib_thread)
#ifdef REMAP_TIMING
        if (ll_timing) then
          if (il_nbthreads.gt.1) then
!$          dl_tstop = OMP_GET_WTIME() 
            dla_timer(ib_thread,3) = dla_timer(ib_thread,3) + dl_tstop - dl_tstart
          else
            call timer_stop(5)
          end if
        end if
#endif
        CYCLE grid_loop1
      END IF
      
#ifdef REMAP_TIMING
      if (ll_timing) then
        if (il_nbthreads.gt.1) then
!$        dl_tstop = OMP_GET_WTIME() 
          dla_timer(ib_thread,3) = dla_timer(ib_thread,3) + dl_tstop - dl_tstart
        else
          call timer_stop(5)
        end if
      end if
#endif

#ifdef REMAP_TIMING
      if (ll_timing) then
        if (il_nbthreads.gt.1) then
!$         dl_tstart = OMP_GET_WTIME()
        else
           call timer_start(6,'remap_bicubic_reduced weights')
        end if
      end if
#endif

      ! Some case exceptional when just one point per line found 
      
      IF (ila_nbr_found(1)==1) THEN  ! elimination of point
        ila_nbr_found(1)=0
        ila_src_add(1,1)=0
      END IF
      IF (ila_nbr_found(4)==1) THEN 
        ila_nbr_found(4)=0
        ila_src_add(4,1)=0
      END IF
      
      IF (ila_nbr_found(2)==1 .or. ila_nbr_found(3)==1) THEN
        ila_add_dist(:)=4
        rla_dist(:)=bignum

        ! search for the 2 nearest points or line of points
        DO ib_i=1,4
          IF (ila_nbr_found(ib_i) > 1) THEN
            rl_distance=ABS(rla_src_lats(ib_i)-rl_plat)                
          ELSE IF (ila_nbr_found(ib_i)==1) THEN
            rl_coslat_dst = COS(rl_plat)
            rl_sinlat_dst = SIN(rl_plat)
            rl_coslon_dst = COS(rl_plon)
            rl_sinlon_dst = SIN(rl_plon)
            d_dist = rl_coslat_dst*COS(rla_src_lats(ib_i))* &
                     (rl_coslon_dst*COS(rla_src_lons(ib_i,1)) + &
                     rl_sinlon_dst*SIN(rla_src_lons(ib_i,1)))+&
                     rl_sinlat_dst*SIN(rla_src_lats(ib_i)) 
            IF (d_dist < -1.0d0) THEN
              d_dist = -1.0d0
            ELSE IF (d_dist > 1.0d0) THEN
              d_dist = 1.0d0
            END IF
            rl_distance= ACOS(d_dist)
          ELSE
            rl_distance=bignum
          END IF

          IF (rl_distance < rla_dist(1)) THEN
            ila_add_dist(2)=ila_add_dist(1)
            ila_add_dist(1)=ib_i
            rla_dist(2)=rla_dist(1)
            rla_dist(1)=rl_distance
          ELSE IF (rl_distance < rla_dist(2)) THEN
            ila_add_dist(2)=ib_i
            rla_dist(2)=rl_distance
          END IF
        END DO

        IF (ila_nbr_found(ila_add_dist(1))>1 .and. &
            ila_nbr_found(ila_add_dist(2))>1) THEN
          ! linearie
          ll_linear=.true.             
        ELSE 
          ! do distance weighted averege
          rla_wght_lon(:,:)=0
          DO ib_i=1,2
            SELECT CASE (ila_nbr_found(ila_add_dist(ib_i)))
            CASE (4)
              CALL calcul_wght_irreg(rla_src_lons(ila_add_dist(ib_i),:),&
                                     rl_plon, rla_wght_lon(ila_add_dist(ib_i),:))        
              rla_wght_lon(ila_add_dist(ib_i),:) = rla_wght_lon(ila_add_dist(ib_i),:)/& 
                                                   rla_dist(ib_i)
            CASE (3)
              CALL calcul_wght_3(rla_src_lons(ila_add_dist(ib_i),1:3),&
                                 rl_plon, rla_wght_lon(ila_add_dist(ib_i),1:3))
              rla_wght_lon(ila_add_dist(ib_i),1:3) = rla_wght_lon(ila_add_dist(ib_i),1:3)/& 
                                                     rla_dist(ib_i)
            CASE (2)            
              CALL calcul_wght_2(rla_src_lons(ila_add_dist(ib_i),1:2),&
                                 rl_plon, rla_wght_lon(ila_add_dist(ib_i),1:2))
              rla_wght_lon(ila_add_dist(ib_i),1:2) = rla_wght_lon(ila_add_dist(ib_i),1:2)/& 
                                                     rla_dist(ib_i)
            CASE (1)
              rla_wght_lon(ila_add_dist(ib_i),1) = 1/rla_dist(ib_i)
            END SELECT
          END DO

          rl_distance=0
          DO ib_i=1,4
            rl_distance=rl_distance + sum(rla_wght_lon(ib_i,:))
          END DO

          rla_weight(:,:)=rla_wght_lon(:,:)/rl_distance

          CALL store_link_bicub(ib_dst_add, ila_src_add , rla_weight, ib_thread)
#ifdef REMAP_TIMING
          if (ll_timing) then
            if (il_nbthreads.gt.1) then
!$            dl_tstop = OMP_GET_WTIME() 
              dla_timer(ib_thread,4) = dla_timer(ib_thread,4) + dl_tstop - dl_tstart
            else
              call timer_stop(6)
            end if
          end if
#endif
          CYCLE grid_loop1
        END IF
      END IF

      !
      ! Calculation of weights for longitudes
      !  
     
      rla_wght_lon(:,:)=0       
      DO ib_i=1,4                         
        SELECT CASE (ila_nbr_found(ib_i))
        CASE (4)              
            CALL calcul_wght_irreg(rla_src_lons(ib_i,:), rl_plon, rla_wght_lon(ib_i,:))
        CASE (3)
            CALL calcul_wght_3(rla_src_lons(ib_i,1:3), rl_plon, rla_wght_lon(ib_i,1:3))
        CASE (2)            
            CALL calcul_wght_2(rla_src_lons(ib_i,1:2), rl_plon, rla_wght_lon(ib_i,1:2))
        END SELECT        
      END DO

      IF (ll_linear) THEN
        rla_wght_lat(:)=0         
        CALL calcul_wght_2(rla_src_lats(ila_add_dist(:)), rl_plat, rla_wght_temp(1:2))
        rla_wght_lat(ila_add_dist(1))=rla_wght_temp(1)
        rla_wght_lat(ila_add_dist(2))=rla_wght_temp(2)

        DO ib_i=1,4
          rla_weight(ib_i,:)=rla_wght_lat(ib_i)*rla_wght_lon(ib_i,:)
        END DO
          
        CALL store_link_bicub(ib_dst_add, ila_src_add , rla_weight, ib_thread)
#ifdef REMAP_TIMING
        if (ll_timing) then
          if (il_nbthreads.gt.1) then
!$          dl_tstop = OMP_GET_WTIME() 
            dla_timer(ib_thread,4) = dla_timer(ib_thread,4) + dl_tstop - dl_tstart
          else
            call timer_stop(6)
          end if
        end if
#endif
        CYCLE grid_loop1
      END IF
    
      !
      ! Calculation of weights for latitudes
      !
      il_count=0
      DO ib_i=1,4
        IF (ila_nbr_found(ib_i)/=0) THEN
          il_count=il_count+1
          rla_lats_temp(il_count)=rla_src_lats(ib_i)
        END IF
      END DO
      
      SELECT CASE (il_count)
      CASE (4)              
        CALL calcul_wght_irreg(rla_lats_temp, rl_plat, rla_wght_temp(:))
      CASE (3)
        CALL calcul_wght_3(rla_lats_temp(1:3), rl_plat, rla_wght_temp(1:3))
      CASE (2)
        CALL calcul_wght_2(rla_lats_temp(1:2), rl_plat, rla_wght_temp(1:2))
      CASE (1)
        rla_wght_temp(1)=1
      END SELECT
      
      il_count=0
      DO ib_i=1,4
        IF (ila_nbr_found(ib_i)/=0) THEN
          il_count=il_count+1
          rla_wght_lat(ib_i)=rla_wght_temp(il_count)
        ELSE
          rla_wght_lat(ib_i)=0
        END IF
      END DO
      ! 
      ! Calculation of total weight, elementwise multiplication
      !
        
      DO ib_i=1,4
        rla_weight(ib_i,:)=rla_wght_lat(ib_i)*rla_wght_lon(ib_i,:)
      END DO     
      
      CALL store_link_bicub(ib_dst_add, ila_src_add , rla_weight, ib_thread)
#ifdef REMAP_TIMING
      if (ll_timing) then
        if (il_nbthreads.gt.1) then
!$        dl_tstop = OMP_GET_WTIME() 
          dla_timer(ib_thread,4) = dla_timer(ib_thread,4) + dl_tstop - dl_tstart
        else
          call timer_stop(6)
        end if
      end if
#endif

    END DO grid_loop1
    end do thread_loop
!$OMP END DO

!$OMP END PARALLEL

    if (il_nbthreads .gt. 1) then
#ifdef REMAP_TIMING
      if (ll_timing) call timer_start(3,'remap_bicubic_reduced gather_lk')
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
                  & 'remap_bicubic_reduced distr ',dla_timer(ib_thread,1)
          write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_bicubic_reduced search ',dla_timer(ib_thread,2)
          write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_bicubic_reduced partial neighbours',dla_timer(ib_thread,3)
          write(nulou,"(' Elapsed time for timer ',A24,':',1x,f10.4)")&
                  & 'remap_bicubic_reduced weights',dla_timer(ib_thread,4)
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

        DO ib_i = 1, mpi_size_map-1
        
          buff_base = SUM( ila_num_links_mpi(1:ib_i) ) + 1

          CALL MPI_IRecv(grid1_add_map1(buff_base),&
                   & ila_num_links_mpi(ib_i+1),MPI_INT,ib_i,1,mpi_comm_map,&
                   & ila_req_mpi(1,ib_i),il_err)

          CALL MPI_IRecv(grid2_add_map1(buff_base),&
                   & ila_num_links_mpi(ib_i+1),MPI_INT,ib_i,2,mpi_comm_map,&
                   & ila_req_mpi(2,ib_i),il_err)
 
          CALL MPI_IRecv(wts_map1(:,buff_base),&
                   & num_wts*ila_num_links_mpi(ib_i+1),MPI_DOUBLE,ib_i,0,mpi_comm_map,&
                   & ila_req_mpi(3,ib_i),il_err)

          CALL MPI_IRecv(grid2_frac(ila_mpi_mn(ib_i+1)),&
                   & ila_mpi_mx(ib_i+1)-ila_mpi_mn(ib_i+1)+1,MPI_DOUBLE,ib_i,0,mpi_comm_map,&
                   & ila_req_mpi(4,ib_i),il_err)

        END DO

        DO ib_i=1,4
          CALL MPI_Waitall(mpi_size_map-1,ila_req_mpi(ib_i,:),ila_sta_mpi(1,ib_i,1),il_err)
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
      WRITE (UNIT = nulou,FMT = *) 'Leaving routine remap_bicub_reduced'
      CALL OASIS_FLUSH_SCRIP(nulou)
    ENDIF
!          
  END SUBROUTINE remap_bicub_reduced
    
    
!-----------------------------------------------------------------------
! BOP
!
! !IROUTINE: grid_search_16_points
!
! !INTERFACE:
!  
  SUBROUTINE grid_search_16_points(ida_src_add,   rda_src_lats, rda_src_lons,&
                                   ida_nbr_found, bin,          rd_plat, &
                                   rd_plon,       ld_extrapdone)
!    
! !USES:
!  
! !RETURN VALUE:
!    
    INTEGER (KIND=int_kind), DIMENSION(4,4), INTENT(out) :: &
       ida_src_add    ! searched addresses of the unmasked points enclosing
                      ! target point
      
    REAL (KIND=dbl_kind), DIMENSION(4,4), INTENT(out) :: &
       rda_src_lons   ! longitudes of the searched points

    REAL (KIND=dbl_kind), DIMENSION(4), INTENT(out) :: &
       rda_src_lats   ! latitudes  of the searched points 
    
    INTEGER (KIND=int_kind), DIMENSION(4), INTENT(out) :: &
       ida_nbr_found  ! indicates for each line how many points found
    
    INTEGER (KIND=int_kind), INTENT(out) :: &
       bin            ! actual bin for target point
!    
! !PARAMETERS:
!  
    REAL (KIND=dbl_kind), INTENT(in) :: &
       rd_plat, &     ! latitude  of the target point
       rd_plon      ! longitude of the target point
          
    LOGICAL, INTENT(in) :: ld_extrapdone ! true if extrapolation done
    
    INTEGER (KIND=int_kind) :: &
       ib_k, ib_j, ib_i, &        ! iteration indices
       il_min, il_max, il_inter   ! begin and end for actual bin
    
    INTEGER (KIND=int_kind), DIMENSION(4,2) :: &
       ila_corners                ! temp addresses for bins   
                       
!
! !DESCRIPTION:   
!  This routine finds the location of the target point in the source
!  grid and returns those of the 16 nearest points that are unmasked.
!  The source grid is a reduced grid. 
!
! !REVISION HISTORY:
!  2002.10.21  J.Ghattas   created
!
! EOP
!-----------------------------------------------------------------------
! $Id: remap_bicubic_reduced.F90 2826 2010-12-10 11:14:21Z valcke $
! $Author: valcke $
!-----------------------------------------------------------------------   
      
     
    !
    ! serch of actual bin
    !
     
    
    IF (rd_plat > bin_lats_r(1,1)) THEN ! norther of the first bin
        bin=0
        ila_corners(1:2,1:2)= 0          
        ila_corners(3,1)= bin_addr1_r(1,1)+1
        ila_corners(3,2)= bin_addr1_r(2,1)
        ila_corners(4,1)= bin_addr1_r(1,2)
        ila_corners(4,2)= bin_addr1_r(2,2)
        
    ELSE IF (rd_plat > bin_lats_r(1,2)) THEN ! in the first bin
        bin=1
        ila_corners(1,1:2)= 0
        ila_corners(2,1)= bin_addr1_r(1,1)+1
        ila_corners(2,2)= bin_addr1_r(2,1)
        ila_corners(3,1)= bin_addr1_r(1,2)
        ila_corners(3,2)= bin_addr1_r(2,2)
        ila_corners(4,1)= bin_addr1_r(1,3)  
        ila_corners(4,2)= bin_addr1_r(2,3)
                
    ELSE IF (rd_plat < bin_lats_r(1,num_srch_red)) THEN 
        ! South of the last complet bin
        bin=num_srch_red
        ila_corners(1,1) = bin_addr1_r(1,num_srch_red-1)
        ila_corners(1,2) = bin_addr1_r(2,num_srch_red-1)
        ila_corners(2,1) = bin_addr1_r(1,num_srch_red)
        ila_corners(2,2) = bin_addr1_r(2,num_srch_red)
        ila_corners(3:4,1:2) = 0                               
                    
    ELSE IF (rd_plat < bin_lats_r(1,num_srch_red-1)) THEN
        ! in the last bin which is complet
        ! the bin (num_srch_red-1) is the last bin which is complet
        bin=num_srch_red-1
        ila_corners(1,1) = bin_addr1_r(1,num_srch_red-2)
        ila_corners(1,2) = bin_addr1_r(2,num_srch_red-2)
        ila_corners(2,1) = bin_addr1_r(1,num_srch_red-1)
        ila_corners(2,2) = bin_addr1_r(2,num_srch_red-1)
        ila_corners(3,1) = bin_addr1_r(1,num_srch_red)
        ila_corners(3,2) = bin_addr1_r(2,num_srch_red)
        ila_corners(4,1:2) = 0           
    ELSE 
        il_min=2
        il_max=num_srch_red-1
        DO WHILE (il_min /= il_max-1)
          il_inter=(il_max-il_min)/2 + il_min
          IF (rd_plat <= bin_lats_r(1,il_min) .and. &
             rd_plat > bin_lats_r(1,il_inter)) THEN
              il_max=il_inter
          ELSE
              il_min=il_inter
          END IF
        END DO
        bin=il_min
        ila_corners(1,1) = bin_addr1_r(1,bin-1)
        ila_corners(1,2) = bin_addr1_r(2,bin-1)
        ila_corners(2,1) = bin_addr1_r(1,bin)
        ila_corners(2,2) = bin_addr1_r(2,bin)
        ila_corners(3,1) = bin_addr1_r(1,bin+1) 
        ila_corners(3,2) = bin_addr1_r(2,bin+1)
        ila_corners(4,1) = bin_addr1_r(1,bin+2) 
        ila_corners(4,2) = bin_addr1_r(2,bin+2) 
        
        IF (ila_corners(1,1)==0) THEN 
            ila_corners(1,1)=1
        END IF
    END IF
        
    DO ib_k=1,4 
      IF (ila_corners(ib_k,1) .NE. 0)        &
         rda_src_lats(ib_k)= grid1_center_lat(ila_corners(ib_k,1))
    ENDDO

    !
    ! now perform a more detailed search for each line
    !
     
    ida_src_add(:,:)=0
    ida_nbr_found(:)=0
    rda_src_lons(:,:)=0
    
    DO ib_k=1,4 ! for each line of found points
      IF (ila_corners(ib_k,1)==0) THEN
          CYCLE 
      END IF

      il_min=ila_corners(ib_k,1)
      il_max=ila_corners(ib_k,2)

      IF (rd_plon < grid1_center_lon(il_min)) THEN                       
          DO ib_j=il_max-1, il_max
            IF (grid1_mask(ib_j) .or. ld_extrapdone) THEN 
                ida_nbr_found(ib_k)=ida_nbr_found(ib_k)+1
                ida_src_add(ib_k,ida_nbr_found(ib_k)) = ib_j
                rda_src_lons(ib_k,ida_nbr_found(ib_k))= &
                   grid1_center_lon(ib_j)-pi2
            END IF
          END DO
          DO ib_j=il_min, il_min+1
            IF (grid1_mask(ib_j) .or. ld_extrapdone) THEN 
                ida_nbr_found(ib_k)=ida_nbr_found(ib_k)+1
                ida_src_add(ib_k,ida_nbr_found(ib_k)) = ib_j
                rda_src_lons(ib_k,ida_nbr_found(ib_k))= &
                   grid1_center_lon(ib_j)
            END IF
          END DO
          
      ELSE IF (rd_plon < grid1_center_lon(il_min+1)) THEN
          IF (grid1_mask(il_max) .or. ld_extrapdone) THEN 
              ida_nbr_found(ib_k)=ida_nbr_found(ib_k)+1
              ida_src_add(ib_k,ida_nbr_found(ib_k)) = il_max
              rda_src_lons(ib_k,ida_nbr_found(ib_k))= &
                 grid1_center_lon(il_max)-pi2
          END IF
          DO ib_j=il_min, il_min+2
            IF (grid1_mask(ib_j) .or. ld_extrapdone) THEN 
                ida_nbr_found(ib_k)=ida_nbr_found(ib_k)+1
                ida_src_add(ib_k,ida_nbr_found(ib_k)) = ib_j
                rda_src_lons(ib_k,ida_nbr_found(ib_k))= &
                   grid1_center_lon(ib_j)
            END IF
          END DO
          
      ELSE IF (rd_plon > grid1_center_lon(il_max)) THEN
          DO ib_j=il_max-1, il_max
            IF (grid1_mask(ib_j) .or. ld_extrapdone) THEN 
                ida_nbr_found(ib_k)=ida_nbr_found(ib_k)+1
                ida_src_add(ib_k,ida_nbr_found(ib_k)) = ib_j
                rda_src_lons(ib_k,ida_nbr_found(ib_k))= &
                   grid1_center_lon(ib_j)
            END IF
          END DO
          DO ib_j=il_min, il_min+1
            IF (grid1_mask(ib_j) .or. ld_extrapdone) THEN 
                ida_nbr_found(ib_k)=ida_nbr_found(ib_k)+1
                ida_src_add(ib_k,ida_nbr_found(ib_k)) = ib_j
                rda_src_lons(ib_k,ida_nbr_found(ib_k))= &
                   grid1_center_lon(ib_j)+pi2
            END IF
          END DO
          
      ELSE IF (rd_plon > grid1_center_lon(il_max-1)) THEN
          DO ib_j=il_max-2, il_max
            IF (grid1_mask(ib_j) .or. ld_extrapdone) THEN 
                ida_nbr_found(ib_k)=ida_nbr_found(ib_k)+1
                ida_src_add(ib_k,ida_nbr_found(ib_k)) = ib_j
                rda_src_lons(ib_k,ida_nbr_found(ib_k))= &
                   grid1_center_lon(ib_j)
            END IF
          END DO
          IF (grid1_mask(il_min) .or. ld_extrapdone) THEN 
              ida_nbr_found(ib_k)=ida_nbr_found(ib_k)+1
              ida_src_add(ib_k,ida_nbr_found(ib_k)) = il_min
              rda_src_lons(ib_k,ida_nbr_found(ib_k))= &
                 grid1_center_lon(il_min)+pi2
          END IF
          
      ELSE           
          
          DO WHILE (il_min/=il_max-1)
            il_inter=(il_max-il_min)/2 + il_min
            IF (rd_plon >= grid1_center_lon(il_min) .and. &
               rd_plon < grid1_center_lon(il_inter)) THEN
                il_max=il_inter
            ELSE
                il_min=il_inter
            END IF
          END DO
          DO ib_i= il_min-1, il_min+2
            IF (grid1_mask(ib_i) .or. ld_extrapdone) THEN 
                ida_nbr_found(ib_k)=ida_nbr_found(ib_k)+1
                ida_src_add(ib_k,ida_nbr_found(ib_k))=ib_i
                rda_src_lons(ib_k,ida_nbr_found(ib_k))= &
                   grid1_center_lon(ib_i)
            END IF
          END DO                    

      END IF
        
    END DO

    
  END SUBROUTINE grid_search_16_points
  


!-----------------------------------------------------------------------
! BOP
!
! !IROUTINE:  calcul_wght_irreg
!
! !INTERFACE:
! 
  SUBROUTINE calcul_wght_irreg(rda_x, rd_pt, rda_wght)
!  
! !USES:
!                       
! !RETURN VALUE:
!  
    REAL (KIND=dbl_kind), DIMENSION(4), INTENT(out) :: &
       rda_wght   ! array of weights for the points x
!      
! !PARAMETERS:
! 
    REAL (KIND=dbl_kind), DIMENSION(4), INTENT(in) :: &
       rda_x ! array of positions on source grid, lat or lon
      
    REAL (KIND=dbl_kind),INTENT(in) :: &
       rd_pt  ! position of target point to interpolate
       
    REAL (KIND=dbl_kind) :: &
       rl_t1, rl_t2, rl_t3, rl_t4, rl_t5, rl_t6, rl_t7, rl_t8, rl_t9, &
       rl_u1, rl_u2, rl_u3, rl_u4, &
       rl_k1, rl_k2, rl_k3, &
       rl_d1, rl_d2, rl_d3, rl_d4, &
       rl_c1, rl_c2, rl_c3, rl_c4, &
       rl_b1, rl_b2, rl_b3, rl_b4, &
       rl_a1, rl_a2, rl_a3, rl_a4, &
       rl_y1, rl_y2, rl_y3, &
       rl_a1_y, rl_a2_y, rl_a3_y, rl_a4_y, &
       rl_b1_y, rl_b2_y, rl_b3_y, rl_b4_y, &
       rl_c1_y, rl_c2_y, rl_c3_y, rl_c4_y
!                       
! !DESCRIPTION:
!  Calculates a the weights of four points for a bicubic interpolation. 
!  The distances between the points can be irregulier. 
!
! !REVISION HISTORY:
!  2002.10.21  J.Ghattas  created
!
! EOP
!-----------------------------------------------------------------------
! $Id: remap_bicubic_reduced.F90 2826 2010-12-10 11:14:21Z valcke $
! $Author: valcke $
!-----------------------------------------------------------------------    
 
    
    IF (rda_x(1)/=0.and. rda_x(2)/=0 .and. rda_x(3)/=0 .and. rda_x(4)/=0) THEN
          
        rl_t1 = 1/rda_x(1) - 1/rda_x(2)
        rl_t2 = 1/rda_x(1)**2 - 1/rda_x(2)**2
        rl_t3 = 1/rda_x(1)**3 - 1/rda_x(2)**3
        rl_t4 = 1/rda_x(1) - 1/rda_x(3)
        rl_t5 = 1/rda_x(1)**2 - 1/rda_x(3)**2
        rl_t6 = 1/rda_x(1)**3 - 1/rda_x(3)**3
        rl_t7 = 1/rda_x(1) - 1/rda_x(4)
        rl_t8 = 1/rda_x(1)**2 - 1/rda_x(4)**2
        rl_t9 = 1/rda_x(1)**3 - 1/rda_x(4)**3
          
        rl_u1 = rl_t2/rl_t1 - rl_t5/rl_t4
        rl_u2 = rl_t3/rl_t1 - rl_t6/rl_t4
        rl_u3 = rl_t2/rl_t1 - rl_t8/rl_t7
        rl_u4 = rl_t3/rl_t1 - rl_t9/rl_t7
        
        rl_k1 = (1/(rl_t1*rl_u1)-1/(rl_t1*rl_u3)) / (rl_u2/rl_u1-rl_u4/rl_u3)
        rl_k2 = -1/(rl_t4*rl_u1) / (rl_u2/rl_u1-rl_u4/rl_u3)
        rl_k3 = 1/(rl_t7*rl_u3) / (rl_u2/rl_u1-rl_u4/rl_u3)
        
        
        rl_d1=(rl_k1+rl_k2+rl_k3)/rda_x(1)**3
        rl_d2 = -rl_k1/rda_x(2)**3
        rl_d3 = -rl_k2/rda_x(3)**3
        rl_d4 = -rl_k3/rda_x(4)**3
        
        rl_c1 = 1/rl_u1*(1/(rl_t1*rda_x(1)**3)-1/(rl_t4*rda_x(1)**3)- &
           rl_u2*rl_d1)
        rl_c2 = 1/rl_u1*(1/(-rl_t1*rda_x(2)**3)-rl_u2*rl_d2)
        rl_c3 = 1/rl_u1*(1/(rl_t4*rda_x(3)**3)-rl_u2*rl_d3)
        rl_c4 = 1/rl_u1*(-rl_u2*rl_d4)
        
        rl_b1 = 1/rl_t1/rda_x(1)**3-rl_t2/rl_t1*rl_c1-rl_t3/rl_t1*rl_d1
        rl_b2 = -1/rl_t1/rda_x(2)**3-rl_t2/rl_t1*rl_c2-rl_t3/rl_t1*rl_d2
        rl_b3 = -rl_t2/rl_t1*rl_c3-rl_t3/rl_t1*rl_d3
        rl_b4 = -rl_t2/rl_t1*rl_c4-rl_t3/rl_t1*rl_d4
        
        rl_a1 = 1/rda_x(1)**3-1/rda_x(1)*rl_b1-1/rda_x(1)**2*rl_c1- &
           1/rda_x(1)**3*rl_d1
        rl_a2 = -1/rda_x(1)*rl_b2-1/rda_x(1)**2*rl_c2-1/rda_x(1)**3*rl_d2
        rl_a3 = -1/rda_x(1)*rl_b3-1/rda_x(1)**2*rl_c3-1/rda_x(1)**3*rl_d3
        rl_a4 = -1/rda_x(1)*rl_b4-1/rda_x(1)**2*rl_c4-1/rda_x(1)**3*rl_d4
        
       ! Weights  
        rda_wght(1) = rl_a1*rd_pt**3 + rl_b1*rd_pt**2 + rl_c1*rd_pt + rl_d1
        rda_wght(2) = rl_a2*rd_pt**3 + rl_b2*rd_pt**2 + rl_c2*rd_pt + rl_d2
        rda_wght(3) = rl_a3*rd_pt**3 + rl_b3*rd_pt**2 + rl_c3*rd_pt + rl_d3
        rda_wght(4) = rl_a4*rd_pt**3 + rl_b4*rd_pt**2 + rl_c4*rd_pt + rl_d4
        
    ELSE ! there is one point = 0
          
        rl_d1=0; rl_d2=0; rl_d3=0; rl_d4=0
        
        ! Transformation for each case
        IF (rda_x(1)==0) THEN
            rl_y1=rda_x(2); rl_y2=rda_x(3); rl_y3=rda_x(4)
            rl_d1=1
        ELSE IF (rda_x(2)==0) THEN
            rl_y1=rda_x(1); rl_y2=rda_x(3); rl_y3=rda_x(4)
            rl_d2=1
        ELSE IF (rda_x(3)==0) THEN
            rl_y1=rda_x(1); rl_y2=rda_x(2); rl_y3=rda_x(4)
            rl_d3=1
        ELSE 
            rl_y1=rda_x(1); rl_y2=rda_x(2); rl_y3=rda_x(3)
            rl_d4=1
        END IF
        
        ! Solving the system 
        rl_t1 = 1/rl_y1-1/rl_y2
        rl_t2 = 1/rl_y1**2-1/rl_y2**2
        rl_t3 = 1/rl_y1-1/rl_y3
        rl_t4 = 1/rl_y1**2-1/rl_y3**2
        
        rl_c1_y =(1/rl_y1**3/rl_t1-1/rl_y1**3/rl_t3)/(rl_t2/rl_t1-rl_t4/rl_t3)
        rl_c2_y = -1/rl_y2**3/rl_t1/(rl_t2/rl_t1-rl_t4/rl_t3)
        rl_c3_y = 1/rl_y3**3/rl_t3/(rl_t2/rl_t1-rl_t4/rl_t3)
        rl_c4_y=(-1/rl_y1**3/rl_t1+1/rl_y2**3/rl_t1+ &
           1/rl_y1**3/rl_t3-1/rl_y3**3/rl_t3)/(rl_t2/rl_t1-rl_t4/rl_t3)
        
        rl_b1_y = 1/rl_y1**3/rl_t1 - rl_c1_y*rl_t2/rl_t1
        rl_b2_y = -1/rl_y2**3/rl_t1 - rl_c2_y*rl_t2/rl_t1
        rl_b3_y = -rl_c3_y*rl_t2/rl_t1
        rl_b4_y = -1/rl_y1**3/rl_t1 + 1/rl_y2**3/rl_t1 - rl_c4_y*rl_t2/rl_t1
        
        rl_a1_y = 1/rl_y1**3 - rl_b1_y/rl_y1 - rl_c1_y/rl_y1**2
        rl_a2_y = -rl_b2_y/rl_y1 - rl_c2_y/rl_y1**2
        rl_a3_y = -rl_b3_y/rl_y1 - rl_c3_y/rl_y1**2
        rl_a4_y = -1/rl_y1**3 - rl_b4_y/rl_y1 - rl_c4_y/rl_y1**2
          
        ! Retransformation
        IF (rda_x(1)==0) THEN
            rl_a1=rl_a4_y; rl_a2=rl_a1_y; rl_a3=rl_a2_y; rl_a4=rl_a3_y
            rl_b1=rl_b4_y; rl_b2=rl_b1_y; rl_b3=rl_b2_y; rl_b4=rl_b3_y
            rl_c1=rl_c4_y; rl_c2=rl_c1_y; rl_c3=rl_c2_y; rl_c4=rl_c3_y
        ELSE IF (rda_x(2)==0) THEN
            rl_a1=rl_a1_y; rl_a2=rl_a4_y; rl_a3=rl_a2_y; rl_a4=rl_a3_y
            rl_b1=rl_b1_y; rl_b2=rl_b4_y; rl_b3=rl_b2_y; rl_b4=rl_b3_y
            rl_c1=rl_c1_y; rl_c2=rl_c4_y; rl_c3=rl_c2_y; rl_c4=rl_c3_y
        ELSE IF (rda_x(3)==0) THEN
            rl_a1=rl_a1_y; rl_a2=rl_a2_y; rl_a3=rl_a4_y; rl_a4=rl_a3_y
            rl_b1=rl_b1_y; rl_b2=rl_b2_y; rl_b3=rl_b4_y; rl_b4=rl_b3_y
            rl_c1=rl_c1_y; rl_c2=rl_c2_y; rl_c3=rl_c4_y; rl_c4=rl_c3_y
        ELSE 
            rl_a1=rl_a1_y; rl_a2=rl_a2_y; rl_a3=rl_a3_y; rl_a4=rl_a4_y
            rl_b1=rl_b1_y; rl_b2=rl_b2_y; rl_b3=rl_b3_y; rl_b4=rl_b4_y
            rl_c1=rl_c1_y; rl_c2=rl_c2_y; rl_c3=rl_c3_y; rl_c4=rl_c4_y
        END IF
        
        ! Weights  
        rda_wght(1) = rl_a1*rd_pt**3 + rl_b1*rd_pt**2 + rl_c1*rd_pt +rl_d1
        rda_wght(2) = rl_a2*rd_pt**3 + rl_b2*rd_pt**2 + rl_c2*rd_pt +rl_d2
        rda_wght(3) = rl_a3*rd_pt**3 + rl_b3*rd_pt**2 + rl_c3*rd_pt +rl_d3
        rda_wght(4) = rl_a4*rd_pt**3 + rl_b4*rd_pt**2 + rl_c4*rd_pt +rl_d4
        
    END IF
      
      
  END SUBROUTINE calcul_wght_irreg
  
!-----------------------------------------------------------------------
! BOP
!
! !IROUTINE:  calcul_wght_3
!
! !INTERFACE:
  
  SUBROUTINE calcul_wght_3(rda_x, rd_pt, rda_wght)

! !USES:
  
! !RETURN VALUE:
 
    REAL (KIND=dbl_kind), DIMENSION(3), INTENT(out) :: &
       rda_wght         ! array of weights for the points x
    
! !PARAMETERS:

    REAL (KIND=dbl_kind), DIMENSION(3), INTENT(in) :: &
       rda_x         ! array of positions on source grid, lat or lon
    
    REAL (KIND=dbl_kind), INTENT(in) :: &
       rd_pt        ! position of target point to interpolate
    
    REAL (KIND=dbl_kind) :: &
       rl_c1, rl_c2, rl_c3, &
       rl_a1, rl_a2, rl_a3, &
       rl_b1, rl_b2, rl_b3, &
       rl_t1, rl_t2, rl_t3, rl_t4
      
! !DESCRIPTION:
!  Calculates a the weights of 3 points for a parabolic interpolation.
! 
! !REVISION HISTORY:
!  2002.10.21  J.Ghattas  created
!
! EOP
!-----------------------------------------------------------------------
! $Id: remap_bicubic_reduced.F90 2826 2010-12-10 11:14:21Z valcke $
! $Author: valcke $
!-----------------------------------------------------------------------
    
    
    IF (rda_x(1)/=0 .and. rda_x(2)/=0 .and. rda_x(3)/=0) THEN    
        rl_t1 = 1/rda_x(1)-1/rda_x(2)
        rl_t2 = 1/rda_x(1)**2-1/rda_x(2)**2
        rl_t3 = 1/rda_x(1)-1/rda_x(3)
        rl_t4 = 1/rda_x(1)**2-1/rda_x(3)**2
        
        rl_c1 = (1/rda_x(1)**2/rl_t1-1/rda_x(1)**2/rl_t3) / &
           (rl_t2/rl_t1-rl_t4/rl_t3)
        rl_c2 = -1/rda_x(2)**2/rl_t1 / (rl_t2/rl_t1-rl_t4/rl_t3)
        rl_c3 = 1/rda_x(3)**2/rl_t3 / (rl_t2/rl_t1-rl_t4/rl_t3)
        
        rl_b1 = 1/rda_x(1)**2/rl_t1 - rl_c1*rl_t2/rl_t1
        rl_b2 = -1/rda_x(2)**2/rl_t1 - rl_c2*rl_t2/rl_t1
        rl_b3 = - rl_c3*rl_t2/rl_t1
        
        rl_a1 = 1/rda_x(1)**2 - rl_b1/rda_x(1) - rl_c1/rda_x(1)**2
        rl_a2 = - rl_b2/rda_x(1) - rl_c2/rda_x(1)**2
        rl_a3 = - rl_b3/rda_x(1) - rl_c3/rda_x(1)**2
        
        
    ELSE IF (rda_x(1)==0) THEN
        rl_c1 = 1; rl_c2 = 0; rl_c3 = 0
        rl_b1 = (-1/rda_x(2)**2+1/rda_x(3)**2) / (1/rda_x(2)-1/rda_x(3))
        rl_b2 = 1/rda_x(2)**2 / (1/rda_x(2)-1/rda_x(3))
        rl_b3 = -1/rda_x(3)**2 / (1/rda_x(2)-1/rda_x(3))
        
        rl_a1 = -1/rda_x(2)**2 - rl_b1/rda_x(2)
        rl_a2 = 1/rda_x(2)**2 - rl_b2/rda_x(2)
        rl_a3 = - rl_b3/rda_x(2)
        
    ELSE IF (rda_x(2)==0) THEN
        
        rl_c1 = 0; rl_c2 = 1; rl_c3 = 0
        rl_b1 = 1/rda_x(1)**2 / (1/rda_x(1)-1/rda_x(3))
        rl_b2 = (-1/rda_x(1)**2+1/rda_x(3)**2) / (1/rda_x(1)-1/rda_x(3))
        rl_b3 = -1/rda_x(3)**2 / (1/rda_x(1)-1/rda_x(3))
        
        rl_a1 = 1/rda_x(1)**2 - rl_b1/rda_x(1)
        rl_a2 = -1/rda_x(1)**2 - rl_b2/rda_x(1)
        rl_a3 = - rl_b3/rda_x(1)
        
    ELSE !rda_x(3)==0
        rl_c1 = 0; rl_c2 = 0; rl_c3 = 1
        rl_b1 = 1/rda_x(1)**2 / (1/rda_x(1)-1/rda_x(2))
        rl_b2 = -1/rda_x(2)**2 / (1/rda_x(1)-1/rda_x(2))
        rl_b3 = (-1/rda_x(1)**2+1/rda_x(2)**2) / (1/rda_x(1)-1/rda_x(2))
        
        rl_a1 = 1/rda_x(1)**2 - rl_b1/rda_x(1)
        rl_a2 = - rl_b2/rda_x(1)
        rl_a3 = -1/rda_x(1)**2 - rl_b3/rda_x(1)
        
        
    END IF
   
    ! Weights  
    rda_wght(1) = rl_a1*rd_pt**2 + rl_b1*rd_pt + rl_c1
    rda_wght(2) = rl_a2*rd_pt**2 + rl_b2*rd_pt + rl_c2
    rda_wght(3) = rl_a3*rd_pt**2 + rl_b3*rd_pt + rl_c3
    
    
  END SUBROUTINE calcul_wght_3
    

!-----------------------------------------------------------------------
! BOP
!
! !IROUTINE:  calcul_wght_2
!
! !INTERFACE:

  SUBROUTINE calcul_wght_2(rda_x, rd_pt, rda_wght)
                        
! !USES:
                       
! !RETURN VALUE:
  
    REAL (KIND=dbl_kind), DIMENSION(2), INTENT(out) :: &
       rda_wght      ! array of weights for the points x
     
! !PARAMETERS:

    REAL (KIND=dbl_kind), DIMENSION(2), INTENT(in) :: &
       rda_x      ! array of positions on source grid, lat or lon
      
    REAL (KIND=dbl_kind), INTENT(in) :: &
       rd_pt     ! position of target point to interpolate
    
    REAL (KIND=dbl_kind) :: rl_b1, rl_b2, rl_a1, rl_a2
                           
! !DESCRIPTION:
!  Calculates a the weights of 2 points for a linair interpolation.
!
! !REVISION HISTORY:
!  2002.10.21   J.Ghattas    created
!
! EOP
!-----------------------------------------------------------------------
! $Id: remap_bicubic_reduced.F90 2826 2010-12-10 11:14:21Z valcke $
! $Author: valcke $
!-----------------------------------------------------------------------  
    
     
    IF (rda_x(1)/=0 .and. rda_x(2)/=0) THEN
        rl_b1 = 1/(1-rda_x(1)/rda_x(2))
        rl_b2 = -1/(rda_x(2)/rda_x(1)-1)
        rl_a1 = 1/rda_x(1) - rl_b1/rda_x(1)
        rl_a2 = - rl_b2/rda_x(1)
        
    ELSE IF (rda_x(1)==0) THEN
        rl_b1=1; rl_b2=0
        rl_a1=-1/rda_x(2)
        rl_a2=1/rda_x(2)
    ELSE
        rl_b1=0; rl_b2=1
        rl_a1=1/rda_x(1)
        rl_a2=-1/rda_x(1)
    END IF
      
    rda_wght(1) = rl_a1*rd_pt + rl_b1 
    rda_wght(2) = rl_a2*rd_pt + rl_b2
    
  END SUBROUTINE calcul_wght_2
    

!-----------------------------------------------------------------------
! BOP
!
! !IROUTINE:  store_link_bicub
!
! !INTERFACE:
 
  SUBROUTINE store_link_bicub(id_dst_add, ida_src_add, rda_wght, id_thread)
    
! !USES:
  
! !RETURN VALUE:

! !PARAMETERS:

    INTEGER (KIND=int_kind), INTENT(in) :: &
       id_dst_add    ! address on destination grid
    
    INTEGER (KIND=int_kind), DIMENSION(4,4), INTENT(in) :: &
       ida_src_add   ! addresses for links on source grid
    
    REAL (KIND=dbl_kind), DIMENSION(4,4), INTENT(in) :: &
       rda_wght      ! array of remapping weights for these links
      
    INTEGER (KIND=int_kind), INTENT(in) :: &
       id_thread     ! local threaded task
    
    INTEGER (KIND=int_kind) :: ib_i, ib_j, ib_ind, &
       il_num_links_old  ! placeholder for old link number
    
!EM    INTEGER (KIND=int_kind), DIMENSION(num_neighbors) :: &
!EM       ila_src_add   ! reshaped addresses
    
!EM    REAL (KIND=dbl_kind), DIMENSION(num_neighbors) :: &
!EM       rla_wght      ! reshaped weights
    
! !DESCRIPTION:
!  This routine stores the addresses and weights for 16 links associated 
!  with one destination point in the appropriate address.  
!
! !REVISION HISTORY:
!  2002.10.21    J.Ghattas   created
!
! EOP
!-----------------------------------------------------------------------
! $Id: remap_bicubic_reduced.F90 2826 2010-12-10 11:14:21Z valcke $
! $Author: valcke $
!-----------------------------------------------------------------------    
    
   
    !
    ! Increment number of links and check if remap arrays need
    ! to be increased to accomodate the new link.  then store the link.
    !

    if (il_nbthreads .eq. 1) then

     
      il_num_links_old  = num_links_map1
      num_links_map1 = il_num_links_old + num_neighbors
    
      IF (num_links_map1 > max_links_map1) THEN
        CALL resize_remap_vars(1,MAX(resize_increment,num_neighbors))
      END IF

!EM      ila_src_add=RESHAPE(ida_src_add,(/num_neighbors/))
!EM      rla_wght=RESHAPE(rda_wght,(/num_neighbors/))
    
      ib_ind = 0
      do ib_j=1,4
        do ib_i=1,4
          ib_ind = ib_ind + 1
          grid1_add_map1(il_num_links_old+ib_ind) = ida_src_add(ib_i,ib_j)
          grid2_add_map1(il_num_links_old+ib_ind) = id_dst_add
          wts_map1(1,il_num_links_old+ib_ind) = rda_wght(ib_i,ib_j)
        end do
      end do

    else

      sga_remap(id_thread)%num_links = sga_remap(id_thread)%num_links + num_neighbors

      if (sga_remap(id_thread)%num_links > sga_remap(id_thread)%max_links) &
            call sga_remap(id_thread)%resize(int(0.2*real(sga_remap(id_thread)%max_links)))

      ib_ind = 0
      do ib_j=1,4
        do ib_i=1,4
          ib_ind = ib_ind + 1
          sga_remap(id_thread)%grid1_add(sga_remap(id_thread)%num_links-num_neighbors+ib_ind) = ida_src_add(ib_i,ib_j)
          sga_remap(id_thread)%grid2_add(sga_remap(id_thread)%num_links-num_neighbors+ib_ind) = id_dst_add
          sga_remap(id_thread)%wts(1,sga_remap(id_thread)%num_links-num_neighbors+ib_ind) = rda_wght(ib_i,ib_j)
        end do
      end do

    endif

        
  END SUBROUTINE store_link_bicub
    
    
END MODULE remap_bicubic_reduced
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
