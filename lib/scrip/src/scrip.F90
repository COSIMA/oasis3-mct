!****
!               *****************************
!               * OASIS ROUTINE  -  LEVEL 1 *
!               * -------------     ------- *
!               *****************************
!****
!***********************************************************************
!     This routine belongs to the SCRIP library. It is modified to run
!     within OASIS.
!     Modifications:
!       - routine does not read namelist but gets parameters from the
!         calling routine scriprmp.f
!       - map-method and noralize-option are written in capital letters
!       - routine grid_init is not called from scrip any more but was
!         called earlier from scriprmp
!       - call of two extra routines: free_grids and free_remap_vars to 
!         allow multiple calls of SCRIP
!       - added case for GAUSWGT 
!       - added 'REDUCED' case for bilinear and bicubic.
!       - hard coded num_maps=1 for USE in OASIS
!       - added lextrapdone argument
!
!     Modified by            V. Gayler,  M&D                  20.09.2001
!     Modified by            D. Declat,  CERFACS              27.06.2002
!     Modified by            S. Valcke,  CERFACS              27.08.2002
!***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This routine is the driver for computing the addresses and weights 
!     for interpolating between two grids on a sphere.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: scrip.f 1831 2009-01-09 17:19:08Z valcke $
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

      subroutine scrip (interp_file1, map1_name, m_method, n_opt, &
                        lextrapdone, rl_varmul, id_scripvoi, cons_order, &
                        mpi_comm_map, mpi_size_map, mpi_rank_map, mpi_root_map)

!-----------------------------------------------------------------------

      use kinds_mod                  ! module defining data types
      use constants                  ! module for common constants
      use iounits                    ! I/O unit manager
      use timers                     ! CPU timers
      use grids                      ! module with grid information
      use remap_vars                 ! common remapping variables
      use remap_conservative         ! routines for conservative remap
      use remap_distance_gaussian_weight  ! routines for dist-weight and gaussian remap
      use remap_bi_interp            ! routines for bicubic  interp
      use remap_bicubic_reduced      ! routines for bicubic interp
      use remap_write                ! routines for remap output
      use fracnnei_mod

      USE mod_oasis_flush

      implicit none

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      character (char_len), intent(in) :: interp_file1, & ! filename for output remap data (map1)
                                          map1_name       ! name for mapping from grid1 to grid2

      character*8, intent(in) ::          m_method, &     ! choice for mapping method
                                          n_opt,    &     ! option for normalizing weights
                                          cons_order      ! conservation order, FIRST or SECOND

      LOGICAL ::            lextrapdone   ! logical, true if EXTRAP done on field

      REAL (kind=dbl_kind) ::     rl_varmul             ! Gaussian variance (for GAUSWGT)

      INTEGER (kind=int_kind) ::   id_scripvoi          ! number of neighbours for DISTWGT and GAUSWGT

      integer (kind=int_kind) :: mpi_comm_map, mpi_rank_map, mpi_size_map, mpi_root_map

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n             ! dummy counter

      integer (kind=int_kind) :: ib            ! dummy counter

      character (char_len) :: interp_file2, &  ! filename for output remap data (map2)
                              map2_name,    &  ! name for mapping from grid2 to grid1
                              output_opt,   &  ! option for output conventions
                              map_method,   &  ! choice for mapping method
                              normalize_opt    ! option for normalizing weights

!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine scrip'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     initialize timers
!
!-----------------------------------------------------------------------

      call timers_init
      do n=1,max_timers
        call timer_clear(n)
      end do

!-----------------------------------------------------------------------
!
!     initialize variables of former SCRIP namelist
!
!-----------------------------------------------------------------------

      interp_file2  = 'unknown'
      map2_name     = 'unknown'
      luse_grid1_area = .false.
      luse_grid2_area = .false.
      num_maps      = 1
      output_opt    = 'scrip'

      map_method = m_method
      normalize_opt = n_opt

      select case(map_method)
      case ('CONSERV')
        map_type = map_type_conserv
        if (cons_order == 'FIRST') then
           conserve_opt = 1
        elseif (cons_order == 'SECOND') then
           conserve_opt = 2
        else
           stop 'unknown conserve_order '
        endif
      case ('BILINEAR')
        map_type = map_type_bilinear
      case ('BICUBIC')
        map_type = map_type_bicubic
      case ('DISTWGT')
        map_type = map_type_distwgt
      case ('GAUSWGT')
        map_type = map_type_gauswgt
      case default
        stop 'unknown mapping method'
      end select
      
      SELECT CASE (normalize_opt)
      CASE ('FRACNNEI')
        lfracnnei = .true.
      END SELECT
         
      select case(normalize_opt(1:4))
      case ('NONE')
        norm_opt = norm_opt_none
      case ('FRAC')
        norm_opt = norm_opt_frcarea
      case ('DEST')
        norm_opt = norm_opt_dstarea
      CASE ('NONO')
        norm_opt = norm_opt_nonorm
      case default
        stop 'unknown normalization option'
      end select
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' Computing remappings between: ' ,grid1_name
         WRITE (UNIT = nulou,FMT = *)'                          and  ' ,grid2_name
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     initialize some remapping variables.
!
!-----------------------------------------------------------------------
       call init_remap_vars (id_scripvoi)
!-----------------------------------------------------------------------
!
!     call appropriate interpolation setup routine based on type of
!     remapping requested.
!
!-----------------------------------------------------------------------
      select case(map_type)
      case(map_type_conserv)
          CALL timer_start(1,'remap_conserv overall')
          CALL remap_conserv(mpi_comm_map, mpi_size_map, mpi_rank_map, mpi_root_map)
!       CALL remap_conserv()
          DO ib = 2, 4
            CALL timer_print(ib)
            CALL timer_clear(ib)
          END DO
      case(map_type_bilinear)
          CALL timer_start(1,'remap_bi overall')
          CALL remap_bi(lextrapdone, &
                        mpi_comm_map, mpi_size_map, mpi_rank_map, mpi_root_map)
      case(map_type_distwgt)
          CALL timer_start(1,'remap_dist_gaus_wgt overall')
          CALL remap_dist_gaus_wgt (lextrapdone, id_scripvoi, &
                                    mpi_comm_map, mpi_size_map, mpi_rank_map, mpi_root_map)
      case(map_type_gauswgt)
          CALL timer_start(1,'remap_dist_gaus_wgt overall')
          CALL remap_dist_gaus_wgt (lextrapdone, id_scripvoi, &
                                    mpi_comm_map, mpi_size_map, mpi_rank_map, mpi_root_map, &
                                    rl_varmul)
      case(map_type_bicubic)
          IF (restrict_TYPE == 'REDUCED') then
              CALL timer_start(1,'remap_bicubic_reduced overall')
              CALL remap_bicub_reduced(lextrapdone, &
                                       mpi_comm_map, mpi_size_map, mpi_rank_map, mpi_root_map)
          ELSE
              CALL timer_start(1,'remap_bi overall')
              CALL remap_bi(lextrapdone, &
                            mpi_comm_map, mpi_size_map, mpi_rank_map, mpi_root_map)
          ENDIF
       case default
           stop 'Invalid Map Type'
      end select

      CALL timer_stop(1)
      CALL timer_print(1)
      CALL timer_clear(1)
      
      IF (mpi_rank_map == mpi_root_map) THEN

      CALL timer_start(1,'scrip sort_add')
      CALL sort_add(grid2_add_map1, grid1_add_map1, wts_map1, num_links_map1, num_wts)
      CALL timer_stop(1)

      IF (map_type == map_type_conserv) THEN
         
         CALL timer_start(2,'scrip conserv uniq_add')
         CALL uniq_add(grid2_add_map1, grid1_add_map1, wts_map1, num_links_map1, num_wts)
         IF (num_links_map1 /= max_links_map1) THEN
            CALL resize_remap_vars(1, num_links_map1-max_links_map1)
         ENDIF
         CALL timer_stop(2)
         IF (lfracnnei) THEN
            CALL timer_start(3,'scrip conserv fracnnei')
            CALL fracnnei()
            lfracnnei = .FALSE.
            CALL timer_stop(3)
         END IF
      END IF
!
#ifdef TREAT_OVERLAY
!
! Change address if overlap point were found
      IF (map_type == 1) THEN
          DO n = 1, num_links_map1
            IF (grid1_add_map1(n) .ne. 0) THEN
                grid1_add_map1(n) = grid1_add_repl1(grid1_add_map1(n))
            ENDIF
          END DO
      ENDIF
!
#endif
!
      DO n = 1, num_links_map1
        IF (.not. grid2_mask(grid2_add_map1(n)))  wts_map1(:,n)=0.
      enddo
!
!-----------------------------------------------------------------------
!
!     reduce size of remapping arrays and then write remapping info
!     to a file.
!
!-----------------------------------------------------------------------
      if (num_links_map1 /= max_links_map1) then
        call resize_remap_vars(1, num_links_map1-max_links_map1)
      endif
      if ((num_maps > 1) .and. (num_links_map2 /= max_links_map2)) then
        call resize_remap_vars(2, num_links_map2-max_links_map2)
      endif

      call write_remap(map1_name, map2_name, interp_file1, interp_file2, output_opt)

      CALL timer_print(1)
      CALL timer_clear(1)
      IF (map_type == map_type_conserv) THEN
         call timer_print(2)
         call timer_clear(2)
         CALL timer_print(3)
         CALL timer_clear(3)
      END IF

      END IF ! IF (mpi_rank_map == mpi_root_map) THEN

!-----------------------------------------------------------------------
!
!     deallocate allocatable arrays
!
!-----------------------------------------------------------------------

      call free_grids
      call free_remap_vars
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine scrip'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!-----------------------------------------------------------------------!
      end subroutine scrip
!
      subroutine sort_add(add1, add2, weights, num_links, num_wts)

!-----------------------------------------------------------------------
!
!     this routine sorts address and weight arrays based on the
!     destination address with the source address as a secondary
!     sorting criterion.  the method is a standard heap sort.
!
!-----------------------------------------------------------------------

      use kinds_mod     ! defines common data types
      use constants     ! defines common scalar constants
      USE mod_oasis_flush

      implicit none

!-----------------------------------------------------------------------
!
!     Input and Output arrays
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) :: num_links, num_wts
      integer (kind=int_kind), intent(inout), dimension(num_links) :: &
                                                  add1,   &    ! destination address array (num_links)
                                                  add2         ! source      address array

      real (kind=dbl_kind), intent(inout), dimension(num_wts, num_links) :: &
                                                  weights     ! remapping weights (num_wts, num_links)


!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: add1_tmp, add2_tmp, &   ! temp for addresses during swap
                                 nwgt, lvl, final_lvl, & ! level indexes for heap sort levels
                                 chk_lvl1, chk_lvl2, max_lvl

      real (kind=dbl_kind), dimension(SIZE(weights,DIM=1)) :: wgttmp  ! temp for holding wts during swap
!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine sort_add'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     determine total number of links to sort and number of weights
!
!-----------------------------------------------------------------------

!      num_links = SIZE(add1)
!      num_wts   = SIZE(weights, DIM=1)

!-----------------------------------------------------------------------
!
!     start at the lowest level (N/2) of the tree and sift lower 
!     values to the bottom of the tree, promoting the larger numbers
!
!-----------------------------------------------------------------------

      do lvl=num_links/2,1,-1

        final_lvl = lvl
        add1_tmp = add1(lvl)
        add2_tmp = add2(lvl)
        wgttmp(:) = weights(:,lvl)

        !***
        !*** loop until proper level is found for this link, or reach
        !*** bottom
        !***

        sift_loop1: do

          !***
          !*** find the largest of the two daughters
          !***

          chk_lvl1 = 2*final_lvl
          chk_lvl2 = 2*final_lvl+1
          if (chk_lvl1 .EQ. num_links) chk_lvl2 = chk_lvl1

          if ((add1(chk_lvl1) >  add1(chk_lvl2)) .OR. &
              ((add1(chk_lvl1) == add1(chk_lvl2)) .AND. (add2(chk_lvl1) >  add2(chk_lvl2)))) then
            max_lvl = chk_lvl1
          else 
            max_lvl = chk_lvl2
          endif

          !***
          !*** if the parent is greater than both daughters,
          !*** the correct level has been found
          !***

          if ((add1_tmp .GT. add1(max_lvl)) .OR. &
              ((add1_tmp .EQ. add1(max_lvl)) .AND. (add2_tmp .GT. add2(max_lvl)))) then
            add1(final_lvl) = add1_tmp
            add2(final_lvl) = add2_tmp
            weights(:,final_lvl) = wgttmp(:)
            exit sift_loop1

          !***
          !*** otherwise, promote the largest daughter and push
          !*** down one level in the tree.  if haven't reached
          !*** the end of the tree, repeat the process.  otherwise
          !*** store last values and exit the loop
          !***

          else 
            add1(final_lvl) = add1(max_lvl)
            add2(final_lvl) = add2(max_lvl)
            weights(:,final_lvl) = weights(:,max_lvl)

            final_lvl = max_lvl
            if (2*final_lvl > num_links) then
              add1(final_lvl) = add1_tmp
              add2(final_lvl) = add2_tmp
              weights(:,final_lvl) = wgttmp(:)
              exit sift_loop1
            endif
          endif
        end do sift_loop1
      end do

!-----------------------------------------------------------------------
!
!     now that the heap has been sorted, strip off the top (largest)
!     value and promote the values below
!
!-----------------------------------------------------------------------

      do lvl=num_links,3,-1

        !***
        !*** move the top value and insert it into the correct place
        !***

        add1_tmp = add1(lvl)
        add1(lvl) = add1(1)

        add2_tmp = add2(lvl)
        add2(lvl) = add2(1)

        wgttmp(:) = weights(:,lvl)
        weights(:,lvl) = weights(:,1)

        !***
        !*** as above this loop sifts the tmp values down until proper 
        !*** level is reached
        !***

        final_lvl = 1

        sift_loop2: do

          !***
          !*** find the largest of the two daughters
          !***

          chk_lvl1 = 2*final_lvl
          chk_lvl2 = 2*final_lvl+1
          if (chk_lvl2 >= lvl) chk_lvl2 = chk_lvl1

          if ((add1(chk_lvl1) >  add1(chk_lvl2)) .OR. &
              ((add1(chk_lvl1) == add1(chk_lvl2)) .AND. (add2(chk_lvl1) >  add2(chk_lvl2)))) then
            max_lvl = chk_lvl1
          else 
            max_lvl = chk_lvl2
          endif

          !***
          !*** if the parent is greater than both daughters,
          !*** the correct level has been found
          !***

          if ((add1_tmp >  add1(max_lvl)) .OR. & 
              ((add1_tmp == add1(max_lvl)) .AND. (add2_tmp >  add2(max_lvl)))) then
            add1(final_lvl) = add1_tmp
            add2(final_lvl) = add2_tmp
            weights(:,final_lvl) = wgttmp(:)
            exit sift_loop2

          !***
          !*** otherwise, promote the largest daughter and push
          !*** down one level in the tree.  if haven't reached
          !*** the end of the tree, repeat the process.  otherwise
          !*** store last values and exit the loop
          !***

          else 
            add1(final_lvl) = add1(max_lvl)
            add2(final_lvl) = add2(max_lvl)
            weights(:,final_lvl) = weights(:,max_lvl)

            final_lvl = max_lvl
            if (2*final_lvl >= lvl) then
              add1(final_lvl) = add1_tmp
              add2(final_lvl) = add2_tmp
              weights(:,final_lvl) = wgttmp(:)
              exit sift_loop2
            endif
          endif
        end do sift_loop2
      end do

      !***
      !*** swap the last two entries
      !***


      add1_tmp = add1(2)
      add1(2)  = add1(1)
      add1(1)  = add1_tmp

      add2_tmp = add2(2)
      add2(2)  = add2(1)
      add2(1)  = add2_tmp

      wgttmp (:)   = weights(:,2)
      weights(:,2) = weights(:,1)
      weights(:,1) = wgttmp (:)
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine sort_add'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------

      end subroutine sort_add

!-----------------------------------------------------------------------

      subroutine uniq_add(add1, add2, weights, num_links, num_wts)

!-----------------------------------------------------------------------
!
!     this routine packs repeated links (same src and dst add)
!     by summation of the weights
!
!-----------------------------------------------------------------------

      use kinds_mod     ! defines common data types
      use constants     ! defines common scalar constants
      USE mod_oasis_flush

      implicit none

!-----------------------------------------------------------------------
!
!     Input and Output arrays
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(inout) :: num_links
      integer (kind=int_kind), intent(in) :: num_wts
      integer (kind=int_kind), intent(inout), dimension(num_links) :: &
                                                  add1,   &    !  destination address array (num_links)
                                                  add2         ! source address array

      real (kind=dbl_kind), intent(inout), dimension(num_wts, num_links) :: &
                                                  weights     !  remapping weights (num_wts, num_links)

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      INTEGER (kind=int_kind) :: bott_add, top_add ! Equal address zone delimiter
      INTEGER (kind=int_kind) :: n                 ! Weights loop counter
      LOGICAL (kind=log_kind),DIMENSION(num_links) :: pck_msk ! Boolean mask for packing
!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine uniq_add'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF

      bott_add = num_links ! Proceed bottom up
      pck_msk(:) = .TRUE.  ! All links taken by default

      goup: DO WHILE (bott_add .GT. 1) ! Proceed bottom up
         
         ! Look for equal address zones: delimit by bottom and top
         ! address

         top_add = bott_add - 1

         equal: DO WHILE (add2(top_add)==add2(bott_add).AND.add1(top_add)==add1(bott_add))
            top_add = top_add - 1
            IF (top_add == 0) EXIT equal
            
         END DO equal
      
         top_add = top_add + 1

         ! If the zone is not empty, cumulate weights in the topmost
         ! location
         ! and flag out the other ones

         IF (top_add /= bott_add) THEN

            DO n=1, num_wts
               weights(n,top_add) = SUM(weights(n,top_add:bott_add))
            END DO

            pck_msk(top_add+1:bott_add) = .FALSE.
            
         END IF

         ! Look for the followig zone by moving the bottom up

         bott_add = top_add - 1

      END DO goup

      ! Count non flagged out links

      num_links = COUNT(pck_msk)

      ! Collapse toward the top the non flagged out links

      add1(1:num_links) = PACK(add1,pck_msk)
      add2(1:num_links) = PACK(add2,pck_msk)
      DO n = 1, num_wts
         weights(n,1:num_links) = PACK(weights(n,:),pck_msk)
      END DO
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine uniq_add'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------

      end subroutine uniq_add

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
