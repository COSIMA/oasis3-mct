program lucia_analysis
!
! ===================================
! LUCIA post-processing analysis tool
! ===================================
!
! Purpose:
!          - Load balance an OASIS-based coupled system
!          - Assess scalabilities of each model of the coupled system
!          - Estimate coupling cost (interpolations) and models jitter
!
! Getting started:
!
!          - compile your model with OASIS version newer than 
!          - launch coupled sytem with second LOGPRT parameter equal 
!                to -1 (in namcouple file)
!          - process lucia.??.?????? files with lucia-mct script 
!                shell provided with the present FORTRAN program
!
implicit none
!
!   Information related to each coupled field
!
type FIELD_SEQUENCE
    integer*4 ::  namID           ! field rank in namcouple
    integer*4 ::  source_model    ! index of source model
    integer*4 ::  target_model    ! index of target model
    integer*4 ::  source_comm_nb  ! number of send actions from source
    integer*4 ::  target_comm_nb  ! number of receive actions from target
end type FIELD_SEQUENCE

    ! Explicit logical flags
    logical :: l_put, l_before, l_exch_still_valid, l_exch_not_yet_valid, l_add_interp_time
!
    character(len=9)  ::  c_verif          ! Lucia identifyer (no more used)
    character(len=3)  ::  c_field_code     ! code for field number or info provided during init
    character(len=24) ::  c_field_name     
    character(len=15) ::  c_comm_type      ! kind of exchange (put or get)

    ! Parameters
    character(len=3)  ::  c_ident_put = "put"
    character(len=3)  ::  c_ident_before = "Before"

    ! Buffer arrays
    character(len=3)   :: c_test_name
    character(len=24)  :: c_dummy
    character(len=300) :: c_argument, log_file_name
!
    character(len=20), dimension(:,:), allocatable ::  field_name
    character(len=10), dimension(:), allocatable   ::  model_name
    character(len=10), dimension(:), allocatable   ::  model_code  ! model rank on OASIS sequence
!
    ! for first static allocation, maximum number of coupling fields
    integer*4  :: max_nb_fields = 300
!
    ! Indexes or temporary arrays
    integer*4  :: i, j, k, l, mi, narg, i_err, newf, tmp_field_code

    integer*4  :: nb_models
    integer*4  :: nb_tot_log_files, log_nb  ! number of log file processed and index

    integer*4  :: cpl_field_nb        ! total number of coupling fields 
                                      ! same as NFIELDS parameter in namcouple
    integer*4  :: i_cpl               ! coupling field index (namcouple order)

    integer*4  :: clk_i               ! event index    1: before send
                                      !                2: after  send
                                      !                3: before receive
                                      !                4: after  receive

    integer*4  ::  before_send = 1
    integer*4  ::  after_send  = 2
    integer*4  ::  before_recv = 3
    integer*4  ::  after_recv  = 4
    integer*4  ::  after_send_or_recv  = 2

    integer*4  :: max_comm_nb         ! maximum nb of coupling tstep among coupling fields
    integer*4  :: i_first_exchg(2)    ! indexes of first coupling field exchanged

    integer*4, dimension(:), allocatable ::  nb_mpi           ! array of mpi partition nb per model
    integer*4, dimension(:), allocatable ::  i_stride         ! stride for log file counting per model
    integer*4, dimension(:), allocatable ::  i_file_count     ! log file nb per model
    integer*4, dimension(:), allocatable ::  valid_comm_nb    ! nb of valid coupling tstep per model
    integer*4, dimension(:), allocatable ::  first_valid_comm ! index of first valid communication

    integer*4, dimension(:), allocatable ::  i_cpl_field      ! index of coupling field in exchange sequence
    integer*4, dimension(:,:), allocatable ::  field_code     ! index of coupling fields in namcouple exchange

    integer*4, dimension(:,:), allocatable ::  comm_type   ! Communication type 
                                                           !  0: get 
                                                           !  1: put

    integer*4, dimension(:,:), allocatable ::  comm_nb        ! nb of coupling tstep per model and per field
!
    real*8 :: r_clock                  ! read clock time
    real*8 :: r_min_time, r_max_time   ! time boundaries
    real*8 :: r_reference = -1.E8      ! reference time
    real*8 :: r_mean                   ! tmp buffer
    real*8 :: temp_t                   ! tmp buffer
    real*8 :: r_impossible_value= 1.E13      ! reference time
    real*8 :: r_test_impossible= 1.E12      ! reference time

    real*8, dimension(:), allocatable ::  calc_time, noncalc_time   ! calculation and non calculation time per model

    !                                     Timing for interpolation time and jitter per model
    real*8, dimension(:), allocatable ::  r_interp_measure, r_interp_time, r_jitter_time

    !                                     Evaluation of variance among log files
    real*8, dimension(:), allocatable ::  send_spread, receive_spread, calc_spread

    real*8, dimension(:,:), allocatable   ::  start_time            ! beginning of first coupling sequence
    real*8, dimension(:,:,:), allocatable ::  min_clock_measure     ! measure of min among log files
    real*8, dimension(:,:,:), allocatable ::  max_clock_measure     ! measure of max among log files
    real*8, dimension(:,:,:), allocatable ::  calc_noncalc_measure  ! calculation and non calculation time for each event
!
!   Informations on coupling fields
    type(FIELD_SEQUENCE), dimension(:), allocatable :: cpl_fields 
!
!   external function
    integer*4 :: iargc
!
!
!   GET THE NUMBER OF COMMAND LINE ARGUMENTS.
!
    narg = iargc()
!
!   CHECK THE NUMBER OF COMMAND LINE ARGUMENTS.
!
    if ( narg==0 ) then
        write (6,*)
        write (6,*) ' Error: Missing arguments'
        stop
    else if ( narg<6 ) then
        write (6,*) ' Wrong number of line arguments '
        write (6,*) ' Coupled models should be 2 at least '
        stop
    end if
!
!   3 argument per model
    nb_models = narg / 3
!
!   ALLOCATIONS of nb_models dimensional arrays
    allocate(nb_mpi(nb_models))
    allocate(i_stride(nb_models))
    allocate(i_file_count(nb_models))
    allocate(i_cpl_field(nb_models))
    allocate(model_name(nb_models))
    allocate(model_code(nb_models))
    allocate(calc_time(nb_models))
    allocate(noncalc_time(nb_models))
    allocate(r_jitter_time(nb_models))
    allocate(send_spread(nb_models))
    allocate(receive_spread(nb_models))
    allocate(calc_spread(nb_models))
    allocate(r_interp_time(nb_models))
    allocate(valid_comm_nb(nb_models))
    allocate(first_valid_comm(nb_models))

!   ALLOCATIONS of nb_models x max_nb_fields dimensional arrays
    allocate(field_name(nb_models, max_nb_fields))
    allocate(field_code(nb_models, max_nb_fields))
    allocate(comm_type(nb_models, max_nb_fields))
    allocate(comm_nb(nb_models, max_nb_fields))
    allocate(start_time(nb_models, max_nb_fields))

    comm_nb(:,:) = 0
    start_time(:,:) = r_impossible_value
!
!   DEFAULT VALUES FOR COMMAND LINE ARGUMENTS.
!
    nb_mpi(:) = 1
    model_code(:) = "none"
    model_name(:) = "none"
    field_name(:,:) = "none"
!
!  1. CHECK THE COMMAND LINE ARGUMENTS.
!
    do i = 1, nb_models
!
       ! GET MODEL RANK ON OASIS SEQUENCE
       call getarg( 3*i-2, model_code(i) )
!
       ! GET LOG FILE NB
       call getarg( 3*i-1, c_argument )
       read(c_argument,'(i6)') i_stride(i)
       ! must be greater than 1
       i_stride(i) = MAX(i_stride(i),1)
!
       ! GET NUMBER OF MPI SUBDOMAINS
       call getarg( 3*i, c_argument )
       read(c_argument,'(i6)') nb_mpi(i)
       ! check that stride still greater than 1
       i_stride(i) = MAX ( nb_mpi(i) / MAX((i_stride(i)-1),1), 1)
!
    end do
!
!   2. READ OASIS-LUCIA LOG FILES CONTAINT
!
!   DATA ARE READ FOR A FIRST TIME TO FIND ARRAYs LENGTH
!   AND COUPLING FIELDS EXCHANGE SEQUENCE
!
    write(6,*) ' '
    write(6,*) ' Processing OASIS LUCIA log files '
    write(6,*) ' '
!
    i_cpl_field(:)=0

!   Loop on model number
    do i = 1, nb_models
!
       i_file_count(i) = 0
       k = 1

       write(6,*) 'Computed log files for model ', model_code(i), nb_mpi(i), i_stride(i)
       call flush(6)

       ! Loop on log file number
       do j = 0, nb_mpi(i), i_stride(i)

          ! Count number of log file per model
          i_file_count(i) = i_file_count(i) + 1

          write(log_file_name,'("lucia.",a2,".",i6.6)'),model_code(i),j
          write(6,'(TL16,A,1X)', advance='no') TRIM(log_file_name)
          call flush(6)

          OPEN (unit=10, &
                file=TRIM(log_file_name), &
                action="read", &
                status="OLD", &
                form="formatted", &
                iostat=i_err)
          if ( i_err .ne. 0 ) then
             write(6,*) 'Error opening ASCII file ', TRIM(log_file_name)
             stop
          end if

!         write (6,*) ' open ', log_file_name
!
!
!         FIRST GUESS: GET FIELD NAMES AND EXCHANGE TYPE
!
          REWIND(10)
          i_err=0
          ! For each line of the log file
          DO WHILE ( i_err /= -1 )
             READ(10, '(A9,A3,A12,A4,F16.5)', iostat=i_err) &
                                    &      c_verif, &
                                    &      c_field_code, &
                                    &      c_dummy, &
                                    &      c_comm_type, &
                                    &      r_clock
             ! EOF
             IF ( i_err == -1 ) cycle
!
!            if ( i == 2 ) write(6,*) ' file ', TRIM(c_verif), TRIM(c_field_code)

             ! Skip if initial synchro
             IF ( c_field_code(1:3) == "IT " ) cycle

             ! Skip if interpolation time measurement
             IF ( INDEX ( TRIM(c_comm_type),'rpo'  ) /= 0 ) cycle

             ! Read model names
             IF ( c_field_code(1:3) == "MD " ) THEN
                IF ( TRIM(model_name(i)) == "none" ) model_name(i) = TRIM(c_dummy)
                cycle
             ENDIF
                   
             ! Read field names as declared in namcouple
             IF ( c_field_code(1:3) == "SN " .or. c_field_code(1:3) == "RC " ) THEN
                BACKSPACE(10)
                READ(10, '(A9,A3,A5,A)', iostat=i_err) &
                                         &      c_verif, &
                                         &      c_field_code, &
                                         &      c_dummy, &
                                   &            c_field_name
                IF ( j == 0 ) THEN
                   read(c_dummy(1:4),'(i4)') k
                   field_name(i,k) = TRIM(c_field_name)
                ENDIF
                cycle
             ENDIF
!
!            PROCESS INFORMATION FROM STANDARD TIMING LINE
! 
             ! When coupling field list is empty (beginning)
             IF ( i_cpl_field(i) == 0 ) THEN
                ! Fill list with first field found
                i_cpl_field(i) = 1
                READ(c_field_code,'(i3)') field_code(i, i_cpl_field(i))
                ! Start counting coupling steps
                comm_nb(i,i_cpl_field(i)) = 1

!                      if ( i == 2 ) write(6,*) ' field ', TRIM(field_code(i, 1)), comm_nb(i,1)

                ! Identify if it is a put or a get communication
                IF ( INDEX ( TRIM(c_comm_type), c_ident_put ) /= 0 ) THEN
                   ! This communication is a put
                   comm_type(i, i_cpl_field(i)) = 0
                   start_time(i,i_cpl_field(i)) = r_clock
                ELSE
                   ! This communication is a get
                   comm_type(i, i_cpl_field(i)) = 1
                   start_time(i,i_cpl_field(i)) = r_clock
                ENDIF

             ! When field list is not empty (loop)
             ELSE 
                ! Coupling field index initialized
                mi = 1
                newf = 0
                ! Check model field number already identified
                DO WHILE ( mi <= i_cpl_field(i) )
                   read(c_field_code,'(i3)') tmp_field_code
                   IF ( field_code(i, mi) == tmp_field_code ) &
                      newf = mi
                   mi = mi + 1
                END DO
                ! Another field found because not identified in the list
                IF ( newf == 0 ) THEN
                   ! Fill list with new field found (same than above)
                   i_cpl_field(i) = i_cpl_field(i) + 1
                   READ(c_field_code,'(i3)') field_code(i, i_cpl_field(i))
                   comm_nb(i,i_cpl_field(i)) = 1

                   ! Identify if it is a put or a get communication
                   IF ( INDEX ( TRIM(c_comm_type), c_ident_put ) /= 0 ) THEN
                      ! This communication is a put
                      comm_type(i, i_cpl_field(i)) = 0
                      start_time(i,i_cpl_field(i)) = r_clock
                   ELSE
                      comm_type(i, i_cpl_field(i)) = 1
                      start_time(i,i_cpl_field(i)) = r_clock
                   ENDIF

                ! Just another coupling step for an already identified field
                ELSE
                   comm_nb(i,newf) = comm_nb(i,newf) + 1
                END IF
             END IF

          ! End loop on read lines
          END DO   

          CLOSE(10)

       ! End loop on log files
       END DO

       write(6,*) ' '
!
!          write(6,*) ' nbFields ', model_name(i) , i_cpl_field(i)
!          do k = 1, i_cpl_field(i)
!            write(6,*) ' Field ', field_code(i, k), comm_type(i,k)
!          ENDDO
!
    ! End loop on models
    END DO
!
!   Coupling fields counterd twice (as sent and received field)
    cpl_field_nb = SUM(i_cpl_field(:))/2

    ALLOCATE ( cpl_fields ( cpl_field_nb ) )
!
!    write(6,*) ' nb fields ', cpl_field_nb
!    call flush(6)
!
!
    DO i = 1, nb_models
       ! Count total send/received fields (divide by proc number)
       comm_nb(i,:) = comm_nb(i,:) / i_file_count(i)
    END DO
!
!   3. FIND EXCHANGE ORDER
!
    j = 1
    ! loop on coupling fields
    DO WHILE ( j <= cpl_field_nb )
       ! Find the earliest exchange
       i_first_exchg = MINLOC(start_time(:,:))
       ! Index of model doing the first exchange
       mi = i_first_exchg(1)
       ! Index of first field exchanged
       i = i_first_exchg(2)

       ! only if it is a send field
       IF ( comm_type(mi,i) == 0 ) THEN
          ! Find the exchange number in OASIS sequence (namcouple)
          cpl_fields(j)%namID = field_code ( mi, i ) 
          ! Set its source model
          cpl_fields(j)%source_model = mi
          ! Set how many times this coupling field has been sent
          cpl_fields(j)%source_comm_nb = comm_nb( mi, i )
          ! 
          DO k = 1, nb_models
             IF ( k == mi ) cycle 
              IF ( ANY ( field_code ( k, 1:i_cpl_field(k) ) == cpl_fields(j)%namID)) THEN
                 ! Set its target model
                 cpl_fields(j)%target_model = k
                 DO l = 1, i_cpl_field(k)
                    IF ( field_code(k,l) == cpl_fields(j)%namID ) &
                       ! Set how many times this coupling field has been received
                       cpl_fields(j)%target_comm_nb = comm_nb( k, l )
                 END DO
              END IF
          END DO
          j = j + 1
       END IF
       start_time(mi,i) = r_impossible_value
    END DO
!
    write (6,*) ' '
    write (6,*) ' "Lucia" analysis '
    write (6,*) ' '
    write (6,*) ' Exchanged fields (based on first exchange): '
!
    write (6,*) ' From model :                  to model '
    DO i = 1, cpl_field_nb
       write (6,*) '         ',  TRIM(model_name(cpl_fields(i)%source_model)), &
                ' ( ', TRIM(field_name(cpl_fields(i)%source_model,cpl_fields(i)%namID)), &
                ' )      ', TRIM(model_name(cpl_fields(i)%target_model)), &
                ' ( ', TRIM(field_name(cpl_fields(i)%target_model,cpl_fields(i)%namID)) , ' )'
    END DO
!    write (6,*) ' '
!    call flush(6)
!
!  4. CHECK COMMUNICATION NUMBER CONCORDANCE
!
    do i = 1, cpl_field_nb
       IF ( cpl_fields(i)%target_comm_nb /= cpl_fields(i)%source_comm_nb ) THEN
          write(6,*) ' WARNING - Coupler exchanges: ' , & 
         TRIM(field_name(cpl_fields(i)%source_model,cpl_fields(i)%namID)) , &
         ' sent ', cpl_fields(i)%source_comm_nb, &
         ' but ', TRIM(field_name(cpl_fields(i)%target_model,cpl_fields(i)%namID)) , &
         ' received ', cpl_fields(i)%target_comm_nb
!
!         In case of unbalanced exchange number (abnormal stop),
!         restrain communication number according to the last valid exchange number
!
          cpl_fields(i)%source_comm_nb = MIN ( cpl_fields(i)%target_comm_nb, cpl_fields(i)%source_comm_nb)
          cpl_fields(i)%target_comm_nb = MIN ( cpl_fields(i)%target_comm_nb, cpl_fields(i)%source_comm_nb)
       ENDIF
    end do
!
!   Find valid number of coupling 
!
    do j = 1, cpl_field_nb
       ! OASIS sends = OASIS receives. Count only before event (/2)
       cpl_fields(j)%target_comm_nb = MIN ( cpl_fields(j)%source_comm_nb , cpl_fields(j)%target_comm_nb ) / 2
       ! Same number of "received" and "send" exchange
       cpl_fields(j)%source_comm_nb = cpl_fields(j)%target_comm_nb
    end do

    !  Substract 1 to number of coupling tstep 
    !  (last exchange ignored to avoid side effect of termination phase)
    max_comm_nb = MAXVAL ( cpl_fields(:)%source_comm_nb ) - 1
    cpl_fields(:)%source_comm_nb = cpl_fields(:)%source_comm_nb - 1
    cpl_fields(:)%target_comm_nb = cpl_fields(:)%source_comm_nb

    ! Set the maximum number of coupling tstep per model
    valid_comm_nb(:) = 0
    do i = 1, nb_models
       do j = 1, cpl_field_nb
          IF ( cpl_fields(j)%source_model == i .or. cpl_fields(j)%target_model == i ) &
             valid_comm_nb(i) = MAX(valid_comm_nb(i),cpl_fields(j)%source_comm_nb)
       end do
    end do
       
! 
!  5. Allocate Timing Arrays 
!    -  to fill with minimum or maximum clock time among log files
!       before and after each coupling communications (put and get)
!       and for each coupling field
!
    ALLOCATE ( min_clock_measure ( cpl_field_nb, max_comm_nb , 4 ) )
    ALLOCATE ( max_clock_measure ( cpl_field_nb, max_comm_nb , 4 ) )

!   initialize min/max counters
    min_clock_measure = r_impossible_value
    max_clock_measure = r_impossible_value * (-1.)
!
!    - to store calculation / non calculation time 
!      for each log file of the same model
!
!   calc_noncalc_measure ( : ,  1 ) : total 'calculation' time
!   calc_noncalc_measure ( : ,  2 ) : total time spent during send operation
!   calc_noncalc_measure ( : ,  3 ) : total time spent during receive operation
!
    nb_tot_log_files = SUM ( i_file_count(:) )
    ALLOCATE ( calc_noncalc_measure ( nb_tot_log_files, max_comm_nb, 3 ) )
    ALLOCATE ( r_interp_measure ( nb_tot_log_files ) )

!   initialize measures total
    calc_noncalc_measure (:,:,:) = 0
!
!   6. READ AGAIN OASIS-LUCIA LOG FILES CONTAINT
!      AND FILL ARRAYS WITH ALL CLOCK TIME MEASURES 
!
    log_nb = 0
    r_max_time = r_impossible_value * (-1.)
    r_min_time = r_impossible_value

    ! Loop on model number
    DO i = 1, nb_models
       ! Loop on log file number
       DO j = 0, nb_mpi(i), i_stride(i)

          ! Count number of log file
          log_nb = log_nb + 1

          write(log_file_name,'("lucia.",a2,".",i6.6)'),model_code(i),j

          OPEN (unit=10, &
                file=TRIM(log_file_name), &
                action="read", &
                status="OLD", &
                form="formatted", &
                iostat=i_err)
          IF ( i_err .ne. 0 ) then
             write(6,*) 'Error opening ASCII file ', TRIM(log_file_name)
             STOP
          END IF

          REWIND(10)

          l_exch_still_valid = .true.
          l_exch_not_yet_valid = .true.

          c_test_name="not"
          mi = 0
          r_interp_measure(log_nb)=0
          l_add_interp_time=.false.

          i_err=0
          ! For each line of the log file
          DO WHILE ( i_err /= -1 .and. l_exch_still_valid )
             READ(10, '(A9,A3,A16,F16.5)', iostat=i_err) &
                       &   c_verif, &
                       &   c_field_code, &
                       &   c_comm_type, &
                       &   r_clock
             IF ( i_err == -1 ) CYCLE

!            Substract first clock measure to store anomaly instead of raw value
!            (to avoid too big values when additionning)
             IF ( c_field_code(1:3) == "IT " ) THEN
                r_reference = r_clock
                CYCLE
             ENDIF

             ! Skip model names
             IF ( c_field_code(1:3) == "MD " ) cycle
                   
             ! Skip field names
             IF ( c_field_code(1:3) == "SN " .or. c_field_code(1:3) == "RC " ) cycle
                   
             r_clock = r_clock - r_reference

             ! Special treatment for interpolation time :cumulated
             IF ( INDEX ( TRIM(c_comm_type), 'interpo' ) /= 0 ) then
                IF ( l_add_interp_time ) then
                   r_interp_measure(log_nb) = r_interp_measure(log_nb) + r_clock
                   l_add_interp_time=.false.
                ELSE
                   r_interp_measure(log_nb) = r_interp_measure(log_nb) - r_clock
                   l_add_interp_time=.true.
                ENDIF
                CYCLE
             ENDIF

!
!            PROCESS INFORMATION FROM STANDARD TIMING LINE
! 

             ! Get the name of the first field exchanged by this model
             IF ( TRIM(c_test_name) == "not" ) c_test_name=TRIM(c_field_code)

!            write(6,*) 'c_comm_type ', c_comm_type
!            write(6,*) 'c_test_name ', c_test_name
!            write(6,*) 'TRIM(c_field_code) ', TRIM(c_field_code)

!
!            Find field name as declared in namcouple
!
             k = 1
             DO WHILE ( k <= cpl_field_nb )
                READ(c_field_code,'(i3)') tmp_field_code
                IF ( cpl_fields(k)%namID == tmp_field_code ) &
                   i_cpl = k
                k = k + 1
             END DO

!            write(6,*) 'field number ', i_cpl
!
!            Determine if the exchange is a put or a get
!                      if the timing is set before or after the exchange

             l_put = .false. 
             l_before = .false. 
!
!            and attribute the corresponding index:
!                                1: Before put
!                                2: After put
!                                3: Before get
!                                4: After get

             IF ( INDEX ( TRIM(c_comm_type), c_ident_put ) /= 0 ) &
                l_put = .true.
             IF ( INDEX ( TRIM(c_comm_type), c_ident_before ) /= 0 ) &
                l_before = .true.

             IF ( l_before ) THEN
                IF ( l_put ) THEN
                   clk_i = before_send
                ELSE
                   clk_i = before_recv
                ENDIF
             ELSE
                IF ( l_put ) THEN
                   clk_i = after_send
                ELSE
                   clk_i = after_recv
                ENDIF
             ENDIF

!            write(6,*) 'index number ', clk_i
!
!            Determine exchange validity
!
!            Measures start (coupler) the first time than a field is received 
!            This excludes restart reading sequence side effect if any
!
!
             IF ( l_exch_not_yet_valid .AND. cpl_fields(i_cpl)%source_comm_nb == valid_comm_nb(i) ) THEN
                l_exch_not_yet_valid = .false.
                ! Get the name of the first field exchanged by this model
                c_test_name = TRIM(c_field_code)
                ! and at what time it is
                r_min_time = MIN ( r_clock, r_min_time )
             END IF

             ! If before exchange of the first coupling field
             ! on a not yet valid exchange
             IF ( TRIM(c_field_code) == TRIM(c_test_name) .and. l_before &
                  .and. .not. l_exch_not_yet_valid ) THEN

                ! Increase exchange number
                mi = mi + 1

                ! Before the last valid exchange
                IF ( mi <= valid_comm_nb(i) ) THEN
                   ! calc/nocalc current index initialization
                   calc_noncalc_measure ( log_nb, mi, 1 ) = &
                   calc_noncalc_measure ( log_nb, mi, 1 ) -  r_clock
                ENDIF

                ! After the first exchange
                IF ( mi > 1 ) THEN
                   ! calc/nocalc previous index finalization
                   calc_noncalc_measure ( log_nb, mi-1, 1 ) = &
                   calc_noncalc_measure ( log_nb, mi-1, 1 ) +  r_clock
                ENDIF

                ! Increase time counter to find timing of last exchange
                r_max_time = MAX ( r_clock, r_max_time )

!                   write(6,*) 'still valid ', l_exch_still_valid

             END IF

             ! Reach maximum number of valid exchanges
             IF ( mi > valid_comm_nb(i) ) &
                l_exch_still_valid = .false.

             ! Do not fill timer arrays if exchange not yet or no more valid
             IF ( l_exch_not_yet_valid .or. .not. l_exch_still_valid ) CYCLE

!                   write(6,*) 'not cycled ', TRIM(c_field_code)

             ! Fill mix/max array compared to previous log file measures
             min_clock_measure(i_cpl ,mi ,clk_i) =  &
                 MIN ( min_clock_measure(i_cpl ,mi ,clk_i), r_clock )

             max_clock_measure(i_cpl ,mi ,clk_i) =  &
                 MAX ( max_clock_measure(i_cpl ,mi ,clk_i), r_clock )

             ! Fill calc/noncalc array for each log file
             ! Sending time
             IF ( clk_i == after_send ) THEN
                 calc_noncalc_measure ( log_nb, mi, 2 ) = &
                 calc_noncalc_measure ( log_nb, mi, 2 ) +  r_clock
             ELSE IF ( clk_i == before_send ) THEN
                 calc_noncalc_measure ( log_nb, mi, 2 ) = &
                 calc_noncalc_measure ( log_nb, mi, 2 ) -  r_clock
             ! Receiving time
             ELSE IF ( clk_i == after_recv ) THEN
                 calc_noncalc_measure ( log_nb, mi, 3 ) = &
                 calc_noncalc_measure ( log_nb, mi, 3 ) +  r_clock
             ELSE IF ( clk_i == before_recv ) THEN
                 calc_noncalc_measure ( log_nb, mi, 3 ) = &
                 calc_noncalc_measure ( log_nb, mi, 3 ) -  r_clock
             ENDIF
             ! Calculation time
             IF ( MOD ( clk_i , after_send_or_recv ) == 1 ) THEN
                 calc_noncalc_measure ( log_nb, mi, 1 ) = &
                 calc_noncalc_measure ( log_nb, mi, 1 ) +  r_clock
             ELSE
                 calc_noncalc_measure ( log_nb, mi, 1 ) = &
                 calc_noncalc_measure ( log_nb, mi, 1 ) -  r_clock
             ENDIF
             
          ! End loop on read lines
          END DO

          CLOSE(10)

       ! End loop on log files
       END DO

    ! End loop on models
    END DO
!
!   7. ANALYSIS
!
    calc_time (:) = 0 ;  noncalc_time (:) = 0
    send_spread (:) = 0 ;  receive_spread (:) = 0; calc_spread (:) = 0
!
!   7.1 ANALYSIS ON MAXIMUM MEAN VALUES AMONG LOG FILES
!
    k = 1

    ! Loop on models
    DO i = 1, nb_models

!       write(6,*), ' Model   : ', i

       ! For most frequently coupled fields
       IF ( valid_comm_nb(i) == max_comm_nb ) THEN
          ! Start analysis on third exchange to avoid side effect
          first_valid_comm(i) = 3
       ELSE
          ! only on second for the others
          first_valid_comm(i) = 2
       END IF

       ! Special treatment for models not involved in coupling (IO servers)
       IF ( valid_comm_nb(i) == 0 ) first_valid_comm(i) = 1

       ! Loop on valid coupling tsteps
       DO j = first_valid_comm(i), valid_comm_nb(i)

          ! Maximum values over log files are added for all valid coupling tsteps
          ! ... for time spent by models on calculations
          calc_time (i) = calc_time (i) + MAXVAL(calc_noncalc_measure (k:k+i_file_count(i)-1, j, 1))
          ! ... for time spent by models on OASIS exchanges (send and receive operations)
          noncalc_time (i) = noncalc_time (i) + &
                             MAXVAL(calc_noncalc_measure (k:k+i_file_count(i)-1, j, 2)) + &
                             MAXVAL(calc_noncalc_measure (k:k+i_file_count(i)-1, j, 3))

          ! Variance among log file is calculated for those 2 values
          r_mean = SUM ( calc_noncalc_measure (k:k+i_file_count(i)-1, j, 2) ) / i_file_count(i)
          send_spread (i) = send_spread (i) + &
                            SQRT ( SUM ( ( calc_noncalc_measure (k:k+i_file_count(i)-1, j, 2) - &
                                           r_mean ) ** 2 ) )
          r_mean = SUM ( calc_noncalc_measure (k:k+i_file_count(i)-1, j, 3) ) / i_file_count(i)
          receive_spread (i) = receive_spread (i) + &
                            SQRT ( SUM ( ( calc_noncalc_measure (k:k+i_file_count(i)-1, j, 3) - &
                                           r_mean ) ** 2 ) ) 
          r_mean = SUM ( calc_noncalc_measure (k:k+i_file_count(i)-1, j, 1) ) / i_file_count(i)
          calc_spread (i) = calc_spread (i) + &
                            SQRT ( SUM ( ( calc_noncalc_measure (k:k+i_file_count(i)-1, j, 1) - &
                                           r_mean ) ** 2 ) )
 !
       END DO

       ! Time spent on OASIS interpolation is a mean value among log files
       r_interp_time(i) = SUM(r_interp_measure(k:k+i_file_count(i)-1))/i_file_count(i)

       ! Counter on log file index among total log file number
       k = k + i_file_count(i)

    END DO
!
!   WRITE INFO ON STANDARD OUTPUT
!
!   Old analysis, no more active
!
!    WRITE(6,*) ' '
!    WRITE(6,*), ' Component -           Computation -       Waiting time (s) -  # cpl step '

!    DO i = 1, nb_models
!       WRITE(6,'(2X, A6, 5X, F10.2, A7, F6.2, A3, 5X, F10.2, A7, F6.2, A4, I4)'), &
!                  model_name(i), &
!                  calc_time(i), &
!                  ' ( +/- ', calc_spread(i) , ' ) ', &
!                  noncalc_time(i), &
!                  ' ( +/- ', send_spread(i)+receive_spread(i), ' )  ', &
!                  valid_comm_nb(i)-first_valid_comm(i)+1
!    END DO
!    WRITE(6,*), ' '
!    call flush(6)
!
!   7.2 ANALYSIS ON BOUNDARY VALUES AMONG LOG FILES
!
!
    r_min_time = 0. ; r_max_time = 0.
!
    calc_time(:) = 0.
    noncalc_time(:) = 0.
    calc_time(:) = 0.
    r_jitter_time(:) = 0.

    ! loop on models
    DO k = 1, nb_models
       ! loop on coupling fields
       DO i = 1, cpl_field_nb

          ! If sent field
          IF ( cpl_fields(i)%source_model == k ) THEN
             ! loop on coupling time steps
                                     ! WARNING valid_comm_nb depends on cpl_field_nb (if different model
                                     ! with diff cpl time step)
             DO j = first_valid_comm(k), valid_comm_nb(k)
                ! If a timing is available for this coupling field at this coupling time step
                IF ( max_clock_measure (i,j,2) < r_test_impossible .and. max_clock_measure (i,j,1) < r_test_impossible ) &
                   ! add sending time to the total of non calculation time
                   noncalc_time(k) = max_clock_measure (i,j,2) - max_clock_measure (i,j,1) + &
                                     noncalc_time(k)
                   ! WARNING : sending time starts when slowest mpi process check on log files is before sending
                   !            and stops when slowest mpi process check on log files is after sending
                IF ( max_clock_measure (i,j,1) < r_test_impossible .and. ABS(min_clock_measure (i,j,1)) < r_test_impossible ) &
                   ! Measure before sending between slowest and fastest mpi process check on log files
                   r_jitter_time(k) = max_clock_measure (i,j,1) - min_clock_measure (i,j,1) + &
                                      r_jitter_time(k)
             ENDDO
          ! If received field
          ELSE IF ( cpl_fields(i)%target_model == k ) THEN
             ! loop on coupling time steps
                                     ! WARNING valid_comm_nb depends on cpl_field_nb (if different model
                                     ! with diff cpl time step)
             DO j = first_valid_comm(k), valid_comm_nb(k)
                ! If a timing is available for this coupling field at this coupling time step
                IF ( max_clock_measure (i,j,4) < r_test_impossible .and. max_clock_measure (i,j,3) < r_test_impossible ) &
                   ! add receiving time to the total of non calculation time
                   noncalc_time(k) = max_clock_measure (i,j,4) - max_clock_measure (i,j,3) + &
                                     noncalc_time(k)
                   ! WARNING : receiving time starts when slowest mpi process check on log files is before receiving
                   !            and stops when slowest mpi process check on log files is after receiving
                IF ( max_clock_measure (i,j,3) < r_test_impossible .and. ABS(min_clock_measure (i,j,3)) < r_test_impossible ) &
                   ! Measure before receiving between slowest and fastest mpi process check on log files
                   r_jitter_time(k) = max_clock_measure (i,j,3) - min_clock_measure (i,j,3) + &
                                      r_jitter_time(k)
             ENDDO
          ENDIF
       ENDDO
!
       r_min_time = r_impossible_value * (-1.)
       r_max_time = r_impossible_value * (-1.)

!      CALCULATE TIME BOUNDS

       l_put = .true.

       ! Loop on coupling fields
       DO i = 1, cpl_field_nb
          ! on target model
          IF ( cpl_fields(i)%target_model == k ) THEN
             ! Measure first valid time when field received (after receiving)
             temp_t = max_clock_measure(i,first_valid_comm(k)-1,4)
             ! If later than reference
             IF ( temp_t > r_min_time .and. temp_t < r_test_impossible ) &
                ! Set it as reference 
                r_min_time = temp_t
             ! Measure last valid time when field received (after receiving)
             temp_t = max_clock_measure(i,valid_comm_nb(k),4)
             ! If later than reference
             IF ( temp_t > r_max_time .and. temp_t < r_test_impossible ) &
                ! Set it as reference 
                r_max_time = temp_t
             l_put = .false.
          ENDIF
       ENDDO

       ! IF NO RECEIVED FIELD ON MODEL DO THE SAME THAN PREVIOUSLY BUT WITH SENT FIELDS
       IF ( l_put ) THEN
          ! Loop on coupling fields
          DO i = 1, cpl_field_nb
             ! on target model
             IF ( cpl_fields(i)%source_model == k ) THEN
                ! Measure first valid time when field received (after receiving)
                temp_t = max_clock_measure(i,first_valid_comm(k)-1,2)
                ! If later than reference
                IF ( temp_t > r_min_time .and. temp_t < r_test_impossible ) &
                   ! Set it as reference 
                   r_min_time = temp_t
                ! Measure last valid time when field received (after receiving)
                temp_t = max_clock_measure(i,valid_comm_nb(k),2)
                ! If later than reference
                IF ( temp_t > r_max_time .and. temp_t < r_test_impossible ) &
                   ! Set it as reference 
                   r_max_time = temp_t
             ENDIF
          ENDDO
       ENDIF
!
!      CALCULATION TIME defined as total time minus OASIS communication time
       calc_time(k) = r_max_time - r_min_time - noncalc_time(k)

    ! End loop on models
    ENDDO
!
    WRITE(6,*) ' '
    WRITE(6,*), ' Load balance analysis '
    WRITE(6,*) ' '
    WRITE(6,*), ' Component -         Calculations   -     Waiting time (s) - # cpl step :'
!
!   WRITE INFO ON DAT FILE FOR GNUPLOT AND STANDARD OUTPUT
!
    WRITE(6,*) ' '
    OPEN (10, file="info.dat")
    DO i = 1, nb_models
       WRITE(10,'(I2, 2X, F10.3, 2X, F10.3, 2X, A6)'), &
                  i, calc_time(i), noncalc_time(i), model_name(i)
       WRITE(6,'(2X, A6, 16X, F10.2, 12X, F10.2, 10X, I4)'), &
                    model_name(i), calc_time(i), noncalc_time(i), valid_comm_nb(i)-first_valid_comm(i)+1
    ENDDO
    CLOSE (10)
          
    WRITE (6,*) ' '
!
    WRITE(6,*), ' Additional informations'
    WRITE(6,*), ' Component -  OASIS mean interpolation time -    Jitter  (s): '
    DO i = 1, nb_models
       WRITE(6,'(2X, A6, 12X, F10.2, 18X, F10.2 )'), &
                  model_name(i), r_interp_time(i), r_jitter_time(i)
    END DO
!
    WRITE (6,*) '  '
    WRITE (6,*) ' lucia completed successfully '
    WRITE (6,*) '  '

end program lucia_analysis
