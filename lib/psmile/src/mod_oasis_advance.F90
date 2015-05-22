
!> Advances the OASIS coupling

MODULE mod_oasis_advance

    USE mod_oasis_kinds
    USE mod_oasis_data
    USE mod_oasis_parameters
    USE mod_oasis_coupler
    USE mod_oasis_map
    USE mod_oasis_part
    USE mod_oasis_timer
    USE mod_oasis_var
    USE mod_oasis_sys
    USE mod_oasis_io
    USE mod_oasis_mpi
    USE mct_mod

    IMPLICIT NONE

    private

    public oasis_advance_init
    public oasis_advance_run


contains

!---------------------------------------------------------------------

!> Initializes the OASIS fields

  SUBROUTINE oasis_advance_init(kinfo)

!   ----------------------------------------------------------------
!   This routine handles initial restart and communication
!   of data for fields with positive lags
!   ----------------------------------------------------------------

    IMPLICIT none
!   ----------------------------------------------------------------
    INTEGER(kind=ip_i4_p), intent(inout) :: kinfo    !< status, not used
!   ----------------------------------------------------------------
    integer(kind=ip_i4_p) :: cplid,partid,varid
    INTEGER(kind=ip_i4_p) :: nf,lsize,nflds,npc
    integer(kind=ip_i4_p) :: dt,ltime,lag,getput
    integer(kind=ip_i4_p) :: msec
    real   (kind=ip_r8_p), allocatable :: array(:)  ! data
    real   (kind=ip_r8_p), allocatable :: array2(:) ! data
    real   (kind=ip_r8_p), allocatable :: array3(:) ! data
    real   (kind=ip_r8_p), allocatable :: array4(:) ! data
    real   (kind=ip_r8_p), allocatable :: array5(:) ! data
    logical               :: a2on,a3on,a4on,a5on    ! data 2-5 logicals
    integer(kind=ip_i4_p) :: mseclag   ! model time + lag
    character(len=ic_xl)  :: rstfile   ! restart filename
    character(len=ic_med) :: vstring   ! temporary string
    type(prism_coupler_type),pointer :: pcpointer
    character(len=*),parameter :: subname = '(oasis_advance_init)'
    logical, parameter :: local_timers_on = .false.

    call oasis_debug_enter(subname)

    !----------------------------------------------------------------
    !> oasis_advance_init does the following
    !> * Aborts if it's called from non-active tasks
    !----------------------------------------------------------------

    if (mpi_comm_local == MPI_COMM_NULL) then
       write(nulprt,*) subname,estr,'called on non coupling task'
       call oasis_abort()
    endif

    kinfo = OASIS_OK

    IF (local_timers_on) call oasis_timer_start ('advance_init')

    if (OASIS_debug >= 2) then
       write(nulprt,*) '   subname         at         time    time+lag   act: field '
       write(nulprt,*) '   diags :     fldname    min      max      sum '
    endif

    !----------------------------------------------------------------
    !> * Loop over all coupler connections, 
    !>   Loop over get and put connections,
    !>   For valid connections
    !----------------------------------------------------------------

    call oasis_debug_note(subname//' loop over cplid')
    DO cplid = 1,prism_mcoupler
    DO npc = 1,2
    if (npc == 1) pcpointer => prism_coupler_put(cplid)
    if (npc == 2) pcpointer => prism_coupler_get(cplid)
    if (pcpointer%valid) then
       dt    = pcpointer%dt
       lag   = pcpointer%lag
       ltime = pcpointer%ltime
       getput= pcpointer%getput
       rstfile=TRIM(pcpointer%rstfile)
       partid= pcpointer%partID
       msec = 0   ! reasonable default to start with
       mseclag = msec

       IF (OASIS_Debug >= 2) THEN
           WRITE(nulprt,*) '----------------------------------------------------------------'
           WRITE(nulprt,*) subname,' Field cplid :',cplid
           WRITE(nulprt,*) subname,' read restart:',npc,TRIM(pcpointer%fldlist)
           WRITE(nulprt,*) subname,' lag prism_mcoupler :',lag,prism_mcoupler
           WRITE(nulprt,*) '----------------------------------------------------------------'
           CALL oasis_flush(nulprt)
       ENDIF

       !------------------------------------------------
       !>   * Checks that lag is reasonable
       !------------------------------------------------

       IF (lag > dt .OR. lag <= -dt) THEN
          WRITE(nulprt,*) subname,estr,'lag out of dt range cplid/dt/lag=',cplid,dt,lag
          CALL oasis_abort()
       ENDIF

       !------------------------------------------------
       !>   * For put fields that need to read a restart file because of a lag, 
       !>     call oasis_advance_run and read in the restart file.
       !>     Set readrest to true in oasis_advance_run to indicate it's called from init.
       ! right now, do not know whether any hot map terms are there
       ! assume they are if something is read, otherwise not
       !------------------------------------------------
       IF ( (getput == OASIS3_PUT .AND. lag > 0) ) THEN
          msec=0-lag
          lsize = mct_aVect_lsize(pcpointer%aVect1)
          nflds = mct_aVect_nRAttr(pcpointer%aVect1)

          ALLOCATE(array(lsize))
          ALLOCATE(array2(lsize))
          ALLOCATE(array3(lsize))
          ALLOCATE(array4(lsize))
          ALLOCATE(array5(lsize))

          DO nf = 1,nflds
             varid = pcpointer%varid(nf)
             CALL oasis_advance_run(OASIS_Out,varid,msec,kinfo,nff=nf, &
                                 namid=cplid,array1din=array,&
                                 readrest=.TRUE., array2=array2,array3=array3, &
                                 array4=array4,array5=array5)
          ENDDO
          IF (OASIS_Debug >= 2) THEN
             write(nulprt,*) subname,' advance_run ',cplid,TRIM(pcpointer%fldlist)
          endif

          DEALLOCATE(array)
          DEALLOCATE(array2)
          DEALLOCATE(array3)
          DEALLOCATE(array4)
          DEALLOCATE(array5)
       ENDIF  ! put
    ENDIF ! valid
    enddo ! npc
    ENDDO ! cplid

    !----------------------------------------------------------------
    !> * Loop over all coupler connections, 
    !>   Loop over get and put connections,
    !>   For valid connections
    !----------------------------------------------------------------

    DO cplid=1, prism_mcoupler
    DO npc = 1,2
    if (npc == 1) pcpointer => prism_coupler_put(cplid)
    if (npc == 2) pcpointer => prism_coupler_get(cplid)
    IF (pcpointer%valid) then
       dt    = pcpointer%dt
       lag   = pcpointer%lag
       ltime = pcpointer%ltime
       getput= pcpointer%getput
       rstfile=TRIM(pcpointer%rstfile)
       partid= pcpointer%partID
       msec = 0   ! reasonable default to start with
       mseclag = msec
      
       IF (OASIS_Debug >= 2) THEN
           WRITE(nulprt,*) '----------------------------------------------------------------'
           WRITE(nulprt,*) subname,' Field cplid :',cplid
           WRITE(nulprt,*) subname,' loctrans:',npc,TRIM(pcpointer%fldlist)
           WRITE(nulprt,*) '----------------------------------------------------------------'
           CALL oasis_flush(nulprt)
       ENDIF

       !------------------------------------------------
       !>   * Read restart for LOCTRANS fields.
       !>     Do after restart and advance above because prism_advance_run
       !>     fills in the avect with the array info
       !------------------------------------------------

       call oasis_debug_note(subname//' check for loctrans restart')
       IF (getput == OASIS3_PUT .AND. pcpointer%trans /= ip_instant) THEN
          if (len_trim(rstfile) < 1) then
             write(nulprt,*) subname,estr,'restart undefined'
             call oasis_abort()
          endif
          if (OASIS_debug >= 2) then
             write(nulprt,*) subname,' at ',msec,mseclag,' RTRN: ',&
                trim(pcpointer%fldlist),' ',trim(rstfile)
          endif
          lsize = mct_aVect_lsize(pcpointer%aVect1)

          write(vstring,'(a,i6.6,a)') 'loc',pcpointer%namID,'_cnt'
          call oasis_io_read_array(rstfile,prism_part(partid)%mpicom,iarray=pcpointer%avcnt,&
                                   ivarname=trim(vstring),abort=.false.)

          write(vstring,'(a,i6.6,a)') 'loc',pcpointer%namID,'_'
          call oasis_io_read_avfile(rstfile,pcpointer%avect1,&
                                    prism_part(partid)%pgsmap,prism_part(partid)%mpicom, &
                                    abort=.false.,nampre=trim(vstring))

          call mct_aVect_init(pcpointer%aVect2,pcpointer%aVect1,lsize)
          call mct_aVect_zero(pcpointer%aVect2)
          write(vstring,'(a,i6.6,a)') 'av2loc',pcpointer%namID,'_'
          call oasis_io_read_avfile(rstfile,pcpointer%avect2,&
                                    prism_part(partid)%pgsmap,prism_part(partid)%mpicom, &
                                    abort=.false.,nampre=trim(vstring),&
                                    didread=pcpointer%aVon(2))
          if (.not. pcpointer%aVon(2)) then
             call mct_aVect_clean(pcpointer%avect2)
          endif

          call mct_aVect_init(pcpointer%aVect3,pcpointer%aVect1,lsize)
          call mct_aVect_zero(pcpointer%aVect3)
          write(vstring,'(a,i6.6,a)') 'av3loc',pcpointer%namID,'_'
          call oasis_io_read_avfile(rstfile,pcpointer%avect3,&
                                    prism_part(partid)%pgsmap,prism_part(partid)%mpicom, &
                                    abort=.false.,nampre=trim(vstring),&
                                    didread=pcpointer%aVon(3))
          if (.not. pcpointer%aVon(3)) then
             call mct_aVect_clean(pcpointer%avect3)
          endif

          call mct_aVect_init(pcpointer%aVect4,pcpointer%aVect1,lsize)
          call mct_aVect_zero(pcpointer%aVect4)
          write(vstring,'(a,i6.6,a)') 'av4loc',pcpointer%namID,'_'
          call oasis_io_read_avfile(rstfile,pcpointer%avect4,&
                                    prism_part(partid)%pgsmap,prism_part(partid)%mpicom, &
                                    abort=.false.,nampre=trim(vstring),&
                                    didread=pcpointer%aVon(4))
          if (.not. pcpointer%aVon(4)) then
             call mct_aVect_clean(pcpointer%avect4)
          endif

          call mct_aVect_init(pcpointer%aVect5,pcpointer%aVect1,lsize)
          call mct_aVect_zero(pcpointer%aVect5)
          write(vstring,'(a,i6.6,a)') 'av5loc',pcpointer%namID,'_'
          call oasis_io_read_avfile(rstfile,pcpointer%avect5,&
                                    prism_part(partid)%pgsmap,prism_part(partid)%mpicom, &
                                    abort=.false.,nampre=trim(vstring),&
                                    didread=pcpointer%aVon(5))
          if (.not. pcpointer%aVon(5)) then
             call mct_aVect_clean(pcpointer%avect5)
          endif

          if (OASIS_debug >= 20) then
             write(nulprt,*) subname,'  DEBUG read loctrans restart',&
                             cplid,pcpointer%avcnt
             write(nulprt,*) subname,'  DEBUG read loctrans restart',cplid,&
                             minval(pcpointer%avect1%rAttr),&
                             maxval(pcpointer%avect1%rAttr)
          endif
       endif
    ENDIF  ! valid
    enddo  ! npc
    ENDDO  ! cplid
    IF (local_timers_on) call oasis_timer_stop ('advance_init')

    call oasis_debug_exit(subname)

  end SUBROUTINE oasis_advance_init
!---------------------------------------------------------------------

!> Advances the OASIS coupling

!> Only one from array1din, array1dout, or array2dout can be passed in.
!> readrest is set to true when called by the oasis_advance_init method.
!> Arrays 2 to 5 are for the higher order terms (hot)

  SUBROUTINE oasis_advance_run(mop,varid,msec,kinfo,nff,namid,&
             array1din,array1dout,array2dout,readrest,&
             a2on,array2,a3on,array3,a4on,array4,a5on,array5)

    IMPLICIT none
!   ----------------------------------------------------------------
    integer(kind=ip_i4_p), intent(in)    :: mop      !< OASIS_Out or OASIS_In
    INTEGER(kind=ip_i4_p), intent(in)    :: varid    !< prism_var id
    INTEGER(kind=ip_i4_p), intent(in)    :: msec     !< model time
    INTEGER(kind=ip_i4_p), intent(inout) :: kinfo    !< status

    INTEGER(kind=ip_i4_p), optional :: nff           !< specify particular field for restart
    INTEGER(kind=ip_i4_p), optional :: namid         !< only do this namcouple
                                                     !< method for restart
    REAL   (kind=ip_r8_p), optional :: array1din(:)  !< 1D put data
    REAL   (kind=ip_r8_p), optional :: array1dout(:) !< 1D get data
    REAL   (kind=ip_r8_p), optional :: array2dout(:,:) !< 2D get data
    logical              , optional :: readrest      !< special flag to indicate this 
                                                     !< is called from the advance_init
    logical              , optional :: a2on      !< logical for array2
    REAL   (kind=ip_r8_p), optional :: array2(:) !< hot put data
    logical              , optional :: a3on      !< logical for array3
    REAL   (kind=ip_r8_p), optional :: array3(:) !< hot put data
    logical              , optional :: a4on      !< logical for array4
    REAL   (kind=ip_r8_p), optional :: array4(:) !< hot put data
    logical              , optional :: a5on      !< logical for array5
    REAL   (kind=ip_r8_p), optional :: array5(:) !< hot put data
!   ----------------------------------------------------------------
    character(len=ic_lvar):: vname
    INTEGER(kind=ip_i4_p) :: cplid,rouid,mapid,partid
    INTEGER(kind=ip_i4_p) :: nfav,nsav,nsa,n,nc,nf,npc
    INTEGER(kind=ip_i4_p) :: lsize,nflds
    integer(kind=ip_i4_p) :: tag,dt,ltime,lag,getput,maxtime,conserv
    logical               :: consbfb
    logical               :: sndrcv,output,input,unpack
    logical               :: snddiag,rcvdiag
    logical               :: arrayon(prism_coupler_avsmax)
    LOGICAL               :: a11on,a22on,a33on,a44on,a55on
    real(kind=ip_double_p):: sndmult,sndadd,rcvmult,rcvadd
    character(len=ic_xl)  :: rstfile   ! restart filename
    character(len=ic_xl)  :: inpfile   ! input filename
    integer(kind=ip_i4_p) :: nx,ny
    integer(kind=ip_i4_p) :: mseclag   ! model time + lag
    real(kind=ip_r8_p)    :: rcnt      ! 1./cnt
    character(len=ic_med) :: tstring   ! timer label string
    character(len=ic_med) :: fstring   ! output file string
    character(len=ic_med) :: cstring   ! temporary string
    character(len=ic_med) :: vstring   ! temporary string
    logical               :: comm_now  ! time to communicate
    logical               :: time_now  ! coupling time
    logical               :: lreadrest ! local readrest
    logical               :: runit     ! advance the variable
    TYPE(mct_avect)       :: avtest    ! temporary
    type(mct_avect)       :: avtmp   ! data read from restart
    type(mct_avect)       :: avtmp2  ! data read from restart
    type(mct_avect)       :: avtmp3  ! data read from restart
    type(mct_avect)       :: avtmp4  ! data read from restart
    type(mct_avect)       :: avtmp5  ! data read from restart
    type(prism_coupler_type),pointer :: pcpointer
    type(prism_coupler_type),pointer :: pcpointmp
    logical, parameter    :: allow_no_restart = .false.
!------------------------------------------------------------------
! use this to set restart fields to zero if no restart file exists.
! by default, code will abort if restart file is missing.  this
! logical avoids that error and allows the run to continue.
!    logical, parameter    :: allow_no_restart = .true.
!------------------------------------------------------------------
    character(len=*),parameter :: subname = '(oasis_advance_run)'
    character(len=*),parameter :: F01 = '(a,i3.3)'
!   ----------------------------------------------------------------

    call oasis_debug_enter(subname)

    !----------------------------------------------------------------
    !> oasis_advance_run does the following
    !> * Aborts if it's called from non-active tasks
    !----------------------------------------------------------------

    if (mpi_comm_local == MPI_COMM_NULL) then
       write(nulprt,*) subname,estr,'called on non coupling task'
       call oasis_abort()
    endif

    kinfo = OASIS_OK
    if (varid < 1 .or. varid > prism_nvar) then
       write(nulprt,*) subname,estr,'invalid varid',varid
       call oasis_abort()
    endif
    vname = prism_var(varid)%name

    lreadrest = .false.
    if (present(readrest)) then
       lreadrest = readrest
    endif
    if (lreadrest) kinfo = OASIS_fromrest

    !------------------------------------------------
    !> * Verify field (var) is either In or Out
    !------------------------------------------------

    if (mop /= OASIS_Out .and. mop /= OASIS_In) then
       write(nulprt,*) subname,estr,'at ',msec,mseclag,' for var = ',trim(vname)
       write(nulprt,*) subname,estr,'mop invalid expecting OASIS_Out or OASIS_In = ',mop
       call oasis_abort()
    endif

    call oasis_debug_note(subname//' loop over var ncpl')

    !------------------------------------------------
    ! Check namid
    !------------------------------------------------

    if (present(namid)) then
       IF (OASIS_debug >= 20) THEN
          write(nulprt,*) subname,'namid',namid,prism_var(varid)%cpl(1:prism_var(varid)%ncpl)
       endif
       ! verify namid is associated with the variable or abort
       runit = .false.
       do nc = 1,prism_var(varid)%ncpl
          cplid   = prism_var(varid)%cpl(nc)
          if (cplid == namid) runit = .true.
       enddo
       if (.not.runit) then
          WRITE(nulprt,*) subname,estr,'namid not found for var = ',trim(vname)
          WRITE(nulprt,*) subname,estr,'namid = ',namid
          CALL oasis_abort()
       endif
    endif

    !------------------------------------------------
    !> * Loop over all the couplers associated with this var
    !  or if nam is present, just for nc == nam
    !------------------------------------------------

    DO nc = 1,prism_var(varid)%ncpl
     cplid   = prism_var(varid)%cpl(nc)
     runit = .true.
     if (present(namid)) then
        runit = .false.
        if (cplid == namid) runit = .true.
     endif
     if (runit) then
       cplid   = prism_var(varid)%cpl(nc)
       if (mop == OASIS_Out) pcpointer => prism_coupler_put(cplid)
       if (mop == OASIS_In ) pcpointer => prism_coupler_get(cplid)

       !------------------------------------------------
       !>   * check this prism_coupler is valid
       !------------------------------------------------
       if (.not.pcpointer%valid) then
          WRITE(nulprt,*) subname,estr,'invalid prism_coupler for var = ',trim(vname)
          CALL oasis_abort()
       endif

       !------------------------------------------------
       !>   *  check again that model op matches coupler op
       !------------------------------------------------
       getput  = pcpointer%getput
       if ((mop == OASIS_Out .and. getput == OASIS3_PUT) .or. &
           (mop == OASIS_In  .and. getput == OASIS3_GET)) then
          !-continue
       else
          write(nulprt,*) subname,estr,'model def_var in-out does not match model get-put call for var = ',trim(vname)
          call oasis_abort()
       endif

       !------------------------------------------------
       !>   *  set a bunch of local variables
       !------------------------------------------------
       rouid   = pcpointer%routerid
       mapid   = pcpointer%mapperid
       tag     = pcpointer%tag
       dt      = pcpointer%dt
       lag     = pcpointer%lag
       ltime   = pcpointer%ltime
       sndrcv  = pcpointer%sndrcv
       rstfile = TRIM(pcpointer%rstfile)
       inpfile = TRIM(pcpointer%inpfile)
       maxtime = pcpointer%maxtime
       output  = pcpointer%output
       input   = pcpointer%input
       partid  = pcpointer%partID
       conserv = pcpointer%conserv
       consbfb = .TRUE.
       IF (TRIM(pcpointer%consopt) == "opt") consbfb = .FALSE.
       snddiag = pcpointer%snddiag
       rcvdiag = pcpointer%rcvdiag
       sndadd  = pcpointer%sndadd
       sndmult = pcpointer%sndmult
       rcvadd  = pcpointer%rcvadd
       rcvmult = pcpointer%rcvmult
      
       unpack = (sndrcv .OR. input)
      
       CALL oasis_debug_note(subname//' set nx and ny')
       IF (prism_part(partid)%nx >= 1) THEN
          nx = prism_part(partid)%nx
          ny = prism_part(partid)%ny
       ELSE
          nx = prism_part(partid)%gsize
          ny = 1
       ENDIF
      
       IF (OASIS_debug >= 20) THEN
          WRITE(nulprt,*) subname,'  DEBUG nx, ny = ',nx,ny
          CALL oasis_flush(nulprt)
       ENDIF

       !------------------------------------------------
       !>   * check that lag is reasonable
       !------------------------------------------------

       IF (ABS(lag) > dt) THEN
          WRITE(nulprt,*) subname,estr,'lag setting greater than dt for var = ',trim(vname)
          CALL oasis_abort()
       ENDIF

       !------------------------------------------------
       !>   *  read restart for call from init phase
       ! right now, do not know whether any hot map terms are there
       ! assume they are if something is read, otherwise not
       !------------------------------------------------

       call oasis_debug_note(subname//' check for lag restart')
       IF (getput == OASIS3_PUT .AND. lag > 0 .AND. lreadrest) THEN
       ! effective model time of restart : msec
           mseclag = msec + lag
           IF (.not.present(nff)) THEN
               WRITE(nulprt,*) subname,estr,'nff optional argument not passed but expected for var = ',trim(vname)
               CALL oasis_abort()
           ENDIF
           IF (LEN_TRIM(rstfile) < 1) THEN
               WRITE(nulprt,*) subname,estr,'restart file undefined for var = ',trim(vname)
               CALL oasis_abort()
           ENDIF
           lsize = mct_aVect_lsize(pcpointer%aVect1)
           IF (OASIS_debug >= 2) THEN
               WRITE(nulprt,*) subname,' at ',msec,mseclag,' RRST: ',&
                  TRIM(pcpointer%fldlist),' ',TRIM(rstfile)
           ENDIF
           CALL mct_aVect_init(avtmp,rlist=pcpointer%fldlist,lsize=lsize)
           if (allow_no_restart) then
              avtmp%rAttr(nff,1:lsize) = 0.0
              CALL oasis_io_read_avfile(TRIM(rstfile),avtmp,prism_part(partid)%pgsmap,prism_part(partid)%mpicom, &
                                        abort=.false.,didread=a11on)
           else
              CALL oasis_io_read_avfile(TRIM(rstfile),avtmp,prism_part(partid)%pgsmap,prism_part(partid)%mpicom)
              a11on = .true.
           endif

           CALL mct_aVect_init(avtmp2,rlist=pcpointer%fldlist,lsize=lsize)
           CALL oasis_io_read_avfile(TRIM(rstfile),avtmp2,prism_part(partid)%pgsmap,prism_part(partid)%mpicom, &
                                     abort=.FALSE.,nampre='av2_',didread=a22on)
           
           CALL mct_aVect_init(avtmp3,rlist=pcpointer%fldlist,lsize=lsize)
           CALL oasis_io_read_avfile(TRIM(rstfile),avtmp3,prism_part(partid)%pgsmap,prism_part(partid)%mpicom, &
                                    abort=.FALSE.,nampre='av3_',didread=a33on)

           CALL mct_aVect_init(avtmp4,rlist=pcpointer%fldlist,lsize=lsize)
           CALL oasis_io_read_avfile(TRIM(rstfile),avtmp4,prism_part(partid)%pgsmap,prism_part(partid)%mpicom, &
                                     abort=.FALSE.,nampre='av4_',didread=a44on)

           CALL mct_aVect_init(avtmp5,rlist=pcpointer%fldlist,lsize=lsize)
           CALL oasis_io_read_avfile(TRIM(rstfile),avtmp5,prism_part(partid)%pgsmap,prism_part(partid)%mpicom, &
                                    abort=.false.,nampre='av5_',didread=a55on)

           array1din(1:lsize) = avtmp%rAttr(nff,1:lsize)
           IF (a22on) array2(1:lsize) = avtmp2%rAttr(nff,1:lsize)
           IF (a33on) array3(1:lsize) = avtmp3%rAttr(nff,1:lsize)
           IF (a44on) array4(1:lsize) = avtmp4%rAttr(nff,1:lsize)
           IF (a55on) array5(1:lsize) = avtmp5%rAttr(nff,1:lsize)

           CALL mct_avect_clean(avtmp)
           IF (a22on) CALL mct_avect_clean(avtmp2)
           IF (a33on) CALL mct_avect_clean(avtmp3)
           IF (a44on) CALL mct_avect_clean(avtmp4)
           IF (a55on) CALL mct_avect_clean(avtmp5)
       ENDIF

       !------------------------------------------------
       !>   * compute lag time, only on put side
       !>   * set time_now, is it a coupling period?
       !------------------------------------------------

       call oasis_debug_note(subname//' set mseclag')
       if (getput == OASIS3_PUT) then
          mseclag = msec + lag
       elseif (getput == OASIS3_GET) then
          mseclag = msec
       endif

       if (OASIS_debug >= 20) then
          write(nulprt,*) subname,'  DEBUG msec,mseclag = ',msec,mseclag
          CALL oasis_flush(nulprt)
       endif

       time_now = .false.
       if (mod(mseclag,dt) == 0) time_now = .true.

       !------------------------------------------------
       !>   * check that model hasn't gone past maxtime
       !------------------------------------------------

       if (msec >= maxtime) then
          write(nulprt,*) subname,estr,'at ',msec,mseclag,' for var = ',trim(vname)
          write(nulprt,*) subname,estr,'model time beyond namcouple maxtime = ',msec,maxtime
          call oasis_abort()
       endif

       !------------------------------------------------
       !>   * check that model isn't going backwards
       ! msec >= 0 does the check only in run mode, not in initialization
       !------------------------------------------------

       if (lcouplertime /= ispval .and. msec >= 0 .and. msec < lcouplertime) then
          write(nulprt,*) subname,estr,'at ',msec,mseclag,' for var = ',trim(vname)
          write(nulprt,*) subname,estr,'model seems to be running backwards = ',lcouplertime
          call oasis_abort()
       endif

       !------------------------------------------------
       !>   * check that variable didn't miss a coupling period 
       ! check only for send recv operations where deadlock 
       ! is possible
       !------------------------------------------------

       do n = 1,prism_mcoupler
       do npc = 1,2
       if (npc == 1) pcpointmp => prism_coupler_put(n)
       if (npc == 2) pcpointmp => prism_coupler_get(n)
       if (pcpointmp%valid) then
          if ((pcpointmp%ltime /= ispval) .and. &
              (sndrcv .and. pcpointmp%sndrcv) .and. &
              (msec >  pcpointmp%ltime + pcpointmp%dt)) then
             write(nulprt,*) subname,estr,'coupling skipped at earlier time, potential deadlock '
             write(nulprt,*) subname,estr,'my coupler = ',cplid,' variable = ',&
                             trim(pcpointer%fldlist)
             write(nulprt,*) subname,estr,'current time = ',msec,' mseclag = ',mseclag
             write(nulprt,*) subname,estr,'skipped coupler = ',n,' variable = ',&
                             trim(pcpointmp%fldlist)
             write(nulprt,*) subname,estr,'skipped coupler last time and dt = ',&
                             pcpointmp%ltime,pcpointmp%dt
             WRITE(nulprt,*) subname,estr,'model timestep does not match coupling timestep'
             call oasis_abort()
          endif
       endif  ! valid
       enddo  ! npc
       enddo  ! prism_mcoupler

       !------------------------------------------------
       !>   * check that prior sequences weren't missed at this 
       !>     step for get (recv) operation.
       ! attempts to trap deadlocks before they happen
       !------------------------------------------------

       if (sndrcv .and. getput == OASIS3_GET) then
          if (lastseqtime /= ispval .and. msec == lastseqtime  .and. pcpointer%seq < lastseq) then
             write(nulprt,*) subname,estr,'coupling sequence out of order, potential deadlock '
             write(nulprt,*) subname,estr,'my coupler = ',cplid,' variable = ',&
                             trim(pcpointer%fldlist)
             write(nulprt,*) subname,' ERRRO: sequence number = ',pcpointer%seq
             write(nulprt,*) subname,estr,'current time = ',msec,' mseclag = ',mseclag
             write(nulprt,*) subname,estr,'last sequence and time = ',lastseq,lastseqtime
             WRITE(nulprt,*) subname,estr,'model sequence does not match coupling sequence'
             call oasis_abort()
          else
             lastseq = pcpointer%seq
             lastseqtime = msec
          endif
          if (OASIS_debug >= 20) then
             write(nulprt,*) subname,' DEBUG sequence ',trim(vname),msec,lastseqtime,lastseq,pcpointer%seq
          endif
       endif

       !------------------------------------------------
       !>   * compute field index and check sizes
       !------------------------------------------------

       call oasis_debug_note(subname//' compute field index and sizes')
       nfav = mct_avect_indexra(pcpointer%avect1,trim(vname))
       nsav = mct_avect_lsize(pcpointer%avect1)
       if (lag > 0 .and. lreadrest) nsa=size(array1din)
       if (present(array1din )) nsa = size(array1din )
       if (present(array1dout)) nsa = size(array1dout)
       if (present(array2dout)) nsa = size(array2dout)

       if (OASIS_debug >= 20) then
          write(nulprt,*) subname,'  DEBUG nfav,nsav,nsa = ',nfav,nsav,nsa
       endif

       if (nsav /= nsa) then
          write(nulprt,*) subname,estr,'at ',msec,mseclag,' for var = ',trim(vname)
          write(nulprt,*) subname,estr,'in field size passed into get/put compare to expected size ',nsav,nsa
          call oasis_abort()
       endif

       !------------------------------------------------
       !>   * check for higher order coupling fields
       !>     and get everything ready
       ! arrayon is what's passed this time
       ! optional args only on put side
       !------------------------------------------------

       IF (.not.lreadrest) THEN
       arrayon = .false.
       arrayon(1) = .true.
       if (present(a2on)) arrayon(2) = a2on
       if (present(a3on)) arrayon(3) = a3on
       if (present(a4on)) arrayon(4) = a4on
       if (present(a5on)) arrayon(5) = a5on

       if ((getput == OASIS3_GET) .or. &
           (getput == OASIS3_PUT .and. trim(pcpointer%maploc) == "dst" )) then
          if (arrayon(2) .or. arrayon(3) .or. &
              arrayon(4) .or. arrayon(5)) then
             write(nulprt,*) subname,estr,'at ',msec,mseclag,' for var = ',trim(vname)
             write(nulprt,*) subname,estr,'higher order mapping not allowed on get side'
             write(nulprt,*) subname,estr,'consider changing map location from dst to src'
             call oasis_abort()
          endif
       endif

       if ((arrayon(2) .and. .not.present(array2)) .or. &
           (arrayon(3) .and. .not.present(array3)) .or. &
           (arrayon(4) .and. .not.present(array4)) .or. &
           (arrayon(5) .and. .not.present(array5))) then
          write(nulprt,*) subname,estr,'at ',msec,mseclag,' for var = ',trim(vname)
          write(nulprt,*) subname,estr,'arrayon true but array not sent'
          call oasis_abort()
       ! With the current way of using oasis_advance_run, the above test is useless but we keep the test
       ! as someone might be later adding an interface call that would violate the consistency
       endif

       ! initialize aVect2-5 here if not already allocated

       if (arrayon(2) .and. .not. pcpointer%aVon(2)) then
          call mct_aVect_init(pcpointer%aVect2,pcpointer%aVect1,nsav)
          call mct_aVect_zero(pcpointer%aVect2)
          pcpointer%aVon(2) = .true.
          if (OASIS_debug >= 2) then
             write(nulprt,*) subname,' at ',msec,mseclag,' ALLO: ',&
                             trim(vname),' ','aVect2'
          endif
       endif

       if (arrayon(3) .and. .not. pcpointer%aVon(3)) then
          call mct_aVect_init(pcpointer%aVect3,pcpointer%aVect1,nsav)
          call mct_aVect_zero(pcpointer%aVect3)
          pcpointer%aVon(3) = .true.
          if (OASIS_debug >= 2) then
             write(nulprt,*) subname,' at ',msec,mseclag,' ALLO: ',&
                             trim(vname),' ','aVect3'
          endif
       endif

       if (arrayon(4) .and. .not. pcpointer%aVon(4)) then
          call mct_aVect_init(pcpointer%aVect4,pcpointer%aVect1,nsav)
          call mct_aVect_zero(pcpointer%aVect4)
          pcpointer%aVon(4) = .true.
          if (OASIS_debug >= 2) then
             write(nulprt,*) subname,' at ',msec,mseclag,' ALLO: ',&
                             trim(vname),' ','aVect4'
          endif
       endif

       if (arrayon(5) .and. .not. pcpointer%aVon(5)) then
          call mct_aVect_init(pcpointer%aVect5,pcpointer%aVect1,nsav)
          call mct_aVect_zero(pcpointer%aVect5)
          pcpointer%aVon(5) = .true.
          if (OASIS_debug >= 2) then
             write(nulprt,*) subname,' at ',msec,mseclag,' ALLO: ',&
                             trim(vname),' ','aVect5'
          endif
       endif
       endif  ! .not. lreadrest

       !------------------------------------------------
       !>   * update avect1-5 on put side and apply appropriate transform
       !>   * if its coupling time, set status of this var to ready
       ! on restart, treat as instant value
       !------------------------------------------------

       if (getput == OASIS3_PUT) then

          call oasis_debug_note(subname//' loctrans operation')
          write(tstring,F01) 'pcpy_',cplid
          call oasis_timer_start(tstring)

          cstring = 'none'
          if (lreadrest .or. pcpointer%trans == ip_instant) then
             if (time_now) then
                cstring = 'instant'
                do n = 1,nsav
                   pcpointer%avect1%rAttr(nfav,n) = array1din(n)
                   if (pcpointer%aVon(2)) then
                      if (present(array2)) then
                         pcpointer%avect2%rAttr(nfav,n) = array2(n)
                      else
                         pcpointer%avect2%rAttr(nfav,n) = 0.0
                      endif
                   endif
                   if (pcpointer%aVon(3)) then
                      if (present(array3)) then
                         pcpointer%avect3%rAttr(nfav,n) = array3(n)
                      else
                         pcpointer%avect3%rAttr(nfav,n) = 0.0
                      endif
                   endif
                   if (pcpointer%aVon(4)) then
                      if (present(array4)) then
                         pcpointer%avect4%rAttr(nfav,n) = array4(n)
                      else
                         pcpointer%avect4%rAttr(nfav,n) = 0.0
                      endif
                   endif
                   if (pcpointer%aVon(5)) then
                      if (present(array5)) then
                         pcpointer%avect5%rAttr(nfav,n) = array5(n)
                      else
                         pcpointer%avect5%rAttr(nfav,n) = 0.0
                      endif
                   endif
                enddo
                pcpointer%avcnt(nfav) = 1
             endif

          elseif (pcpointer%trans == ip_average) then
             cstring = 'average'
             if (kinfo == OASIS_OK) kinfo = OASIS_LocTrans
             do n = 1,nsav
                pcpointer%avect1%rAttr(nfav,n) = &
                   pcpointer%avect1%rAttr(nfav,n) + array1din(n)
                if (pcpointer%aVon(2)) then
                   if (present(array2)) then
                      pcpointer%avect2%rAttr(nfav,n) = &
                         pcpointer%avect2%rAttr(nfav,n) + array2(n)
                   endif
                endif
                if (pcpointer%aVon(3)) then
                   if (present(array3)) then
                      pcpointer%avect3%rAttr(nfav,n) = &
                         pcpointer%avect3%rAttr(nfav,n) + array3(n)
                   endif
                endif
                if (pcpointer%aVon(4)) then
                   if (present(array4)) then
                      pcpointer%avect4%rAttr(nfav,n) = &
                         pcpointer%avect4%rAttr(nfav,n) + array4(n)
                   endif
                endif
                if (pcpointer%aVon(5)) then
                   if (present(array5)) then
                      pcpointer%avect5%rAttr(nfav,n) = &
                         pcpointer%avect5%rAttr(nfav,n) + array5(n)
                   endif
                endif
             enddo
             pcpointer%avcnt(nfav) = pcpointer%avcnt(nfav) + 1

          elseif (pcpointer%trans == ip_accumul) then
             cstring = 'accumul'
             if (kinfo == OASIS_OK) kinfo = OASIS_LocTrans
             do n = 1,nsav
                pcpointer%avect1%rAttr(nfav,n) = &
                   pcpointer%avect1%rAttr(nfav,n) + array1din(n)
                if (pcpointer%aVon(2)) then
                   if (present(array2)) then
                      pcpointer%avect2%rAttr(nfav,n) = &
                         pcpointer%avect2%rAttr(nfav,n) + array2(n)
                   endif
                endif
                if (pcpointer%aVon(3)) then
                   if (present(array3)) then
                      pcpointer%avect3%rAttr(nfav,n) = &
                         pcpointer%avect3%rAttr(nfav,n) + array3(n)
                   endif
                endif
                if (pcpointer%aVon(4)) then
                   if (present(array4)) then
                      pcpointer%avect4%rAttr(nfav,n) = &
                         pcpointer%avect4%rAttr(nfav,n) + array4(n)
                   endif
                endif
                if (pcpointer%aVon(5)) then
                   if (present(array5)) then
                      pcpointer%avect5%rAttr(nfav,n) = &
                         pcpointer%avect5%rAttr(nfav,n) + array5(n)
                   endif
                endif
             enddo
             pcpointer%avcnt(nfav) = 1

          elseif (pcpointer%trans == ip_max) then
             cstring = 'max'
             if (kinfo == OASIS_OK) kinfo = OASIS_LocTrans
             if (pcpointer%aVon(2) .or. pcpointer%aVon(3) .or. &
                 pcpointer%aVon(4) .or. pcpointer%aVon(5)) then
                write(nulprt,*) subname,estr,'at ',msec,mseclag,' for var = ',trim(vname)
                write(nulprt,*) subname,estr,'higher order mapping with MAX transform not supported'
                call oasis_abort()      
             endif
             do n = 1,nsav
                if (pcpointer%avcnt(nfav) == 0) then
                   pcpointer%avect1%rAttr(nfav,n) = array1din(n)
                else
                   pcpointer%avect1%rAttr(nfav,n) = &
                      max(pcpointer%avect1%rAttr(nfav,n),array1din(n))
                endif
             enddo
             pcpointer%avcnt(nfav) = 1

          elseif (pcpointer%trans == ip_min) then
             cstring = 'min'
             if (kinfo == OASIS_OK) kinfo = OASIS_LocTrans
             if (pcpointer%aVon(2) .or. pcpointer%aVon(3) .or. &
                 pcpointer%aVon(4) .or. pcpointer%aVon(5)) then
                write(nulprt,*) subname,estr,'at ',msec,mseclag,' for var = ',trim(vname)
                write(nulprt,*) subname,estr,'higher order mapping with MIN transform not supported'
                call oasis_abort()      
             endif
             do n = 1,nsav
                if (pcpointer%avcnt(nfav) == 0) then
                   pcpointer%avect1%rAttr(nfav,n) = array1din(n)
                else
                   pcpointer%avect1%rAttr(nfav,n) = &
                      min(pcpointer%avect1%rAttr(nfav,n),array1din(n))
                endif
             enddo
             pcpointer%avcnt(nfav) = 1

          else
             write(nulprt,*) subname,estr,'transform not known for var = ',trim(vname),pcpointer%trans
             call oasis_abort()
          endif
          call oasis_timer_stop(tstring)

          if (OASIS_debug >= 2 .and. trim(cstring) /= 'none') then
             write(nulprt,*) subname,' at ',msec,mseclag,' PACK: ',&
                             trim(vname),' ',trim(cstring)
          endif

          if (OASIS_debug >= 20) then
             write(nulprt,*) subname,'  DEBUG loctrans update ',cplid,' ',&
             trim(cstring),pcpointer%avcnt(nfav)
          endif

          if (time_now) then
             pcpointer%status(nfav) = OASIS_COMM_READY
          endif
       endif

       !------------------------------------------------
       !>   * decide if it's time to communicate based on time
       ! also, on the put side, status of all vars
       ! must be ready which means all vars have called put.
       ! on get side, all ready means all vars have unpacked
       ! from last get.
       !------------------------------------------------

       call oasis_debug_note(subname//' comm_now compute')
       comm_now = .false.
       if (time_now) then
          comm_now = .true.
          do nf = 1,pcpointer%nflds
             if (pcpointer%status(nf) /= OASIS_COMM_READY) then
                comm_now = .false.
                if (OASIS_debug >= 15) then
                   write(nulprt,*) subname,' at ',msec,mseclag,' STAT: ',nf,' NOT READY'
                endif
                 kinfo=OASIS_Waitgroup
             else
                if (OASIS_debug >= 15) then
                   write(nulprt,*) subname,' at ',msec,mseclag,' STAT: ',nf,' READY'
                endif
             endif
          enddo
       endif

       !------------------------------------------------
       !>   * If it's time to communicate
       !------------------------------------------------

       if (comm_now) then

          call oasis_debug_note(subname//' comm_now')

          !------------------------------------------------
          !>     * check again that time is correct
          ! this is the time critical bit, we need to make sure the
          ! model is truly advancing in time when comms are called.
          ! must ignore the initial call, ltime = 0
          !------------------------------------------------

          if (pcpointer%ltime /= ispval .and. msec <= pcpointer%ltime) then
             write(nulprt,*) subname,estr,'model did not advance in time correctly for var = ',trim(vname)
             write(nulprt,*) subname,estr,'msec, ltime = ',msec,pcpointer%ltime
             call oasis_abort()
          endif

          !------------------------------------------------
          !>     * average as needed for some transforms
          ! (not cache friendly yet)
          !------------------------------------------------

          IF (getput == OASIS3_PUT) THEN
             call oasis_debug_note(subname//' loctrans calc')
             write(tstring,F01) 'pavg_',cplid
             call oasis_timer_start(tstring)
             do nf = 1,pcpointer%nflds
                if (pcpointer%avcnt(nf) > 1) then
                   rcnt = 1.0/pcpointer%avcnt(nf)
                   do n = 1,nsav
                      pcpointer%avect1%rAttr(nf,n) = &
                         pcpointer%avect1%rAttr(nf,n) * rcnt
                      if (pcpointer%aVon(2)) then
                         pcpointer%avect2%rAttr(nf,n) = &
                            pcpointer%avect2%rAttr(nf,n) * rcnt
                      endif
                      if (pcpointer%aVon(3)) then
                         pcpointer%avect3%rAttr(nf,n) = &
                            pcpointer%avect3%rAttr(nf,n) * rcnt
                      endif
                      if (pcpointer%aVon(4)) then
                         pcpointer%avect4%rAttr(nf,n) = &
                            pcpointer%avect4%rAttr(nf,n) * rcnt
                      endif
                      if (pcpointer%aVon(5)) then
                         pcpointer%avect5%rAttr(nf,n) = &
                            pcpointer%avect5%rAttr(nf,n) * rcnt
                      endif
                   enddo             
                endif
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG loctrans calc0 = ',cplid,nf,&
                                   pcpointer%avcnt(nf)
                   write(nulprt,*) subname,'  DEBUG loctrans calc1 = ',cplid,nf,&
                                   minval(pcpointer%avect1%rAttr(nf,:)),&
                                   maxval(pcpointer%avect1%rAttr(nf,:))
                   call oasis_flush(nulprt)
                   if (pcpointer%aVon(2)) &
                   write(nulprt,*) subname,'  DEBUG loctrans calc2 = ',cplid,nf,&
                                   minval(pcpointer%avect2%rAttr(nf,:)),&
                                   maxval(pcpointer%avect2%rAttr(nf,:))
                   if (pcpointer%aVon(3)) &
                   write(nulprt,*) subname,'  DEBUG loctrans calc3 = ',cplid,nf,&
                                   minval(pcpointer%avect3%rAttr(nf,:)),&
                                   maxval(pcpointer%avect3%rAttr(nf,:))
                   if (pcpointer%aVon(4)) &
                   write(nulprt,*) subname,'  DEBUG loctrans calc4 = ',cplid,nf,&
                                   minval(pcpointer%avect4%rAttr(nf,:)),&
                                   maxval(pcpointer%avect4%rAttr(nf,:))
                   if (pcpointer%aVon(5)) &
                   write(nulprt,*) subname,'  DEBUG loctrans calc5 = ',cplid,nf,&
                                   minval(pcpointer%avect5%rAttr(nf,:)),&
                                   maxval(pcpointer%avect5%rAttr(nf,:))
                endif
             enddo             
             call oasis_timer_stop(tstring)
         ENDIF

          !------------------------------------------------
          !>     * write to restart file if put and at the end of the run, 
          !>       turn off communication
          ! past namcouple runtime (maxtime) no communication
          ! do restart if time+lag = maxtime, this assumes coupling
          ! period and lag and maxtime are all nicely consistent
          !------------------------------------------------

          if (mseclag >= maxtime) then
             sndrcv = .false.   ! turn off communication
             unpack = .false.   ! nothing to unpack
             if (getput == OASIS3_PUT .and. lag > 0 .and. mseclag == maxtime) then
                kinfo = OASIS_ToRest
                call oasis_debug_note(subname//' lag restart write')
                write(tstring,F01) 'wrst_',cplid
                call oasis_timer_start(tstring)
                call oasis_io_write_avfile(rstfile,pcpointer%avect1, &
                   prism_part(partid)%pgsmap,prism_part(partid)%mpicom,nx,ny)
                if (pcpointer%aVon(2)) &
                   call oasis_io_write_avfile(rstfile,pcpointer%avect2, &
                      prism_part(partid)%pgsmap,prism_part(partid)%mpicom,nx,ny,nampre='av2_')
                if (pcpointer%aVon(3)) &
                   call oasis_io_write_avfile(rstfile,pcpointer%avect3, &
                      prism_part(partid)%pgsmap,prism_part(partid)%mpicom,nx,ny,nampre='av3_')
                if (pcpointer%aVon(4)) &
                   call oasis_io_write_avfile(rstfile,pcpointer%avect4, &
                      prism_part(partid)%pgsmap,prism_part(partid)%mpicom,nx,ny,nampre='av4_')
                if (pcpointer%aVon(5)) &
                   call oasis_io_write_avfile(rstfile,pcpointer%avect5, &
                      prism_part(partid)%pgsmap,prism_part(partid)%mpicom,nx,ny,nampre='av5_')
                call oasis_timer_stop(tstring)
                if (OASIS_debug >= 2) then
                   write(nulprt,*) subname,' at ',msec,mseclag,' WRST: ', &
                      trim(mct_avect_exportRList2c(pcpointer%avect1)),' ',&
                      trim(rstfile)
                   call oasis_flush(nulprt)
                endif
             endif
          endif

          !------------------------------------------------
          !>     * map and communicate operations
          !------------------------------------------------

          if (sndrcv) then
          if (getput == OASIS3_PUT) then
             kinfo = OASIS_sent
             call oasis_debug_note(subname//' put section')
             if (OASIS_debug >= 2) then
                write(nulprt,*) subname,' at ',msec,mseclag,' SEND: ', &
                   trim(mct_avect_exportRList2c(pcpointer%avect1))
                call oasis_flush(nulprt)
             endif
             if (sndadd /= 0.0_ip_double_p .or. sndmult /= 1.0_ip_double_p) then
                call oasis_debug_note(subname//' apply sndmult sndadd')
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG sndmult,add = ',sndmult,sndadd
                   write(nulprt,*) subname,'  DEBUG put b4 sndmult,add = ',cplid,&
                                   minval(pcpointer%avect1%rAttr),&
                                   maxval(pcpointer%avect1%rAttr)
                endif
                pcpointer%avect1%rAttr(:,:) = pcpointer%avect1%rAttr(:,:)*sndmult &
                                                         + sndadd
             endif
             if (snddiag) call oasis_advance_avdiag(pcpointer%avect1,prism_part(partid)%mpicom)
             if (mapid > 0) then
                write(tstring,F01) 'pmap_',cplid
                call oasis_debug_note(subname//' put map')
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG put av11 b4 map = ',cplid,&
                                   minval(pcpointer%avect1%rAttr),&
                                   maxval(pcpointer%avect1%rAttr)
                   if (pcpointer%aVon(2)) &
                   write(nulprt,*) subname,'  DEBUG put av2 b4 map = ',cplid,&
                                   minval(pcpointer%avect2%rAttr),&
                                   maxval(pcpointer%avect2%rAttr)
                   if (pcpointer%aVon(3)) &
                   write(nulprt,*) subname,'  DEBUG put av3 b4 map = ',cplid,&
                                   minval(pcpointer%avect3%rAttr),&
                                   maxval(pcpointer%avect3%rAttr)
                   if (pcpointer%aVon(4)) &
                   write(nulprt,*) subname,'  DEBUG put av4 b4 map = ',cplid,&
                                   minval(pcpointer%avect4%rAttr),&
                                   maxval(pcpointer%avect4%rAttr)
                   if (pcpointer%aVon(5)) &
                   write(nulprt,*) subname,'  DEBUG put av5 b4 map = ',cplid,&
                                   minval(pcpointer%avect5%rAttr),&
                                   maxval(pcpointer%avect5%rAttr)
                endif
                call oasis_timer_start(tstring)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' Before interpo ', MPI_Wtime()
                call mct_avect_zero(pcpointer%avect1m)
                call oasis_advance_map(pcpointer%avect1, &
                     pcpointer%avect1m,prism_mapper(mapid),conserv,consbfb, &
                     pcpointer%aVon  ,pcpointer%avect2, &
                     pcpointer%avect3,pcpointer%avect4, &
                     pcpointer%avect5)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' After  interpo ', MPI_Wtime()
                call oasis_timer_stop(tstring)
                write(tstring,F01) 'psnd_',cplid
                call oasis_debug_note(subname//' put send')
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG put av1m b4 send = ',cplid,&
                                   minval(pcpointer%avect1m%rAttr),&
                                   maxval(pcpointer%avect1m%rAttr)
                endif
                call oasis_timer_start(tstring)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' Before MPI put ', MPI_Wtime()
                call mct_waitsend(prism_router(rouid)%router)
                call mct_isend(pcpointer%avect1m,prism_router(rouid)%router,tag)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' After  MPI put ', MPI_Wtime()
                call oasis_timer_stop(tstring)
             ELSE
                write(tstring,F01) 'psnd_',cplid
                call oasis_debug_note(subname//' put send')
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG put av1 b4 send = ',cplid,&
                                   minval(pcpointer%avect1%rAttr),&
                                   maxval(pcpointer%avect1%rAttr)
                endif
                call oasis_timer_start(tstring)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' Before MPI put ', MPI_Wtime()
                call mct_waitsend(prism_router(rouid)%router)
                call mct_isend(pcpointer%avect1,prism_router(rouid)%router,tag)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' After  MPI put ', MPI_Wtime()
                call oasis_timer_stop(tstring)
             ENDIF
          elseif (getput == OASIS3_GET) then
             call oasis_debug_note(subname//' get section')
             if (OASIS_debug >= 2 ) then
                write(nulprt,*) subname,' at ',msec,mseclag,' RECV: ', &
                   trim(mct_avect_exportRList2c(pcpointer%avect1))
                call oasis_flush(nulprt)
             endif
             if (mapid > 0) then
                call oasis_debug_note(subname//' get recv')
                write(tstring,F01) 'grcv_',cplid
                call oasis_timer_start(tstring)
                call mct_avect_zero(pcpointer%avect1m)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' Before MPI get ', MPI_Wtime()
                call mct_recv(pcpointer%avect1m,prism_router(rouid)%router,tag)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' After  MPI get ', MPI_Wtime()
                call oasis_timer_stop(tstring)
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG get af recv = ',cplid,&
                                   minval(pcpointer%avect1m%rAttr),&
                                   maxval(pcpointer%avect1m%rAttr)
                endif
                call oasis_debug_note(subname//' get map')
                write(tstring,F01) 'gmap_',cplid
                call oasis_timer_start(tstring)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' Before interpo ', MPI_Wtime()
                call mct_avect_zero(pcpointer%avect1)
                call oasis_advance_map(pcpointer%avect1m, &
                     pcpointer%avect1,prism_mapper(mapid),conserv,consbfb)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' After  interpo ', MPI_Wtime()
                call oasis_timer_stop(tstring)
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG get af map = ',cplid,&
                                   minval(pcpointer%avect1%rAttr),&
                                   maxval(pcpointer%avect1%rAttr)
                endif
             else
                write(tstring,F01) 'grcv_',cplid
                call oasis_debug_note(subname//' get recv')
                call oasis_timer_start(tstring)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' Before MPI get ', MPI_Wtime()
                call mct_recv(pcpointer%avect1,prism_router(rouid)%router,tag)
                if (LUCIA_debug > 0) &
                   WRITE(nullucia, FMT='(A,I3.3,A,F16.5)') &
                              'Balance: ',pcpointer%namID,' After  MPI get ', MPI_Wtime()
                call oasis_timer_stop(tstring)
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG get af recv = ',cplid,&
                                   minval(pcpointer%avect1%rAttr),&
                                   maxval(pcpointer%avect1%rAttr)
                endif
             endif
             call oasis_debug_note(subname//' apply rcvmult rcvadd')
             if (rcvadd /= 0.0_ip_double_p .or. rcvmult /= 1.0_ip_double_p) then
                pcpointer%avect1%rAttr(:,:) = pcpointer%avect1%rAttr(:,:)*rcvmult &
                                                         + rcvadd
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG rcvmult,add = ',rcvmult,rcvadd
                   write(nulprt,*) subname,'  DEBUG get af rcvmult,add = ',cplid,&
                                   minval(pcpointer%avect1%rAttr),&
                                   maxval(pcpointer%avect1%rAttr)
                endif
             endif
             if (rcvdiag) call oasis_advance_avdiag(pcpointer%avect1,prism_part(partid)%mpicom)
          endif  ! getput
          endif  ! sndrcv

          !------------------------------------------------
          !>     * write to output files if output is turned on
          !------------------------------------------------

          if (output) then
             if (kinfo == OASIS_sent) then
                kinfo = OASIS_sentout
             elseif (kinfo == OASIS_torest) then
                kinfo = OASIS_torestout
             else
                kinfo = OASIS_output
             endif
             call oasis_debug_note(subname//' output')
             write(tstring,F01) 'wout_',cplid
             call oasis_timer_start(tstring)
             if (OASIS_debug >= 2) then
                write(nulprt,*) subname,' at ',msec,mseclag,' WRIT: ', &
                   trim(mct_avect_exportRList2c(pcpointer%avect1))
                call oasis_flush(nulprt)
             endif
             write(fstring,'(A,I2.2)') '_'//trim(compnm)//'_',cplid
             call oasis_io_write_avfbf(pcpointer%avect1,prism_part(partid)%pgsmap,prism_part(partid)%mpicom, &
                nx,ny,msec,fstring)
             call oasis_timer_stop(tstring)

             if (OASIS_debug >= 30) then
                call mct_avect_init(avtest,pcpointer%avect1,&
                                    mct_aVect_lsize(pcpointer%avect1))
                write(tstring,F01) 'rinp_',cplid
                call oasis_timer_start(tstring)
                call oasis_io_read_avfbf(avtest,prism_part(partid)%pgsmap,prism_part(partid)%mpicom,msec,fstring)
                write(nulprt,*) subname,' DEBUG write/read test avfbf should be zero ',&
                                sum(pcpointer%avect1%rAttr-avtest%rAttr)
                call mct_avect_clean(avtest)
                call oasis_timer_stop(tstring)
             endif

          endif

          !------------------------------------------------
          !>     * set avcnt, avect1, ltime, and status
          !------------------------------------------------

          call oasis_debug_note(subname//' reset status')
          if (getput == OASIS3_PUT) then
             if (.not.lreadrest) pcpointer%ltime = msec
             pcpointer%status(:) = OASIS_COMM_WAIT
             pcpointer%avcnt(:) = 0
             call mct_avect_zero(pcpointer%avect1)
             if (pcpointer%aVon(2)) &
                call mct_avect_zero(pcpointer%avect2)
             if (pcpointer%aVon(3)) &
                call mct_avect_zero(pcpointer%avect3)
             if (pcpointer%aVon(4)) &
                call mct_avect_zero(pcpointer%avect4)
             if (pcpointer%aVon(5)) &
                call mct_avect_zero(pcpointer%avect5)
             if (OASIS_debug >= 20) then
                write(nulprt,*) subname,'  DEBUG put reset status = '
             endif
          elseif (getput == OASIS3_GET) then
             if (.not.lreadrest) pcpointer%ltime = msec
             pcpointer%status(:) = OASIS_COMM_WAIT
             if (OASIS_debug >= 20) then
                write(nulprt,*) subname,'  DEBUG get reset status = '
             endif
          endif

       else

          !------------------------------------------------
          ! no action, document
          !------------------------------------------------

          if (OASIS_debug >= 15) then
             if (getput == OASIS3_PUT) then
                write(nulprt,*) subname,' at ',msec,mseclag,' SKIP: ', &
                   trim(mct_avect_exportRList2c(pcpointer%avect1))
             elseif (getput == OASIS3_GET) then
                write(nulprt,*) subname,' at ',msec,mseclag,' SKIP: ', &
                   trim(mct_avect_exportRList2c(pcpointer%avect1))
             endif
             call oasis_flush(nulprt)
          endif

       endif   ! comm_now

          !------------------------------------------------
          !>   * at the end of the run only,  save fields associated
          !>     with non-instant loctrans operations to restart files
          !------------------------------------------------

          IF (mseclag + dt >= maxtime .AND. &
             getput == OASIS3_PUT .and. pcpointer%trans /= ip_instant) then
             call oasis_debug_note(subname//' loctrans restart write')
             write(tstring,F01) 'wtrn_',cplid
             call oasis_timer_start(tstring)
             WRITE(vstring,'(a,i6.6,a)') 'loc',pcpointer%namID,'_cnt'
             CALL oasis_io_write_array(rstfile,prism_part(partid)%mpicom,iarray=pcpointer%avcnt,&
                                       ivarname=TRIM(vstring))
             write(vstring,'(a,i6.6,a)') 'loc',pcpointer%namID,'_'
             CALL oasis_io_write_avfile(rstfile,pcpointer%avect1, &
                prism_part(partid)%pgsmap,prism_part(partid)%mpicom,nx,ny,nampre=TRIM(vstring))
             if (pcpointer%aVon(2)) then
                write(vstring,'(a,i6.6,a)') 'av2loc',pcpointer%namID,'_'
                CALL oasis_io_write_avfile(rstfile,pcpointer%avect2, &
                   prism_part(partid)%pgsmap,prism_part(partid)%mpicom,nx,ny,nampre=TRIM(vstring))
             endif
             if (pcpointer%aVon(3)) then
                write(vstring,'(a,i6.6,a)') 'av3loc',pcpointer%namID,'_'
                CALL oasis_io_write_avfile(rstfile,pcpointer%avect3, &
                   prism_part(partid)%pgsmap,prism_part(partid)%mpicom,nx,ny,nampre=TRIM(vstring))
             endif
             if (pcpointer%aVon(4)) then
                write(vstring,'(a,i6.6,a)') 'av4loc',pcpointer%namID,'_'
                CALL oasis_io_write_avfile(rstfile,pcpointer%avect4, &
                   prism_part(partid)%pgsmap,prism_part(partid)%mpicom,nx,ny,nampre=TRIM(vstring))
             endif
             if (pcpointer%aVon(5)) then
                write(vstring,'(a,i6.6,a)') 'av5loc',pcpointer%namID,'_'
                CALL oasis_io_write_avfile(rstfile,pcpointer%avect5, &
                   prism_part(partid)%pgsmap,prism_part(partid)%mpicom,nx,ny,nampre=TRIM(vstring))
             endif
             call oasis_timer_stop(tstring)
             if (OASIS_debug >= 2) then
                write(nulprt,*) subname,' at ',msec,mseclag,' WTRN: ', &
                   trim(mct_avect_exportRList2c(pcpointer%avect1)),' ',trim(rstfile)
                call oasis_flush(nulprt)
             endif
             if (OASIS_debug >= 20) then
                write(nulprt,*) subname,'  DEBUG write loctrans restart',cplid,&
                                pcpointer%avcnt
                write(nulprt,*) subname,'  DEBUG write loctrans restart',cplid,&
                                minval(pcpointer%avect1%rAttr),&
                                maxval(pcpointer%avect1%rAttr)
             endif
         ENDIF

       !------------------------------------------------
       !>   * GET only, unpack avect1 if it's newly received
       !------------------------------------------------

       if (getput == OASIS3_GET) then
         IF (time_now .AND. unpack) THEN
             if (kinfo == OASIS_output) then
                kinfo = OASIS_recvout
             elseif (kinfo == OASIS_fromrest) then
                kinfo = OASIS_fromrestout
             else
                kinfo = OASIS_recvd
             endif
             if (input) then
                kinfo = OASIS_input
                call oasis_debug_note(subname//' input')
                if (OASIS_debug >= 2) then
                   write(nulprt,*) subname,' at ',msec,mseclag,' READ: ', &
                      trim(mct_avect_exportRList2c(pcpointer%avect1))
                   call oasis_flush(nulprt)
                endif
                write(tstring,F01) 'grin_',cplid
                call oasis_timer_start(tstring)
                if (trim(inpfile) /= trim(cspval)) then
                   call oasis_io_read_avfbf(pcpointer%avect1,prism_part(partid)%pgsmap,prism_part(partid)%mpicom,&
                                            msec,filename=trim(inpfile))
                else
                   fstring = '_'//trim(compnm)
                   call oasis_io_read_avfbf(pcpointer%avect1,prism_part(partid)%pgsmap,prism_part(partid)%mpicom,&
                                            msec,f_string=fstring)
                endif
                call oasis_timer_stop(tstring)
             endif
             if (OASIS_debug >= 2) then
                write(nulprt,*) subname,' at ',msec,mseclag,' UPCK: ',trim(vname)
             endif
             write(tstring,F01) 'gcpy_',cplid
             call oasis_debug_note(subname//' get copy to array')
             call oasis_timer_start(tstring)
             if (present(array1dout)) array1dout(:) = &
                       pcpointer%avect1%rAttr(nfav,:)
             if (present(array2dout)) array2dout(:,:) = &
                      RESHAPE(pcpointer%avect1%rAttr(nfav,:),SHAPE(array2dout))
             call oasis_timer_stop(tstring)
             if (OASIS_debug >= 20) then
                if (present(array1dout)) write(nulprt,*) subname,'  DEBUG array copy = ',&
                         cplid,minval(array1dout),maxval(array1dout)
                if (present(array2dout)) write(nulprt,*) subname,'  DEBUG array copy = ',&
                         cplid,minval(array2dout),maxval(array2dout)
             endif
         ENDIF
          if (time_now) pcpointer%status(nfav) = OASIS_COMM_READY
       endif

       !------------------------------------------------
       !>   * always remember last id and last coupler time
       !------------------------------------------------

       lcouplerid = cplid
       lcouplertime = msec

       if (OASIS_debug >= 2) then
          write(nulprt,*) subname,' at ',msec,mseclag,' KINF: ',trim(vname),kinfo
       endif

     endif  ! runit
    enddo  ! nc = 1,var%ncpl

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_advance_run


!-------------------------------------------------------------------

!> Provides interpolation functionality

!> Maps (regrids, interpolates) data from av1 to avd.
!> av2-av5 are for higher order mapping (hot).

  SUBROUTINE oasis_advance_map(av1,avd,mapper,conserv,consbfb,&
                               avon,av2,av3,av4,av5)

    ! NOTE: mask = 0 is active point according to oasis3 conserv.f

    implicit none
    type(mct_aVect)        ,intent(in)    :: av1  !< source av
    type(mct_aVect)        ,intent(inout) :: avd    !< dst av
    type(prism_mapper_type),intent(inout) :: mapper !< prism_mapper
    integer(kind=ip_i4_p)  ,intent(in),optional :: conserv  !< conserv flag
    logical                ,intent(in),optional :: consbfb  !< conserv bfb option
    logical                ,intent(in),optional :: avon(:) !< which source hot are on
    type(mct_aVect)        ,intent(in),optional :: av2  !< source av2 hot
    type(mct_aVect)        ,intent(in),optional :: av3  !< source av3 hot
    type(mct_aVect)        ,intent(in),optional :: av4  !< source av4 hot
    type(mct_aVect)        ,intent(in),optional :: av5  !< source av5 hot

    integer(kind=ip_i4_p)  :: fsize,lsizes,lsized,nf,ni,n,m
    real(kind=ip_r8_p)     :: sumtmp, wts_sums, wts_sumd, zradi, zlagr
    integer(kind=ip_i4_p),allocatable :: imasks(:),imaskd(:)
    real(kind=ip_r8_p),allocatable :: areas(:),aread(:)
    real(kind=ip_r8_p),allocatable  :: av_sums(:),av_sumd(:)  ! local sums
    character(len=ic_med) :: tstring   ! timer label string
    type(mct_aVect)       :: avdtmp    ! for summing multiple mapping weights
    type(mct_aVect)       :: av2g      ! for bfb sums
    logical               :: lconsbfb
    integer(kind=ip_i4_p),parameter :: avsmax = prism_coupler_avsmax
    logical               :: locavon(avsmax)   ! local avon
    integer(kind=ip_i4_p) :: avonsize, nterm
    character(len=*),parameter :: subname = '(oasis_advance_map)'

    call oasis_debug_enter(subname)

    !> oasis_advance_map does the following
    !> * check for conservation flags

    lconsbfb = .true.
    if (present(consbfb)) then
       lconsbfb = consbfb
    endif

    !> * check for higher order terms
    !--- assume avon and av2-5 are not passed but av1 always is ---
    avonsize = 1
    nterm = 1
    locavon = .false.
    locavon(1) = .true.

    !--- but if avon is passed, use avon flags ---
    if (present(avon)) then
       avonsize = size(avon)
       nterm = min(avsmax,avonsize)
       locavon(1:nterm) = avon(1:nterm)
    else
       !--- if avon is not passed, av2-5 should not be ---
       if (present(av2) .or. present(av3) .or. present(av4) .or. present(av5)) then
          WRITE(nulprt,*) subname,estr,'av2-5 passed but avon not passed'
          CALL oasis_abort()
       endif
    endif

    !> * check consistency between weights and coupling terms
    do n = 1,nterm
       if (locavon(n) .and. n > mapper%nwgts) then
          WRITE(nulprt,*) subname,estr,'in nwgts and coupling terms',mapper%nwgts,n
          CALL oasis_abort()
       endif
    enddo

    !> * run mct sparse matrix mapper on data and separately on hot as needed

    if (locavon(1)) then
       if (mct_avect_nRattr(av1) /= mct_avect_nRattr(avd)) then
          WRITE(nulprt,*) subname,estr,'in av1 num of flds'
          CALL oasis_abort()
       endif
       call mct_sMat_avMult(av1, mapper%sMatP(1), avd)
    endif

    if (locavon(2).or.locavon(3).or.locavon(4).or.locavon(5)) then
       lsized = mct_avect_lsize(avd)
       call mct_aVect_init(avdtmp,avd,lsized)

       if (locavon(2)) then
          if (mct_avect_nRattr(av2) /= mct_avect_nRattr(avd)) then
             WRITE(nulprt,*) subname,estr,'in av2 num of flds'
             CALL oasis_abort()
          endif
          call mct_sMat_avMult(av2, mapper%sMatP(2), avdtmp)
          avd%rAttr = avd%rAttr + avdtmp%rAttr
       endif

       if (locavon(3)) then
          if (mct_avect_nRattr(av3) /= mct_avect_nRattr(avd)) then
             WRITE(nulprt,*) subname,estr,'in av3 num of flds'
             CALL oasis_abort()
          endif
          call mct_sMat_avMult(av3, mapper%sMatP(3), avdtmp)
          avd%rAttr = avd%rAttr + avdtmp%rAttr
       endif

       if (locavon(4)) then
          if (mct_avect_nRattr(av4) /= mct_avect_nRattr(avd)) then
             WRITE(nulprt,*) subname,estr,'in av4 num of flds'
             CALL oasis_abort()
          endif
          call mct_sMat_avMult(av4, mapper%sMatP(4), avdtmp)
          avd%rAttr = avd%rAttr + avdtmp%rAttr
       endif

       if (locavon(5)) then
          if (mct_avect_nRattr(av5) /= mct_avect_nRattr(avd)) then
             WRITE(nulprt,*) subname,estr,'in av5 num of flds'
             CALL oasis_abort()
          endif
          call mct_sMat_avMult(av5, mapper%sMatP(5), avdtmp)
          avd%rAttr = avd%rAttr + avdtmp%rAttr
       endif

       call mct_aVect_clean(avdtmp)
    endif


    call oasis_debug_note(subname//' map')

    !> * enforce conservation

    IF (PRESENT(conserv)) THEN
    call oasis_debug_note(subname//' conserv')
    IF (conserv /= ip_cnone) THEN
       fsize = mct_avect_nRattr(av1)
       allocate(av_sums(fsize),av_sumd(fsize))

       zradi = 1./(eradius*eradius)

       !-------------------
       ! extract mask and area and compute sum of masked area for source
       !-------------------
       lsizes = mct_avect_lsize(mapper%av_ms)
       allocate(imasks(lsizes),areas(lsizes))
       nf = mct_aVect_indexIA(mapper%av_ms,'mask')
       imasks(:) = mapper%av_ms%iAttr(nf,:)
       nf = mct_aVect_indexRA(mapper%av_ms,'area')
       areas(:) = mapper%av_ms%rAttr(nf,:)*zradi

       if (lconsbfb) then
          call mct_avect_gather(mapper%av_ms,av2g,prism_part(mapper%spart)%gsmap,&
                                0,mpi_comm_local)
          wts_sums = 0.0_ip_r8_p
          if (mpi_rank_local == 0) then
             ni = mct_aVect_indexIA(av2g,'mask')
             nf = mct_aVect_indexRA(av2g,'area')
             do n = 1,mct_avect_lsize(av2g)
                if (av2g%iAttr(ni,n) == 0) wts_sums = wts_sums + av2g%rAttr(nf,n)*zradi
             enddo
          endif
          call oasis_mpi_bcast(wts_sums,mpi_comm_local,subname//" bcast wts_sums")
       if (mpi_rank_local == 0) then 
          call mct_avect_clean(av2g)
       endif 
       else
          sumtmp = 0.0_ip_r8_p
          do n = 1,lsizes
             if (imasks(n) == 0) sumtmp = sumtmp + areas(n)
          enddo
          call oasis_mpi_sum(sumtmp,wts_sums,mpi_comm_local,string=subname//':wts_sums',&
                             all=.true.)
       endif

       !-------------------
       ! extract mask and area and compute sum of masked area for destination
       !-------------------
       lsized = mct_avect_lsize(mapper%av_md)
       allocate(imaskd(lsized),aread(lsized))
       nf = mct_aVect_indexIA(mapper%av_md,'mask')
       imaskd(:) = mapper%av_md%iAttr(nf,:)
       nf = mct_aVect_indexRA(mapper%av_md,'area')
       aread(:) = mapper%av_md%rAttr(nf,:)*zradi

       if (lconsbfb) then
          call mct_avect_gather(mapper%av_md,av2g,prism_part(mapper%dpart)%gsmap,0,mpi_comm_local)
          wts_sumd = 0.0_ip_r8_p
          if (mpi_rank_local == 0) then
             ni = mct_aVect_indexIA(av2g,'mask')
             nf = mct_aVect_indexRA(av2g,'area')
             do n = 1,mct_avect_lsize(av2g)
                if (av2g%iAttr(ni,n) == 0) wts_sumd = wts_sumd + av2g%rAttr(nf,n)*zradi
             enddo
          endif
          call oasis_mpi_bcast(wts_sumd,mpi_comm_local,subname//" bcast wts_sumd")
       if (mpi_rank_local == 0) then
          call mct_avect_clean(av2g)
       endif
       else
          sumtmp = 0.0_ip_r8_p
          do n = 1,lsized
             if (imaskd(n) == 0) sumtmp = sumtmp + aread(n)
          enddo
          call oasis_mpi_sum(sumtmp,wts_sumd,mpi_comm_local,string=subname//':wts_sumd',all=.true.)
       endif

       if (OASIS_debug >= 30) then
          write(nulprt,*) subname,' DEBUG conserve src mask ',minval(imasks),&
                          maxval(imasks),sum(imasks)
          write(nulprt,*) subname,' DEBUG conserve dst mask ',minval(imaskd),&
                          maxval(imaskd),sum(imaskd)
          write(nulprt,*) subname,' DEBUG conserve src area ',minval(areas),&
                          maxval(areas),sum(areas)
          write(nulprt,*) subname,' DEBUG conserve dst area ',minval(aread),&
                          maxval(aread),sum(aread)
          write(nulprt,*) subname,' DEBUG conserve wts_sum  ',wts_sums,wts_sumd
       endif

       !-------------------
       ! compute global sums of av1
       ! assume av1 is the thing to be conserved
       !-------------------
       call oasis_advance_avsum(av1,av_sums,prism_part(mapper%spart)%gsmap,mpi_comm_local, &
                                mask=imasks,wts=areas,consbfb=lconsbfb)
       call oasis_advance_avsum(avd,av_sumd,prism_part(mapper%dpart)%gsmap,mpi_comm_local, &
                                mask=imaskd,wts=aread,consbfb=lconsbfb)

       if (OASIS_debug >= 20) then
          write(nulprt,*) subname,' DEBUG src sum b4 conserve ',av_sums
          write(nulprt,*) subname,' DEBUG dst sum b4 conserve ',av_sumd
       endif

       if (conserv == ip_cglobal) then
          if (wts_sumd == 0.0_ip_r8_p) then
              WRITE(nulprt,*) subname,estr,'global conserve sums to zero '
              CALL oasis_abort()
          endif
          do m = 1,fsize
             zlagr = (av_sumd(m) - av_sums(m)) / wts_sumd
             do n = 1,lsized
                if (imaskd(n) == 0) avd%rAttr(m,n) = avd%rAttr(m,n) - zlagr
             enddo
          enddo
       elseif (conserv == ip_cglbpos) then
          do m = 1,fsize
             if (av_sumd(m) == 0.0_ip_r8_p .and. av_sums(m) /= 0.0_ip_r8_p) then
                 WRITE(nulprt,*) subname,estr,'cglpos conserve one of the sums is zero'
                 CALL oasis_abort()
             elseif (av_sumd(m) /= 0.0_ip_r8_p) then
                zlagr = av_sums(m) / av_sumd(m)
                do n = 1,lsized
                   if (imaskd(n) == 0) avd%rAttr(m,n) = avd%rAttr(m,n) * zlagr
                enddo
             endif
          enddo
       elseif (conserv == ip_cbasbal) then
          if (wts_sumd == 0.0_ip_r8_p .or. wts_sums == 0.0_ip_r8_p) then
              WRITE(nulprt,*) subname,estr,'cbasbal conserve both sums are zero'
              CALL oasis_abort()
          endif
          do m = 1,fsize
             zlagr = (av_sumd(m) - (av_sums(m)*(wts_sumd/wts_sums))) / wts_sumd
             do n = 1,lsized
                if (imaskd(n) == 0) avd%rAttr(m,n) = avd%rAttr(m,n) - zlagr
             enddo
          enddo
       elseif (conserv == ip_cbaspos) then
          do m = 1,fsize
             if (av_sumd(m) == 0.0_ip_r8_p .and. av_sums(m) /= 0.0_ip_r8_p) then
                 WRITE(nulprt,*) subname,estr,'cbaspos conserve one of the sums is zero'
                 CALL oasis_abort()
             elseif (av_sumd(m) /= 0.0_ip_r8_p) then
                zlagr = (av_sums(m)/av_sumd(m)) * (wts_sumd/wts_sums)
                do n = 1,lsized
                   if (imaskd(n) == 0) avd%rAttr(m,n) = avd%rAttr(m,n) * zlagr
                enddo
             endif
          enddo
       else
           WRITE(nulprt,*) subname,estr,'conserv option unknown = ',conserv
           CALL oasis_abort()
       endif

       if (OASIS_debug >= 20) then
          call oasis_advance_avsum(av1,av_sums,prism_part(mapper%spart)%gsmap,mpi_comm_local, &
                                   mask=imasks,wts=areas,consbfb=lconsbfb)
          call oasis_advance_avsum(avd,av_sumd,prism_part(mapper%dpart)%gsmap,mpi_comm_local, &
                                   mask=imaskd,wts=aread,consbfb=lconsbfb)
          write(nulprt,*) subname,' DEBUG src sum af conserve ',av_sums
          write(nulprt,*) subname,' DEBUG dst sum af conserve ',av_sumd
          CALL oasis_flush(nulprt)
       endif

       deallocate(imasks,imaskd,areas,aread)
       deallocate(av_sums,av_sumd)
   ENDIF
   ENDIF  ! present conserve

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_advance_map

!-------------------------------------------------------------------

!> A generic method for summing fields in an attribute vector

  SUBROUTINE oasis_advance_avsum(av,sum,gsmap,mpicom,mask,wts,consbfb)

    implicit none
    type(mct_aVect)      ,intent(in)    :: av      ! input av
    real(kind=ip_r8_p)   ,intent(inout) :: sum(:)  ! sum of av fields
    type(mct_gsMap)      ,intent(in)    :: gsmap   ! gsmap associate with av
    integer(kind=ip_i4_p),intent(in)    :: mpicom  ! mpicom
    integer(kind=ip_i4_p),intent(in),optional :: mask(:) ! mask to apply to av
    real(kind=ip_r8_p)   ,intent(in),optional :: wts(:)  ! wts to apply to av
    logical              ,intent(in),optional :: consbfb ! bfb conserve

    integer(kind=ip_i4_p) :: n,m,ierr,mytask
    integer(kind=ip_i4_p) :: lsize,fsize        ! local size of av, number of flds in av
    real(kind=ip_r8_p),allocatable  :: lsum(:)  ! local sums
    real(kind=ip_r8_p),allocatable  :: lwts(:)  ! local wts taking into account mask and wts
    type(mct_aVect)       :: av1, av1g    ! use av1,av1g for gather and bfb sum
    logical               :: lconsbfb     ! local conserve bfb
    character(len=*),parameter :: subname = '(oasis_advance_avsum)'

    call oasis_debug_enter(subname)

    lconsbfb = .true.
    if (present(consbfb)) then
       lconsbfb = consbfb
    endif

    fsize = mct_avect_nRattr(av)
    lsize = mct_avect_lsize(av)

    allocate(lsum(fsize))
    lsum = 0.0_ip_r8_p
    allocate(lwts(lsize))
    lwts = 1.0_ip_r8_p

    if (size(sum) /= fsize) then
        WRITE(nulprt,*) subname,estr,'size sum ne size av'
        CALL oasis_abort()
    endif

    if (present(mask)) then
       if (size(mask) /= lsize) then
           WRITE(nulprt,*) subname,estr,'size mask ne size av'
           CALL oasis_abort()
       endif
       do n = 1,lsize
          if (mask(n) /= 0) lwts(n) = 0.0_ip_r8_p
       enddo
    endif

    if (present(wts)) then
       if (size(wts) /= lsize) then
           WRITE(nulprt,*) subname,estr,'size wts ne size av'
           CALL oasis_abort()
       endif
       do n = 1,lsize
          lwts(n) = lwts(n) * wts(n)
       enddo
    endif

    if (lconsbfb) then
       call mct_avect_init(av1,av,lsize)
       do n = 1,lsize
       do m = 1,fsize
          av1%rAttr(m,n) = av%rAttr(m,n)*lwts(n)
       enddo
       enddo
       call mct_avect_gather(av1,av1g,gsmap,0,mpicom)
       call MPI_COMM_RANK(mpicom,mytask,ierr)
       sum = 0.0_ip_r8_p
       if (mytask == 0) then
          do n = 1,mct_avect_lsize(av1g)
          do m = 1,fsize
             sum(m) = sum(m) + av1g%rAttr(m,n)
          enddo
          enddo
       endif
       call oasis_mpi_bcast(sum,mpicom,subname//" bcast sum")
       call mct_avect_clean(av1)
    if (mytask == 0) then 
       call mct_avect_clean(av1g)
    endif
    else
       lsum = 0.0_ip_r8_p
       do n = 1,lsize
       do m = 1,fsize
          lsum(m) = lsum(m) + av%rAttr(m,n)*lwts(n)
       enddo
       enddo
       call oasis_mpi_sum(lsum,sum,mpicom,string=trim(subname)//':sum',all=.true.)
    endif

    deallocate(lsum)
    deallocate(lwts)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_advance_avsum

!-------------------------------------------------------------------

!> A generic method for writing the global sums of fields in an attribute vector

  SUBROUTINE oasis_advance_avdiag(av,mpicom,mask,wts)

    implicit none
    type(mct_aVect)      ,intent(in)    :: av    ! input av
    integer(kind=ip_i4_p),intent(in)    :: mpicom  ! mpi communicator
    integer(kind=ip_i4_p),intent(in),optional :: mask(:) ! mask to apply to av
    real(kind=ip_r8_p)   ,intent(in),optional :: wts(:)  ! wts to apply to av

    integer(kind=ip_i4_p) :: n,m,ierr,mype
    integer(kind=ip_i4_p) :: lsize,fsize        ! local size of av, number of flds in av
    logical               :: first_call  
    real(kind=ip_r8_p)    :: lval               ! local temporary
    real(kind=ip_r8_p),allocatable  :: lsum(:)  ! local sum
    real(kind=ip_r8_p),allocatable  :: lmin(:)  ! local min
    real(kind=ip_r8_p),allocatable  :: lmax(:)  ! local max
    real(kind=ip_r8_p),allocatable  :: gsum(:)  ! global sum
    real(kind=ip_r8_p),allocatable  :: gmin(:)  ! global min
    real(kind=ip_r8_p),allocatable  :: gmax(:)  ! global max
    real(kind=ip_r8_p),allocatable  :: lwts(:)  ! local wts taking into account mask and wts
    type(mct_string) :: mstring     ! mct char type
    character(len=64):: itemc       ! string converted to char
    character(len=*),parameter :: subname = '(oasis_advance_avdiag)'

    call oasis_debug_enter(subname)

    fsize = mct_avect_nRattr(av)
    lsize = mct_avect_lsize(av)

    allocate(lsum(fsize))
    allocate(lmin(fsize))
    allocate(lmax(fsize))
    allocate(gsum(fsize))
    allocate(gmin(fsize))
    allocate(gmax(fsize))

    allocate(lwts(lsize))
    lwts = 1.0_ip_r8_p
!!$    lmin=HUGE(lwts)
!!$    lmax=-lmin
    if (present(mask)) then
       if (size(mask) /= lsize) then
           WRITE(nulprt,*) subname,estr,'size mask ne size av'
           CALL oasis_abort()
       endif
       do n = 1,lsize
          if (mask(n) /= 0) lwts(n) = 0.0_ip_r8_p
       enddo
    endif

    if (present(wts)) then
       if (size(wts) /= lsize) then
           WRITE(nulprt,*) subname,estr,'size wts ne size av'
           CALL oasis_abort()
       endif
       do n = 1,lsize
          lwts(n) = lwts(n) * wts(n)
       enddo
    endif

    lsum = 0.0_ip_r8_p
    lmin =  9.99e36    ! in case lsize is zero
    lmax = -9.99e36    ! in case lsize is zero
    do m = 1,fsize
    first_call = .true.
    do n = 1,lsize
       lval = av%rAttr(m,n)*lwts(n)
       lsum(m) = lsum(m) + lval
       if (lwts(n) /= 0.0_ip_r8_p) then
          if (first_call) then
             lmin(m) = lval
             lmax(m) = lval
             first_call = .false.
          else
             lmin(m) = min(lmin(m),lval)
             lmax(m) = max(lmax(m),lval)
          endif
       endif
    enddo
    enddo

    mype = -1
    if (mpicom /= MPI_COMM_NULL) then
       call MPI_COMM_RANK(mpicom,mype,ierr)
       call oasis_mpi_sum(lsum,gsum,mpicom,string=trim(subname)//':sum',all=.false.)
       call oasis_mpi_min(lmin,gmin,mpicom,string=trim(subname)//':min',all=.false.)
       call oasis_mpi_max(lmax,gmax,mpicom,string=trim(subname)//':max',all=.false.)
    endif
    if (mype == 0) then
       do m = 1,fsize
          call mct_aVect_getRList(mstring,m,av)
          itemc = mct_string_toChar(mstring)
          call mct_string_clean(mstring)
          write(nulprt,'(a,a16,3g21.12)') '   diags: ',trim(itemc),gmin(m),gmax(m),gsum(m)
       enddo
    endif

    deallocate(lsum,lmin,lmax)
    deallocate(gsum,gmin,gmax)
    deallocate(lwts)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_advance_avdiag

!-------------------------------------------------------------------
END MODULE mod_oasis_advance

