MODULE mod_oasis_advance

    USE mod_oasis_kinds
    USE mod_oasis_data
    USE mod_oasis_parameters
    USE mod_oasis_coupler
    USE mod_oasis_part
    USE mod_oasis_timer
    USE mod_oasis_var
    USE mod_oasis_sys
    USE mod_oasis_io
    USE mct_mod

    IMPLICIT NONE

    private

    public oasis_advance_init
    public oasis_advance_run

contains

!---------------------------------------------------------------------
  SUBROUTINE oasis_advance_init(kinfo)

!   ----------------------------------------------------------------
!   This routine handles initial restart and communication
!   of data for fields with positive lags
!   ----------------------------------------------------------------

    IMPLICIT none
!   ----------------------------------------------------------------
    INTEGER(kind=ip_i4_p), intent(inout) :: kinfo    ! status
!   ----------------------------------------------------------------
    integer(kind=ip_i4_p) :: cplid,partid,varid
    integer(kind=ip_i4_p) :: nf,lsize,nflds
    integer(kind=ip_i4_p) :: dt,ltime,lag,getput
    integer(kind=ip_i4_p) :: msec
    type(mct_avect)       :: avtmp  ! data read from restart
    real   (kind=ip_r8_p), allocatable :: array(:) ! data
    integer(kind=ip_i4_p) :: mseclag   ! model time + lag
    character(len=ic_xl)  :: rstfile   ! restart filename
    character(len=ic_med) :: vstring   ! temporary string
    character(len=*),parameter :: subname = 'oasis_advance_init'

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    call oasis_timer_start ('advance_init')

    if (OASIS_debug >= 2) then
       write(nulprt,*) '   subname         at         time    time+lag   act: field '
       write(nulprt,*) '   diags :     fldname    min      max      sum '
    endif

    call oasis_debug_note(subname//' loop over cplid')
    do cplid = 1,prism_ncoupler
       dt    = prism_coupler(cplid)%dt
       lag   = prism_coupler(cplid)%lag
       ltime = prism_coupler(cplid)%ltime
       getput= prism_coupler(cplid)%getput
       rstfile=trim(prism_coupler(cplid)%rstfile)
       partid= prism_coupler(cplid)%partID
       msec = 0   ! reasonable default to start with
       mseclag = msec

       !------------------------------------------------
       ! check that lag is reasonable
       !------------------------------------------------

       if (lag > dt .or. lag <= -dt) then
          write(nulprt,*) subname,' ERROR lag out of dt range cplid/dt/lag=',cplid,dt,lag
          WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
          call oasis_abort_noarg()
       endif

       !------------------------------------------------
       ! read restart and call advance for the current fields
       !------------------------------------------------

       call oasis_debug_note(subname//' check for lag restart')
       if (getput == OASIS3_PUT .and. lag > 0) then
          msec = 0-lag   ! effective model time of restart
          mseclag = msec + lag
          if (len_trim(rstfile) < 1) then
             write(nulprt,*) subname,' ERROR restart undefined'
             WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
             call oasis_abort_noarg()
          endif
          lsize = mct_aVect_lsize(prism_coupler(cplid)%aVect1)
          nflds = mct_aVect_nRAttr(prism_coupler(cplid)%aVect1)
          call mct_aVect_init(avtmp,rlist=prism_coupler(cplid)%fldlist,lsize=lsize)
          if (OASIS_debug >= 2) then
             write(nulprt,*) subname,' at ',msec,mseclag,' RRST: ',&
                trim(prism_coupler(cplid)%fldlist),' ',trim(rstfile)
          endif
          call oasis_io_read_avfile(trim(rstfile),avtmp,prism_part(partid)%gsmap)
          allocate(array(lsize))
          do nf = 1,nflds
             varid = prism_coupler(cplid)%varid(nf)
             array(1:lsize) = avtmp%rAttr(nf,1:lsize)
             call oasis_advance_run(OASIS_Out,varid,msec,array,kinfo,readrest=.true.)
          enddo
          deallocate(array)
          call mct_avect_clean(avtmp)
       endif

       !------------------------------------------------
       ! read restart for LOCTRANS fields
       ! do after restart and advance above because prism_advance_run
       ! fills in the avect with the array info
       !------------------------------------------------

       call oasis_debug_note(subname//' check for loctrans restart')
       if (getput == OASIS3_PUT .and. prism_coupler(cplid)%trans /= ip_instant) then
          if (len_trim(rstfile) < 1) then
             write(nulprt,*) subname,' ERROR restart undefined'
             WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
             call oasis_abort_noarg()
          endif
          if (OASIS_debug >= 2) then
             write(nulprt,*) subname,' at ',msec,mseclag,' RTRN: ',&
                trim(prism_coupler(cplid)%fldlist),' ',trim(rstfile)
          endif
          write(vstring,'(a,i2.2,a)') 'loc',prism_coupler(cplid)%trans,'_'
          call oasis_io_read_avfile(rstfile,prism_coupler(cplid)%avect1,&
                                    prism_part(partid)%gsmap,abort=.false.,nampre=trim(vstring))
          write(vstring,'(a,i2.2,a)') 'loc',prism_coupler(cplid)%trans,'_cnt'
          call oasis_io_read_array(rstfile,iarray=prism_coupler(cplid)%avcnt,&
                                   ivarname=trim(vstring),abort=.false.)

          if (OASIS_debug >= 20) then
             write(nulprt,*) subname,'  DEBUG read loctrans restart',&
                             cplid,prism_coupler(cplid)%avcnt
             write(nulprt,*) subname,'  DEBUG read loctrans restart',cplid,&
                             minval(prism_coupler(cplid)%avect1%rAttr),&
                             maxval(prism_coupler(cplid)%avect1%rAttr)
          endif
       endif

    enddo ! cplid
    call oasis_timer_stop ('advance_init')

    call oasis_debug_exit(subname)

  end SUBROUTINE oasis_advance_init
!---------------------------------------------------------------------
  SUBROUTINE oasis_advance_run(mop,varid,msec,array,kinfo,readrest)

    IMPLICIT none
!   ----------------------------------------------------------------
    integer(kind=ip_i4_p), intent(in)    :: mop      ! OASIS_Out or OASIS_In
    INTEGER(kind=ip_i4_p), intent(in)    :: varid    ! prism_var id
    INTEGER(kind=ip_i4_p), intent(in)    :: msec     ! model time
    REAL   (kind=ip_r8_p), intent(inout) :: array(:) ! data
    INTEGER(kind=ip_i4_p), intent(inout) :: kinfo    ! status
    logical              , intent(in),optional :: readrest  ! special flag to indicate this 
                                                            ! is called from the advance_init 
                                                            ! method for restart
!   ----------------------------------------------------------------
    character(len=ic_lvar):: vname
    integer(kind=ip_i4_p) :: cplid,rouid,mapid,partid
    integer(kind=ip_i4_p) :: nfav,nsav,nsa,n,nc,nf
    integer(kind=ip_i4_p) :: tag,dt,ltime,lag,getput,maxtime,conserv
    logical               :: consbfb
    logical               :: sndrcv,output,input,unpack
    logical               :: snddiag,rcvdiag
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
    TYPE(mct_avect)       :: avtest    ! temporary
!    type(mct_avect),pointer  :: avect1, avect2
!    type(mct_sMatP),pointer  :: sMatP
!    type(mct_router),pointer :: router
    character(len=*),parameter :: subname = 'oasis_advance_run '
    character(len=*),parameter :: F01 = '(a,i3.3)'
!   ----------------------------------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK
    vname = prism_var(varid)%name

    lreadrest = .false.
    if (present(readrest)) then
       lreadrest = readrest
    endif
    if (lreadrest) kinfo = OASIS_fromrest

    !------------------------------------------------
    ! validate mop
    !------------------------------------------------

    if (mop /= OASIS_Out .and. mop /= OASIS_In) then
       write(nulprt,*) subname,' at ',msec,mseclag,'  ERROR: ',trim(vname)
       write(nulprt,*) subname,' ERROR mop invalid ',mop
       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
       CALL oasis_flush(nulprt)
       call oasis_abort_noarg()
    endif

    !------------------------------------------------
    ! for all the couplers associated with this var
    !------------------------------------------------

    call oasis_debug_note(subname//' loop over var ncpl')
    do nc = 1,prism_var(varid)%ncpl
       cplid   = prism_var(varid)%cpl(nc)
       rouid   = prism_coupler(cplid)%routerid
       mapid   = prism_coupler(cplid)%mapperid
       tag     = prism_coupler(cplid)%tag
       dt      = prism_coupler(cplid)%dt
       lag     = prism_coupler(cplid)%lag
       ltime   = prism_coupler(cplid)%ltime
       getput  = prism_coupler(cplid)%getput
       sndrcv  = prism_coupler(cplid)%sndrcv
       rstfile =trim(prism_coupler(cplid)%rstfile)
       inpfile =trim(prism_coupler(cplid)%inpfile)
       maxtime = prism_coupler(cplid)%maxtime
       output  = prism_coupler(cplid)%output
       input   = prism_coupler(cplid)%input
       partid  = prism_coupler(cplid)%partID
       conserv = prism_coupler(cplid)%conserv
       consbfb = .true.
       if (trim(prism_coupler(cplid)%consopt) == "opt") consbfb = .false.
       snddiag = prism_coupler(cplid)%snddiag
       rcvdiag = prism_coupler(cplid)%rcvdiag
       sndadd  = prism_coupler(cplid)%sndadd
       sndmult = prism_coupler(cplid)%sndmult
       rcvadd  = prism_coupler(cplid)%rcvadd
       rcvmult = prism_coupler(cplid)%rcvmult
!      avect1  => prism_coupler(cplid)%avect1
!      avect2  => prism_coupler(cplid)%avect2
!      sMatP   => prism_mapper(mapid)%sMatP
!      router  => prism_router(rouid)%router

       unpack = (sndrcv .or. input)

       call oasis_debug_note(subname//' set nx and ny')
       if (prism_part(partid)%nx >= 1) then
          nx = prism_part(partid)%nx
          ny = prism_part(partid)%ny
       else
          nx = prism_part(partid)%gsize
          ny = 1
       endif

       if (OASIS_debug >= 20) then
          write(nulprt,*) subname,'  DEBUG nx, ny = ',nx,ny
       endif

       !------------------------------------------------
       ! check that lag is reasonable
       !------------------------------------------------

       if (abs(lag) > dt) then
          write(nulprt,*) subname,' ERROR lag gt dt for cplid',cplid
          WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
          call oasis_abort_noarg()
       endif

       !------------------------------------------------
       ! check that model op matches coupler op
       !------------------------------------------------

       if ((mop == OASIS_Out .and. getput == OASIS3_PUT) .or. &
           (mop == OASIS_In  .and. getput == OASIS3_GET)) then
          !-continue
       else
          write(nulprt,*) subname,' ERROR model op does not match coupler op',mop,getput
          WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
          call oasis_abort_noarg()
       endif

       !------------------------------------------------
       ! compute lag time, only on put side
       ! set time now, is it a coupling period?
       !------------------------------------------------

       call oasis_debug_note(subname//' set mseclag')
       if (getput == OASIS3_PUT) then
          mseclag = msec + lag
       elseif (getput == OASIS3_GET) then
          mseclag = msec
       endif

       if (OASIS_debug >= 20) then
          write(nulprt,*) subname,'  DEBUG msec,mseclag = ',msec,mseclag
       endif

       time_now = .false.
       if (mod(mseclag,dt) == 0) time_now = .true.

       !------------------------------------------------
       ! check that model hasn't gone past maxtime
       !------------------------------------------------

       if (msec >= maxtime) then
          write(nulprt,*) subname,' at ',msec,mseclag,'  ERROR: ',trim(vname)
          write(nulprt,*) subname,' ERROR model time beyond namcouple maxtime',&
                          msec,maxtime
          WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
          call oasis_abort_noarg()
       endif

       !------------------------------------------------
       ! check that model isn't going backwards
       !------------------------------------------------

       if (lcouplertime /= ispval .and. msec < lcouplertime) then
          write(nulprt,*) subname,' at ',msec,mseclag,'  ERROR: ',trim(vname)
          write(nulprt,*) subname,' ERROR model seems to be running backwards',&
                          msec,lcouplertime
          WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
          call oasis_abort_noarg()
       endif

       !------------------------------------------------
       ! check that varible didn't miss a coupling period 
       ! also check that prior sequences weren't missed at this 
       ! step if get operation.  only done for sndrcv operations.
       ! attempts to trap deadlocks before they happen
       !------------------------------------------------

       do n = 1,prism_ncoupler
          if ((prism_coupler(n)%ltime /= ispval) .and. &
              (sndrcv .and. prism_coupler(n)%sndrcv) .and. &
              (msec >  prism_coupler(n)%ltime + prism_coupler(n)%dt)) then
             write(nulprt,*) subname,' ERROR coupling skipped at earlier time, &
                             & potential deadlock '
             write(nulprt,*) subname,' my coupler = ',cplid,' variable = ',&
                             trim(prism_coupler(cplid)%fldlist)
             write(nulprt,*) subname,' current time = ',msec,' mseclag = ',mseclag
             write(nulprt,*) subname,' skipped coupler = ',n,' variable = ',&
                             trim(prism_coupler(n)%fldlist)
             write(nulprt,*) subname,' skipped coupler last time and dt = ',&
                             prism_coupler(n)%ltime,prism_coupler(n)%dt
             WRITE(nulprt,*) subname,' ERROR model timestep does not match coupling timestep'
             WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
             call oasis_flush(nulprt)
             call oasis_abort_noarg()
          endif
          if ((prism_coupler(n)%ltime /= ispval) .and. &
              (sndrcv .and. prism_coupler(n)%sndrcv .and. getput == OASIS3_GET) .and. &
              (prism_coupler(cplid)%seq > prism_coupler(n)%seq) .and. &
              (msec >= prism_coupler(n)%ltime + prism_coupler(n)%dt)) then
             write(nulprt,*) subname,' ERROR coupling sequence out of order, &
                             & potential deadlock '
             write(nulprt,*) subname,' my coupler = ',cplid,' variable = ',&
                             trim(prism_coupler(cplid)%fldlist)
             write(nulprt,*) subname,' sequence number = ',prism_coupler(cplid)%seq
             write(nulprt,*) subname,' current time = ',msec,' mseclag = ',mseclag
             write(nulprt,*) subname,' skipped coupler = ',n,' variable = ',&
                             trim(prism_coupler(n)%fldlist)
             write(nulprt,*) subname,' skipped coupler last time and dt = ',&
                             prism_coupler(n)%ltime,prism_coupler(n)%dt
             write(nulprt,*) subname,' skipped sequence number = ',prism_coupler(n)%seq
             WRITE(nulprt,*) subname,' ERROR model sequence does not match coupling sequence'
             WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
             call oasis_abort_noarg()
          endif
       enddo

       !------------------------------------------------
       ! compute field index and check sizes
       !------------------------------------------------

       call oasis_debug_note(subname//' compute field index and sizes')
       nfav = mct_avect_indexra(prism_coupler(cplid)%avect1,trim(vname))
       nsav = mct_avect_lsize(prism_coupler(cplid)%avect1)
       nsa = size(array)

       if (OASIS_debug >= 20) then
          write(nulprt,*) subname,'  DEBUG nfav,nsav,nsa = ',nfav,nsav,nsa
       endif

       if (nsav /= nsa) then
          write(nulprt,*) subname,' at ',msec,mseclag,'  ERROR: ',trim(vname)
          write(nulprt,*) subname,' ERROR sizes ',nsav,nsa
          WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
          call oasis_abort_noarg()
       endif

       !------------------------------------------------
       ! update avect1 on put side, apply appropriate transform
       ! if its coupling time, set status of this var to ready
       ! on restart, treat as instant value
       !------------------------------------------------

       if (getput == OASIS3_PUT) then

          call oasis_debug_note(subname//' loctrans operation')
          write(tstring,F01) 'pcpy_',cplid
          call oasis_timer_start(tstring)

          cstring = 'none'
          if (lreadrest .or. prism_coupler(cplid)%trans == ip_instant) then
             if (time_now) then
                cstring = 'instant'
                do n = 1,nsav
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) = array(n)
                enddo
                prism_coupler(cplid)%avcnt(nfav) = 1
             endif

          elseif (prism_coupler(cplid)%trans == ip_average) then
             cstring = 'average'
             if (kinfo == OASIS_OK) kinfo = OASIS_LocTrans
             do n = 1,nsav
                prism_coupler(cplid)%avect1%rAttr(nfav,n) = &
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) + array(n)
             enddo
             prism_coupler(cplid)%avcnt(nfav) = prism_coupler(cplid)%avcnt(nfav) + 1

          elseif (prism_coupler(cplid)%trans == ip_accumul) then
             cstring = 'accumul'
             if (kinfo == OASIS_OK) kinfo = OASIS_LocTrans
             do n = 1,nsav
                prism_coupler(cplid)%avect1%rAttr(nfav,n) = &
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) + array(n)
             enddo
             prism_coupler(cplid)%avcnt(nfav) = 1

          elseif (prism_coupler(cplid)%trans == ip_max) then
             cstring = 'max'
             if (kinfo == OASIS_OK) kinfo = OASIS_LocTrans
             do n = 1,nsav
                if (prism_coupler(cplid)%avcnt(nfav) == 0) then
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) = array(n)
                else
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) = &
                      max(prism_coupler(cplid)%avect1%rAttr(nfav,n),array(n))
                endif
             enddo
             prism_coupler(cplid)%avcnt(nfav) = 1

          elseif (prism_coupler(cplid)%trans == ip_min) then
             cstring = 'min'
             if (kinfo == OASIS_OK) kinfo = OASIS_LocTrans
             do n = 1,nsav
                if (prism_coupler(cplid)%avcnt(nfav) == 0) then
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) = array(n)
                else
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) = &
                      min(prism_coupler(cplid)%avect1%rAttr(nfav,n),array(n))
                endif
             enddo
             prism_coupler(cplid)%avcnt(nfav) = 1

          else
             write(nulprt,*) subname,' ERROR: trans not known ',prism_coupler(cplid)%trans
             WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
             call oasis_abort_noarg()
          endif
          call oasis_timer_stop(tstring)

          if (OASIS_debug >= 2 .and. trim(cstring) /= 'none') then
             write(nulprt,*) subname,' at ',msec,mseclag,' PACK: ',&
                             trim(vname),' ',trim(cstring)
          endif

          if (OASIS_debug >= 20) then
             write(nulprt,*) subname,'  DEBUG loctrans update ',cplid,' ',&
             trim(cstring),prism_coupler(cplid)%avcnt(nfav)
          endif

          if (time_now) then
             prism_coupler(cplid)%status(nfav) = OASIS_COMM_READY
          endif
       endif

       !------------------------------------------------
       ! decide if it's time to communicate based on 
       ! time.  also, on the put side, status of all vars
       ! must be ready which means all vars have called put.
       ! on get side, all ready means all vars have unpacked
       ! from last get.
       !------------------------------------------------

       call oasis_debug_note(subname//' comm_now compute')
       comm_now = .false.
       if (time_now) then
          comm_now = .true.
          do nf = 1,prism_coupler(cplid)%nflds
             if (prism_coupler(cplid)%status(nf) /= OASIS_COMM_READY) then
                comm_now = .false.
                if (OASIS_debug >= 15) then
                   write(nulprt,*) subname,' at ',msec,mseclag,' STAT: ',nf,' NOT READY'
                endif
             else
                if (OASIS_debug >= 15) then
                   write(nulprt,*) subname,' at ',msec,mseclag,' STAT: ',nf,' READY'
                endif
             endif
          enddo
       endif

       if (comm_now) then

          call oasis_debug_note(subname//' comm_now')

          !------------------------------------------------
          ! this is the time critical bit, we need to make sure the
          ! model is truly advancing in time when comms are called.
          ! must ignore the initial call, ltime = 0
          !------------------------------------------------

          if (prism_coupler(cplid)%ltime /= ispval .and. msec <= prism_coupler(cplid)%ltime) then
             write(nulprt,*) subname,' ERROR: model did not advance in time correctly',&
                             msec,prism_coupler(cplid)%ltime
             WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
             call oasis_abort_noarg()
          endif

          !------------------------------------------------
          ! average as needed (not cache friendly yet)
          !------------------------------------------------

          if (getput == OASIS3_PUT) then
             call oasis_debug_note(subname//' loctrans calc')
             write(tstring,F01) 'pavg_',cplid
             call oasis_timer_start(tstring)
             do nf = 1,prism_coupler(cplid)%nflds
                if (prism_coupler(cplid)%avcnt(nf) > 1) then
                   rcnt = 1.0/prism_coupler(cplid)%avcnt(nf)
                   do n = 1,nsav
                      prism_coupler(cplid)%avect1%rAttr(nf,n) = &
                         prism_coupler(cplid)%avect1%rAttr(nf,n) * rcnt
                   enddo             
                endif
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG loctrans calc1 = ',cplid,nf,&
                                   prism_coupler(cplid)%avcnt(nf)
                   write(nulprt,*) subname,'  DEBUG loctrans calc2 = ',cplid,nf,&
                                   minval(prism_coupler(cplid)%avect1%rAttr(nf,:)),&
                                   maxval(prism_coupler(cplid)%avect1%rAttr(nf,:))
                endif
             enddo             
             call oasis_timer_stop(tstring)
          endif

          !------------------------------------------------
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
                call oasis_io_write_avfile(rstfile,prism_coupler(cplid)%avect1, &
                   prism_part(partid)%gsmap,nx,ny)
                call oasis_timer_stop(tstring)
                if (OASIS_debug >= 2) then
                   write(nulprt,*) subname,' at ',msec,mseclag,' WRST: ', &
                      trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1)),' ',&
                      trim(rstfile)
                   call oasis_flush(nulprt)
                endif
             endif
          endif

          !------------------------------------------------
          ! map and communicate operations
          !------------------------------------------------

          if (sndrcv) then
          if (getput == OASIS3_PUT) then
             kinfo = OASIS_sent
             call oasis_debug_note(subname//' put section')
             if (OASIS_debug >= 2) then
                write(nulprt,*) subname,' at ',msec,mseclag,' SEND: ', &
                   trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
                call oasis_flush(nulprt)
             endif
             if (sndadd /= 0.0_ip_double_p .or. sndmult /= 1.0_ip_double_p) then
                call oasis_debug_note(subname//' apply sndmult sndadd')
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG sndmult,add = ',sndmult,sndadd
                   write(nulprt,*) subname,'  DEBUG put b4 sndmult,add = ',cplid,&
                                   minval(prism_coupler(cplid)%avect1%rAttr),&
                                   maxval(prism_coupler(cplid)%avect1%rAttr)
                endif
                prism_coupler(cplid)%avect1%rAttr(:,:) = prism_coupler(cplid)%avect1%rAttr(:,:)*sndmult &
                                                         + sndadd
             endif
             if (snddiag) call oasis_advance_avdiag(prism_coupler(cplid)%avect1,mpi_comm_local)
             if (mapid > 0) then
                write(tstring,F01) 'pmap_',cplid
                call oasis_debug_note(subname//' put map')
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG put b4 map = ',cplid,&
                                   minval(prism_coupler(cplid)%avect1%rAttr),&
                                   maxval(prism_coupler(cplid)%avect1%rAttr)
                endif
                call oasis_timer_start(tstring)
                call mct_avect_zero(prism_coupler(cplid)%avect2)
                call oasis_advance_map(prism_coupler(cplid)%avect1, &
                     prism_coupler(cplid)%avect2,prism_mapper(mapid),conserv,consbfb)
                call oasis_timer_stop(tstring)
                write(tstring,F01) 'psnd_',cplid
                call oasis_debug_note(subname//' put send')
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG put b4 send = ',cplid,&
                                   minval(prism_coupler(cplid)%avect2%rAttr),&
                                   maxval(prism_coupler(cplid)%avect2%rAttr)
                endif
                call oasis_timer_start(tstring)
                call mct_waitsend(prism_router(rouid)%router)
                call mct_isend(prism_coupler(cplid)%avect2,prism_router(rouid)%router,tag)
                call oasis_timer_stop(tstring)
             else
                write(tstring,F01) 'psnd_',cplid
                call oasis_debug_note(subname//' put send')
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG put b4 send = ',cplid,&
                                   minval(prism_coupler(cplid)%avect1%rAttr),&
                                   maxval(prism_coupler(cplid)%avect1%rAttr)
                endif
                call oasis_timer_start(tstring)
                call mct_waitsend(prism_router(rouid)%router)
                call mct_isend(prism_coupler(cplid)%avect1,prism_router(rouid)%router,tag)
                call oasis_timer_stop(tstring)
             endif
          elseif (getput == OASIS3_GET) then
             call oasis_debug_note(subname//' get section')
             if (OASIS_debug >= 2 ) then
                write(nulprt,*) subname,' at ',msec,mseclag,' RECV: ', &
                   trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
                call oasis_flush(nulprt)
             endif
             if (mapid > 0) then
                call oasis_debug_note(subname//' get recv')
                write(tstring,F01) 'grcv_',cplid
                call oasis_timer_start(tstring)
                call mct_avect_zero(prism_coupler(cplid)%avect2)
                call mct_recv(prism_coupler(cplid)%avect2,prism_router(rouid)%router,tag)
                call oasis_timer_stop(tstring)
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG get af recv = ',cplid,&
                                   minval(prism_coupler(cplid)%avect2%rAttr),&
                                   maxval(prism_coupler(cplid)%avect2%rAttr)
                endif
                call oasis_debug_note(subname//' get map')
                write(tstring,F01) 'gmap_',cplid
                call oasis_timer_start(tstring)
                call mct_avect_zero(prism_coupler(cplid)%avect1)
                call oasis_advance_map(prism_coupler(cplid)%avect2, &
                     prism_coupler(cplid)%avect1,prism_mapper(mapid),conserv,consbfb)
                call oasis_timer_stop(tstring)
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG get af map = ',cplid,&
                                   minval(prism_coupler(cplid)%avect1%rAttr),&
                                   maxval(prism_coupler(cplid)%avect1%rAttr)
                endif
             else
                write(tstring,F01) 'grcv_',cplid
                call oasis_debug_note(subname//' get recv')
                call oasis_timer_start(tstring)
                call mct_recv(prism_coupler(cplid)%avect1,prism_router(rouid)%router,tag)
                call oasis_timer_stop(tstring)
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG get af recv = ',cplid,&
                                   minval(prism_coupler(cplid)%avect1%rAttr),&
                                   maxval(prism_coupler(cplid)%avect1%rAttr)
                endif
             endif
             call oasis_debug_note(subname//' apply rcvmult rcvadd')
             if (rcvadd /= 0.0_ip_double_p .or. rcvmult /= 1.0_ip_double_p) then
                prism_coupler(cplid)%avect1%rAttr(:,:) = prism_coupler(cplid)%avect1%rAttr(:,:)*rcvmult &
                                                         + rcvadd
                if (OASIS_debug >= 20) then
                   write(nulprt,*) subname,'  DEBUG rcvmult,add = ',rcvmult,rcvadd
                   write(nulprt,*) subname,'  DEBUG get af rcvmult,add = ',cplid,&
                                   minval(prism_coupler(cplid)%avect1%rAttr),&
                                   maxval(prism_coupler(cplid)%avect1%rAttr)
                endif
             endif
             if (rcvdiag) call oasis_advance_avdiag(prism_coupler(cplid)%avect1,mpi_comm_local)
          endif  ! getput
          endif  ! sndrcv

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
                   trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
                call oasis_flush(nulprt)
             endif
             write(fstring,'(A,I2.2)') '_'//trim(compnm)//'_',cplid
             call oasis_io_write_avfbf(prism_coupler(cplid)%avect1,prism_part(partid)%gsmap, &
                nx,ny,msec,fstring)
             call oasis_timer_stop(tstring)

             if (OASIS_debug >= 30) then
                call mct_avect_init(avtest,prism_coupler(cplid)%avect1,&
                                    mct_aVect_lsize(prism_coupler(cplid)%avect1))
                write(tstring,F01) 'rinp_',cplid
                call oasis_timer_start(tstring)
                call oasis_io_read_avfbf(avtest,prism_part(partid)%gsmap,msec,fstring)
                write(nulprt,*) subname,' DEBUG write/read test avfbf should be zero ',&
                                sum(prism_coupler(cplid)%avect1%rAttr-avtest%rAttr)
                call mct_avect_clean(avtest)
                call oasis_timer_stop(tstring)
             endif

          endif

          !------------------------------------------------
          ! set avcnt, avect1, ltime, and status
          !------------------------------------------------

          call oasis_debug_note(subname//' reset status')
          if (getput == OASIS3_PUT) then
             prism_coupler(cplid)%ltime = msec
             prism_coupler(cplid)%status(:) = OASIS_COMM_WAIT
             prism_coupler(cplid)%avcnt(:) = 0
             call mct_avect_zero(prism_coupler(cplid)%avect1)
             if (OASIS_debug >= 20) then
                write(nulprt,*) subname,'  DEBUG put reset status = '
             endif
          elseif (getput == OASIS3_GET) then
             prism_coupler(cplid)%ltime = msec
             prism_coupler(cplid)%status(:) = OASIS_COMM_WAIT
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
                   trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
             elseif (getput == OASIS3_GET) then
                write(nulprt,*) subname,' at ',msec,mseclag,' SKIP: ', &
                   trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
             endif
             call oasis_flush(nulprt)
          endif

       endif   ! comm_now

          !------------------------------------------------
          ! sav non-instant loctrans operations for future restart
          !   at the end of the run only
          !------------------------------------------------

          IF (mseclag + dt >= maxtime .AND. &
             getput == OASIS3_PUT .and. prism_coupler(cplid)%trans /= ip_instant) then
             call oasis_debug_note(subname//' loctrans restart write')
             write(tstring,F01) 'wtrn_',cplid
             call oasis_timer_start(tstring)
             write(vstring,'(a,i2.2,a)') 'loc',prism_coupler(cplid)%trans,'_'
             CALL oasis_io_write_avfile(rstfile,prism_coupler(cplid)%avect1, &
                prism_part(partid)%gsmap,nx,ny,nampre=TRIM(vstring))
             WRITE(vstring,'(a,i2.2,a)') 'loc',prism_coupler(cplid)%trans,'_cnt'
             CALL oasis_io_write_array(rstfile,iarray=prism_coupler(cplid)%avcnt,&
                                       ivarname=TRIM(vstring))
             call oasis_timer_stop(tstring)
             if (OASIS_debug >= 2) then
                write(nulprt,*) subname,' at ',msec,mseclag,' WTRN: ', &
                   trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1)),' ',trim(rstfile)
                call oasis_flush(nulprt)
             endif
             if (OASIS_debug >= 20) then
                write(nulprt,*) subname,'  DEBUG write loctrans restart',cplid,&
                                prism_coupler(cplid)%avcnt
                write(nulprt,*) subname,'  DEBUG write loctrans restart',cplid,&
                                minval(prism_coupler(cplid)%avect1%rAttr),&
                                maxval(prism_coupler(cplid)%avect1%rAttr)
             endif
         ENDIF

       !------------------------------------------------
       ! GET only, unpack avect1 if its coupling time
       !------------------------------------------------

       if (getput == OASIS3_GET) then
         if (time_now .and. unpack) then
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
                      trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
                   call oasis_flush(nulprt)
                endif
                write(tstring,F01) 'grin_',cplid
                call oasis_timer_start(tstring)
                if (trim(inpfile) /= trim(cspval)) then
                   call oasis_io_read_avfbf(prism_coupler(cplid)%avect1,prism_part(partid)%gsmap,&
                                            msec,filename=trim(inpfile))
                else
                   fstring = '_'//trim(compnm)
                   call oasis_io_read_avfbf(prism_coupler(cplid)%avect1,prism_part(partid)%gsmap,&
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
             do n = 1,nsav
                array(n) = prism_coupler(cplid)%avect1%rAttr(nfav,n)
             enddo
             call oasis_timer_stop(tstring)
             if (OASIS_debug >= 20) then
                write(nulprt,*) subname,'  DEBUG array copy = ',cplid,&
                                minval(array),maxval(array)
             endif
          else
             array(:) = 0.
          endif
          if (time_now) prism_coupler(cplid)%status(nfav) = OASIS_COMM_READY
       endif

       !------------------------------------------------
       ! always remember last id and last coupler time
       !------------------------------------------------

       lcouplerid = cplid
       lcouplertime = msec

       if (OASIS_debug >= 2) then
          write(nulprt,*) subname,' at ',msec,mseclag,' KINF: ',trim(vname),kinfo
       endif

    enddo  ! nc = 1,var%ncpl

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_advance_run


!-------------------------------------------------------------------

  SUBROUTINE oasis_advance_map(avs,avd,mapper,conserv,consbfb)

    ! NOTE: mask = 0 is active point according to oasis3 conserv.f

    implicit none
    type(mct_aVect)        ,intent(in)    :: avs    ! source av
    type(mct_aVect)        ,intent(inout) :: avd    ! dst av
    type(prism_mapper_type),intent(inout) :: mapper ! prism_mapper
    integer(kind=ip_i4_p)  ,intent(in),optional :: conserv  ! conserv flag
    logical                ,intent(in),optional :: consbfb  ! conserv bfb option

    integer(kind=ip_i4_p)  :: fsize,lsizes,lsized,nf,ni,n,m
    real(kind=ip_r8_p)     :: sumtmp, wts_sums, wts_sumd, zradi, zlagr
    integer(kind=ip_i4_p),allocatable :: imasks(:),imaskd(:)
    real(kind=ip_r8_p),allocatable :: areas(:),aread(:)
    real(kind=ip_r8_p),allocatable  :: av_sums(:),av_sumd(:)  ! local sums
    character(len=ic_med) :: tstring   ! timer label string
    type(mct_aVect)       :: av2g      ! for bfb sums
    logical               :: lconsbfb
    character(len=*),parameter :: subname = 'oasis_advance_map'

    call oasis_debug_enter(subname)

    lconsbfb = .true.
    if (present(consbfb)) then
       lconsbfb = consbfb
    endif

    if (mct_avect_nRattr(avs) /= mct_avect_nRattr(avd)) then
        WRITE(nulprt,*) subname,' ERROR in av num of flds'
        WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
        CALL oasis_flush(nulprt)
        CALL oasis_abort_noarg()
    endif

    call oasis_debug_note(subname//' map')
    call mct_sMat_avMult(avs, mapper%sMatP, avd)

    if (present(conserv)) then
    call oasis_debug_note(subname//' conserv')
    if (conserv /= ip_cnone) then
       fsize = mct_avect_nRattr(avs)
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
          call mct_avect_clean(av2g)
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
          call mct_avect_clean(av2g)
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
       ! compute global sums of avs
       !-------------------
       call oasis_advance_avsum(avs,av_sums,prism_part(mapper%spart)%gsmap,mpi_comm_local, &
                                mask=imasks,wts=areas,consbfb=lconsbfb)
       call oasis_advance_avsum(avd,av_sumd,prism_part(mapper%dpart)%gsmap,mpi_comm_local, &
                                mask=imaskd,wts=aread,consbfb=lconsbfb)

       if (OASIS_debug >= 20) then
          write(nulprt,*) subname,' DEBUG src sum b4 conserve ',av_sums
          write(nulprt,*) subname,' DEBUG dst sum b4 conserve ',av_sumd
       endif

       if (conserv == ip_cglobal) then
          if (wts_sumd == 0.0_ip_r8_p) then
              WRITE(nulprt,*) subname,' ERROR: conserve global wts_sumd/sums zero'
              WRITE(nulprt,*) subname,' abort by model :',compid,&
                              ' proc :',mpi_rank_local
              CALL oasis_flush(nulprt)
              CALL oasis_abort_noarg()
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
                 WRITE(nulprt,*) subname,' ERROR: conserve cglbpos av_sumd/sums'
                 WRITE(nulprt,*) subname,' abort by model :',compid,&
                                 ' proc :',mpi_rank_local
                 CALL oasis_flush(nulprt)
                 CALL oasis_abort_noarg()
             elseif (av_sumd(m) /= 0.0_ip_r8_p) then
                zlagr = av_sums(m) / av_sumd(m)
                do n = 1,lsized
                   if (imaskd(n) == 0) avd%rAttr(m,n) = avd%rAttr(m,n) * zlagr
                enddo
             endif
          enddo
       elseif (conserv == ip_cbasbal) then
          if (wts_sumd == 0.0_ip_r8_p .or. wts_sums == 0.0_ip_r8_p) then
              WRITE(nulprt,*) subname,' ERROR: conserve wts_sumd/sums zero'
              WRITE(nulprt,*) subname,' abort by model :',compid,&
                              ' proc :',mpi_rank_local
              CALL oasis_flush(nulprt)
              CALL oasis_abort_noarg()
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
                 WRITE(nulprt,*) subname,' ERROR: conserve cglbpos av_sumd/sums'
                 WRITE(nulprt,*) subname,' abort by model :',compid,&
                                 ' proc :',mpi_rank_local
                 CALL oasis_flush(nulprt)
                 CALL oasis_abort_noarg()
             elseif (av_sumd(m) /= 0.0_ip_r8_p) then
                zlagr = (av_sums(m)/av_sumd(m)) * (wts_sumd/wts_sums)
                do n = 1,lsized
                   if (imaskd(n) == 0) avd%rAttr(m,n) = avd%rAttr(m,n) * zlagr
                enddo
             endif
          enddo
       else
           WRITE(nulprt,*) subname,' ERROR: conserv option'
           WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
           CALL oasis_flush(nulprt)
           CALL oasis_abort_noarg()
       endif

       if (OASIS_debug >= 20) then
          call oasis_advance_avsum(avs,av_sums,prism_part(mapper%spart)%gsmap,mpi_comm_local, &
                                   mask=imasks,wts=areas,consbfb=lconsbfb)
          call oasis_advance_avsum(avd,av_sumd,prism_part(mapper%dpart)%gsmap,mpi_comm_local, &
                                   mask=imaskd,wts=aread,consbfb=lconsbfb)
          write(nulprt,*) subname,' DEBUG src sum af conserve ',av_sums
          write(nulprt,*) subname,' DEBUG dst sum af conserve ',av_sumd
          CALL oasis_flush(nulprt)
       endif

       deallocate(imasks,imaskd,areas,aread)
       deallocate(av_sums,av_sumd)
    endif
    endif  ! present conserve

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_advance_map

!-------------------------------------------------------------------

  SUBROUTINE oasis_advance_avsum(av,sum,gsmap,mpicom,mask,wts,consbfb)

    implicit none
    type(mct_aVect)      ,intent(in)    :: av      ! av
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
    character(len=*),parameter :: subname = 'oasis_advance_avsum'

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
        WRITE(nulprt,*) subname,' ERROR: size sum ne size av'
        WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
        CALL oasis_flush(nulprt)
        CALL oasis_abort_noarg()
    endif

    if (present(mask)) then
       if (size(mask) /= lsize) then
           WRITE(nulprt,*) subname,' ERROR: size mask ne size av'
           WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
           CALL oasis_flush(nulprt)
           CALL oasis_abort_noarg()
       endif
       do n = 1,lsize
          if (mask(n) /= 0) lwts(n) = 0.0_ip_r8_p
       enddo
    endif

    if (present(wts)) then
       if (size(wts) /= lsize) then
           WRITE(nulprt,*) subname,' ERROR: size wts ne size av'
           WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
           CALL oasis_flush(nulprt)
           CALL oasis_abort_noarg()
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
       call mct_avect_clean(av1g)
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

  SUBROUTINE oasis_advance_avdiag(av,mpicom,mask,wts)

    implicit none
    type(mct_aVect)      ,intent(in)    :: av    ! av
    integer(kind=ip_i4_p),intent(in)    :: mpicom  ! mpicom
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
    character(len=*),parameter :: subname = 'oasis_advance_avdiag'

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
           WRITE(nulprt,*) subname,' ERROR: size mask ne size av'
           WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
           CALL oasis_flush(nulprt)
           CALL oasis_abort_noarg()
       endif
       do n = 1,lsize
          if (mask(n) /= 0) lwts(n) = 0.0_ip_r8_p
       enddo
    endif

    if (present(wts)) then
       if (size(wts) /= lsize) then
           WRITE(nulprt,*) subname,' ERROR: size wts ne size av'
           WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
           CALL oasis_flush(nulprt)
           CALL oasis_abort_noarg()
       endif
       do n = 1,lsize
          lwts(n) = lwts(n) * wts(n)
       enddo
    endif

    lsum = 0.0_ip_r8_p
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

