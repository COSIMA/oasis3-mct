MODULE mod_prism_advance

    USE mod_prism_kinds
    USE mod_prism_data
    USE mod_prism_parameters
    USE mod_prism_coupler
    USE mod_prism_part
    USE mod_prism_timer
    USE mod_prism_var
    USE mod_prism_sys
    USE mod_prism_io
    USE mct_mod

    IMPLICIT NONE

    private

    public prism_advance_init
    public prism_advance_run

contains

!---------------------------------------------------------------------
  SUBROUTINE prism_advance_init()

!   ----------------------------------------------------------------
!   This routine handles initial restart and communication
!   of data for fields with positive lags
!   ----------------------------------------------------------------

    IMPLICIT none
    integer(kind=ip_i4_p) :: cplid,partid,varid
    integer(kind=ip_i4_p) :: nf,lsize,nflds
    integer(kind=ip_i4_p) :: dt,ltime,lag,getput
    integer(kind=ip_i4_p) :: msec,kinfo
    type(mct_avect)       :: avtmp  ! data read from restart
    real   (kind=ip_r8_p), allocatable :: array(:) ! data
    integer(kind=ip_i4_p) :: mseclag   ! model time + lag
    character(len=ic_xl)  :: rstfile   ! restart filename
    character(len=*),parameter :: subname = 'prism_advance_init'

    call prism_timer_start ('advance_init')

    write(nulprt,*) '   subname         at           time  time+lag   act: field '

    do cplid = 1,prism_ncoupler
       dt    = prism_coupler(cplid)%dt
       lag   = prism_coupler(cplid)%lag
       ltime = prism_coupler(cplid)%ltime
       getput= prism_coupler(cplid)%getput
       rstfile=trim(prism_coupler(cplid)%rstfile)
       partid= prism_coupler(cplid)%partID
       msec = 0-lag   ! effective model time of restart

       !------------------------------------------------
       ! check that lag is reasonable
       !------------------------------------------------

       if (lag > dt .or. lag <= -dt) then
          write(nulprt,*) subname,' ERROR lag out of dt range cplid/dt/lag=',cplid,dt,lag
          call prism_sys_abort(compid,subname,' ERROR lag gt dt')
       endif

       !------------------------------------------------
       ! read restart and call advance for the current fields
       !------------------------------------------------

       if (getput == PRISM_PUT .and. lag > 0) then
          mseclag = msec + lag
          if (len_trim(rstfile) < 1) then
             write(nulprt,*) subname,' ERROR restart undefined'
             call prism_sys_abort(compid,subname,' ERROR restart undefined')
          endif
          lsize = mct_aVect_lsize(prism_coupler(cplid)%aVect1)
          nflds = mct_aVect_nRAttr(prism_coupler(cplid)%aVect1)
          call mct_aVect_init(avtmp,rlist=prism_coupler(cplid)%fldlist,lsize=lsize)
          write(nulprt,*) subname,' at ',msec,mseclag,' RRST: ',&
             trim(prism_coupler(cplid)%fldlist),' ',trim(rstfile)
          call prism_io_read_avfile(trim(rstfile),avtmp,prism_part(partid)%gsmap)
          allocate(array(lsize))
          do nf = 1,nflds
             varid = prism_coupler(cplid)%varid(nf)
             array(1:lsize) = avtmp%rAttr(nf,1:lsize)
             call prism_advance_run(PRISM_Out,varid,msec,array,kinfo)
          enddo
          deallocate(array)
          call mct_avect_clean(avtmp)
       endif
    enddo ! cplid
    call prism_timer_stop ('advance_init')

  end SUBROUTINE prism_advance_init
!---------------------------------------------------------------------
  SUBROUTINE prism_advance_run(mop,varid,msec,array,kinfo)

    IMPLICIT none
!   ----------------------------------------------------------------
    integer(kind=ip_i4_p), intent(in)    :: mop      ! PRISM_Out or PRISM_In
    INTEGER(kind=ip_i4_p), intent(in)    :: varid    ! prism_var id
    INTEGER(kind=ip_i4_p), intent(in)    :: msec     ! model time
    REAL   (kind=ip_r8_p), intent(inout) :: array(:) ! data
    INTEGER(kind=ip_i4_p), intent(out)   :: kinfo    ! status
!   ----------------------------------------------------------------
    character(len=ic_lvar):: vname
    integer(kind=ip_i4_p) :: cplid,rouid,mapid,partid
    integer(kind=ip_i4_p) :: nfav,nsav,nsa,n,nc,nf
    integer(kind=ip_i4_p) :: tag,dt,ltime,lag,getput,maxtime,conserv
    logical               :: sndrcv,output,input,unpack
    character(len=ic_xl)  :: rstfile   ! restart filename
    character(len=ic_xl)  :: inpfile   ! input filename
    integer(kind=ip_i4_p) :: nx,ny
    integer(kind=ip_i4_p) :: mseclag   ! model time + lag
    real(kind=ip_r8_p)    :: rcnt      ! 1./cnt
    character(len=ic_med) :: tstring   ! timer label string
    character(len=ic_med) :: fstring   ! output file string
    character(len=ic_med) :: cstring   ! temporary string
    logical               :: comm_now  ! time to communicate
    logical               :: time_now  ! coupling time
    type(mct_avect)       :: avtest    ! temporary
!    type(mct_avect),pointer  :: avect1, avect2
!    type(mct_sMatP),pointer  :: sMatP
!    type(mct_router),pointer :: router
    character(len=*),parameter :: subname = 'prism_advance_run '
    character(len=*),parameter :: F01 = '(a,i3.3)'
!   ----------------------------------------------------------------

    kinfo = PRISM_OK
    vname = prism_var(varid)%name

    !------------------------------------------------
    ! validate mop
    !------------------------------------------------

    if (mop /= PRISM_Out .and. mop /= PRISM_In) then
       write(nulprt,*) subname,' at ',msec,mseclag,'  ERROR: ',trim(vname)
       write(nulprt,*) subname,' ERROR mop invalid ',mop
       call prism_sys_abort(compid,subname,' ERROR model op invalid')
    endif

    !------------------------------------------------
    ! for all the couplers associated with this var
    !------------------------------------------------

    do nc = 1,prism_var(varid)%ncpl
       cplid = prism_var(varid)%cpl(nc)
       rouid = prism_coupler(cplid)%routerid
       mapid = prism_coupler(cplid)%mapperid
       tag   = prism_coupler(cplid)%tag
       dt    = prism_coupler(cplid)%dt
       lag   = prism_coupler(cplid)%lag
       ltime = prism_coupler(cplid)%ltime
       getput= prism_coupler(cplid)%getput
       sndrcv= prism_coupler(cplid)%sndrcv
       rstfile=trim(prism_coupler(cplid)%rstfile)
       inpfile=trim(prism_coupler(cplid)%inpfile)
       maxtime = prism_coupler(cplid)%maxtime
       output= prism_coupler(cplid)%output
       input = prism_coupler(cplid)%input
       partid= prism_coupler(cplid)%partID
       conserv = prism_coupler(cplid)%conserv
!      avect1 => prism_coupler(cplid)%avect1
!      avect2 => prism_coupler(cplid)%avect2
!      sMatP  => prism_mapper(mapid)%sMatP
!      router => prism_router(rouid)%router

       unpack = (sndrcv .or. input)

       if (prism_part(partid)%nx >= 1) then
          nx = prism_part(partid)%nx
          ny = prism_part(partid)%ny
       else
          nx = prism_part(partid)%gsize
          ny = 1
       endif

       !------------------------------------------------
       ! check that lag is reasonable
       !------------------------------------------------

       if (abs(lag) > dt) then
          write(nulprt,*) subname,' ERROR lag gt dt for cplid',cplid
          call prism_sys_abort(compid,subname,' ERROR lag gt dt')
       endif

       !------------------------------------------------
       ! check that model op matches coupler op
       !------------------------------------------------

       if ((mop == PRISM_Out .and. getput == PRISM_PUT) .or. &
           (mop == PRISM_In  .and. getput == PRISM_GET)) then
          !-continue
       else
          write(nulprt,*) subname,' ERROR model op does not match coupler op',mop,getput
          call prism_sys_abort(compid,subname,' ERROR model op does not match coupler op')
       endif

       !------------------------------------------------
       ! compute lag time, only on put side
       ! set time now, is it a coupling period?
       !------------------------------------------------

       if (getput == PRISM_PUT) then
          mseclag = msec + lag
       elseif (getput == PRISM_GET) then
          mseclag = msec
       endif

       time_now = .false.
       if (mod(mseclag,dt) == 0) time_now = .true.

       !------------------------------------------------
       ! check that model isn't going backwards
       !------------------------------------------------

       if (lcouplertime /= ispval .and. msec < lcouplertime) then
          write(nulprt,*) subname,' at ',msec,mseclag,'  ERROR: ',trim(vname)
          write(nulprt,*) subname,' ERROR model seems to be running backwards',msec,lcouplertime
          call prism_sys_abort(compid,subname,' ERROR model running backwards')
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
             write(nulprt,*) subname,' ERROR coupling skipped at earlier time, potential deadlock '
             write(nulprt,*) subname,' my coupler = ',cplid,' variable = ',trim(prism_coupler(cplid)%fldlist)
             write(nulprt,*) subname,' current time = ',msec,' mseclag = ',mseclag
             write(nulprt,*) subname,' skipped coupler = ',n,' variable = ',trim(prism_coupler(n)%fldlist)
             write(nulprt,*) subname,' skipped coupler last time and dt = ',prism_coupler(n)%ltime,prism_coupler(n)%dt
             call prism_sys_abort(compid,subname,' ERROR model timestep does not match coupling timestep')
          endif
          if ((prism_coupler(n)%ltime /= ispval) .and. &
              (sndrcv .and. prism_coupler(n)%sndrcv .and. getput == PRISM_GET) .and. &
              (prism_coupler(cplid)%seq > prism_coupler(n)%seq) .and. &
              (msec >= prism_coupler(n)%ltime + prism_coupler(n)%dt)) then
             write(nulprt,*) subname,' ERROR coupling sequence out of order, potential deadlock '
             write(nulprt,*) subname,' my coupler = ',cplid,' variable = ',trim(prism_coupler(cplid)%fldlist)
             write(nulprt,*) subname,' sequence number = ',prism_coupler(cplid)%seq
             write(nulprt,*) subname,' current time = ',msec,' mseclag = ',mseclag
             write(nulprt,*) subname,' skipped coupler = ',n,' variable = ',trim(prism_coupler(n)%fldlist)
             write(nulprt,*) subname,' skipped coupler last time and dt = ',prism_coupler(n)%ltime,prism_coupler(n)%dt
             write(nulprt,*) subname,' skipped sequence number = ',prism_coupler(n)%seq
             call prism_sys_abort(compid,subname,' ERROR model sequence does not match coupling sequence')
          endif
       enddo

       !------------------------------------------------
       ! compute field index and check sizes
       !------------------------------------------------

       nfav = mct_avect_indexra(prism_coupler(cplid)%avect1,trim(vname))
       nsav = mct_avect_lsize(prism_coupler(cplid)%avect1)
       nsa = size(array)

       if (nsav /= nsa) then
          write(nulprt,*) subname,' at ',msec,mseclag,'  ERROR: ',trim(vname)
          write(nulprt,*) subname,' ERROR sizes ',nsav,nsa
          call prism_sys_abort(compid,subname,'ERROR sizes')
       endif

       !------------------------------------------------
       ! update avect1 on put side, apply appropriate transform
       ! if its coupling time, set status of this var to ready
       !------------------------------------------------

       if (getput == PRISM_PUT) then

          write(tstring,F01) 'pcpy_',cplid
          call prism_timer_start(tstring)

          cstring = 'none'
          if (prism_coupler(cplid)%trans == ip_average) then
             cstring = 'average'
             do n = 1,nsav
                prism_coupler(cplid)%avect1%rAttr(nfav,n) = &
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) + array(n)
             enddo
             prism_coupler(cplid)%avcnt(nfav) = prism_coupler(cplid)%avcnt(nfav) + 1

          elseif (prism_coupler(cplid)%trans == ip_accumul) then
             cstring = 'accumul'
             do n = 1,nsav
                prism_coupler(cplid)%avect1%rAttr(nfav,n) = &
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) + array(n)
             enddo
             prism_coupler(cplid)%avcnt(nfav) = 1

          elseif (prism_coupler(cplid)%trans == ip_max) then
             cstring = 'max'
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
             do n = 1,nsav
                if (prism_coupler(cplid)%avcnt(nfav) == 0) then
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) = array(n)
                else
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) = &
                      min(prism_coupler(cplid)%avect1%rAttr(nfav,n),array(n))
                endif
             enddo
             prism_coupler(cplid)%avcnt(nfav) = 1

          elseif (prism_coupler(cplid)%trans == ip_instant) then
             if (time_now) then
                cstring = 'instant'
                do n = 1,nsav
                   prism_coupler(cplid)%avect1%rAttr(nfav,n) = array(n)
                enddo
                prism_coupler(cplid)%avcnt(nfav) = 1
             endif

          else
             write(nulprt,*) subname,' ERROR: trans not known ',prism_coupler(cplid)%trans
             call prism_sys_abort(compid,subname,' ERROR: trans not known')
          endif
          call prism_timer_stop(tstring)

          if (PRISM_Debug >= 2 .and. trim(cstring) /= 'none') then
             write(nulprt,*) subname,' at ',msec,mseclag,' PACK: ',trim(vname),' ',trim(cstring)
          endif

          if (time_now) then
             prism_coupler(cplid)%status(nfav) = PRISM_COMM_READY
             kinfo = PRISM_sent
          endif
       endif

       !------------------------------------------------
       ! decide if it's time to communicate based on 
       ! time.  also, on the put side, status of all vars
       ! must be ready which means all vars have called put.
       ! on get side, all ready means all vars have unpacked
       ! from last get.
       !------------------------------------------------

       comm_now = .false.
       if (time_now) then
          comm_now = .true.
          do nf = 1,prism_coupler(cplid)%nflds
             if (prism_coupler(cplid)%status(nf) /= PRISM_COMM_READY) then
                comm_now = .false.
                if (PRISM_Debug >= 5) then
                   write(nulprt,*) subname,' at ',msec,mseclag,' STAT: ',nf,' NOT READY'
                endif
             else
                if (PRISM_Debug >= 5) then
                   write(nulprt,*) subname,' at ',msec,mseclag,' STAT: ',nf,' READY'
                endif
             endif
          enddo
       endif

       if (comm_now) then

          !------------------------------------------------
          ! this is the time critical bit, we need to make sure the
          ! model is truly advancing in time when comms are called.
          ! must ignore the initial call, ltime = 0
          !------------------------------------------------

          if (prism_coupler(cplid)%ltime /= ispval .and. msec <= prism_coupler(cplid)%ltime) then
             write(nulprt,*) subname,' ERROR: model did not advance in time correctly',msec,prism_coupler(cplid)%ltime
             call prism_sys_abort(compid,subname,' ERROR: model did not advance in time correctly')
          endif

          !------------------------------------------------
          ! average as needed (not cache friendly yet)
          !------------------------------------------------

          if (getput == PRISM_PUT) then
             write(tstring,F01) 'pavg_',cplid
             call prism_timer_start(tstring)
             do nf = 1,prism_coupler(cplid)%nflds
                if (prism_coupler(cplid)%avcnt(nf) > 1) then
                   rcnt = 1.0/prism_coupler(cplid)%avcnt(nf)
                   do n = 1,nsav
                      prism_coupler(cplid)%avect1%rAttr(nf,n) = prism_coupler(cplid)%avect1%rAttr(nf,n) * rcnt
                   enddo             
                endif
             enddo             
             call prism_timer_stop(tstring)
          endif

          !------------------------------------------------
          ! past namcouple runtime (maxtime) no communication
          ! do restart
          !------------------------------------------------

          if (mseclag > maxtime) then
             sndrcv = .false.   ! turn off communication
             unpack = .false.   ! nothing to unpack
             if (getput == PRISM_PUT .and. lag > 0) then
!tcx generate restart always       if (getput == PRISM_PUT) then
                write(tstring,F01) 'wrst_',cplid
                call prism_timer_start(tstring)
                call prism_io_write_avfile(rstfile,prism_coupler(cplid)%avect1, &
                   prism_part(partid)%gsmap,nx,ny)
                call prism_timer_stop(tstring)
                if (PRISM_Debug > 0) then
                   write(nulprt,*) subname,' at ',msec,mseclag,' WRST: ', &
                      trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1)),' ',trim(rstfile)
                   call prism_sys_flush(nulprt)
                endif
             endif
          endif

          !------------------------------------------------
          ! map and communicate operations
          !------------------------------------------------

          if (sndrcv) then
          if (getput == PRISM_PUT) then
             if (PRISM_Debug > 0) then
                write(nulprt,*) subname,' at ',msec,mseclag,' SEND: ', &
                   trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
                call prism_sys_flush(nulprt)
             endif
             if (mapid > 0) then
                write(tstring,F01) 'pmap_',cplid
                call prism_timer_start(tstring)
                call mct_avect_zero(prism_coupler(cplid)%avect2)
                call prism_advance_map(prism_coupler(cplid)%avect1, &
                     prism_coupler(cplid)%avect2,prism_mapper(mapid),conserv)
                call prism_timer_stop(tstring)
                write(tstring,F01) 'psnd_',cplid
                call prism_timer_start(tstring)
                call mct_waitsend(prism_router(rouid)%router)
                call mct_isend(prism_coupler(cplid)%avect2,prism_router(rouid)%router,tag)
                call prism_timer_stop(tstring)
             else
                write(tstring,F01) 'psnd_',cplid
                call prism_timer_start(tstring)
                call mct_waitsend(prism_router(rouid)%router)
                call mct_isend(prism_coupler(cplid)%avect1,prism_router(rouid)%router,tag)
                call prism_timer_stop(tstring)
             endif
          elseif (getput == PRISM_GET) then
             if (PRISM_Debug > 0) then
                write(nulprt,*) subname,' at ',msec,mseclag,' RECV: ', &
                   trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
                call prism_sys_flush(nulprt)
             endif
             if (mapid > 0) then
                write(tstring,F01) 'grcv_',cplid
                call prism_timer_start(tstring)
                call mct_avect_zero(prism_coupler(cplid)%avect2)
                call mct_recv(prism_coupler(cplid)%avect2,prism_router(rouid)%router,tag)
                call prism_timer_stop(tstring)
                write(tstring,F01) 'gmap_',cplid
                call prism_timer_start(tstring)
                call mct_avect_zero(prism_coupler(cplid)%avect1)
                call prism_advance_map(prism_coupler(cplid)%avect2, &
                     prism_coupler(cplid)%avect1,prism_mapper(mapid),conserv)
                call prism_timer_stop(tstring)
             else
                write(tstring,F01) 'grcv_',cplid
                call prism_timer_start(tstring)
                call mct_recv(prism_coupler(cplid)%avect1,prism_router(rouid)%router,tag)
                call prism_timer_stop(tstring)
             endif
          endif  ! getput
          endif  ! sndrcv

          if (output) then
             write(tstring,F01) 'wout_',cplid
             call prism_timer_start(tstring)
             if (PRISM_Debug > 0) then
                write(nulprt,*) subname,' at ',msec,mseclag,' WRIT: ', &
                   trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
                call prism_sys_flush(nulprt)
             endif
             fstring = '_'//trim(compnm)
             call prism_io_write_avfbf(prism_coupler(cplid)%avect1,prism_part(partid)%gsmap, &
                nx,ny,msec,fstring)
             call prism_timer_stop(tstring)
!tcx test-----------
             if (PRISM_Debug >= 5) then
                call mct_avect_init(avtest,prism_coupler(cplid)%avect1,mct_aVect_lsize(prism_coupler(cplid)%avect1))
                write(tstring,F01) 'rinp_',cplid
                call prism_timer_start(tstring)
                call prism_io_read_avfbf(avtest,prism_part(partid)%gsmap,msec,fstring)
                write(nulprt,*) subname,' tcx test avfbf should be zero ',sum(prism_coupler(cplid)%avect1%rAttr-avtest%rAttr)
                call mct_avect_clean(avtest)
                call prism_timer_stop(tstring)
             endif
!tcx test------------
          endif

          !------------------------------------------------
          ! set avcnt, avect1, ltime, and status
          !------------------------------------------------

          if (getput == PRISM_PUT) then
             prism_coupler(cplid)%ltime = msec
             prism_coupler(cplid)%status(:) = PRISM_COMM_WAIT
             prism_coupler(cplid)%avcnt(:) = 0
             call mct_avect_zero(prism_coupler(cplid)%avect1)
          elseif (getput == PRISM_GET) then
             prism_coupler(cplid)%ltime = msec
             prism_coupler(cplid)%status(:) = PRISM_COMM_WAIT
          endif

       else

          !------------------------------------------------
          ! no action, document
          !------------------------------------------------

          if (PRISM_Debug >= 5) then
             if (getput == PRISM_PUT) then
                write(nulprt,*) subname,' at ',msec,mseclag,' SKIP: ', &
                   trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
             elseif (getput == PRISM_GET) then
                write(nulprt,*) subname,' at ',msec,mseclag,' SKIP: ', &
                   trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
             endif
             call prism_sys_flush(nulprt)
          endif

       endif   ! comm_now

       !------------------------------------------------
       ! GET only, unpack avect1 if its coupling time
       !------------------------------------------------

       if (getput == PRISM_GET) then
         if (time_now .and. unpack) then
             if (input) then
                if (PRISM_Debug > 0) then
                   write(nulprt,*) subname,' at ',msec,mseclag,' READ: ', &
                      trim(mct_avect_exportRList2c(prism_coupler(cplid)%avect1))
                   call prism_sys_flush(nulprt)
                endif
                write(tstring,F01) 'grin_',cplid
                call prism_timer_start(tstring)
                if (trim(inpfile) /= trim(cspval)) then
                   call prism_io_read_avfbf(prism_coupler(cplid)%avect1,prism_part(partid)%gsmap,msec,filename=trim(inpfile))
                else
                   fstring = '_'//trim(compnm)
                   call prism_io_read_avfbf(prism_coupler(cplid)%avect1,prism_part(partid)%gsmap,msec,string=fstring)
                endif
                call prism_timer_stop(tstring)
             endif
             if (PRISM_Debug >= 2) then
                write(nulprt,*) subname,' at ',msec,mseclag,' UPCK: ',trim(vname)
             endif
             write(tstring,F01) 'gcpy_',cplid
             call prism_timer_start(tstring)
             do n = 1,nsav
                array(n) = prism_coupler(cplid)%avect1%rAttr(nfav,n)
             enddo
             call prism_timer_stop(tstring)
             kinfo = PRISM_recvd
          else
             array(:) = 0.
             kinfo = PRISM_OK
          endif
          if (time_now) prism_coupler(cplid)%status(nfav) = PRISM_COMM_READY
       endif

       !------------------------------------------------
       ! always remember last id and last coupler time
       !------------------------------------------------

       lcouplerid = cplid
       lcouplertime = msec

    enddo  ! nc = 1,var%ncpl

    return

  END SUBROUTINE prism_advance_run


!-------------------------------------------------------------------

  SUBROUTINE prism_advance_map(avs,avd,mapper,conserv)

    ! NOTE: mask = 0 is active point according to oasis3 conserv.f

    implicit none
    type(mct_aVect)        ,intent(in)    :: avs    ! source av
    type(mct_aVect)        ,intent(inout) :: avd    ! dst av
    type(prism_mapper_type),intent(inout) :: mapper ! prism_mapper
    integer(kind=ip_i4_p)  ,intent(in),optional :: conserv  ! conserv flag

    integer(kind=ip_i4_p)  :: fsize,lsizes,lsized,nf,n,m
    real(kind=ip_r8_p)     :: sumtmp, wts_sums, wts_sumd, zradi, zlagr
    integer(kind=ip_i4_p),allocatable :: imasks(:),imaskd(:)
    real(kind=ip_r8_p),allocatable :: areas(:),aread(:)
    real(kind=ip_r8_p),allocatable  :: av_sums(:),av_sumd(:)  ! local sums
    character(len=ic_med) :: tstring   ! timer label string
    character(len=*),parameter :: subname = 'prism_advance_map'

    if (mct_avect_nRattr(avs) /= mct_avect_nRattr(avd)) then
       call prism_sys_abort(compid,subname,' ERROR in av num of flds')
    endif

    write(tstring,'(A)') 'map_smat'
    call prism_timer_start(tstring)
    call mct_sMat_avMult(avs, mapper%sMatP, avd)
    call prism_timer_stop(tstring)

    if (present(conserv)) then
    if (conserv /= ip_cnone) then
       write(tstring,'(A)') 'map_conserv'
       call prism_timer_start(tstring)
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
       sumtmp = 0.0_ip_r8_p
       do n = 1,lsizes
          if (imasks(n) == 0) sumtmp = sumtmp + areas(n)
       enddo
       call prism_mpi_sum(sumtmp,wts_sums,mpi_comm_local,string=subname//':wts_sums',all=.true.)

       !-------------------
       ! extract mask and area and compute sum of masked area for destination
       !-------------------
       lsized = mct_avect_lsize(mapper%av_md)
       allocate(imaskd(lsized),aread(lsized))
       nf = mct_aVect_indexIA(mapper%av_md,'mask')
       imaskd(:) = mapper%av_md%iAttr(nf,:)
       nf = mct_aVect_indexRA(mapper%av_md,'area')
       aread(:) = mapper%av_md%rAttr(nf,:)*zradi
       sumtmp = 0.0_ip_r8_p
       do n = 1,lsized
          if (imaskd(n) == 0) sumtmp = sumtmp + aread(n)
       enddo
       call prism_mpi_sum(sumtmp,wts_sumd,mpi_comm_local,string=subname//':wts_sumd',all=.true.)

       ! tcx debug
       write(nulprt,*) subname,'tcxms ',minval(imasks),maxval(imasks),sum(imasks)
       write(nulprt,*) subname,'tcxmd ',minval(imaskd),maxval(imaskd),sum(imaskd)
       write(nulprt,*) subname,'tcxas ',minval(areas),maxval(areas),sum(areas)
       write(nulprt,*) subname,'tcxad ',minval(aread),maxval(aread),sum(aread)
       write(nulprt,*) subname,'tcxwsums  ',wts_sums,wts_sumd

       !-------------------
       ! compute global sums of avs
       !-------------------
       call prism_advance_avsum(avs,av_sums,mpi_comm_local,mask=imasks,wts=areas)
       call prism_advance_avsum(avd,av_sumd,mpi_comm_local,mask=imaskd,wts=aread)

       ! tcx debug
       write(nulprt,*) subname,' tcx1avs ',av_sums
       write(nulprt,*) subname,' tcx1avd ',av_sumd

       if (conserv == ip_cglobal) then
          if (wts_sumd == 0.0_ip_r8_p) then
             call prism_sys_abort(compid,subname,' ERROR: conserve global wts_sumd/sums zero')
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
                call prism_sys_abort(compid,subname,' ERROR: conserve cglbpos av_sumd/sums')
             elseif (av_sumd(m) /= 0.0_ip_r8_p) then
                zlagr = av_sums(m) / av_sumd(m)
                do n = 1,lsized
                   if (imaskd(n) == 0) avd%rAttr(m,n) = avd%rAttr(m,n) * zlagr
                enddo
             endif
          enddo
       elseif (conserv == ip_cbasbal) then
          if (wts_sumd == 0.0_ip_r8_p .or. wts_sums == 0.0_ip_r8_p) then
             call prism_sys_abort(compid,subname,' ERROR: conserve wts_sumd/sums zero')
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
                call prism_sys_abort(compid,subname,' ERROR: conserve cglbpos av_sumd/sums')
             elseif (av_sumd(m) /= 0.0_ip_r8_p) then
                zlagr = (av_sums(m)/av_sumd(m)) * (wts_sumd/wts_sums)
                do n = 1,lsized
                   if (imaskd(n) == 0) avd%rAttr(m,n) = avd%rAttr(m,n) * zlagr
                enddo
             endif
          enddo
       else
          call prism_sys_abort(compid,subname,' ERROR: conserv option')
       endif

       ! tcx debug
       call prism_advance_avsum(avs,av_sums,mpi_comm_local,mask=imasks,wts=areas)
       call prism_advance_avsum(avd,av_sumd,mpi_comm_local,mask=imaskd,wts=aread)
       write(nulprt,*) subname,' tcx2avs ',av_sums
       write(nulprt,*) subname,' tcx2avd ',av_sumd

       deallocate(imasks,imaskd,areas,aread)
       deallocate(av_sums,av_sumd)
       call prism_timer_stop(tstring)
    endif
    endif

  END SUBROUTINE prism_advance_map

!-------------------------------------------------------------------

  SUBROUTINE prism_advance_avsum(av,sum,mpicom,mask,wts)

    implicit none
    type(mct_aVect)      ,intent(in)    :: av    ! av
    real(kind=ip_r8_p)   ,intent(inout) :: sum(:)  ! sum of av fields
    integer(kind=ip_i4_p),intent(in)    :: mpicom  ! mpicom
    integer(kind=ip_i4_p),intent(in),optional :: mask(:) ! mask to apply to av
    real(kind=ip_r8_p)   ,intent(in),optional :: wts(:)  ! wts to apply to av

    integer(kind=ip_i4_p) :: n,m
    integer(kind=ip_i4_p) :: lsize,fsize        ! local size of av, number of flds in av
    real(kind=ip_r8_p),allocatable  :: lsum(:)  ! local sums
    real(kind=ip_r8_p),allocatable  :: lwts(:)  ! local wts taking into account mask and wts
    character(len=*),parameter :: subname = 'prism_advance_avsum'

    fsize = mct_avect_nRattr(av)
    lsize = mct_avect_lsize(av)

    allocate(lsum(fsize))
    lsum = 0.0_ip_r8_p
    allocate(lwts(lsize))
    lwts = 1.0_ip_r8_p

    if (size(sum) /= fsize) then
       call prism_sys_abort(compid,subname,' ERROR: size sum ne size av')
    endif

    if (present(mask)) then
       if (size(mask) /= lsize) then
          call prism_sys_abort(compid,subname,' ERROR: size mask ne size av')
       endif
       do n = 1,lsize
          if (mask(n) /= 0) lwts(n) = 0.0_ip_r8_p
       enddo
    endif

    if (present(wts)) then
       if (size(wts) /= lsize) then
          call prism_sys_abort(compid,subname,' ERROR: size wts ne size av')
       endif
       do n = 1,lsize
          lwts(n) = lwts(n) * wts(n)
       enddo
    endif

    lsum = 0.0_ip_r8_p
    do n = 1,lsize
    do m = 1,fsize
       lsum(m) = lsum(m) + av%rAttr(m,n)*lwts(n)
    enddo
    enddo

    call prism_mpi_sum(lsum,sum,mpicom,string=trim(subname)//':sum',all=.true.)

    deallocate(lsum)
    deallocate(lwts)

  END SUBROUTINE prism_advance_avsum

!-------------------------------------------------------------------
END MODULE mod_prism_advance

