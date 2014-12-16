
!> Auxiliary OASIS user interfaces

MODULE mod_oasis_auxiliary_routines
!---------------------------------------------------------------------

    USE mod_oasis_kinds
    USE mod_oasis_data
    USE mod_oasis_parameters
    USE mod_oasis_coupler
    USE mod_oasis_timer
    USE mod_oasis_var
    USE mod_oasis_sys
    USE mod_oasis_io
    USE mod_oasis_mpi
    USE mct_mod

    implicit none
    private

    public oasis_get_ncpl
    public oasis_put_inquire
    public oasis_get_freqs

#include "oasis_os.h"

    integer(kind=ip_i4_p)     istatus(MPI_STATUS_SIZE)

!---------------------------------------------------------------------
  CONTAINS
!---------------------------------------------------------------------

!> Return the number of namcouple couplings for a variable

    SUBROUTINE oasis_get_ncpl(varid, ncpl, kinfo)

    IMPLICIT none
    !-------------------------------------
    INTEGER(kind=ip_i4_p) , INTENT(in)  :: varid   !< variable id
    INTEGER(kind=ip_i4_p) , INTENT(out) :: ncpl    !< number of namcouple couplings
    INTEGER(kind=ip_i4_p) , INTENT(out) :: kinfo   !< return code
    !-------------------------------------
    CHARACTER(len=ic_lvar)  :: vname
    CHARACTER(len=*),PARAMETER :: subname = 'oasis_get_ncpl'
    !-------------------------------------

    CALL oasis_debug_enter(subname)

    IF (mpi_comm_local == MPI_COMM_NULL) THEN
        WRITE(nulprt,*) subname,' ERROR called on non coupling task'
        WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
        CALL oasis_flush(nulprt)
        CALL oasis_abort_noarg()
    ENDIF

    kinfo = OASIS_OK
    vname = prism_var(varid)%name
    
    IF (varid == OASIS_Var_Uncpl) THEN
        WRITE(nulprt,*) subname, &
           ' Routine is called for a variable not in namcouple: it will not be sent'
        CALL oasis_flush(nulprt)
        CALL oasis_abort_noarg()
        RETURN
    ENDIF
    
    ncpl  = prism_var(varid)%ncpl
    
    IF (ncpl <= 0) THEN
        IF (OASIS_debug >= 2) WRITE(nulprt,*) subname,' variable not coupled ',&
                               TRIM(vname)
        CALL oasis_debug_exit(subname)
        RETURN
    ELSE 
        IF (OASIS_debug >= 2)  WRITE(nulprt,*) subname,' Variable: ',TRIM(vname),&
                               ' used in ',ncpl,' couplings' 
        CALL oasis_flush(nulprt)
    ENDIF
    
  END SUBROUTINE oasis_get_ncpl
!---------------------------------------------------------------------

!> Returns the coupling periods for a given variable

  SUBROUTINE oasis_get_freqs(varid, mop, ncpl, cpl_freqs, kinfo)

    IMPLICIT none
    !-------------------------------------
    INTEGER(kind=ip_i4_p) , INTENT(in)  :: varid          !< variable id
    INTEGER(kind=ip_i4_p) , INTENT(in)  :: ncpl           !< number of namcouple couplings
    INTEGER(kind=ip_i4_p) , INTENT(in)  :: mop            !< OASIS_Out or OASIS_In type
    INTEGER(kind=ip_i4_p) , INTENT(out) :: cpl_freqs(ncpl)!< coupling period (sec)
    INTEGER(kind=ip_i4_p) , INTENT(out) :: kinfo          !< return code
    !-------------------------------------
    CHARACTER(len=ic_lvar)  :: vname
    INTEGER(kind=ip_i4_p)   :: ncpl_calc, cplid, nc
    CHARACTER(len=*),PARAMETER :: subname = 'oasis_get_freqs'
    !-------------------------------------

    CALL oasis_debug_enter(subname)

    IF (mpi_comm_local == MPI_COMM_NULL) THEN
        WRITE(nulprt,*) subname,' ERROR called on non coupling task'
        WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
        CALL oasis_flush(nulprt)
        CALL oasis_abort_noarg()
    ENDIF

    kinfo = OASIS_OK
    vname = prism_var(varid)%name
    
    IF (varid == OASIS_Var_Uncpl) THEN
        WRITE(nulprt,*) subname, &
           ' Routine is called for a variable not in namcouple: it will not be sent'
        CALL oasis_flush(nulprt)
        CALL oasis_abort_noarg()
        RETURN
    ENDIF
    
    ncpl_calc  = prism_var(varid)%ncpl

    IF (ncpl_calc /= ncpl) THEN
        WRITE(nulprt,*) subname,' Wrong number of couplings for variable: ',TRIM(vname), &
                        ncpl_calc, ncpl
        CALL oasis_flush(nulprt)
        CALL oasis_abort_noarg()
    ENDIF
    
    IF (ncpl <= 0) THEN
        IF (OASIS_debug >= 2) WRITE(nulprt,*) subname,' variable not coupled ',&
                               TRIM(vname)
        CALL oasis_flush(nulprt)
        CALL oasis_debug_exit(subname)
        RETURN
    ENDIF

    DO nc = 1,prism_var(varid)%ncpl
      cplid           = prism_var(varid)%cpl(nc)
      IF (mop == OASIS_Out) THEN
          cpl_freqs(nc)   = prism_coupler_put(cplid)%dt
      ENDIF
      IF (mop == OASIS_In ) THEN
          cpl_freqs(nc)   = prism_coupler_get(cplid)%dt
      ENDIF

      IF (OASIS_Debug >=2 ) THEN
          WRITE(nulprt,*)  subname,' Coupling frequency of this field ',TRIM(vname),&
                           ' for coupling ',nc, ' is ',cpl_freqs(nc)
      ENDIF

      IF (cpl_freqs(nc) .le. 0) THEN
          WRITE(nulprt,*) subname,' The coupling frequency is < or equal to 0'
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
      ENDIF
    ENDDO

  END SUBROUTINE oasis_get_freqs
!---------------------------------------------------------------------

!> Return kinfo code indicating type of action carried out for a variable at the specified time

  SUBROUTINE oasis_put_inquire(varid,msec,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in)  :: varid   !< variable id
    integer(kind=ip_i4_p) , intent(in)  :: msec    !< model time in seconds
    integer(kind=ip_i4_p) , intent(out) :: kinfo   !< return code
    !-------------------------------------
    character(len=ic_lvar)     :: vname
    INTEGER(kind=ip_i4_p)      :: ncpl, nc, cplid
    INTEGER(kind=ip_i4_p)      :: lag, mseclag, trans, dt, getput, maxtime
    LOGICAL                    :: time_now, sndrcv, output
    character(len=*),parameter :: subname = 'oasis_put_inquire'
    !-------------------------------------

    CALL oasis_debug_enter(subname)

    IF (mpi_comm_local == MPI_COMM_NULL) THEN
        WRITE(nulprt,*) subname,' ERROR called on non coupling task'
        WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
        CALL oasis_flush(nulprt)
        CALL oasis_abort_noarg()
    ENDIF

    kinfo = OASIS_OK
    vname = prism_var(varid)%name
    
    IF (varid == OASIS_Var_Uncpl) THEN
        WRITE(nulprt,*) subname, &
           ' Routine oasis_put is called for a variable not in namcouple: it will not be sent'
        CALL oasis_flush(nulprt)
        CALL oasis_abort_noarg()
        RETURN
    ENDIF
    
    ncpl  = prism_var(varid)%ncpl
    
    IF (ncpl <= 0) THEN
        IF (OASIS_debug >= 2) WRITE(nulprt,*) subname,' variable not coupled ',&
                               TRIM(vname)
        CALL oasis_debug_exit(subname)
        RETURN
    ENDIF

    DO nc = 1,prism_var(varid)%ncpl

      cplid   = prism_var(varid)%cpl(nc)
      dt      = prism_coupler_put(cplid)%dt
      lag     = prism_coupler_put(cplid)%lag
      getput  = prism_coupler_put(cplid)%getput
      sndrcv  = prism_coupler_put(cplid)%sndrcv
      maxtime = prism_coupler_put(cplid)%maxtime
      output  = prism_coupler_put(cplid)%output
      trans   = prism_coupler_put(cplid)%trans

      !------------------------------------------------
      ! check that lag is reasonable
      !------------------------------------------------

      IF (ABS(lag) > dt) THEN
          WRITE(nulprt,*) subname,' ERROR lag gt dt for cplid',cplid
          WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
      ENDIF

      !------------------------------------------------
      ! check that field is OASIS_PUT
      !------------------------------------------------

      IF (getput == OASIS3_GET) THEN
          WRITE(nulprt,*) subname,' ERROR : routine can only be called for OASIS_PUT variable'
          WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
      ENDIF

      CALL oasis_debug_note(subname//' set mseclag')
      IF (getput == OASIS3_PUT) THEN
          mseclag = msec + lag
      ENDIF

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

      time_now = .FALSE.
      IF (MOD(mseclag,dt) == 0) time_now = .TRUE.

      !-------------------------------------------------------------------
      ! Test what is the current status of the field if time_now = .TRUE.
      !-------------------------------------------------------------------

      IF (time_now .EQV. .TRUE.) THEN
          !
          IF (OASIS_debug >= 2) THEN
              WRITE(nulprt,*) subname,' Coupling time for : ',trim(vname)
              WRITE(nulprt,*) subname,'  Coupling time for var for nc : ',&
                  TRIM(mct_avect_exportRList2c(prism_coupler_put(cplid)%avect1)),nc
              WRITE(nulprt,*) subname,' dt,msec,mseclag = ',dt,msec,mseclag
              CALL oasis_flush(nulprt)
          ENDIF

          IF ( (trans == ip_average) .OR. (trans == ip_accumul) .OR. (trans == ip_max) &
             .OR. (trans == ip_min) ) THEN
              IF (kinfo == OASIS_OK) kinfo = OASIS_LocTrans
              IF (OASIS_debug >= 2) THEN
                  WRITE(nulprt,*) subname,' status at ',msec,mseclag,' WTRN '
                  CALL oasis_flush(nulprt)
              ENDIF
          ENDIF

          !-------------------------------------------------------------------
          ! past namcouple runtime (maxtime) no communication
          ! do restart if time+lag = maxtime, this assumes coupling
          ! period and lag and maxtime are all nicely consistent
          !-------------------------------------------------------------------
          IF (mseclag >= maxtime) THEN
              IF (getput == OASIS3_PUT .AND. lag > 0 .AND. mseclag == maxtime) THEN
                  kinfo = OASIS_ToRest
                  IF (OASIS_debug >= 2) THEN
                      WRITE(nulprt,*) subname,' status at ',msec,mseclag,' WRST '
                      CALL oasis_flush(nulprt)
                  ENDIF
              ENDIF
          ENDIF
          
          !------------------------------------------------
          ! communication
          !------------------------------------------------
          IF (sndrcv) THEN
              IF (getput == OASIS3_PUT) THEN
                  kinfo = OASIS_sent
                  IF (OASIS_debug >= 2) THEN
                      WRITE(nulprt,*) subname,' status at ',msec,mseclag,' will be SENT '
                      CALL oasis_flush(nulprt)
                  ENDIF
                  
              ENDIF
          ENDIF

          !------------------------------------------------
          ! save debug file if EXPOUT or OUTPUT
          !------------------------------------------------
          IF (output) THEN
              IF (kinfo == OASIS_sent) THEN
                  kinfo = OASIS_sentout
              ELSEIF (kinfo == OASIS_torest) THEN
                  kinfo = OASIS_torestout
              ELSE
                  kinfo = OASIS_output
              ENDIF
              IF (OASIS_debug >= 2) THEN
                  WRITE(nulprt,*) subname,' status at ',msec,mseclag,' will be WRIT '
                  CALL oasis_flush(nulprt)
              ENDIF
          ENDIF

          !------------------------------------------------
          ! sav non-instant loctrans operations for future restart
          !   at the end of the run only
          !------------------------------------------------

          IF (mseclag + dt >= maxtime .AND. &
             getput == OASIS3_PUT .and. trans /= ip_instant) then
              IF (OASIS_debug >= 2) THEN
                  WRITE(nulprt,*) subname,' at ',msec,mseclag,' will be WTRN: '
                  CALL oasis_flush(nulprt)
              ENDIF
          ENDIF
      ELSE
          IF (OASIS_Debug >=2) THEN
              WRITE(nulprt,*) 'Nothing to do'
          ENDIF
      ENDIF ! time_now
      IF (OASIS_debug >= 2) THEN
          WRITE(nulprt,*) subname,' kinfo: ',kinfo
          CALL oasis_flush(nulprt)
      ENDIF
    ENDDO

    CALL oasis_debug_exit(subname)

    END SUBROUTINE oasis_put_inquire

!---------------------------------------------------------------------------------
  END MODULE mod_oasis_auxiliary_routines

