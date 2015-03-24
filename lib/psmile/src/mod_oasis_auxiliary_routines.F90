
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

    public oasis_get_localcomm
    public oasis_set_couplcomm
    public oasis_create_couplcomm
    public oasis_get_debug
    public oasis_set_debug
    public oasis_get_intercomm
    public oasis_get_intracomm
    public oasis_get_ncpl
    public oasis_put_inquire
    public oasis_get_freqs

#include "oasis_os.h"

    integer(kind=ip_i4_p)     istatus(MPI_STATUS_SIZE)

!---------------------------------------------------------------------
  CONTAINS
!---------------------------------------------------------------------

!> OASIS user query for the local MPI communicator

  SUBROUTINE oasis_get_localcomm(localcomm,kinfo)

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p),intent(out)   :: localcomm  !< MPI communicator
    INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo  !< return code
!   ---------------------------------------------------------
    character(len=*),parameter :: subname = '(oasis_get_localcomm)'
!   ---------------------------------------------------------

    call oasis_debug_enter(subname)
    if (present(kinfo)) then
       kinfo = OASIS_OK
    endif

    ! from prism_data
    localcomm = mpi_comm_local
    IF (OASIS_debug >= 2) THEN
        WRITE(nulprt,*) 'localcomm :',localcomm
        CALL oasis_FLUSH(nulprt)
    ENDIF

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_localcomm
!----------------------------------------------------------------------

!> OASIS user call to specify a local communicator

  SUBROUTINE oasis_set_couplcomm(localcomm,kinfo)

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p),intent(in)   :: localcomm  !< MPI communicator
    INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo  !< return code
!   ---------------------------------------------------------
    integer(kind=ip_intwp_p) :: ierr
    character(len=*),parameter :: subname = '(oasis_set_couplcomm)'
!   ---------------------------------------------------------

    call oasis_debug_enter(subname)
    if (present(kinfo)) then
       kinfo = OASIS_OK
    endif

    !------------------------
    !--- update mpi_comm_local from component
    !------------------------

    mpi_comm_local = localcomm

    !------------------------
    !--- and now update necessary info
    !------------------------

    mpi_size_local = -1
    mpi_rank_local = -1
    if (mpi_comm_local /= MPI_COMM_NULL) then
       CALL MPI_Comm_Size(mpi_comm_local,mpi_size_local,ierr)
       CALL MPI_Comm_Rank(mpi_comm_local,mpi_rank_local,ierr)
       mpi_root_local = 0
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_set_couplcomm
!----------------------------------------------------------------------

!> OASIS user call to create a new communicator

  SUBROUTINE oasis_create_couplcomm(icpl,allcomm,cplcomm,kinfo)

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p),intent(in)   :: icpl  !< coupling process flag 
    INTEGER (kind=ip_intwp_p),intent(in)   :: allcomm  !< input MPI communicator 
    INTEGER (kind=ip_intwp_p),intent(out)  :: cplcomm  !< reduced MPI communicator
    INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo  !< return code
!   ---------------------------------------------------------
    integer(kind=ip_intwp_p) :: ierr
    character(len=*),parameter :: subname = '(oasis_create_couplcomm)'
!   ---------------------------------------------------------

    call oasis_debug_enter(subname)
    if (present(kinfo)) then
       kinfo = OASIS_OK
    endif

    !------------------------
    !--- generate cplcomm from allcomm and icpl
    !------------------------

    CALL MPI_COMM_Split(allcomm,icpl,1,cplcomm,ierr)
    IF (ierr /= 0) THEN
       WRITE (nulprt,*) subname,estr,'MPI_Comm_Split ierr = ',ierr
       call oasis_abort()
    ENDIF

    !------------------------
    !--- update mpi_comm_local from component
    !------------------------

    call oasis_set_couplcomm(cplcomm)

    IF (OASIS_debug >= 2)  THEN
       WRITE (nulprt,*) 'New local coupling comm =',cplcomm
       CALL oasis_flush(nulprt)
    ENDIF

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_create_couplcomm
!----------------------------------------------------------------------

!> OASIS user interface to query debug level

  SUBROUTINE oasis_get_debug(debug,kinfo)

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p),intent(out)   :: debug  !< debug level
    INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo  !< return code
!   ---------------------------------------------------------
    character(len=*),parameter :: subname = '(oasis_get_debug)'
!   ---------------------------------------------------------

    call oasis_debug_enter(subname)
    if (present(kinfo)) then
       kinfo = OASIS_OK
    endif

    debug = OASIS_debug

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_debug
!----------------------------------------------------------------------

!> OASIS user interface to set debug level

  SUBROUTINE oasis_set_debug(debug,kinfo)

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p),intent(in)   :: debug  !< debug level
    INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo  !< return code
!   ---------------------------------------------------------
    character(len=*),parameter :: subname = '(oasis_set_debug)'
!   ---------------------------------------------------------

    call oasis_debug_enter(subname)
    if (present(kinfo)) then
       kinfo = OASIS_OK
    endif

    OASIS_debug = debug
    if (OASIS_debug >= 2) then
       write(nulprt,*) subname,' set OASIS_debug to ',OASIS_debug
       CALL oasis_flush(nulprt)
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_set_debug
!----------------------------------------------------------------------

!> OASIS user interface to establish an intercomm communicator between the root of two models

  SUBROUTINE oasis_get_intercomm(new_comm, cdnam, kinfo)

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p),intent(out) :: new_comm  !< out MPI communicator
    CHARACTER(len=*),intent(in) :: cdnam               !< other model name to link with
    INTEGER (kind=ip_intwp_p),intent(out),optional :: kinfo  !< return code

    INTEGER (kind=ip_intwp_p)			  :: n, il, ierr, tag
    LOGICAL :: found
!   ---------------------------------------------------------
    character(len=*),parameter :: subname = '(oasis_get_intercomm)'
!   ---------------------------------------------------------

    call oasis_debug_enter(subname)
    if (present(kinfo)) then
       kinfo = OASIS_OK
    endif

    found = .false.
    do n = 1,prism_amodels
       if (trim(cdnam) == trim(prism_modnam(n))) then
          if (found) then
             write(nulprt,*) subname,estr,'found same model name twice'
             call oasis_abort()
          endif
          il = n
          found = .true.
       endif
    enddo

    if (.not. found) then
       write(nulprt,*) subname,estr,'input model name not found'
       call oasis_abort()
    endif

    IF (OASIS_debug >= 2) THEN
       WRITE(nulprt,*) subname, 'cdnam :',trim(cdnam),' il :',il, &
                       'mpi_root_global(il) :',mpi_root_global(il),&
                       'mpi_comm_local :',mpi_comm_local
       CALL oasis_flush(nulprt)
    ENDIF

    tag=ICHAR(TRIM(compnm))+ICHAR(TRIM(cdnam))
    CALL mpi_intercomm_create(mpi_comm_local, 0, mpi_comm_global, &
                              mpi_root_global(il), tag, new_comm, ierr)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_intercomm
!----------------------------------------------------------------------

!> OASIS user interface to establish an intracomm communicator between the root of two models

  SUBROUTINE oasis_get_intracomm(new_comm, cdnam, kinfo)

    IMPLICIT NONE

    INTEGER (kind=ip_intwp_p),intent(out) :: new_comm  !< output MPI communicator
    CHARACTER(len=*),intent(in) :: cdnam               !< other model name
    INTEGER (kind=ip_intwp_p),intent(out),optional :: kinfo  !< return code

    INTEGER (kind=ip_intwp_p)			  :: tmp_intercomm
    INTEGER (kind=ip_intwp_p)			  :: ierr
!   ---------------------------------------------------------
    character(len=*),parameter :: subname = '(oasis_get_intracomm)'
!   ---------------------------------------------------------

    call oasis_debug_enter(subname)
    if (present(kinfo)) then
       kinfo = OASIS_OK
    endif

    call oasis_get_intercomm(tmp_intercomm, cdnam, kinfo)

    CALL mpi_intercomm_merge(tmp_intercomm,.FALSE., new_comm, ierr)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_intracomm
!----------------------------------------------------------------------

!> OASIS user query for the number of unique couplings associated with a variable

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
       WRITE(nulprt,*) subname,estr,'called on non coupling task'
       CALL oasis_abort()
    ENDIF

    kinfo = OASIS_OK
    vname = prism_var(varid)%name
    
    IF (varid == OASIS_Var_Uncpl) THEN
       WRITE(nulprt,*) subname,estr, &
          'Routine is called for a variable not in namcouple: it will not be sent'
       CALL oasis_abort()
    ENDIF
    
    ncpl  = prism_var(varid)%ncpl
    
    IF (ncpl <= 0) THEN
       IF (OASIS_debug >= 2) WRITE(nulprt,*) subname,' Variable not coupled ',&
                              TRIM(vname)
    ELSE 
       IF (OASIS_debug >= 2)  WRITE(nulprt,*) subname,' Variable: ',TRIM(vname),&
                              ' used in ',ncpl,' couplings' 
    ENDIF

    CALL oasis_debug_exit(subname)
    
  END SUBROUTINE oasis_get_ncpl
!---------------------------------------------------------------------

!> OASIS user query for the coupling periods for a given variable

  SUBROUTINE oasis_get_freqs(varid, mop, ncpl, cpl_freqs, kinfo)

    IMPLICIT none
    !-------------------------------------
    INTEGER(kind=ip_i4_p) , INTENT(in)  :: varid          !< variable id
    INTEGER(kind=ip_i4_p) , INTENT(in)  :: mop            !< OASIS_Out or OASIS_In type
    INTEGER(kind=ip_i4_p) , INTENT(in)  :: ncpl           !< number of namcouple couplings
    INTEGER(kind=ip_i4_p) , INTENT(out) :: cpl_freqs(ncpl)!< coupling period (sec)
    INTEGER(kind=ip_i4_p) , INTENT(out) :: kinfo          !< return code
    !-------------------------------------
    CHARACTER(len=ic_lvar)  :: vname
    INTEGER(kind=ip_i4_p)   :: ncpl_calc, cplid, nc
    CHARACTER(len=*),PARAMETER :: subname = 'oasis_get_freqs'
    !-------------------------------------

    CALL oasis_debug_enter(subname)

    IF (mpi_comm_local == MPI_COMM_NULL) THEN
       WRITE(nulprt,*) subname,estr,'called on non coupling task'
       CALL oasis_abort()
    ENDIF

    kinfo = OASIS_OK
    vname = prism_var(varid)%name
    
    IF (varid == OASIS_Var_Uncpl) THEN
       WRITE(nulprt,*) subname,estr, &
          'Routine is called for a variable not in namcouple: it will not be sent'
       CALL oasis_abort()
    ENDIF
    
    ncpl_calc  = prism_var(varid)%ncpl

    IF (ncpl_calc /= ncpl) THEN
       WRITE(nulprt,*) subname,estr,' Wrong number of couplings for variable: ',TRIM(vname), &
                       ncpl_calc, ncpl
       CALL oasis_abort()
    ENDIF
    
    IF (ncpl <= 0) THEN
       IF (OASIS_debug >= 2) WRITE(nulprt,*) subname,' variable not coupled ',&
                              TRIM(vname)
    ENDIF

    DO nc = 1,ncpl
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
          WRITE(nulprt,*) subname,estr,' The coupling frequency is < or equal to 0'
          CALL oasis_abort()
       ENDIF
    ENDDO

    CALL oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_freqs
!---------------------------------------------------------------------

!> OASIS user query to indicate put return code expected at a specified time for a given variable

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
       WRITE(nulprt,*) subname,estr,'called on non coupling task'
       CALL oasis_abort()
    ENDIF

    kinfo = OASIS_OK
    vname = prism_var(varid)%name
    
    IF (varid == OASIS_Var_Uncpl) THEN
       WRITE(nulprt,*) subname,estr, &
          'Routine oasis_put is called for a variable not in namcouple: it will not be sent'
       CALL oasis_abort()
    ENDIF
    
    ncpl  = prism_var(varid)%ncpl
    
    IF (ncpl <= 0) THEN
       IF (OASIS_debug >= 2) WRITE(nulprt,*) subname,' variable not coupled ',&
                              TRIM(vname)
    ENDIF

    DO nc = 1,ncpl

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
          WRITE(nulprt,*) subname,estr,' ERROR lag gt dt for cplid',cplid
          CALL oasis_abort()
       ENDIF

       !------------------------------------------------
       ! check that field is OASIS_PUT
       !------------------------------------------------

       IF (getput == OASIS3_GET) THEN
          WRITE(nulprt,*) subname,estr,'routine can only be called for OASIS_PUT variable'
          CALL oasis_abort()
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
          write(nulprt,*) subname,estr,'model time beyond namcouple maxtime',&
                          msec,maxtime
          call oasis_abort()
       endif

       time_now = .FALSE.
       IF (MOD(mseclag,dt) == 0) time_now = .TRUE.

       !-------------------------------------------------------------------
       ! Test what is the current status of the field if time_now = .TRUE.
       !-------------------------------------------------------------------

       IF (time_now .EQV. .TRUE.) THEN

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
    ENDDO  ! nc

    CALL oasis_debug_exit(subname)

    END SUBROUTINE oasis_put_inquire

!---------------------------------------------------------------------------------
  END MODULE mod_oasis_auxiliary_routines

