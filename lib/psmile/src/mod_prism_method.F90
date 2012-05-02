MODULE mod_prism_method

   USE mod_prism_kinds
   USE mod_prism_sys
   USE mod_prism_data
   USE mod_oasis_print
   USE mod_prism_parameters
   USE mod_prism_namcouple
   USE mod_prism_coupler
   USE mod_prism_advance
   USE mod_prism_timer
   USE mod_prism_ioshr
   USE mod_prism_grid
   USE mod_prism_mpi
   USE mct_mod

   IMPLICIT NONE

   private

   public prism_method_init
   public prism_method_terminate
   public prism_method_getlocalcomm
   public prism_method_getdebug
   public prism_method_setdebug
   public prism_method_get_intercomm
   public prism_method_get_intracomm
   public prism_method_enddef

#ifdef __VERBOSE
   integer(kind=ip_intwp_p),parameter :: debug=2
#else
   integer(kind=ip_intwp_p),parameter :: debug=1
#endif
   logical,save :: lg_mpiflag

CONTAINS

!----------------------------------------------------------------------
   SUBROUTINE prism_method_init(mynummod,cdnam,kinfo)

   ! This is COLLECTIVE, all pes must call

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: mynummod     
   CHARACTER(len=*)         ,intent(in)    :: cdnam
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: mpi_err
   integer(kind=ip_intwp_p) :: n,iu
   integer(kind=ip_intwp_p) :: icolor,ikey
   character(len=ic_med)    :: filename
   character(len=ic_med)    :: pio_type
   integer(kind=ip_intwp_p) :: pio_stride
   integer(kind=ip_intwp_p) :: pio_root
   integer(kind=ip_intwp_p) :: pio_numtasks
   integer(kind=ip_intwp_p),allocatable :: tmparr(:)
   character(len=*),parameter :: subname = 'prism_method_init'
!  ---------------------------------------------------------

   if (present(kinfo)) then
      kinfo = PRISM_OK
   endif
   call prism_data_zero()

   !------------------------
   !--- Initialize MPI
   !------------------------

   lg_mpiflag = .FALSE.
   CALL MPI_Initialized ( lg_mpiflag, mpi_err )
   IF ( .NOT. lg_mpiflag ) THEN
      if (PRISM_DEBUG >= 0) WRITE (0,FMT='(A)') subname//': Calling MPI_Init'
      CALL MPI_INIT ( mpi_err )
   else
      if (PRISM_DEBUG >= 0) WRITE (0,FMT='(A)') subname//': Not Calling MPI_Init'
   ENDIF

#ifdef use_comm_MPI1
   mpi_comm_global = MPI_COMM_WORLD
#elif define use_comm_MPI2
   mpi_comm_global = ??
#endif

   CALL MPI_Comm_Size(mpi_comm_global,mpi_size_global,mpi_err)
   CALL MPI_Comm_Rank(mpi_comm_global,mpi_rank_global,mpi_err)

   !------------------------
   !--- nout file, need mpi_rank_global
   !------------------------
   iu=-1

   call prism_sys_unitsetmin(200)
   IF (mpi_rank_global == 0) THEN
       CALL prism_sys_unitget(iu)
       nulprt1 = iu
       WRITE(filename,'(a,i6.6)') 'nout.',mpi_rank_global
       OPEN(nulprt1,file=filename)
   ENDIF

   !------------------------
   !--- Initialize namcouple
   !------------------------

   call prism_namcouple_init()
   prism_debug = namlogprt

   !------------------------
   !--- Set compid (need namcouple model names)
   !------------------------

   compid = -1
   compnm = trim(cdnam)
   do n = 1,prism_nmodels
      if (trim(cdnam) == trim(prism_modnam(n))) compid = n
   enddo
   mynummod = compid
   if (compid < 0) then
       IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,' model not found in namcouple ',TRIM(cdnam)
       ENDIF
       CALL prism_sys_abort()
   endif

   IF (mpi_rank_global == 0) THEN
       CLOSE(nulprt1)
   ENDIF

   !------------------------
   !--- Re-Set MPI info (need compid for MPI1 COMM_SPLIT)
   !------------------------

#ifdef use_comm_MPI1

   mpi_comm_global = MPI_COMM_WORLD
   ikey = compid
   icolor = compid
   call MPI_COMM_SPLIT(MPI_COMM_WORLD,icolor,ikey,mpi_comm_local,mpi_err)

#elif define use_comm_MPI2

   mpi_comm_global = ??
   mpi_comm_local = MPI_COMM_WORLD

#endif

   CALL MPI_Comm_Size(mpi_comm_global,mpi_size_global,mpi_err)
   CALL MPI_Comm_Rank(mpi_comm_global,mpi_rank_global,mpi_err)

   CALL MPI_Comm_Size(mpi_comm_local,mpi_size_local,mpi_err)
   CALL MPI_Comm_Rank(mpi_comm_local,mpi_rank_local,mpi_err)
   mpi_root_local = 0

   allocate(mpi_root_global(prism_nmodels))
   allocate(tmparr(prism_nmodels))
   tmparr = -1
   do n = 1,prism_nmodels
      if (trim(cdnam) == trim(prism_modnam(n)) .and. &
          mpi_rank_local == mpi_root_local) then
         tmparr(n) = mpi_rank_global
      endif
   enddo

   !------------------------
   !--- pout file
   !------------------------

   iu=-1
   IF (PRISM_Debug == 0) THEN
       IF (mpi_rank_local == 0) THEN
!   call prism_sys_unitfree(iu)
           CALL prism_sys_unitget(iu)
           nulprt = iu
           WRITE(filename,'(a,i2.2,a,i6.6)') 'pout.',compid,'.',mpi_rank_local
           OPEN(nulprt,file=filename)
       ENDIF
   ELSE
       CALL prism_sys_unitget(iu)
       nulprt = iu
       WRITE(filename,'(a,i2.2,a,i6.6)') 'pout.',compid,'.',mpi_rank_local
       OPEN(nulprt,file=filename)
   ENDIF

   CALL oasis_pprintc(subname,2,':',char1=' OPEN pout file ')
   CALL oasis_pprinti(subname,2,' compid : ',int1=compid)
   CALL oasis_pprintc(subname,2,' model : ',char1=TRIM(cdnam))

   call prism_mpi_max(tmparr,mpi_root_global,MPI_COMM_WORLD, &
      string=subname//':mpi_root_global',all=.true.)

   deallocate(tmparr)

   call prism_sys_debug_enter(subname)

   !------------------------
   !--- MCT
   !------------------------

   call mct_world_init(prism_nmodels,mpi_comm_global,mpi_comm_local,compid)

   !------------------------
   !--- PIO
   !------------------------
#if (PIO_DEFINED)
! tcraig, not working as of Oct 2011
   pio_type = 'netcdf'
   pio_stride = -99
   pio_root = -99
   pio_numtasks = -99
   call prism_ioshr_init(mpi_comm_local,pio_type,pio_stride,pio_root,pio_numtasks)
#endif

   !------------------------
   !--- Timer Initialization
   !------------------------

   call prism_timer_init (trim(cdnam), trim(cdnam)//'.timers', mpi_comm_local)
   call prism_timer_start('total after init')

   !------------------------
   !--- Diagnostics
   !------------------------

   CALL oasis_pprinti(subname,2,' compid         = ',int1=compid)
   CALL oasis_pprintc(subname,2,' compnm         = ',char1=trim(compnm))
   CALL oasis_pprinti(subname,2,' mpi_comm_world = ',int1=MPI_COMM_WORLD)
   CALL oasis_pprinti(subname,2,' mpi_comm_global= ',int1=mpi_comm_global)
   CALL oasis_pprinti(subname,2,'     size_global= ',int1=mpi_size_global)
   CALL oasis_pprinti(subname,2,'     rank_global= ',int1=mpi_rank_global)
   CALL oasis_pprinti(subname,2,' mpi_comm_local = ',int1=mpi_comm_local)
   CALL oasis_pprinti(subname,2,'     size_local = ',int1=mpi_size_local)
   CALL oasis_pprinti(subname,2,'     rank_local = ',int1=mpi_rank_local)
   CALL oasis_pprinti(subname,2,'     root_local = ',int1=mpi_root_local)
   CALL oasis_pprinti(subname,2,' prism_debug    = ',int1=prism_debug)
   CALL oasis_pprintc(subname,2,':',char1='prism models ')
   DO n = 1,prism_nmodels
     CALL oasis_pprintc(subname,2,' prism_model = ',char1=TRIM(prism_modnam(n)))
     CALL oasis_pprinti(subname,2,' n, root,    = ',int1=n,int2=mpi_root_global(n))
   ENDDO

   if (PRISM_Debug >= 4) then
      call mpi_barrier(mpi_comm_global,mpi_err)
   endif

   call prism_sys_debug_exit(subname)

   END SUBROUTINE prism_method_init

!----------------------------------------------------------------------
   SUBROUTINE prism_method_terminate(kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: mpi_err
   character(len=*),parameter :: subname = 'prism_method_terminate'
!  ---------------------------------------------------------

   call prism_sys_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = PRISM_OK
   endif

   call prism_timer_stop('total after init')
   call prism_timer_print()

   CALL MPI_BARRIER (mpi_comm_global, mpi_err)
   IF ( .NOT. lg_mpiflag ) THEN
       CALL oasis_pprintc(subname,2,':',char1=' Calling MPI_Finalize')
       CALL MPI_Finalize ( mpi_err )
   else
       CALL oasis_pprintc(subname,2,':',char1=' Not Calling MPI_Finalize')
   ENDIF

   CALL oasis_pprintc(subname,2,':',char1=' SUCCESSFUL RUN')

   call prism_sys_debug_exit(subname)

   END SUBROUTINE prism_method_terminate

!----------------------------------------------------------------------
   SUBROUTINE prism_method_getlocalcomm(localcomm,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: localcomm
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'prism_method_getlocalcomm'
!  ---------------------------------------------------------

   call prism_sys_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = PRISM_OK
   endif

   ! from prism_data
   localcomm = mpi_comm_local

   call prism_sys_debug_exit(subname)

   END SUBROUTINE prism_method_getlocalcomm
!----------------------------------------------------------------------
   SUBROUTINE prism_method_getdebug(debug,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: debug
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'prism_method_getdebug'
!  ---------------------------------------------------------

   call prism_sys_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = PRISM_OK
   endif

   debug = prism_debug

   call prism_sys_debug_exit(subname)

   END SUBROUTINE prism_method_getdebug
!----------------------------------------------------------------------
   SUBROUTINE prism_method_setdebug(debug,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(in)   :: debug
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'prism_method_setdebug'
!  ---------------------------------------------------------

   call prism_sys_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = PRISM_OK
   endif

   prism_debug = debug
   CALL oasis_pprinti(subname,2,' set prism_debug to ',int1=prism_debug)

   call prism_sys_debug_exit(subname)

   END SUBROUTINE prism_method_setdebug
!----------------------------------------------------------------------
   SUBROUTINE prism_method_get_intercomm(new_comm, cdnam, kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out) :: new_comm
   CHARACTER(len=*),intent(in) :: cdnam
   INTEGER (kind=ip_intwp_p),intent(out),optional :: kinfo

   INTEGER (kind=ip_intwp_p)	:: n, il, ierr, tag
   LOGICAL :: found
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'prism_method_get_intercomm'
!  ---------------------------------------------------------

   call prism_sys_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = PRISM_OK
   endif

   found = .false.
   do n = 1,prism_nmodels
      if (trim(cdnam) == trim(prism_modnam(n))) then
         if (found) then
             CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
             CALL oasis_pprintc(subname,2,' error :',char1='found same model name twice')
             CALL prism_sys_abort()
         endif
         il = n
         found = .true.
      endif
   enddo

   if (.not. found) then
       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
       CALL oasis_pprintc(subname,2,' error :',char1='input model name not found')
       CALL prism_sys_abort()
   endif

   tag=ICHAR(TRIM(compnm))+ICHAR(TRIM(cdnam))
   CALL mpi_intercomm_create(mpi_comm_local, 0, MPI_COMM_WORLD, mpi_root_global(il), tag, new_comm, ierr)

   call prism_sys_debug_exit(subname)

   END SUBROUTINE prism_method_get_intercomm
!----------------------------------------------------------------------
   SUBROUTINE prism_method_get_intracomm(new_comm, cdnam, kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out) :: new_comm
   CHARACTER(len=*),intent(in) :: cdnam
   INTEGER (kind=ip_intwp_p),intent(out),optional :: kinfo

   INTEGER (kind=ip_intwp_p)	:: tmp_intercomm
   INTEGER (kind=ip_intwp_p)	:: ierr
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'prism_method_get_intracomm'
!  ---------------------------------------------------------

   call prism_sys_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = PRISM_OK
   endif

   call prism_method_get_intercomm(tmp_intercomm, cdnam, kinfo)

   CALL mpi_intercomm_merge(tmp_intercomm,.FALSE., new_comm, ierr)

   call prism_sys_debug_exit(subname)

   END SUBROUTINE prism_method_get_intracomm
!----------------------------------------------------------------------
   SUBROUTINE prism_method_enddef(kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer (kind=ip_intwp_p) :: n
   character(len=*),parameter :: subname = 'prism_method_enddef'
!  ---------------------------------------------------------

   call prism_sys_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = PRISM_OK
   endif

   CALL MPI_BARRIER (mpi_comm_global, mpi_err)

   ! write grid info to files one model at a time
   do n = 1,prism_nmodels
      if (compid == n .and. mpi_rank_local == mpi_root_local) then
         call prism_grid_write2files()
      endif
      CALL MPI_BARRIER (mpi_comm_global, mpi_err)
   enddo

   call prism_coupler_setup()
   call prism_advance_init()

   call prism_sys_debug_exit(subname)

   END SUBROUTINE prism_method_enddef
!----------------------------------------------------------------------

END MODULE mod_prism_method
