MODULE mod_prism_method

   USE mod_prism_kinds
   USE mod_prism_sys
   USE mod_prism_data
   USE mod_prism_parameters
   USE mod_prism_namcouple
   USE mod_prism_coupler
   USE mod_prism_advance
   USE mod_prism_timer
   USE mod_prism_ioshr
   USE mct_mod

   IMPLICIT NONE

   private

   public prism_method_init
   public prism_method_terminate
   public prism_method_getlocalcomm
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
   character(len=*),parameter :: subname = 'prism_method_init'
!  ---------------------------------------------------------

   kinfo = PRISM_OK
   call prism_data_zero()

   !------------------------
   !--- Initialize MPI
   !------------------------

   lg_mpiflag = .FALSE.
   CALL MPI_Initialized ( lg_mpiflag, mpi_err )
   IF ( .NOT. lg_mpiflag ) THEN
      WRITE (0,FMT='(A)') subname//': Calling MPI_Init'
      CALL MPI_INIT ( mpi_err )
   else
      WRITE (0,FMT='(A)') subname//': Not Calling MPI_Init'
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

   call prism_sys_unitsetmin(200)
   call prism_sys_unitget(iu)
   nulprt = iu
   write(filename,'(a,i6.6)') 'nout.',mpi_rank_global
   open(nulprt,file=filename)

   !------------------------
   !--- Initialize namcouple
   !------------------------

   call prism_namcouple_init()

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
      write(nulprt,*) subname,' model not found in namcouple ',trim(cdnam)
      call prism_sys_abort(compid,subname,'model name not in namcouple')
   endif
   if (PRISM_Debug >= 2) then
      write(nulprt,*) subname,' model compid ',trim(cdnam),compid
   endif

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

   !------------------------
   !--- pout file
   !------------------------

   close(nulprt)
   call prism_sys_unitfree(iu)
   call prism_sys_unitget(iu)
   nulprt = iu
   write(filename,'(a,i2.2,a,i6.6)') 'pout.',compid,'.',mpi_rank_local
   open(nulprt,file=filename)

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

   write(nulprt,*) subname,' compid         = ',compid
   write(nulprt,*) subname,' compnm         = ',trim(compnm)
   write(nulprt,*) subname,' mpi_comm_world = ',MPI_COMM_WORLD
   write(nulprt,*) subname,' mpi_comm_global= ',mpi_comm_global
   write(nulprt,*) subname,'     size_global= ',mpi_size_global
   write(nulprt,*) subname,'     rank_global= ',mpi_rank_global
   write(nulprt,*) subname,' mpi_comm_local = ',mpi_comm_local
   write(nulprt,*) subname,'     size_local = ',mpi_size_local
   write(nulprt,*) subname,'     rank_local = ',mpi_rank_local
   write(nulprt,*) subname,'     root_local = ',mpi_root_local
   call prism_sys_flush(nulprt)

   if (PRISM_Debug >= 2) then
      call mpi_barrier(mpi_comm_global,mpi_err)
   endif

   END SUBROUTINE prism_method_init

!----------------------------------------------------------------------
   SUBROUTINE prism_method_terminate(kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: mpi_err
   character(len=*),parameter :: subname = 'prism_method_terminate'
!  ---------------------------------------------------------

   kinfo = PRISM_OK

   call prism_timer_stop('total after init')
   call prism_timer_print()

   CALL MPI_BARRIER (mpi_comm_global, mpi_err)
   IF ( .NOT. lg_mpiflag ) THEN
      WRITE (nulprt,FMT='(A)') subname//': Calling MPI_Finalize'
      CALL MPI_Finalize ( mpi_err )
   else
      WRITE (nulprt,FMT='(A)') subname//': Not Calling MPI_Finalize'
   ENDIF

   write(nulprt,*) subname,' SUCCESSFUL RUN'

   END SUBROUTINE prism_method_terminate

!----------------------------------------------------------------------
   SUBROUTINE prism_method_getlocalcomm(localcomm,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: localcomm
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'prism_method_getlocalcomm'
!  ---------------------------------------------------------

   kinfo = PRISM_OK

   ! from prism_data
   localcomm = mpi_comm_local

   END SUBROUTINE prism_method_getlocalcomm
!----------------------------------------------------------------------
   SUBROUTINE prism_method_enddef(kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'prism_method_enddef'
!  ---------------------------------------------------------

   kinfo = PRISM_OK

   CALL MPI_BARRIER (mpi_comm_global, mpi_err)

   call prism_coupler_setup()
   call prism_advance_init()

   END SUBROUTINE prism_method_enddef
!----------------------------------------------------------------------

END MODULE mod_prism_method
