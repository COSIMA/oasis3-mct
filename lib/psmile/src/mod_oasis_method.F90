
!> High level OASIS user interfaces

MODULE mod_oasis_method

   USE mod_oasis_kinds
   USE mod_oasis_mem
   USE mod_oasis_sys
   USE mod_oasis_data
   USE mod_oasis_parameters
   USE mod_oasis_namcouple
   USE mod_oasis_part
   USE mod_oasis_var
   USE mod_oasis_coupler
   USE mod_oasis_advance
   USE mod_oasis_timer
   USE mod_oasis_ioshr
   USE mod_oasis_grid
   USE mod_oasis_mpi
   USE mod_oasis_string
   USE mct_mod

   IMPLICIT NONE

   private

   public oasis_init_comp
   public oasis_terminate
   public oasis_enddef

#ifdef __VERBOSE
   integer(kind=ip_intwp_p),parameter :: debug=2
#else
   integer(kind=ip_intwp_p),parameter :: debug=1
#endif
   logical,save :: lg_mpiflag

CONTAINS

!----------------------------------------------------------------------

!> OASIS user init method

   SUBROUTINE oasis_init_comp(mynummod,cdnam,kinfo,coupled)

   !> * This is COLLECTIVE, all pes must call

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: mynummod     !< component model ID
   CHARACTER(len=*)         ,intent(in)    :: cdnam        !< model name
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo  !< return code
   logical                  ,intent(in)   ,optional :: coupled  !< flag to specify whether this component is coupled in oasis
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: ierr
   INTEGER(kind=ip_intwp_p) :: n,nns,iu
   integer(kind=ip_intwp_p) :: icolor,ikey
   CHARACTER(len=ic_med)    :: filename,filename2
   character(len=ic_med)    :: pio_type
   integer(kind=ip_intwp_p) :: pio_stride
   integer(kind=ip_intwp_p) :: pio_root
   integer(kind=ip_intwp_p) :: pio_numtasks
   INTEGER(kind=ip_intwp_p),ALLOCATABLE :: tmparr(:)
   INTEGER(kind=ip_intwp_p) :: k,i,m,k1,k2
   INTEGER(kind=ip_intwp_p) :: nt
   INTEGER(kind=ip_intwp_p) :: nvar
   INTEGER(kind=ip_intwp_p) :: mpi_size_world
   INTEGER(kind=ip_intwp_p) :: mpi_rank_world
   INTEGER(kind=ip_intwp_p) :: mall
   logical                  :: found
   character(len=ic_lvar),pointer :: compnmlist(:)
   logical,pointer          :: coupledlist(:)
   character(len=ic_lvar)   :: tmp_modnam
   logical                  :: tmp_modcpl
   character(len=ic_lvar)   :: i_name
   character(len=*),parameter :: subname = '(oasis_init_comp)'
!  ---------------------------------------------------------

   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif
   call oasis_data_zero()

   oasis_coupled = .true.
   if (present(coupled)) then
      oasis_coupled = coupled
   endif

   !------------------------
   !> * Initialize MPI
   !------------------------

   lg_mpiflag = .FALSE.
   CALL MPI_Initialized ( lg_mpiflag, ierr )
   IF ( .NOT. lg_mpiflag ) THEN
      if (OASIS_debug >= 0) WRITE (0,FMT='(A)') subname//': Calling MPI_Init'
      CALL MPI_INIT ( ierr )
   else
      if (OASIS_debug >= 0) WRITE (0,FMT='(A)') subname//': Not Calling MPI_Init'
   ENDIF

! Initial default for early part of init
#ifdef use_comm_MPI1
   mpi_comm_global = MPI_COMM_WORLD
#elif defined use_comm_MPI2
   mpi_comm_global = ??
#endif

   CALL MPI_Comm_Size(MPI_COMM_WORLD,mpi_size_world,ierr)
   CALL MPI_Comm_Rank(MPI_COMM_WORLD,mpi_rank_world,ierr)
   mpi_rank_global = mpi_rank_world

   !------------------------
   !> * Set initial output file, need mpi_rank_world
   !------------------------

   iu=-1

   call oasis_unitsetmin(1024)
   IF (mpi_rank_world == 0) THEN
       CALL oasis_unitget(iu)
       nulprt1 = iu
       WRITE(filename,'(a,i6.6)') 'nout.',mpi_rank_world
       OPEN(nulprt1,file=filename)
   ENDIF

   !------------------------
   !> * Initialize namcouple.
   !>   First on rank 0 to write error messages
   !>   then on all other ranks.  All tasks will
   !>   read the namcouple file independently.
   !------------------------

   IF (mpi_rank_world == 0) THEN
      call oasis_namcouple_init()
   endif
   call oasis_mpi_barrier(MPI_COMM_WORLD)
   IF (mpi_rank_world /= 0) THEN
      call oasis_namcouple_init()
   endif
   OASIS_debug = namlogprt
   TIMER_debug = namtlogprt

   ! If TIMER_debug < 0 activate LUCIA load balancing analysis
   LUCIA_debug = ABS(MIN(namtlogprt,0))

   !------------------------
   !> * Check if NFIELDS=0, there is no coupling.
   ! No information must be written in the debug files as
   ! the different structures are not allocated
   !------------------------

   IF ( nnamcpl == 0 ) THEN
       IF (mpi_rank_world == 0) THEN
           WRITE (UNIT = nulprt1,FMT = *) subname,wstr, &
              'The models are not exchanging any field ($NFIELDS = 0) '
           WRITE (UNIT = nulprt1,FMT = *)  &
              'so we force OASIS_debug = 0 for all processors '
           OASIS_debug = 0
           CALL oasis_flush(nulprt1)
       ENDIF
   ENDIF

   !------------------------
   !> * Determine the total number of coupling fields from namcouple.
   !>   Set maxvar parameter and allocate prism_var.
   ! to avoid a parameter in oasis_def_var and mod_oasis_coupler
   !------------------------

   size_namfld=0
   DO n = 1,nnamcpl
     size_namfld = size_namfld + oasis_string_listGetNum(namsrcfld(n))
   ENDDO
   maxvar = size_namfld * 2    ! multiply by 2 to allow sending to self
   IF (mpi_rank_world == 0) THEN
       WRITE (UNIT = nulprt1,FMT = *) 'Total number of coupling fields :',maxvar
       CALL oasis_flush(nulprt1)
   ENDIF

   ALLOCATE(prism_var(maxvar))

   !------------------------
   !> * Store all the names of the fields exchanged in the namcouple
   ! which can be different of namsrcfld(:) and namdstfld(:) if multiple 
   ! fields are exchanged together
   !------------------------

   ALLOCATE(total_namsrcfld(size_namfld))
   ALLOCATE(total_namdstfld(size_namfld))
   total_namsrcfld = ''
   total_namdstfld = ''

   m=0
   DO nns = 1,nnamcpl
     n = namsort2nn(nns)
     k1=oasis_string_listGetNum(namsrcfld(n))
     k2=oasis_string_listGetNum(namdstfld(n))
     if (k1 /= k2) then
       WRITE(nulprt,*) subname,estr,'namcouple field numbers do not agree '
       WRITE(nulprt,*) subname,estr,'namsrcfld = ',trim(namsrcfld(n))
       WRITE(nulprt,*) subname,estr,'namdstfld = ',trim(namdstfld(n))
       call oasis_abort()
     endif
     DO i=1,k1
       m=m+1
       CALL oasis_string_listGetName(namsrcfld(n),i,i_name)
       total_namsrcfld(m)=trim(i_name)
       CALL oasis_string_listGetName(namdstfld(n),i,i_name)
       total_namdstfld(m)=trim(i_name)
     ENDDO
   ENDDO
   nvar = m

   IF (OASIS_Debug >= 15 .and. mpi_rank_world == 0) THEN
      DO m=1,nvar
         WRITE (UNIT = nulprt1,FMT = *) subname,'Coupling fields  namsrcfld:',&
                                     TRIM(total_namsrcfld(m))
         WRITE (UNIT = nulprt1,FMT = *) subname,'Coupling fields namdstfld:',&
                                     TRIM(total_namdstfld(m))
         CALL oasis_flush(nulprt1)
      ENDDO
   ENDIF

   !------------------------
   ! check (not needed anymore)
   !------------------------

   if (len_trim(cdnam) > ic_lvar) then
      WRITE(nulprt1,*) subname,estr,'model name too long = ',trim(cdnam)
      write(nulprt1,*) subname,estr,'max model name length = ',ic_lvar
      call oasis_abort()
   endif

   !------------------------
   !> * Gather model names from all tasks to generate active model list on all tasks.
   !--- Check that the coupled flag from all tasks is consistent for a given model or abort
   !--- Size of compnm is ic_lvar
   !------------------------

   compnm = trim(cdnam)
   allocate(compnmlist(mpi_size_world))
   allocate(coupledlist(mpi_size_world))
   call MPI_GATHER(compnm, ic_lvar, MPI_CHARACTER, compnmlist, ic_lvar, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
   call MPI_GATHER(oasis_coupled, 1, MPI_LOGICAL, coupledlist, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)

   prism_nmodels = 0
   prism_modnam(:) = ' '
   prism_modcpl(:) = .false.
   if (mpi_rank_world == 0) then
      if (OASIS_Debug >= 15) then
         do n = 1,mpi_size_world
            write(nulprt1,*) subname,' compnm gather ',n,trim(compnmlist(n)),coupledlist(n)
            call oasis_flush(nulprt1)
         enddo
      endif

      !--- generate unique list of models and coupling status
      !--- check for coupled flag consistency
      do n = 1,mpi_size_world
         found = .false.
         m = 0
         do while (.not.found .and. m < prism_nmodels)
            m = m + 1
            if (compnmlist(n) == prism_modnam(m)) then
               found = .true.
               if (coupledlist(n) .neqv. prism_modcpl(m)) then
                  WRITE(nulprt1,*) subname,estr,'inconsistent coupled flag in oasis_init_comp.'
                  WRITE(nulprt1,*) subname,estr,'the optional argument, coupled, in oasis_init_comp '
                  WRITE(nulprt1,*) subname,estr,'must be identical on all tasks of a component.'
                  call oasis_abort()
               endif
            endif
         enddo
         if (.not.found) then
            prism_nmodels = prism_nmodels + 1
            if (prism_nmodels > prism_mmodels) then
               WRITE(nulprt1,*) subname,estr,'prism_nmodels too large, increase prism_mmodels in mod_oasis_data'
               call oasis_abort()
            endif
            prism_modnam(prism_nmodels) = trim(compnmlist(n))
            prism_modcpl(prism_nmodels) = coupledlist(n)
         endif
      enddo

      !--- sort so coupled are first, uncoupled are last
      !--- makes using only active models via "prism_amodels" easier
      prism_amodels = prism_nmodels
      do n = prism_nmodels,1,-1
         if (.not.prism_modcpl(n)) then
            tmp_modnam = prism_modnam(n)
            tmp_modcpl = prism_modcpl(n)
            do m = n,prism_nmodels-1
               prism_modnam(m) = prism_modnam(m+1)
               prism_modcpl(m) = prism_modcpl(m+1)
            enddo
            prism_modnam(prism_nmodels) = tmp_modnam
            prism_modcpl(prism_nmodels) = tmp_modcpl
            prism_amodels = prism_amodels - 1
         endif
      enddo

      !--- document and check list
      do n = 1,prism_amodels
         write(nulprt1,*) subname,'   COUPLED models ',n,trim(prism_modnam(n)),prism_modcpl(n)
         if (.not.prism_modcpl(n)) then
            WRITE(nulprt1,*) subname,estr,'model expected to be coupled but is not = ',trim(prism_modnam(n))
            CALL oasis_abort()
         endif
         call oasis_flush(nulprt1)
      enddo
      do n = prism_amodels+1,prism_nmodels
         write(nulprt1,*) subname,' UNCOUPLED models ',n,trim(prism_modnam(n)),prism_modcpl(n)
         if (prism_modcpl(n)) then
            WRITE(nulprt1,*) subname,estr,'model expected to be uncoupled but is not = ',trim(prism_modnam(n))
            CALL oasis_abort()
         endif
         call oasis_flush(nulprt1)
      enddo
   endif

   deallocate(compnmlist)
   deallocate(coupledlist)

   !------------------------
   !> * Broadcast the model list to all MPI tasks
   !------------------------
   call oasis_mpi_bcast(prism_nmodels,MPI_COMM_WORLD,subname//' prism_nmodels')
   call oasis_mpi_bcast(prism_amodels,MPI_COMM_WORLD,subname//' prism_amodels')
   call oasis_mpi_bcast(prism_modnam ,MPI_COMM_WORLD,subname//' prism_modnam')
   call oasis_mpi_bcast(prism_modcpl ,MPI_COMM_WORLD,subname//' prism_modcpl')

   !------------------------
   !> * Compute compid
   !------------------------

   compid = -1
   do n = 1,prism_nmodels
      if (trim(cdnam) == trim(prism_modnam(n))) compid = n
   enddo
   mynummod = compid
   IF (mpi_rank_world == 0) THEN
      WRITE(nulprt1,*) subname, 'cdnam :',TRIM(cdnam),' mynummod :',mynummod
      CALL oasis_flush(nulprt1)
   ENDIF

! tcraig, this should never happen based on logic above
   if (compid < 0) then
      WRITE(nulprt1,*) subname,estr,'prism_modnam internal inconsistency = ',TRIM(cdnam)
      CALL oasis_abort()
   endif

   !------------------------
   !> * Re-Set MPI info based on active model tasks
   !  (need compid for MPI1 COMM_SPLIT)
   !------------------------

   mpi_rank_global = -1
#ifdef use_comm_MPI1

   !------------------------
   !>   * Set mpi_comm_local based on compid
   !------------------------

   ikey = 0
   icolor = compid
   call MPI_COMM_SPLIT(MPI_COMM_WORLD,icolor,ikey,mpi_comm_local,ierr)

   !------------------------
   !>   * Set mpi_comm_global based on oasis_coupled flag
   !------------------------

   ikey = 0
   icolor = 1
   if (.not.oasis_coupled) icolor = 0
   call MPI_COMM_SPLIT(MPI_COMM_WORLD,icolor,ikey,mpi_comm_global,ierr)
!tcx   if (.not.oasis_coupled) mpi_comm_global = MPI_COMM_NULL

#elif defined use_comm_MPI2

   mpi_comm_global = ??
   mpi_comm_local = MPI_COMM_WORLD

#endif

   !------------------------
   !> * Reset debug levels
   !  verbose level disabled if load balance analysis
   !------------------------

   IF ( LUCIA_debug > 0 .AND. OASIS_debug > 0 ) THEN
      WRITE (UNIT = nulprt1,FMT = *) subname,wstr, &
       ' With LUCIA load balance analysis '
      WRITE (UNIT = nulprt1,FMT = *)  &
       ' we set OASIS_debug = 0 '
      OASIS_debug = 0
      CALL oasis_flush(nulprt1)
   ENDIF

   IF (mpi_rank_world == 0) CLOSE(nulprt1)

   if (.not.oasis_coupled) then
      return
   endif

   CALL MPI_Comm_Size(mpi_comm_global,mpi_size_global,ierr)
   CALL MPI_Comm_Rank(mpi_comm_global,mpi_rank_global,ierr)

   CALL MPI_Comm_Size(mpi_comm_local,mpi_size_local,ierr)
   CALL MPI_Comm_Rank(mpi_comm_local,mpi_rank_local,ierr)
   mpi_root_local = 0

   !------------------------
   !> * Open log files
   !------------------------

   iu=-1
   CALL oasis_unitget(iu)

       IF (OASIS_debug <= 1) THEN
           CALL oasis_mpi_bcast(iu,mpi_comm_local,TRIM(subname)//':unit of master',0)
           IF (mpi_rank_local == 0) THEN
               nulprt=iu
               WRITE(filename,'(a,i2.2)') 'debug.root.',compid
               OPEN(nulprt,file=filename)
               WRITE(nulprt,*) subname,' OPEN debug file for root pe, unit :',nulprt
               call oasis_flush(nulprt)
           ELSE
               nulprt=iu+mpi_size_global
               WRITE(filename2,'(a,i2.2)') 'debug.notroot.',compid
               OPEN(nulprt,file=filename2,position='append')
!               WRITE(nulprt,*) subname,' OPEN debug file for not root pe, unit :',nulprt
!               CALL oasis_flush(nulprt)
           ENDIF
       ELSE
           nulprt=iu
           WRITE(filename,'(a,i2.2,a,i6.6)') 'debug.',compid,'.',mpi_rank_local
           OPEN(nulprt,file=filename)
           WRITE(nulprt,*) subname,' OPEN debug file, unit :',nulprt
           CALL oasis_flush(nulprt)
       ENDIF

       IF ( (OASIS_debug == 1) .AND. (mpi_rank_local == 0)) OASIS_debug=10

       IF (OASIS_debug >= 2) THEN
           WRITE(nulprt,*) subname,' model compid ',TRIM(cdnam),compid
           CALL oasis_flush(nulprt)
       ENDIF

   iu=-1
   CALL oasis_unitget(iu)

   ! If load balance analysis, new log files opened (lucia.*)
   IF ( LUCIA_debug > 0 ) THEN
      IF (mpi_size_local < 20 ) THEN
         nullucia=iu
      ! Open LUCIA log file on a subset of process only
      ELSE IF (mpi_size_local < 100 .AND. MOD(mpi_rank_local,mpi_size_local/5) == 0 ) THEN
         nullucia=iu
      ELSE IF (mpi_size_local >= 100 .AND. MOD(mpi_rank_local,mpi_size_local/20) == 0 ) THEN
         nullucia=iu
      ELSE
         nullucia = 0
      ENDIF
      ! Define log file name and open it
      IF (nullucia /= 0) THEN
         WRITE(filename,'(a,i2.2,a,i6.6)') 'lucia.',compid,'.',mpi_rank_local
         OPEN(nullucia,file=filename)
!         WRITE(nullucia,*) subname,' OPEN LUCIA load balancing analysis file, unit :',nullucia
!         CALL oasis_flush(nullucia)
      ENDIF
   ENDIF

   call oasis_debug_enter(subname)

   !------------------------
   !> * Set mpi_root_global 
   ! (after nulprt set)
   !------------------------

   call mod_oasis_setrootglobal()

   !------------------------
   !--- PIO
   !------------------------
#if (PIO_DEFINED)
! tcraig, not working as of Oct 2011
   pio_type = 'netcdf'
   pio_stride = -99
   pio_root = -99
   pio_numtasks = -99
   call oasis_ioshr_init(mpi_comm_local,pio_type,pio_stride,pio_root,pio_numtasks)
#endif

   !------------------------
   !> * Memory Initialization
   !------------------------

   IF (OASIS_debug >= 2)  THEN
       CALL oasis_mem_init(nulprt)
       CALL oasis_mem_print(nulprt,subname)
   ENDIF

   !------------------------
   !> * Timer Initialization
   !------------------------

   ! Allocate timer memory based on maxvar
   nt = 7*maxvar+30
   call oasis_timer_init (trim(cdnam), trim(cdnam)//'.timers',nt)
   call oasis_timer_start('total')
   call oasis_timer_start('init_thru_enddef')

   !------------------------
   !> * Diagnostics
   !------------------------

   if (OASIS_debug >= 15)  then
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
      write(nulprt,*) subname,' OASIS_debug    = ',OASIS_debug
      do n = 1,prism_amodels
         write(nulprt,*) subname,'   n,prism_model,root = ',&
            n,TRIM(prism_modnam(n)),mpi_root_global(n)
      enddo
      call oasis_flush(nulprt)
   endif

   IF ( LUCIA_debug > 0 ) THEN
      ! We stop all process to read clock time (almost) synchroneously
      call oasis_mpi_barrier(mpi_comm_global)
      IF ( nullucia /= 0 ) THEN
         WRITE(nullucia, FMT='(A,F16.5)') 'Balance: IT                  ', MPI_Wtime()
         WRITE(nullucia, FMT='(A12,A)'  ) 'Balance: MD ', trim(compnm)
         call oasis_flush(nullucia)
      ELSE
         ! Since now, non printing process do not participate to load balance analysis
         LUCIA_debug = 0
      ENDIF
   ENDIF

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_init_comp

!----------------------------------------------------------------------

!> OASIS user finalize method

   SUBROUTINE oasis_terminate(kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo  !< return code
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: ierr
   character(len=*),parameter :: subname = '(oasis_terminate)'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (.not. oasis_coupled) then
      call oasis_debug_exit(subname)
      return
   endif

   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   !------------------------
   !> * Print timer information
   !------------------------

   call oasis_timer_stop('total')
   call oasis_timer_print()

   !------------------------
   !> * Call MPI finalize
   !------------------------

   IF ( .NOT. lg_mpiflag ) THEN
       IF (OASIS_debug >= 2)  THEN
           WRITE (nulprt,FMT='(A)') subname//': Calling MPI_Finalize'
           CALL oasis_flush(nulprt)
       ENDIF
       CALL MPI_Finalize ( ierr )
   else
       IF (OASIS_debug >= 2)  THEN
           WRITE (nulprt,FMT='(A)') subname//': Not Calling MPI_Finalize'
           CALL oasis_flush(nulprt)
       ENDIF
   ENDIF

   !------------------------
   !> * Write SUCCESSFUL RUN
   !------------------------
   IF (OASIS_debug >= 2)  THEN
       CALL oasis_mem_print(nulprt,subname)
   ENDIF

   IF (mpi_rank_local == 0)  THEN
       WRITE(nulprt,*) subname,' SUCCESSFUL RUN'
       CALL oasis_flush(nulprt)
   ENDIF

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_terminate

!----------------------------------------------------------------------

!> OASIS user interface specifying the OASIS definition phase is complete

   SUBROUTINE oasis_enddef(kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo  !< return code
!  ---------------------------------------------------------
   integer (kind=ip_intwp_p) :: n
   integer (kind=ip_intwp_p) :: lkinfo
   integer (kind=ip_intwp_p) :: icpl, ierr
   integer (kind=ip_intwp_p) :: newcomm
   character(len=*),parameter :: subname = '(oasis_enddef)'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   
   !------------------------
   !> * Check enddef called only once per task
   !------------------------

   if (enddef_called) then
       write(nulprt,*) subname,estr,'enddef called already'
       CALL oasis_abort()
   endif
   enddef_called = .true.

   if (.not. oasis_coupled) then
      call oasis_debug_exit(subname)
      return
   endif

   lkinfo = OASIS_OK

   CALL oasis_timer_start ('oasis_enddef')
   IF (OASIS_debug >= 2)  THEN
       CALL oasis_mem_print(nulprt,subname//':start')
   ENDIF

   !------------------------
   !> * Reset mpi_comm_global because active tasks might have been excluded
   !--- for changes to mpi_comm_local since init
   !------------------------

   icpl = MPI_UNDEFINED
   if (mpi_comm_local /= MPI_COMM_NULL) icpl = 1
   CALL MPI_COMM_Split(mpi_comm_global,icpl,1,newcomm,ierr)
   mpi_comm_global = newcomm

   !------------------------
   !> * For active tasks only
   !------------------------

   if (mpi_comm_global /= MPI_COMM_NULL) then

      !------------------------
      !>   * Update mpi_comm_global
      !------------------------

      CALL MPI_Comm_Size(mpi_comm_global,mpi_size_global,ierr)
      CALL MPI_Comm_Rank(mpi_comm_global,mpi_rank_global,ierr)

      !------------------------
      !>   * Update mpi_root_global
      !------------------------

      call mod_oasis_setrootglobal()

      !------------------------
      !>   * Document
      !------------------------

      if (OASIS_debug >= 2)  then
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
         write(nulprt,*) subname,' OASIS_debug    = ',OASIS_debug
         do n = 1,prism_amodels
            write(nulprt,*) subname,'   n,prism_model,root = ',&
               n,TRIM(prism_modnam(n)),mpi_root_global(n)
         enddo
         CALL oasis_flush(nulprt)
      endif

      !------------------------
      !>   * Reconcile partitions, call part_setup
      !--- generate gsmaps from partitions
      !------------------------

      call oasis_part_setup()
      IF (OASIS_debug >= 2)  THEN
          CALL oasis_mem_print(nulprt,subname//':part_setup')
      ENDIF

      !------------------------
      !>   * Reconcile variables, call var_setup
      !------------------------

      call oasis_var_setup()
      IF (OASIS_debug >= 2)  THEN
          CALL oasis_mem_print(nulprt,subname//':var_setup')
      ENDIF

      !------------------------
      !>   * Write grid info to files one model at a time
      !------------------------

      call oasis_mpi_barrier(mpi_comm_global)
      do n = 1,prism_amodels
         if (compid == n) then
            call oasis_write2files()
         endif
         call oasis_mpi_barrier(mpi_comm_global)
      enddo
      IF (OASIS_debug >= 2)  THEN
          CALL oasis_mem_print(nulprt,subname//':write2files')
      ENDIF

      !------------------------
      !>   * MCT Initialization
      !------------------------

      call mct_world_init(prism_amodels,mpi_comm_global,mpi_comm_local,compid)
      IF (OASIS_debug >= 2)  THEN
         WRITE(nulprt,*) subname, ' done mct_world_init '
         CALL oasis_flush(nulprt)
      ENDIF

      !------------------------
      !>   * Initialize coupling via call to coupler_setup
      !------------------------

      call oasis_coupler_setup()
      IF (OASIS_debug >= 2)  THEN
         WRITE(nulprt,*) subname, ' done prism_coupler_setup '
         CALL oasis_flush(nulprt)
      ENDIF
      IF (OASIS_debug >= 2)  THEN
          CALL oasis_mem_print(nulprt,subname//':coupler_setup')
      ENDIF
      !------------------------
      !>   * Call advance_init to initialize coupling fields from restarts
      !------------------------

      call oasis_advance_init(lkinfo)
      IF (OASIS_debug >= 2)  THEN
         WRITE(nulprt,*) subname, ' done prism_advance_init '
         CALL oasis_flush(nulprt)
      ENDIF
      IF (OASIS_debug >= 2)  THEN
          CALL oasis_mem_print(nulprt,subname//':advance_init')
      ENDIF

   endif   !  (mpi_comm_local /= MPI_COMM_NULL)

   !--- Force OASIS_OK here rather than anything else ---

   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif
   CALL oasis_timer_stop ('oasis_enddef')
   call oasis_timer_stop('init_thru_enddef')

   IF (OASIS_debug >= 2)  THEN
       CALL oasis_mem_print(nulprt,subname//':end')
   ENDIF

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_enddef
!----------------------------------------------------------------------

!> Local method to compute each models' global task ids, exists for reuse in enddef

 SUBROUTINE mod_oasis_setrootglobal()

   INTEGER(kind=ip_intwp_p) :: n, ierr
   INTEGER(kind=ip_intwp_p),ALLOCATABLE :: tmparr(:)
   character(len=*),parameter :: subname = '(oasis_setrootglobal)'

   !------------------------
   !--- set mpi_root_global
   !------------------------

   if (allocated(mpi_root_global)) then
      deallocate(mpi_root_global)
   endif
   allocate(mpi_root_global(prism_amodels))
   allocate(tmparr(prism_amodels))
   tmparr = -1
   do n = 1,prism_amodels
      if (compid == n .and. mpi_rank_local == mpi_root_local) then
         tmparr(n) = mpi_rank_global
      endif
   enddo
   call oasis_mpi_max(tmparr,mpi_root_global,mpi_comm_global, &
      string=subname//':mpi_root_global',all=.true.)
   deallocate(tmparr)

   do n = 1,prism_amodels
      IF (mpi_root_global(n) < 0) THEN
         WRITE(nulprt,*) subname,estr,'global root invalid, check couplcomm for active tasks'
         CALL oasis_abort()
      ENDIF
   enddo

END SUBROUTINE mod_oasis_setrootglobal
!----------------------------------------------------------------------

END MODULE mod_oasis_method
