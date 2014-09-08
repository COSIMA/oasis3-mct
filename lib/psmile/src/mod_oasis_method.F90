MODULE mod_oasis_method

   USE mod_oasis_kinds
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
   public oasis_get_localcomm
   public oasis_set_couplcomm
   public oasis_create_couplcomm
   public oasis_get_debug
   public oasis_set_debug
   public oasis_get_intercomm
   public oasis_get_intracomm
   public oasis_enddef

#ifdef __VERBOSE
   integer(kind=ip_intwp_p),parameter :: debug=2
#else
   integer(kind=ip_intwp_p),parameter :: debug=1
#endif
   logical,save :: lg_mpiflag

CONTAINS

!----------------------------------------------------------------------
   SUBROUTINE oasis_init_comp(mynummod,cdnam,kinfo,coupled)

   ! This is COLLECTIVE, all pes must call

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: mynummod     
   CHARACTER(len=*)         ,intent(in)    :: cdnam   
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
   logical                  ,intent(in)   ,optional :: coupled  ! is this component coupled in oasis
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
   !--- Initialize MPI
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
   !--- nout file, need mpi_rank_world
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
   !--- Initialize namcouple
   !--- first on rank 0 to write error messages
   !--- then on all other ranks
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

   ! If NFIELDS=0 there is no coupling
   ! No information must be written in the debug files as
   ! the different structures are not allocated

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

   ! Determines the total number of fields to avoid a parameter in oasis_def_var
   ! and mod_oasis_coupler
   maxvar=0
   DO n = 1,nnamcpl
     maxvar = maxvar + oasis_string_listGetNum(namsrcfld(n))
   ENDDO
   maxvar = maxvar * 2    ! multiply by 2 to allow sending to self
   IF (mpi_rank_world == 0) THEN
       WRITE (UNIT = nulprt1,FMT = *) 'Total number of coupling fields :',maxvar
       CALL oasis_flush(nulprt1)
   ENDIF

   ALLOCATE(prism_var(maxvar))

   ! Store all the names of the fields exchanged in the namcouple
   ! which can be different of namsrcfld(:) and namdstfld(:) if multiple 
   ! fields are exchanged together
   ALLOCATE(total_namsrcfld(maxvar))
   ALLOCATE(total_namdstfld(maxvar))
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
   !--- Set compid (need namcouple model names)
   !------------------------

   if (len_trim(cdnam) > ic_lvar) then
      WRITE(nulprt1,*) subname,estr,'model name too long = ',trim(cdnam)
      write(nulprt1,*) subname,estr,'max model name length = ',ic_lvar
      call oasis_abort()
   endif

   !------------------------
   !--- Gather model names from all tasks to generate active model list
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
                  WRITE(nulprt1,*) subname,estr,'inconsistent coupled flag in oasis_init_comp'
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

   !--- Broadcast the model list to all MPI tasks
   call oasis_mpi_bcast(prism_nmodels,MPI_COMM_WORLD,subname//' prism_nmodels')
   call oasis_mpi_bcast(prism_amodels,MPI_COMM_WORLD,subname//' prism_amodels')
   call oasis_mpi_bcast(prism_modnam ,MPI_COMM_WORLD,subname//' prism_modnam')
   call oasis_mpi_bcast(prism_modcpl ,MPI_COMM_WORLD,subname//' prism_modcpl')

   !--- Finally compute compid ---

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
   !--- Re-Set MPI info (need compid for MPI1 COMM_SPLIT)
   !------------------------

   mpi_rank_global = -1
#ifdef use_comm_MPI1

   ! Set mpi_comm_local based on compid
   ikey = 0
   icolor = compid
   call MPI_COMM_SPLIT(MPI_COMM_WORLD,icolor,ikey,mpi_comm_local,ierr)

   ! Set mpi_comm_global based on oasis_coupled flag
   ikey = 0
   icolor = 1
   if (.not.oasis_coupled) icolor = 0
   call MPI_COMM_SPLIT(MPI_COMM_WORLD,icolor,ikey,mpi_comm_global,ierr)
!tcx   if (.not.oasis_coupled) mpi_comm_global = MPI_COMM_NULL

#elif defined use_comm_MPI2

   mpi_comm_global = ??
   mpi_comm_local = MPI_COMM_WORLD

#endif

   ! We change debug level (verbose level disabled if load balance analysis)
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
   !--- debug file
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
   !--- mpi_root_global (after nulprt set)
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
   !--- Timer Initialization
   !------------------------

   ! Allocate timer memory based on maxvar
   nt = 7*maxvar+30
   call oasis_timer_init (trim(cdnam), trim(cdnam)//'.timers',nt)
   call oasis_timer_start('total')
   call oasis_timer_start('init_thru_enddef')

   !------------------------
   !--- Diagnostics
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
   SUBROUTINE oasis_terminate(kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
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

   call oasis_timer_stop('total')
   call oasis_timer_print()

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

   IF (mpi_rank_local == 0)  THEN
       WRITE(nulprt,*) subname,' SUCCESSFUL RUN'
       CALL oasis_flush(nulprt)
   ENDIF

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_terminate

!----------------------------------------------------------------------
   SUBROUTINE oasis_get_localcomm(localcomm,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: localcomm
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = '(oasis_get_localcomm)'
!  ---------------------------------------------------------

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
   SUBROUTINE oasis_set_couplcomm(localcomm,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(in)   :: localcomm
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: ierr
   character(len=*),parameter :: subname = '(oasis_set_couplcomm)'
!  ---------------------------------------------------------

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
   SUBROUTINE oasis_create_couplcomm(icpl,allcomm,cplcomm,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(in)   :: icpl
   INTEGER (kind=ip_intwp_p),intent(in)   :: allcomm
   INTEGER (kind=ip_intwp_p),intent(out)  :: cplcomm
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: ierr
   character(len=*),parameter :: subname = '(oasis_create_couplcomm)'
!  ---------------------------------------------------------

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
   SUBROUTINE oasis_get_debug(debug,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: debug
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = '(oasis_get_debug)'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   debug = OASIS_debug

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_get_debug
!----------------------------------------------------------------------
   SUBROUTINE oasis_set_debug(debug,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(in)   :: debug
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = '(oasis_set_debug)'
!  ---------------------------------------------------------

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
   SUBROUTINE oasis_get_intercomm(new_comm, cdnam, kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out) :: new_comm
   CHARACTER(len=*),intent(in) :: cdnam
   INTEGER (kind=ip_intwp_p),intent(out),optional :: kinfo

   INTEGER (kind=ip_intwp_p)	:: n, il, ierr, tag
   LOGICAL :: found
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = '(oasis_get_intercomm)'
!  ---------------------------------------------------------

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
   SUBROUTINE oasis_get_intracomm(new_comm, cdnam, kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out) :: new_comm
   CHARACTER(len=*),intent(in) :: cdnam
   INTEGER (kind=ip_intwp_p),intent(out),optional :: kinfo

   INTEGER (kind=ip_intwp_p)	:: tmp_intercomm
   INTEGER (kind=ip_intwp_p)	:: ierr
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = '(oasis_get_intracomm)'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   call oasis_get_intercomm(tmp_intercomm, cdnam, kinfo)

   CALL mpi_intercomm_merge(tmp_intercomm,.FALSE., new_comm, ierr)

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_get_intracomm
!----------------------------------------------------------------------
   SUBROUTINE oasis_enddef(kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer (kind=ip_intwp_p) :: n
   integer (kind=ip_intwp_p) :: lkinfo
   integer (kind=ip_intwp_p) :: icpl, ierr
   integer (kind=ip_intwp_p) :: newcomm
   character(len=*),parameter :: subname = '(oasis_enddef)'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)

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

   !------------------------
   !--- reset mpi_comm_global
   !--- for changes to mpi_comm_local since init
   !------------------------

   icpl = MPI_UNDEFINED
   if (mpi_comm_local /= MPI_COMM_NULL) icpl = 1
   CALL MPI_COMM_Split(mpi_comm_global,icpl,1,newcomm,ierr)
   mpi_comm_global = newcomm

   !------------------------
   !--- for active tasks only
   !------------------------

   if (mpi_comm_global /= MPI_COMM_NULL) then

      !------------------------
      !--- Update mpi_comm_global
      !------------------------

      CALL MPI_Comm_Size(mpi_comm_global,mpi_size_global,ierr)
      CALL MPI_Comm_Rank(mpi_comm_global,mpi_rank_global,ierr)

      !------------------------
      !--- update mpi_root_global
      !------------------------

      call mod_oasis_setrootglobal()

      !------------------------
      !--- document
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
      !--- reconcile partitions
      !--- generate gsmaps from partitions
      !------------------------

      call oasis_part_setup()

      !------------------------
      !--- reconcile variables
      !------------------------

      call oasis_var_setup()

      !------------------------
      !--- write grid info to files one model at a time
      !------------------------

      do n = 1,prism_amodels
         if (compid == n .and. mpi_rank_local == mpi_root_local) then
            call oasis_write2files()
         endif
         call oasis_mpi_barrier(mpi_comm_global)
      enddo

      !------------------------
      !--- MCT Initialization
      !------------------------

      call mct_world_init(prism_amodels,mpi_comm_global,mpi_comm_local,compid)
      IF (OASIS_debug >= 2)  THEN
         WRITE(nulprt,*) subname, ' done mct_world_init '
         CALL oasis_flush(nulprt)
      ENDIF

      call oasis_coupler_setup()
      IF (OASIS_debug >= 2)  THEN
         WRITE(nulprt,*) subname, ' done prism_coupler_setup '
         CALL oasis_flush(nulprt)
      ENDIF

      call oasis_advance_init(lkinfo)
      IF (OASIS_debug >= 2)  THEN
         WRITE(nulprt,*) subname, ' done prism_advance_init '
         CALL oasis_flush(nulprt)
      ENDIF

   endif   !  (mpi_comm_local /= MPI_COMM_NULL)

   !--- Force OASIS_OK here rather than anything else ---

   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   call oasis_timer_stop('init_thru_enddef')

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_enddef
!----------------------------------------------------------------------
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
