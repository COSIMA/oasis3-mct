MODULE mod_oasis_method

   USE mod_oasis_kinds
   USE mod_oasis_sys
   USE mod_oasis_data
   USE mod_oasis_parameters
   USE mod_oasis_namcouple
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
   SUBROUTINE oasis_init_comp(mynummod,cdnam,kinfo)

   ! This is COLLECTIVE, all pes must call

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: mynummod     
   CHARACTER(len=*)         ,intent(in)    :: cdnam
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: mpi_err
   INTEGER(kind=ip_intwp_p) :: n,nns,iu
   integer(kind=ip_intwp_p) :: icolor,ikey
   CHARACTER(len=ic_med)    :: filename,filename2
   character(len=ic_med)    :: pio_type
   integer(kind=ip_intwp_p) :: pio_stride
   integer(kind=ip_intwp_p) :: pio_root
   integer(kind=ip_intwp_p) :: pio_numtasks
   character(len=*),parameter :: subname = 'oasis_init_comp'
!  ---------------------------------------------------------

   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif
   call oasis_data_zero()

   !------------------------
   !--- Initialize MPI
   !------------------------

   lg_mpiflag = .FALSE.
   CALL MPI_Initialized ( lg_mpiflag, mpi_err )
   IF ( .NOT. lg_mpiflag ) THEN
      if (OASIS_debug >= 0) WRITE (0,FMT='(A)') subname//': Calling MPI_Init'
      CALL MPI_INIT ( mpi_err )
   else
      if (OASIS_debug >= 0) WRITE (0,FMT='(A)') subname//': Not Calling MPI_Init'
   ENDIF

#ifdef use_comm_MPI1
   mpi_comm_global = MPI_COMM_WORLD
#elif defined use_comm_MPI2
   mpi_comm_global = ??
#endif

   CALL MPI_Comm_Size(mpi_comm_global,mpi_size_global,mpi_err)
   CALL MPI_Comm_Rank(mpi_comm_global,mpi_rank_global,mpi_err)

   !------------------------
   !--- nout file, need mpi_rank_global
   !------------------------

   iu=-1

   call oasis_unitsetmin(1024)
   IF (mpi_rank_global == 0) THEN
       CALL oasis_unitget(iu)
       nulprt1 = iu
       WRITE(filename,'(a,i6.6)') 'nout.',mpi_rank_global
       OPEN(nulprt1,file=filename)
   ENDIF

   !------------------------
   !--- Initialize namcouple
   !--- first on rank 0 to write error messages
   !--- then on all other ranks
   !------------------------

   IF (mpi_rank_global == 0) THEN
      call oasis_namcouple_init()
   endif
   call oasis_mpi_barrier(mpi_comm_global)
   IF (mpi_rank_global /= 0) THEN
      call oasis_namcouple_init()
   endif
   OASIS_debug = namlogprt

   ! If NFIELDS=0 there is no coupling
   ! No information must be written in the debug files as
   ! the different structures are not allocated
   !
   IF ( nnamcpl == 0 ) THEN
       WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
       WRITE (UNIT = nulprt1,FMT = *)  &
          ' The models are not exchanging any field ($NFIELDS = 0) '
       WRITE (UNIT = nulprt1,FMT = *)  &
          ' so we force OASIS_debug = 0 for all processors '
       OASIS_debug = 0
   ENDIF

   ! Determines the total number of fields to avoid a parameter in oasis_def_var
   ! and mod_oasis_coupler
   mvar=0
   DO nns = 1,nnamcpl
     n = namfldsort(nns)
     mvar = mvar + oasis_string_listGetNum(namsrcfld(n))
   ENDDO
   WRITE (UNIT = nulprt1,FMT = *) 'Total number of coupling fields :',mvar
   !
   ALLOCATE(prism_var(mvar))
   !
   ! Define mtimer as a function of mvars instead of a parameter
   mtimer = 7*mvar+30
   ALLOCATE(timer(mtimer))
   ALLOCATE(sum_ctime(mtimer))
   ALLOCATE(sum_wtime(mtimer))
   ALLOCATE(timer_count(mtimer))

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
           WRITE(nulprt1,*) subname,' model not found in namcouple ',&
                            TRIM(cdnam)
       ENDIF
       CALL oasis_abort_noarg()
   endif

   IF (mpi_rank_global == 0) CLOSE(nulprt1)

   !------------------------
   !--- Re-Set MPI info (need compid for MPI1 COMM_SPLIT)
   !------------------------

#ifdef use_comm_MPI1

   mpi_comm_global = MPI_COMM_WORLD
   ikey = compid
   icolor = compid
   call MPI_COMM_SPLIT(MPI_COMM_WORLD,icolor,ikey,mpi_comm_local,mpi_err)

#elif defined use_comm_MPI2

   mpi_comm_global = ??
   mpi_comm_local = MPI_COMM_WORLD

#endif

!------------------------------------

   CALL MPI_Comm_Size(mpi_comm_global,mpi_size_global,mpi_err)
   CALL MPI_Comm_Rank(mpi_comm_global,mpi_rank_global,mpi_err)

   CALL MPI_Comm_Size(mpi_comm_local,mpi_size_local,mpi_err)
   CALL MPI_Comm_Rank(mpi_comm_local,mpi_rank_local,mpi_err)
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
               WRITE(filename2,'(a,i2.2)') 'debug_notroot.',compid
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
       ENDIF

   call oasis_debug_enter(subname)

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

   call oasis_timer_init (trim(cdnam), trim(cdnam)//'.timers', mpi_comm_local)
   call oasis_timer_start('total after init')

   !------------------------
   !--- Diagnostics
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
      write(nulprt,*) subname,' prism models: '
      call oasis_flush(nulprt)
   endif

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_init_comp

!----------------------------------------------------------------------
   SUBROUTINE oasis_terminate(kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: mpi_err
   character(len=*),parameter :: subname = 'oasis_terminate'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   call oasis_timer_stop('total after init')
   call oasis_timer_print()

   call oasis_mpi_barrier(mpi_comm_global)
   IF ( .NOT. lg_mpiflag ) THEN
       IF (OASIS_debug >= 2)  THEN
           WRITE (nulprt,FMT='(A)') subname//': Calling MPI_Finalize'
       ENDIF
       CALL MPI_Finalize ( mpi_err )
   else
       IF (OASIS_debug >= 2)  THEN
           WRITE (nulprt,FMT='(A)') subname//': Not Calling MPI_Finalize'
       ENDIF
   ENDIF

   IF (OASIS_debug >= 2)  THEN
       WRITE(nulprt,*) subname,' SUCCESSFUL RUN'
   ENDIF

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_terminate

!----------------------------------------------------------------------
   SUBROUTINE oasis_get_localcomm(localcomm,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: localcomm
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_get_localcomm'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   ! from prism_data
   localcomm = mpi_comm_local

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_get_localcomm
!----------------------------------------------------------------------
   SUBROUTINE oasis_set_couplcomm(localcomm,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(in)   :: localcomm
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_set_couplcomm'
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
      CALL MPI_Comm_Size(mpi_comm_local,mpi_size_local,mpi_err)
      CALL MPI_Comm_Rank(mpi_comm_local,mpi_rank_local,mpi_err)
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
   integer(kind=ip_intwp_p) :: mpi_err
   character(len=*),parameter :: subname = 'oasis_create_couplcomm'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   !------------------------
   !--- generate cplcomm from allcomm and icpl
   !------------------------

   CALL MPI_COMM_Split(allcomm,icpl,1,cplcomm,mpi_err)
   IF (mpi_err /= 0) THEN
      WRITE (nulprt,*) subname,' ERROR: MPI_Comm_Split abort ',mpi_err
      call oasis_abort_noarg()
   ENDIF

   !------------------------
   !--- update mpi_comm_local from component
   !------------------------

   call oasis_set_couplcomm(cplcomm)

   IF (OASIS_debug >= 2)  THEN
       WRITE (nulprt,*) 'New local coupling comm =',cplcomm
   ENDIF

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_create_couplcomm
!----------------------------------------------------------------------
   SUBROUTINE oasis_get_debug(debug,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: debug
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_get_debug'
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
   character(len=*),parameter :: subname = 'oasis_set_debug'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   OASIS_debug = debug
   if (OASIS_debug >= 2) then
      write(nulprt,*) subname,' set OASIS_debug to ',OASIS_debug
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
   character(len=*),parameter :: subname = 'oasis_get_intercomm'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   found = .false.
   do n = 1,prism_nmodels
      if (trim(cdnam) == trim(prism_modnam(n))) then
         if (found) then
            write(nulprt,*) subname,' ERROR: found same model name twice'
            WRITE(nulprt,*) subname,' abort by model :',compid,&
            ' proc :',mpi_rank_local
            call oasis_abort_noarg()
         endif
         il = n
         found = .true.
      endif
   enddo

   if (.not. found) then
      write(nulprt,*) subname,' ERROR: input model name not found'
      WRITE(nulprt,*) subname,' abort by model :',compid,&
      ' proc :',mpi_rank_local
      call oasis_abort_noarg()
   endif

   tag=ICHAR(TRIM(compnm))+ICHAR(TRIM(cdnam))
   CALL mpi_intercomm_create(mpi_comm_local, 0, MPI_COMM_WORLD, &
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
   character(len=*),parameter :: subname = 'oasis_get_intracomm'
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
   integer(kind=ip_intwp_p),allocatable :: tmparr(:)
   character(len=*),parameter :: subname = 'oasis_enddef'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   lkinfo = OASIS_OK

   !------------------------
   !--- write grid info to files one model at a time
   !------------------------

   do n = 1,prism_nmodels
      if (compid == n .and. mpi_rank_local == mpi_root_local) then
         call oasis_write2files()
      endif
      call oasis_mpi_barrier(mpi_comm_global)
   enddo

   !------------------------
   !--- derive mpi_root_global
   !------------------------

   allocate(mpi_root_global(prism_nmodels))
   allocate(tmparr(prism_nmodels))
   tmparr = -1
   do n = 1,prism_nmodels
      if (compid == n .and. &
          mpi_rank_local == mpi_root_local) then
         tmparr(n) = mpi_rank_global
      endif
   enddo
   call oasis_mpi_max(tmparr,mpi_root_global,MPI_COMM_WORLD, &
      string=subname//':mpi_root_global',all=.true.)
   deallocate(tmparr)

   if (OASIS_debug >= 2)  then
      do n = 1,prism_nmodels
         write(nulprt,*) subname,'   n,prism_model,root = ',&
         n,trim(prism_modnam(n)),mpi_root_global(n)
      enddo
      call oasis_flush(nulprt)
   endif

   do n = 1,prism_nmodels
      if (mpi_root_global(n) < 0) then
         write(nulprt,*) subname,'   n,prism_model,root = ',&
         n,trim(prism_modnam(n)),mpi_root_global(n)
         write(nulprt,*) subname,' ERROR: global root invalid, &
         & check couplcomm for active tasks'
         call oasis_abort_noarg()
      endif
   enddo

   !------------------------
   !--- MCT Initialization
   !------------------------

   call mct_world_init(prism_nmodels,mpi_comm_global,mpi_comm_local,compid)
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

   !--- Force OASIS_OK here rather than anything else ---

   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_enddef
!----------------------------------------------------------------------

END MODULE mod_oasis_method
