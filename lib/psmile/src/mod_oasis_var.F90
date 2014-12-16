
!> OASIS variable data and methods

  MODULE mod_oasis_var

  USE mod_oasis_kinds
  USE mod_oasis_data
  USE mod_oasis_parameters
  USE mod_oasis_sys
  USE mod_oasis_mpi
  USE mod_oasis_timer
  USE mod_oasis_part

  IMPLICIT none

  private

  !--- interfaces ---
  public oasis_def_var
  public oasis_var_setup

  !--- datatypes ---

  integer(ip_intwp_p),public   :: maxvar  !< number of potential variables, derived from namcouple input
  integer(kind=ip_i4_p),parameter,public :: mvarcpl = 10   !< max namcouples per variable

  !> Model variable data for model coupling
  type prism_var_type
     character(len=ic_lvar):: name  !< variable name
     integer(kind=ip_i4_p) :: part  !< variable partition
     integer(kind=ip_i4_p) :: ndim  !< rank of variable
     integer(kind=ip_i4_p) :: num   !< size of variable
     integer(kind=ip_i4_p) :: ops   !< input or output
     integer(kind=ip_i4_p) :: type  !< type kind of variable
     integer(kind=ip_i4_p) :: size  !< total size of field
     integer(kind=ip_i4_p) :: ncpl  !< number of namcouple couplers
     integer(kind=ip_i4_p) :: cpl(mvarcpl)  !< list of namcouple couplers
  end type prism_var_type

  integer(kind=ip_intwp_p),public :: prism_nvar = 0    !< number of variables defined
  TYPE(prism_var_type),POINTER,public :: prism_var(:)  !< list of defined variables

  CONTAINS

!---------------------------------------------------------------

!> The OASIS user interface to define variables

  SUBROUTINE oasis_def_var(id_nports, cdport, id_part, &
         id_var_nodims, kinout, id_var_shape, ktype, kinfo)
     !---------------------------------------------------------------
     INTEGER(kind=ip_i4_p),intent(out) :: id_nports    !< coupling field ID
     CHARACTER(len=*)     ,intent(in)  :: cdport       !< field name as in namcouple
     INTEGER(kind=ip_i4_p),intent(in)  :: id_part      !< partition ID
     INTEGER(kind=ip_i4_p),intent(in)  :: id_var_nodims(2)  !< rank and number of bundles
     INTEGER(kind=ip_i4_p),intent(in)  :: kinout       !< input or output flag
     INTEGER(kind=ip_i4_p),intent(in)  :: id_var_shape(2*id_var_nodims(1)) !< size of field
     INTEGER(kind=ip_i4_p),intent(in)  :: ktype        !< type of coupling field
     INTEGER(kind=ip_i4_p),intent(out),optional :: kinfo    !< return code
     !---------------------------------------------------------------
     INTEGER(kind=ip_i4_p)  :: n
     CHARACTER(len=ic_lvar) :: trimmed_cdport   ! Trimmed version of cdport
     character(len=*),parameter :: subname = '(oasis_def_var)'
     LOGICAL    :: l_field_in_namcouple
     !---------------------------------------------------------------

     call oasis_debug_enter(subname)
     if (.not. oasis_coupled) then
        call oasis_debug_exit(subname)
        return
     endif

     !-------------------------------------------------     
     !> * Check len of incoming variable name
     ! Trim incoming name once to avoid multiple trim operations
     ! in subsequent loops
     !-------------------------------------------------     

     if (len_trim(cdport) > ic_lvar) then
        WRITE(nulprt,*) subname,estr,'variable too long = ',trim(cdport)
        WRITE(nulprt,*) subname,estr,'max variable length (ic_lvar) = ',ic_lvar
        call oasis_abort()
     endif
     trimmed_cdport = trim(cdport)

     kinfo = OASIS_Ok

     l_field_in_namcouple = .FALSE.
     n = 0

     !-------------------------------------------------     
     !> * Search for field in namcouple field lists
     !-------------------------------------------------     

     ! If either condition ceases to be true then bail out of the loop
     DO WHILE (n < size_namfld .AND. (.NOT.l_field_in_namcouple))
        n = n+1
        IF ( (trimmed_cdport == total_namsrcfld(n)).OR.    &
             (trimmed_cdport == total_namdstfld(n)) ) THEN 
              l_field_in_namcouple = .TRUE.
        ENDIF       
     enddo

     !-------------------------------------------------     
     !> * Return if field not found in namcouple
     !-------------------------------------------------     

     if (.not. l_field_in_namcouple) then
        id_nports = OASIS_Var_Uncpl
        if (OASIS_debug >= 2) then
           write(nulprt,*) subname,' variable not in namcouple return ',trimmed_cdport
           call oasis_flush(nulprt)
        endif
        call oasis_debug_exit(subname)
        return
     endif

     !-------------------------------------------------     
     !> * Abort if field already defined
     !-------------------------------------------------     

     do n = 1,prism_nvar
        if (trimmed_cdport == prism_var(n)%name) then
           write(nulprt,*) subname,estr,'variable already defined with def_var = ',trimmed_cdport
           write(nulprt,*) subname,estr,'check oasis_def_var calls in your model'
           call oasis_abort()
        endif
     enddo

     !-------------------------------------------------     
     !> * Increment the variable and store the values
     !-------------------------------------------------     

     prism_nvar = prism_nvar + 1
     id_nports = prism_nvar

     if (prism_nvar > maxvar) then
        write(nulprt,*) subname,estr,'prism_nvar too large = ',prism_nvar,maxvar
        write(nulprt,*) subname,estr,'check maxvar set in oasis_init_comp'
        call oasis_abort()
     endif

     call oasis_var_zero(prism_var(prism_nvar))
     prism_var(prism_nvar)%name = trimmed_cdport
     prism_var(prism_nvar)%part = id_part
     prism_var(prism_nvar)%ndim = id_var_nodims(1)
     prism_var(prism_nvar)%num  = id_var_nodims(2)
     prism_var(prism_nvar)%ops  = kinout
     prism_var(prism_nvar)%type = ktype
     prism_var(prism_nvar)%size = 1
     do n = 1,prism_var(prism_nvar)%ndim
        prism_var(prism_nvar)%size = prism_var(prism_nvar)%size*(id_var_shape(2*n)-&
                                     id_var_shape(2*n-1)+1)
     enddo
     prism_var(prism_nvar)%ncpl = 0
     prism_var(prism_nvar)%cpl  = 0

     !----------------------------------
     !> * Write some diagnostics
     !----------------------------------

     if (OASIS_debug >= 2) then
        write(nulprt,*) ' '
        write(nulprt,*) subname,' prism_nvar    = ',prism_nvar
        write(nulprt,*) subname,' varname = ',prism_nvar,trim(prism_var(prism_nvar)%name)
        write(nulprt,*) subname,' varpart = ',prism_nvar,prism_var(prism_nvar)%part
        write(nulprt,*) subname,' varndim = ',prism_nvar,prism_var(prism_nvar)%ndim
        write(nulprt,*) subname,' varnum  = ',prism_nvar,prism_var(prism_nvar)%num
        write(nulprt,*) subname,' varops  = ',prism_nvar,prism_var(prism_nvar)%ops
        write(nulprt,*) subname,' vartype = ',prism_nvar,prism_var(prism_nvar)%type
        write(nulprt,*) subname,' varsize = ',prism_nvar,prism_var(prism_nvar)%size
        write(nulprt,*) ' '
        CALL oasis_flush(nulprt)
     endif

     call oasis_debug_exit(subname)

   END SUBROUTINE oasis_def_var

!---------------------------------------------------------------

!> Synchronize variables across all tasks, called at oasis enddef.

  SUBROUTINE oasis_var_setup()
   IMPLICIT NONE

   !--------------------------------------------------------
   integer(kind=ip_intwp_p) :: m,n,p,v
   INTEGER(kind=ip_intwp_p) :: ierr, taskid
   integer(kind=ip_intwp_p) :: vcnt, tot_vnum, nvarroot, varcheck, varcheckall
   integer(kind=ip_intwp_p) :: status(MPI_STATUS_SIZE)  ! mpi status info
   logical                  :: found
   character(len=ic_lvar)  ,pointer :: loc_vname(:),vname0(:),vname(:), loc_vname0(:), vname1(:)
   character(len=ic_lvar2) ,pointer :: loc_pname(:),pname0(:),pname(:), loc_pname0(:), pname1(:)
   integer(kind=ip_intwp_p),pointer :: loc_inout(:),inout0(:),inout(:), loc_inout0(:), inout1(:)
   character(len=ic_lvar)  ,pointer :: root_vname(:)
   integer(kind=ip_intwp_p),pointer :: vnum(:),rcnts(:),displ(:)
   logical, parameter :: local_timers_on = .false.
   logical, parameter :: gatherall_on = .false.
   character(len=*),parameter :: subname = '(oasis_var_setup)'
   !--------------------------------------------------------

   call oasis_debug_enter(subname)

   call oasis_timer_start('var_setup')

   !-------------------------------------------------     
   !> * Check whether all tasks have same vars defined.
   !>   If so, then skip a bunch of the work to sort it out.
   !-------------------------------------------------     

   if (local_timers_on) call oasis_timer_start('var_setup_check1')

   nvarroot = 0
   if (mpi_rank_local == 0) then
     nvarroot = prism_nvar
   endif
   call oasis_mpi_bcast(nvarroot, mpi_comm_local, subname//' nvarroot')

   allocate(root_vname(nvarroot))
   root_vname = ' '
   if (mpi_rank_local == 0) then
      do n = 1,prism_nvar
         root_vname(n) = prism_var(n)%name
      enddo
   endif
   call oasis_mpi_bcast(root_vname, mpi_comm_local, subname//' root_vname')

   varcheck = 1
   if (OASIS_DEBUG >= 20) then
      write(nulprt,*) subname,' nvarroot ',nvarroot,prism_nvar
   endif
   if (nvarroot /= prism_nvar) varcheck = 0
   n = 0
   do while (varcheck == 1 .and. n < prism_nvar)
      n = n + 1
      if (root_vname(n) /= prism_var(n)%name) varcheck = 0
      if (OASIS_DEBUG >= 20) then
         write(nulprt,*) subname,' root_vname ',n,trim(root_vname(n)),' ',trim(prism_var(n)%name),varcheck
      endif
   enddo
   call oasis_mpi_min(varcheck,varcheckall,mpi_comm_local, subname//' varcheck',all=.true.)

   if (OASIS_DEBUG >= 15) then
      write(nulprt,*) subname,' varcheck = ',varcheck,varcheckall
   endif
   if (local_timers_on) call oasis_timer_stop ('var_setup_check1')

   !-------------------------------------------------     
   ! all vars same on all tasks, just return
   !-------------------------------------------------     

   if (varcheckall == 1) then
      call oasis_timer_stop('var_setup')
      call oasis_debug_exit(subname)
      return
   endif

   !-----------------------------------------------------------------

   !-------------------------------------------------     
   !> * Gather variable information across all tasks
   !-------------------------------------------------     

   if (local_timers_on) call oasis_timer_start('var_setup_gather')

   allocate(vnum(mpi_size_local))
   vnum = 0

   !-------------------------------------------------     
   !>   * Gather number of variables
   !-------------------------------------------------     

   call MPI_GATHER(prism_nvar, 1, MPI_INTEGER, vnum, 1, MPI_INTEGER, 0, mpi_comm_local, ierr)
   if (local_timers_on) call oasis_timer_stop('var_setup_gather')

 if (gatherall_on) then

   !-------------------------------------------------     
   !>   * If gatherall, Gather variable names
   !-------------------------------------------------     

   if (local_timers_on) call oasis_timer_start('var_setup_gather')
   if (mpi_rank_local == 0) then
      tot_vnum = sum(vnum)
      allocate(vname0(tot_vnum))
      allocate(rcnts(mpi_size_local),displ(mpi_size_local))
      do m = 1,mpi_size_local
         rcnts(m) = vnum(m) * ic_lvar
         if (m == 1) then
            displ(m) = 0
         else
            displ(m) = displ(m-1) + rcnts(m-1)
         endif
      enddo
   else
      allocate(vname0(1),rcnts(1),displ(1))
   endif

   allocate(loc_vname(prism_nvar))
   do n = 1,prism_nvar
      loc_vname(n) = prism_var(n)%name
   enddo
   call MPI_GATHERV(loc_vname, prism_nvar*ic_lvar, MPI_CHARACTER, vname0, rcnts, displ, MPI_CHARACTER, 0, mpi_comm_local, ierr) 
   deallocate(loc_vname)
   deallocate(rcnts, displ)

   !-------------------------------------------------     
   !>   * If gatherall, Gather partition names
   !-------------------------------------------------     

   if (mpi_rank_local == 0) then
      tot_vnum = sum(vnum)
      allocate(pname0(tot_vnum))
      allocate(rcnts(mpi_size_local),displ(mpi_size_local))
      do n = 1,mpi_size_local
         rcnts(n) = vnum(n) * ic_lvar2
         if (n == 1) then
            displ(n) = 0
         else
            displ(n) = displ(n-1) + rcnts(n-1)
         endif
      enddo
   else
      allocate(pname0(1),rcnts(1),displ(1))
   endif

   allocate(loc_pname(prism_nvar))
   do n = 1,prism_nvar
      loc_pname(n) = prism_part(prism_var(n)%part)%partname
   enddo
   call MPI_GATHERV(loc_pname, prism_nvar*ic_lvar2, MPI_CHARACTER, pname0, rcnts, displ, MPI_CHARACTER, 0, mpi_comm_local, ierr) 
   deallocate(loc_pname)

   !-------------------------------------------------     
   !>   * If gatherall, Gather in/out variable setting
   !-------------------------------------------------     

   if (mpi_rank_local == 0) then
      tot_vnum = sum(vnum)
      allocate(inout0(tot_vnum))
      allocate(rcnts(mpi_size_local),displ(mpi_size_local))
      do n = 1,mpi_size_local
         rcnts(n) = vnum(n)
         if (n == 1) then
            displ(n) = 0
         else
            displ(n) = displ(n-1) + rcnts(n-1)
         endif
      enddo
   else
      allocate(inout0(1),rcnts(1),displ(1))
   endif

   allocate(loc_inout(prism_nvar))
   do n = 1,prism_nvar
      loc_inout(n) = prism_var(n)%ops
   enddo
   call MPI_GATHERV(loc_inout, prism_nvar, MPI_INTEGER, inout0, rcnts, displ, MPI_INTEGER, 0, mpi_comm_local, ierr) 
   deallocate(loc_inout)
   if (local_timers_on) call oasis_timer_stop ('var_setup_gather')

   !-------------------------------------------------     
   !> * If gatherall, Determine the unique variable names on the root
   !-------------------------------------------------     

   if (local_timers_on) call oasis_timer_start('var_setup_rootsrch')
   if (mpi_rank_local == 0) then
      vcnt = 0
      do n = 1,tot_vnum
         if (OASIS_Debug >= 15) &
            write(nulprt,*) subname,' check vname0 ',n,trim(vname0(n))
         v = 0
         found = .false.
         do while (v < vcnt .and. .not.found)
            v = v + 1
            if (vname0(n) == vname0(v)) then
               found = .true.
               !--- check that var, part, and inout consistent on all tasks
               if (pname0(n) /= pname0(v) .or. inout0(n) /= inout0(v)) then
                  write(nulprt,*) subname,estr,'inconsistent var and part name: ',&
                                  trim(vname0(n)),' ',trim(pname0(n)),' ',trim(pname0(v))
                  write(nulprt,*) subname,estr,'inconsistent var and inout opt: ',&
                                  trim(vname0(n)),' ',inout0(n),' ',inout0(v)
                  call oasis_abort()
               endif
            endif
         enddo
         if (.not.found) then
            vcnt = vcnt + 1
            vname0(vcnt) = vname0(n)
            pname0(vcnt) = pname0(n)
            inout0(vcnt) = inout0(n)
         endif
      enddo
   endif
   if (local_timers_on) call oasis_timer_stop ('var_setup_rootsrch')

 else   ! gatherall_on

   allocate(loc_vname(prism_nvar))
   allocate(loc_pname(prism_nvar))
   allocate(loc_inout(prism_nvar))
   do n = 1,prism_nvar
      loc_vname(n) = prism_var(n)%name
      loc_inout(n) = prism_var(n)%ops
      loc_pname(n) = prism_part(prism_var(n)%part)%partname
   enddo

   if (mpi_rank_local == 0) then
      vcnt = 0
      allocate(pname0(max(prism_nvar,20)))  ! 20 is arbitrary starting number
      allocate(vname0(max(prism_nvar,20)))  ! 20 is arbitrary starting number
      allocate(inout0(max(prism_nvar,20)))  ! 20 is arbitrary starting number
   else
      allocate(pname0(1))
      allocate(vname0(1))
      allocate(inout0(1))
   endif

   !-------------------------------------------------     
   !> * If not gatherall, Loop over each task
   !-------------------------------------------------     

   do m = 1,mpi_size_local
      taskid = m - 1

      !-------------------------------------------------     
      !>   * If not gatherall, Send variable, partition, inout to root
      !-------------------------------------------------     

      if (mpi_rank_local == taskid .and. prism_nvar > 0) then
         if (local_timers_on) call oasis_timer_start('var_setup_gather')
         if (mpi_rank_local /= 0) then
            if (OASIS_Debug >= 15) then
               write(nulprt,*) subname,' send prism_nvar ',mpi_rank_local,m,prism_nvar,ic_lvar2
               call oasis_flush(nulprt)
            endif
            call MPI_SEND(loc_vname, prism_nvar*ic_lvar , MPI_CHARACTER, 0, 10000+m, mpi_comm_local, ierr)
            call oasis_mpi_chkerr(ierr,subname//':send vname')
            call MPI_SEND(loc_pname, prism_nvar*ic_lvar2, MPI_CHARACTER, 0, 50000+m, mpi_comm_local, ierr)
            call oasis_mpi_chkerr(ierr,subname//':send pname')
            call MPI_SEND(loc_inout, prism_nvar         , MPI_INTEGER  , 0, 90000+m, mpi_comm_local, ierr)
            call oasis_mpi_chkerr(ierr,subname//':send pname')
         endif
         if (local_timers_on) call oasis_timer_stop ('var_setup_gather')
      endif

      !-------------------------------------------------     
      !>   * If not gatherall, Recv variable, partition, inout on root
      !>   * If not gatherall, Determine the unique variable names on the root
      !-------------------------------------------------     

      if (mpi_rank_local == 0 .and. vnum(m) > 0) then
         if (local_timers_on) call oasis_timer_start ('var_setup_gather')
         if (OASIS_Debug >= 15) then
            write(nulprt,*) subname,' recv prism_nvar ',mpi_rank_local,m,vnum(m),ic_lvar2
            call oasis_flush(nulprt)
         endif
         allocate(loc_vname0(vnum(m)))
         allocate(loc_pname0(vnum(m)))
         allocate(loc_inout0(vnum(m)))
         if (taskid == 0) then
            loc_vname0 = loc_vname   ! copy local values
            loc_pname0 = loc_pname   ! copy local values
            loc_inout0 = loc_inout   ! copy local values
         else
            call MPI_RECV(loc_vname0, vnum(m)*ic_lvar , MPI_CHARACTER, taskid, 10000+m, mpi_comm_local, status, ierr)
            call oasis_mpi_chkerr(ierr,subname//':recv')
            call MPI_RECV(loc_pname0, vnum(m)*ic_lvar2, MPI_CHARACTER, taskid, 50000+m, mpi_comm_local, status, ierr)
            call oasis_mpi_chkerr(ierr,subname//':recv')
            call MPI_RECV(loc_inout0, vnum(m)         , MPI_INTEGER  , taskid, 90000+m, mpi_comm_local, status, ierr)
            call oasis_mpi_chkerr(ierr,subname//':recv')
         endif
         if (local_timers_on) call oasis_timer_stop ('var_setup_gather')

         if (local_timers_on) call oasis_timer_start('var_setup_rootsrch')
         do n = 1,vnum(m)
            if (OASIS_Debug >= 15) write(nulprt,*) subname,' check loc_vname0 ',m,n,trim(loc_vname0(n))

            v = 0
            found = .false.
            do while (v < vcnt .and. .not.found)
               v = v + 1
               if (loc_vname0(n) == vname0(v)) then
                  found = .true.
                  !--- check that var, part, and inout consistent on all tasks
                  if (loc_pname0(n) /= pname0(v) .or. loc_inout0(n) /= inout0(v)) then
                     write(nulprt,*) subname,estr,'inconsistent var and part name: ',&
                                     trim(loc_vname0(n)),' ',trim(loc_pname0(n)),' ',trim(pname0(v))
                     write(nulprt,*) subname,estr,'inconsistent var and inout opt: ',&
                                     trim(loc_vname0(n)),' ',loc_inout0(n),' ',inout0(v)
                     call oasis_abort()
                  endif
               endif
            enddo
            if (.not.found) then
               vcnt = vcnt + 1
               if (vcnt > size(vname0)) then
                  allocate(vname1(size(vname0)))
                  allocate(pname1(size(pname0)))
                  allocate(inout1(size(inout0)))
                  vname1 = vname0
                  pname1 = pname0
                  inout1 = inout0
                  deallocate(vname0,pname0,inout0)
                  if (OASIS_Debug >= 15) then
                     write(nulprt,*) subname,' resize vname0 ',size(vname1),vcnt+vnum(m)
                     call oasis_flush(nulprt)
                  endif
                  allocate(vname0(vcnt+vnum(m)))
                  allocate(pname0(vcnt+vnum(m)))
                  allocate(inout0(vcnt+vnum(m)))
                  vname0(1:size(vname1)) = vname1(1:size(vname1))
                  pname0(1:size(pname1)) = pname1(1:size(pname1))
                  inout0(1:size(inout1)) = inout1(1:size(inout1))
                  deallocate(vname1,pname1,inout1)
               endif
               vname0(vcnt) = loc_vname0(n)
               pname0(vcnt) = loc_pname0(n)
               inout0(vcnt) = loc_inout0(n)
            endif
          enddo  ! vnum
         deallocate(loc_vname0)
         deallocate(loc_pname0)
         deallocate(loc_inout0)
         if (local_timers_on) call oasis_timer_stop('var_setup_rootsrch')

      endif  ! rank == 0 and vnum > 0
   enddo  ! mpi_size_local

   deallocate(loc_vname)
   deallocate(loc_pname)
   deallocate(loc_inout)

 endif  ! gatherall_on

   deallocate(vnum)

   !-------------------------------------------------     
   !> * Broadcast the variable information to all tasks
   !-------------------------------------------------     

   if (local_timers_on) call oasis_timer_start('var_setup_bcast')
   call oasis_mpi_bcast(vcnt,mpi_comm_local,subname//' vcnt')
   allocate(vname(vcnt))
   allocate(pname(vcnt))
   allocate(inout(vcnt))
   if (mpi_rank_local == 0) then
      vname(1:vcnt) = vname0(1:vcnt)
      pname(1:vcnt) = pname0(1:vcnt)
      inout(1:vcnt) = inout0(1:vcnt)
   endif
   deallocate(vname0)
   deallocate(pname0)
   deallocate(inout0)
   call oasis_mpi_bcast(vname,mpi_comm_local,subname//' vname')
   call oasis_mpi_bcast(pname,mpi_comm_local,subname//' pname')
   call oasis_mpi_bcast(inout,mpi_comm_local,subname//' inout')

   !--- document

   if (OASIS_debug >= 15) then
      do n = 1,vcnt
         write(nulprt,*) subname,' variables: ',n,trim(vname(n)),' ',trim(pname(n)),inout(n)
      enddo
   endif
   if (local_timers_on) call oasis_timer_stop ('var_setup_bcast')

   !-------------------------------------------------     
   !> * Initialize variables on tasks where they are not previously defined.
   !-------------------------------------------------     

   if (local_timers_on) call oasis_timer_start('var_setup_initvar')
   do v = 1,vcnt

      !--- either a prism_var that already exists
      found = .false.
      n = 0
      do while (n < prism_nvar .and. .not.found)
         n = n + 1
         if (prism_var(n)%name == vname(v)) then
            found = .true.
         endif
      enddo

      !--- or a new prism_var that must be instantiated
      if (.not.found) then
         prism_nvar = prism_nvar + 1

         call oasis_var_zero(prism_var(prism_nvar))
         prism_var(prism_nvar)%name = vname(v)
         prism_var(prism_nvar)%ops  = inout(v)
         prism_var(prism_nvar)%ncpl = 0
         !--- figure out the local part id for the part name
         p = 0
         found = .false.
         do while (p < prism_npart .and. .not.found)
            p = p + 1
            if (prism_part(p)%partname == pname(v)) then
               found = .true.
            endif
         enddo
         if (found) then
            prism_var(prism_nvar)%part = p
            if (OASIS_debug >= 15) then
               write(nulprt,*) subname,' found part match ',trim(vname(v)),trim(pname(v)),p
            endif
         else
            write(nulprt,*) subname,estr,'prism part not found part = ',trim(pname(v)),' var = ',trim(vname(v))
            call oasis_abort()
         endif
   
         if (OASIS_debug >= 2) then
            write(nulprt,*) ' '
            write(nulprt,*) subname,' add var = ',prism_nvar,trim(prism_var(prism_nvar)%name),&
                            prism_var(prism_nvar)%part,&
                            trim(prism_part(prism_var(prism_nvar)%part)%partname),prism_var(prism_nvar)%ops
            CALL oasis_flush(nulprt)
         ENDIF
      endif

   enddo   ! v = 1,vcnt
   if (local_timers_on) call oasis_timer_stop ('var_setup_initvar')

   call oasis_timer_stop('var_setup')
      
   call oasis_debug_exit(subname)

   END SUBROUTINE oasis_var_setup

!---------------------------------------------------------------

!> Zero variable information

  SUBROUTINE oasis_var_zero(prism_var)
   IMPLICIT NONE

   !--------------------------------------------------------
   type(prism_var_type),intent(inout) :: prism_var
   character(len=*),parameter :: subname = '(oasis_var_zero)'
   !--------------------------------------------------------

   call oasis_debug_enter(subname)

   prism_var%name = 'oasis_var_name_unset'
   prism_var%part = -1
   prism_var%ndim = -1
   prism_var%num  = -1
   prism_var%ops  = -1
   prism_var%type = -1
   prism_var%size = -1
   prism_var%ncpl = 0
   prism_var%cpl  = -1

   call oasis_debug_exit(subname)

   END SUBROUTINE oasis_var_zero

!---------------------------------------------------------------
 END MODULE mod_oasis_var

