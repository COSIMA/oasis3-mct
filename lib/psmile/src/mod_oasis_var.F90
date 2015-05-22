
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
   integer(kind=ip_intwp_p) :: vcnt
   logical                  :: found, fastcheckout
   character(len=ic_lvar)  ,pointer :: vname0(:),vname(:)
   character(len=ic_lvar2) ,pointer :: pname0(:),pname(:)
   integer(kind=ip_intwp_p),pointer :: inout0(:),inout(:)
   logical, parameter :: local_timers_on = .false.
   character(len=*),parameter :: subname = '(oasis_var_setup)'
   !--------------------------------------------------------

   call oasis_debug_enter(subname)

   IF (local_timers_on) call oasis_timer_start('var_setup')

   IF (local_timers_on) call oasis_timer_start('var_setup_reducelists')
   allocate(vname0(prism_nvar))
   allocate(pname0(prism_nvar))
   allocate(inout0(prism_nvar))
   do n = 1,prism_nvar
      vname0(n) = prism_var(n)%name
      inout0(n) = prism_var(n)%ops
      pname0(n) = prism_part(prism_var(n)%part)%partname
   enddo

   call oasis_mpi_reducelists(vname0,mpi_comm_local,vcnt,vname,'var_setup', &
        fastcheck=.true.,fastcheckout=fastcheckout, &
        linp2=pname0,lout2=pname,linp3=inout0,lout3=inout)

   deallocate(vname0)
   deallocate(pname0)
   deallocate(inout0)
   IF (local_timers_on) call oasis_timer_stop('var_setup_reducelists')

   !-------------------------------------------------     
   !> * Initialize variables on tasks where they are not previously defined.
   ! if fastcheck worked, then don't need to do this extra work to add undefined vars
   !-------------------------------------------------     

   if (.not. fastcheckout) then

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

   endif   ! fastcheckout

   deallocate(vname,pname,inout)

   IF (local_timers_on) call oasis_timer_stop('var_setup')
      
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

