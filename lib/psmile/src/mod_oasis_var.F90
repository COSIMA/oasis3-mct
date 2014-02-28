  MODULE mod_oasis_var

  USE mod_oasis_kinds
  USE mod_oasis_data
  USE mod_oasis_parameters
  USE mod_oasis_sys

  IMPLICIT none

  private

  !--- interfaces ---
  public oasis_def_var

  !--- datatypes ---

  integer(kind=ip_intwp_p),public :: prism_nvar = 0

  CONTAINS

!---------------------------------------------------------------

  SUBROUTINE oasis_def_var(id_nports, cdport, id_part, &
         id_var_nodims, kinout, id_var_shape, ktype, kinfo)
     !---------------------------------------------------------------
     INTEGER(kind=ip_i4_p) :: kinout, ktype, id_nports,id_part
     INTEGER(kind=ip_i4_p) :: id_var_nodims(2),id_var_shape(2*id_var_nodims(1))
     CHARACTER(len=*)         :: cdport
     INTEGER(kind=ip_i4_p),optional :: kinfo
     !---------------------------------------------------------------
     INTEGER(kind=ip_i4_p)  :: n
     CHARACTER(len=ic_lvar) :: trimmed_cdport   ! Trimmed version of cdport
     character(len=*),parameter :: subname = '(oasis_def_var)'
     LOGICAL    :: l_field_in_namcouple
     !---------------------------------------------------------------

     call oasis_debug_enter(subname)

     ! Check len of incoming variable name and
     ! Trim incoming name once to avoid multiple trim operations
     ! in subsequent loops
     if (len_trim(cdport) > ic_lvar) then
        WRITE(nulprt,*) subname,' Abort variable '//trim(cdport)//' too long. max len (ic_lvar) = ',ic_lvar
        call oasis_abort()
     endif
     trimmed_cdport = trim(cdport)

     kinfo = OASIS_Ok

     l_field_in_namcouple = .FALSE.
     n = 0
     
     ! If either condition ceases to be true then bail out of the loop
     DO WHILE (n < mvar .AND. (.NOT.l_field_in_namcouple))
        n = n+1
        IF ( (trimmed_cdport == total_namsrcfld(n)).OR.    &
             (trimmed_cdport == total_namdstfld(n)) ) THEN 
              l_field_in_namcouple = .TRUE.
        ENDIF       
     enddo

     if (.not. l_field_in_namcouple) then
        id_nports = OASIS_Var_Uncpl
        if (OASIS_debug >= 2) then
           write(nulprt,*) subname,' variable not in namcouple return ',trimmed_cdport
           call oasis_flush(nulprt)
        endif
        call oasis_debug_exit(subname)
        return
     endif

! --- This is checked in mod_oasis_coupler:oasis_coupler_setup
!     do n = 1,prism_nvar
!        if (trimmed_cdport == trim(prism_var(n)%name)) then
!           write(nulprt,*) subname,' variable already defined with var_def ',trimmed_cdport
!           WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
!           call oasis_abort()
!        endif
!     enddo

     prism_nvar = prism_nvar + 1
     id_nports = prism_nvar

     if (prism_nvar > mvar) then
        write(nulprt,*) subname,' ERROR prism_nvar too large ',prism_nvar,mvar
        WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
        call oasis_abort()
     endif

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
    !--- some diagnostics
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

 END MODULE mod_oasis_var

