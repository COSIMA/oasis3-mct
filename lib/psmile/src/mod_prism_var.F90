  MODULE mod_prism_var

  USE mod_prism_kinds
  USE mod_prism_data
  USE mod_oasis_print
  USE mod_prism_parameters
  USE mod_prism_sys

  IMPLICIT none

  private

  !--- interfaces ---
  public prism_var_def

  !--- datatypes ---
  public :: prism_var_type

  integer(kind=ip_i4_p),parameter :: mvar = 100
  integer(kind=ip_i4_p),parameter :: mvarcpl = 10

  type prism_var_type
     character(len=ic_lvar):: name
     integer(kind=ip_i4_p) :: part
     integer(kind=ip_i4_p) :: ndim
     integer(kind=ip_i4_p) :: num
     integer(kind=ip_i4_p) :: ops
     integer(kind=ip_i4_p) :: type
     integer(kind=ip_i4_p) :: size
     integer(kind=ip_i4_p) :: ncpl
     integer(kind=ip_i4_p) :: cpl(mvarcpl)
  end type prism_var_type

  integer(kind=ip_intwp_p),public :: prism_nvar = 0
  type(prism_var_type),public :: prism_var(mvar)

  CONTAINS

!---------------------------------------------------------------

  SUBROUTINE prism_var_def(id_nports, cdport, id_part, &
         id_var_nodims, kinout, id_var_shape, ktype, kinfo)
!    ---------------------------------------------------------------
     INTEGER(kind=ip_i4_p) :: kinout, ktype, id_nports,id_part
     INTEGER(kind=ip_i4_p) :: id_var_nodims(2),id_var_shape(2*id_var_nodims(1))
     CHARACTER(len=*)         :: cdport
     INTEGER(kind=ip_i4_p),optional :: kinfo
!    ---------------------------------------------------------------
     integer(kind=ip_i4_p) :: n
     character(len=*),parameter :: subname = 'prism_var_def'
!    ---------------------------------------------------------------

     call prism_sys_debug_enter(subname)

     kinfo = PRISM_Ok

     do n = 1,prism_nvar
        if (trim(cdport) == trim(prism_var(n)%name)) then
           CALL oasis_pprintc(subname,2,'  variable already defined with var_def ',char1=trim(cdport))
           CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
           CALL oasis_pprintc(subname,2,' error :',char1=' variable already defined')
           call prism_sys_abort()
        endif
     enddo

     prism_nvar = prism_nvar + 1
     id_nports = prism_nvar

     if (prism_nvar > mvar) then
        CALL oasis_pprinti(subname,2,' ERROR prism_nvar too large ',int1=prism_nvar,int2=mvar)
        CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
        CALL oasis_pprintc(subname,2,' error :',char1=' prism_nvar too large')
        call prism_sys_abort()
     endif

     prism_var(prism_nvar)%name = trim(cdport)
     prism_var(prism_nvar)%part = id_part
     prism_var(prism_nvar)%ndim = id_var_nodims(1)
     prism_var(prism_nvar)%num  = id_var_nodims(2)
     prism_var(prism_nvar)%ops  = kinout
     prism_var(prism_nvar)%type = ktype
     prism_var(prism_nvar)%size = 1
     do n = 1,prism_var(prism_nvar)%ndim
        prism_var(prism_nvar)%size = prism_var(prism_nvar)%size*(id_var_shape(2*n)-id_var_shape(2*n-1)+1)
     enddo
     prism_var(prism_nvar)%ncpl = 0
     prism_var(prism_nvar)%cpl  = 0

    !----------------------------------
    !--- some diagnostics
    !----------------------------------

     CALL oasis_pprintc(subname,2,' : ',char1=' Begin infos ')
     CALL oasis_pprinti(subname,2,' prism_nvar             = ',int1=prism_nvar)
     CALL oasis_pprintc(subname,2,' varname                = ',char1=trim(prism_var(prism_nvar)%name))
     CALL oasis_pprinti(subname,2,' prism_nvar, varpart    = ',int1=prism_nvar,int2=prism_var(prism_nvar)%part)
     CALL oasis_pprinti(subname,2,' prism_nvar, varndim    = ',int1=prism_nvar,int2=prism_var(prism_nvar)%ndim)
     CALL oasis_pprinti(subname,2,' prism_nvar, varnum     = ',int1=prism_nvar,int2=prism_var(prism_nvar)%num)
     CALL oasis_pprinti(subname,2,' prism_nvar, varops     = ',int1=prism_nvar,int2=prism_var(prism_nvar)%ops)
     CALL oasis_pprinti(subname,2,' prism_nvar, vartype    = ',int1=prism_nvar,int2=prism_var(prism_nvar)%type)
     CALL oasis_pprinti(subname,2,' prism_nvar, varsize    = ',int1=prism_nvar,int2=prism_var(prism_nvar)%size)
     CALL oasis_pprintc(subname,2,' : ',char1=' End infos ')

     call prism_sys_debug_exit(subname)

  END SUBROUTINE prism_var_def

END MODULE mod_prism_var

