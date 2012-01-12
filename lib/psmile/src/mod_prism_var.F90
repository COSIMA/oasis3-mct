  MODULE mod_prism_var

  USE mod_prism_kinds
  USE mod_prism_data
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

     kinfo = PRISM_Ok

     do n = 1,prism_nvar
        if (trim(cdport) == trim(prism_var(n)%name)) then
           write(nulprt,*) subname,' variable already defined with var_def ',trim(cdport)
           call prism_sys_abort(compid,subname,' ERROR variable already defined')
        endif
     enddo

     prism_nvar = prism_nvar + 1
     id_nports = prism_nvar

     if (prism_nvar > mvar) then
        write(nulprt,*) subname,' ERROR prism_nvar too large ',prism_nvar,mvar
        call prism_sys_abort(compid,subname,'ERROR prism_nvar too large')
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

     RETURN

  END SUBROUTINE prism_var_def

END MODULE mod_prism_var

