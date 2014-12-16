
!> Provides a common location for several OASIS variables

MODULE mod_oasis_data
!     - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
  USE mod_oasis_kinds

  IMPLICIT NONE

  public

! public prism_data_zero

#include <mpif.h>

! GENERAL

  INTEGER(kind=ip_intwp_p)  :: nulprt, nulprt1, nullucia  ! unit numbers for log files
  INTEGER(kind=ip_i4_p)	    :: OASIS_debug
  INTEGER(kind=ip_i4_p)     :: TIMER_debug
  INTEGER(kind=ip_i4_p)     :: LUCIA_debug

  logical                   :: enddef_called   ! true when enddef is called, for error checking

  INTEGER(kind=ip_i4_p)     :: size_namfld
  CHARACTER(len=ic_lvar), POINTER :: total_namsrcfld(:), total_namdstfld(:)

! Models

  ! These are identical on all MPI tasks
  INTEGER(kind=ip_i4_p),parameter :: prism_mmodels = 20
  INTEGER(kind=ip_i4_p)           :: prism_nmodels    ! number of models
  INTEGER(kind=ip_i4_p)           :: prism_amodels    ! number of active models
  character(len=ic_lvar)          :: prism_modnam(prism_mmodels)  ! model names
  logical                         :: prism_modcpl(prism_mmodels)  ! model coupling flags

  ! These are task specific
  character(len=ic_lvar):: compnm         ! name of model on TASK
  integer(kind=ip_i4_p) :: compid         ! integer id associated with model on TASK
  logical               :: oasis_coupled  ! flag whether this TASK is coupled


! MPI

  INTEGER(kind=ip_i4_p) :: mpi_comm_global
  INTEGER(kind=ip_i4_p) :: mpi_rank_global
  INTEGER(kind=ip_i4_p) :: mpi_size_global
  INTEGER(kind=ip_i4_p) :: mpi_comm_local
  INTEGER(kind=ip_i4_p) :: mpi_rank_local
  INTEGER(kind=ip_i4_p) :: mpi_size_local
  INTEGER(kind=ip_i4_p) :: mpi_root_local
  INTEGER(kind=ip_i4_p) :: mpi_err
  INTEGER(kind=ip_i4_p),allocatable :: mpi_root_global(:)  ! for each model, the rank in comm_world 
                                                           ! of the root process

! PARAMETERS

  character(len=*) ,parameter :: cspval = "spval_undef"
  real(ip_double_p),parameter :: rspval = 1.0e36
  integer(ip_i4_p) ,parameter :: ispval = -999999

  real(ip_double_p),parameter :: prism_pi = 3.14159265358979323846
  real(ip_double_p),parameter :: eradius = 6371229.    ! meters

!------------------------------------------------------------
CONTAINS
!------------------------------------------------------------

!< Initialize the module data

  SUBROUTINE oasis_data_zero()

  IMPLICIT NONE

  character(len=*),parameter :: subname = '(oasis_data_zero)'

  nulprt = 6
  nulprt1 = 6
  nullucia = 60
  OASIS_debug = 0
  TIMER_debug = 0
  LUCIA_debug = 0
  compid = -1
  compnm = trim(cspval)
  oasis_coupled = .false.
  mpi_comm_global = -1
  mpi_rank_global = -1
  mpi_size_global = -1
  mpi_comm_local = -1
  mpi_rank_local = -1
  mpi_size_local = -1
  enddef_called = .false.
  
END SUBROUTINE oasis_data_zero

!------------------------------------------------------------
END MODULE mod_oasis_data


