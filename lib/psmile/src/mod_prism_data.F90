MODULE mod_prism_data
!     - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
  USE mod_prism_kinds

  IMPLICIT NONE

  public

! public prism_data_zero

#include <mpif.h>

! GENERAL

  integer(kind=ip_i4_p) :: compid
  character(len=ic_lvar):: compnm

! MPI

  INTEGER(kind=ip_i4_p) :: mpi_comm_global
  INTEGER(kind=ip_i4_p) :: mpi_rank_global
  INTEGER(kind=ip_i4_p) :: mpi_size_global
  INTEGER(kind=ip_i4_p) :: mpi_comm_local
  INTEGER(kind=ip_i4_p) :: mpi_rank_local
  INTEGER(kind=ip_i4_p) :: mpi_size_local
  INTEGER(kind=ip_i4_p) :: mpi_root_local
  INTEGER(kind=ip_i4_p) :: mpi_err

  character(len=*) ,parameter :: cspval = "undef"
  real(ip_double_p),parameter :: rspval = 1.0e36
  integer(ip_i4_p) ,parameter :: ispval = -999999

  real(ip_double_p),parameter :: prism_pi = 3.14159265358979323846
  real(ip_double_p),parameter :: eradius = 6371229.    ! meters

!------------------------------------------------------------
CONTAINS
!------------------------------------------------------------

  SUBROUTINE prism_data_zero()

  IMPLICIT NONE

  character(len=*),parameter :: subname = 'prism_data_zero'

  nulprt = 6
  compid = -1
  compnm = trim(cspval)
  mpi_comm_global = -1
  mpi_rank_global = -1
  mpi_size_global = -1
  mpi_comm_local = -1
  mpi_rank_local = -1
  mpi_size_local = -1
  
  END SUBROUTINE prism_data_zero

!------------------------------------------------------------
END MODULE mod_prism_data


