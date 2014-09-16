
MODULE mod_flddiag

contains

SUBROUTINE flddiag(field,fmin,fmax,fsum,comm)
  !***************************************************************************

  use mod_oasis_kinds
  use mod_oasis_mpi

  IMPLICIT NONE

  real*8, intent(in)  :: field(:,:)
  real*8, intent(out) :: fmin,fmax,fsum
  integer, intent(in) :: comm

  real*8 :: lvali
  real*8 :: lvalo

  lvali = 9.99e36

  if (size(field) > 0) &
     lvali = minval(field)
  call oasis_mpi_min(lvali,lvalo,comm, all=.true.)
  fmin = lvalo

  lvali = -9.99e36

  if (size(field) > 0) &
     lvali = maxval(field)
  call oasis_mpi_max(lvali,lvalo,comm, all=.true.)
  fmax = lvalo

  lvali = 0.0

  if (size(field) > 0) &
     lvali = sum(field)
  call oasis_mpi_sum(lvali,lvalo,comm, all=.true.)
  fsum = lvalo

END SUBROUTINE flddiag

END MODULE mod_flddiag
