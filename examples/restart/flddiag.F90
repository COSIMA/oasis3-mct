SUBROUTINE flddiag(field,fmin,fmax,fsum,comm,nx,ny)
  !***************************************************************************

  use mod_prism_kinds
  use mod_prism_mpi

  IMPLICIT NONE

#ifdef NO_USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(6,37)   ! real
#else
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307) ! double
#endif

  integer,  intent(in)  :: nx,ny
  real(wp), intent(in)  :: field(nx,ny)
  real(wp), intent(out) :: fmin,fmax,fsum
  integer,  intent(in)  :: comm

  real(ip_double_p) :: lvali
  real(ip_double_p) :: lvalo

  lvali = minval(field)
  call prism_mpi_min(lvali,lvalo,comm)
  fmin = lvalo

  lvali = maxval(field)
  call prism_mpi_max(lvali,lvalo,comm)
  fmax = lvalo

  lvali = sum(field)
  call prism_mpi_sum(lvali,lvalo,comm)
  fsum = lvalo

END SUBROUTINE flddiag

