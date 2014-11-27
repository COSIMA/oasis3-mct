!*********************************************************************************
SUBROUTINE hdlerr(istatus, line)
  !*********************************************************************************
  use netcdf
  implicit none
  !
  ! Check for error message from NetCDF call
  !
  integer, intent(in) :: istatus, line
  integer             :: ierror
  !
  IF (istatus .NE. NF90_NOERR) THEN
      write ( * , * ) 'NetCDF problem at line',line
      write ( * , * ) 'Stopped '
      STOP
  ENDIF
  !
  RETURN
END SUBROUTINE hdlerr
