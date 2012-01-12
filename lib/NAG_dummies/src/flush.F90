SUBROUTINE flush(iunit)
#ifdef __NAGf95
  USE f90_unix_io, ONLY: nag_flush => flush

  IMPLICIT NONE

  INTEGER :: iunit

  CALL nag_flush(iunit)
#endif
END SUBROUTINE flush
