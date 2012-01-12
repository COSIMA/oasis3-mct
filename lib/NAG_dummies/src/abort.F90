SUBROUTINE abort
#ifdef __NAGf95
  USE f90_unix_proc, ONLY: nag_abort => abort

  IMPLICIT NONE

  CALL nag_abort()
#endif
END SUBROUTINE abort
