MODULE mod_prism_sys

   USE mod_prism_kinds
   USE mod_prism_data

   IMPLICIT NONE

   private

   public prism_sys_abort
   public prism_sys_flush
   public prism_sys_unitsetmin
   public prism_sys_unitget
   public prism_sys_unitfree
   public prism_sys_debug_enter
   public prism_sys_debug_exit
   public prism_sys_debug_note

   integer(ip_intwp_p),parameter :: muni = 20
   integer(ip_intwp_p),save :: unitno(muni) = -1
   integer(ip_intwp_p),save :: maxion
   integer(ip_intwp_p),parameter :: tree_delta = 2
   integer(ip_intwp_p),save :: tree_indent = 0

!--------------------------------------------------------------------
CONTAINS
!--------------------------------------------------------------------

   SUBROUTINE prism_sys_abort()

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER                      :: ierror
   character(len=*),parameter :: subname = 'prism_sys_abort'
!--------------------------------------------------------------------

#if defined use_comm_MPI1 || defined use_comm_MPI2
   CALL MPI_ABORT (mpi_comm_global, 0, ierror)
#endif

   STOP

   END SUBROUTINE prism_sys_abort

!==========================================================================
   SUBROUTINE prism_sys_flush(nu)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: nu
!--------------------------------------------------------------------
   character(len=*),parameter :: subname = 'prism_sys_flush'
!--------------------------------------------------------------------

   call flush(nu)

   END SUBROUTINE prism_sys_flush

!==========================================================================
   SUBROUTINE prism_sys_unitget(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(out) :: uio
!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p) :: n1
   logical :: found
   character(len=*),parameter :: subname = 'prism_sys_unitget'
!--------------------------------------------------------------------

   n1 = 0
   found = .false.
   do while (n1 < muni .and. .not.found)
      n1 = n1 + 1
      if (unitno(n1) < 0) then
         found = .true.
         uio = n1 + maxion
         unitno(n1) = uio
         if (PRISM_DEBUG >= 2) write(nulprt,*) subname,n1,uio
      endif
   enddo

   if (.not.found) then
      write(nulprt,*) subname,' ERROR no unitno available '
      WRITE(nulprt,*) subname,' abort by model ',compid,' proc :',mpi_rank_local
      call prism_sys_abort()
   endif
     
   END SUBROUTINE prism_sys_unitget

!==========================================================================
   SUBROUTINE prism_sys_unitsetmin(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: uio
!--------------------------------------------------------------------
   character(len=*),parameter :: subname = 'prism_sys_unitsetmin'
!--------------------------------------------------------------------

   maxion = uio
   if (PRISM_DEBUG >= 20) write(nulprt,*) subname,maxion
     
   END SUBROUTINE prism_sys_unitsetmin

!==========================================================================
   SUBROUTINE prism_sys_unitfree(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: uio
!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p) :: n1
   character(len=*),parameter :: subname = 'prism_sys_unitfree'
!--------------------------------------------------------------------

   do n1 = 1,muni
      if (unitno(n1) == uio) then
         unitno(n1) = -1
         if (PRISM_DEBUG >= 20) write(nulprt,*) subname,n1,uio
      endif
   enddo

   END SUBROUTINE prism_sys_unitfree

!=========================================================================
!==========================================================================
subroutine prism_sys_debug_enter(string)

   IMPLICIT NONE

!--------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: string
   character(len=*),parameter :: subname = 'prism_sys_debug_enter'
   CHARACTER(len=1), pointer :: ch_blank(:)
   CHARACTER(len=500) :: tree_enter

   if (PRISM_DEBUG >= 10) then
       ALLOCATE (ch_blank(tree_indent))
       ch_blank='-'
       tree_enter='**** ENTER '//TRIM(string)
       WRITE(nulprt,*) ch_blank,TRIM(tree_enter)
       tree_indent = tree_indent + tree_delta
       DEALLOCATE (ch_blank)
       CALL prism_sys_flush(nulprt)
   endif

end subroutine prism_sys_debug_enter

!==========================================================================
subroutine prism_sys_debug_exit(string)

   IMPLICIT NONE

!--------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: string
   character(len=*),parameter :: subname = 'prism_sys_debug_exit'
   CHARACTER(len=1), pointer :: ch_blank(:)
   CHARACTER(len=500)        :: tree_exit

   IF (PRISM_DEBUG >= 10) THEN
       tree_indent = MAX(0,tree_indent - tree_delta)
       ALLOCATE (ch_blank(tree_indent))
       ch_blank='-'
       tree_exit='**** EXIT  '//TRIM(string)
       WRITE(nulprt,*) ch_blank,TRIM(tree_exit)
       DEALLOCATE (ch_blank)
       CALL prism_sys_flush(nulprt)
   ENDIF

end subroutine prism_sys_debug_exit

!==========================================================================
subroutine prism_sys_debug_note(string)

   IMPLICIT NONE

!--------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: string
   character(len=*),parameter :: subname = 'prism_sys_debug_note'
   CHARACTER(len=1), pointer :: ch_blank(:)
   CHARACTER(len=500) :: tree_note

   if (PRISM_DEBUG >= 12) then
       ALLOCATE (ch_blank(tree_indent))
       ch_blank='-'
       tree_note='**** NOTE '//TRIM(string)
       WRITE(nulprt,*) ch_blank,TRIM(tree_note)
      DEALLOCATE(ch_blank)
      call prism_sys_flush(nulprt)
   endif

end subroutine prism_sys_debug_note

!==========================================================================

END MODULE mod_prism_sys
