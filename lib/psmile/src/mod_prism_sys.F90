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

   SUBROUTINE prism_sys_abort(id_compid, cd_routine, cd_message)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: id_compid
   CHARACTER(len=*), INTENT(in) :: cd_routine
   CHARACTER(len=*), INTENT(in) :: cd_message
!--------------------------------------------------------------------
   INTEGER                      :: ierror
   character(len=*),parameter :: subname = 'prism_sys_abort'
!--------------------------------------------------------------------

   IF (id_compid .ne. 0) THEN
      WRITE (nulprt,'(a,i4)') subname//' from '// &
         trim(cd_routine)//' by model ',id_compid
   ELSE
      WRITE (nulprt,'(a)') subname//' from '//trim(cd_routine)
   ENDIF
   WRITE (nulprt,'(a)') subname//' error = '//trim(cd_message)
   call prism_sys_flush(nulprt)

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
      call prism_sys_abort(compid,subname,'ERROR no unitno available')
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

   if (PRISM_DEBUG >= 10) then
      write(nulprt,'(<tree_indent>x,2a)') '**TREE ENTER ',trim(string)
      tree_indent = tree_indent + tree_delta
      call prism_sys_flush(nulprt)
   endif

end subroutine prism_sys_debug_enter

!==========================================================================
subroutine prism_sys_debug_exit(string)

   IMPLICIT NONE

!--------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: string
   character(len=*),parameter :: subname = 'prism_sys_debug_exit'

   if (PRISM_DEBUG >= 10) then
      tree_indent = max(0,tree_indent - tree_delta)
      write(nulprt,'(<tree_indent>x,2a)') '**TREE EXIT  ',trim(string)
      call prism_sys_flush(nulprt)
   endif

end subroutine prism_sys_debug_exit

!==========================================================================
subroutine prism_sys_debug_note(string)

   IMPLICIT NONE

!--------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: string
   character(len=*),parameter :: subname = 'prism_sys_debug_note'

   if (PRISM_DEBUG >= 12) then
      write(nulprt,'(<tree_indent>x,2a)') '**TREE NOTE  ',trim(string)
      call prism_sys_flush(nulprt)
   endif

end subroutine prism_sys_debug_note

!==========================================================================

END MODULE mod_prism_sys
