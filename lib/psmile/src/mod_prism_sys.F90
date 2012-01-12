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
   public prism_sys_toUpper
   public prism_sys_toLower

   integer(ip_intwp_p),parameter :: muni = 20
   integer(ip_intwp_p) :: unitno(muni) = -1
   integer(ip_intwp_p) :: maxion

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

!--------------------------------------------------------------------
   SUBROUTINE prism_sys_flush(nu)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: nu
!--------------------------------------------------------------------
   character(len=*),parameter :: subname = 'prism_sys_flush'
!--------------------------------------------------------------------

   call flush(nu)

   END SUBROUTINE prism_sys_flush

!--------------------------------------------------------------------
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
         write(nulprt,*) subname,n1,uio
      endif
   enddo

   if (.not.found) then
      write(nulprt,*) subname,' ERROR no unitno available '
      call prism_sys_abort(compid,subname,'ERROR no unitno available')
   endif
     
   END SUBROUTINE prism_sys_unitget

!--------------------------------------------------------------------
   SUBROUTINE prism_sys_unitsetmin(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: uio
!--------------------------------------------------------------------
   character(len=*),parameter :: subname = 'prism_sys_unitsetmin'
!--------------------------------------------------------------------

   maxion = uio
   write(nulprt,*) subname,maxion
     
   END SUBROUTINE prism_sys_unitsetmin

!--------------------------------------------------------------------
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
         write(nulprt,*) subname,n1,uio
      endif
   enddo

   END SUBROUTINE prism_sys_unitfree

!===============================================================================
!BOP ===========================================================================
! !IROUTINE: prism_sys_toUpper -- Convert string to upper case
!
! !DESCRIPTION:
!     Convert the input string to upper-case.
!     Use achar and iachar intrinsics to ensure use of ascii collating sequence.
!
! !REVISION HISTORY:
!
! !INTERFACE: ------------------------------------------------------------------

function prism_sys_toUpper(str)

   implicit none

! !INPUT/OUTPUT PARAMETERS:
   character(len=*), intent(in) :: str      ! String to convert to upper case
   character(len=len(str))      :: prism_sys_toUpper

   !----- local -----
   integer(ip_intwp_p) :: i             ! Index
   integer(ip_intwp_p) :: aseq          ! ascii collating sequence
   integer(ip_intwp_p) :: LowerToUpper  ! integer to convert case
   character(len=1)     :: ctmp          ! Character temporary

   !----- formats -----
   character(*),parameter :: subName =   "prism_sys_toUpper"

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   LowerToUpper = iachar("A") - iachar("a")

   do i = 1, len(str)
      ctmp = str(i:i)
      aseq = iachar(ctmp)
      if ( aseq >= iachar("a") .and. aseq <= iachar("z") ) &
           ctmp = achar(aseq + LowertoUpper)
      prism_sys_toUpper(i:i) = ctmp
   end do

end function prism_sys_toUpper

!===============================================================================
!BOP ===========================================================================
! !IROUTINE: prism_sys_toLower -- Convert string to lower case
!
! !DESCRIPTION:
!     Convert the input string to lower-case.
!     Use achar and iachar intrinsics to ensure use of ascii collating sequence.
!
! !REVISION HISTORY:
!
! !INTERFACE: ------------------------------------------------------------------

function prism_sys_toLower(str)

   implicit none

! !INPUT/OUTPUT PARAMETERS:
   character(len=*), intent(in) :: str      ! String to convert to lower case
   character(len=len(str))      :: prism_sys_toLower

   !----- local -----
   integer(ip_intwp_p) :: i            ! Index
   integer(ip_intwp_p) :: aseq         ! ascii collating sequence
   integer(ip_intwp_p) :: UpperToLower ! integer to convert case
   character(len=1)     :: ctmp         ! Character temporary

   !----- formats -----
   character(*),parameter :: subName =   "prism_sys_toLower"

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   UpperToLower = iachar("a") - iachar("A")

   do i = 1, len(str)
      ctmp = str(i:i)
      aseq = iachar(ctmp)
      if ( aseq >= iachar("A") .and. aseq <= iachar("Z") ) &
           ctmp = achar(aseq + UpperToLower)
      prism_sys_toLower(i:i) = ctmp
   end do

end function prism_sys_toLower
!-------------------------------------------------------------------------------


END MODULE mod_prism_sys
