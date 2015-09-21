
!> System type methods

MODULE mod_oasis_sys

   USE mod_oasis_kinds
   USE mod_oasis_data

   IMPLICIT NONE

   character(len=*),parameter,public :: astr = ' ABORT: '   ! abort string
   character(len=*),parameter,public :: estr = ' ERROR: '   ! error string
   character(len=*),parameter,public :: wstr = ' WARNING: ' ! warning string

   private

   public oasis_abort
   public oasis_flush
   public oasis_unitsetmin
   public oasis_unitget
   public oasis_unitfree
   public oasis_debug_enter
   public oasis_debug_exit
   public oasis_debug_note

   integer(ip_intwp_p),parameter :: muni = 20
   integer(ip_intwp_p),save :: unitno(muni) = -1
   integer(ip_intwp_p),save :: maxion
   integer(ip_intwp_p),parameter :: tree_delta = 2
   integer(ip_intwp_p),save :: tree_indent = 0

!--------------------------------------------------------------------
CONTAINS
!--------------------------------------------------------------------

!--------------------------------------------------------------------

!> OASIS abort method, publically available to users

   SUBROUTINE oasis_abort(id_compid, cd_routine, cd_message, rcode)

   IMPLICIT NONE
!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in),optional :: id_compid  !< component id
   CHARACTER(len=*), INTENT(in),optional :: cd_routine   !< string defining calling routine
   CHARACTER(len=*), INTENT(in),optional :: cd_message   !< error message string
   INTEGER,INTENT(in),optional           :: rcode        !< optional code to return to invoking environment 
!--------------------------------------------------------------------
   INTEGER                      :: ierror, errcode
   character(len=*),parameter   :: subname = '(oasis_abort)'
!--------------------------------------------------------------------

   if (present(id_compid)) &
   WRITE (nulprt,*) subname,astr,'compid    = ',id_compid
   if (present(cd_routine)) &
   WRITE (nulprt,*) subname,astr,'called by = ',trim(cd_routine)
   if (present(cd_message))  &
   WRITE (nulprt,*) subname,astr,'message   = ',trim(cd_message)
   IF (PRESENT(rcode))  THEN
       errcode=rcode
       WRITE (nulprt,*) subname,astr,'errcode   = ',errcode
   ELSE
       errcode=1
   ENDIF

   WRITE (nulprt,*) subname,astr,'on model  = ',trim(compnm)
   WRITE (nulprt,*) subname,astr,'on global rank = ',mpi_rank_global
   WRITE (nulprt,*) subname,astr,'on local  rank = ',mpi_rank_local
   WRITE (nulprt,*) subname,astr,'CALLING ABORT FROM OASIS LAYER NOW'
   CALL oasis_flush(nulprt)

#if defined use_comm_MPI1 || defined use_comm_MPI2
   CALL MPI_ABORT (mpi_comm_global, errcode, ierror)
#endif

   STOP

 END SUBROUTINE oasis_abort

!==========================================================================

!> Flushes output to file

   SUBROUTINE oasis_flush(nu)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: nu  !< unit number of file
!--------------------------------------------------------------------
   character(len=*),parameter :: subname = '(oasis_flush)'
!--------------------------------------------------------------------

   CALL FLUSH(nu)

 END SUBROUTINE oasis_flush

!==========================================================================

!> Get a free unit number

   SUBROUTINE oasis_unitget(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(out) :: uio  !< unit number
!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p) :: n1
   logical :: found
   character(len=*),parameter :: subname = '(oasis_unitget)'
!--------------------------------------------------------------------

   n1 = 0
   found = .false.
   do while (n1 < muni .and. .not.found)
      n1 = n1 + 1
      if (unitno(n1) < 0) then
         found = .true.
         uio = n1 + maxion
         unitno(n1) = uio
         if (OASIS_debug >= 2) write(nulprt,*) subname,n1,uio
      endif
   enddo

   if (.not.found) then
      write(nulprt,*) subname,estr,'no unit number available '
      call oasis_abort()
   endif
     
 END SUBROUTINE oasis_unitget

!==========================================================================

!> Set the minimum unit number allowed

   SUBROUTINE oasis_unitsetmin(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: uio  !< unit number
!--------------------------------------------------------------------
   character(len=*),parameter :: subname = '(oasis_unitsetmin)'
!--------------------------------------------------------------------

   maxion = uio
   if (OASIS_debug >= 20) write(nulprt,*) subname,maxion
     
 END SUBROUTINE oasis_unitsetmin

!==========================================================================

!> Release a unit number for reuse

   SUBROUTINE oasis_unitfree(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: uio  !< unit number
!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p) :: n1
   character(len=*),parameter :: subname = '(oasis_unitfree)'
!--------------------------------------------------------------------

   do n1 = 1,muni
      if (unitno(n1) == uio) then
         unitno(n1) = -1
         if (OASIS_debug >= 20) write(nulprt,*) subname,n1,uio
      endif
   enddo

 END SUBROUTINE oasis_unitfree

!=========================================================================
!==========================================================================

!> Used when a subroutine is entered, write info to log file at some debug level

SUBROUTINE oasis_debug_enter(string)

   IMPLICIT NONE

!--------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: string !< name of the subroutine

   character(len=*),parameter :: subname = '(oasis_debug_enter)'
   CHARACTER(len=1), pointer :: ch_blank(:)
   CHARACTER(len=500) :: tree_enter

   if (OASIS_debug >= 10) then
       ALLOCATE (ch_blank(tree_indent))
       ch_blank='-'
       tree_enter='-- ENTER '//TRIM(string)
       WRITE(nulprt,*) ch_blank,TRIM(tree_enter)
       tree_indent = tree_indent + tree_delta
       DEALLOCATE (ch_blank)
       CALL oasis_flush(nulprt)
   endif

 END SUBROUTINE oasis_debug_enter

!==========================================================================

!> Used when a subroutine is exited, write info to log file at some debug level

SUBROUTINE oasis_debug_exit(string)

   IMPLICIT NONE

!--------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: string  !< name of subroutine

   character(len=*),parameter :: subname = '(oasis_debug_exit)'
   CHARACTER(len=1), pointer :: ch_blank(:)
   CHARACTER(len=500)        :: tree_exit

   IF (OASIS_debug >= 10) THEN
       tree_indent = MAX(0,tree_indent - tree_delta)
       ALLOCATE (ch_blank(tree_indent))
       ch_blank='-'
       tree_exit='-- EXIT  '//TRIM(string)
       WRITE(nulprt,*) ch_blank,TRIM(tree_exit)
       DEALLOCATE (ch_blank)
       CALL oasis_flush(nulprt)
   ENDIF

 END SUBROUTINE oasis_debug_exit

!==========================================================================

!> Used to write information from a subroutine, write info to log file at some debug level

SUBROUTINE oasis_debug_note(string)

   IMPLICIT NONE

!--------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: string  !< string to write

   character(len=*),parameter :: subname = '(oasis_debug_note)'
   CHARACTER(len=1), pointer :: ch_blank(:)
   CHARACTER(len=500) :: tree_note

   if (OASIS_debug >= 12) then
       ALLOCATE (ch_blank(tree_indent))
       ch_blank='-'
       tree_note='-- NOTE '//TRIM(string)
       WRITE(nulprt,*) ch_blank,TRIM(tree_note)
      DEALLOCATE(ch_blank)
      call oasis_flush(nulprt)
   endif

 END SUBROUTINE oasis_debug_note

!==========================================================================

END MODULE mod_oasis_sys
