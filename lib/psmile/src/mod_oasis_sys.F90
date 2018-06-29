
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
   public oasis_unitsetmax
   public oasis_unitget
   public oasis_unitfree
   public oasis_debug_enter
   public oasis_debug_exit
   public oasis_debug_note
   public oasis_sys_sortC
   public oasis_sys_sortI
   public oasis_sys_sortIkey

   integer(ip_intwp_p),save :: minion = 1024
   integer(ip_intwp_p),save :: maxion = 9999
   integer(ip_intwp_p),parameter :: tree_delta = 2
   integer(ip_intwp_p),save :: tree_indent = 0

!--------------------------------------------------------------------
CONTAINS
!--------------------------------------------------------------------

!--------------------------------------------------------------------

!> OASIS abort method, publically available to users

SUBROUTINE oasis_abort(id_compid, cd_routine, cd_message, file, line, rcode)

   IMPLICIT NONE
!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in),optional :: id_compid  !< component id
   CHARACTER(len=*), INTENT(in),optional :: cd_routine   !< string defining calling routine
   CHARACTER(len=*), INTENT(in),optional :: cd_message   !< error message string
   CHARACTER(len=*), INTENT(in),optional :: file         !< file called from
   INTEGER,INTENT(in),optional           :: line         !< line in file called from
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
   if (present(file))  &
       WRITE (nulprt,*) subname,astr,'file      = ',trim(file)
   if (present(line))  &
       WRITE (nulprt,*) subname,astr,'line      = ',line
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
   INTEGER(kind=ip_intwp_p) :: n1  ! search for unit number 
   logical :: found,l_open
   character(len=*),parameter :: subname = '(oasis_unitget)'
!--------------------------------------------------------------------

   ! start at maxion and decrement the unit numbers
   n1 = maxion+1
   found = .false.
   do while (n1 > minion .and. .not.found)
      n1 = n1 - 1
      inquire(unit=n1,opened=l_open)
      if(.not.l_open) found=.true.
      if (found .and. OASIS_debug >= 2) write(nulprt,*) subname,n1
   enddo

   if (.not.found) then
      write(nulprt,*) subname,estr,'no unit number available '
      write(nulprt,*) subname,estr,'min/max units checked = ',minion,maxion
      call oasis_abort(file=__FILE__,line=__LINE__)
   endif

   uio = n1
     
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

   minion = uio
   if (OASIS_debug >= 20) write(nulprt,*) subname,minion
     
END SUBROUTINE oasis_unitsetmin

!==========================================================================

!> Set the maximum unit number allowed

SUBROUTINE oasis_unitsetmax(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: uio  !< unit number
!--------------------------------------------------------------------
   character(len=*),parameter :: subname = '(oasis_unitsetmax)'
!--------------------------------------------------------------------

   maxion = uio
   if (OASIS_debug >= 20) write(nulprt,*) subname,maxion
     
END SUBROUTINE oasis_unitsetmax

!==========================================================================

!> Release a unit number for reuse

SUBROUTINE oasis_unitfree(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: uio  !< unit number
!--------------------------------------------------------------------
   character(len=*),parameter :: subname = '(oasis_unitfree)'
!--------------------------------------------------------------------

! tcraig, this is a no-op since we are no longer tracking units
! explicitly.  instead, we are searching for free units and using them.
! either a unit number is open or closed and we'll check that explicitly

   if (OASIS_debug >= 20) write(nulprt,*) subname,uio

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

!> Sort a character array and compute a sort key.

! !DESCRIPTION: 
!     Sort a character array and the associated array(s) based on a
!     reasonably fast sort algorithm

! !INTERFACE:  -----------------------------------------------------------------

subroutine oasis_sys_sortC(num, fld, sortkey)

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p
   integer,parameter :: CL = ic_lvar

! !INPUT/OUTPUT PARAMETERS:

   integer(IN),      intent(in)    :: num        !< size of array
   character(len=CL),intent(inout) :: fld(:)     !< sort field
   integer(IN)      ,intent(inout) :: sortkey(:) !< sort key

! !EOP

   !--- local ---
   integer(IN)    :: n1,n2
   character(CL), pointer :: tmpfld(:)
   integer(IN)  , pointer :: tmpkey(:)

   !--- formats ---
   character(*),parameter :: subName = '(oasis_sys_sortC) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   call oasis_debug_enter(subname)

   allocate(tmpfld((num+1)/2))
   allocate(tmpkey((num+1)/2))
   call oasis_sys_mergesortC(num,fld,tmpfld,sortkey,tmpkey)
   deallocate(tmpfld)
   deallocate(tmpkey)
    
!   call oasis_debug_exit(subname)

end subroutine oasis_sys_sortC

!==========================================================================

!> Sort a integer array and compute a sort key.

! !DESCRIPTION: 
!     Sort a character array and the associated array(s) based on a
!     reasonably fast sort algorithm

! !INTERFACE:  -----------------------------------------------------------------

subroutine oasis_sys_sortI(num, fld, sortkey)

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p
   integer,parameter :: CL = ic_lvar

! !INPUT/OUTPUT PARAMETERS:

   integer(IN),intent(in)    :: num        !< size of array
   integer(IN),intent(inout) :: fld(:)     !< sort field
   integer(IN),intent(inout) :: sortkey(:) !< sort key

! !EOP

   !--- local ---
   integer(IN)    :: n1,n2
   integer(IN), pointer :: tmpfld(:)
   integer(IN), pointer :: tmpkey(:)

   !--- formats ---
   character(*),parameter :: subName = '(oasis_sys_sortI) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   call oasis_debug_enter(subname)

   allocate(tmpfld((num+1)/2))
   allocate(tmpkey((num+1)/2))
   call oasis_sys_mergesortI(num,fld,tmpfld,sortkey,tmpkey)
   deallocate(tmpfld)
   deallocate(tmpkey)
    
!   call oasis_debug_exit(subname)

end subroutine oasis_sys_sortI

!------------------------------------------------------------

!> Sort an integer array using a sort key.

! !DESCRIPTION: 
!     Rearrange and integer array based on an input sortkey

! !INTERFACE:  -----------------------------------------------------------------

subroutine oasis_sys_sortIkey(num, arr, sortkey)

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p
   integer,parameter :: CL = ic_lvar

! !INPUT/OUTPUT PARAMETERS:

   integer(IN),intent(in)    :: num        !< size of array
   integer(IN),intent(inout) :: arr(:)     !< field to sort
   integer(IN),intent(in)    :: sortkey(:) !< sort key

! !EOP

   !--- local ---
   integer(IN)    :: n1,n2
   integer(IN), pointer :: tmparr(:)

   !--- formats ---
   character(*),parameter :: subName = '(oasis_sys_sortIkey) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   call oasis_debug_enter(subname)

   if (num /= size(arr) .or. num /= size(sortkey)) then
      WRITE(nulprt,*) subname,estr,'on size of input arrays :',num,size(arr),size(sortkey)
      call oasis_abort(file=__FILE__,line=__LINE__)
   endif

   allocate(tmparr(num))
   tmparr(1:num) = arr(1:num)
   do n1 = 1,num
      arr(n1) = tmparr(sortkey(n1))
   enddo
   deallocate(tmparr)
    
!   call oasis_debug_exit(subname)

end subroutine oasis_sys_sortIkey

!==========================================================================
!==========================================================================

!> Generic oasis_sys_mergesortC routine for character strings
 
recursive subroutine oasis_sys_mergesortC(N,A,T,S,Z)
 
   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p
   integer,parameter :: CL = ic_lvar

   integer                          , intent(in)    :: N   ! size
   character(CL), dimension(N)      , intent(inout) :: A   ! data to sort
   character(CL), dimension((N+1)/2), intent(out)   :: T   ! data tmp
   integer(IN)  , dimension(N)      , intent(inout) :: S   ! sortkey
   integer(IN)  , dimension((N+1)/2), intent(out)   :: Z   ! sortkey tmp
 
   integer :: NA,NB
   character(CL) :: V
   integer(IN) :: Y
   character(*),parameter :: subName = '(oasis_sys_mergesortC) '

!   write(nulprt,*) subname//' N = ',N
 
   if (N < 2) return
   if (N == 2) then
      if (A(1) > A(2)) then
         V = A(1)
         Y = S(1)
         A(1) = A(2)
         S(1) = S(2)
         A(2) = V
         S(2) = Y
      endif
      return
   endif      
   NA=(N+1)/2
   NB=N-NA
 
   call oasis_sys_mergesortC(NA,A,T,S,Z)
   call oasis_sys_mergesortC(NB,A(NA+1),T,S(NA+1),Z)
 
   if (A(NA) > A(NA+1)) then
      T(1:NA)=A(1:NA)
      Z(1:NA)=S(1:NA)
      call oasis_sys_mergeC(T,Z,NA,A(NA+1),S(NA+1),NB,A,S,N)
   endif
   return
 
end subroutine oasis_sys_mergesortC

!==========================================================================

!> Merge routine needed for mergesortC for character strings

subroutine oasis_sys_mergeC(A,X,NA,B,Y,NB,C,Z,NC)
 
   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p
   integer,parameter :: CL = ic_lvar

   integer, intent(in) :: NA,NB,NC         ! Normal usage: NA+NB = NC
   character(CL), intent(inout) :: A(NA)        ! B overlays C(NA+1:NC)
   integer(IN)  , intent(inout) :: X(NA)        ! B overlays C(NA+1:NC)
   character(CL), intent(in)    :: B(NB)
   integer(IN)  , intent(in)    :: Y(NB)
   character(CL), intent(inout) :: C(NC)
   integer(IN)  , intent(inout) :: Z(NC)
 
   integer :: I,J,K
   character(*),parameter :: subName = '(oasis_sys_mergeC) '
 
!   write(nulprt,*) subname//' NA,NB,NC = ',NA,NB,NC

   I = 1; J = 1; K = 1;
   do while(I <= NA .and. J <= NB)
      if (A(I) <= B(J)) then
         C(K) = A(I)
         Z(K) = X(I)
         I = I+1
      else
         C(K) = B(J)
         Z(K) = Y(J)
         J = J+1
      endif
      K = K + 1
   enddo
   do while (I <= NA)
      C(K) = A(I)
      Z(K) = X(I)
      I = I + 1
      K = K + 1
   enddo
   return
 
end subroutine oasis_sys_mergeC

!==========================================================================

!> Generic oasis_sys_mergesortI routine for an integer array
 
recursive subroutine oasis_sys_mergesortI(N,A,T,S,Z)
 
   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p
   integer,parameter :: CL = ic_lvar

   integer                        , intent(in)    :: N   ! size
   integer(IN), dimension(N)      , intent(inout) :: A   ! data to sort
   integer(IN), dimension((N+1)/2), intent(out)   :: T   ! data tmp
   integer(IN), dimension(N)      , intent(inout) :: S   ! sortkey
   integer(IN), dimension((N+1)/2), intent(out)   :: Z   ! sortkey tmp
 
   integer :: NA,NB
   integer(IN) :: V
   integer(IN) :: Y
   character(*),parameter :: subName = '(oasis_sys_mergesortI) '

!   write(nulprt,*) subname//' N = ',N
 
   if (N < 2) return
   if (N == 2) then
      if (A(1) > A(2)) then
         V = A(1)
         Y = S(1)
         A(1) = A(2)
         S(1) = S(2)
         A(2) = V
         S(2) = Y
      endif
      return
   endif      
   NA=(N+1)/2
   NB=N-NA
 
   call oasis_sys_mergesortI(NA,A,T,S,Z)
   call oasis_sys_mergesortI(NB,A(NA+1),T,S(NA+1),Z)
 
   if (A(NA) > A(NA+1)) then
      T(1:NA)=A(1:NA)
      Z(1:NA)=S(1:NA)
      call oasis_sys_mergeI(T,Z,NA,A(NA+1),S(NA+1),NB,A,S,N)
   endif
   return
 
end subroutine oasis_sys_mergesortI

!==========================================================================

!> Merge routine needed for mergesortI for integer array

subroutine oasis_sys_mergeI(A,X,NA,B,Y,NB,C,Z,NC)
 
   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p
   integer,parameter :: CL = ic_lvar

   integer, intent(in) :: NA,NB,NC         ! Normal usage: NA+NB = NC
   integer(IN), intent(inout) :: A(NA)        ! B overlays C(NA+1:NC)
   integer(IN), intent(inout) :: X(NA)        ! B overlays C(NA+1:NC)
   integer(IN), intent(in)    :: B(NB)
   integer(IN), intent(in)    :: Y(NB)
   integer(IN), intent(inout) :: C(NC)
   integer(IN), intent(inout) :: Z(NC)
 
   integer :: I,J,K
   character(*),parameter :: subName = '(oasis_sys_mergeI) '
 
!   write(nulprt,*) subname//' NA,NB,NC = ',NA,NB,NC

   I = 1; J = 1; K = 1;
   do while(I <= NA .and. J <= NB)
      if (A(I) <= B(J)) then
         C(K) = A(I)
         Z(K) = X(I)
         I = I+1
      else
         C(K) = B(J)
         Z(K) = Y(J)
         J = J+1
      endif
      K = K + 1
   enddo
   do while (I <= NA)
      C(K) = A(I)
      Z(K) = X(I)
      I = I + 1
      K = K + 1
   enddo
   return
 
end subroutine oasis_sys_mergeI

!==========================================================================

END MODULE mod_oasis_sys
