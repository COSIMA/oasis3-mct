!> Provides a generic and simpler interface into MPI calls for OASIS.

MODULE mod_oasis_mpi

!-------------------------------------------------------------------------------
! PURPOSE: general layer on MPI functions
!-------------------------------------------------------------------------------

   use mod_oasis_kinds
   USE mod_oasis_data ,ONLY: nulprt, OASIS_Debug
   USE mod_oasis_sys  ,ONLY: oasis_debug_enter, oasis_debug_exit, oasis_flush, oasis_abort, astr
   USE mod_oasis_timer,ONLY: oasis_timer_start, oasis_timer_stop

   implicit none
   private

! PUBLIC: Public interfaces

   public :: oasis_mpi_chkerr
   public :: oasis_mpi_send
   public :: oasis_mpi_recv
   public :: oasis_mpi_bcast
   public :: oasis_mpi_gathScatVInit
   public :: oasis_mpi_gatherV
   public :: oasis_mpi_scatterV
   public :: oasis_mpi_sum
   public :: oasis_mpi_min
   public :: oasis_mpi_max
   public :: oasis_mpi_commsize
   public :: oasis_mpi_commrank
   public :: oasis_mpi_initialized
   public :: oasis_mpi_wtime
   public :: oasis_mpi_abort
   public :: oasis_mpi_barrier
   public :: oasis_mpi_init
   public :: oasis_mpi_finalize
   public :: oasis_mpi_reducelists

   !> Generic overloaded interface into MPI send
   interface oasis_mpi_send ; module procedure &
     oasis_mpi_sendi0, &
     oasis_mpi_sendi1, &
     oasis_mpi_sendr0, &
     oasis_mpi_sendr1, &
     oasis_mpi_sendr3
   end interface

   !> Generic overloaded interface into MPI receive
   interface oasis_mpi_recv ; module procedure &
     oasis_mpi_recvi0, &
     oasis_mpi_recvi1, &
     oasis_mpi_recvr0, &
     oasis_mpi_recvr1, &
     oasis_mpi_recvr3
   end interface

   !> Generic overloaded interface into MPI broadcast
   interface oasis_mpi_bcast ; module procedure &
     oasis_mpi_bcastc0, &
     oasis_mpi_bcastc1, &
     oasis_mpi_bcastl0, &
     oasis_mpi_bcastl1, &
     oasis_mpi_bcasti0, &
     oasis_mpi_bcasti1, &
     oasis_mpi_bcasti2, &
     oasis_mpi_bcastr0, &
     oasis_mpi_bcastr1, &
     oasis_mpi_bcastr2, &
     oasis_mpi_bcastr3
   end interface

   !> Generic interface to oasis_mpi_gathScatVInit
   interface oasis_mpi_gathScatVInit ; module procedure &
     oasis_mpi_gathScatVInitr1
   end interface

   !> Generic interfaces into an MPI vector gather
   interface oasis_mpi_gatherv ; module procedure &
     oasis_mpi_gatherVr1
   end interface

   !> Generic interfaces into an MPI vector scatter
   interface oasis_mpi_scatterv ; module procedure &
     oasis_mpi_scatterVr1
   end interface

   !> Generic overloaded interface into MPI sum reduction
   interface oasis_mpi_sum ; module procedure &
     oasis_mpi_sumi0, &
     oasis_mpi_sumi1, &
     oasis_mpi_sumb0, &
     oasis_mpi_sumb1, &
     oasis_mpi_sumr0, &
     oasis_mpi_sumr1, &
     oasis_mpi_sumr2, &
     oasis_mpi_sumr3
   end interface

   !> Generic overloaded interface into MPI min reduction
   interface oasis_mpi_min ; module procedure &
     oasis_mpi_mini0, &
     oasis_mpi_mini1, &
     oasis_mpi_minr0, &
     oasis_mpi_minr1
   end interface

   !> Generic overloaded interface into MPI max reduction
   interface oasis_mpi_max ; module procedure &
     oasis_mpi_maxi0, &
     oasis_mpi_maxi1, &
     oasis_mpi_maxr0, &
     oasis_mpi_maxr1
   end interface

! mpi library include file
#include <mpif.h>         

!===============================================================================
CONTAINS
!===============================================================================

!> Checks MPI error codes and aborts

!> This method compares rcode to MPI_SUCCESS.  If rcode is an error,
!> it queries MPI_ERROR_STRING for the error string associated with rcode, writes
!> it out, and aborts with the string passed through the interface.

SUBROUTINE oasis_mpi_chkerr(rcode,string)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: rcode   !< MPI error code 
   character(*),     intent(in) :: string  !< abort message

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_chkerr)'
   character(MPI_MAX_ERROR_STRING)  :: lstring
   integer(ip_i4_p)             :: len
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: layer on MPI error checking
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lstring = ' '
   if (rcode /= MPI_SUCCESS) then
     call MPI_ERROR_STRING(rcode,lstring,len,ierr)
     call oasis_mpi_abort(subname//trim(string)//':'//trim(lstring),rcode)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_chkerr

!===============================================================================
!===============================================================================

!> Send a scalar integer

SUBROUTINE oasis_mpi_sendi0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec     !< send value
   integer(ip_i4_p), intent(in) :: pid      !< pid to send to
   integer(ip_i4_p), intent(in) :: tag      !< tag
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sendi0)'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a single integer
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1

   call MPI_SEND(lvec,lsize,MPI_INTEGER,pid,tag,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sendi0

!===============================================================================
!===============================================================================

!> Send an array of 1D integers

SUBROUTINE oasis_mpi_sendi1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec(:)  !< send values
   integer(ip_i4_p), intent(in) :: pid      !< pid to send to
   integer(ip_i4_p), intent(in) :: tag      !< tag
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sendi1)'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a vector of integers
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(lvec)

   call MPI_SEND(lvec,lsize,MPI_INTEGER,pid,tag,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sendi1

!===============================================================================
!===============================================================================

!> Send a scalar double

SUBROUTINE oasis_mpi_sendr0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),intent(in) :: lvec     !< send values
   integer(ip_i4_p), intent(in) :: pid      !< pid to send to
   integer(ip_i4_p), intent(in) :: tag      !< tag
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sendr0)'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a real scalar
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1

   call MPI_SEND(lvec,lsize,MPI_REAL8,pid,tag,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sendr0

!===============================================================================
!===============================================================================

!> Send an array of 1D doubles

SUBROUTINE oasis_mpi_sendr1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),intent(in) :: lvec(:)  !< send values
   integer(ip_i4_p), intent(in) :: pid      !< pid to send to
   integer(ip_i4_p), intent(in) :: tag      !< tag
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sendr1)'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(lvec)

   call MPI_SEND(lvec,lsize,MPI_REAL8,pid,tag,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sendr1

!===============================================================================
!===============================================================================

!> Send an array of 3D doubles

SUBROUTINE oasis_mpi_sendr3(array,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),intent(in) :: array(:,:,:)  !< send values
   integer(ip_i4_p), intent(in) :: pid           !< pid to send to
   integer(ip_i4_p), intent(in) :: tag           !< tag
   integer(ip_i4_p), intent(in) :: comm          !< mpi communicator
   character(*),optional,intent(in) :: string        !< to identify caller

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sendr3)'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(array)

   call MPI_SEND(array,lsize,MPI_REAL8,pid,tag,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sendr3

!===============================================================================
!===============================================================================

!> Receive a scalar integer

SUBROUTINE oasis_mpi_recvi0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(out):: lvec     !< receive values
   integer(ip_i4_p), intent(in) :: pid      !< pid to recv from
   integer(ip_i4_p), intent(in) :: tag      !< tag
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_recvi0)'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: status(MPI_STATUS_SIZE)  ! mpi status info
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1

   call MPI_RECV(lvec,lsize,MPI_INTEGER,pid,tag,comm,status,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_recvi0

!===============================================================================
!===============================================================================

!> Receive an array of 1D integers

SUBROUTINE oasis_mpi_recvi1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(out):: lvec(:)  !< receive values
   integer(ip_i4_p), intent(in) :: pid      !< pid to recv from
   integer(ip_i4_p), intent(in) :: tag      !< tag
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_recvi1)'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: status(MPI_STATUS_SIZE)  ! mpi status info
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(lvec)

   call MPI_RECV(lvec,lsize,MPI_INTEGER,pid,tag,comm,status,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_recvi1

!===============================================================================
!===============================================================================

!> Receive a scalar double

SUBROUTINE oasis_mpi_recvr0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),intent(out):: lvec     !< receive values
   integer(ip_i4_p), intent(in) :: pid      !< pid to recv from
   integer(ip_i4_p), intent(in) :: tag      !< tag
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_recvr0)'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: status(MPI_STATUS_SIZE)  ! mpi status info
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1

   call MPI_RECV(lvec,lsize,MPI_REAL8,pid,tag,comm,status,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_recvr0

!===============================================================================
!===============================================================================

!> Receive an array of 1D doubles

SUBROUTINE oasis_mpi_recvr1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),intent(out):: lvec(:)  !< receive values
   integer(ip_i4_p), intent(in) :: pid      !< pid to recv from
   integer(ip_i4_p), intent(in) :: tag      !< tag
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_recvr1)'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: status(MPI_STATUS_SIZE)  ! mpi status info
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(lvec)

   call MPI_RECV(lvec,lsize,MPI_REAL8,pid,tag,comm,status,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_recvr1

!===============================================================================
!===============================================================================

!> Receive an array of 3D doubles

SUBROUTINE oasis_mpi_recvr3(array,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),intent(out):: array(:,:,:)  !< receive values
   integer(ip_i4_p), intent(in) :: pid           !< pid to recv from
   integer(ip_i4_p), intent(in) :: tag           !< tag
   integer(ip_i4_p), intent(in) :: comm          !< mpi communicator
   character(*),optional,intent(in) :: string        !< to identify caller

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_recvr3)'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: status(MPI_STATUS_SIZE)  ! mpi status info
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(array)

   call MPI_RECV(array,lsize,MPI_REAL8,pid,tag,comm,status,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_recvr3

!===============================================================================
!===============================================================================

!> Broadcast a scalar integer

SUBROUTINE oasis_mpi_bcasti0(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p),     intent(inout):: vec      !< values to broadcast
   integer(ip_i4_p),     intent(in)   :: comm     !< mpi communicator
   character(*),optional,intent(in)   :: string   !< to identify caller
   integer(ip_i4_p), optional, intent(in) :: pebcast  !< bcast pe, default is task 0

   !----- local ---
   character(*),parameter         :: subname = '(oasis_mpi_bcasti0)'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast an integer
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_INTEGER,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcasti0

!===============================================================================
!===============================================================================

!> Broadcast a scalar logical

SUBROUTINE oasis_mpi_bcastl0(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   logical,              intent(inout):: vec      !< values to broadcast
   integer(ip_i4_p),     intent(in)   :: comm     !< mpi communicator
   character(*),optional,intent(in)   :: string   !< to identify caller
   integer(ip_i4_p), optional, intent(in) :: pebcast  !< bcast pe, default is task 0

   !----- local ---
   character(*),parameter         :: subname = '(oasis_mpi_bcastl0)'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a logical
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_LOGICAL,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastl0

!===============================================================================
!===============================================================================

!> Broadcast a character string

SUBROUTINE oasis_mpi_bcastc0(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   character(len=*),     intent(inout):: vec      !< values to broadcast
   integer(ip_i4_p),     intent(in)   :: comm     !< mpi communicator
   character(*),optional,intent(in)   :: string   !< to identify caller
   integer(ip_i4_p), optional, intent(in) :: pebcast  !< bcast pe, default is task 0

   !----- local ---
   character(*),parameter         :: subname = '(oasis_mpi_bcastc0)'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a character string
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = len(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_CHARACTER,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastc0

!===============================================================================
!===============================================================================

!> Broadcast an array of 1D character strings

SUBROUTINE oasis_mpi_bcastc1(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   character(len=*),     intent(inout):: vec(:)   !< values to broadcast
   integer(ip_i4_p),     intent(in)   :: comm     !< mpi communicator
   character(*),optional,intent(in)   :: string   !< to identify caller
   integer(ip_i4_p), optional, intent(in) :: pebcast  !< bcast pe, default is task 0

   !----- local ---
   character(*),parameter         :: subname = '(oasis_mpi_bcastc1)'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a character string
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(vec)*len(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_CHARACTER,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastc1

!===============================================================================
!===============================================================================

!> Broadcast a scalar double

SUBROUTINE oasis_mpi_bcastr0(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(inout):: vec      !< values to broadcast
   integer(ip_i4_p),     intent(in)   :: comm     !< mpi communicator
   character(*),optional,intent(in)   :: string   !< to identify caller
   integer(ip_i4_p), optional, intent(in) :: pebcast  !< bcast pe, default is task 0

   !----- local ---
   character(*),parameter         :: subname = '(oasis_mpi_bcastr0)'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a real
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_REAL8,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastr0

!===============================================================================
!===============================================================================

!> Broadcast an array of 1D integers

SUBROUTINE oasis_mpi_bcasti1(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p),     intent(inout):: vec(:)   !< values to broadcast
   integer(ip_i4_p),     intent(in)   :: comm     !< mpi communicator
   character(*),optional,intent(in)   :: string   !< to identify caller
   integer(ip_i4_p), optional, intent(in) :: pebcast  !< bcast pe, default is task 0

   !----- local ---
   character(*),parameter         :: subname = '(oasis_mpi_bcasti1)'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a vector of integers
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_INTEGER,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcasti1

!===============================================================================
!===============================================================================

!> Broadcast an array of 1D logicals

SUBROUTINE oasis_mpi_bcastl1(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   logical,              intent(inout):: vec(:)   !< values to broadcast
   integer(ip_i4_p),     intent(in)   :: comm     !< mpi communicator
   character(*),optional,intent(in)   :: string   !< to identify caller
   integer(ip_i4_p), optional, intent(in) :: pebcast  !< bcast pe, default is task 0

   !----- local ---
   character(*),parameter         :: subname = '(oasis_mpi_bcastl1)'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a logical
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_LOGICAL,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastl1

!===============================================================================
!===============================================================================

!> Broadcast an array of 1D doubles

SUBROUTINE oasis_mpi_bcastr1(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(inout):: vec(:)   !< values to broadcast
   integer(ip_i4_p),     intent(in)   :: comm     !< mpi communicator
   character(*),optional,intent(in)   :: string   !< to identify caller
   integer(ip_i4_p), optional, intent(in) :: pebcast  !< bcast pe, default is task 0

   !----- local ---
   character(*),parameter         :: subname = '(oasis_mpi_bcastr1)'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_REAL8,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastr1

!===============================================================================
!===============================================================================

!> Broadcast an array of 2D doubles

SUBROUTINE oasis_mpi_bcastr2(arr,comm,string,pebcast)

   IMPLICIT none

   !----- arguments -----
   real(ip_double_p),    intent(inout):: arr(:,:) !< values to broadcast
   integer(ip_i4_p),     intent(in)   :: comm     !< mpi communicator
   character(*),optional,intent(in)   :: string   !< to identify caller
   integer(ip_i4_p), optional, intent(in) :: pebcast  !< bcast pe, default is task 0

   !----- local -----
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

   !----- formats -----
   character(*),parameter         :: subname = '(oasis_mpi_bcastr2)'

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a 2d array of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(arr)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(arr,lsize,MPI_REAL8,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastr2

!===============================================================================
!===============================================================================

!> Broadcast an array of 2D integers

SUBROUTINE oasis_mpi_bcasti2(arr,comm,string,pebcast)

   IMPLICIT none

   !----- arguments -----
   integer,              intent(inout):: arr(:,:) !< values to broadcast
   integer(ip_i4_p),     intent(in)   :: comm     !< mpi communicator
   character(*),optional,intent(in)   :: string   !< to identify caller
   integer(ip_i4_p), optional, intent(in) :: pebcast  !< bcast pe, default is task 0

   !----- local -----
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

   !----- formats -----
   character(*),parameter         :: subname = '(oasis_mpi_bcasti2)'

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a 2d array of integers
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(arr)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(arr,lsize,MPI_INTEGER,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcasti2

!===============================================================================
!===============================================================================

!> Broadcast an array of 3D doubles

SUBROUTINE oasis_mpi_bcastr3(arr,comm,string,pebcast)

   IMPLICIT none

   !----- arguments -----
   real(ip_double_p),    intent(inout):: arr(:,:,:) !< values to broadcast
   integer(ip_i4_p),     intent(in)   :: comm       !< mpi communicator
   character(*),optional,intent(in)   :: string     !< to identify caller
   integer(ip_i4_p), optional, intent(in) :: pebcast  !< bcast pe, default is task 0

   !----- local -----
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

   !----- formats -----
   character(*),parameter         :: subname = '(oasis_mpi_bcastr3)'

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a 3d array of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(arr)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(arr,lsize,MPI_REAL8,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastr3

!===============================================================================
!===============================================================================

!> Initialize variables for oasis_mpi_gatherv and oasis_mpi_scatterv

!> This method initializes glob1DArr, globSize, and displs for use
!> in the oasis_mpi_gatherv and oasis_mpi_scatterv routines.  locArr is the
!> distributed array to gather from or scatter to.

SUBROUTINE oasis_mpi_gathScatvInitr1(comm, rootid, locArr, glob1DArr, globSize, &
                                   displs, string )

   IMPLICIT none

   !----- arguments -----
   integer(ip_i4_p), intent(in)    :: comm          !< mpi communicator
   integer(ip_i4_p), intent(in)    :: rootid        !< MPI task to gather/scatter on
   real(ip_double_p),intent(in)    :: locArr(:)     !< Local array of distributed data
   real(ip_double_p),pointer       :: glob1DArr(:)  !< Global 1D array of gathered data
   integer(ip_i4_p), pointer       :: globSize(:)   !< Size of each distributed piece
   integer(ip_i4_p), pointer       :: displs(:)     !< Displacements for receive
   character(*),optional,intent(in):: string        !< to identify caller

   !----- local -----
   integer(ip_i4_p)               :: npes          ! Number of MPI tasks
   integer(ip_i4_p)               :: locSize       ! Size of local distributed data
   integer(ip_i4_p), pointer      :: sendSize(:)   ! Size to send for initial gather
   integer(ip_i4_p)               :: i             ! Index
   integer(ip_i4_p)               :: rank          ! Rank of this MPI task
   integer(ip_i4_p)               :: nSize         ! Maximum size to send
   integer(ip_i4_p)               :: ierr          ! Error code
   integer(ip_i4_p)               :: nSiz1D        ! Size of 1D global array
   integer(ip_i4_p)               :: maxSize       ! Maximum size

   !----- formats -----
   character(*),parameter         :: subname = '(oasis_mpi_gathScatvInitr1)'

!-------------------------------------------------------------------------------
! PURPOSE: Setup arrays for a gatherv/scatterv operation
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   locSize = size(locarr)
   call oasis_mpi_commsize( comm, npes )
   call oasis_mpi_commrank( comm, rank )
   allocate( globSize(npes) )
   !
   ! --- Gather the send global sizes from each MPI task -----------------------
   !
   allocate( sendSize(npes) )
   sendSize(:) = 1
   globSize(:) = 1
   call MPI_GATHER( locSize, 1, MPI_INTEGER, globSize, sendSize, &
                    MPI_INTEGER, rootid, comm, ierr )
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif
   deallocate( sendSize )
   !
   ! --- Prepare the displacement and allocate arrays -------------------------
   !
   allocate( displs(npes) )
   displs(1) = 0
   if ( rootid /= rank )then
      maxSize = 1
      globSize = 1
   else
      maxSize = maxval(globSize)
   end if
   nsiz1D  = min(maxSize,globSize(1))
   do i = 2, npes
      nSize = min(maxSize,globSize(i-1))
      displs(i) = displs(i-1) + nSize
      nsiz1D = nsiz1D + min(maxSize,globSize(i))
   end do
   allocate( glob1DArr(nsiz1D) )
   !----- Do some error checking for the root task arrays computed ----
   if ( rootid == rank )then
      if ( nsiz1D /= sum(globSize) ) &
         call oasis_mpi_abort( subName//" : Error, size of global array not right" )
      if ( any(displs < 0) .or. any(displs >= nsiz1D) ) &
         call oasis_mpi_abort( subName//" : Error, displacement array not right" )
      if ( (displs(npes)+globSize(npes)) /= nsiz1D ) &
         call oasis_mpi_abort( subName//" : Error, displacement array values too big" )
   end if

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_gathScatvInitr1

!===============================================================================
!===============================================================================

!> Gather a vector of distributed data to a rootid

!> This method passes in glob1DArr, globSize, and displs computed
!> in the oasis_mpi_gathscatvinit routine and uses that information to 
!> gather the locArr into the glob1Darr on processor rootid in communicator
!> comm.

SUBROUTINE oasis_mpi_gathervr1(locarr, locSize, glob1DArr, globSize, displs, rootid, &
                             comm, string )

   IMPLICIT none

   !----- arguments -----
   real(ip_double_p),intent(in)    :: locArr(:)     !< Local array
   real(ip_double_p),intent(inout) :: glob1DArr(:)  !< Global 1D array to receive in on
   integer(ip_i4_p), intent(in)    :: locSize       !< Number to send from this PE
   integer(ip_i4_p), intent(in)    :: globSize(:)   !< Number to receive from each PE
   integer(ip_i4_p), intent(in)    :: displs(:)     !< Displacements for receives
   integer(ip_i4_p), intent(in)    :: rootid        !< MPI task to gather on
   integer(ip_i4_p), intent(in)    :: comm          !< mpi communicator
   character(*),optional,intent(in):: string        !< to identify caller

   !----- local -----
   integer(ip_i4_p)               :: ierr          ! Error code

   !----- formats -----
   character(*),parameter         :: subname = '(oasis_mpi_gathervr1)'

!-------------------------------------------------------------------------------
! PURPOSE: Gather a 1D array of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_GATHERV( locarr, locSize, MPI_REAL8, glob1Darr, globSize, displs, &
                     MPI_REAL8, rootid, comm, ierr )
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_gathervr1

!===============================================================================
!===============================================================================

!> Scatter a vector of global data from a rootid

!> This method passes in glob1DArr, globSize, and displs computed
!> in the oasis_mpi_gathscatvinit routine and uses that information to 
!> scatter glob1Darr on processor rootid in communicator comm to locarr
!> on other processors.

SUBROUTINE oasis_mpi_scattervr1(locarr, locSize, glob1Darr, globSize, displs, rootid, &
                              comm, string )

   IMPLICIT none

   !----- arguments -----
   real(ip_double_p),intent(out)   :: locarr(:)     !< Local array
   real(ip_double_p),intent(in)    :: glob1Darr(:)  !< Global 1D array to send from
   integer(ip_i4_p), intent(in)    :: locSize       !< Number to receive this PE
   integer(ip_i4_p), intent(in)    :: globSize(:)   !< Number to send to each PE
   integer(ip_i4_p), intent(in)    :: displs(:)     !< Displacements for send
   integer(ip_i4_p), intent(in)    :: rootid        !< MPI task to scatter on
   integer(ip_i4_p), intent(in)    :: comm          !< mpi communicator
   character(*),optional,intent(in):: string        !< to identify caller

   !----- local -----
   integer(ip_i4_p)               :: ierr          ! Error code

   !----- formats -----
   character(*),parameter         :: subname = '(oasis_mpi_scattervr1)'

!-------------------------------------------------------------------------------
! PURPOSE: Scatter a 1D array of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_SCATTERV( glob1Darr, globSize, displs, MPI_REAL8, locarr, locSize, &
                      MPI_REAL8, rootid, comm, ierr )
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_scattervr1


!===============================================================================
!===============================================================================

!> Compute a global Sum for a scalar integer

SUBROUTINE oasis_mpi_sumi0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec     !< local values
   integer(ip_i4_p), intent(out):: gvec     !< global values
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sumi0)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumi0

!===============================================================================
!===============================================================================

!> Compute a 1D array of global sums for an array of 1D integers

!> This sums an array of local integers to an array of summed integers.
!> This does not reduce the array to a scalar.

SUBROUTINE oasis_mpi_sumi1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec(:)  !< local values
   integer(ip_i4_p), intent(out):: gvec(:)  !< global values
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sumi1)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumi1

!===============================================================================
!===============================================================================

!> Compute a global sum for a scalar 8 byte integer

SUBROUTINE oasis_mpi_sumb0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i8_p), intent(in) :: lvec     !< local values
   integer(ip_i8_p), intent(out):: gvec     !< global values
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sumb0)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumb0

!===============================================================================
!===============================================================================

!> Compute a 1D array of global sums for an array of 1D 8 byte integers

!> This sums an array of local integers to an array of summed integers.
!> This does not reduce the array to a scalar.

SUBROUTINE oasis_mpi_sumb1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i8_p), intent(in) :: lvec(:)  !< local values
   integer(ip_i8_p), intent(out):: gvec(:)  !< global values
   integer(ip_i4_p), intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sumb1)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumb1

!===============================================================================
!===============================================================================

!> Compute a global sum for a scalar double

SUBROUTINE oasis_mpi_sumr0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec     !< local values
   real(ip_double_p),    intent(out):: gvec     !< global values
   integer(ip_i4_p),     intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sumr0)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumr0

!===============================================================================
!===============================================================================

!> Compute a 1D array of global sums for an array of 1D doubles

!> This sums an array of local doubles to an array of summed doubles.
!> This does not reduce the array to a scalar.

SUBROUTINE oasis_mpi_sumr1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec(:)  !< local values
   real(ip_double_p),    intent(out):: gvec(:)  !< global values
   integer(ip_i4_p),     intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sumr1)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumr1

!===============================================================================
!===============================================================================

!> Compute a 2D array of global sums for an array of 2D doubles

!> This sums an array of local doubles to an array of summed doubles.
!> This does not reduce the array to a scalar.

SUBROUTINE oasis_mpi_sumr2(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec(:,:)!< local values
   real(ip_double_p),    intent(out):: gvec(:,:)!< global values
   integer(ip_i4_p),     intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sumr2)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumr2

!===============================================================================
!===============================================================================

!> Compute a 3D array of global sums for an array of 3D doubles

!> This sums an array of local doubles to an array of summed doubles.
!> This does not reduce the array to a scalar.

SUBROUTINE oasis_mpi_sumr3(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec(:,:,:) !< local values
   real(ip_double_p),    intent(out):: gvec(:,:,:) !< global values
   integer(ip_i4_p),     intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_sumr3)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumr3

!===============================================================================
!===============================================================================

!> Compute a global minimum for a scalar integer

SUBROUTINE oasis_mpi_mini0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p),     intent(in) :: lvec     !< local values
   integer(ip_i4_p),     intent(out):: gvec     !< global values
   integer(ip_i4_p),     intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_mini0)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_mini0

!===============================================================================
!===============================================================================

!> Compute an array of global minimums for an array of 1D integers

SUBROUTINE oasis_mpi_mini1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p),     intent(in) :: lvec(:)  !< local values
   integer(ip_i4_p),     intent(out):: gvec(:)  !< global values
   integer(ip_i4_p),     intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_mini1)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_mini1

!===============================================================================
!===============================================================================

!> Compute an global minimum for a scalar double

SUBROUTINE oasis_mpi_minr0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec     !< local values
   real(ip_double_p),    intent(out):: gvec     !< global values
   integer(ip_i4_p),     intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_minr0)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_minr0

!===============================================================================
!===============================================================================

!> Compute an array of global minimums for an array of 1D doubles

SUBROUTINE oasis_mpi_minr1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec(:)  !< local values
   real(ip_double_p),    intent(out):: gvec(:)  !< global values
   integer(ip_i4_p),     intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_minr1)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_minr1

!===============================================================================
!===============================================================================

!> Compute a global maximum for a scalar integer

SUBROUTINE oasis_mpi_maxi0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p),     intent(in) :: lvec     !< local values
   integer(ip_i4_p),     intent(out):: gvec     !< global values
   integer(ip_i4_p),     intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_maxi0)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_maxi0

!===============================================================================
!===============================================================================

!> Compute an array of global maximums for an array of 1D integers

SUBROUTINE oasis_mpi_maxi1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p),     intent(in) :: lvec(:)  !< local values
   integer(ip_i4_p),     intent(out):: gvec(:)  !< global values
   integer(ip_i4_p),     intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_maxi1)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_maxi1

!===============================================================================
!===============================================================================

!> Compute a global maximum for a scalar double

SUBROUTINE oasis_mpi_maxr0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec     !< local values
   real(ip_double_p),    intent(out):: gvec     !< global values
   integer(ip_i4_p),     intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_maxr0)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_maxr0

!===============================================================================
!===============================================================================

!> Compute an array of global maximums for an array of 1D doubles

SUBROUTINE oasis_mpi_maxr1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec(:)  !< local values
   real(ip_double_p),    intent(out):: gvec(:)  !< global values
   integer(ip_i4_p) ,    intent(in) :: comm     !< mpi communicator
   character(*),optional,intent(in) :: string   !< to identify caller
   logical,     optional,intent(in) :: all      !< if true call allreduce, otherwise reduce to task 0

   !----- local ---
   character(*),parameter       :: subname = '(oasis_mpi_maxr1)'
   logical                      :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_maxr1

!===============================================================================
!===============================================================================

!> Get the total number of tasks associated with a communicator

SUBROUTINE oasis_mpi_commsize(comm,size,string)

   IMPLICIT none

   !----- arguments ---
   integer,intent(in)                 :: comm   !< mpi communicator
   integer,intent(out)                :: size   !< output comm size
   character(*),optional,intent(in)   :: string !< to identify caller

   !----- local ---
   character(*),parameter             :: subname = '(oasis_mpi_commsize)'
   integer(ip_i4_p)                   :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI commsize
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_COMM_SIZE(comm,size,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_commsize

!===============================================================================
!===============================================================================

!> Get the rank (task ID) for a task in a communicator

SUBROUTINE oasis_mpi_commrank(comm,rank,string)

   IMPLICIT none

   !----- arguments ---
   integer,intent(in)                 :: comm    !< mpi communicator
   integer,intent(out)                :: rank    !< output task ID
   character(*),optional,intent(in)   :: string  !< to identify caller

   !----- local ---
   character(*),parameter             :: subname = '(oasis_mpi_commrank)'
   integer(ip_i4_p)                   :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI commrank
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_COMM_RANK(comm,rank,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_commrank

!===============================================================================
!===============================================================================

!> Check whether MPI has been initialized

SUBROUTINE oasis_mpi_initialized(flag,string)

   IMPLICIT none

   !----- arguments ---
   logical,intent(out)                :: flag     !< true if MPI_INITIALIZED has been called
   character(*),optional,intent(in)   :: string   !< to identify caller

   !----- local ---
   character(*),parameter             :: subName = '(oasis_mpi_initialized)'
   integer(ip_i4_p)                   :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI initialized
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_INITIALIZED(flag,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_initialized

!===============================================================================
!===============================================================================

!> Return a timestamp from MPI_WTIME

SUBROUTINE oasis_mpi_wtime(wtime)

   IMPLICIT none

   !----- arguments ---
   real(ip_r8_p), intent(out) :: wtime  !< time in MPI_WTIME units

   !----- local ---
   character(*),parameter             :: subName = '(oasis_mpi_wtime)'

!-------------------------------------------------------------------------------
! PURPOSE: MPI wtime
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   wtime = MPI_WTIME()

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_wtime

!===============================================================================
!===============================================================================

!> Write error messages and Call MPI_ABORT

SUBROUTINE oasis_mpi_abort(string,rcode)

   IMPLICIT none

   !----- arguments ---
   character(*),optional,intent(in)   :: string   !< to identify caller
   integer,optional,intent(in)        :: rcode    !< optional code

   !----- local ---
   character(*),parameter             :: subName = '(oasis_mpi_abort)'
   character(len=256)                 :: lstr
   integer(ip_i4_p)                   :: ierr
   integer                            :: rc       ! return code

!-------------------------------------------------------------------------------
! PURPOSE: MPI abort
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   if ( present(string) .and. present(rcode)) then
      write(lstr,'(a,i6.6)') trim(string)//' rcode = ',rcode
   elseif (present(string)) then
      lstr = trim(string)
   else
      lstr = ' '
   endif

   IF ( PRESENT(rcode)) THEN
       CALL oasis_abort(cd_routine=subName,cd_message=TRIM(string),rcode=rcode)
   ELSE
       CALL oasis_abort(cd_routine=subName,cd_message=TRIM(string))
   ENDIF

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_abort

!===============================================================================
!===============================================================================

!> Call MPI_BARRIER for a particular communicator

SUBROUTINE oasis_mpi_barrier(comm,string)

   IMPLICIT none

   !----- arguments ---
   integer,intent(in)                 :: comm     !< mpi communicator
   character(*),optional,intent(in)   :: string   !< to identify caller

   !----- local ---
   character(*),parameter         :: subname = '(oasis_mpi_barrier)'
   integer(ip_i4_p)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI barrier
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_BARRIER(comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_barrier

!===============================================================================
!===============================================================================

!> Call MPI_INIT

SUBROUTINE oasis_mpi_init(string)

   IMPLICIT none

   !----- arguments ---
   character(*),optional,intent(in)   :: string   !< to identify caller

   !----- local ---
   character(*),parameter         :: subname = '(oasis_mpi_init)'
   integer(ip_i4_p)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI init
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_INIT(ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_init

!===============================================================================
!===============================================================================

!> Call MPI_FINALZE

SUBROUTINE oasis_mpi_finalize(string)

   IMPLICIT none

   !----- arguments ---
   character(*),optional,intent(in)   :: string   !< to identify caller

   !----- local ---
   character(*),parameter         :: subname = '(oasis_mpi_finalize)'
   integer(ip_i4_p)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI finalize
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_FINALIZE(ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_finalize

!===============================================================================
!===============================================================================

!> Custom method for reducing MPI lists across pes for OASIS

SUBROUTINE oasis_mpi_reducelists(linp1,comm,cntout,lout1,callstr,fastcheck,fastcheckout, &
   linp2,lout2,spval2,linp3,lout3,spval3)

   IMPLICIT none

   !----- arguments ---
   character(*),pointer,intent(in)    :: linp1(:)  !< input list on each task
   integer             ,intent(in)    :: comm      !< mpi communicator
   integer             ,intent(out)   :: cntout    !< size of lout1 list
   character(*),pointer,intent(inout) :: lout1(:)  !< reduced output list, same on all tasks
   character(*)        ,intent(in)    :: callstr   !< to identify caller
   logical             ,intent(in)   ,optional :: fastcheck !< run a fastcheck first
   logical             ,intent(out)  ,optional :: fastcheckout !< true if fastcheck worked
   character(*),pointer,intent(in)   ,optional :: linp2(:)  !< input list on each task
   character(*),pointer,intent(inout),optional :: lout2(:)  !< reduced output list, same on all tasks
   character(*)        ,intent(in)   ,optional :: spval2    !< unset value for linp2
   integer     ,pointer,intent(in)   ,optional :: linp3(:)  !< input list on each task
   integer     ,pointer,intent(inout),optional :: lout3(:)  !< reduced output list, same on all tasks
   integer             ,intent(in)   ,optional :: spval3    !< unset value for linp3

   !----- local ---
   integer(kind=ip_i4_p) :: m,n,k,p
   integer(kind=ip_i4_p) :: llen,lsize
   integer(kind=ip_i4_p) :: cnt, cntr
   integer(kind=ip_i4_p) :: commrank, commsize
   integer(kind=ip_i4_p) :: listcheck, listcheckall
   integer(kind=ip_i4_p) :: maxloops, sendid, recvid, kfac
   logical               :: found, present2, present3
   integer(kind=ip_i4_p) :: status(MPI_STATUS_SIZE)  ! mpi status info
   character(len=ic_lvar2),pointer :: recv_varf1(:),varf1a(:),varf1b(:)
   character(len=ic_lvar2),pointer :: recv_varf2(:),varf2a(:),varf2b(:)
   integer(kind=ip_i4_p)  ,pointer :: recv_varf3(:),varf3a(:),varf3b(:)
   character(len=ic_lvar2) :: string
   logical, parameter      :: local_timers_on = .false.
   integer(ip_i4_p)        :: ierr
   character(*),parameter  :: subname = '(oasis_mpi_reducelists)'

!-------------------------------------------------------------------------------
! PURPOSE: Custom method for reducing MPI lists for OASIS using a log2
! algorithm.  This generates a list on all tasks that consists of the intersection
! of all the values on all the tasks with each value listed once.  linp1
! is the input list, possibly different on each task.  lout1
! is the resulting list, the same on each task, consistenting of all unique
! values of linp1 from all tasks.  This ultimately reduces the list onto
! the root task and then it's broadcast.  The reduction occurs via a binary
! type reduction from tasks to other tasks.
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   string = trim(callstr)
   if (present(fastcheckout)) fastcheckout = .false.   ! by default
   call oasis_mpi_commrank(comm,commrank,string=subname//trim(string))
   call oasis_mpi_commsize(comm,commsize,string=subname//trim(string))

   !-----------------------------------------------
   !> * Check argument consistency
   !-----------------------------------------------

   if ((present(linp2) .and. .not.present(lout2)) .or. &
       (present(lout2) .and. .not.present(linp2))) then
      call oasis_mpi_abort(subname//trim(string)//" linp2 lout2 both must be present ")
   endif
   present2 = present(linp2)

   if ((present(linp3) .and. .not.present(lout3)) .or. &
       (present(lout3) .and. .not.present(linp3))) then
      call oasis_mpi_abort(subname//trim(string)//" linp3 lout3 both must be present ")
   endif
   present3 = present(linp3)

   if (len(linp1) > len(varf1a)) then
      call oasis_mpi_abort(subname//trim(string)//" linp1 too long ")
   endif

   if (present(linp2)) then
      if (size(linp2) /= size(linp1)) then
         call oasis_mpi_abort(subname//trim(string)//" linp1 linp2 not same size ")
      endif
      if (len(linp2) > len(varf2a)) then
         call oasis_mpi_abort(subname//trim(string)//" linp2 too long ")
      endif
      if (len(varf1a) /= len(varf2a)) then
         call oasis_mpi_abort(subname//trim(string)//" varf1a varf2a not same len ")
      endif
   endif

   if (present(linp3)) then
      if (size(linp3) /= size(linp1)) then
         call oasis_mpi_abort(subname//trim(string)//" linp1 linp3 not same size ")
      endif
   endif

   !-----------------------------------------------
   !> * Fast compare on all tasks
   ! If all tasks have same list, just skip the reduction
   !-----------------------------------------------

   if (present(fastcheck)) then
   if (fastcheck) then

      if (local_timers_on) call oasis_timer_start(trim(string)//'_rl_fastcheck')

      lsize = -1
      if (commrank == 0) then
         lsize = size(linp1)
      endif
      call oasis_mpi_bcast(lsize, comm, subname//trim(string)//' lsize check')

      ! varf1a holds linp1 from root on all tasks
      allocate(varf1a(lsize))
      varf1a = ' '
      if (commrank == 0) then
         varf1a(1:lsize) = linp1(1:lsize)
      endif
      call oasis_mpi_bcast(varf1a, comm, subname//trim(string)//' varf1a check')

      listcheck = 1
      if (OASIS_DEBUG >= 20) then
         write(nulprt,*) subname//trim(string),' sizes ',lsize,size(linp1)
      endif
      if (lsize /= size(linp1)) listcheck = 0
      n = 0
      do while (listcheck == 1 .and. n < lsize)
         n = n + 1
         if (varf1a(n) /= linp1(n)) listcheck = 0
         if (OASIS_DEBUG >= 20) then
            write(nulprt,*) subname//trim(string),' fcheck varf1a ',n,trim(linp1(n)),' ',trim(linp1(n)),listcheck
         endif
      enddo
      deallocate(varf1a)
      call oasis_mpi_min(listcheck,listcheckall,comm, subname//trim(string)//' listcheck',all=.true.)

      if (OASIS_DEBUG >= 15) then
         write(nulprt,*) subname//trim(string),' listcheck = ',listcheck,listcheckall
      endif
      if (local_timers_on) call oasis_timer_stop(trim(string)//'_rl_fastcheck')

      !-------------------------------------------------     
      ! linp1 same on all tasks, update lout1, lout2, lout3 and return
      !-------------------------------------------------     

      if (listcheckall == 1) then
         cntout = lsize
         allocate(lout1(lsize))
         lout1(1:lsize) = linp1(1:lsize)
         if (present2) then
            allocate(lout2(lsize))
            lout2(1:lsize) = linp2(1:lsize)
         endif
         if (present3) then
            allocate(lout3(lsize))
            lout3(1:lsize) = linp3(1:lsize)
         endif
         call oasis_debug_exit(subname)
         if (present(fastcheckout)) fastcheckout = .true.
         return
      endif

   endif  ! fastcheck
   endif  ! present fastcheck

   !-----------------------------------------------
   !> * Generate initial unique local name list
   !-----------------------------------------------

   llen = len(linp1)
   lsize = size(linp1)
   if (OASIS_Debug >= 15) then
      write(nulprt,*) subname//trim(string),' len, size = ',llen,lsize
      call oasis_flush(nulprt)
   endif

   allocate(varf1a(max(lsize,20)))  ! 20 is arbitrary starting number
   if (present2) allocate(varf2a(max(lsize,20)))  ! 20 is arbitrary starting number
   if (present3) allocate(varf3a(max(lsize,20)))  ! 20 is arbitrary starting number
   cnt = 0
   do n = 1,lsize
      p = 0
      found = .false.
      do while (p < cnt .and. .not.found)
         p = p + 1
         if (linp1(n) == varf1a(p)) found = .true.
      enddo
      if (.not.found) then
         cnt = cnt + 1
         varf1a(cnt) = linp1(n)
         if (present2) varf2a(cnt) = linp2(n)
         if (present3) varf3a(cnt) = linp3(n)
      endif
   enddo

   !-----------------------------------------------
   !> * Log2 reduction of linp over tasks to root
   !-----------------------------------------------

   maxloops = int(sqrt(float(commsize+1)))+1
   do m = 1,maxloops

      kfac = 2**m

      recvid = commrank + kfac/2  ! task to recv from 
      if (mod(commrank,kfac) /= 0 .or. &
         recvid < 0 .or. recvid > commsize-1) &
         recvid = -1

      sendid = commrank - kfac/2  ! task to send to
      if (mod(commrank+kfac/2,kfac) /= 0 .or. &
         sendid < 0 .or. sendid > commsize-1) &
         sendid = -1

      if (OASIS_Debug >= 15) then
         write(nulprt,*) subname//trim(string),' send/recv ids ',m,commrank,sendid,recvid
         call oasis_flush(nulprt)
      endif

      !-----------------------------------------------
      !>   * Send list
      !-----------------------------------------------

      if (sendid >= 0) then
         if (local_timers_on) call oasis_timer_start(trim(string)//'_rl_send')
         call MPI_SEND(cnt, 1, MPI_INTEGER, sendid, 5900+m, comm, ierr)
         call oasis_mpi_chkerr(ierr,subname//trim(string)//':send cnt')
         if (cnt > 0) then
            if (OASIS_Debug >= 15) then
               write(nulprt,*) subname//trim(string),' send size ',commrank,m,cnt,ic_lvar2
               call oasis_flush(nulprt)
            endif
            call MPI_SEND(varf1a(1:cnt), cnt*ic_lvar2, MPI_CHARACTER, sendid, 6900+m, comm, ierr)
            call oasis_mpi_chkerr(ierr,subname//trim(string)//':send varf1a')
            if (present2) then
               call MPI_SEND(varf2a(1:cnt), cnt*ic_lvar2, MPI_CHARACTER, sendid, 7900+m, comm, ierr)
               call oasis_mpi_chkerr(ierr,subname//trim(string)//':send varf2a')
            endif
            if (present3) then
               call MPI_SEND(varf3a(1:cnt), cnt, MPI_INTEGER, sendid, 8900+m, comm, ierr)
               call oasis_mpi_chkerr(ierr,subname//trim(string)//':send varf3a')
            endif
         endif  ! cnt > 0
         if (local_timers_on) call oasis_timer_stop (trim(string)//'_rl_send')
      endif  ! sendid >= 0

      !-----------------------------------------------
      !>   * Recv list
      !>   * Determine the unique list
      !-----------------------------------------------

      if (recvid >= 0) then
         if (local_timers_on) call oasis_timer_start (trim(string)//'_rl_recv')
         call MPI_RECV(cntr, 1, MPI_INTEGER, recvid, 5900+m, comm, status, ierr)
         call oasis_mpi_chkerr(ierr,subname//trim(string)//':recv cntr')
         if (cntr > 0) then
            if (OASIS_Debug >= 15) then
               write(nulprt,*) subname//trim(string),' recv size ',commrank,m,cntr,ic_lvar2
               call oasis_flush(nulprt)
            endif
            allocate(recv_varf1(cntr))
            call MPI_RECV(recv_varf1, cntr*ic_lvar2, MPI_CHARACTER, recvid, 6900+m, comm, status, ierr)
            call oasis_mpi_chkerr(ierr,subname//trim(string)//':recv varf1')
            if (present2) then
               allocate(recv_varf2(cntr))
               call MPI_RECV(recv_varf2, cntr*ic_lvar2, MPI_CHARACTER, recvid, 7900+m, comm, status, ierr)
               call oasis_mpi_chkerr(ierr,subname//trim(string)//':recv varf2')
            endif
            if (present3) then
               allocate(recv_varf3(cntr))
               call MPI_RECV(recv_varf3, cntr, MPI_INTEGER, recvid, 8900+m, comm, status, ierr)
               call oasis_mpi_chkerr(ierr,subname//trim(string)//':recv varf3')
            endif
         endif  ! cntr > 0
         if (local_timers_on) call oasis_timer_stop (trim(string)//'_rl_recv')

         if (local_timers_on) call oasis_timer_start(trim(string)//'_rl_rootsrch')
         do n = 1,cntr
            if (OASIS_Debug >= 15) write(nulprt,*) subname//trim(string),' check recv_varf1 ',m,n,trim(recv_varf1(n))

            p = 0
            found = .false.
            do while (p < cnt .and. .not.found)
               p = p + 1
               if (recv_varf1(n) == varf1a(p)) then
                  found = .true.
                  if (present2) then
                     if (present(spval2)) then
                        !--- use something other than spval2 if it exists and check consistency
                        if (varf2a(p) == spval2) then
                           varf2a(p) = recv_varf2(n)
                        elseif (recv_varf2(n) /= spval2 .and. varf2a(p) /= recv_varf2(n)) then
                           call oasis_abort(cd_routine=subname//trim(string),cd_message= &
                                'inconsistent linp2 value: '//trim(recv_varf2(n))//':'//trim(varf1a(p))//':'//trim(varf2a(p)))
                        endif
                     else
                        if (varf2a(p) /= recv_varf2(n)) then
                           call oasis_abort(cd_routine=subname//trim(string),cd_message= &
                                'inconsistent linp2 value: '//trim(recv_varf2(n))//':'//trim(varf1a(p))//':'//trim(varf2a(p)))
                        endif
                     endif
                  endif
                  if (present3) then
                     if (present(spval3)) then
                        !--- use something other than spval3 if it exists and check consistency
                        if (varf3a(p) == spval3) then
                           varf3a(p) = recv_varf3(n)
                        elseif (recv_varf3(n) /= spval3 .and. varf3a(p) /= recv_varf3(n)) then
                           write(nulprt,*) subname//trim(string),astr,'inconsistent linp3 var: ',&
                                     recv_varf3(n),':',trim(varf1a(p)),':',varf3a(p)
                           call oasis_abort(cd_routine=subname//trim(string),cd_message= &
                                'inconsistent linp3 value: '//trim(varf1a(p)))
                        endif
                     else
                        if (varf3a(p) /= recv_varf3(n)) then
                           write(nulprt,*) subname//trim(string),astr,'inconsistent linp3 var: ',&
                                     recv_varf3(n),':',trim(varf1a(p)),':',varf3a(p)
                           call oasis_abort(cd_routine=subname//trim(string),cd_message= &
                                'inconsistent linp3 value: '//trim(varf1a(p)))
                        endif
                     endif
                  endif
               endif
            enddo
            if (.not.found) then
               cnt = cnt + 1
               if (cnt > size(varf1a)) then
                  allocate(varf1b(size(varf1a)))
                  varf1b = varf1a
                  deallocate(varf1a)
                  if (OASIS_Debug >= 15) then
                     write(nulprt,*) subname//trim(string),' resize varf1a ',size(varf1b),cnt+cntr
                     call oasis_flush(nulprt)
                  endif
                  allocate(varf1a(cnt+cntr))
                  varf1a(1:size(varf1b)) = varf1b(1:size(varf1b))
                  deallocate(varf1b)
                  if (present2) then
                     allocate(varf2b(size(varf2a)))
                     varf2b = varf2a
                     deallocate(varf2a)
                     if (OASIS_Debug >= 15) then
                        write(nulprt,*) subname//trim(string),' resize varf2a ',size(varf2b),cnt+cntr
                        call oasis_flush(nulprt)
                     endif
                     allocate(varf2a(cnt+cntr))
                     varf2a(1:size(varf2b)) = varf2b(1:size(varf2b))
                     deallocate(varf2b)
                  endif
                  if (present3) then
                     allocate(varf3b(size(varf3a)))
                     varf3b = varf3a
                     deallocate(varf3a)
                     if (OASIS_Debug >= 15) then
                        write(nulprt,*) subname//trim(string),' resize varf3a ',size(varf3b),cnt+cntr
                        call oasis_flush(nulprt)
                     endif
                     allocate(varf3a(cnt+cntr))
                     varf3a(1:size(varf3b)) = varf3b(1:size(varf3b))
                     deallocate(varf3b)
                  endif
               endif
               varf1a(cnt) = recv_varf1(n)
               if (present2) varf2a(cnt) = recv_varf2(n)
               if (present3) varf3a(cnt) = recv_varf3(n)
            endif
         enddo  ! cntr
         if (local_timers_on) call oasis_timer_stop(trim(string)//'_rl_rootsrch')
         if (cntr > 0) then
            deallocate(recv_varf1)
            if (present2) deallocate(recv_varf2)
            if (present3) deallocate(recv_varf3)
         endif

      endif  ! recvid >= 0

   enddo  ! maxloops

   !-------------------------------------------------     
   !> * Broadcast the list information to all tasks from root
   !-------------------------------------------------     

   if (local_timers_on) then
      call oasis_timer_start(trim(string)//'_rl_bcast_barrier')
      if (comm /= MPI_COMM_NULL) &
               call MPI_BARRIER(comm, ierr)
      call oasis_timer_stop(trim(string)//'_rl_bcast_barrier')
   endif
   if (local_timers_on) call oasis_timer_start(trim(string)//'_rl_bcast')
   call oasis_mpi_bcast(cnt,comm,subname//trim(string)//' cnt')
   cntout = cnt
   allocate(lout1(cntout))
   if (commrank == 0) then
      do n = 1,cntout
         lout1(n) = trim(varf1a(n))
      enddo
   endif
   deallocate(varf1a)
   call oasis_mpi_bcast(lout1,comm,subname//trim(string)//' lout1')

   if (present2) then
      allocate(lout2(cntout))
      if (commrank == 0) then
         do n = 1,cntout
            lout2(n) = trim(varf2a(n))
         enddo
      endif
      deallocate(varf2a)
      call oasis_mpi_bcast(lout2,comm,subname//trim(string)//' lout2')
   endif

   if (present3) then
      allocate(lout3(cntout))
      if (commrank == 0) then
         do n = 1,cntout
            lout3(n) = varf3a(n)
         enddo
      endif
      deallocate(varf3a)
      call oasis_mpi_bcast(lout3,comm,subname//trim(string)//' lout3')
   endif

   !--- document

   if (OASIS_debug >= 15) then
      do n = 1,cnt
         if (present2 .and. present3) then
            write(nulprt,*) subname,trim(string),' list: ',n,trim(lout1(n)),' ',trim(lout2(n)),lout3(n)
         elseif (present2) then
            write(nulprt,*) subname,trim(string),' list: ',n,trim(lout1(n)),' ',trim(lout2(n))
         elseif (present3) then
            write(nulprt,*) subname,trim(string),' list: ',n,trim(lout1(n)),lout3(n)
         else
            write(nulprt,*) subname,trim(string),' list: ',n,trim(lout1(n))
         endif
      enddo
      call oasis_flush(nulprt)
   endif
   if (local_timers_on) call oasis_timer_stop (trim(string)//'_rl_bcast')

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_reducelists

!===============================================================================
!===============================================================================

END MODULE mod_oasis_mpi
