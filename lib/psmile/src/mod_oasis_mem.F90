!> Provides methods for querying memory use

MODULE mod_oasis_mem

!-------------------------------------------------------------------------------
! PURPOSE: memory use query methods
!    Should call oasis_mem_init once before calling other interfaces
!-------------------------------------------------------------------------------

   use mod_oasis_kinds, only : ip_double_p
   use mod_oasis_sys  , only : oasis_debug_enter, oasis_debug_exit

   implicit none
   private
    
! PUBLIC: Public interfaces

   public ::  oasis_mem_getusage, &
	      oasis_mem_init, &
              oasis_mem_print
    
! PUBLIC: Public interfaces

   real(ip_double_p) :: mb_blk = 1.0_ip_double_p
   logical           :: initset = .false.

!===============================================================================
CONTAINS
!===============================================================================

!> Initialize memory conversion to MB

subroutine oasis_mem_init(iunit)

   implicit none

   !----- arguments -----

   integer, optional :: iunit   !< output unit number for optional writes
     
   !----- local -----

   ! --- Memory stats --- 
   integer :: msize                   ! memory size (high water)
   integer :: mrss                    ! resident size (current memory use)
   integer :: msize0,msize1           ! temporary size
   integer :: mrss0,mrss1,mrss2       ! temporary rss
   integer :: mshare,mtext,mdatastack
   integer :: ierr
 
   integer :: GPTLget_memusage

   real(ip_double_p),allocatable :: mem_tmp(:)
   character(*),parameter  :: subname = '(oasis_mem_init)'
    
   !---------------------------------------------------

   call oasis_debug_enter(subname)

   ierr = GPTLget_memusage (msize, mrss0, mshare, mtext, mdatastack)
   allocate(mem_tmp(1024*1024))    ! 1 MWord, 8 MB
   mem_tmp = -1.0
   ierr = GPTLget_memusage (msize, mrss1, mshare, mtext, mdatastack)
   deallocate(mem_tmp)
   ierr = GPTLget_memusage (msize, mrss2, mshare, mtext, mdatastack)
   mb_blk = 1.0_ip_double_p
   if (mrss1 - mrss0 > 0) then
      mb_blk = (8.0_ip_double_p)/((mrss1-mrss0)*1.0_ip_double_p)
      initset = .true.
   endif

   if (present(iunit)) then
      write(iunit,'(A,l4)')    subname//' Initset conversion flag is ',initset
      write(iunit,'(A,f16.2)') subname//' 8 MB memory   alloc in MB is ',(mrss1-mrss0)*mb_blk
      write(iunit,'(A,f16.2)') subname//' 8 MB memory dealloc in MB is ',(mrss1-mrss2)*mb_blk
      write(iunit,'(A,f16.2)') subname//' Memory block size conversion in bytes is ',mb_blk*1024_ip_double_p*1024.0_ip_double_p
   endif

   call oasis_debug_exit(subname)

end subroutine oasis_mem_init

!===============================================================================

!> Determine memory use

subroutine oasis_mem_getusage(r_msize,r_mrss)

   implicit none

   !----- arguments ---
   real(ip_double_p),intent(out) :: r_msize  !< memory usage value
   real(ip_double_p),intent(out) :: r_mrss   !< memory usage value

   !----- local ---
   integer :: msize,mrss
   integer :: mshare,mtext,mdatastack
   integer :: ierr
   integer :: GPTLget_memusage
   character(*),parameter  :: subname = '(oasis_mem_getusage)'

   !---------------------------------------------------

   call oasis_debug_enter(subname)

   ierr = GPTLget_memusage (msize, mrss, mshare, mtext, mdatastack)
   r_msize = msize*mb_blk
   r_mrss  = mrss*mb_blk

   call oasis_debug_exit(subname)

end subroutine oasis_mem_getusage

!===============================================================================

!> Print memory use

subroutine oasis_mem_print(iunit,string)

   implicit none

   !----- arguments ---
   integer, intent(in) :: iunit    !< unit number to write to
   character(len=*),optional, intent(in) :: string  !< optional string

   !----- local ---   
   real(ip_double_p)  :: msize,mrss
   character(len=128) :: lstring
   character(*),parameter  :: subname = '(oasis_mem_print)'

   !---------------------------------------------------

   call oasis_debug_enter(subname)

   lstring = ' '
   if (present(string)) then
      lstring = string
   endif

   call oasis_mem_getusage(msize,mrss)

   if (initset) then
      write(iunit,'(2a,2f14.4,1x,a)') subname,' memory use (MB) = ',msize,mrss,trim(lstring)
   else
      write(iunit,'(2a,2f14.4,1x,a)') subname,' memory use (??) = ',msize,mrss,trim(lstring)
   endif

   call oasis_debug_exit(subname)

end subroutine oasis_mem_print

!===============================================================================

END MODULE mod_oasis_mem
