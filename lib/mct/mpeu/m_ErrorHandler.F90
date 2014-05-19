!
! !MODULE: m_ErrorHandler - alternative MPI error handlers. Presently relies on
! intel-fc traceback. FIXME: use mpeu traceback.
!
! !DESCRIPTION:
!
! !INTERFACE:

module m_ErrorHandler

#if defined(__INTEL_COMPILER)
    use ifcore
#endif
    use m_mpiF90

    implicit none
    private

    public error_handler_traceback

    character(len=*), parameter :: myname='MCT(MPEU)::m_ErrorHandler'

contains

    subroutine error_handler_traceback(comm, err_code)

        integer, intent(in) :: comm, err_code
        integer :: ierr, rank
        character(len=12) :: rank_str

        call MP_comm_rank(MP_COMM_WORLD, rank, ierr)
        write (rank_str, '(i12)') rank

#if defined(__INTEL_COMPILER)
        call tracebackqq('MPI traceback error handler called by rank: '//rank_str)
#endif
        call MP_abort(comm, err_code, ierr)

    end subroutine error_handler_traceback

end module m_ErrorHandler
