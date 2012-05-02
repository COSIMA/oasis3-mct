!-----------------------------------------------------------------------
! Copyright 2010, CERFACS, Toulouse, France.
! Copyright 2010, DKRZ, Hamburg, Germany.
! All rights reserved. Use is subject to OASIS4 license terms.
!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!
! Module prism_timer contains functionallity, which can be used to
! measure the time consumed in specific parts of the code.
!
! Available routines:
!  prism_timer_init         allocates timers
!  prism_timer_start        starts specific timer
!  prism_timer_stop         stops specific timer and sums up measured time intervals
!  prism_timer_print        root process prints all timers of all processes sharing
!                            the same mpi communicator provided to prism_timer_init
!                            in addition it frees all memory allocated by timers
!
!
! !REVISION HISTORY:
!
!   Date      Programmer   Description
! ----------  ----------   -----------
! 03.01.11    M. Hanke     created (based on psmile_timer.F90 and
!                                   prismdrv_timer.F90 from SV and JL)
! 20.09.11    T. Craig     extended
!
!----------------------------------------------------------------------
!
!  $Id: prism_timer.F90 2849 2011-01-05 08:14:13Z hanke $
!  $Author: hanke $
!
!----------------------------------------------------------------------

module mod_prism_timer

   use mod_prism_kinds
   use mod_prism_data
   USE mod_oasis_print
   use mod_prism_sys
   use mod_prism_mpi

   implicit none

   ! name of the application
   character (len=ic_med) :: app_name

   ! Communicator for which this timer is valid
   integer :: comm_timer

   ! size of the communicator
   integer :: comm_size

   ! rank of the local process within the communicator
   integer :: comm_rank

   ! name of the time statistics file
   character (len=ic_med) :: file_name

   type timer_details

      ! label of timer
      character (len=ic_med) :: label

      ! wall time values
      double precision :: start_wtime, end_wtime

      ! cpu time values
      double precision :: start_ctime, end_ctime

      ! is the timer running now
      character(len=1) :: runflag

   end type timer_details

   integer,parameter :: mtimer = 500
   integer :: ntimer
   type (timer_details) :: timer(mtimer)
   double precision     :: sum_ctime(mtimer) ! these values are not part of timer details
   double precision     :: sum_wtime(mtimer) ! because they are later used in an mpi call
   integer              :: count(mtimer)     ! number of calls

   integer :: output_unit = 901
   logical,save :: single_timer_header
   character(len=1),parameter :: t_stopped = ' '
   character(len=1),parameter :: t_running = '*'

   LOGICAL :: ll_timer = .TRUE.

   contains

! --------------------------------------------------------------------------------

      subroutine prism_timer_init (app, file, comm)

         implicit none

         character (len=*), intent (in)   :: app
         character (len=*), intent (in)   :: file
         integer, intent (in)             :: comm

         integer :: ierror,n
         character(len=*),parameter :: subname = 'prism_timer_init'

         IF (ll_timer .EQV. .TRUE.) THEN

         call prism_sys_unitget(output_unit)

         app_name  = trim (app)
         file_name = trim (file)

         ntimer = 0
         do n = 1,mtimer
            timer(n)%label       = ' '
            timer(n)%start_wtime = 0
            timer(n)%end_wtime   = 0
            timer(n)%start_ctime = 0
            timer(n)%end_ctime   = 0
            timer(n)%runflag     = t_stopped

            sum_wtime(n)         = 0
            sum_ctime(n)         = 0
            count(n)             = 0
         enddo

         ! initialise MPI specific data
         comm_timer = comm

         call MPI_Comm_size(comm, comm_size, ierror)
         call MPI_Comm_rank(comm, comm_rank, ierror)

         write(file_name,'(a,i4.4)') trim(file)//'_',comm_rank

         open(output_unit, file=trim(file_name), form="FORMATTED", &
             status="UNKNOWN")
         write(output_unit,*) ''
         close(output_unit)
         single_timer_header = .false.
         ENDIF

      end subroutine prism_timer_init

! --------------------------------------------------------------------------------

      subroutine prism_timer_start (timer_label, barrier)

         implicit none

         character(len=*), intent (in) :: timer_label
         logical, intent (in), optional :: barrier

         integer :: ierr
         integer :: timer_id
         real :: cpu_time_arg
         character(len=*),parameter :: subname = 'prism_timer_start'

         IF (ll_timer .EQV. .TRUE.) THEN
         call prism_timer_c2i(timer_label,timer_id)
         if (timer_id < 0) then
            if (ntimer+1 > mtimer) then
                CALL oasis_pprintc(subname,2,':',char1=' WARNING timer number exceeded')
               return
            endif
            ntimer = ntimer + 1
            timer_id = ntimer
            timer(timer_id)%label = trim(timer_label)
         endif

         if (present(barrier)) then
            if (barrier) then
               call MPI_BARRIER(comm_timer, ierr)
            endif
         endif

         timer(timer_id)%start_wtime = MPI_WTIME()
         call cpu_time(cpu_time_arg)
         timer(timer_id)%start_ctime = cpu_time_arg
         count(timer_id) = count(timer_id) + 1
         timer(timer_id)%runflag = t_running
         ENDIF

      end subroutine prism_timer_start

! --------------------------------------------------------------------------------

      subroutine prism_timer_stop (timer_label)

         character(len=*), intent (in) :: timer_label
         real :: cpu_time_arg
         integer :: timer_id
         character(len=*),parameter :: subname = 'prism_timer_stop'

         IF (ll_timer .EQV. .TRUE.) THEN
         call prism_timer_c2i(timer_label,timer_id)
         if (timer_id < 0) then
             CALL oasis_pprintc(subname,2,' WARNING: timer_label does not exist ',char1=TRIM(timer_label))
            return
         endif

         if (timer(timer_id)%runflag == t_stopped) then
             CALL oasis_pprinti(subname,2,' WARNING timer_id not started ',int1=timer_id)
            return
         endif

         timer(timer_id)%end_wtime = MPI_WTIME()
         call cpu_time(cpu_time_arg)
         timer(timer_id)%end_ctime = cpu_time_arg

         sum_wtime(timer_id) = sum_wtime(timer_id) + &
                               timer(timer_id)%end_wtime - timer(timer_id)%start_wtime
         sum_ctime(timer_id) = sum_ctime(timer_id) + &
                               timer(timer_id)%end_ctime - timer(timer_id)%start_ctime
         timer(timer_id)%runflag = t_stopped
         ENDIF

      end subroutine prism_timer_stop

! --------------------------------------------------------------------------------

      subroutine prism_timer_print(timer_label)

         implicit none

         character(len=*), optional, intent(in) :: timer_label

         integer :: timer_id
         real, allocatable             :: sum_ctime_global_tmp(:,:)
         double precision, allocatable :: sum_wtime_global_tmp(:,:)
         integer, allocatable          :: count_global_tmp(:,:)
         character(len=ic_med), allocatable :: label_global_tmp(:,:)
         real, allocatable             :: sum_ctime_global(:,:)
         double precision, allocatable :: sum_wtime_global(:,:)
         integer, allocatable          :: count_global(:,:)
         double precision, allocatable :: rarr(:)
         integer, allocatable          :: iarr(:)
         character(len=ic_med), allocatable :: carr(:)
         character(len=ic_med), allocatable :: label_list(:)
         double precision   :: rval
         integer            :: ival
         character(len=ic_med) :: cval
         logical            :: onetimer
         logical            :: found
         integer, parameter :: root = 0
         integer            :: k, n, m
         integer            :: nlabels
         integer            :: ierror
         integer            :: ntimermax
         integer            :: pe1,pe2
         integer            :: minpe,maxpe,mcnt
         double precision   :: mintime,maxtime,meantime
         character(len=*),parameter :: subname = 'prism_timer_print'


         IF (ll_timer .EQV. .TRUE.) THEN
         onetimer = .false.
         if (present(timer_label)) then
            onetimer = .true.
            call prism_timer_c2i(timer_label,timer_id)
            if (timer_id < 1) then
                CALL oasis_pprintc(subname,2,' WARNING: invalid timer_label',char1=TRIM(timer_label))
               return
            endif
         endif

!-----------------------------------------------------
         if (onetimer) then

            open(output_unit, file=trim(file_name), form="FORMATTED", &
              status="UNKNOWN", position="APPEND")

            if (.not.single_timer_header) then
               write(output_unit,'(32x,2(2x,a,5x,a,6x,a,4x))') &
                    ' wtime ','on pe','count',' ctime ','on pe','count'
               single_timer_header = .true.
            endif
            n = timer_id
            WRITE(output_unit,'(1x,i3,2x,a24,a1,1x,2(f10.4,i8,i12,4x))') &
                  n, timer(n)%label, timer(n)%runflag, &
                  sum_wtime(n), comm_rank, count(n), &
                  sum_ctime(n), comm_rank, count(n)

            close(output_unit)
!----------
            return
!----------

         endif
!-----------------------------------------------------

         open(output_unit, file=trim(file_name), form="FORMATTED", &
           status="UNKNOWN", position="APPEND")
         write(output_unit,*)''
         write(output_unit,*)' =================================='
         write(output_unit,*)' ', trim(app_name)
         write(output_unit,*)' Local processor times '
         write(output_unit,*)' =================================='
         write(output_unit,*)''

         do n = 1,ntimer

            if (.not.single_timer_header) then
               write(output_unit,'(32x,2(2x,a,5x,a,6x,a,4x))') &
                    ' wtime ','on pe','count',' ctime ','on pe','count'
               single_timer_header = .true.
            endif
            WRITE(output_unit,'(1x,i3,2x,a24,a1,1x,2(f10.4,i8,i12,4x))') &
                  n, timer(n)%label, timer(n)%runflag, &
                  sum_wtime(n), comm_rank, count(n), &
                  sum_ctime(n), comm_rank, count(n)

         enddo

         close(output_unit)

         if (comm_size > 1) then

            call prism_mpi_max(ntimer,ntimermax,comm_timer,string='ntimer',all=.true.)

            allocate (sum_ctime_global_tmp(ntimermax, comm_size), &
                      sum_wtime_global_tmp(ntimermax, comm_size), stat=ierror)
            IF ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error sum_global_tmp')
            allocate (count_global_tmp(ntimermax, comm_size), stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error count_global_tmp')
            allocate (label_global_tmp(ntimermax, comm_size), stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error label_global_tmp')

            sum_ctime_global_tmp = 0.0
            sum_wtime_global_tmp = 0.0
            count_global_tmp = 0
            label_global_tmp = ' '

            ! gathering of timer values on root process

! tcraig, causes memory failure on corail for some reason
!            call MPI_Gather(sum_ctime(1), ntimermax, MPI_DOUBLE_PRECISION, sum_ctime_global_tmp(1,1), &
!                            ntimermax, MPI_DOUBLE_PRECISION, root, comm_timer, ierror)
!            call MPI_Gather(sum_wtime(1), ntimermax, MPI_DOUBLE_PRECISION, sum_wtime_global_tmp(1,1), &
!                            ntimermax, MPI_DOUBLE_PRECISION, root, comm_timer, ierror)
!            call MPI_Gather(count(1), ntimermax, MPI_INTEGER, count_global_tmp(1,1), &
!                            ntimermax, MPI_INTEGER, root, comm_timer, ierror)

! tcraig, this doesn't work either
!            allocate(rarr(ntimermax),stat=ierror)
!            if ( ierror /= 0 ) write(nulprt,*) subname,' WARNING: allocate error rarr'
!            rarr(1:ntimermax) = sum_ctime(1:ntimermax)
!            call MPI_Gather(rarr,ntimermax,MPI_DOUBLE_PRECISION,sum_ctime_global_tmp,ntimermax,MPI_DOUBLE_PRECISION,root,comm_timer,ierror)
!            rarr(1:ntimermax) = sum_wtime(1:ntimermax)
!            call MPI_Gather(rarr,ntimermax,MPI_DOUBLE_PRECISION,sum_wtime_global_tmp,ntimermax,MPI_DOUBLE_PRECISION,root,comm_timer,ierror)
!            deallocate(rarr,stat=ierror)
!            if ( ierror /= 0 ) write(nulprt,*) subname,' WARNING: deallocate error rarr'
!
!            allocate(iarr(ntimermax),stat=ierror)
!            if ( ierror /= 0 ) write(nulprt,*) subname,' WARNING: allocate error iarr'
!            iarr(1:ntimermax) = count(1:ntimermax)
!            call MPI_Gather(iarr,ntimermax,MPI_INTEGER,count_global_tmp,ntimermax,MPI_INTEGER,root,comm_timer,ierror)
!            deallocate(iarr,stat=ierror)
!            if ( ierror /= 0 ) write(nulprt,*) subname,' WARNING: deallocate error iarr'

! tcraig this works but requires lots of gather calls, could be better
            allocate(rarr(comm_size),iarr(comm_size),carr(comm_size),stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error rarr')
            do n = 1,ntimermax
               cval = timer(n)%label
               carr(:) = ' '
               call MPI_Gather(cval,len(cval),MPI_CHARACTER,carr(1),len(cval),MPI_CHARACTER,root,comm_timer,ierror)
               if (comm_rank == root) then
                  do m = 1,comm_size
                     label_global_tmp(n,m) = trim(carr(m))
                  enddo
               endif

               rval = sum_ctime(n)
               call MPI_Gather(rval,1,MPI_DOUBLE_PRECISION,rarr(1),1,MPI_DOUBLE_PRECISION,root,comm_timer,ierror)
               if (comm_rank == root) then
                  sum_ctime_global_tmp(n,1:comm_size) = rarr(1:comm_size)
               endif

               rval = sum_wtime(n)
               call MPI_Gather(rval,1,MPI_DOUBLE_PRECISION,rarr(1),1,MPI_DOUBLE_PRECISION,root,comm_timer,ierror)
               if (comm_rank == root) then
                  sum_wtime_global_tmp(n,1:comm_size) = rarr(1:comm_size)
               endif

               ival = count(n)
               call MPI_Gather(ival,1,MPI_INTEGER,iarr(1),1,MPI_INTEGER,root,comm_timer,ierror)
               if (comm_rank == root) then
                  count_global_tmp(n,1:comm_size) = iarr(1:comm_size)
               endif
            enddo
            deallocate(rarr,iarr,carr,stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: deallocate error rarr')

            ! now sort all the timers out by names

            allocate(carr(ntimermax*comm_size),stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error carr')
            nlabels = 0
            do n = 1,ntimermax
            do m = 1,comm_size
               found = .false.
               do k = 1,nlabels
                  if (trim(label_global_tmp(n,m)) == trim(carr(k))) found = .true.
                  if (trim(label_global_tmp(n,m)) == '') found = .false.
               enddo
               if (.not.found) then
                  nlabels = nlabels + 1
                  carr(nlabels) = trim(label_global_tmp(n,m))
               endif
            enddo
            enddo

            allocate(label_list(nlabels),stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error label_list')
            do k = 1,nlabels
               label_list(k) = trim(carr(k))
            enddo
            deallocate(carr,stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: deallocate error carr')
            allocate(sum_ctime_global(nlabels,comm_size),stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error sum_ctime_global')
            allocate(sum_wtime_global(nlabels,comm_size),stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error sum_wtime_global')
            allocate(count_global(nlabels,comm_size),stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error count_global')

            sum_ctime_global = 0
            sum_wtime_global = 0
            count_global = 0

            do k = 1,nlabels
            do m = 1,ntimermax
            do n = 1,comm_size
               if (trim(label_list(k)) == trim(label_global_tmp(m,n))) then
                  sum_ctime_global(k,n) = sum_ctime_global_tmp(m,n)
                  sum_wtime_global(k,n) = sum_wtime_global_tmp(m,n)
                  count_global(k,n) = count_global_tmp(m,n)
               endif
            enddo
            enddo
            enddo

            deallocate(label_global_tmp,stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: deallocate error label_global_tmp')
            deallocate(sum_ctime_global_tmp,stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: deallocate error sum_ctime_global_tmp')
            deallocate(sum_wtime_global_tmp,stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: deallocate error sum_wtime_global_tmp')
            deallocate(count_global_tmp,stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: deallocate error count_global')

         else
            nlabels = ntimer
            allocate(label_list(nlabels),stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error label_list')
            allocate(sum_ctime_global(nlabels,comm_size),stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error sum_ctime_global')
            allocate(sum_wtime_global(nlabels,comm_size),stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error sum_wtime_global')
            allocate(count_global(nlabels,comm_size),stat=ierror)
            if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: allocate error count_global')
            do k = 1,nlabels
               label_list(k) = timer(k)%label
            enddo
            sum_ctime_global(:,1) = sum_ctime(:)
            sum_wtime_global(:,1) = sum_wtime(:)
            count_global(:,1) = count(:)
         endif ! (comm_size > 1)

         ! if this is the root process
         if (comm_rank == root) then

         open(output_unit, file=trim(file_name), form="FORMATTED", &
              status="UNKNOWN", position="APPEND")

         if (onetimer) then

            if (.not.single_timer_header) then
               write(output_unit,'(32x,2(2x,a,5x,a,6x,a,4x))') &
                    'mintime','on pe','count','maxtime','on pe','count'
               single_timer_header = .true.
            endif
            n = 0
            do k = 1,nlabels
               if (trim(timer_label) == trim(label_list(k))) n = k
            enddo
            if (n < 1) then
                CALL oasis_pprintc(subname,2,' WARNING: invalid timer_label',char1=TRIM(timer_label))
               return
            endif
            mintime = sum_ctime_global(n,1)
            minpe = 1
            maxtime = sum_ctime_global(n,1)
            maxpe = 1
            do k = 1,comm_size
               if (sum_ctime_global(n,k) < mintime) then
                  mintime = sum_ctime_global(n,k)
                  minpe = k
               endif
               if (sum_ctime_global(n,k) > maxtime) then
                  maxtime = sum_ctime_global(n,k)
                  maxpe = k
               endif
            enddo
            WRITE(output_unit,'(1x,i3,2x,a24,a1,1x,2(f10.4,i8,i12,4x))') &
                  n, label_list(n), timer(n)%runflag, &
                  sum_ctime_global(n,minpe), minpe, count_global(n,minpe), &
                  sum_ctime_global(n,maxpe), maxpe, count_global(n,maxpe)

         else

            single_timer_header = .false.

            write(output_unit,*)''
            write(output_unit,*)' =================================='
            write(output_unit,*)' ', trim(app_name)
            write(output_unit,*)' Overall Elapsed Min/Max statistics'
            write(output_unit,*)' =================================='
            write(output_unit,*)''
            write(output_unit,'(32x,2(2x,a,5x,a,6x,a,4x),a,3x)') &
                 'mintime','on pe','count','maxtime','on pe','count','meantime'
            do n = 1,nlabels
               mintime = 1.0e36
               minpe = -1
               maxtime = -1.0e36
               maxpe = -1
               meantime = 0.0
               mcnt = 0
               do k = 1,comm_size
                  if (count_global(n,k) > 0) then
                     meantime = meantime + sum_wtime_global(n,k)
                     mcnt = mcnt + 1
                     if (sum_wtime_global(n,k) < mintime) then
                        mintime = sum_wtime_global(n,k)
                        minpe = k
                     endif
                     if (sum_wtime_global(n,k) > maxtime) then
                        maxtime = sum_wtime_global(n,k)
                        maxpe = k
                     endif
                  endif
               enddo
               if (mcnt > 0) meantime = meantime / float(mcnt)
               WRITE(output_unit,'(1x,i3,2x,a24,a1,1x,2(f10.4,i8,i12,4x),f10.4)') &
                     n, label_list(n), timer(n)%runflag, &
                     sum_wtime_global(n,minpe), minpe-1, count_global(n,minpe), &
                     sum_wtime_global(n,maxpe), maxpe-1, count_global(n,maxpe), &
                     meantime
            enddo
            write(output_unit,*)''
            write(output_unit,*)' =================================='
            write(output_unit,*)' ', trim(app_name)
            write(output_unit,*)' Overall Count statistics'
            write(output_unit,*)' =================================='
            write(output_unit,*)''
            write(output_unit,'(a)',advance="NO") " P r o c e s s o r s   ----------> "
            write(output_unit,'(8(3x,i2,5x))')(k-1,k=1,comm_size)
            do n = 1, nlabels

               WRITE(output_unit,'(1x,i3,2x,a24,a1,1x,(8i10))') n, label_list(n), timer(n)%runflag, &
                                                               (count_global(n,k),k=1,comm_size)
            enddo
            write(output_unit,*)''
            write(output_unit,*)' =================================='
            write(output_unit,*)' ', trim(app_name)
            write(output_unit,*)' Overall CPU time statistics'
            write(output_unit,*)' =================================='
            write(output_unit,*)''
            write(output_unit,'(a)',advance="NO") " P r o c e s s o r s   ----------> "
            write(output_unit,'(8(3x,i2,5x))')(k-1,k=1,comm_size)
            do n = 1, nlabels

               WRITE(output_unit,'(1x,i3,2x,a24,a1,1x,(8f10.4))') n, label_list(n), timer(n)%runflag, &
                                                               (sum_ctime_global(n,k),k=1,comm_size)
            enddo
            write(output_unit,*)''
            write(output_unit,*)' ======================================'
            write(output_unit,*)' ', trim(app_name)
            write(output_unit,*)' Overall Elapsed time statistics'
            write(output_unit,*)' ======================================'
            write(output_unit,*)''
            write(output_unit,'(a)',advance="NO") " P r o c e s s o r s   ----------> "
            write(output_unit,'(8(3x,i2,5x))')(k-1,k=1,comm_size)
            do n = 1, nlabels

               WRITE(output_unit,'(1x,i3,2x,a24,a1,1x,(8f10.4))') n, label_list(n), timer(n)%runflag, &
                                                               (sum_wtime_global(n,k),k=1,comm_size)
            enddo
            write(output_unit,*)''
            write(output_unit,*)' ======================================'

         endif ! (onetimer)

         close(output_unit)

         endif ! (comm_rank == root)

         deallocate (sum_ctime_global, stat=ierror)
         if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: deallocate error sum_ctime_global')
         deallocate (sum_wtime_global, stat=ierror)
         if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: deallocate error sum_wtime_global')
         deallocate (count_global,stat=ierror)
         if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: deallocate error count_global')
         deallocate (label_list,stat=ierror)
         if ( ierror /= 0 ) CALL oasis_pprintc(subname,2,':',char1=' WARNING: deallocate error label_list')
         ENDIF

      end subroutine prism_timer_print

! --------------------------------------------------------------------------------
      subroutine prism_timer_c2i(tname,tid)

         character(len=*),intent(in)  :: tname
         integer         ,intent(out) :: tid

         integer :: n

         IF (ll_timer .EQV. .TRUE.) THEN
         tid = -1
         do n = 1,ntimer
            if (trim(tname) == trim(timer(n)%label)) tid = n
         enddo
         ENDIF

      end subroutine prism_timer_c2i

! --------------------------------------------------------------------------------
end module mod_prism_timer
