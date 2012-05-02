module mod_prism_getput_interface
!---------------------------------------------------------------------

    use mod_prism_kinds
    use mod_prism_data
    USE mod_oasis_print
    use mod_prism_parameters
    use mod_prism_advance
    use mod_prism_var
    use mod_prism_sys
    use mct_mod

#include "psmile_os.h"

    integer(kind=ip_i4_p)     istatus(MPI_STATUS_SIZE)

  interface prism_put_proto
#ifndef __NO_4BYTE_REALS
     module procedure prism_put_proto_r14
     module procedure prism_put_proto_r24
#endif
     module procedure prism_put_proto_r18, &
                      prism_put_proto_r28
  end interface

  interface prism_get_proto
#ifndef __NO_4BYTE_REALS
     module procedure prism_get_proto_r14
     module procedure prism_get_proto_r24
#endif
     module procedure prism_get_proto_r18, &
                      prism_get_proto_r28
  end interface

!---------------------------------------------------------------------
contains
!---------------------------------------------------------------------

  subroutine prism_put_proto_r14(id_port_id,kstep,wr_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p), intent(inout) :: wr_field(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'prism_put_proto_r14'
    !-------------------------------------

    call prism_sys_debug_enter(subname)

    kinfo = PRISM_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       CALL oasis_pprintc(subname,15,' variable not coupled ',char1=trim(prism_var(nfld)%name))
       call prism_sys_debug_exit(subname)
       return
    endif

    ns = size(wr_field,dim=1)

    allocate(array(ns))

    n = 0
    do ni = 1,ns
       n = n + 1
       array(n) = wr_field(ni)
    enddo

    call prism_advance_run(PRISM_Out,nfld,kstep,array,kinfo)

    deallocate(array)

    call prism_sys_debug_exit(subname)

  end subroutine prism_put_proto_r14

!---------------------------------------------------------------------
  subroutine prism_put_proto_r18(id_port_id,kstep,wr_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p), intent(inout) :: wr_field(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'prism_put_proto_r18'
    !-------------------------------------

    call prism_sys_debug_enter(subname)

    kinfo = PRISM_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       CALL oasis_pprintc(subname,15,' variable not coupled ',char1=TRIM(prism_var(nfld)%name))
       call prism_sys_debug_exit(subname)
       return
    endif

    ns = size(wr_field,dim=1)

    allocate(array(ns))

    n = 0
    do ni = 1,ns
       n = n + 1
       array(n) = wr_field(ni)
    enddo

    call prism_advance_run(PRISM_Out,nfld,kstep,array,kinfo)

    deallocate(array)

    call prism_sys_debug_exit(subname)

  end subroutine prism_put_proto_r18

!---------------------------------------------------------------------
  subroutine prism_put_proto_r24(id_port_id,kstep,wr_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p), intent(inout) :: wr_field(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'prism_put_proto_r24'
    !-------------------------------------

    call prism_sys_debug_enter(subname)

    kinfo = PRISM_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       CALL oasis_pprintc(subname,15,' variable not coupled ',char1=TRIM(prism_var(nfld)%name))
       call prism_sys_debug_exit(subname)
       return
    endif

    nis = size(wr_field,dim=1)
    njs = size(wr_field,dim=2)
    ns = nis*njs

    allocate(array(ns))

    n = 0
    do nj = 1,njs
    do ni = 1,nis
       n = n + 1
       array(n) = wr_field(ni,nj)
    enddo
    enddo

    call prism_advance_run(PRISM_Out,nfld,kstep,array,kinfo)

    deallocate(array)

    call prism_sys_debug_exit(subname)

  end subroutine prism_put_proto_r24

!---------------------------------------------------------------------
  subroutine prism_put_proto_r28(id_port_id,kstep,wr_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p), intent(inout) :: wr_field(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'prism_put_proto_r28'
    !-------------------------------------

    call prism_sys_debug_enter(subname)

    kinfo = PRISM_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       CALL oasis_pprintc(subname,15,' variable not coupled ',char1=TRIM(prism_var(nfld)%name))
       call prism_sys_debug_exit(subname)
       return
    endif

    nis = size(wr_field,dim=1)
    njs = size(wr_field,dim=2)
    ns = nis*njs

    allocate(array(ns))

    n = 0
    do nj = 1,njs
    do ni = 1,nis
       n = n + 1
       array(n) = wr_field(ni,nj)
    enddo
    enddo

    call prism_advance_run(PRISM_Out,nfld,kstep,array,kinfo)

    deallocate(array)

    call prism_sys_debug_exit(subname)

  end subroutine prism_put_proto_r28

!-------------------------------------------------------------------
!---------------------------------------------------------------------
  subroutine prism_get_proto_r14(id_port_id,kstep,rd_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p), intent(inout) :: rd_field(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'prism_get_proto_r14'
    !-------------------------------------

    call prism_sys_debug_enter(subname)

    kinfo = PRISM_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       CALL oasis_pprintc(subname,15,' variable not coupled ',char1=TRIM(prism_var(nfld)%name))
       call prism_sys_debug_exit(subname)
       return
    endif

    ns = size(rd_field,dim=1)

    allocate(array(ns))

    call prism_advance_run(PRISM_In,nfld,kstep,array,kinfo)

    if (kinfo == PRISM_recvd) then
       n = 0
       do ni = 1,ns
          n = n + 1
          rd_field(ni) = array(n)
       enddo
    endif

    call prism_sys_debug_exit(subname)

  end subroutine prism_get_proto_r14

!---------------------------------------------------------------------
  subroutine prism_get_proto_r18(id_port_id,kstep,rd_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p), intent(inout) :: rd_field(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'prism_get_proto_r18'
    !-------------------------------------

    call prism_sys_debug_enter(subname)

    kinfo = PRISM_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       CALL oasis_pprintc(subname,15,' variable not coupled ',char1=TRIM(prism_var(nfld)%name))
       call prism_sys_debug_exit(subname)
       return
    endif

    ns = size(rd_field,dim=1)

    allocate(array(ns))

    call prism_advance_run(PRISM_In,nfld,kstep,array,kinfo)

    if (kinfo == PRISM_recvd) then
       n = 0
       do ni = 1,ns
          n = n + 1
          rd_field(ni) = array(n)
       enddo
    endif

    call prism_sys_debug_exit(subname)

  end subroutine prism_get_proto_r18

!---------------------------------------------------------------------
  subroutine prism_get_proto_r24(id_port_id,kstep,rd_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p), intent(inout) :: rd_field(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'prism_get_proto_r24'
    !-------------------------------------

    call prism_sys_debug_enter(subname)

    kinfo = PRISM_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       CALL oasis_pprintc(subname,15,' variable not coupled ',char1=TRIM(prism_var(nfld)%name))
       call prism_sys_debug_exit(subname)
       return
    endif

    nis = size(rd_field,dim=1)
    njs = size(rd_field,dim=2)
    ns = nis*njs

    allocate(array(ns))

    call prism_advance_run(PRISM_In,nfld,kstep,array,kinfo)

    if (kinfo == PRISM_recvd) then
       n = 0
       do nj = 1,njs
       do ni = 1,nis
          n = n + 1
          rd_field(ni,nj) = array(n)
       enddo
       enddo
    endif

    call prism_sys_debug_exit(subname)

  end subroutine prism_get_proto_r24

!---------------------------------------------------------------------
  subroutine prism_get_proto_r28(id_port_id,kstep,rd_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p), intent(inout) :: rd_field(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'prism_get_proto_r28'
    !-------------------------------------

    call prism_sys_debug_enter(subname)

    kinfo = PRISM_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       CALL oasis_pprintc(subname,15,' variable not coupled ',char1=TRIM(prism_var(nfld)%name))
       call prism_sys_debug_exit(subname)
       return
    endif

    nis = size(rd_field,dim=1)
    njs = size(rd_field,dim=2)
    ns = nis*njs

    allocate(array(ns))

    call prism_advance_run(PRISM_In,nfld,kstep,array,kinfo)

    if (kinfo == PRISM_recvd) then
       n = 0
       do nj = 1,njs
       do ni = 1,nis
          n = n + 1
          rd_field(ni,nj) = array(n)
       enddo
       enddo
    endif

    call prism_sys_debug_exit(subname)

  end subroutine prism_get_proto_r28

!-------------------------------------------------------------------

end module mod_prism_getput_interface

