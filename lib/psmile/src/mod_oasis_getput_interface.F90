MODULE mod_oasis_getput_interface
!---------------------------------------------------------------------

    use mod_oasis_kinds
    use mod_oasis_data
    use mod_oasis_parameters
    use mod_oasis_advance
    use mod_oasis_var
    use mod_oasis_sys
    use mct_mod

#include "oasis_os.h"

    integer(kind=ip_i4_p)     istatus(MPI_STATUS_SIZE)

  interface oasis_put
#ifndef __NO_4BYTE_REALS
     module procedure oasis_put_r14
     module procedure oasis_put_r24
#endif
     module procedure oasis_put_r18
     module procedure oasis_put_r28
  end interface

  interface oasis_get
#ifndef __NO_4BYTE_REALS
     module procedure oasis_get_r14
     module procedure oasis_get_r24
#endif
     module procedure oasis_get_r18
     module procedure oasis_get_r28
  end interface

!---------------------------------------------------------------------
contains
!---------------------------------------------------------------------

  SUBROUTINE oasis_put_r14(id_port_id,kstep,wr_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p), intent(in) :: wr_field(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'oasis_put_r14'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    ns = size(wr_field,dim=1)

    allocate(array(ns))

    n = 0
    do ni = 1,ns
       n = n + 1
       array(n) = wr_field(ni)
    enddo

    call oasis_advance_run(OASIS_Out,nfld,kstep,array,kinfo)

    deallocate(array)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r14

!---------------------------------------------------------------------
  SUBROUTINE oasis_put_r18(id_port_id,kstep,wr_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p), intent(in) :: wr_field(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'oasis_put_r18'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    ns = size(wr_field,dim=1)

    allocate(array(ns))

    n = 0
    do ni = 1,ns
       n = n + 1
       array(n) = wr_field(ni)
    enddo

    call oasis_advance_run(OASIS_Out,nfld,kstep,array,kinfo)

    deallocate(array)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r18

!---------------------------------------------------------------------
  SUBROUTINE oasis_put_r24(id_port_id,kstep,wr_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p), intent(in) :: wr_field(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'oasis_put_r24'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
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

    call oasis_advance_run(OASIS_Out,nfld,kstep,array,kinfo)

    deallocate(array)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r24

!---------------------------------------------------------------------
  SUBROUTINE oasis_put_r28(id_port_id,kstep,wr_field,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p), intent(in) :: wr_field(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs
    integer(kind=ip_i4_p) :: n,ni,nj
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = 'oasis_put_r28'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
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

    call oasis_advance_run(OASIS_Out,nfld,kstep,array,kinfo)

    deallocate(array)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r28

!-------------------------------------------------------------------
!---------------------------------------------------------------------
  SUBROUTINE oasis_get_r14(id_port_id,kstep,rd_field,kinfo)

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
    character(len=*),parameter :: subname = 'oasis_get_r14'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    ns = size(rd_field,dim=1)

    allocate(array(ns))

    call oasis_advance_run(OASIS_In,nfld,kstep,array,kinfo)

    if (kinfo /= OASIS_OK) then
       n = 0
       do ni = 1,ns
          n = n + 1
          rd_field(ni) = array(n)
       enddo
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r14

!---------------------------------------------------------------------
  SUBROUTINE oasis_get_r18(id_port_id,kstep,rd_field,kinfo)

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
    character(len=*),parameter :: subname = 'oasis_get_r18'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    ns = size(rd_field,dim=1)

    allocate(array(ns))

    call oasis_advance_run(OASIS_In,nfld,kstep,array,kinfo)

    if (kinfo /= OASIS_OK) then
       n = 0
       do ni = 1,ns
          n = n + 1
          rd_field(ni) = array(n)
       enddo
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r18

!---------------------------------------------------------------------
  SUBROUTINE oasis_get_r24(id_port_id,kstep,rd_field,kinfo)

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
    character(len=*),parameter :: subname = 'oasis_get_r24'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    nis = size(rd_field,dim=1)
    njs = size(rd_field,dim=2)
    ns = nis*njs

    allocate(array(ns))

    call oasis_advance_run(OASIS_In,nfld,kstep,array,kinfo)

    if (kinfo /= OASIS_OK) then
       n = 0
       do nj = 1,njs
       do ni = 1,nis
          n = n + 1
          rd_field(ni,nj) = array(n)
       enddo
       enddo
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r24

!---------------------------------------------------------------------
  SUBROUTINE oasis_get_r28(id_port_id,kstep,rd_field,kinfo)

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
    character(len=*),parameter :: subname = 'oasis_get_r28'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    nis = size(rd_field,dim=1)
    njs = size(rd_field,dim=2)
    ns = nis*njs

    allocate(array(ns))

    call oasis_advance_run(OASIS_In,nfld,kstep,array,kinfo)

    if (kinfo /= OASIS_OK) then
       n = 0
       do nj = 1,njs
       do ni = 1,nis
          n = n + 1
          rd_field(ni,nj) = array(n)
       enddo
       enddo
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r28

!-------------------------------------------------------------------

END MODULE mod_oasis_getput_interface

