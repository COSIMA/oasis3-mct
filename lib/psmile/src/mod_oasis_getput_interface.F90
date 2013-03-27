MODULE mod_oasis_getput_interface
!---------------------------------------------------------------------

    use mod_oasis_kinds
    use mod_oasis_data
    use mod_oasis_parameters
    use mod_oasis_advance
    use mod_oasis_var
    use mod_oasis_sys
    use mct_mod

    implicit none
    private

    public oasis_put
    public oasis_get

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
  SUBROUTINE oasis_put_r14(id_port_id,kstep,field_array1,kinfo, &
    field_array2, field_array3, field_array4, field_array5)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p), intent(in) :: field_array1(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    real(kind=ip_single_p), intent(in) , optional :: field_array2(:)
    real(kind=ip_single_p), intent(in) , optional :: field_array3(:)
    real(kind=ip_single_p), intent(in) , optional :: field_array4(:)
    real(kind=ip_single_p), intent(in) , optional :: field_array5(:)
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nsx
    integer(kind=ip_i4_p) :: n
    logical :: a2on, a3on, a4on, a5on
    real(kind=ip_r8_p), allocatable :: array1(:)
    real(kind=ip_r8_p), allocatable :: array2(:)
    real(kind=ip_r8_p), allocatable :: array3(:)
    real(kind=ip_r8_p), allocatable :: array4(:)
    real(kind=ip_r8_p), allocatable :: array5(:)
    character(len=*),parameter :: subname = 'oasis_put_r14'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_put is called for a variable not in namcouple: it will not be sent'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    ns = size(field_array1,dim=1)
    allocate(array1(ns))

    a2on = .false.
    a3on = .false.
    a4on = .false.
    a5on = .false.

    if (present(field_array2)) then
       a2on = .true.
       nsx = size(field_array2,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR array2 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array2(ns))
    endif

    if (present(field_array3)) then
       a3on = .true.
       nsx = size(field_array3,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR array3 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array3(ns))
    endif

    if (present(field_array4)) then
       a4on = .true.
       nsx = size(field_array4,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR array4 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array4(ns))
    endif

    if (present(field_array5)) then
       a5on = .true.
       nsx = size(field_array5,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR array5 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array5(ns))
    endif

    do n = 1,ns
       array1(n) = field_array1(n)
       if (a2on) array2(n) = field_array2(n)
       if (a3on) array3(n) = field_array3(n)
       if (a4on) array4(n) = field_array4(n)
       if (a5on) array5(n) = field_array5(n)
    enddo

    call oasis_advance_run(OASIS_Out,nfld,kstep,array1,kinfo, &
      a2on=a2on,array2=array2,a3on=a3on,array3=array3, &
      a4on=a4on,array4=array4,a5on=a5on,array5=array5)

    deallocate(array1)
    if (a2on) deallocate(array2)
    if (a3on) deallocate(array3)
    if (a4on) deallocate(array4)
    if (a5on) deallocate(array5)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r14

!-------------------------------------------------------------------
!---------------------------------------------------------------------
  SUBROUTINE oasis_put_r18(id_port_id,kstep,field_array1,kinfo, &
    field_array2, field_array3, field_array4, field_array5)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p), intent(in) :: field_array1(:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    real(kind=ip_double_p), intent(in) , optional :: field_array2(:)
    real(kind=ip_double_p), intent(in) , optional :: field_array3(:)
    real(kind=ip_double_p), intent(in) , optional :: field_array4(:)
    real(kind=ip_double_p), intent(in) , optional :: field_array5(:)
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nsx
    integer(kind=ip_i4_p) :: n
    logical :: a2on, a3on, a4on, a5on
    real(kind=ip_r8_p), allocatable :: array1(:)
    real(kind=ip_r8_p), allocatable :: array2(:)
    real(kind=ip_r8_p), allocatable :: array3(:)
    real(kind=ip_r8_p), allocatable :: array4(:)
    real(kind=ip_r8_p), allocatable :: array5(:)
    character(len=*),parameter :: subname = 'oasis_put_r18'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_put is called for a variable not in namcouple: it will not be sent'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    ns = size(field_array1,dim=1)
    allocate(array1(ns))

    a2on = .false.
    a3on = .false.
    a4on = .false.
    a5on = .false.

    if (present(field_array2)) then
       a2on = .true.
       nsx = size(field_array2,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR array2 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array2(ns))
    endif

    if (present(field_array3)) then
       a3on = .true.
       nsx = size(field_array3,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR array3 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array3(ns))
    endif

    if (present(field_array4)) then
       a4on = .true.
       nsx = size(field_array4,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR array4 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array4(ns))
    endif

    if (present(field_array5)) then
       a5on = .true.
       nsx = size(field_array5,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,' ERROR array5 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array5(ns))
    endif

    do n = 1,ns
       array1(n) = field_array1(n)
       if (a2on) array2(n) = field_array2(n)
       if (a3on) array3(n) = field_array3(n)
       if (a4on) array4(n) = field_array4(n)
       if (a5on) array5(n) = field_array5(n)
    enddo

    call oasis_advance_run(OASIS_Out,nfld,kstep,array1,kinfo, &
      a2on=a2on,array2=array2,a3on=a3on,array3=array3, &
      a4on=a4on,array4=array4,a5on=a5on,array5=array5)

    deallocate(array1)
    if (a2on) deallocate(array2)
    if (a3on) deallocate(array3)
    if (a4on) deallocate(array4)
    if (a5on) deallocate(array5)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r18

!-------------------------------------------------------------------
!---------------------------------------------------------------------
  SUBROUTINE oasis_put_r24(id_port_id,kstep,field_array1,kinfo, &
    field_array2, field_array3, field_array4, field_array5)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_single_p), intent(in) :: field_array1(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    real(kind=ip_single_p), intent(in) , optional :: field_array2(:,:)
    real(kind=ip_single_p), intent(in) , optional :: field_array3(:,:)
    real(kind=ip_single_p), intent(in) , optional :: field_array4(:,:)
    real(kind=ip_single_p), intent(in) , optional :: field_array5(:,:)
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs,nisx,njsx
    integer(kind=ip_i4_p) :: n,ni,nj
    logical :: a2on, a3on, a4on, a5on
    real(kind=ip_r8_p), allocatable :: array1(:)
    real(kind=ip_r8_p), allocatable :: array2(:)
    real(kind=ip_r8_p), allocatable :: array3(:)
    real(kind=ip_r8_p), allocatable :: array4(:)
    real(kind=ip_r8_p), allocatable :: array5(:)
    character(len=*),parameter :: subname = 'oasis_put_r24'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_put is called for a variable not in namcouple: it will not be sent'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    nis = size(field_array1,dim=1)
    njs = size(field_array1,dim=2)
    ns = nis*njs
    allocate(array1(ns))

    a2on = .false.
    a3on = .false.
    a4on = .false.
    a5on = .false.

    if (present(field_array2)) then
       a2on = .true.
       nisx = size(field_array2,dim=1)
       njsx = size(field_array2,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR array2 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array2(ns))
    endif

    if (present(field_array3)) then
       a3on = .true.
       nisx = size(field_array3,dim=1)
       njsx = size(field_array3,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR array3 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array3(ns))
    endif

    if (present(field_array4)) then
       a4on = .true.
       nisx = size(field_array4,dim=1)
       njsx = size(field_array4,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR array4 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array4(ns))
    endif

    if (present(field_array5)) then
       a5on = .true.
       nisx = size(field_array5,dim=1)
       njsx = size(field_array5,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR array5 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array5(ns))
    endif

    n = 0
    do nj = 1,njs
    do ni = 1,nis
       n = n + 1
       array1(n) = field_array1(ni,nj)
       if (a2on) array2(n) = field_array2(ni,nj)
       if (a3on) array3(n) = field_array3(ni,nj)
       if (a4on) array4(n) = field_array4(ni,nj)
       if (a5on) array5(n) = field_array5(ni,nj)
    enddo
    enddo

    call oasis_advance_run(OASIS_Out,nfld,kstep,array1,kinfo, &
      a2on=a2on,array2=array2,a3on=a3on,array3=array3, &
      a4on=a4on,array4=array4,a5on=a5on,array5=array5)

    deallocate(array1)
    if (a2on) deallocate(array2)
    if (a3on) deallocate(array3)
    if (a4on) deallocate(array4)
    if (a5on) deallocate(array5)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r24

!-------------------------------------------------------------------
!---------------------------------------------------------------------
  SUBROUTINE oasis_put_r28(id_port_id,kstep,field_array1,kinfo, &
    field_array2, field_array3, field_array4, field_array5)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: id_port_id,kstep
    real(kind=ip_double_p), intent(in) :: field_array1(:,:)
    integer(kind=ip_i4_p) , intent(out), optional :: kinfo
    real(kind=ip_double_p), intent(in) , optional :: field_array2(:,:)
    real(kind=ip_double_p), intent(in) , optional :: field_array3(:,:)
    real(kind=ip_double_p), intent(in) , optional :: field_array4(:,:)
    real(kind=ip_double_p), intent(in) , optional :: field_array5(:,:)
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nis,njs,nisx,njsx
    integer(kind=ip_i4_p) :: n,ni,nj
    logical :: a2on, a3on, a4on, a5on
    real(kind=ip_r8_p), allocatable :: array1(:)
    real(kind=ip_r8_p), allocatable :: array2(:)
    real(kind=ip_r8_p), allocatable :: array3(:)
    real(kind=ip_r8_p), allocatable :: array4(:)
    real(kind=ip_r8_p), allocatable :: array5(:)
    character(len=*),parameter :: subname = 'oasis_put_r28'
    !-------------------------------------

    call oasis_debug_enter(subname)

    kinfo = OASIS_OK

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_put is called for a variable not in namcouple: it will not be sent'
       call oasis_abort_noarg()
       return
    endif

    nfld = id_port_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    nis = size(field_array1,dim=1)
    njs = size(field_array1,dim=2)
    ns = nis*njs
    allocate(array1(ns))

    a2on = .false.
    a3on = .false.
    a4on = .false.
    a5on = .false.

    if (present(field_array2)) then
       a2on = .true.
       nisx = size(field_array2,dim=1)
       njsx = size(field_array2,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR array2 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array2(ns))
    endif

    if (present(field_array3)) then
       a3on = .true.
       nisx = size(field_array3,dim=1)
       njsx = size(field_array3,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR array3 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array3(ns))
    endif

    if (present(field_array4)) then
       a4on = .true.
       nisx = size(field_array4,dim=1)
       njsx = size(field_array4,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR array4 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array4(ns))
    endif

    if (present(field_array5)) then
       a5on = .true.
       nisx = size(field_array5,dim=1)
       njsx = size(field_array5,dim=2)
       if (nisx /= nis .or. njsx /= njs) then
          write(nulprt,*) subname,' ERROR array5 size does not match array ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_flush(nulprt)
          CALL oasis_abort_noarg()
       endif
       allocate(array5(ns))
    endif

    n = 0
    do nj = 1,njs
    do ni = 1,nis
       n = n + 1
       array1(n) = field_array1(ni,nj)
       if (a2on) array2(n) = field_array2(ni,nj)
       if (a3on) array3(n) = field_array3(ni,nj)
       if (a4on) array4(n) = field_array4(ni,nj)
       if (a5on) array5(n) = field_array5(ni,nj)
    enddo
    enddo

    call oasis_advance_run(OASIS_Out,nfld,kstep,array1,kinfo, &
      a2on=a2on,array2=array2,a3on=a3on,array3=array3, &
      a4on=a4on,array4=array4,a5on=a5on,array5=array5)

    deallocate(array1)
    if (a2on) deallocate(array2)
    if (a3on) deallocate(array3)
    if (a4on) deallocate(array4)
    if (a5on) deallocate(array5)

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

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_get is called for variable not in namcouple; it will not be received'
       if (OASIS_debug >= 1) write(nulprt,*) subname,' BE CAREFUL NOT TO USE IT !!!!!'
       call oasis_abort_noarg()
       return
    endif

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

    deallocate(array)
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

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_get is called for variable not in namcouple; it will not be received'
       if (OASIS_debug >= 1) write(nulprt,*) subname,' BE CAREFUL NOT TO USE IT !!!!!'
       call oasis_abort_noarg()
       return
    endif

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

    deallocate(array)
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

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_get is called for variable not in namcouple; it will not be received'
       if (OASIS_debug >= 1) write(nulprt,*) subname,' BE CAREFUL NOT TO USE IT !!!!!'
       call oasis_abort_noarg()
       return
    endif

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

    deallocate(array)
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

    if (id_port_id == OASIS_Var_Uncpl) then
       if (OASIS_debug >= 1) write(nulprt,*) subname, &
          ' Routine oasis_get is called for variable not in namcouple; it will not be received'
       if (OASIS_debug >= 1) write(nulprt,*) subname,' BE CAREFUL NOT TO USE IT !!!!!'
       call oasis_abort_noarg()
       return
    endif

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

    deallocate(array)
    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r28

!-------------------------------------------------------------------

END MODULE mod_oasis_getput_interface

