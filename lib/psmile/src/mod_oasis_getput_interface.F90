
!> OASIS send/receive (put/get) user interfaces

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

!> Generic overloaded interface for data put (send)
  interface oasis_put
#ifndef __NO_4BYTE_REALS
     module procedure oasis_put_r14
     module procedure oasis_put_r24
     module procedure oasis_put_r34
#endif
     module procedure oasis_put_r18
     module procedure oasis_put_r28
     module procedure oasis_put_r38
  end interface

!> Generic overloaded interface for data get (receive)
  interface oasis_get
#ifndef __NO_4BYTE_REALS
     module procedure oasis_get_r14
     module procedure oasis_get_r24
     module procedure oasis_get_r34
#endif
     module procedure oasis_get_r18
     module procedure oasis_get_r28
     module procedure oasis_get_r38
  end interface

!---------------------------------------------------------------------
contains
!---------------------------------------------------------------------
#ifndef __NO_4BYTE_REALS

!> Send 4 byte real 1D data

  SUBROUTINE oasis_put_r14(var_id,kstep,fld1,kinfo, &
    fld2, fld3, fld4, fld5, write_restart)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id      !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep       !< model time in seconds
    real(kind=ip_single_p)             :: fld1(:)     !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo       !< return code
    real(kind=ip_single_p), optional :: fld2(:)       !< higher order field data
    real(kind=ip_single_p), optional :: fld3(:)       !< higher order field data
    real(kind=ip_single_p), optional :: fld4(:)       !< higher order field data
    real(kind=ip_single_p), optional :: fld5(:)       !< higher order field data
    logical               , optional :: write_restart !< write restart now
    !-------------------------------------
    logical :: lwrst
    character(len=*),parameter :: subname = '(oasis_put_r14)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (prism_var(var_id)%num > 1) then
       write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
       write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
       call oasis_abort()
    endif

    if (present(write_restart)) then
       lwrst = write_restart
    else
       lwrst = .false.
    endif

    if (present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
       CALL oasis_put_worker(var_id,kstep,DBLE(fld1),kinfo,DBLE(fld2), &
            DBLE(fld3),DBLE(fld4),DBLE(fld5),write_restart=lwrst)
    elseif (.not.present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
       CALL oasis_put_worker(var_id,kstep,DBLE(fld1),kinfo,DBLE(fld2), &
            DBLE(fld3),DBLE(fld4),write_restart=lwrst)
    elseif (.not.present(fld5) .and. .not.present(fld4) .and. present(fld3) .and. present(fld2)) then
       CALL oasis_put_worker(var_id,kstep,DBLE(fld1),kinfo,DBLE(fld2), &
            DBLE(fld3),write_restart=lwrst)
    elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. present(fld2)) then
       CALL oasis_put_worker(var_id,kstep,DBLE(fld1),kinfo,DBLE(fld2), &
            write_restart=lwrst)
    elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. .not.present(fld2)) then
       CALL oasis_put_worker(var_id,kstep,DBLE(fld1),kinfo,write_restart=lwrst)
    else
       WRITE(nulprt,*) subname,estr,' Wrong field array argument list in oasis_put'
       CALL oasis_abort()
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r14
#endif

!-------------------------------------------------------------------

!> Send 4 byte real 1D data

  SUBROUTINE oasis_put_r18(var_id,kstep,fld1,kinfo, &
    fld2, fld3, fld4, fld5, write_restart)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id      !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep       !< model time in seconds
    real(kind=ip_double_p)             :: fld1(:)     !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo       !< return code
    real(kind=ip_double_p), optional :: fld2(:)       !< higher order field data
    real(kind=ip_double_p), optional :: fld3(:)       !< higher order field data
    real(kind=ip_double_p), optional :: fld4(:)       !< higher order field data
    real(kind=ip_double_p), optional :: fld5(:)       !< higher order field data
    logical               , optional :: write_restart !< write restart now
    !-------------------------------------
    logical :: lwrst
    character(len=*),parameter :: subname = '(oasis_put_r18)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (prism_var(var_id)%num > 1) then
       write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
       write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
       call oasis_abort()
    endif

    if (present(write_restart)) then
       lwrst = write_restart
    else
       lwrst = .false.
    endif

    if (present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
       CALL oasis_put_worker(var_id,kstep,fld1,kinfo,fld2, &
            fld3,fld4,fld5,write_restart=lwrst)
    elseif (.not.present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
       CALL oasis_put_worker(var_id,kstep,fld1,kinfo,fld2, &
            fld3,fld4,write_restart=lwrst)
    elseif (.not.present(fld5) .and. .not.present(fld4) .and. present(fld3) .and. present(fld2)) then
       CALL oasis_put_worker(var_id,kstep,fld1,kinfo,fld2, &
            fld3,write_restart=lwrst)
    elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. present(fld2)) then
       CALL oasis_put_worker(var_id,kstep,fld1,kinfo,fld2, &
            write_restart=lwrst)
    elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. .not.present(fld2)) then
       CALL oasis_put_worker(var_id,kstep,fld1,kinfo,write_restart=lwrst)
    else
       WRITE(nulprt,*) subname,estr,' Wrong field array argument list in oasis_put'
       CALL oasis_abort()
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r18

!---------------------------------------------------------------------
#ifndef __NO_4BYTE_REALS

!> Send 4 byte real 2D data

  SUBROUTINE oasis_put_r24(var_id,kstep,fld1,kinfo, &
    fld2, fld3, fld4, fld5, write_restart)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id      !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep       !< model time in seconds
    real(kind=ip_single_p) :: fld1(:,:)               !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo       !< return code
    real(kind=ip_single_p), optional :: fld2(:,:)     !< higher order field data
    real(kind=ip_single_p), optional :: fld3(:,:)     !< higher order field data
    real(kind=ip_single_p), optional :: fld4(:,:)     !< higher order field data
    real(kind=ip_single_p), optional :: fld5(:,:)     !< higher order field data
    logical               , optional :: write_restart !< write restart now
    !-------------------------------------
    logical :: lwrst
    integer(kind=ip_i4_p) :: n, size_fld1
    character(len=*),parameter :: subname = '(oasis_put_r24)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (present(write_restart)) then
       lwrst = write_restart
    else
       lwrst = .false.
    endif

    if (prism_var(var_id)%num > 1) then
    ! treat data as 1d bundled data

       if (size(fld1,dim=2) /= prism_var(var_id)%num) then
          write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
          write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
          write(nulprt,*) subname,estr,'passing in field with incorrect 2nd dim size = ',size(fld1,dim=2)
          call oasis_abort()
       endif

       size_fld1 = size(fld1,dim=2)
       if (present(fld2)) then
          if (size(fld2,dim=2) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld2 size different than fld1 size ',size_fld1,size(fld2,dim=2)
             call oasis_abort()
          endif
       endif
       if (present(fld3)) then
          if (size(fld3,dim=2) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld3 size different than fld1 size ',size_fld1,size(fld3,dim=2)
             call oasis_abort()
          endif
       endif
       if (present(fld4)) then
          if (size(fld4,dim=2) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld4 size different than fld1 size ',size_fld1,size(fld4,dim=2)
             call oasis_abort()
          endif
       endif
       if (present(fld5)) then
          if (size(fld5,dim=2) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld5 size different than fld1 size ',size_fld1,size(fld5,dim=2)
             call oasis_abort()
          endif
       endif

       do n = 1,prism_var(var_id)%num
          if (present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,DBLE(fld1(:,n)),kinfo,DBLE(fld2(:,n)), &
                  DBLE(fld3(:,n)),DBLE(fld4(:,n)),DBLE(fld5(:,n)),write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,DBLE(fld1(:,n)),kinfo,DBLE(fld2(:,n)), &
                  DBLE(fld3(:,n)),DBLE(fld4(:,n)),write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,DBLE(fld1(:,n)),kinfo,DBLE(fld2(:,n)), &
                  DBLE(fld3(:,n)),write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,DBLE(fld1(:,n)),kinfo,DBLE(fld2(:,n)), &
                  write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. .not.present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,DBLE(fld1(:,n)),kinfo,write_restart=lwrst,varnum=n)
          else
             WRITE(nulprt,*) subname,estr,' Wrong field array argument list in oasis_put'
             CALL oasis_abort()
          endif
       enddo

    else
    ! treat data as 2d unbundled data

       if (present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
          CALL oasis_put_worker(var_id,kstep,DBLE(PACK(fld1,mask=.true.)),kinfo,DBLE(PACK(fld2,mask=.true.)), &
               DBLE(PACK(fld3,mask=.true.)),DBLE(PACK(fld4,mask=.true.)),DBLE(PACK(fld5,mask=.true.)),write_restart=lwrst)
       elseif (.not.present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
          CALL oasis_put_worker(var_id,kstep,DBLE(PACK(fld1,mask=.true.)),kinfo,DBLE(PACK(fld2,mask=.true.)), &
               DBLE(PACK(fld3,mask=.true.)),DBLE(PACK(fld4,mask=.true.)),write_restart=lwrst)
       elseif (.not.present(fld5) .and. .not.present(fld4) .and. present(fld3) .and. present(fld2)) then
          CALL oasis_put_worker(var_id,kstep,DBLE(PACK(fld1,mask=.true.)),kinfo,DBLE(PACK(fld2,mask=.true.)), &
               DBLE(PACK(fld3,mask=.true.)),write_restart=lwrst)
       elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. present(fld2)) then
          CALL oasis_put_worker(var_id,kstep,DBLE(PACK(fld1,mask=.true.)),kinfo,DBLE(PACK(fld2,mask=.true.)), &
               write_restart=lwrst)
       elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. .not.present(fld2)) then
          CALL oasis_put_worker(var_id,kstep,DBLE(PACK(fld1,mask=.true.)),kinfo,write_restart=lwrst)
       else
          WRITE(nulprt,*) subname,estr,' Wrong field array argument list in oasis_put'
          CALL oasis_abort()
       endif

    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r24
#endif

!---------------------------------------------------------------------
#ifndef __NO_4BYTE_REALS

!> Send 4 byte real 2D bundled data

  SUBROUTINE oasis_put_r34(var_id,kstep,fld1,kinfo, &
    fld2, fld3, fld4, fld5, write_restart)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id      !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep       !< model time in seconds
    real(kind=ip_single_p) :: fld1(:,:,:)             !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo       !< return code
    real(kind=ip_single_p), optional :: fld2(:,:,:)   !< higher order field data
    real(kind=ip_single_p), optional :: fld3(:,:,:)   !< higher order field data
    real(kind=ip_single_p), optional :: fld4(:,:,:)   !< higher order field data
    real(kind=ip_single_p), optional :: fld5(:,:,:)   !< higher order field data
    logical               , optional :: write_restart !< write restart now
    !-------------------------------------
    logical :: lwrst
    integer(kind=ip_i4_p) :: n, size_fld1
    character(len=*),parameter :: subname = '(oasis_put_r34)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (present(write_restart)) then
       lwrst = write_restart
    else
       lwrst = .false.
    endif

    if (prism_var(var_id)%num > 1) then
    ! treat data as 2d bundled data

       if (size(fld1,dim=3) /= prism_var(var_id)%num) then
          write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
          write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
          write(nulprt,*) subname,estr,'passing in field with incorrect 3rd dim size = ',size(fld1,dim=3)
          call oasis_abort()
       endif

       size_fld1 = size(fld1,dim=3)
       if (present(fld2)) then
          if (size(fld2,dim=3) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld2 size different than fld1 size ',size_fld1,size(fld2,dim=3)
             call oasis_abort()
          endif
       endif
       if (present(fld3)) then
          if (size(fld3,dim=3) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld3 size different than fld1 size ',size_fld1,size(fld3,dim=3)
             call oasis_abort()
          endif
       endif
       if (present(fld4)) then
          if (size(fld4,dim=3) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld4 size different than fld1 size ',size_fld1,size(fld4,dim=3)
             call oasis_abort()
          endif
       endif
       if (present(fld5)) then
          if (size(fld5,dim=3) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld5 size different than fld1 size ',size_fld1,size(fld5,dim=3)
             call oasis_abort()
          endif
       endif

       do n = 1,prism_var(var_id)%num
          if (present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,DBLE(PACK(fld1(:,:,n),mask=.true.)),kinfo,DBLE(PACK(fld2(:,:,n),mask=.true.)), &
                  DBLE(PACK(fld3(:,:,n),mask=.true.)),DBLE(PACK(fld4(:,:,n),mask=.true.)),DBLE(PACK(fld5(:,:,n),mask=.true.)), &
                  write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,DBLE(PACK(fld1(:,:,n),mask=.true.)),kinfo,DBLE(PACK(fld2(:,:,n),mask=.true.)), &
                  DBLE(PACK(fld3(:,:,n),mask=.true.)),DBLE(PACK(fld4(:,:,n),mask=.true.)),write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,DBLE(PACK(fld1(:,:,n),mask=.true.)),kinfo,DBLE(PACK(fld2(:,:,n),mask=.true.)), &
                  DBLE(PACK(fld3(:,:,n),mask=.true.)),write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,DBLE(PACK(fld1(:,:,n),mask=.true.)),kinfo,DBLE(PACK(fld2(:,:,n),mask=.true.)), &
                  write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. .not.present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,DBLE(PACK(fld1(:,:,n),mask=.true.)),kinfo,write_restart=lwrst,varnum=n)
          else
             WRITE(nulprt,*) subname,estr,' Wrong field array argument list in oasis_put'
             CALL oasis_abort()
          endif
       enddo

    else
       WRITE(nulprt,*) subname,estr,' Dimension sizes incorrect'
       CALL oasis_abort()

    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r34
#endif
!-------------------------------------------------------------------
!---------------------------------------------------------------------

!> Send 8 byte real 2D data

  SUBROUTINE oasis_put_r28(var_id,kstep,fld1,kinfo, &
    fld2, fld3, fld4, fld5, write_restart)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id      !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep       !< model time in seconds
    real(kind=ip_double_p) :: fld1(:,:)               !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo       !< return code
    real(kind=ip_double_p), optional :: fld2(:,:)     !< higher order field data
    real(kind=ip_double_p), optional :: fld3(:,:)     !< higher order field data
    real(kind=ip_double_p), optional :: fld4(:,:)     !< higher order field data
    real(kind=ip_double_p), optional :: fld5(:,:)     !< higher order field data
    logical               , optional :: write_restart !< write restart now
    !-------------------------------------
    logical :: lwrst
    integer(kind=ip_i4_p) :: n, size_fld1
    character(len=*),parameter :: subname = '(oasis_put_r28)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (present(write_restart)) then
       lwrst = write_restart
    else
       lwrst = .false.
    endif

    if (prism_var(var_id)%num > 1) then
    ! treat data as 1d bundled data

       if (size(fld1,dim=2) /= prism_var(var_id)%num) then
          write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
          write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
          write(nulprt,*) subname,estr,'passing in field with incorrect 2nd dim size = ',size(fld1,dim=2)
          call oasis_abort()
       endif

       size_fld1 = size(fld1,dim=2)
       if (present(fld2)) then
          if (size(fld2,dim=2) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld2 size different than fld1 size ',size_fld1,size(fld2,dim=2)
             call oasis_abort()
          endif
       endif
       if (present(fld3)) then
          if (size(fld3,dim=2) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld3 size different than fld1 size ',size_fld1,size(fld3,dim=2)
             call oasis_abort()
          endif
       endif
       if (present(fld4)) then
          if (size(fld4,dim=2) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld4 size different than fld1 size ',size_fld1,size(fld4,dim=2)
             call oasis_abort()
          endif
       endif
       if (present(fld5)) then
          if (size(fld5,dim=2) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld5 size different than fld1 size ',size_fld1,size(fld5,dim=2)
             call oasis_abort()
          endif
       endif

       do n = 1,prism_var(var_id)%num
          if (present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,(fld1(:,n)),kinfo,(fld2(:,n)), &
                  (fld3(:,n)),(fld4(:,n)),(fld5(:,n)),write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,(fld1(:,n)),kinfo,(fld2(:,n)), &
                  (fld3(:,n)),(fld4(:,n)),write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,(fld1(:,n)),kinfo,(fld2(:,n)), &
                  (fld3(:,n)),write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,(fld1(:,n)),kinfo,(fld2(:,n)), &
                  write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. .not.present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,(fld1(:,n)),kinfo,write_restart=lwrst,varnum=n)
          else
             WRITE(nulprt,*) subname,estr,' Wrong field array argument list in oasis_put'
             CALL oasis_abort()
          endif
       enddo

    else
    ! treat data as 2d unbundled data

       if (present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
          CALL oasis_put_worker(var_id,kstep,PACK(fld1,mask=.true.),kinfo,PACK(fld2,mask=.true.), &
               PACK(fld3,mask=.true.),PACK(fld4,mask=.true.),PACK(fld5,mask=.true.),write_restart=lwrst)
       elseif (.not.present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
          CALL oasis_put_worker(var_id,kstep,PACK(fld1,mask=.true.),kinfo,PACK(fld2,mask=.true.), &
               PACK(fld3,mask=.true.),PACK(fld4,mask=.true.),write_restart=lwrst)
       elseif (.not.present(fld5) .and. .not.present(fld4) .and. present(fld3) .and. present(fld2)) then
          CALL oasis_put_worker(var_id,kstep,PACK(fld1,mask=.true.),kinfo,PACK(fld2,mask=.true.), &
               PACK(fld3,mask=.true.),write_restart=lwrst)
       elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. present(fld2)) then
          CALL oasis_put_worker(var_id,kstep,PACK(fld1,mask=.true.),kinfo,PACK(fld2,mask=.true.), &
               write_restart=lwrst)
       elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. .not.present(fld2)) then
          CALL oasis_put_worker(var_id,kstep,PACK(fld1,mask=.true.),kinfo,write_restart=lwrst)
       else
          WRITE(nulprt,*) subname,estr,' Wrong field array argument list in oasis_put'
          CALL oasis_abort()
       endif

    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r28

!-------------------------------------------------------------------
!---------------------------------------------------------------------

!> Send 8 byte real 2D bundled data

  SUBROUTINE oasis_put_r38(var_id,kstep,fld1,kinfo, &
    fld2, fld3, fld4, fld5, write_restart)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id      !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep       !< model time in seconds
    real(kind=ip_double_p) :: fld1(:,:,:)             !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo       !< return code
    real(kind=ip_double_p), optional :: fld2(:,:,:)   !< higher order field data
    real(kind=ip_double_p), optional :: fld3(:,:,:)   !< higher order field data
    real(kind=ip_double_p), optional :: fld4(:,:,:)   !< higher order field data
    real(kind=ip_double_p), optional :: fld5(:,:,:)   !< higher order field data
    logical               , optional :: write_restart !< write restart now
    !-------------------------------------
    logical :: lwrst
    integer(kind=ip_i4_p) :: n, size_fld1
    character(len=*),parameter :: subname = '(oasis_put_r38)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (present(write_restart)) then
       lwrst = write_restart
    else
       lwrst = .false.
    endif

    if (prism_var(var_id)%num > 1) then
    ! treat data as 1d bundled data

       if (size(fld1,dim=3) /= prism_var(var_id)%num) then
          write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
          write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
          write(nulprt,*) subname,estr,'passing in field with incorrect 3rd dim size = ',size(fld1,dim=3)
          call oasis_abort()
       endif

       size_fld1 = size(fld1,dim=3)
       if (present(fld2)) then
          if (size(fld2,dim=3) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld2 size different than fld1 size ',size_fld1,size(fld2,dim=3)
             call oasis_abort()
          endif
       endif
       if (present(fld3)) then
          if (size(fld3,dim=3) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld3 size different than fld1 size ',size_fld1,size(fld3,dim=3)
             call oasis_abort()
          endif
       endif
       if (present(fld4)) then
          if (size(fld4,dim=3) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld4 size different than fld1 size ',size_fld1,size(fld4,dim=3)
             call oasis_abort()
          endif
       endif
       if (present(fld5)) then
          if (size(fld5,dim=3) /= size_fld1) then
             write(nulprt,*) subname,estr,'fld5 size different than fld1 size ',size_fld1,size(fld5,dim=3)
             call oasis_abort()
          endif
       endif

       do n = 1,prism_var(var_id)%num
          if (present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,PACK(fld1(:,:,n),mask=.true.),kinfo,PACK(fld2(:,:,n),mask=.true.), &
                  PACK(fld3(:,:,n),mask=.true.),PACK(fld4(:,:,n),mask=.true.),PACK(fld5(:,:,n),mask=.true.), &
                  write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,PACK(fld1(:,:,n),mask=.true.),kinfo,PACK(fld2(:,:,n),mask=.true.), &
                  PACK(fld3(:,:,n),mask=.true.),PACK(fld4(:,:,n),mask=.true.),write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,PACK(fld1(:,:,n),mask=.true.),kinfo,PACK(fld2(:,:,n),mask=.true.), &
                  PACK(fld3(:,:,n),mask=.true.),write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,PACK(fld1(:,:,n),mask=.true.),kinfo,PACK(fld2(:,:,n),mask=.true.), &
                  write_restart=lwrst,varnum=n)
          elseif (.not.present(fld5) .and. .not.present(fld4) .and. .not.present(fld3) .and. .not.present(fld2)) then
             CALL oasis_put_worker(var_id,kstep,PACK(fld1(:,:,n),mask=.true.),kinfo,write_restart=lwrst,varnum=n)
          else
             WRITE(nulprt,*) subname,estr,' Wrong field array argument list in oasis_put'
             CALL oasis_abort()
          endif
       enddo

    else
       WRITE(nulprt,*) subname,estr,' Dimension sizes incorrect'
       CALL oasis_abort()

    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_r38

!-------------------------------------------------------------------
!---------------------------------------------------------------------
!> Send worker routine puts 8 byte real 1D data

  SUBROUTINE oasis_put_worker(var_id,kstep,fld1,kinfo, &
    fld2, fld3, fld4, fld5, write_restart, varnum)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id      !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep       !< model time in seconds
    real(kind=ip_double_p)             :: fld1(:)     !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo       !< return code
    real(kind=ip_double_p), optional :: fld2(:)       !< higher order field data
    real(kind=ip_double_p), optional :: fld3(:)       !< higher order field data
    real(kind=ip_double_p), optional :: fld4(:)       !< higher order field data
    real(kind=ip_double_p), optional :: fld5(:)       !< higher order field data
    logical               , optional :: write_restart !< write restart now
    integer(kind=ip_i4_p) , optional :: varnum        !< varnum in bundled field
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: ns,nsx
    integer(kind=ip_i4_p) :: n
    integer(kind=ip_i4_p) :: lvarnum
    logical :: a2on, a3on, a4on, a5on
    logical :: lwrst
    character(len=*),parameter :: subname = '(oasis_put_worker)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (.not. enddef_called) then
       write(nulprt,*) subname,estr,'called before oasis_enddef'
       call oasis_abort()
    endif

    if (var_id == OASIS_Var_Uncpl) then
       write(nulprt,*) subname,estr,'oasis_put is called for a variable not in namcouple'
       call oasis_abort()
       call oasis_debug_exit(subname)
       return
    endif

    if (var_id < 1 .or. var_id > prism_nvar) then
       write(nulprt,*) subname,estr,'oasis_put is called for a variable not defined'
       call oasis_abort()
       call oasis_debug_exit(subname)
       return
    endif

    if (present(write_restart)) then
       lwrst = write_restart
    else
       lwrst = .false.
    endif

    if (present(varnum)) then
       lvarnum = varnum
    else
       lvarnum = 1
    endif

    nfld = var_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    ns = size(fld1,dim=1)

    a2on = .false.
    a3on = .false.
    a4on = .false.
    a5on = .false.

    if (present(fld2)) then
       a2on = .true.
       nsx = size(fld2,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,estr,'fld2 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_abort()
       endif
    endif

    if (present(fld3)) then
       a3on = .true.
       nsx = size(fld3,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,estr,'fld3 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_abort()
       endif
    endif

    if (present(fld4)) then
       a4on = .true.
       nsx = size(fld4,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,estr,'fld4 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_abort()
       endif
    endif

    if (present(fld5)) then
       a5on = .true.
       nsx = size(fld5,dim=1)
       if (nsx /= ns) then
          write(nulprt,*) subname,estr,'fld5 size does not match fld ', &
                          trim(prism_var(nfld)%name)
          CALL oasis_abort()
       endif
    endif

    IF ((.NOT. a2on) .AND. (.NOT. a3on) .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din=fld1,readrest=.FALSE.,writrest=lwrst,varnum=lvarnum)
    ELSE IF (a2on .AND. (.NOT. a3on) .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din=fld1,readrest=.FALSE.,&
                               a2on=a2on,array2=fld2,writrest=lwrst,varnum=lvarnum)
    ELSE IF (a2on .AND. a3on .AND. (.NOT. a4on) .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din= fld1,readrest=.FALSE.,&
                               a2on=a2on,array2=fld2,&
                               a3on=a3on,array3=fld3,writrest=lwrst,varnum=lvarnum)
    ELSE IF (a2on .AND. a3on .AND. a4on .AND. (.NOT. a5on)) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din=fld1,readrest=.FALSE.,&
                               a2on=a2on,array2=fld2,&
                               a3on=a3on,array3=fld3,&
                               a4on=a4on,array4=fld4,writrest=lwrst,varnum=lvarnum)
    ELSE IF (a2on .AND. a3on .AND. a4on .AND. a5on) THEN
        CALL oasis_advance_run(OASIS_Out,nfld,kstep,kinfo,&
                               array1din=fld1,readrest=.FALSE.,&
                               a2on=a2on,array2=fld2,&
                               a3on=a3on,array3=fld3,&
                               a4on=a4on,array4=fld4,&
                               a5on=a5on,array5=fld5,writrest=lwrst,varnum=lvarnum)
    ELSE
        WRITE(nulprt,*) subname,estr,' Wrong field array argument list in oasis_put'
        CALL oasis_abort()
    ENDIF

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_put_worker

!-------------------------------------------------------------------
!---------------------------------------------------------------------
#ifndef __NO_4BYTE_REALS 

!> Receive 4 byte real 1D data

  SUBROUTINE oasis_get_r14(var_id,kstep,fld,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id     !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep      !< model time in seconds
    real(kind=ip_single_p), intent(inout) :: fld(:)  !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo      !< return code
    !-------------------------------------
    integer(kind=ip_i4_p) :: ns
    real(kind=ip_r8_p), allocatable :: array(:)
    character(len=*),parameter :: subname = '(oasis_get_r14)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (prism_var(var_id)%num > 1) then
       write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
       write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
       call oasis_abort()
    endif

    ns = size(fld,dim=1)
    allocate(array(ns))

    call oasis_get_worker(var_id,kstep,array,kinfo)

    IF (kinfo /= OASIS_OK) THEN
        fld(:) = REAL(array(:))
    ENDIF

    deallocate(array)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r14
#endif

!---------------------------------------------------------------------

!> Receive 8 byte real 1D data

  SUBROUTINE oasis_get_r18(var_id,kstep,fld,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id     !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep      !< model time in seconds
    real(kind=ip_r8_p)    , intent(inout) :: fld(:)  !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo      !< return code
    !-------------------------------------
    character(len=*),parameter :: subname = '(oasis_get_r18)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (prism_var(var_id)%num > 1) then
       write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
       write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
       call oasis_abort()
    endif

    call oasis_get_worker(var_id,kstep,fld,kinfo)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r18

!---------------------------------------------------------------------
#ifndef __NO_4BYTE_REALS

!> Receive 4 byte real 2D data

  SUBROUTINE oasis_get_r24(var_id,kstep,fld,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id      !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep       !< model time in seconds
    real(kind=ip_single_p), intent(inout) :: fld(:,:) !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo       !< return code
    !-------------------------------------
    integer(kind=ip_i4_p) :: ns,nis,njs,n
    REAL(kind=ip_r8_p), ALLOCATABLE :: array(:)
    character(len=*),parameter :: subname = '(oasis_get_r24)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (prism_var(var_id)%num > 1) then
    ! treat as 1d bundled data
       if (size(fld,dim=2) /= prism_var(var_id)%num) then
          write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
          write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
          write(nulprt,*) subname,estr,'passing in field with incorrect 2nd dim size = ',size(fld,dim=2)
          call oasis_abort()
       endif

       nis = size(fld,dim=1)
       ALLOCATE(array(nis))

       do n = 1,prism_var(var_id)%num
          kinfo = OASIS_OK
          CALL oasis_get_worker(var_id,kstep,array,kinfo,varnum=n)
          IF (kinfo /= OASIS_OK) THEN
             fld(:,n) = REAL(array(:))
          ENDIF
       enddo

       deallocate(array)

    else
    ! treat as 2d unbundled data

       nis = size(fld,dim=1)
       njs = size(fld,dim=2)
       ns = nis*njs

       ALLOCATE(array(ns))

       CALL oasis_get_worker(var_id,kstep,array,kinfo)

       IF (kinfo /= OASIS_OK) THEN
          fld(:,:) = REAL(RESHAPE(array(:),SHAPE(fld)))
       ENDIF

       deallocate(array)

    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r24
#endif

!---------------------------------------------------------------------
!---------------------------------------------------------------------
#ifndef __NO_4BYTE_REALS

!> Receive 4 byte real 2D bundled data

  SUBROUTINE oasis_get_r34(var_id,kstep,fld,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id      !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep       !< model time in seconds
    real(kind=ip_single_p), intent(inout) :: fld(:,:,:) !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo       !< return code
    !-------------------------------------
    integer(kind=ip_i4_p) :: ns,nis,njs,n
    REAL(kind=ip_r8_p), ALLOCATABLE :: array(:)
    character(len=*),parameter :: subname = '(oasis_get_r34)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (prism_var(var_id)%num > 1) then
    ! treat as 2d bundled data
       if (size(fld,dim=3) /= prism_var(var_id)%num) then
          write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
          write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
          write(nulprt,*) subname,estr,'passing in field with incorrect 3rd dim size = ',size(fld,dim=3)
          call oasis_abort()
       endif

       nis = size(fld,dim=1)
       njs = size(fld,dim=2)
       ns = nis*njs

       ALLOCATE(array(ns))

       do n = 1,prism_var(var_id)%num
          kinfo = OASIS_OK
          CALL oasis_get_worker(var_id,kstep,array,kinfo,varnum=n)
          IF (kinfo /= OASIS_OK) THEN
             fld(:,:,n) = REAL(RESHAPE(array(:),SHAPE(fld(:,:,n))))
          ENDIF
       enddo

       deallocate(array)

    else

       WRITE(nulprt,*) subname,estr,' Dimension sizes incorrect'
       CALL oasis_abort()

    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r34
#endif

!---------------------------------------------------------------------

!> Receive 8 byte real 2D data

  SUBROUTINE oasis_get_r28(var_id,kstep,fld,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id      !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep       !< model time in seconds
    real(kind=ip_double_p), intent(inout) :: fld(:,:) !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo       !< return code
    !-------------------------------------
    integer(kind=ip_i4_p) :: ns,nis,njs,n
    REAL(kind=ip_r8_p), ALLOCATABLE :: array(:)
    character(len=*),parameter :: subname = '(oasis_get_r28)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (prism_var(var_id)%num > 1) then
    ! treat as 1d bundled data
       if (size(fld,dim=2) /= prism_var(var_id)%num) then
          write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
          write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
          write(nulprt,*) subname,estr,'passing in field with incorrect 2nd dim size = ',size(fld,dim=2)
          call oasis_abort()
       endif

       nis = size(fld,dim=1)
       ALLOCATE(array(nis))

       do n = 1,prism_var(var_id)%num
          kinfo = OASIS_OK
          CALL oasis_get_worker(var_id,kstep,array,kinfo,varnum=n)
          IF (kinfo /= OASIS_OK) THEN
             fld(:,n) = array(:)
          ENDIF
       enddo

       deallocate(array)

    else
    ! treat as 2d unbundled data

       nis = size(fld,dim=1)
       njs = size(fld,dim=2)
       ns = nis*njs

       ALLOCATE(array(ns))

       CALL oasis_get_worker(var_id,kstep,array,kinfo)

       IF (kinfo /= OASIS_OK) THEN
          fld(:,:) = RESHAPE(array(:),SHAPE(fld))
       ENDIF

       deallocate(array)
    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r28

!-------------------------------------------------------------------
!---------------------------------------------------------------------

!> Receive 8 byte real 2D bundled data

  SUBROUTINE oasis_get_r38(var_id,kstep,fld,kinfo)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id      !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep       !< model time in seconds
    real(kind=ip_double_p), intent(inout) :: fld(:,:,:) !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo       !< return code
    !-------------------------------------
    integer(kind=ip_i4_p) :: ns,nis,njs,n
    REAL(kind=ip_r8_p), ALLOCATABLE :: array(:)
    character(len=*),parameter :: subname = '(oasis_get_r38)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (prism_var(var_id)%num > 1) then
    ! treat as 2d bundled data
       if (size(fld,dim=3) /= prism_var(var_id)%num) then
          write(nulprt,*) subname,estr,'called for variable ',trim(prism_var(var_id)%name)
          write(nulprt,*) subname,estr,'expecting bundled field with num = ',prism_var(var_id)%num
          write(nulprt,*) subname,estr,'passing in field with incorrect 3rd dim size = ',size(fld,dim=3)
          call oasis_abort()
       endif

       nis = size(fld,dim=1)
       njs = size(fld,dim=2)
       ns = nis*njs

       ALLOCATE(array(ns))

       do n = 1,prism_var(var_id)%num
          kinfo = OASIS_OK
          CALL oasis_get_worker(var_id,kstep,array,kinfo,varnum=n)
          IF (kinfo /= OASIS_OK) THEN
             fld(:,:,n) = RESHAPE(array(:),SHAPE(fld(:,:,n)))
          ENDIF
       enddo

       deallocate(array)

    else
       WRITE(nulprt,*) subname,estr,' Dimension sizes incorrect'
       CALL oasis_abort()

    endif

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_r38

!-------------------------------------------------------------------
!> Receive subroutine that actually does the work on 8 byte 1D data

  SUBROUTINE oasis_get_worker(var_id,kstep,fld,kinfo,varnum)

    IMPLICIT none
    !-------------------------------------
    integer(kind=ip_i4_p) , intent(in) :: var_id     !< variable id
    integer(kind=ip_i4_p) , intent(in) :: kstep      !< model time in seconds
    real(kind=ip_double_p), intent(inout) :: fld(:)  !< field data
    integer(kind=ip_i4_p) , intent(out):: kinfo      !< return code
    integer(kind=ip_i4_p) , optional   :: varnum     !< variable num in bundled field
    !-------------------------------------
    integer(kind=ip_i4_p) :: nfld,ncpl
    integer(kind=ip_i4_p) :: lvarnum
    character(len=*),parameter :: subname = '(oasis_get_worker)'
    !-------------------------------------

    call oasis_debug_enter(subname)
    kinfo = OASIS_OK
    if (.not. oasis_coupled) then
       call oasis_debug_exit(subname)
       return
    endif

    if (.not. enddef_called) then
       write(nulprt,*) subname,estr,'called before oasis_enddef'
       call oasis_abort()
    endif

    if (var_id == OASIS_Var_Uncpl) then
       write(nulprt,*) subname,estr,'oasis_get is called for a variable not in namcouple'
       write(nulprt,*) subname,' BE CAREFUL NOT TO USE IT !!!!!'
       call oasis_abort()
       call oasis_debug_exit(subname)
       return
    endif

    if (var_id < 1 .or. var_id > prism_nvar) then
       write(nulprt,*) subname,estr,'oasis_get is called for a variable not defined'
       call oasis_abort()
       call oasis_debug_exit(subname)
       return
    endif

    if (present(varnum)) then
       lvarnum = varnum
    else
       lvarnum = 1
    endif

    nfld = var_id
    ncpl  = prism_var(nfld)%ncpl

    if (ncpl <= 0) then
       if (OASIS_debug >= 15) write(nulprt,*) subname,' variable not coupled ',&
                              trim(prism_var(nfld)%name)
       call oasis_debug_exit(subname)
       return
    endif

    CALL oasis_advance_run(OASIS_In,nfld,kstep,kinfo,array1dout=fld,readrest=.FALSE.,varnum=lvarnum)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_get_worker

!-------------------------------------------------------------------

END MODULE mod_oasis_getput_interface

