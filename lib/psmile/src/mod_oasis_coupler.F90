
!> Initialize the OASIS coupler infrastructure

MODULE mod_oasis_coupler
!     - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
  USE mod_oasis_kinds
  USE mod_oasis_data
  USE mod_oasis_parameters
  USE mod_oasis_namcouple
  USE mod_oasis_sys
  USE mod_oasis_map
  USE mod_oasis_part
  USE mod_oasis_var
  USE mod_oasis_mpi
  USE mod_oasis_string
  USE mod_oasis_io
  USE mod_oasis_timer
  USE mct_mod
  USE grids    ! scrip
  USE netcdf

  IMPLICIT NONE

  private

  public oasis_coupler_setup

! Type of data

  public prism_router_type
  public prism_coupler_type

! COUPLING INFO

  !> Router information for rearranging data on tasks
  type prism_router_type
     !--- fixed at initialization ---
     type(mct_router)      :: router     !< router
  end type prism_router_type

  integer(kind=ip_i4_p),public,parameter :: prism_coupler_avsmax=5  !< maximum number of higher order terms in mapping

  !> Coupler data for managing all aspects of coupling in OASIS
  type prism_coupler_type
     !--- fixed at initialization ---
     type(mct_aVect)       :: aVect1   !< primary aVect
     type(mct_aVect)       :: aVect1m  !< extra aVect needed for mapping
     type(mct_aVect)       :: aVect2   !< higher order mapping data
     type(mct_aVect)       :: aVect3   !< higher order mapping data
     type(mct_aVect)       :: aVect4   !< higher order mapping data
     type(mct_aVect)       :: aVect5   !< higher order mapping data
     logical               :: aVon(prism_coupler_avsmax)  !< flags indicating whether aVects 2-5 are active
     character(len=ic_xl)  :: rstfile  !< restart file
     character(len=ic_xl)  :: inpfile  !< input file if data is read
     character(len=ic_xl)  :: fldlist  !< field list
     integer(kind=ip_i4_p) :: nflds    !< number of fields
     integer(kind=ip_i4_p),pointer :: varid(:)    !< varid for each field
     logical               :: valid    !< is this coupler valid
     integer(kind=ip_i4_p) :: namID    !< namcouple ID
     integer(kind=ip_i4_p) :: partID   !< local variable partition ID
     integer(kind=ip_i4_p) :: rpartID  !< router partition ID
     integer(kind=ip_i4_p) :: routerID !< router ID
     integer(kind=ip_i4_p) :: mapperID !< mapper ID
     character(len=ic_med) :: maploc   !< map location setting, src or dst
     integer(kind=ip_i4_p) :: ops      !< namcouple operation (ip_exported,...)
     integer(kind=ip_i4_p) :: comp     !< other model compid to couple
     integer(kind=ip_i4_p) :: tag      !< communcation tag
     integer(kind=ip_i4_p) :: seq      !< sequence number
     integer(kind=ip_i4_p) :: dt       !< coupling period (secs)
     integer(kind=ip_i4_p) :: lag      !< put lag positive is put sooner (secs)
     integer(kind=ip_i4_p) :: maxtime  !< max time for the coupler
     integer(kind=ip_i4_p) :: trans    !< transformation (ip_average,...)
     integer(kind=ip_i4_p) :: conserv  !< conserve operation (ip_cnone,ip_cglobal,...)
     character(len=ic_med) :: consopt  !< conserve option (bfb, opt)
     integer(kind=ip_i4_p) :: getput   !< get/put flag
     logical               :: sndrcv   !< send recv flag
     logical               :: output   !< output flag
     logical               :: input    !< input flag
     logical               :: snddiag  !< diagnose src fields as part of coupling
     logical               :: rcvdiag  !< diagnose rcv fields as part of coupling
     real(kind=ip_double_p):: sndmult  !< send field multiplier term
     real(kind=ip_double_p):: sndadd   !< send field addition term
     real(kind=ip_double_p):: rcvmult  !< receive field multiplier term
     real(kind=ip_double_p):: rcvadd   !< receive field addition term
     !--- time varying info ---
     integer(kind=ip_i4_p) :: ltime    !< time at last coupling
     integer(kind=ip_i4_p),pointer :: avcnt(:)  !< counter for averaging
     integer(kind=ip_i4_p),pointer :: status(:) !< status of variables in coupler
  end type prism_coupler_type

  integer(kind=ip_i4_p)           :: prism_mrouter   !< max routers
  integer(kind=ip_i4_p)           :: prism_nrouter = 0  !< router counter
  type(prism_router_type) ,public, pointer:: prism_router(:)  !< prism_router array

  integer(kind=ip_i4_p)   ,public :: prism_mcoupler  !< max couplers
  type(prism_coupler_type),public, pointer :: prism_coupler_put(:)  !< prism_coupler put array
  type(prism_coupler_type),public, pointer :: prism_coupler_get(:)  !< prism_coupler get array

  integer(kind=ip_i4_p)   ,public :: lcouplerid    !< last coupler id
  integer(kind=ip_i4_p)   ,public :: lcouplertime  !< last coupler time 
  integer(kind=ip_i4_p)   ,public :: lastseq       !< last coupler sequence
  integer(kind=ip_i4_p)   ,public :: lastseqtime   !< last coupler sequence time


!#include <netcdf.inc>

!------------------------------------------------------------
CONTAINS
!------------------------------------------------------------

!> Main routine to setup couplers

!> This routine initializes all the coupler data based on the namcouple
!> inputs and the calls into the OASIS initialization interfaces from models.
!> It reconciles everything.  This is called from oasis_enddef.

  SUBROUTINE oasis_coupler_setup()

  !----------------------------------------------------------
  ! This routine reconciles the coupling stuff
  !----------------------------------------------------------

  IMPLICIT none

  integer(kind=ip_i4_p) :: n,n1,n2,nn,nv,nm,nv1,nv1a,nns,lnn,nc,nf,nvf,npc,r1
  integer(kind=ip_i4_p) :: pe
  integer(kind=ip_i4_p) :: part1, part2
  integer(kind=ip_i4_p) :: spart,dpart ! src, dst partitions for mapping
        ! part1 = my local part, partID
        ! part2 = other partition for mapping
        ! spart = src part for mapping; put=part1, get=part2
        ! dpart = dst part for mapping; put=part2, get=part1
  integer(kind=ip_i4_p) :: mapID,namID
  type(mct_sMat),pointer :: sMati(:)
  integer(kind=ip_i4_p) :: ncid,dimid,status
  integer(kind=ip_i4_p) :: lsize,gsize
  integer(kind=ip_i4_p) :: svarid
  integer(kind=ip_i4_p),allocatable :: varidtmp(:)
  integer(kind=ip_i4_p) :: part
  character(len=ic_med) :: cstring
  character(len=ic_lvar):: myfld
  integer(kind=ip_i4_p) :: myfldi
  character(len=ic_xl)  :: myfldlist  ! field list
  character(len=ic_lvar):: otfld
  character(len=ic_xl)  :: otfldlist  ! field list
  integer(kind=ip_i4_p) :: nx,ny
  character(len=ic_lvar):: gridname
  character(len=ic_long):: tmp_mapfile
  integer(kind=ip_i4_p) :: flag
  logical               :: found, exists, found2
  integer(kind=ip_i4_p) :: mynvar
  integer(kind=ip_i4_p) :: nwgts
  character(len=ic_lvar):: tmpfld
  type(prism_coupler_type),pointer :: pcpointer
  type(prism_coupler_type),pointer :: pcpntpair
  integer(kind=ip_i4_p) :: ifind,nfind
  character(len=ic_lvar),pointer :: myvar(:)
  integer(kind=ip_i4_p) ,pointer :: myops(:)
  integer(kind=ip_i4_p) ,pointer :: nallvar(:)
  character(len=ic_lvar),pointer :: allvar(:,:)
  integer(kind=ip_i4_p) ,pointer :: allops(:,:)
  integer(kind=ip_i4_p) ,pointer :: namsrc_checkused(:) ! 0 = not used
  integer(kind=ip_i4_p) ,pointer :: namsrc_checkused_g(:)  ! 0 = not used
  type sortnamfld_type
     integer(kind=ip_i4_p) :: num                 ! total number of namcouple fields
     integer(kind=ip_i4_p) ,pointer :: namnum(:)  ! namcouple number
     integer(kind=ip_i4_p) ,pointer :: fldnum(:)  ! namcouple field number in namcouple
     character(len=ic_lvar),pointer :: fld(:)     ! namcouple field name
  end type sortnamfld_type
  type(sortnamfld_type) :: sortnsrc
  type(sortnamfld_type) :: sortndst
  type sortvarfld_type
     integer(kind=ip_i4_p) :: num                 ! total number of var fields
     integer(kind=ip_i4_p) ,pointer :: modnum(:)  ! model number
     integer(kind=ip_i4_p) ,pointer :: varnum(:)  ! var field number in model
     character(len=ic_lvar),pointer :: fld(:)     ! variable field name
  end type sortvarfld_type
  type(sortvarfld_type) :: sortvars
  type(sortvarfld_type) :: sorttest
  integer(kind=ip_i4_p) ,pointer :: sortkey(:)
!  character(len=*),parameter :: smatread_method = 'orig'
  character(len=*),parameter :: smatread_method = 'ceg'
  logical, parameter :: local_timers_on = .false.

  character(len=*),parameter :: subname = '(oasis_coupler_setup)'

  !----------------------------------------------------------

  call oasis_debug_enter(subname)
!  call oasis_mpi_barrier(mpi_comm_global)
  IF (local_timers_on) call oasis_timer_start('cpl_setup')

  if (local_timers_on) call oasis_timer_start('cpl_setup_n1')

  write(nulprt,*) subname,' smatread_method = ',trim(smatread_method)

  !-----------------------------------------
  !> * Allocate and zero prism_router, prism_mapper, prism_coupler based on nnamcpl
  ! there cannot be more than that needed
  !-----------------------------------------

  call oasis_debug_note(subname//' set defaults for datatypes')

  prism_mrouter = nnamcpl * 2   ! multiply by 2 for coupling to self
  allocate(prism_router(prism_mrouter))
  prism_nrouter = 0

  prism_mmapper = nnamcpl
  allocate(prism_mapper(prism_mmapper))
  prism_nmapper = 0
  prism_mapper(:)%nwgts = 0
  prism_mapper(:)%file  = ""
  prism_mapper(:)%loc   = ""
  prism_mapper(:)%opt   = ""
  prism_mapper(:)%optval= ""
  prism_mapper(:)%init  = .false.
  prism_mapper(:)%spart = ispval
  prism_mapper(:)%dpart = ispval
  prism_mapper(:)%AVred = .false.

  prism_mcoupler = nnamcpl
  allocate(prism_coupler_put(prism_mcoupler))
  allocate(prism_coupler_get(prism_mcoupler))

  do nc = 1,prism_mcoupler
  do npc = 1,2
     if (npc == 1) then
        pcpointer => prism_coupler_put(nc)
        pcpntpair => prism_coupler_get(nc)
     endif
     if (npc == 2) then
        pcpointer => prism_coupler_get(nc)
        pcpntpair => prism_coupler_put(nc)
     endif
     pcpointer%rstfile = ""
     pcpointer%inpfile = ""
     pcpointer%fldlist = ""
     pcpointer%nflds   = 0
     pcpointer%namID   = 0
     pcpointer%valid   = .false.
!tcx is this alloc pcpointer or prism_coupler_*
     allocate(pcpointer%varid(1))
     pcpointer%varid(:) = ispval
     pcpointer%aVon(:) = .false.
     pcpointer%ops     = ispval
     pcpointer%comp    = ispval
     pcpointer%routerID  = ispval
     pcpointer%mapperID  = ispval
     pcpointer%maploc  = ""
     pcpointer%tag     = ispval
     pcpointer%dt      = ispval
     pcpointer%lag     = 0
     pcpointer%maxtime = 0
     pcpointer%getput  = ispval
     pcpointer%sndrcv  = .false.
     pcpointer%output  = .false.
     pcpointer%input   = .false.
     pcpointer%trans   = ip_instant
     pcpointer%conserv = ip_cnone
     pcpointer%ltime   = ispval
     pcpointer%snddiag = .false.
     pcpointer%rcvdiag = .false.
     pcpointer%sndmult = 1.0_ip_double_p
     pcpointer%sndadd  = 0.0_ip_double_p
     pcpointer%rcvmult = 1.0_ip_double_p
     pcpointer%rcvadd  = 0.0_ip_double_p
  enddo  ! npc
  enddo  ! nc

  lcouplerid   = ispval
  lcouplertime = ispval
  lastseq      = ispval
  lastseqtime  = ispval

  !----------------------------------------------------------
  !> * Generate model variable lists across all models based on def_var calls.
  !> These will be reconciled with the namcouple input.  These are sorted
  !> to improve search performance later.
  !----------------------------------------------------------

  call oasis_debug_note(subname//' share var info between models')

  allocate(allvar(maxvar,prism_amodels))
  allocate(nallvar(prism_amodels))
  allocate(allops(maxvar,prism_amodels))
  allocate(myvar(maxvar))
  allocate(myops(maxvar))

  allvar = " "
  nallvar = 0
  allops = -1
  if (local_timers_on) call oasis_timer_start('cpl_setup_n1_bcast')
  do n = 1,prism_amodels
     if (n == compid) then
        myvar = " "
        myops = 0
        mynvar = prism_nvar
        do n1 = 1, prism_nvar
           myvar(n1) = trim(prism_var(n1)%name)
           myops(n1) = prism_var(n1)%ops
           ! check that each var name is unique for a given model
           do n2 = 1,n1-1
              if (myvar(n1) == myvar(n2)) then
                 WRITE(nulprt,*) subname,estr,'variable name defined more than once by def_var = ',trim(myvar(n1))
                 call oasis_abort()
              endif
           enddo
        enddo
     endif
     if (OASIS_debug >= 5) then
        write(nulprt,*) subname,' BCAST from ',n,mpi_root_global(n)
        call oasis_flush(nulprt)
     endif
     call oasis_mpi_bcast(mynvar,mpi_comm_global,'mynvar',mpi_root_global(n))
     if (OASIS_debug >= 5) then
        write(nulprt,*) subname,' bcast mynvar ',mynvar
        call oasis_flush(nulprt)
     endif
     nallvar(n) = mynvar
     call oasis_mpi_bcast(myvar,mpi_comm_global,'myvar',mpi_root_global(n))
     if (OASIS_debug >= 5) then
        write(nulprt,*) subname,' bcast myvar ',trim(myvar(1))
        call oasis_flush(nulprt)
     endif
     allvar(:,n) = myvar(:)
     call oasis_mpi_bcast(myops,mpi_comm_global,'myops',mpi_root_global(n))
     if (OASIS_debug >= 5) then
        write(nulprt,*) subname,' bcast myops ',myops(1)
        call oasis_flush(nulprt)
     endif
     allops(:,n) = myops(:)
  enddo
  if (local_timers_on) call oasis_timer_stop('cpl_setup_n1_bcast')

  deallocate(myvar,myops)

  if (OASIS_debug >= 2) then
     write(nulprt,*) subname,' model variable info:'
     do nm = 1,prism_amodels
        write(nulprt,'(8x,a,2i6)') ' model,nvars = ',nm,nallvar(nm)
        do nv = 1,nallvar(nm)
           cstring = 'unknown'
           if (allops(nv,nm) == OASIS_Out) cstring = 'prism_out'
           if (allops(nv,nm) == OASIS_In)  cstring = 'prism_in'
           write(nulprt,'(16x,a,2i6,2x,a,i6,2x,a)') ' model,idx,var,ops = ',nm,nv,&
                                                      trim(allvar(nv,nm)),allops(nv,nm),&
                                                      trim(cstring)
        enddo
     enddo
     write(nulprt,*) ' '
     call oasis_flush(nulprt)
  endif

  ! generate sortvars sorted list

  n1 = 0
  do n = 1,prism_amodels
     n1 = n1 + nallvar(n)
  enddo
  allocate(sortvars%fld(n1))
  allocate(sortvars%modnum(n1))
  allocate(sortvars%varnum(n1))
  allocate(sortkey(n1))
  sortvars%num = n1

  n1 = 0
  do n = 1,prism_amodels
  do n2 = 1,nallvar(n)
     n1 = n1 + 1
     sortkey(n1) = n1
     sortvars%fld(n1) = allvar(n2,n)
     sortvars%modnum(n1) = n
     sortvars%varnum(n1) = n2
  enddo
  enddo

  call cplsort(sortvars%num, sortvars%fld, sortkey)
  call cplsortkey(sortvars%num, sortvars%modnum, sortkey)
  call cplsortkey(sortvars%num, sortvars%varnum, sortkey)

  if (OASIS_debug >= 15) then
     write(nulprt,*) subname//' Sorted array : sortvars'
     do n1 = 1,sortvars%num
        write(nulprt,*) subname,'sort sortvars',n1,sortkey(n1),sortvars%modnum(n1),sortvars%varnum(n1),trim(sortvars%fld(n1))
     enddo
  endif

  deallocate(sortkey)

  !----------------------------------------------------------
  !> * Setup couplers based on namcouple and model variable info.
  ! These must be paired up consistently, create couplers in
  ! sorted order (nns)
  ! nn = namcpl counter, sorted
  ! nm = model counter, compid is my nm
  ! nv = variable counter
  ! nv1 = my variable counter
  !----------------------------------------------------------

  if (local_timers_on) call oasis_timer_stop ('cpl_setup_n1')

  !--------------------------------
  !> * Preprocess namcouple strings and sort for faster searches
  !--------------------------------

  ! count namcouple field names

  if (local_timers_on) call oasis_timer_start('cpl_setup_n2')
  n1 = 0
  n2 = 0
  do nn = 1,nnamcpl
     n1 = n1 + oasis_string_listGetNum(namsrcfld(nn))
     n2 = n2 + oasis_string_listGetNum(namdstfld(nn))
     if (n1 /= n2) then
        WRITE(nulprt,*) subname,estr,'number of fields in namcouple inconsistent ',nn,n1,n2
        WRITE(nulprt,*) subname,estr,'namcouple src fields = ',trim(namsrcfld(nn))
        WRITE(nulprt,*) subname,estr,'namcouple dst fields = ',trim(namdstfld(nn))
        call oasis_abort()
     endif
  enddo

  ! allocate space
  ! note: n2==n1

  sortnsrc%num = n1
  allocate(sortnsrc%fld(n1))
  allocate(sortnsrc%namnum(n1))
  allocate(sortnsrc%fldnum(n1))
  sortndst%num = n2
  allocate(sortndst%fld(n2))
  allocate(sortndst%namnum(n2))
  allocate(sortndst%fldnum(n2))

  ! this will check that all namcouple vars are used in application
  allocate(namsrc_checkused(sortnsrc%num))
  namsrc_checkused = 0

  ! fill and sort sortnsrc

  allocate(sortkey(sortnsrc%num))
  n1 = 0
  do nn = 1,nnamcpl
  do n2 = 1,oasis_string_listGetNum(namsrcfld(nn))
     n1 = n1 + 1
     sortkey(n1) = n1
     sortnsrc%namnum(n1) = nn
     sortnsrc%fldnum(n1) = n2
     call oasis_string_listGetName(namsrcfld(nn),n2,sortnsrc%fld(n1))
  enddo
  enddo

  call cplsort(sortnsrc%num, sortnsrc%fld, sortkey)
  call cplsortkey(sortnsrc%num, sortnsrc%namnum, sortkey)
  call cplsortkey(sortnsrc%num, sortnsrc%fldnum, sortkey)

  if (OASIS_debug >= 15) then
     write(nulprt,*) subname//' Sorted array : sortnsrc'
     do n1 = 1,sortnsrc%num
        write(nulprt,*) subname,'sort sortnsrc',n1,sortkey(n1), &
           sortnsrc%namnum(n1),sortnsrc%fldnum(n1),trim(sortnsrc%fld(n1))
     enddo
  endif
  deallocate(sortkey)

  ! fill and sort sortndst

  allocate(sortkey(sortndst%num))
  n1 = 0
  do nn = 1,nnamcpl
  do n2 = 1,oasis_string_listGetNum(namdstfld(nn))
     n1 = n1 + 1
     sortkey(n1) = n1
     sortndst%namnum(n1) = nn
     sortndst%fldnum(n1) = n2
     call oasis_string_listGetName(namdstfld(nn),n2,sortndst%fld(n1))
  enddo
  enddo

  call cplsort(sortndst%num, sortndst%fld, sortkey)
  call cplsortkey(sortndst%num, sortndst%namnum, sortkey)
  call cplsortkey(sortndst%num, sortndst%fldnum, sortkey)

  if (OASIS_debug >= 15) then
     write(nulprt,*) subname//' Sorted array : sortndst'
     do n1 = 1,sortndst%num
        write(nulprt,*) subname,'sort sortndst',n1,sortkey(n1), &
           sortndst%namnum(n1),sortndst%fldnum(n1),trim(sortndst%fld(n1))
     enddo
  endif
  deallocate(sortkey)

  if (OASIS_debug >= 1500) then

     write(nulprt,*) subname,' Test sort code: '

     n1 = 10
     allocate(sorttest%fld(n1))
     allocate(sorttest%modnum(n1))
     allocate(sorttest%varnum(n1))
     allocate(sortkey(n1))
     sorttest%num = n1

     sorttest%fld(:) = 'A'
     do n1 = 1,sorttest%num
        sortkey(n1) = n1
        if (n1 ==  1) sorttest%fld(n1) = 'D'
        if (n1 ==  2) sorttest%fld(n1) = 'C'
        if (n1 ==  4) sorttest%fld(n1) = 'C'
        if (n1 ==  5) sorttest%fld(n1) = 'D'
        if (n1 ==  8) sorttest%fld(n1) = 'C'
        if (n1 ==  9) sorttest%fld(n1) = 'B'
        if (n1 == 10) sorttest%fld(n1) = 'C'
        sorttest%modnum(n1) = n1+100
        sorttest%varnum(n1) = n1
     enddo

     call cplsort(sorttest%num, sorttest%fld, sortkey)
     call cplsortkey(sorttest%num, sorttest%modnum, sortkey)
     call cplsortkey(sorttest%num, sorttest%varnum, sortkey)

     write(nulprt,*) subname//' Sorted array : sorttest'
     do n1 = 1,sorttest%num
        write(nulprt,*) subname,'sort sorttest',n1,sortkey(n1), &
           sorttest%modnum(n1),sorttest%varnum(n1),trim(sorttest%fld(n1))
     enddo

     tmpfld = 'A'
     call cplfind(sorttest%num, sorttest%fld, tmpfld, ifind, nfind)
     write(nulprt,*) subname,' cpl find1 ',trim(tmpfld),ifind,nfind
     do n1 = ifind,ifind+nfind-1
        write(nulprt,*) subname,' cpl find2 ',n1,trim(sorttest%fld(n1))
     enddo

     tmpfld = 'B'
     call cplfind(sorttest%num, sorttest%fld, tmpfld, ifind, nfind)
     write(nulprt,*) subname,' cpl find1 ',trim(tmpfld),ifind,nfind
     do n1 = ifind,ifind+nfind-1
        write(nulprt,*) subname,' cpl find2 ',n1,trim(sorttest%fld(n1))
     enddo

     tmpfld = 'C'
     call cplfind(sorttest%num, sorttest%fld, tmpfld, ifind, nfind)
     write(nulprt,*) subname,' cpl find1 ',trim(tmpfld),ifind,nfind
     do n1 = ifind,ifind+nfind-1
        write(nulprt,*) subname,' cpl find2 ',n1,trim(sorttest%fld(n1))
     enddo

     tmpfld = 'D'
     call cplfind(sorttest%num, sorttest%fld, tmpfld, ifind, nfind)
     write(nulprt,*) subname,' cpl find1 ',trim(tmpfld),ifind,nfind
     do n1 = ifind,ifind+nfind-1
        write(nulprt,*) subname,' cpl find2 ',n1,trim(sorttest%fld(n1))
     enddo

     tmpfld = 'E'
     call cplfind(sorttest%num, sorttest%fld, tmpfld, ifind, nfind)
     write(nulprt,*) subname,' cpl find1 ',trim(tmpfld),ifind,nfind
     do n1 = ifind,ifind+nfind-1
        write(nulprt,*) subname,' cpl find2 ',n1,trim(sorttest%fld(n1))
     enddo

     deallocate(sortkey)
     deallocate(sorttest%fld)
     deallocate(sorttest%modnum)
     deallocate(sorttest%varnum)

     write(nulprt,*) subname,' Test cplfind: '
     n1 = max(min(sortndst%num,sortndst%num/3),1)
     tmpfld = sortndst%fld(n1)
     call cplfind(sortndst%num, sortndst%fld, tmpfld, ifind, nfind)
     write(nulprt,*) subname,' cpl find1 ',trim(tmpfld),ifind,nfind
     do n1 = ifind,ifind+nfind-1
        write(nulprt,*) subname,' cpl find2 ',n1,trim(sortndst%fld(n1))
     enddo

     n1 = max(min(sortndst%num,1),1)
     tmpfld = sortndst%fld(n1)
     call cplfind(sortndst%num, sortndst%fld, tmpfld, ifind, nfind)
     write(nulprt,*) subname,' cpl find1 ',trim(tmpfld),ifind,nfind
     do n1 = ifind,ifind+nfind-1
        write(nulprt,*) subname,' cpl find2 ',n1,trim(sortndst%fld(n1))
     enddo

     n1 = max(min(sortndst%num,2),1)
     tmpfld = sortndst%fld(n1)
     call cplfind(sortndst%num, sortndst%fld, tmpfld, ifind, nfind)
     write(nulprt,*) subname,' cpl find1 ',trim(tmpfld),ifind,nfind
     do n1 = ifind,ifind+nfind-1
        write(nulprt,*) subname,' cpl find2 ',n1,trim(sortndst%fld(n1))
     enddo

     n1 = max(min(sortndst%num,sortndst%num-1),1)
     tmpfld = sortndst%fld(n1)
     call cplfind(sortndst%num, sortndst%fld, tmpfld, ifind, nfind)
     write(nulprt,*) subname,' cpl find1 ',trim(tmpfld),ifind,nfind
     do n1 = ifind,ifind+nfind-1
        write(nulprt,*) subname,' cpl find2 ',n1,trim(sortndst%fld(n1))
     enddo

     n1 = max(min(sortndst%num,sortndst%num),1)
     tmpfld = sortndst%fld(n1)
     call cplfind(sortndst%num, sortndst%fld, tmpfld, ifind, nfind)
     write(nulprt,*) subname,' cpl find1 ',trim(tmpfld),ifind,nfind
     do n1 = ifind,ifind+nfind-1
        write(nulprt,*) subname,' cpl find2 ',n1,trim(sortndst%fld(n1))
     enddo

     CALL oasis_flush(nulprt)
  endif

  if (local_timers_on) call oasis_timer_stop ('cpl_setup_n2')

  call oasis_debug_note(subname//' compare vars and namcouple')
  call oasis_debug_note(subname//' setup couplers')

  if (local_timers_on) call oasis_timer_start('cpl_setup_n3')

  !--------------------------------
  !> * Loop over all my model variables
  !--------------------------------

  do nv1 = 1,prism_nvar

     !--------------------------------
     !>   * Get parition and field information
     !--------------------------------

     part1  = prism_var(nv1)%part
     myfld  = prism_var(nv1)%name

     IF (OASIS_debug >= 20) THEN
        WRITE(nulprt,*) ' '
        WRITE(nulprt,*) subname,' get part and fld ',nv1,part1,trim(myfld)
        CALL oasis_flush(nulprt)
     ENDIF

     !--------------------------------
     !>   * Check if variable is In or Out and then find namcouple matches
     !--------------------------------

     if (local_timers_on) call oasis_timer_start('cpl_setup_n3a')
     if (prism_var(nv1)%ops == OASIS_Out) then
        call cplfind(sortnsrc%num, sortnsrc%fld, myfld, ifind, nfind)
     elseif (prism_var(nv1)%ops == OASIS_In) then
        call cplfind(sortndst%num, sortndst%fld, myfld, ifind, nfind)
     endif
     if (local_timers_on) call oasis_timer_stop ('cpl_setup_n3a')

     !--------------------------------
     !>   * Loop over the namcouple matches
     !--------------------------------
     do nf = ifind,ifind+nfind-1
        if (local_timers_on) call oasis_timer_start('cpl_setup_n3b')

        flag = OASIS_NotDef

        if (prism_var(nv1)%ops == OASIS_Out) then
           nn = sortnsrc%namnum(nf)
           myfldi = sortnsrc%fldnum(nf)
           myfldlist = namsrcfld(nn)
           otfldlist = namdstfld(nn)
           flag = OASIS_Out
        elseif (prism_var(nv1)%ops == OASIS_In) then
           nn = sortndst%namnum(nf)
           myfldi = sortndst%fldnum(nf)
           myfldlist = namdstfld(nn)
           otfldlist = namsrcfld(nn)
           flag = OASIS_In
        endif

        nns = namnn2sort(nn)

        IF (OASIS_debug >= 20) THEN
            WRITE(nulprt,*) subname,' found fld1 ',trim(myfld),nv1,nf
            WRITE(nulprt,*) subname,' found fld2 ',trim(myfld),nns,nn,myfldi,flag
            CALL oasis_flush(nulprt)
        ENDIF

        if (local_timers_on) call oasis_timer_stop ('cpl_setup_n3b')

        !--------------------------------
        ! my variable is in this namcouple input
        !--------------------------------

        if (flag /= OASIS_NotDef) then

           if (local_timers_on) call oasis_timer_start('cpl_setup_n3c')

           !--------------------------------
           !>     * Migrate namcouple info into part
           !--------------------------------

           IF (OASIS_debug >= 20) THEN
               WRITE(nulprt,*) subname,' migrate namcouple to part '
               CALL oasis_flush(nulprt)
           ENDIF

           if (flag == OASIS_In) then
              if (prism_part(part1)%nx < 1) then
                 prism_part(part1)%nx = namdst_nx(nn)
                 prism_part(part1)%ny = namdst_ny(nn)
                 prism_part(part1)%gridname = trim(namdstgrd(nn))
              endif
           endif
           if (flag == OASIS_Out) then
              if (prism_part(part1)%nx < 1) then
                 prism_part(part1)%nx = namsrc_nx(nn)
                 prism_part(part1)%ny = namsrc_ny(nn)
                 prism_part(part1)%gridname = trim(namsrcgrd(nn))
              endif
           endif

           IF (OASIS_debug >= 20) THEN
               WRITE(nulprt,*) subname,' Field : ',trim(prism_var(nn)%name)
               WRITE(nulprt,*) subname,' Grid dst : ',trim(namdstgrd(nn))
               WRITE(nulprt,*) subname,' Grid src : ',trim(namsrcgrd(nn))
!               WRITE(nulprt,*) subname,' prism_part : ',prism_part(part1)%gridname
               CALL oasis_flush(nulprt)
           ENDIF

           !--------------------------------
           !>     * Make sure it's either an In or Out, sanity check
           !--------------------------------

           if (flag /= OASIS_In .and. flag /= OASIS_Out) then
              write(nulprt,*) subname,estr,'var must be either OASIS_In or OASIS_Out for var = ',trim(myfld)
              call oasis_abort()
           endif

           if (OASIS_debug >= 20) then
              write(nulprt,'(1x,2a,4i6,2a)') subname,' ca: myfld',nn,compid,&
                                             nv1,myfldi,' ',trim(myfld)
              call oasis_flush(nulprt)
           endif

           !--------------------------------
           !>     * Determine matching field name from namcouple
           !--------------------------------

           if (local_timers_on) call oasis_timer_start('cpl_setup_n3c1')
           otfld = 'NOmatchNOyesNOyesNO'
           call oasis_string_listGetName(otfldlist,myfldi,otfld)
           if (local_timers_on) call oasis_timer_stop ('cpl_setup_n3c1')

           IF (OASIS_debug >= 20) THEN
              WRITE(nulprt,*) subname,' otfld ',trim(otfld)
              CALL oasis_flush(nulprt)
           ENDIF

           !--------------------------------
           !>     * Search for list of models with other variable
           !--------------------------------

           if (local_timers_on) call oasis_timer_start('cpl_setup_n3c2')
           call cplfind(sortvars%num, sortvars%fld, otfld, ifind, nfind)
           if (local_timers_on) call oasis_timer_stop ('cpl_setup_n3c2')
           if (local_timers_on) call oasis_timer_stop ('cpl_setup_n3c')

           !--------------------------------
           !>     * Loop over those other matching variable names
           !--------------------------------
           found = .false.
           do nvf = ifind, ifind+nfind-1

              ! check used appropriate array value, we are using "src" side sorted list
              ! if output, just set the nf value
              ! if input, search for an nn and myfldi match in the list
              
              if (prism_var(nv1)%ops == OASIS_Out) then
                 namsrc_checkused(nf) = 1
                 if (OASIS_debug >= 20) then
                    write(nulprt,*) subname,' set src checkused ',trim(myfld),':',trim(otfld),nf
                    call oasis_flush(nulprt)
                 endif
              endif
              if (prism_var(nv1)%ops == OASIS_In) then
                 n1 = 0
                 found2 = .false.
                 do while (n1 < sortnsrc%num .and. .not.found2)
                    n1 = n1 + 1
                    if (nn == sortnsrc%namnum(n1) .and. myfldi == sortnsrc%fldnum(n1)) then
                       namsrc_checkused(n1) = 1
                       found2 = .true.
                       if (OASIS_debug >= 20) then
                          write(nulprt,*) subname,' set dst checkused ',trim(myfld),':',trim(otfld),n1
                          call oasis_flush(nulprt)
                       endif
                    endif
                 enddo
              endif

              if (local_timers_on) call oasis_timer_start('cpl_setup_n3d')
              nm = sortvars%modnum(nvf)
              nv = sortvars%varnum(nvf)
              
              if (OASIS_debug >= 20) then
                 write(nulprt,*) subname,' match otfld ',trim(otfld),nn
                 call oasis_flush(nulprt)
              endif

              !--------------------------------
              !>       * Check that one side is In and other side is Out for communication
              !>       * Check if input or output, field name should match on both sides.
              !--------------------------------

              if (namfldops(nn) == ip_exported .or. namfldops(nn) == ip_expout) then
! tcraig allow this now
!                if (nm == compid) then
!                   write(nulprt,*) subname,estr,'send recv pair on same model = ', &
!                      trim(myfld),' ',trim(otfld)
!                   call oasis_abort()
!                endif
                 if (flag == OASIS_Out .and. allops(nv,nm) /= OASIS_In) then
                    write(nulprt,*) subname,estr,'send recv pair both Out = ', &
                       trim(myfld),' ',trim(otfld)
                    call oasis_abort()
                 endif
                 if (flag == OASIS_In .and. allops(nv,nm) /= OASIS_Out) then
                    write(nulprt,*) subname,estr,'send recv pair both In = ', &
                       trim(myfld),' ',trim(otfld)
                    call oasis_abort()
                 endif
              endif

              if (namfldops(nn) == ip_input .or. namfldops(nn) == ip_output) then
                 if (trim(myfld) /= trim(otfld)) then
                    write(nulprt,*) subname,estr,'namcouple field names do not match in/out = ', &
                       trim(myfld),' ',trim(otfld)
                    call oasis_abort()
                 endif
              endif

              !--------------------------------
              ! Only an error to find two sources for a destination
              ! Not an error if a two destinations have a single source
              !--------------------------------

              if (flag == OASIS_In .and. found) then
                 write(nulprt,*) subname,estr,'found two sources for field = ',trim(otfld)
                 call oasis_abort()
              endif
              found = .true.

              nc = nns
              if (flag == OASIS_Out) pcpointer => prism_coupler_put(nc)
              if (flag == OASIS_In)  pcpointer => prism_coupler_get(nc)

              !--------------------------------
              !>       * Generate field list, multiple field support
              !--------------------------------

              IF (OASIS_debug >= 20) THEN
                  WRITE(nulprt,*) subname,' set prism_coupler '
                  CALL oasis_flush(nulprt)
              ENDIF

              ! tcraig, changed this to make sure order of fields in list matches on all tasks
              ! Use the field lists in the namcouple
              ! Assumes all namcoupler variables are coupled
              ! The nflds counter doesn't do much anymore here
              ! The varid size should be size(myfldlist)
              ! Will need to change IF all namcoupler variables don't need to be coupled

              pcpointer%nflds = pcpointer%nflds + 1

!tcx
! this used to add fields to list one at a time
!              svarid = size(pcpointer%varid)
!              if (pcpointer%nflds > svarid) then
!                  allocate(varidtmp(svarid))
!                  varidtmp(1:svarid) = pcpointer%varid(1:svarid)
!                  deallocate(pcpointer%varid)
!                  allocate(pcpointer%varid(pcpointer%nflds+10))
!                  pcpointer%varid(1:svarid) = varidtmp(1:svarid)
!                  deallocate(varidtmp)
!              endif
!
!              if (pcpointer%nflds == 1) then
!                 pcpointer%fldlist = trim(myfld)
!              else
!                 pcpointer%fldlist = trim(pcpointer%fldlist)//':'//trim(myfld)
!              endif
!              pcpointer%varid(pcpointer%nflds) = nv1
!tcx

              if (pcpointer%nflds == 1) then
                 pcpointer%fldlist = trim(myfldlist)
                 deallocate(pcpointer%varid)
                 allocate(pcpointer%varid(oasis_string_listGetNum(myfldlist)))
                 pcpointer%varid(:) = ispval
              endif

              svarid = size(pcpointer%varid)
              if (myfldi > svarid .or. pcpointer%nflds > svarid) then
                 WRITE(nulprt,*) subname,estr,'multiple field coupling setup error',svarid,myfldi,pcpointer%nflds
                 call oasis_abort()
              endif

              pcpointer%varid(myfldi) = nv1

              !--------------------------------
              !>       * Add this coupler to list of prism_var couplers
              !--------------------------------

              prism_var(nv1)%ncpl = prism_var(nv1)%ncpl + 1
              if (prism_var(nv1)%ncpl > mvarcpl) then
                 WRITE(nulprt,*) subname,estr,'ncpl too high, max size (mvarcpl) = ',mvarcpl
                 WRITE(nulprt,*) subname,estr,'increase mvarcpl in mod_oasis_var'
                 call oasis_abort()
              endif
              prism_var(nv1)%cpl(prism_var(nv1)%ncpl) = nc

              !--------------------------------
              ! prism_coupler settings
              !>       * Copy namcouple settings into this coupler or
              !>       check that coupler is consistent with prior setting
              !--------------------------------

              if (pcpointer%valid) then
                 if (pcpointer%comp /= nm) then
                    WRITE(nulprt,*) subname,estr,'mismatch in field comp for var = ',trim(myfld)
                    call oasis_abort()
                 endif
                 if (pcpointer%namID /= nn) then
                    WRITE(nulprt,*) subname,estr,'mismatch in field namID for var = ',trim(myfld)
                    call oasis_abort()
                 endif
                 if (pcpointer%partID /= part1) then
                    WRITE(nulprt,*) subname,estr,'mismatch in field partID for var = ',trim(myfld)
                    call oasis_abort()
                 endif

             else
                 pcpointer%comp   = nm
                 pcpointer%seq    = namfldseq(nn)
                 pcpointer%dt     = namflddti(nn)
                 pcpointer%lag    = namfldlag(nn)
                 pcpointer%maxtime= namruntim
                 pcpointer%rstfile= trim(namrstfil(nn))
                 pcpointer%inpfile= trim(naminpfil(nn))
                 pcpointer%mapperID = -1
                 pcpointer%partID = part1
                 pcpointer%rpartID= part1
                 pcpointer%namID  = nn
                 pcpointer%trans  = namfldtrn(nn)
                 pcpointer%conserv= namfldcon(nn)
                 pcpointer%consopt= namfldcoo(nn)
                 pcpointer%ops    = namfldops(nn)
                 pcpointer%tag    = compid*100*1000 + compid*1000 + nn
                 pcpointer%getput = OASIS_NotDef
                 pcpointer%sndrcv = .false.
                 pcpointer%output = .false.
                 pcpointer%input  = .false.
                 pcpointer%sndmult= namfldsmu(nn)
                 pcpointer%sndadd = namfldsad(nn)
                 pcpointer%rcvmult= namflddmu(nn)
                 pcpointer%rcvadd = namflddad(nn)
                 pcpointer%snddiag= namchecki(nn)
                 pcpointer%rcvdiag= namchecko(nn)

                 !--------------------------------
                 !>       * Set prism_coupler input and output flags
                 ! prism_coupler comm flags, need for tags to match up on both sides
                 ! tags assume up to 1000 namcouple inputs and 100 models
                 !--------------------------------

                 IF (OASIS_debug >= 20) THEN
                    WRITE(nulprt,*) subname,' inout flags '
                    CALL oasis_flush(nulprt)
                 ENDIF

                 if (namfldops(nn) == ip_output .or. namfldops(nn) == ip_expout) then
                    pcpointer%output = .true.
                    pcpointer%getput = OASIS3_PUT
                 endif
                 if (namfldops(nn) == ip_input) then
                    pcpointer%input  = .true.
                    pcpointer%getput = OASIS3_GET
                 endif

                 if (namfldops(nn) == ip_exported .or. namfldops(nn) == ip_expout) then
                    pcpointer%sndrcv = .true.
                    if (flag == OASIS_Out) then
                       pcpointer%tag = nm*100*1000 + compid*1000 + nn
                       pcpointer%getput = OASIS3_PUT
                    elseif (flag == OASIS_In) then
                       pcpointer%tag = compid*100*1000 + nm*1000 + nn
                       pcpointer%getput = OASIS3_GET
                    endif
                    !--------------------------------
                    !>       * Setup prism_coupler router
                    ! cannot reuse router because don't really know what's on the other side
                    ! if router is already set for the coupler, then fine, otherwise, set new router
                    !--------------------------------
                    if (pcpointer%routerID == ispval) then
                       prism_nrouter = prism_nrouter+1
                       if (prism_nrouter > prism_mrouter) then
                          write(nulprt,*) subname,estr,'prism_nrouter too large = ',prism_nrouter,prism_mrouter
                          write(nulprt,*) subname,estr,'check prism_mrouter in oasis_coupler_setup '
                          call oasis_abort()
                       endif
                       pcpointer%routerID = prism_nrouter
                    endif
                 endif

                 !--------------------------------
                 !>       * Setup prism_coupler mapper
                 !--------------------------------

                 IF (OASIS_debug >= 20) THEN
                    WRITE(nulprt,*) subname,' mapper '
                    CALL oasis_flush(nulprt)
                 ENDIF

                 tmp_mapfile = nammapfil(nn)

                 if (trim(tmp_mapfile) == 'idmap' .and. trim(namscrmet(nn)) /= trim(cspval)) then
                    if (trim(namscrmet(nn)) == 'CONSERV') then
                       tmp_mapfile = 'rmp_'//trim(namsrcgrd(nn))//'_to_'//trim(namdstgrd(nn))//&
                                     &'_'//trim(namscrmet(nn))//'_'//trim(namscrnor(nn))//'.nc'
                    else
                       tmp_mapfile = 'rmp_'//trim(namsrcgrd(nn))//'_to_'//trim(namdstgrd(nn))//&
                                     &'_'//trim(namscrmet(nn))//'.nc'
                    endif
                 endif

                 if (trim(tmp_mapfile) /= 'idmap') then
                    pcpointer%maploc = trim(nammaploc(nn))
                    if ((flag == OASIS_In  .and. trim(nammaploc(nn)) == 'dst') .or. &
                        (flag == OASIS_Out .and. trim(nammaploc(nn)) == 'src')) then
                       !--------------------------------
                       !>       * Try to reuse mapper already defined,
                       !>       must match mapping file and partition
                       !--------------------------------
                       mapID = -1
                       do n = 1,prism_nmapper
                          if (trim(prism_mapper(n)%file) == trim(tmp_mapfile) .and. &
                              trim(prism_mapper(n)%loc ) == trim(nammaploc(nn)) .and. &
                              trim(prism_mapper(n)%opt ) == trim(nammapopt(nn))) then
                             if (flag == OASIS_In  .and. prism_mapper(n)%dpart == part1) mapID = n
                             if (flag == OASIS_Out .and. prism_mapper(n)%spart == part1) mapID = n
                          endif
                       enddo
                       !--------------------------------
                       !>       * Or get ready to initialize a new mapper
                       !--------------------------------
                       if (mapID < 1) then
                          prism_nmapper = prism_nmapper + 1
                          if (prism_nmapper > prism_mmapper) then
                             write(nulprt,*) subname,estr,'prism_nmapper too large',prism_nmapper,prism_mmapper
                             write(nulprt,*) subname,estr,'check prism_mmapper in oasis_coupler_setup '
                             call oasis_abort()
                          endif
                          mapID = prism_nmapper
                          prism_mapper(mapID)%file = trim(tmp_mapfile)
                          prism_mapper(mapID)%loc  = trim(nammaploc(nn))
                          prism_mapper(mapID)%opt  = trim(nammapopt(nn))
                          prism_mapper(mapID)%srcgrid = trim(namsrcgrd(nn))
                          prism_mapper(mapID)%dstgrid = trim(namdstgrd(nn))
                          if (flag == OASIS_In ) prism_mapper(mapID)%dpart = part1
                          if (flag == OASIS_Out) prism_mapper(mapID)%spart = part1
                          if (OASIS_debug > 15) then
                             write(nulprt,*) subname,' DEBUG new mapper for file ',&
                                             trim(prism_mapper(mapID)%file)
                             call oasis_flush(nulprt)
                          endif
                       endif
                       pcpointer%mapperID = mapID
                    endif  ! flag and nammaploc match
                 endif  ! nammapfil

                 pcpointer%valid  = .true.

              endif  !  valid

              if (local_timers_on) call oasis_timer_stop('cpl_setup_n3d')

           enddo  ! nvf

        endif  ! my var found

     enddo  ! nfind
  enddo  ! nv1
  if (local_timers_on) call oasis_timer_stop ('cpl_setup_n3')
  if (local_timers_on) call oasis_timer_start('cpl_setup_n4')
  if (local_timers_on) call oasis_timer_start('cpl_setup_n4a')

  ! aggregate checkused info across all pes and then check on each component root
  allocate(namsrc_checkused_g(sortnsrc%num))
  call oasis_mpi_max(namsrc_checkused,namsrc_checkused_g,mpi_comm_global,string=trim(subname)//':srccheckused',all=.true.)
  found = .false.
  do n1 = 1,sortnsrc%num
     if (namsrc_checkused_g(n1) /= 1) then
        if (mpi_rank_local == 0) write(nulprt,*) subname,estr,'namcouple variable not used: ',trim(sortnsrc%fld(n1))
        found = .true.
     endif
  enddo
!  call oasis_mpi_barrier(mpi_comm_global)
  if (found) call oasis_abort()
  deallocate(namsrc_checkused_g)

  !--- deallocate temporary ---
  deallocate(allvar,nallvar,allops)
  deallocate(namsrc_checkused)
  deallocate(sortnsrc%fld)
  deallocate(sortnsrc%namnum)
  deallocate(sortnsrc%fldnum)
  deallocate(sortndst%fld)
  deallocate(sortndst%namnum)
  deallocate(sortndst%fldnum)
  deallocate(sortvars%fld)
  deallocate(sortvars%modnum)
  deallocate(sortvars%varnum)

  if (OASIS_debug >= 20) then
     write(nulprt,*) ' '
     write(nulprt,*) subname,' couplers setup'
     do nc = 1,prism_mcoupler
!tcx can't write here, something uninitialized???
!-> CEG it was dpart so added extra if into the print routine
!        if (prism_coupler_put(nc)%valid) call prism_coupler_print(nc,prism_coupler_put(nc))
!        if (prism_coupler_get(nc)%valid) call prism_coupler_print(nc,prism_coupler_get(nc))
     enddo
     write(nulprt,*) ' '
     call oasis_flush(nulprt)
  endif

  if (mpi_comm_local == MPI_COMM_NULL) then
     return
  endif

  !----------------------------------------------------------
  !> * Initialize coupling infrastructure based on initial coupler setup above
  !----------------------------------------------------------

  call oasis_debug_note(subname//' initialize coupling datatypes')

  !----------------------------------------------------------
  !> * Loop over all couplers
  !----------------------------------------------------------

  if (local_timers_on) call oasis_timer_stop('cpl_setup_n4a')

  do nc = 1,prism_mcoupler
  do npc = 1,2
  if (npc == 1) then
     pcpointer => prism_coupler_put(nc)
     pcpntpair => prism_coupler_get(nc)
  endif
  if (npc == 2) then
     pcpointer => prism_coupler_get(nc)
     pcpntpair => prism_coupler_put(nc)
  endif
  if (OASIS_debug >= 20) then
     write(nulprt,*) subname,' DEBUG cb:initialize coupler ',nc,npc,pcpointer%valid
     call oasis_flush(nulprt)
  endif

  if (pcpointer%valid) then
     if (local_timers_on) call oasis_timer_start('cpl_setup_n4b')
     if (OASIS_debug >= 5) then
        write(nulprt,*) subname,' DEBUG ci:initialize coupler ',nc,npc
        call oasis_flush(nulprt)
     endif

     namID = pcpointer%namID
     part1 = pcpointer%partID
     mapID = pcpointer%mapperID

     if (part1 <= 0) then
        write(nulprt,*) subname,estr,'part1 invalid = ',part1
        call oasis_abort()
     endif

     !--------------------------------
     !>   * Initialize avect1 which stores the get/put data
     !--------------------------------

     gsize = mct_gsmap_gsize(prism_part(part1)%gsmap)
     lsize = mct_gsmap_lsize(prism_part(part1)%gsmap,mpi_comm_local)
     if (OASIS_debug >= 15) then
        write(nulprt,'(1x,2a,5i10)') subname,' DEBUG ci:part1 info ',namID,part1,mapID,gsize,lsize
        write(nulprt,'(1x,2a,4i12)') subname,' DEBUG ci:part1a',prism_part(part1)%gsmap%ngseg,&
                                     prism_part(part1)%gsmap%gsize
        do n1 = 1,prism_part(part1)%gsmap%ngseg
           write(nulprt,'(1x,2a,4i12)') subname,' DEBUG ci:part1b',n1,&
                                        prism_part(part1)%gsmap%start(n1),&
                                        prism_part(part1)%gsmap%length(n1),&
                                        prism_part(part1)%gsmap%pe_loc(n1)
        enddo
        call oasis_flush(nulprt)
     endif
     call mct_avect_init(pcpointer%avect1,rList=trim(pcpointer%fldlist),lsize=lsize)
     call mct_avect_zero(pcpointer%avect1)
     pcpointer%aVon(1) = .true.
     if (OASIS_debug >= 15) then
        write(nulprt,*) subname,' DEBUG ci:avect1 initialized '
        call oasis_flush(nulprt)
     endif

     !--------------------------------
     !>   * Compute nflds for this coupling and initialize avcnt and status
     !--------------------------------

     pcpointer%nflds = mct_aVect_nRAttr(pcpointer%avect1)
     allocate(pcpointer%status(pcpointer%nflds))
     allocate(pcpointer%avcnt (pcpointer%nflds))
     pcpointer%avcnt(:) = 0
     if (pcpointer%getput == OASIS3_PUT) pcpointer%status = OASIS_COMM_WAIT
     if (pcpointer%getput == OASIS3_GET) pcpointer%status = OASIS_COMM_READY
     if (local_timers_on) call oasis_timer_stop('cpl_setup_n4b')

     !--------------------------------
     !>   * Initialize the mapper data
     !--------------------------------

     if (mapID > 0) then

        if (prism_mapper(mapID)%init) then
           if (local_timers_on) call oasis_timer_start('cpl_setup_n4c')
           !--------------------------------
           ! mapper already initialized
           !--------------------------------
           if (pcpointer%getput == OASIS3_PUT) then
              part2 = prism_mapper(mapID)%dpart
           else
              part2 = prism_mapper(mapID)%spart
           endif
           gsize = mct_gsmap_gsize(prism_part(part2)%gsmap)
           if (local_timers_on) call oasis_timer_stop('cpl_setup_n4c')
        else
           !--------------------------------
           ! instantiate mapper
           ! read/generate mapping file
           ! read non local grid size
           ! get gsmap for non local grid
           ! read mapping weights and initialize sMatP
           !--------------------------------
           if (local_timers_on) call oasis_timer_start('cpl_setup_n4d')
           if (OASIS_debug >= 15) then
              write(nulprt,*) subname,' DEBUG ci:read mapfile ',trim(prism_mapper(mapID)%file)
              call oasis_flush(nulprt)
           endif
           if (mpi_rank_local == 0) then
              if (local_timers_on) call oasis_timer_start('cpl_setup_n4da')
              if (local_timers_on) call oasis_timer_start('cpl_setup_n4da1')
              inquire(file=trim(prism_mapper(mapID)%file),exist=exists)
              if (local_timers_on) call oasis_timer_stop('cpl_setup_n4da1')
              if (OASIS_debug >= 15) then
                 write(nulprt,*) subname,' DEBUG ci: inquire mapfile ',&
                                 trim(prism_mapper(mapID)%file),exists
                 call oasis_flush(nulprt)
              endif
              if (.not.exists) then
                 if (trim(namscrmet(namID)) /= trim(cspval)) then
                    !--------------------------------
                    ! generate mapping file on root pe
                    ! taken from oasis3 scriprmp
                    !--------------------------------
                    call oasis_timer_start('cpl_setup_genmap')
                    call oasis_map_genmap(mapID,namID)
                    call oasis_timer_stop('cpl_setup_genmap')
                 else
                    write(nulprt,*) subname,estr,'map file does not exist and SCRIPR not set = ',&
                                    trim(prism_mapper(mapID)%file)
                    call oasis_abort()
                 endif
              endif

              !--------------------------------
              ! open mapping file
              !--------------------------------
              if (local_timers_on) call oasis_timer_start('cpl_setup_n4da3')
              status = nf90_open(trim(prism_mapper(mapID)%file),nf90_nowrite,ncid)
              if (OASIS_debug >= 15) then
                 status = nf90_inq_dimid(ncid,'dst_grid_size',dimid)
                 status = nf90_inquire_dimension(ncid,dimid,len=gsize)
                 write(nulprt,*) subname," DEBUG dst_grid_size ",gsize
                 status = nf90_inq_dimid(ncid,'src_grid_size',dimid)
                 status = nf90_inquire_dimension(ncid,dimid,len=gsize)
                 write(nulprt,*) subname," DEBUG src_grid_size ",gsize
              endif
              if (pcpointer%getput == OASIS3_PUT) &
                 status = nf90_inq_dimid(ncid,'dst_grid_size',dimid)
              if (pcpointer%getput == OASIS3_GET) &
                 status = nf90_inq_dimid(ncid,'src_grid_size',dimid)
              status = nf90_inquire_dimension(ncid,dimid,len=gsize)
              if (local_timers_on) call oasis_timer_stop('cpl_setup_n4da3')
              if (local_timers_on) call oasis_timer_stop('cpl_setup_n4da')
           endif  ! rank = 0
           if (local_timers_on) call oasis_timer_start('cpl_setup_n4db')
           call oasis_mpi_bcast(gsize,mpi_comm_local,subname//' gsize')
           if (local_timers_on) call oasis_timer_stop('cpl_setup_n4db')

           if (local_timers_on) call oasis_timer_start('cpl_setup_n4dc')
           if (pcpointer%getput == OASIS3_PUT) then
              nx = namdst_nx(namID)
              ny = namdst_ny(namID)
              gridname = trim(namdstgrd(namID))
           else
              nx = namsrc_nx(namID)
              ny = namsrc_ny(namID)
              gridname = trim(namsrcgrd(namID))
           endif
           if (local_timers_on) call oasis_timer_stop('cpl_setup_n4dc')

           !tcx improve match here with nx,ny,gridname 
           if (local_timers_on) call oasis_timer_start('cpl_setup_n4dd')
           call oasis_part_create(part2,'1d',gsize,nx,ny,gridname,prism_part(part1)%mpicom,mpi_comm_local)
           if (local_timers_on) call oasis_timer_stop('cpl_setup_n4dd')

           if (OASIS_Debug >= 15) then
              write(nulprt,*) subname," DEBUG part_create part1 gsize",prism_part(part1)%gsize
              do r1 = 1,prism_part(part1)%gsmap%ngseg
                 write(nulprt,*) subname," DEBUG part_create part1 info ",&
                                 prism_part(part1)%gsmap%start(r1),prism_part(part1)%gsmap%length(r1),&
                                 prism_part(part1)%gsmap%pe_loc(r1)
              enddo

              write(nulprt,*) subname," DEBUG part_create part2 gsize",prism_part(part2)%gsize
              do r1 = 1,prism_part(part2)%gsmap%ngseg
                 write(nulprt,*) subname," DEBUG part_create part2 info ",prism_part(part2)%gsmap%start(r1),&
                                 prism_part(part2)%gsmap%length(r1),prism_part(part2)%gsmap%pe_loc(r1)
              enddo
           endif

           if (local_timers_on) call oasis_timer_start('cpl_setup_n4de')
           if (prism_part(part2)%nx < 1) then
              prism_part(part2)%nx = nx
              prism_part(part2)%ny = ny
              prism_part(part2)%gridname = trim(gridname)
           endif

           if (pcpointer%getput == OASIS3_PUT) then
             !prism_mapper(mapID)%spart = part1   ! set above
              prism_mapper(mapID)%dpart = part2
           else
              prism_mapper(mapID)%spart = part2
             !prism_mapper(mapID)%dpart = part1   ! set above
           endif
           spart = prism_mapper(mapID)%spart
           dpart = prism_mapper(mapID)%dpart

           !--- cstring sets whether src or dst are rearranged in remap
           !--- src = rearrange and map (bfb), dst = map and rearrange (partial sum)
           if (prism_mapper(mapID)%opt == 'opt') then
              if (prism_part(spart)%gsize > prism_part(dpart)%gsize) then
                 cstring = 'dst'
              else
                 cstring = 'src'
              endif
           elseif (prism_mapper(mapID)%opt == 'bfb') then
              cstring = 'src'
           elseif (prism_mapper(mapID)%opt == 'sum') then
              cstring = 'dst'
           else
              write(nulprt,*) subname,estr,'mapper opt invalid expect bfb or sum  =',trim(prism_mapper(mapID)%opt)
              call oasis_abort()
           endif
           if (prism_mapper(mapID)%optval /= '' .and. &
               prism_mapper(mapID)%optval /= trim(cstring)) then
              write(nulprt,*) subname,estr,'mapper opt changed',&
                              trim(prism_mapper(mapID)%optval),' ',trim(cstring)
              call oasis_abort()
           endif
           prism_mapper(mapID)%optval = trim(cstring)
           if (local_timers_on) call oasis_timer_stop('cpl_setup_n4de')
           if (local_timers_on) call oasis_timer_stop('cpl_setup_n4d')

           !-------------------------------
           ! smatreaddnc allocates sMati to nwgts
           ! then instantiate an sMatP for each set of wgts
           ! to support higher order mapping
           !-------------------------------
           if (smatread_method == "ceg") then
              if (local_timers_on) call oasis_timer_start('smatrd_ceg')
              call oasis_map_smatreaddnc_ceg(sMati,prism_part(spart)%gsmap,prism_part(dpart)%gsmap, &
                 trim(cstring),trim(prism_mapper(mapID)%file),mpi_rank_local,mpi_comm_local,nwgts)
              if (local_timers_on) call oasis_timer_stop('smatrd_ceg')
           else
              if (local_timers_on) call oasis_timer_start('smatrd_orig')
              call oasis_map_smatreaddnc_orig(sMati,prism_part(spart)%gsmap,prism_part(dpart)%gsmap, &
                 trim(cstring),trim(prism_mapper(mapID)%file),mpi_rank_local,mpi_comm_local,nwgts)
              if (local_timers_on) call oasis_timer_stop('smatrd_orig')
           endif
           if (local_timers_on) call oasis_timer_start('cpl_setup_sminit')
           prism_mapper(mapID)%nwgts = nwgts
           allocate(prism_mapper(mapID)%sMatP(nwgts))
           do n = 1,nwgts
              call mct_sMatP_Init(prism_mapper(mapID)%sMatP(n), sMati(n), &
                 prism_part(spart)%gsmap, prism_part(dpart)%gsmap, 0, mpi_comm_local, compid)
              call mct_sMat_Clean(sMati(n))
           enddo
           deallocate(sMati)
           if (local_timers_on) call oasis_timer_stop('cpl_setup_sminit')
           if (local_timers_on) call oasis_timer_start('cpl_setup_n4e')

           lsize = mct_smat_gNumEl(prism_mapper(mapID)%sMatP(1)%Matrix,mpi_comm_local)
           prism_mapper(mapID)%init = .true.
           if (OASIS_debug >= 15) then
              write(nulprt,*) subname," DEBUG ci:done initializing prism_mapper",mapID,&
                              " nElements = ",lsize," nwgts = ",nwgts
              call oasis_flush(nulprt)
           endif
           if (local_timers_on) call oasis_timer_stop('cpl_setup_n4e')
        endif  ! map init

        if (local_timers_on) call oasis_timer_start('cpl_setup_n4f')
        !--------------------------------
        !>   * Read mapper mask and area if not already done
        !--------------------------------
        if (.not.prism_mapper(mapID)%AVred .and. pcpointer%conserv /= ip_cnone) then
           ! initialize and load AV_ms and AV_md

           spart = prism_mapper(mapID)%spart
           dpart = prism_mapper(mapID)%dpart

           lsize = mct_gsmap_lsize(prism_part(spart)%gsmap,mpi_comm_local)
           call mct_avect_init(prism_mapper(mapID)%av_ms,iList='mask',rList='area',lsize=lsize)
           call mct_avect_zero(prism_mapper(mapID)%av_ms)
!           gridname = prism_part(spart)%gridname
           gridname=prism_mapper(mapID)%srcgrid
           call oasis_io_read_avfld('masks.nc',prism_mapper(mapID)%av_ms, &
              prism_part(spart)%gsmap,mpi_comm_local,'mask',trim(gridname)//'.msk',fldtype='int')
           call oasis_io_read_avfld('areas.nc',prism_mapper(mapID)%av_ms, &
              prism_part(spart)%gsmap,mpi_comm_local,'area',trim(gridname)//'.srf',fldtype='real')

           lsize = mct_gsmap_lsize(prism_part(dpart)%gsmap,mpi_comm_local)
           call mct_avect_init(prism_mapper(mapID)%av_md,iList='mask',rList='area',lsize=lsize)
           call mct_avect_zero(prism_mapper(mapID)%av_md)
!           gridname = prism_part(dpart)%gridname
           gridname=prism_mapper(mapID)%dstgrid
           call oasis_io_read_avfld('masks.nc',prism_mapper(mapID)%av_md, &
              prism_part(dpart)%gsmap,mpi_comm_local,'mask',trim(gridname)//'.msk',fldtype='int')
           call oasis_io_read_avfld('areas.nc',prism_mapper(mapID)%av_md, &
              prism_part(dpart)%gsmap,mpi_comm_local,'area',trim(gridname)//'.srf',fldtype='real')

           prism_mapper(mapID)%AVred = .true.

           if (OASIS_debug >= 30) then
              write(nulprt,*) subname,' DEBUG msi ',minval(prism_mapper(mapID)%av_ms%iAttr(:,:)),&
                              maxval(prism_mapper(mapID)%av_ms%iAttr(:,:)),&
                              sum(prism_mapper(mapID)%av_ms%iAttr(:,:))
              write(nulprt,*) subname,' DEBIG msr ',minval(prism_mapper(mapID)%av_ms%rAttr(:,:)),&
                              maxval(prism_mapper(mapID)%av_ms%rAttr(:,:)),&
                              sum(prism_mapper(mapID)%av_ms%rAttr(:,:))
              write(nulprt,*) subname,' DEBUG mdi ',minval(prism_mapper(mapID)%av_md%iAttr(:,:)),&
                              maxval(prism_mapper(mapID)%av_md%iAttr(:,:)),&
                              sum(prism_mapper(mapID)%av_md%iAttr(:,:))
              write(nulprt,*) subname,' DEBUG mdr ',minval(prism_mapper(mapID)%av_md%rAttr(:,:)),&
                              maxval(prism_mapper(mapID)%av_md%rAttr(:,:)),&
                              sum(prism_mapper(mapID)%av_md%rAttr(:,:))
              CALL oasis_flush(nulprt)
           endif
        endif

        !--------------------------------
        !>   * Initialize avect1m, the data in avect1 mapped to another grid
        !--------------------------------

        lsize = mct_gsmap_lsize(prism_part(part2)%gsmap,mpi_comm_local)
        if (OASIS_debug >= 15) then
           write(nulprt,'(1x,2a,4i12)') subname,' DEBUG ci:part2 info ',part2,mapID,gsize,lsize
           write(nulprt,'(1x,2a,4i12)') subname,' DEBUG ci:part2a',prism_part(part2)%gsmap%ngseg,&
                                        prism_part(part2)%gsmap%gsize
           do n1 = 1,prism_part(part2)%gsmap%ngseg
              write(nulprt,'(1x,2a,4i12)') subname,' DEBUG ci:part2b',n1,prism_part(part2)%gsmap%start(n1),&
                                           prism_part(part2)%gsmap%length(n1),prism_part(part2)%gsmap%pe_loc(n1)
           enddo
           call oasis_flush(nulprt)
        endif
        call mct_avect_init(pcpointer%avect1m,rList=trim(pcpointer%fldlist),lsize=lsize)
        call mct_avect_zero(pcpointer%avect1m)
        if (OASIS_debug >= 15) then
           write(nulprt,*) subname,' DEBUG ci:avect1m initialized '
           call oasis_flush(nulprt)
        endif

        !--------------------------------
        ! router partition is always other part
        !--------------------------------

        pcpointer%rpartID = part2
        if (local_timers_on) call oasis_timer_stop('cpl_setup_n4f')
     else

        !--------------------------------
        ! router partition is just coupler part
        ! Set to this by default above
        !--------------------------------

        ! pcpointer%rpartID = pcpointer%partID

     endif  ! no mapper

   endif ! endif of pcpointer%valid
   
!   print'(I3,A,X,L,X,8(I8,X))',mpi_rank_global, 'would have done sndrcv here', pcpointer%sndrcv,pcpointer%comp,compid, &
!           pcpointer%valid, mapID, pcpointer%rPartID, pcpointer%routerID

  enddo   ! npc
  enddo   ! nc

!-------------------------------------------------
! CEG split 1 loop into 2 to allow map reading on different models in parallel.
!-------------------------------------------------

  do nc = 1, prism_mcoupler  ! nc
  do npc=1,2

     if (npc == 1) then
        pcpointer => prism_coupler_put(nc)
        pcpntpair => prism_coupler_get(nc)
     endif
     if (npc == 2) then
        pcpointer => prism_coupler_get(nc)
        pcpntpair => prism_coupler_put(nc)
     endif

     namID = pcpointer%namID
     part1 = pcpointer%partID
     mapID = pcpointer%mapperID

!   print'(I3,A,X,L,X,8(I8,X))',mpi_rank_global, '..finally doing sndrcv here', pcpointer%sndrcv, pcpointer%comp, compid, &
!           pcpointer%valid, mapID, pcpointer%rPartID, pcpointer%routerID
!     if (mapID > 0) then

     !--------------------------------
     !>   * Initialize router based on rpartID
     !--------------------------------

     if (local_timers_on) call oasis_timer_start('cpl_setup_n4_sr')
     if (pcpointer%sndrcv) then

        if (OASIS_debug >= 15) then
           write(nulprt,*) subname,' DEBUG ci:initialize router ',pcpointer%routerID,&
                           pcpointer%comp,pcpointer%rpartID
           call oasis_flush(nulprt)
        endif

        if (compid == pcpointer%comp) then
           ! routers for sending to self
           ! setup router on second pass so rpartID is defined on both sides
           ! setup both routers at the same time
           if (local_timers_on) call oasis_timer_start('cpl_setup_n4_sra')
           if (npc == 2) then
              if (OASIS_debug >= 15) then
                 write(nulprt,*) subname,' DEBUG self router between part ',pcpointer%rpartID,' and part ',pcpntpair%rpartID, &
                    ' with router ',pcpointer%routerID,' and router ',pcpntpair%routerID,' for compid ',compid
              endif
              call mct_router_init(prism_part(pcpointer%rpartID)%gsmap, prism_part(pcpntpair%rpartID)%gsmap, &
                 mpi_comm_local, prism_router(pcpointer%routerID)%router)
              call mct_router_init(prism_part(pcpntpair%rpartID)%gsmap, prism_part(pcpointer%rpartID)%gsmap, &
                 mpi_comm_local, prism_router(pcpntpair%routerID)%router)

              if (OASIS_debug >= 15) then
                 write(nulprt,*) subname," DEBUG ci:done initializing prism_router",&
                                 pcpointer%routerID
                 if (OASIS_debug >= 20) then
                    do r1 = 1,prism_part(pcpointer%rpartID)%gsmap%ngseg
                       write(nulprt,*) subname," DEBUG router gs1 info ",prism_part(pcpointer%rpartID)%gsmap%start(r1),&
                                       prism_part(pcpointer%rpartID)%gsmap%length(r1),prism_part(pcpointer%rpartID)%gsmap%pe_loc(r1)
                    enddo
                    do r1 = 1,prism_part(pcpointer%partID)%gsmap%ngseg
                       write(nulprt,*) subname," DEBUG router gs2 info ",prism_part(pcpointer%partID)%gsmap%start(r1),&
                                       prism_part(pcpointer%partID)%gsmap%length(r1),prism_part(pcpointer%partID)%gsmap%pe_loc(r1)
                    enddo
                    do r1 = 1,prism_part(pcpntpair%rpartID)%gsmap%ngseg
                       write(nulprt,*) subname," DEBUG router gs3 info ",prism_part(pcpntpair%rpartID)%gsmap%start(r1),&
                                       prism_part(pcpntpair%rpartID)%gsmap%length(r1),prism_part(pcpntpair%rpartID)%gsmap%pe_loc(r1)
                    enddo
                    do r1 = 1,prism_part(pcpntpair%partid)%gsmap%ngseg
                       write(nulprt,*) subname," DEBUG router gs4 info ",prism_part(pcpntpair%partid)%gsmap%start(r1),&
                                       prism_part(pcpntpair%partid)%gsmap%length(r1),prism_part(pcpntpair%partid)%gsmap%pe_loc(r1)
                    enddo
                    do r1 = 1,prism_router(pcpointer%routerID)%router%nprocs
                       write(nulprt,*) subname," DEBUG router info ",pcpointer%routerID,r1, &
                          prism_router(pcpointer%routerID)%router%pe_list(r1),prism_router(pcpointer%routerID)%router%locsize(r1)
                    enddo
                 endif
                 call oasis_flush(nulprt)
              endif

              if (OASIS_debug >= 15) then
                 write(nulprt,*) subname," DEBUG ci:done initializing prism_router",&
                                 pcpntpair%routerID
                 if (OASIS_debug >= 20) then
                    do r1 = 1,prism_router(pcpntpair%routerID)%router%nprocs
                       write(nulprt,*) subname," DEBUG router info ",pcpntpair%routerID,r1, &
                          prism_router(pcpntpair%routerID)%router%pe_list(r1),prism_router(pcpntpair%routerID)%router%locsize(r1)
                    enddo
                 endif
                 call oasis_flush(nulprt)
              endif
           endif

           if (local_timers_on) call oasis_timer_stop('cpl_setup_n4_sra')
        else
           if (local_timers_on) call oasis_timer_start('cpl_setup_n4_srb')

           call mct_router_init(pcpointer%comp,prism_part(pcpointer%rpartID)%gsmap, &
              mpi_comm_local,prism_router(pcpointer%routerID)%router)

           if (OASIS_debug >= 15) then
              write(nulprt,*) subname," DEBUG ci:done initializing prism_router",&
                              pcpointer%routerID
              if (OASIS_debug >= 20) then
                 do r1 = 1,prism_router(pcpointer%routerID)%router%nprocs
                    write(nulprt,*) subname," DEBUG router info ",pcpointer%routerID,r1, &
                       prism_router(pcpointer%routerID)%router%pe_list(r1),prism_router(pcpointer%routerID)%router%locsize(r1)
                 enddo
              endif
              call oasis_flush(nulprt)
           endif
           if (local_timers_on) call oasis_timer_stop('cpl_setup_n4_srb')

        endif

     endif
     if (local_timers_on) call oasis_timer_stop('cpl_setup_n4_sr')

  enddo   ! npc
  enddo   ! prism_mcoupler

  if (local_timers_on) call oasis_timer_start('cpl_setup_n4g')
  !----------------------------------------------------------
  !> * Diagnostics for all couplers
  !----------------------------------------------------------

  if (OASIS_debug >= 2) then
     write(nulprt,*) ' '
     write(nulprt,*) subname,' couplers initialized'
     do nc = 1,prism_mcoupler
        if (prism_coupler_put(nc)%valid) call oasis_coupler_print(nc,prism_coupler_put(nc))
        if (prism_coupler_get(nc)%valid) call oasis_coupler_print(nc,prism_coupler_get(nc))
     enddo
     write(nulprt,*) ' '
     CALL oasis_flush(nulprt)
  endif

  IF (LUCIA_debug > 0) THEN
     DO nc = 1, prism_mcoupler
        IF (prism_coupler_put(nc)%valid) &
           WRITE(nullucia, '(A12,I4.4,1X,A)') 'Balance: SN ', prism_coupler_put(nc)%namID, TRIM(prism_coupler_put(nc)%fldlist)
        IF (prism_coupler_get(nc)%valid) &
           WRITE(nullucia, '(A12,I4.4,1X,A)') 'Balance: RC ', prism_coupler_get(nc)%namID, TRIM(prism_coupler_get(nc)%fldlist)
     ENDDO
  ENDIF

  if (local_timers_on) call oasis_timer_stop ('cpl_setup_n4g')
  if (local_timers_on) call oasis_timer_stop ('cpl_setup_n4')
  IF (local_timers_on) call oasis_timer_stop('cpl_setup')

  call oasis_debug_exit(subname)

  END SUBROUTINE oasis_coupler_setup

!------------------------------------------------------------

!> Print routine for oasis_couplers

  SUBROUTINE oasis_coupler_print(cplid,pcprint)

  IMPLICIT NONE

  integer(ip_i4_p),         intent(in) :: cplid   !< coupler id
  type(prism_coupler_type), intent(in) :: pcprint !< specific prism_coupler
  !----------------------------------------------------------
  integer(ip_i4_p) :: mapid, rouid, parid, namid, nflds, rpard
  integer(ip_i4_p) :: spart,dpart
  character(len=*),parameter :: subname = '(oasis_coupler_print)'

  call oasis_debug_enter(subname)

  mapid = pcprint%mapperid
  rouid = pcprint%routerid
  parid = pcprint%partid
  rpard = pcprint%rpartid
  namid = pcprint%namid
  nflds = pcprint%nflds

     write(nulprt,*) ' '
     write(nulprt,*) subname,' model and cplid',compid,cplid
  if (pcprint%getput == OASIS3_PUT) then
     write(nulprt,*) subname,'   send fields      ',trim(pcprint%fldlist)
     write(nulprt,*) subname,'   from model       ',compid
     write(nulprt,*) subname,'   to model         ',pcprint%comp
     write(nulprt,*) subname,'   using router     ',rouid
     write(nulprt,*) subname,'   transform        ',pcprint%trans
     write(nulprt,*) subname,'   snd diagnose     ',pcprint%snddiag
     write(nulprt,*) subname,'   snd fld mult     ',pcprint%sndmult
     write(nulprt,*) subname,'   snd fld add      ',pcprint%sndadd
  endif
  if (pcprint%getput == OASIS3_GET) then
     write(nulprt,*) subname,'   recv fields      ',trim(pcprint%fldlist)
     write(nulprt,*) subname,'   from model       ',pcprint%comp
     write(nulprt,*) subname,'   to model         ',compid
     write(nulprt,*) subname,'   using router     ',rouid
     write(nulprt,*) subname,'   rcv diagnose     ',pcprint%rcvdiag
     write(nulprt,*) subname,'   rcv fld mult     ',pcprint%rcvmult
     write(nulprt,*) subname,'   rcv fld add      ',pcprint%rcvadd
  endif
     write(nulprt,*) subname,'   namcouple op     ',pcprint%ops
     write(nulprt,*) subname,'   valid            ',pcprint%valid
     write(nulprt,*) subname,'   namcouple id     ',namid
     write(nulprt,*) subname,'   variable ids     ',pcprint%varid(1:nflds)
     write(nulprt,*) subname,'   sndrcv flag      ',pcprint%sndrcv
     write(nulprt,*) subname,'   output flag      ',pcprint%output
     write(nulprt,*) subname,'   input flag       ',pcprint%input
     write(nulprt,*) subname,'   input file       ',trim(pcprint%inpfile)
     write(nulprt,*) subname,'   restart file     ',trim(pcprint%rstfile)
     write(nulprt,*) subname,'   tag              ',pcprint%tag
     write(nulprt,*) subname,'   seq              ',pcprint%seq
     write(nulprt,*) subname,'   maxtime          ',pcprint%maxtime
     write(nulprt,*) subname,'   dt, lag          ',pcprint%dt,pcprint%lag
!     write(nulprt,*) subname,'   partid, size ',parid,trim(prism_part(parid)%gridname),&
!                                               prism_part(parid)%gsize
     write(nulprt,*) subname,'   partid, size     ',parid,prism_part(parid)%gsize                                                
     write(nulprt,*) subname,'   partid, nx,ny    ',prism_part(parid)%nx,prism_part(parid)%ny
!     write(nulprt,*) subname,'   rpartid,size ',rpard,trim(prism_part(rpard)%gridname),&
!                                                prism_part(rpard)%gsize
     write(nulprt,*) subname,'   rpartid,size     ',rpard,prism_part(rpard)%gsize
     write(nulprt,*) subname,'   rpartid,nx,ny    ',prism_part(rpard)%nx,prism_part(rpard)%ny
     write(nulprt,*) subname,'   maploc           ',trim(pcprint%maploc)

  if (mapid > 0) then
     WRITE(nulprt,*) subname,'   src grid        :',trim(prism_mapper(mapid)%srcgrid)
     WRITE(nulprt,*) subname,'   dst grid        :',trim(prism_mapper(mapid)%dstgrid)
     write(nulprt,*) subname,'   use map          ',mapid,trim(prism_mapper(mapid)%file)
     write(nulprt,*) subname,'   nwgts            ',mapid,prism_mapper(mapid)%nwgts
     spart = prism_mapper(mapid)%spart
     dpart = prism_mapper(mapid)%dpart
     write(nulprt,*) subname,'   conserve         ',pcprint%conserv
     write(nulprt,*) subname,'   conserve opt     ',pcprint%consopt
     write(nulprt,*) subname,'   location         ',trim(prism_mapper(mapid)%loc)
     write(nulprt,*) subname,'   opt,optval       ',trim(prism_mapper(mapid)%opt),' ',&
                                                    trim(prism_mapper(mapid)%optval)
     write(nulprt,*) subname,'   s/d partids      ',spart,dpart
     if (spart > 0) &
     write(nulprt,*) subname,'   from/to partition',trim(prism_part(spart)%gridname),' ',&
                                                    trim(prism_part(dpart)%gridname)
     write(nulprt,*) subname,'   from nx,ny       ',prism_part(spart)%gsize,prism_part(spart)%nx,&
                                                    prism_part(spart)%ny
     if (dpart > 0) &
     write(nulprt,*) subname,'   to nx,ny         ',prism_part(dpart)%gsize, prism_part(dpart)%nx,&
                                                    prism_part(dpart)%ny
  endif  ! mapid > 0

  call oasis_flush(nulprt)

  call oasis_debug_exit(subname)

  END SUBROUTINE oasis_coupler_print

!------------------------------------------------------------
! !BOP ===========================================================================
!
!> Sort a character array using a sort key.
!
! !DESCRIPTION: 
!     Sort a character array and the associated array(s) based on a
!     reasonably fast sort algorithm
!
! !INTERFACE:  -----------------------------------------------------------------

subroutine cplsort(num, fld, sortkey)

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p
   integer,parameter :: CL = ic_lvar

! !INPUT/OUTPUT PARAMETERS:

   integer(IN),      intent(in)    :: num        !< size of array
   character(len=CL),intent(inout) :: fld(:)     !< sort field
   integer(IN)      ,intent(inout) :: sortkey(:) !< sortkey

! !EOP

   !--- local ---
   integer(IN)    :: n1,n2
   logical        :: stopnow
   character(CL), pointer :: tmpfld(:)
   integer(IN)  , pointer :: tmpkey(:)

   !--- formats ---
   character(*),parameter :: subName = '(cplsort) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   call oasis_debug_enter(subname)

   allocate(tmpfld((num+1)/2))
   allocate(tmpkey((num+1)/2))
   call MergeSort(num,fld,tmpfld,sortkey,tmpkey)
   deallocate(tmpfld)
   deallocate(tmpkey)
    
!   call oasis_debug_exit(subname)

end subroutine cplsort

!------------------------------------------------------------
! !BOP ===========================================================================
!
!> Sort an integer array using a sort key.
!
! !DESCRIPTION: 
!     Rearrange and integer array based on an input sortkey
!
! !INTERFACE:  -----------------------------------------------------------------

subroutine cplsortkey(num, arr, sortkey)

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p
   integer,parameter :: CL = ic_lvar

! !INPUT/OUTPUT PARAMETERS:

   integer(IN),intent(in)    :: num        !< size of array
   integer(IN),intent(inout) :: arr(:)     !< field to sort
   integer(IN),intent(in)    :: sortkey(:) !< sortkey

! !EOP

   !--- local ---
   integer(IN)    :: n1,n2
   integer(IN), pointer :: tmparr(:)

   !--- formats ---
   character(*),parameter :: subName = '(cplsortkey) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   call oasis_debug_enter(subname)

   if (num /= size(arr) .or. num /= size(sortkey)) then
      WRITE(nulprt,*) subname,estr,'on size of input arrays :',num,size(arr),size(sortkey)
      call oasis_abort()
   endif

   allocate(tmparr(num))
   tmparr(1:num) = arr(1:num)
   do n1 = 1,num
      arr(n1) = tmparr(sortkey(n1))
   enddo
   deallocate(tmparr)
    
!   call oasis_debug_exit(subname)

end subroutine cplsortkey

!------------------------------------------------------------
! !BOP ===========================================================================
!
!> Search a character field list for a matching values 
!
! !DESCRIPTION: 
!     Sort a character array and the associated array(s) based on a
!     reasonably fast sort algorithm
!
! !INTERFACE:  -----------------------------------------------------------------

subroutine cplfind(num, fldlist, fld, ifind, nfind)

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p
   integer,parameter :: CL = ic_lvar

! !INPUT/OUTPUT PARAMETERS:

   integer(IN),      intent(in)  :: num         !< size of array
   character(len=CL),intent(in)  :: fldlist(:)  !< sorted field list
   character(len=CL),intent(in)  :: fld         !< field to search for
   integer(IN)      ,intent(out) :: ifind       !< first match index
   integer(IN)      ,intent(out) :: nfind       !< number that match

! !EOP

   !--- local ---
   integer(IN)    :: is,ie,im
   logical        :: found

   !--- formats ---
   character(*),parameter :: subName = '(cplfind) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   call oasis_debug_enter(subname)

    ifind = 0
    nfind = 0

    is = 1
    ie = num
    found = .false.

    ! check endpoints first, the binary search uses integer
    ! math which makes hitting the endpoints more difficult
    ! so check manually.  also if list size is 1, need to do this.

    if (.not.found) then
       im = 1
       if (fld == fldlist(im)) found = .true.
    endif
    if (.not.found) then
       im = num
       if (fld == fldlist(im)) found = .true.
    endif

    ! do a binary search

    do while (.not.found .and. ie > is)
       im = (is + ie) / 2
       im = max(im,is)
       im = min(im,ie)
!       write(nulprt,*) subname,'tcx',is,ie,im,trim(fld),' ',trim(fldlist(im))
       if (fld == fldlist(im)) then
          found = .true.
       elseif (fld > fldlist(im)) then
          is = max(im,is+1)
       else
          ie = min(im,ie-1)
       endif
    enddo

    ! if a match was found, find first and last instance of match in list

    if (found) then
       is = im
       ie = im
       if (is > 1) then
          do while (fld == fldlist(is-1) .and. is > 1)
             is = is - 1
          enddo
       endif
       if (ie < num) then
          do while (fld == fldlist(ie+1) .and. ie < num)
             ie = ie + 1
          enddo
       endif
       ifind = is
       nfind = (ie - is + 1)    
    endif

!   call oasis_debug_exit(subname)

end subroutine cplfind

!------------------------------------------------------------

!> Merge routine needed for mergesort

subroutine Merge(A,X,NA,B,Y,NB,C,Z,NC)
 
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
   character(*),parameter :: subName = '(Merge) '
 
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
 
end subroutine merge

!------------------------------------------------------------

!> Generic mergesort routine 
 
recursive subroutine MergeSort(N,A,T,S,Z)
 
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
   character(*),parameter :: subName = '(MergeSort) '

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
 
   call MergeSort(NA,A,T,S,Z)
   call MergeSort(NB,A(NA+1),T,S(NA+1),Z)
 
   if (A(NA) > A(NA+1)) then
      T(1:NA)=A(1:NA)
      Z(1:NA)=S(1:NA)
      call Merge(T,Z,NA,A(NA+1),S(NA+1),NB,A,S,N)
   endif
   return
 
end subroutine MergeSort

!===============================================================================

END MODULE mod_oasis_coupler


