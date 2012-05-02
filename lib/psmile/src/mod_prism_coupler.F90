MODULE mod_prism_coupler
!     - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
  USE mod_prism_kinds
  USE mod_prism_data
  USE mod_oasis_print
  USE mod_prism_parameters
  USE mod_prism_namcouple
  USE mod_prism_sys
  USE mod_prism_var
  USE mod_prism_part
  USE mod_prism_mpi
  USE mod_prism_string
  USE mod_prism_io
  USE mod_prism_timer
  USE mct_mod
  USE grids    ! scrip

  IMPLICIT NONE

  private

  public prism_coupler_setup
  public prism_router_type
  public prism_mapper_type
  public prism_coupler_type

!  INTEGER(kind=ip_i4_p),parameter :: max = 200

  !--- derived ---
  INTEGER(kind=ip_i4_p),pointer :: model_root(:)

! COUPLING INFO

  type prism_router_type
     !--- fixed at initialization ---
     type(mct_router)      :: router     ! router
  end type prism_router_type

  type prism_mapper_type
     !--- fixed at initialization ---
     type(mct_sMatP)       :: sMatP       ! mapper
     character(len=ic_long):: file
     character(len=ic_med) :: loc         ! location: src,dst
     character(len=ic_med) :: opt         ! optimization: bfb,sum,opt
     character(len=ic_med) :: optval      ! mct map option: src,dst
     logical               :: init
     integer(kind=ip_i4_p) :: spart ! src partition
     integer(kind=ip_i4_p) :: dpart ! dst partition
     type(mct_aVect)       :: AV_ms ! av for CONSERV src: mask, area, etc
     type(mct_aVect)       :: AV_md ! av for CONSERV dst: mask, area, etc
  end type prism_mapper_type

  type prism_coupler_type
     !--- fixed at initialization ---
     type(mct_aVect)       :: aVect1   ! primary aVect
     type(mct_aVect)       :: aVect2   ! extra aVect needed for mapping
     character(len=ic_xl)  :: rstfile  ! restart file
     character(len=ic_xl)  :: inpfile  ! restart file
     character(len=ic_xl)  :: fldlist  ! field list
     integer(kind=ip_i4_p) :: nflds    ! number of fields
     integer(kind=ip_i4_p),pointer :: varid(:)    ! varid for each field
     integer(kind=ip_i4_p) :: namID    ! namcouple ID
     integer(kind=ip_i4_p) :: partID   ! associated partition ID
     integer(kind=ip_i4_p) :: routerID ! router ID
     integer(kind=ip_i4_p) :: mapperID ! mapper ID
     integer(kind=ip_i4_p) :: ops      ! namcouple operation (ip_exported,...)
     integer(kind=ip_i4_p) :: comp     ! other model compid
     integer(kind=ip_i4_p) :: tag      ! comm tag
     integer(kind=ip_i4_p) :: seq      ! sequence number
     integer(kind=ip_i4_p) :: dt       ! coupling period (secs)
     integer(kind=ip_i4_p) :: lag      ! put lag += put sooner (secs)
     integer(kind=ip_i4_p) :: maxtime  ! max time for the coupler
     integer(kind=ip_i4_p) :: trans    ! transformation (ip_average,...)
     integer(kind=ip_i4_p) :: conserv  ! conserve operation (ip_cnone,ip_cglobal,...)
     integer(kind=ip_i4_p) :: getput   ! get/put flag
     logical               :: sndrcv   ! send recv flag
     logical               :: output   ! output flag
     logical               :: input    ! input flag
     logical               :: snddiag  ! diagnose src fields as part of coupling
     logical               :: rcvdiag  ! diagnose rcv fields as part of coupling
     real(kind=ip_double_p):: sndmult  ! field multiplier term
     real(kind=ip_double_p):: sndadd   ! field addition term
     real(kind=ip_double_p):: rcvmult  ! field multiplier term
     real(kind=ip_double_p):: rcvadd   ! field addition term
     !--- time varying info ---
     integer(kind=ip_i4_p) :: ltime    ! time at last coupling
     integer(kind=ip_i4_p),pointer :: avcnt(:)  ! counter for averaging
     integer(kind=ip_i4_p),pointer :: status(:) ! status of variables in coupler
  end type prism_coupler_type

  integer(kind=ip_i4_p)           :: prism_mrouter   ! max routers
  integer(kind=ip_i4_p)           :: prism_nrouter = 0
  type(prism_router_type) ,public, pointer:: prism_router(:)

  integer(kind=ip_i4_p)           :: prism_mmapper   ! max mappers
  integer(kind=ip_i4_p)           :: prism_nmapper = 0
  type(prism_mapper_type) ,public, pointer :: prism_mapper(:)

  integer(kind=ip_i4_p)           :: prism_mcoupler   ! max couplers
  integer(kind=ip_i4_p)   ,public :: prism_ncoupler = 0
  type(prism_coupler_type),public, pointer :: prism_coupler(:)

  integer(kind=ip_i4_p)   ,public :: lcouplerid    ! last coupler id
  integer(kind=ip_i4_p)   ,public :: lcouplertime  ! last coupler time 


#include <netcdf.inc>

!------------------------------------------------------------
CONTAINS
!------------------------------------------------------------

  SUBROUTINE prism_coupler_setup()

  !----------------------------------------------------------
  ! This routine reconciles the coupling stuff
  !----------------------------------------------------------

  IMPLICIT none
  integer(kind=ip_i4_p) :: ii ! for prints loop
  integer(kind=ip_i4_p) :: n,n1,nn,nv,nm,nv1,nns,lnn,nc
  integer(kind=ip_i4_p) :: pe
  integer(kind=ip_i4_p) :: part1, part2
  integer(kind=ip_i4_p) :: spart,dpart,rpart ! src, dst, router partition id
        ! part1 = my local part
        ! part2 = other part
        ! spart = src part for mapping; put=part1, get=part2
        ! dpart = dst part for mapping; put=part2, get=part1
        ! rpart = part used for router init
  integer(kind=ip_i4_p) :: mapID,namID
  type(mct_sMat)        :: sMati
  integer(kind=ip_i4_p) :: ncid,dimid,status
  integer(kind=ip_i4_p) :: lsize,gsize
  integer(kind=ip_i4_p) :: svarid
  integer(kind=ip_i4_p),allocatable :: varidtmp(:)
  integer(kind=ip_i4_p) :: part
  character(len=ic_lvar):: cstring
  character(len=ic_lvar):: myfld
  integer(kind=ip_i4_p) :: myfldi
  character(len=ic_lvar):: otfld
  integer(kind=ip_i4_p) :: otfldi
  integer(kind=ip_i4_p) :: nx,ny
  character(len=ic_lvar):: gridname
  character(len=ic_long):: tmp_mapfile
  integer(kind=ip_i4_p) :: flag
  logical               :: found, exists
  integer(kind=ip_i4_p) :: mynvar
  character(len=ic_lvar),pointer :: myvar(:)
  integer(kind=ip_i4_p) ,pointer :: myops(:)
  integer(kind=ip_i4_p) ,pointer :: nallvar(:)
  character(len=ic_lvar),pointer :: allvar(:,:)
  integer(kind=ip_i4_p) ,pointer :: allops(:,:)
  character(len=*),parameter :: subname = 'prism_coupler_setup'

  !----------------------------------------------------------

  !----------------------------------------------------------
  ! Figure out the global rank of each model's root
  !----------------------------------------------------------

  call prism_sys_debug_enter(subname)
  call prism_timer_start('cpl_setup')

  allocate(model_root(prism_nmodels))
  model_root = -99
  do n = 1,prism_nmodels
     pe = -1
     if (compid == n .and. mpi_rank_local == 0) pe = mpi_rank_global
     call prism_mpi_max(pe,model_root(n),mpi_comm_global, &
        subname//':cg',.true.)
  enddo
  DO ii=1,prism_nmodels
    CALL oasis_pprinti(subname,2,' model root for each model =',int1=ii,int2=model_root(ii))
  ENDDO

  ! allocate prism_router, prism_mapper, prism_coupler based on nnamcpl
  ! there cannot be more than that needed

  lnn=-1
  call prism_sys_debug_note(subname//' set defaults for datatypes')

  prism_mrouter = nnamcpl
  allocate(prism_router(prism_mrouter))
  prism_nrouter = 0

  prism_mmapper = nnamcpl
  allocate(prism_mapper(prism_mmapper))
  prism_nmapper = 0
  prism_mapper(:)%file  = ""
  prism_mapper(:)%loc   = ""
  prism_mapper(:)%opt   = ""
  prism_mapper(:)%optval= ""
  prism_mapper(:)%init  = .false.
  prism_mapper(:)%spart = ispval
  prism_mapper(:)%dpart = ispval

  prism_mcoupler = nnamcpl
  allocate(prism_coupler(prism_mcoupler))
  prism_ncoupler = 0
  prism_coupler(:)%rstfile = ""
  prism_coupler(:)%inpfile = ""
  prism_coupler(:)%fldlist = ""
  prism_coupler(:)%nflds   = 0
  do nc = 1,nnamcpl
     allocate(prism_coupler(nc)%varid(1))
     prism_coupler(nc)%varid(:) = ispval
  enddo
  CALL oasis_pprinti(subname,2,' initialize %varid :',int1=nnamcpl,int2=size(prism_coupler(nnamcpl)%varid))
  prism_coupler(:)%ops     = ispval
  prism_coupler(:)%comp    = ispval
  prism_coupler(:)%routerID  = ispval
  prism_coupler(:)%mapperID  = ispval
  prism_coupler(:)%tag     = ispval
  prism_coupler(:)%dt      = ispval
  prism_coupler(:)%lag     = 0
  prism_coupler(:)%maxtime = 0
  prism_coupler(:)%getput  = ispval
  prism_coupler(:)%sndrcv  = .false.
  prism_coupler(:)%output  = .false.
  prism_coupler(:)%input   = .false.
  prism_coupler(:)%trans   = ip_instant
  prism_coupler(:)%conserv = ip_cnone
  prism_coupler(:)%ltime   = ispval
  prism_coupler(:)%snddiag = .false.
  prism_coupler(:)%rcvdiag = .false.
  prism_coupler(:)%sndmult = 1.0_ip_double_p
  prism_coupler(:)%sndadd  = 0.0_ip_double_p
  prism_coupler(:)%rcvmult = 1.0_ip_double_p
  prism_coupler(:)%rcvadd  = 0.0_ip_double_p

  lcouplerid   = ispval
  lcouplertime = ispval

  !----------------------------------------------------------
  ! Share model variable information across all models
  !----------------------------------------------------------

  if (PRISM_Debug >= 5) then
     CALL oasis_pprintc(subname,5,':',char1='  broadcast var info ')
     call prism_mpi_barrier(mpi_comm_global)
  endif

  call prism_sys_debug_note(subname//' share var info between models')

  allocate(allvar(100,prism_nmodels))
  allocate(nallvar(prism_nmodels))
  allocate(allops(100,prism_nmodels))
  allocate(myvar(100))
  allocate(myops(100))

  allvar = " "
  nallvar = 0
  allops = -1
  do n = 1,prism_nmodels
     if (n == compid) then
        myvar = " "
        myops = 0
        mynvar = prism_nvar
        do n1 = 1, prism_nvar
           myvar(n1) = trim(prism_var(n1)%name)
           myops(n1) = prism_var(n1)%ops
        enddo
     endif

     CALL oasis_pprinti(subname,5,' BCAST from ',int1=n,int2=model_root(n))

     call prism_mpi_bcast(mynvar,mpi_comm_global,'mynvar',model_root(n))
     CALL oasis_pprinti(subname,5,' bcast mynvar ',int1=mynvar)

     nallvar(n) = mynvar
     call prism_mpi_bcast(myvar,mpi_comm_global,'myvar',model_root(n))
     CALL oasis_pprintc(subname,5,' bcast myvar ',char1=trim(myvar(1)))

     allvar(:,n) = myvar(:)
     call prism_mpi_bcast(myops,mpi_comm_global,'myops',model_root(n))
     CALL oasis_pprinti(subname,5,' bcast myops ',int1=myops(1))

     allops(:,n) = myops(:)
  enddo

  deallocate(model_root)
  deallocate(myvar,myops)

  CALL oasis_pprintc(subname,2,':',char1=' model variable info:')
  CALL oasis_pprintc(subname,2,':',char1='======================================================')
  DO nm = 1,prism_nmodels
    CALL oasis_pprinti(subname,2,' model,nvars = ',int1=nm,int2=nallvar(nm))
    CALL oasis_pprintc(subname,2,':',char1='======================================================')
    DO nv = 1,nallvar(nm)
      cstring = 'unknown'
      IF (allops(nv,nm) == PRISM_Out) cstring = 'prism_out'
      IF (allops(nv,nm) == PRISM_In)  cstring = 'prism_in'
      CALL oasis_pprinti(subname,2,' Model, idx, ops  : ',int1=nm,int2=nv,int3=allops(nv,nm))
      CALL oasis_pprintc(subname,2,' Type of variable : ',char1=TRIM(cstring),&
         char2=TRIM(allvar(nv,nm)))
    ENDDO
    CALL oasis_pprintc(subname,2,':',char1='======================================================')
  ENDDO
  CALL oasis_pprintc(subname,2,':',char1=' end variable info')
  IF (PRISM_Debug >= 2) THEN
      CALL prism_mpi_barrier(mpi_comm_global)
  ENDIF

  !----------------------------------------------------------
  ! Setup couplers based on namcouple and model variable info
  ! These must be paired up consistently, create couplers in
  ! sorted order (nns)
  ! nn = namcpl counter, sorted
  ! nm = model counter, compid is my nm
  ! nv = variable counter
  ! nv1 = my variable counter
  !----------------------------------------------------------

  prism_ncoupler = 0

  !--------------------------------
  ! for all namcoupler input
  !--------------------------------

  call prism_sys_debug_note(subname//' compare vars and namcouple')
  call prism_sys_debug_note(subname//' setup couplers')

  do nns = 1,nnamcpl
     nn = namfldsort(nns)

     !--- tcx require for run time error on corail ????----
     call prism_mpi_barrier(mpi_comm_global)
     CALL oasis_pprinti(subname,2,' check nam :',int1=nns)

     !--------------------------------
     ! for all my variables
     !--------------------------------

     do nv1 = 1,nallvar(compid)

        !--- tcx require for run time error on corail ????----
        CALL oasis_pprinti(subname,2,' check var :',int1=nns,int2=nv1)

        !--------------------------------
        ! get my parition and fld
        !--------------------------------

        part1  = prism_var(nv1)%part
        myfld  = trim(allvar(nv1,compid))

        !--------------------------------
        ! check if i'm an In or Out variable
        ! check if my variable matches this namcouple dst or src
        !--------------------------------

        flag = PRISM_NotDef
        if (allops(nv1,compid) == PRISM_Out) then
           myfldi = prism_string_listGetIndexF(namsrcfld(nn),myfld)
           if (myfldi > 0) flag = PRISM_Out
        elseif (allops(nv1,compid) == PRISM_In) then
           myfldi = prism_string_listGetIndexF(namdstfld(nn),myfld)
           if (myfldi > 0) flag = PRISM_In
        endif

        !--------------------------------
        ! my variable is in this namcouple input
        !--------------------------------

        if (flag /= PRISM_NotDef) then

           !--------------------------------
           ! migrate namcouple info into part
           !--------------------------------

           if (flag == PRISM_In) then
              if (prism_part(part1)%nx < 1) then
                 prism_part(part1)%nx = namdst_nx(nn)
                 prism_part(part1)%ny = namdst_ny(nn)
                 prism_part(part1)%gridname = trim(namdstgrd(nn))
              endif
           endif
           if (flag == PRISM_Out) then
              if (prism_part(part1)%nx < 1) then
                 prism_part(part1)%nx = namsrc_nx(nn)
                 prism_part(part1)%ny = namsrc_ny(nn)
                 prism_part(part1)%gridname = trim(namsrcgrd(nn))
              endif
           endif

           !--------------------------------
           ! make sure it's either an In or Out, sanity check
           !--------------------------------

           if (flag /= PRISM_In .and. flag /= PRISM_Out) then
              CALL oasis_pprinti(subname,2,' flag problems :',int1=flag)
              CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
              CALL oasis_pprintc(subname,2,' error :',char1='flag problems')
              call prism_sys_abort()
           endif

           CALL oasis_pprinti(subname,5,' ca: nn, compid =',int1=nn,int2=compid)
           CALL oasis_pprinti(subname,5,' ca: nv, myfldi =',int1=nv,int2=myfldi)
           CALL oasis_pprintc(subname,5,' ca: myfld =',char1=trim(myfld))

           !--------------------------------
           ! look for namcouple coupling variable in all other model variables
           !--------------------------------

           found = .false.
           do nm = 1,prism_nmodels
           do nv = 1,nallvar(nm)

              !--- tcx require for run time error on corail ????----
              CALL oasis_pprinti(subname,2,' check mod nns, nv1 :',int1=nns,int2=nv1)
              CALL oasis_pprinti(subname,2,' check mod nm, nv :',int1=nm,int2=nv)

              otfld  = trim(allvar(nv,nm))
              otfldi = -1

              !--------------------------------
              ! check if other variable is in this namcouple
              !--------------------------------

              if (flag == PRISM_Out) &
                 otfldi = prism_string_listGetIndexF(namdstfld(nn),otfld)
              if (flag == PRISM_In) &
                 otfldi = prism_string_listGetIndexF(namsrcfld(nn),otfld)

              !--------------------------------
              ! matches if they are in the same position in namcouple fld list
              !--------------------------------

              if (otfldi == myfldi) then

                  CALL oasis_pprinti(subname,2,' check fld nns, nv1, nm: ',int1=nns,int2=nv1,int3=nm)
                  CALL oasis_pprinti(subname,2,' check fld nv,otfldi : ',int1=nv,int2=otfldi)

                  CALL oasis_pprinti(subname,5,' ca: otfld nn,nm : ',int1=nn,int2=nm)
                  CALL oasis_pprinti(subname,5,' ca: otfld nv,otfldi : ',int1=nv,int2=otfldi)
                  CALL oasis_pprintc(subname,5,' ca: otfld : ',char1=TRIM(otfld))

                 !--------------------------------
                 ! Do not allow src and dst to be on same model for communication
                 ! Make sure one side is In and other side is Out for communication
                 ! If input or output, field name should match
                 !--------------------------------

                 if (namfldops(nn) == ip_exported .or. namfldops(nn) == ip_expout) then
                    if (nm == compid) then
                       CALL oasis_pprinti(subname,2,' ERROR send recv pair on same model',int1=nm)
                       CALL oasis_pprintc(subname,2,' ERROR send recv pair on same model',char1=TRIM(myfld),&
                                          char2=' and ',char3=trim(otfld))
                       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
                       CALL oasis_pprintc(subname,2,' error :',char1=' send recv pair on same model')
                       CALL prism_sys_abort()
                    endif
                    if (flag == PRISM_Out .and. allops(nv,nm) /= PRISM_In) then
                       CALL oasis_pprintc(subname,2,':',char1=' ERROR send recv pair both Out')
                       CALL oasis_pprintc(subname,2,' ERROR send recv pair both Out',char1=TRIM(myfld), &
                                            char2=' and ',char3=trim(otfld))
                       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
                       CALL oasis_pprintc(subname,2,' error :',char1=' ERROR send recv pair both Out')
                       call prism_sys_abort()
                    endif
                    if (flag == PRISM_In .and. allops(nv,nm) /= PRISM_Out) then
                       CALL oasis_pprintc(subname,2,':',char1=' ERROR send recv pair both In')
                       CALL oasis_pprintc(subname,2,' ERROR send recv pair both In',char1=TRIM(myfld), &
                                           char2=' and ',char3=trim(otfld))
                       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
                       CALL oasis_pprintc(subname,2,' error :',char1=' ERROR send recv pair both In')
                       call prism_sys_abort()
                    endif
                 endif

                 if (namfldops(nn) == ip_input .or. namfldops(nn) == ip_output) then
                    if (trim(myfld) /= trim(otfld)) then
                       CALL oasis_pprintc(subname,2,':',char1=' ERROR namcouple field names to not match for in/out')
                       CALL oasis_pprintc(subname,2,' ERROR namcouple field names to not match for in/out',&
                                          char1=TRIM(myfld),char2=' and ',char3=trim(otfld))
                       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
                       CALL oasis_pprintc(subname,2,' error :',char1=' namcouple field names do not match for in/out')
                       CALL prism_sys_abort()
                    endif
                 endif

                 !--------------------------------
                 ! Only an error to find two sources for a destination
                 ! Not an error if a two destinations have a single source
                 !--------------------------------

                 if (flag == PRISM_In .and. found) then
                    CALL oasis_pprintc(subname,2,' ERROR found two sources for ',char1=trim(otfld))
                    CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
                    CALL oasis_pprintc(subname,2,' error :',char1='found two wources')
                    call prism_sys_abort()
                 endif
                 found = .true.

                 !--------------------------------
                 ! only increment prism_ncoupler if new nn (coupler), otherwise use prior prism_ncoupler
                 ! this supports multiple fields coupled in one communication step
                 !--------------------------------

                 if (nn /= lnn) prism_ncoupler = prism_ncoupler + 1
                 lnn = nn
                 if (prism_ncoupler > prism_mcoupler) then
                    CALL oasis_pprinti(subname,2,' ERROR prism_ncoupler to large : ',int1=prism_ncoupler,&
                                          int2=prism_mcoupler)
                    CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
                    CALL oasis_pprintc(subname,2,' error :',char1=' prism_ncoupler too large')
                    call prism_sys_abort()
                 endif
                 nc = prism_ncoupler

                 !--------------------------------
                 ! prism_coupler fields, multiple field support
                 !--------------------------------

                 prism_coupler(nc)%nflds = prism_coupler(nc)%nflds + 1
                 if (prism_coupler(nc)%nflds == 1) then
                    prism_coupler(nc)%fldlist = trim(myfld)
                 else
                    prism_coupler(nc)%fldlist = trim(prism_coupler(nc)%fldlist)//':'//trim(myfld)
                 endif

                 svarid = size(prism_coupler(nc)%varid)
                 if (prism_coupler(nc)%nflds > svarid) then
                     allocate(varidtmp(svarid))
                     varidtmp(1:svarid) = prism_coupler(nc)%varid(1:svarid)
                     deallocate(prism_coupler(nc)%varid)
                     allocate(prism_coupler(nc)%varid(prism_coupler(nc)%nflds+10))
                     prism_coupler(nc)%varid(1:svarid) = varidtmp(1:svarid)
                     deallocate(varidtmp)
                 endif
                 prism_coupler(nc)%varid(prism_coupler(nc)%nflds) = nv1

                 !--------------------------------
                 ! prism_coupler other settings
                 !--------------------------------

                 prism_coupler(nc)%comp   = nm
                 prism_coupler(nc)%seq    = namfldseq(nn)
                 prism_coupler(nc)%dt     = namflddti(nn)
                 prism_coupler(nc)%lag    = namfldlag(nn)
                 prism_coupler(nc)%maxtime= namruntim
                 prism_coupler(nc)%rstfile= trim(namrstfil(nn))
                 prism_coupler(nc)%inpfile= trim(naminpfil(nn))
                 prism_coupler(nc)%mapperID = -1
                 prism_coupler(nc)%partID = part1
                 prism_coupler(nc)%namID  = nn
                 prism_coupler(nc)%trans  = namfldtrn(nn)
                 prism_coupler(nc)%conserv= namfldcon(nn)
                 prism_coupler(nc)%ops    = namfldops(nn)
                 prism_coupler(nc)%tag    = compid*100*1000 + compid*1000 + nn
                 prism_coupler(nc)%getput = PRISM_NotDef
                 prism_coupler(nc)%sndrcv = .false.
                 prism_coupler(nc)%output = .false.
                 prism_coupler(nc)%input  = .false.
                 prism_coupler(nc)%sndmult= namfldsmu(nn)
                 prism_coupler(nc)%sndadd = namfldsad(nn)
                 prism_coupler(nc)%rcvmult= namflddmu(nn)
                 prism_coupler(nc)%rcvadd = namflddad(nn)
                 prism_coupler(nc)%snddiag= namchecki(nn)
                 prism_coupler(nc)%rcvdiag= namchecko(nn)

                 !--------------------------------
                 ! prism_coupler input and output flags
                 ! prism_coupler comm flags, need for tags to match up on both sides
                 ! tags assume up to 1000 namcouple inputs and 100 models
                 !--------------------------------


                 if (namfldops(nn) == ip_output .or. namfldops(nn) == ip_expout) then
                    prism_coupler(nc)%output = .true.
                    prism_coupler(nc)%getput = PRISM_PUT
                 endif
                 if (namfldops(nn) == ip_input) then
                    prism_coupler(nc)%input  = .true.
                    prism_coupler(nc)%getput = PRISM_GET
                 endif

                 if (namfldops(nn) == ip_exported .or. namfldops(nn) == ip_expout) then
                    prism_coupler(nc)%sndrcv = .true.
                    if (flag == PRISM_Out) then
                       prism_coupler(nc)%tag = nm*100*1000 + compid*1000 + nn
                       prism_coupler(nc)%getput = PRISM_PUT
                    elseif (flag == PRISM_In) then
                       prism_coupler(nc)%tag = compid*100*1000 + nm*1000 + nn
                       prism_coupler(nc)%getput = PRISM_GET
                    endif
                    !--------------------------------
                    ! prism_coupler router
                    ! cannot reuse router because don't really know what's on the other side
                    !--------------------------------
                    prism_nrouter = prism_nrouter + 1
                    if (prism_nrouter > prism_mrouter) then
                       CALL oasis_pprinti(subname,2,' ERROR prism_nrouter to large :  ',int1=prism_nrouter,&
                                             int2=prism_mrouter)
                       CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
                       CALL oasis_pprintc(subname,2,' error :',char1=' prism_nrouter too large')
                       call prism_sys_abort()
                    endif
                    prism_coupler(nc)%routerID = prism_nrouter
                 endif

                 !--------------------------------
                 ! prism_coupler mapper
                 !--------------------------------

                 tmp_mapfile = nammapfil(nn)

                 if (trim(tmp_mapfile) == 'idmap' .and. trim(namscrmet(nn)) /= trim(cspval)) then
                    if (trim(namscrmet(nn)) == 'CONSERV') then
                       tmp_mapfile = 'rmp_'//trim(namsrcgrd(nn))//'_to_'//trim(namdstgrd(nn))//'_'//trim(namscrmet(nn))//'_'//trim(namscrnor(nn))//'.nc'
                    else
                       tmp_mapfile = 'rmp_'//trim(namsrcgrd(nn))//'_to_'//trim(namdstgrd(nn))//'_'//trim(namscrmet(nn))//'.nc'
                    endif
                 endif

                 if (trim(tmp_mapfile) /= 'idmap') then
                 if ((flag == PRISM_In  .and. trim(nammaploc(nn)) == 'dst') .or. &
                     (flag == PRISM_Out .and. trim(nammaploc(nn)) == 'src')) then
                    !--------------------------------
                    ! try to reuse mapper already defined
                    ! must match mapping file and partition
                    !--------------------------------
                    mapid = -1
                    do n = 1,prism_nmapper
                       if (trim(prism_mapper(n)%file) == trim(tmp_mapfile) .and. &
                           trim(prism_mapper(n)%opt ) == trim(nammapopt(nn))) then
                          if (flag == PRISM_In  .and. prism_mapper(n)%dpart == part1) mapid = n
                          if (flag == PRISM_Out .and. prism_mapper(n)%spart == part1) mapid = n
                       endif
                    enddo
                    !--------------------------------
                    ! or will need a new mapper
                    !--------------------------------
                    if (mapid < 1) then
                       prism_nmapper = prism_nmapper + 1
                       if (prism_nmapper > prism_mmapper) then
                          CALL oasis_pprinti(subname,2,' ERROR prism_nmapper too large : ',int1=prism_nmapper, &
                                                int2=prism_mmapper)
                          CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
                          CALL oasis_pprintc(subname,2,' error :',char1=' prism_nmapper too large')
                          call prism_sys_abort()
                       endif
                       mapid = prism_nmapper
                       prism_mapper(mapid)%file = trim(tmp_mapfile)
                       prism_mapper(mapid)%loc  = trim(nammaploc(nn))
                       prism_mapper(mapid)%opt  = trim(nammapopt(nn))
                       if (flag == PRISM_In ) prism_mapper(mapID)%dpart = part1
                       if (flag == PRISM_Out) prism_mapper(mapID)%spart = part1
                       CALL oasis_pprintc(subname,15,' DEBUG new mapper for file :',char1=trim(prism_mapper(mapid)%file))
                    endif
                    prism_coupler(nc)%mapperID = mapid
                 endif  ! flag and nammaploc match
                 endif  ! nammapfil

                 !--------------------------------
                 ! add this coupler to list of prism_var couplers
                 !--------------------------------

                 prism_var(nv1)%ncpl = prism_var(nv1)%ncpl + 1
                 prism_var(nv1)%cpl(prism_var(nv1)%ncpl) = nc

              endif  ! other var in this namcouple

           enddo  ! nv
           enddo  ! nm

        endif  ! my var in this namcouple

     enddo  ! nv1
  enddo  ! nns


  deallocate(allvar,nallvar,allops)

  if (PRISM_Debug >= 2) then 
     call prism_mpi_barrier(mpi_comm_global,subname//' barrier1')
  endif

  !----------------------------------------------------------
  ! Initialize coupling infrastructure based on couplers above
  !----------------------------------------------------------

  call prism_sys_debug_note(subname//' initialize coupling datatypes')

  do nc = 1,prism_ncoupler
     CALL oasis_pprinti(subname,5,' DEBUG ci:initialize coupler ',int1=nc)

     namID = prism_coupler(nc)%namID
     part1 = prism_coupler(nc)%partID
     mapID = prism_coupler(nc)%mapperID

     if (part1 <= 0) then
        CALL oasis_pprinti(subname,2,' ERROR part1 invalid : ',int1=part1)
        CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
        CALL oasis_pprintc(subname,2,' error :',char1=' part1 invalid')
        call prism_sys_abort()
     endif

     !--------------------------------
     ! initialize avect1
     !--------------------------------

     gsize = mct_gsmap_gsize(prism_part(part1)%gsmap)
     lsize = mct_gsmap_lsize(prism_part(part1)%gsmap,mpi_comm_local)

     CALL oasis_pprinti(subname,15,' DEBUG ci:part1 info part1,mapid ',int1=part1,int2=mapid)
     CALL oasis_pprinti(subname,15,' DEBUG ci:part1 info gsize,lsize ',int1=gsize,int2=lsize)
     CALL oasis_pprinti(subname,15,' DEBUG ci:part1a ',int1=prism_part(part1)%gsmap%ngseg,int2=prism_part(part1)%gsmap%gsize)
     DO n1 = 1,prism_part(part1)%gsmap%ngseg
       CALL oasis_pprinti(subname,15,' DEBUG ci:part1b :',int1=n1)
       CALL oasis_pprinti(subname,15,' DEBUG ci:part1b :',int1=prism_part(part1)%gsmap%start(n1),&
                             int2=prism_part(part1)%gsmap%length(n1),&
                             int3=prism_part(part1)%gsmap%pe_loc(n1))
     ENDDO

     call mct_avect_init(prism_coupler(nc)%avect1,rList=trim(prism_coupler(nc)%fldlist),lsize=lsize)
     call mct_avect_zero(prism_coupler(nc)%avect1)
     CALL oasis_pprintc(subname,15,':',char1=' DEBUG ci:avect1 initialized ')

     !--------------------------------
     ! compute nflds for this coupling and initialize avcnt and status
     !--------------------------------

     prism_coupler(nc)%nflds = mct_aVect_nRAttr(prism_coupler(nc)%avect1)
     allocate(prism_coupler(nc)%status(prism_coupler(nc)%nflds))
     allocate(prism_coupler(nc)%avcnt (prism_coupler(nc)%nflds))
     prism_coupler(nc)%avcnt(:) = 0
     if (prism_coupler(nc)%getput == PRISM_PUT) prism_coupler(nc)%status = PRISM_COMM_WAIT
     if (prism_coupler(nc)%getput == PRISM_GET) prism_coupler(nc)%status = PRISM_COMM_READY

     !--------------------------------
     ! initialize mapper
     !--------------------------------

     if (mapID > 0) then

        if (prism_mapper(mapID)%init) then
           !--------------------------------
           ! mapper already initialized
           !--------------------------------
           if (prism_coupler(nc)%getput == PRISM_PUT) then
              part2 = prism_mapper(mapID)%dpart
           else
              part2 = prism_mapper(mapID)%spart
           endif
           gsize = mct_gsmap_gsize(prism_part(part2)%gsmap)
        else
           !--------------------------------
           ! instantiate mapper
           ! read/generate mapping file
           ! read non local grid size
           ! get gsmap for non local grid
           ! read mapping weights and initialize sMatP
           !--------------------------------
           CALL oasis_pprintc(subname,15,' DEBUG ci:read mapfile : ',char1=trim(prism_mapper(mapid)%file))
           if (mpi_rank_local == 0) then
              inquire(file=trim(prism_mapper(mapID)%file),exist=exists)
              CALL oasis_pprintc(subname,15,' DEBUG ci: inquire mapfile : ',char1=TRIM(prism_mapper(mapID)%file))
              CALL oasis_pprintl(subname,15,' DEBUG ci: inquire mapfile : ',log1=exists)

              if (.not.exists) then
                 if (trim(namscrmet(namID)) /= trim(cspval)) then
                    !--------------------------------
                    ! generate mapping file on root pe
                    ! taken from oasis3 scriprmp
                    !--------------------------------
                    call prism_coupler_genmap(mapid,namID)
                 else
                    CALL oasis_pprintc(subname,2,' ERROR map file does not exist and SCRIPR not set ', &
                                          char1=trim(prism_mapper(mapID)%file))
                    CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
                    CALL oasis_pprintc(subname,2,' error :',char1=' map file does not exist')
                    call prism_sys_abort()
                 endif
              endif

              !--------------------------------
              ! open mapping file
              !--------------------------------
              status = nf_open(trim(prism_mapper(mapid)%file),nf_nowrite,ncid)
              if (prism_coupler(nc)%getput == PRISM_PUT) &
                 status = nf_inq_dimid(ncid,'dst_grid_size',dimid)
              if (prism_coupler(nc)%getput == PRISM_GET) &
                 status = nf_inq_dimid(ncid,'src_grid_size',dimid)
              status = nf_inq_dimlen(ncid,dimid,gsize)
           endif
           call prism_mpi_bcast(gsize,mpi_comm_local,subname//' gsize')

           if (prism_coupler(nc)%getput == PRISM_PUT) then
              nx = namdst_nx(namID)
              ny = namdst_ny(namID)
              gridname = trim(namdstgrd(namID))
           else
              nx = namsrc_nx(namID)
              ny = namsrc_ny(namID)
              gridname = trim(namsrcgrd(namID))
           endif

           !tcx improve match here with nx,ny,gridname 
           call prism_part_create(part2,'1d',gsize)

           if (prism_part(part2)%nx < 1) then
              prism_part(part2)%nx = nx
              prism_part(part2)%ny = ny
              prism_part(part2)%gridname = trim(gridname)
           endif

           if (prism_coupler(nc)%getput == PRISM_PUT) then
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
              CALL oasis_pprintc(subname,2,' ERROR mapper opt invalid ', &
                                    char1=trim(prism_mapper(mapID)%opt))
              CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
              CALL oasis_pprintc(subname,2,' error :',char1=' mapper opt invalid')
              CALL prism_sys_abort()
           endif
           if (prism_mapper(mapID)%optval /= '' .and. &
               prism_mapper(mapID)%optval /= trim(cstring)) then
              CALL oasis_pprintc(subname,2,' ERROR mapper opt changed : ',char1=trim(prism_mapper(mapID)%optval))
              CALL oasis_pprintc(subname,2,' ERROR mapper opt changed : ',char1=trim(cstring))
              CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
              CALL oasis_pprintc(subname,2,' error :',char1=' mapper opt changed')
              CALL prism_sys_abort()
           endif
           prism_mapper(mapID)%optval = trim(cstring)

           call prism_timer_start('cpl_smatrd')
           call prism_coupler_smatreaddnc(sMati,prism_part(spart)%gsmap,prism_part(dpart)%gsmap, &
              trim(cstring),trim(prism_mapper(mapid)%file),mpi_rank_local,mpi_comm_local)
           call mct_sMatP_Init(prism_mapper(mapID)%sMatP, sMati, &
              prism_part(spart)%gsmap, prism_part(dpart)%gsmap, 0, mpi_comm_local, compid)
           call prism_timer_stop('cpl_smatrd')

           if (prism_coupler(nc)%conserv /= ip_cnone) then
              ! initialize and load AV_ms and AV_md
              lsize = mct_gsmap_lsize(prism_part(spart)%gsmap,mpi_comm_local)
              call mct_avect_init(prism_mapper(mapid)%av_ms,iList='mask',rList='area',lsize=lsize)
              call mct_avect_zero(prism_mapper(mapid)%av_ms)
              gridname = prism_part(spart)%gridname
              call prism_io_read_avfld('masks.nc',prism_mapper(mapid)%av_ms, &
                 prism_part(spart)%gsmap,'mask',trim(gridname)//'.msk',fldtype='int')
              call prism_io_read_avfld('areas.nc',prism_mapper(mapid)%av_ms, &
                 prism_part(spart)%gsmap,'area',trim(gridname)//'.srf',fldtype='real')

              lsize = mct_gsmap_lsize(prism_part(dpart)%gsmap,mpi_comm_local)
              call mct_avect_init(prism_mapper(mapid)%av_md,iList='mask',rList='area',lsize=lsize)
              call mct_avect_zero(prism_mapper(mapid)%av_md)
              gridname = prism_part(dpart)%gridname
              call prism_io_read_avfld('masks.nc',prism_mapper(mapid)%av_md, &
                 prism_part(dpart)%gsmap,'mask',trim(gridname)//'.msk',fldtype='int')
              call prism_io_read_avfld('areas.nc',prism_mapper(mapid)%av_md, &
                 prism_part(dpart)%gsmap,'area',trim(gridname)//'.srf',fldtype='real')

              CALL oasis_pprinti(subname,30,' DEBUG msi : ',int1=MINVAL(prism_mapper(mapid)%av_ms%iAttr(:,:)),&
                                    int2=MAXVAL(prism_mapper(mapid)%av_ms%iAttr(:,:)),&
                                    int3=sum(prism_mapper(mapid)%av_ms%iAttr(:,:)))
              CALL oasis_pprintr(subname,30,' DEBUG msr : ',r1=MINVAL(prism_mapper(mapid)%av_ms%rAttr(:,:)),&
                                    r2=MAXVAL(prism_mapper(mapid)%av_ms%rAttr(:,:)),&
                                    r3=sum(prism_mapper(mapid)%av_ms%rAttr(:,:)))
              CALL oasis_pprinti(subname,30,' DEBUG mdi : ',int1=MINVAL(prism_mapper(mapid)%av_md%iAttr(:,:)), &
                                    int2=MAXVAL(prism_mapper(mapid)%av_md%iAttr(:,:)),&
                                    int3=sum(prism_mapper(mapid)%av_md%iAttr(:,:)))
              CALL oasis_pprintr(subname,30,' DEBUG mdr : ',r1=MINVAL(prism_mapper(mapid)%av_md%rAttr(:,:)),&
                                    r2=MAXVAL(prism_mapper(mapid)%av_md%rAttr(:,:)),&
                                    r3=sum(prism_mapper(mapid)%av_md%rAttr(:,:)))
           endif

           lsize = mct_smat_gNumEl(prism_mapper(mapID)%sMatP%Matrix,mpi_comm_local)
           prism_mapper(mapID)%init = .true.
           CALL oasis_pprinti(subname,15,' DEBUG ci:done initializing prism_mapper :',int1=mapId,int2=lsize)
           call mct_sMat_Clean(sMati)
        endif  ! map init

        !--------------------------------
        ! initialize avect2
        !--------------------------------

        lsize = mct_gsmap_lsize(prism_part(part2)%gsmap,mpi_comm_local)

        CALL oasis_pprinti(subname,15,' DEBUG ci:part2 info part2 mapid : ',int1=part2,int2=mapid)
        CALL oasis_pprinti(subname,15,' DEBUG ci:part2 info gsize,lsize : ',int1=gsize,int2=lsize)
        CALL oasis_pprinti(subname,15,' DEBUG ci:part2a : ',int1=prism_part(part2)%gsmap%ngseg,&
                             int2=prism_part(part2)%gsmap%gsize)
        DO n1=1,prism_part(part2)%gsmap%ngseg
          CALL oasis_pprinti(subname,15,' DEBUG ci:part2b : ',int1=n1)
          CALL oasis_pprinti(subname,15,' DEBUG ci:part2b : ',int1=prism_part(part2)%gsmap%start(n1),&
                              int2=prism_part(part2)%gsmap%length(n1),&
                                int3=prism_part(part2)%gsmap%pe_loc(n1))
        ENDDO
        call mct_avect_init(prism_coupler(nc)%avect2,rList=trim(prism_coupler(nc)%fldlist),lsize=lsize)
        call mct_avect_zero(prism_coupler(nc)%avect2)
        CALL oasis_pprintc(subname,15,':',char1=' DEBUG ci:avect2 initialized ')

        !--------------------------------
        ! router partition is always other part
        !--------------------------------

        rpart = part2

     else

        !--------------------------------
        ! router partition is just coupler part
        !--------------------------------

        rpart = prism_coupler(nc)%partID

     endif  ! no mapper

     !--------------------------------
     ! initialize router based on rpart
     !--------------------------------

     if (prism_coupler(nc)%sndrcv) then

        CALL oasis_pprinti(subname,15,' DEBUG ci:initialize router : ',int1=prism_coupler(nc)%routerID, &
                              int2=prism_coupler(nc)%comp,int3=rpart)

        call mct_router_init(prism_coupler(nc)%comp,prism_part(rpart)%gsmap, &
           mpi_comm_local,prism_router(prism_coupler(nc)%routerID)%router)

        CALL oasis_pprinti(subname,15,' DEBUG ci:done initializing prism_router : ',int1=prism_coupler(nc)%routerID)
     endif

  enddo

  !----------------------------------------------------------
  ! Diagnostics
  !----------------------------------------------------------

     CALL oasis_pprintc(subname,2,':',char1=' couplers initialized')
     CALL oasis_pprintc(subname,2,' Print infos : ',char1='=========================================')
     do nc = 1,prism_ncoupler
        call prism_coupler_print(nc)
     enddo
     CALL oasis_pprintc(subname,2,' End Print infos : ',char1='=========================================')

  call prism_timer_stop('cpl_setup')

  if (PRISM_Debug >= 2) then
     call prism_mpi_barrier(mpi_comm_global,subname//' barrier2')
  endif

  call prism_sys_debug_exit(subname)

  END SUBROUTINE prism_coupler_setup

!------------------------------------------------------------
  SUBROUTINE prism_coupler_print(cplid)

  IMPLICIT NONE

  integer(ip_i4_p), intent(in) :: cplid
  !----------------------------------------------------------
  INTEGER(ip_i4_p) :: ii ! for prints loop
  integer(ip_i4_p) :: mapid, rouid, parid, namid, nflds
  integer(ip_i4_p) :: spart,dpart
  character(len=*),parameter :: subname = 'prism_coupler_print'

  call prism_sys_debug_enter(subname)

  mapid = prism_coupler(cplid)%mapperid
  rouid = prism_coupler(cplid)%routerid
  parid = prism_coupler(cplid)%partid
  namid = prism_coupler(cplid)%namid
  nflds = prism_coupler(cplid)%nflds


     CALL oasis_pprinti(subname,2,' model and cplid : ',int1=compid,int2=cplid)
  if (prism_coupler(cplid)%getput == PRISM_PUT) then
     CALL oasis_pprintc(subname,2,'  send fields  : ',char1=trim(prism_coupler(cplid)%fldlist))
     CALL oasis_pprinti(subname,2,'  from model   : ',int1=compid)
     CALL oasis_pprinti(subname,2,'  to model     : ',int1=prism_coupler(cplid)%comp)
     CALL oasis_pprinti(subname,2,'  using router : ',int1=rouid)
     CALL oasis_pprinti(subname,2,'  transform    : ',int1=prism_coupler(cplid)%trans)
     CALL oasis_pprintl(subname,2,'  snd diagnose : ',log1=prism_coupler(cplid)%snddiag)
     CALL oasis_pprintr(subname,2,'  snd fld mult : ',r1=prism_coupler(cplid)%sndmult)
     CALL oasis_pprintr(subname,2,'  snd fld add  : ',r1=prism_coupler(cplid)%sndadd)
  endif
  if (prism_coupler(cplid)%getput == PRISM_GET) then
     CALL oasis_pprintc(subname,2,'  recv fields  : ',char1=trim(prism_coupler(cplid)%fldlist))
     CALL oasis_pprinti(subname,2,'  from model   : ',int1=prism_coupler(cplid)%comp)
     CALL oasis_pprinti(subname,2,'  to model     : ',int1=compid)
     CALL oasis_pprinti(subname,2,'  using router : ',int1=rouid)
     CALL oasis_pprintl(subname,2,'  rcv diagnose : ',log1=prism_coupler(cplid)%rcvdiag)
     CALL oasis_pprintr(subname,2,'  rcv fld mult : ',r1=prism_coupler(cplid)%rcvmult)
     CALL oasis_pprintr(subname,2,'  rcv fld add  : ',r1=prism_coupler(cplid)%rcvadd)
  endif
  CALL oasis_pprinti(subname,2,'   namcouple op ',int1=prism_coupler(cplid)%ops)
  CALL oasis_pprinti(subname,2,'   namcouple id ',int1=namid)
  DO ii=1,SIZE(prism_coupler(cplid)%varid)
    CALL oasis_pprinti(subname,2,'   variable ids ',int1=prism_coupler(cplid)%varid(ii))
  ENDDO
  CALL oasis_pprintl(subname,2,'   sndrcv flag  ',log1=prism_coupler(cplid)%sndrcv)   
  CALL oasis_pprintl(subname,2,'   output flag  ',log1=prism_coupler(cplid)%output)
  CALL oasis_pprintl(subname,2,'   input flag   ',log1=prism_coupler(cplid)%input)
  CALL oasis_pprintc(subname,2,'   input file   ',char1=trim(prism_coupler(cplid)%inpfile))
  CALL oasis_pprintc(subname,2,'   restart file ',char1=TRIM(prism_coupler(cplid)%rstfile))
  CALL oasis_pprinti(subname,2,'   tag          ',int1=prism_coupler(cplid)%tag)
  CALL oasis_pprinti(subname,2,'   seq          ',int1=prism_coupler(cplid)%seq)
  CALL oasis_pprinti(subname,2,'   maxtime      ',int1=prism_coupler(cplid)%maxtime)
  CALL oasis_pprinti(subname,2,'   dt, lag      ',int1=prism_coupler(cplid)%dt,int2=prism_coupler(cplid)%lag)
  CALL oasis_pprintc(subname,2,'   grid name    ',char1=TRIM(prism_part(parid)%gridname))
  CALL oasis_pprinti(subname,2,'   partid, size ',int1=parid,int2=prism_part(parid)%gsize)
  CALL oasis_pprinti(subname,2,'   partid, nx,ny',int1=prism_part(parid)%nx,int2=prism_part(parid)%ny)

  IF (mapid > 0) THEN
      CALL oasis_pprinti(subname,2,'   use map  mpid  ',int1=mapid)
      CALL oasis_pprintc(subname,2,'   use map  file  ',char1=TRIM(prism_mapper(mapid)%file))
      spart = prism_mapper(mapid)%spart
      dpart = prism_mapper(mapid)%dpart
      CALL oasis_pprinti(subname,2,'   conserve     ',int1=prism_coupler(cplid)%conserv)
      CALL oasis_pprintc(subname,2,'   location     ',char1=TRIM(prism_mapper(mapid)%loc))
      CALL oasis_pprintc(subname,2,'   opt,optval   ',char1=TRIM(prism_mapper(mapid)%opt), &
                         char2=' : ',char3=TRIM(prism_mapper(mapid)%optval))
      CALL oasis_pprinti(subname,2,'   s/d partids  ',int1=spart,int2=dpart)
      IF (spart > 0) THEN
          CALL oasis_pprintc(subname,2,'   from/to      ',char1=TRIM(prism_part(spart)%gridname),&
                             char2=' : ',char3=TRIM(prism_part(dpart)%gridname))
          CALL oasis_pprintc(subname,2,'   from/to      ',char1=TRIM(prism_part(spart)%gridname),&
                             char2=' : ',char3=TRIM(prism_part(dpart)%gridname))
          CALL oasis_pprinti(subname,2,'   from nx,ny   ',int1=prism_part(spart)%gsize, &
                              int2=prism_part(spart)%nx,int3=prism_part(spart)%ny)
      ENDIF
      IF (dpart > 0) THEN
          CALL oasis_pprinti(subname,2,'   to nx,ny     ',int1=prism_part(dpart)%gsize,&
                           int2=prism_part(dpart)%nx,int3=prism_part(dpart)%ny)
      ENDIF
  ENDIF

  call prism_sys_debug_exit(subname)

  END SUBROUTINE prism_coupler_print

!------------------------------------------------------------
  SUBROUTINE prism_coupler_genmap(mapid,namid)

  IMPLICIT NONE

  integer(ip_i4_p), intent(in) :: mapid
  integer(ip_i4_p), intent(in) :: namid
  !----------------------------------------------------------

  integer(ip_i4_p)              :: src_size,src_rank, ncrn_src
  integer(ip_i4_p) ,allocatable :: src_dims(:),src_mask(:)
  real(ip_double_p),allocatable :: src_lon(:),src_lat(:)
  real(ip_double_p),allocatable :: src_corner_lon(:,:),src_corner_lat(:,:)
  integer(ip_i4_p)              :: dst_size,dst_rank, ncrn_dst
  integer(ip_i4_p) ,allocatable :: dst_dims(:),dst_mask(:)
  real(ip_double_p),allocatable :: dst_lon(:),dst_lat(:)
  real(ip_double_p),allocatable :: dst_corner_lon(:,:),dst_corner_lat(:,:)
  integer(ip_i4_p) ,allocatable :: ifld2(:,:)
  real(ip_double_p),allocatable :: fld2(:,:),fld3(:,:,:)
  integer(ip_i4_p) :: i,j,k,icnt,nx,ny,nc
  logical :: lextrapdone
  logical :: do_corners
  character(len=ic_med) :: filename
  character(len=ic_med) :: fldname
  character(len=*),parameter :: subname = 'prism_coupler_genmap'

  call prism_sys_debug_enter(subname)

  lextrapdone = .false.
  nx = -1  ! must be read
  ny = -1  ! must be read
  nc =  1  ! might not be read, set to something reasonable

  !--- checks first ---

  if (trim(namscrtyp(namID)) /= 'SCALAR') then
     CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
     CALL oasis_pprintc(subname,2,' error :',char1='only scrip type SCALAR mapping supported')
     CALL prism_sys_abort()
  endif

  if (trim(namscrmet(namID)) == 'CONSERV' .and. trim(namscrord(namID)) /= 'FIRST') then
     CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
     CALL oasis_pprintc(subname,2,' error :',char1='only FIRST ORDER mapping supported for CONSERV')
     call prism_sys_abort()
  endif

  if (trim(namscrmet(namID)) == 'BICUBIC') then
     CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
     CALL oasis_pprintc(subname,2,' error :',char1=' BICUBIC mapping not yet supported')
     call prism_sys_abort()
  endif
  
  do_corners = .false.
  if (trim(namscrmet(namID)) == 'CONSERV') then
     do_corners = .true.
  endif

  !--- src data ---

  filename = 'grids.nc'
  if (do_corners) then
     fldname = trim(namsrcgrd(namID))//'.clo'
     call prism_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny,nz=nc)
  else
     fldname = trim(namsrcgrd(namID))//'.lon'
     call prism_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny)
  endif
  CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=trim(fldname))
  CALL oasis_pprinti(subname,15,' nx,ny : ',int1=nx,int2=ny)
  CALL oasis_pprinti(subname,15,' nc : ',int1=nc)
  CALL oasis_pprintl(subname,15,' do_corners : ',log1=do_corners)
  src_rank = 2
  src_size = nx*ny
  allocate(src_dims(src_rank))
  src_dims(1) = nx
  src_dims(2) = ny
  ncrn_src = nc
  allocate(src_mask(src_size))
  allocate(src_lon (src_size))
  allocate(src_lat (src_size))
  allocate(src_corner_lon(ncrn_src,src_size))
  allocate(src_corner_lat(ncrn_src,src_size))

  allocate(ifld2(nx,ny))
  filename = 'masks.nc'
  fldname = trim(namsrcgrd(namID))//'.msk'
  call prism_io_read_field_fromroot(filename,fldname,ifld2=ifld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     src_mask(icnt) = ifld2(i,j)
  enddo; enddo
  CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=TRIM(fldname))
  CALL oasis_pprinti(subname,15,' min and max src_mask : ',int1=minval(src_mask),int2=maxval(src_mask))
  deallocate(ifld2)

  allocate(fld2(nx,ny))
  filename = 'grids.nc'
  fldname = trim(namsrcgrd(namID))//'.lon'
  call prism_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     src_lon(icnt) = fld2(i,j)
  enddo; enddo
  CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=TRIM(fldname))
  CALL oasis_pprintr(subname,15,' min and max src_lon : ',r1=minval(src_lon),r2=maxval(src_lon))
  fldname = trim(namsrcgrd(namID))//'.lat'
  call prism_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     src_lat(icnt) = fld2(i,j)
  enddo; enddo
  CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=TRIM(fldname))
  CALL oasis_pprintr(subname,15,' min and max src_lat : ',r1=minval(src_lat),r2=maxval(src_lat))
  deallocate(fld2)

  if (do_corners) then
     allocate(fld3(nx,ny,nc))
     filename = 'grids.nc'
     fldname = trim(namsrcgrd(namID))//'.clo'
     call prism_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           src_corner_lon(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=TRIM(fldname))
     CALL oasis_pprintr(subname,15,' min and max src_clo : ',r1=MINVAL(src_corner_lon),&
                        r2=MAXVAL(src_corner_lon))
     fldname = trim(namsrcgrd(namID))//'.cla'
     call prism_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           src_corner_lat(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=TRIM(fldname))
     CALL oasis_pprintr(subname,15,' min and max src_cla : ',r1=MINVAL(src_corner_lat),&
                        r2=MAXVAL(src_corner_lat))
     deallocate(fld3)
  else
     src_corner_lon = -9999.
     src_corner_lat = -9999.
  endif

  !--- dst data ---

  filename = 'grids.nc'
  if (do_corners) then
     fldname = trim(namdstgrd(namID))//'.clo'
     call prism_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny,nz=nc)
  else
     fldname = trim(namdstgrd(namID))//'.lon'
     call prism_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny)
  endif
  CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=trim(fldname))
  CALL oasis_pprinti(subname,15,' nx,ny,nc : ',int1=nx,int2=ny,int3=nc)
  dst_rank = 2
  dst_size = nx*ny
  allocate(dst_dims(dst_rank))
  dst_dims(1) = nx
  dst_dims(2) = ny
  ncrn_dst = nc
  allocate(dst_mask(dst_size))
  allocate(dst_lon (dst_size))
  allocate(dst_lat (dst_size))
  allocate(dst_corner_lon(ncrn_dst,dst_size))
  allocate(dst_corner_lat(ncrn_dst,dst_size))

  allocate(ifld2(nx,ny))
  filename = 'masks.nc'
  fldname = trim(namdstgrd(namID))//'.msk'
  call prism_io_read_field_fromroot(filename,fldname,ifld2=ifld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     dst_mask(icnt) = ifld2(i,j)
  enddo; enddo
  CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=TRIM(fldname))
  CALL oasis_pprinti(subname,15,' min and max dst_mask : ',int1=minval(dst_mask),int2=maxval(dst_mask))
  deallocate(ifld2)

  allocate(fld2(nx,ny))
  filename = 'grids.nc'
  fldname = trim(namdstgrd(namID))//'.lon'
  call prism_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     dst_lon(icnt) = fld2(i,j)
  enddo; enddo
  CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=TRIM(fldname))
  CALL oasis_pprintr(subname,15,' min and max dst_lon : ',r1=minval(dst_lon),r2=maxval(dst_lon))
  fldname = trim(namdstgrd(namID))//'.lat'
  call prism_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     dst_lat(icnt) = fld2(i,j)
  enddo; enddo
  CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=TRIM(fldname))
  CALL oasis_pprintr(subname,15,' min and max dst_lat : ',r1=minval(dst_lat),r2=maxval(dst_lat))
  deallocate(fld2)

  if (do_corners) then
     allocate(fld3(nx,ny,nc))
     filename = 'grids.nc'
     fldname = trim(namdstgrd(namID))//'.clo'
     call prism_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           dst_corner_lon(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=TRIM(fldname))
     CALL oasis_pprintr(subname,15,' min and max dst_clo : ',r1=MINVAL(dst_corner_lon),&
                                     r2=MAXVAL(dst_corner_lon))
     fldname = trim(namdstgrd(namID))//'.cla'
     call prism_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           dst_corner_lat(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     CALL oasis_pprintc(subname,15,' read : ',char1=TRIM(filename),char2=' : ',char3=TRIM(fldname))
     CALL oasis_pprintr(subname,15,' min and max dst_cla : ',r1=MINVAL(dst_corner_lat),&
                                      r2=MAXVAL(dst_corner_lat))
     deallocate(fld3)
  else
     dst_corner_lon = -9999.
     dst_corner_lat = -9999.
  endif

  CALL oasis_pprintc(subname,15,' : ',char1=' call grid_init ')

  !--- 0/1 mask convention opposite in scrip vs oasis
  src_mask = 1 - src_mask
  dst_mask = 1 - dst_mask
  call grid_init(namscrmet(namID),namscrres(namID),namscrbin(namID),  &
       src_size, dst_size, src_dims, dst_dims, &
       src_rank, dst_rank, ncrn_src, ncrn_dst, &
       src_mask, dst_mask, namsrcgrd(namID), namdstgrd(namID), &
       src_lat,  src_lon,  dst_lat,  dst_lon, &
       src_corner_lat, src_corner_lon, &
       dst_corner_lat, dst_corner_lon, &
       logunit=nulprt)
  CALL oasis_pprintc(subname,15,' : ',char1=' done grid_init ')

  CALL oasis_pprintc(subname,15,' : ',char1=' call scrip ')
  call scrip(prism_mapper(mapid)%file,prism_mapper(mapid)%file,namscrmet(namID), &
             namscrnor(namID),lextrapdone,namscrvam(namID),namscrnbr(namID))
  CALL oasis_pprintc(subname,15,' : ',char1=' done scrip ')

  deallocate(src_dims, dst_dims)
  deallocate(src_mask)
  deallocate(src_lon)
  deallocate(src_lat)
  deallocate(src_corner_lon)
  deallocate(src_corner_lat)
  deallocate(dst_mask)
  deallocate(dst_lon)
  deallocate(dst_lat)
  deallocate(dst_corner_lon)
  deallocate(dst_corner_lat)

  call prism_sys_debug_exit(subname)

  END SUBROUTINE prism_coupler_genmap

!------------------------------------------------------------

!BOP ===========================================================================
!
! !IROUTINE:  prism_coupler_sMatReaddnc - Do a distributed read of a NetCDF SCRIP file and
!                                return weights in a distributed SparseMatrix
!
! !DESCRIPTION: 
!     Read in mapping matrix data from a SCRIP netCDF data file using
!     a low memory method and then scatter to all pes.  Based on 
!     prism_coupler_sMatReaddnc from CESM1.0.3
!
! !REMARKS:
!   This routine leverages gsmaps to determine scatter pattern
!   The scatter is implemented as a bcast of all weights then a local
!     computation on each pe to determine with weights to keep based
!     on gsmap information.
!   The algorithm to determine whether a weight belongs on a pe involves
!     creating a couple local arrays (lsstart and lscount) which are
!     the local values of start and length from the gsmap.  these are
!     sorted via a bubble sort and then searched via a binary search
!     to check whether a global index is on the local pe.
!   The local buffer sizes are estimated up front based on ngridcell/npes
!     plus 20% (see 1.2 below).  If the local buffer size fills up, then
!     the buffer is reallocated 50% large (see 1.5 below) and the fill
!     continues.  The idea is to trade off memory reallocation and copy
!     with memory usage.  1.2 and 1.5 are arbitary, other values may
!     result in better performance.
!   Once all the matrix weights have been read, the sMat is initialized,
!     the values from the buffers are copied in, and everything is deallocated.
!
! !INTERFACE:  -----------------------------------------------------------------

subroutine prism_coupler_sMatReaddnc(sMat,SgsMap,DgsMap,newdom, &
                            fileName,mytask,mpicom, &
                            areasrc,areadst,ni_i,nj_i,ni_o,nj_o )

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p

! !INPUT/OUTPUT PARAMETERS:

   type(mct_sMat)  ,intent(out)           :: sMat    ! mapping data
   type(mct_gsMap) ,intent(in) ,target    :: SgsMap  ! src gsmap
   type(mct_gSMap) ,intent(in) ,target    :: DgsMap  ! dst gsmap
   character(*)    ,intent(in)            :: newdom  ! type of sMat (src or dst)
        ! src = rearrange and map (bfb), dst = map and rearrange (partial sums)
   character(*)    ,intent(in)            :: filename! netCDF file to read
   integer(IN)     ,intent(in)            :: mytask   ! processor id
   integer(IN)     ,intent(in)            :: mpicom  ! communicator
   type(mct_Avect) ,intent(out), optional :: areasrc ! area of src grid from mapping file
   type(mct_Avect) ,intent(out), optional :: areadst ! area of dst grid from mapping file
   integer(IN)     ,intent(out), optional :: ni_i    ! number of lons on input grid   
   integer(IN)     ,intent(out), optional :: nj_i    ! number of lats on input grid   
   integer(IN)     ,intent(out), optional :: ni_o    ! number of lons on output grid   
   integer(IN)     ,intent(out), optional :: nj_o    ! number of lats on output grid   

! !EOP

   !--- local ---
   integer(IN)           :: n,m     ! generic loop indicies
   integer(IN)           :: na      ! size of source domain
   integer(IN)           :: nb      ! size of destination domain
   integer(IN)           :: ns      ! number of non-zero elements in matrix
   integer(IN)           :: nwgts   ! number of weights per element
   integer(IN)           :: ni,nj   ! number of row and col in the matrix
   integer(IN)           :: igrow   ! aVect index for matrix row
   integer(IN)           :: igcol   ! aVect index for matrix column
   integer(IN)           :: iwgt    ! aVect index for matrix element
   integer(IN)           :: iarea   ! aVect index for area
   integer(IN)           :: rsize   ! size of read buffer
   integer(IN)           :: cnt     ! local num of wgts
   integer(IN)           :: cntold  ! local num of wgts, previous read
   integer(IN)           :: start(1)! netcdf read
   integer(IN)           :: count(1)! netcdf read
   integer(IN)           :: start2(2)! netcdf read
   integer(IN)           :: count2(2)! netcdf read
   integer(IN)           :: bsize   ! buffer size
   integer(IN)           :: nread   ! number of reads 
   logical               :: mywt    ! does this weight belong on my pe
   integer(IN)           :: dims(2) 

   !--- buffers for i/o ---
   real(R8)   ,allocatable :: rtemp(:) ! real temporary
   real(R8)   ,allocatable :: Sbuf(:)  ! real weights
   real(R8)   ,allocatable :: remaps(:,:)  ! real weights with num_wgts dim
   integer(IN),allocatable :: Rbuf(:)  ! ints rows
   integer(IN),allocatable :: Cbuf(:)  ! ints cols

   !--- variables associated with local computation of global indices
   integer(IN)             :: lsize     ! size of local seg map
   integer(IN)             :: commsize  ! size of local communicator
   integer(IN),allocatable :: lsstart(:) ! local seg map info
   integer(IN),allocatable :: lscount(:) ! local seg map info
   type(mct_gsMap),pointer :: mygsmap ! pointer to one of the gsmaps
   integer(IN)             :: l1,l2     ! generice indices for sort
   logical                 :: found     ! for sort

   !--- variable assocaited with local data buffers and reallocation
   real(R8)   ,allocatable :: Snew(:),Sold(:)  ! reals
   integer(IN),allocatable :: Rnew(:),Rold(:)  ! ints
   integer(IN),allocatable :: Cnew(:),Cold(:)  ! ints

   character,allocatable :: str(:)  ! variable length char string
   character(len=ic_long):: attstr  ! netCDF attribute name string
   integer(IN)           :: status   ! netCDF routine return code
   integer(IN)           :: fid     ! netCDF file      ID
   integer(IN)           :: vid     ! netCDF variable  ID
   integer(IN)           :: did     ! netCDF dimension ID
   !--- arbitrary size of read buffer, this is the chunk size weights reading
   integer(IN),parameter :: rbuf_size = 100000

   !--- global source and destination areas ---
   type(mct_Avect) :: areasrc0   ! area of src grid from mapping file
   type(mct_Avect) :: areadst0   ! area of src grid from mapping file

   character(*),parameter :: areaAV_field = 'aream'

   !--- formats ---
   character(*),parameter :: subName = 'prism_coupler_sMatReaddnc'
   character(*),parameter :: F00 = '("prism_coupler_sMatReaddnc",1x,4a)'
   character(*),parameter :: F01 = '("prism_coupler_sMatReaddnc",1x,2(a,i10))'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

 call prism_sys_debug_enter(subname)
 call prism_mpi_commsize(mpicom,commsize)
 if (mytask == 0) then
     CALL oasis_pprintc(subname,2,' : ',char1=' reading mapping matrix data decomposed...')

   !----------------------------------------------------------------------------
   ! open & read the file
   !----------------------------------------------------------------------------
   CALL oasis_pprintc(subname,2,'* file name                  : ',char1=trim(fileName))
   status = nf_open(filename,NF_NOWRITE,fid)
   if (status /= NF_NOERR) then
      CALL oasis_pprintc(subname,2,' : ',char1=TRIM(nf_strerror(status)))
      CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
      CALL oasis_pprintc(subname,2,' error :',char1=filename)
      call prism_sys_abort()
   endif

   !--- get matrix dimensions ----------
!  status = nf_inq_dimid (fid, 'n_s', did)  ! size of sparse matrix
   status = nf_inq_dimid (fid, 'num_links', did)  ! size of sparse matrix
   status = nf_inq_dimlen(fid, did  , ns)
!  status = nf_inq_dimid (fid, 'n_a', did)  ! size of  input vector
   status = nf_inq_dimid (fid, 'src_grid_size', did)  ! size of  input vector
   status = nf_inq_dimlen(fid, did  , na)
!  status = nf_inq_dimid (fid, 'n_b', did)  ! size of output vector
   status = nf_inq_dimid (fid, 'dst_grid_size', did)  ! size of output vector
   status = nf_inq_dimlen(fid, did  , nb)
   status = nf_inq_dimid (fid, 'num_wgts', did)  ! size of output vector
   status = nf_inq_dimlen(fid, did  , nwgts)
   
   if (present(ni_i) .and. present(nj_i) .and. present(ni_o) .and. present(nj_o)) then
!     status = nf_inq_dimid (fid, 'ni_a', did)  ! number of lons in input grid
!     status = nf_inq_dimlen(fid, did  , ni_i)
!     status = nf_inq_dimid (fid, 'nj_a', did)  ! number of lats in input grid
!     status = nf_inq_dimlen(fid, did  , nj_i)
!     status = nf_inq_dimid (fid, 'ni_b', did)  ! number of lons in output grid
!     status = nf_inq_dimlen(fid, did  , ni_o)
!     status = nf_inq_dimid (fid, 'nj_b', did)  ! number of lats in output grid
!     status = nf_inq_dimlen(fid, did  , nj_o)
      status = nf_inq_varid(fid, 'src_grid_dims', vid)
      status = nf_get_var_int(fid, vid, dims)
      ni_i = dims(1)
      nj_i = dims(2)
      status = nf_inq_varid(fid, 'dst_grid_dims', vid)
      status = nf_get_var_int(fid, vid, dims)
      ni_o = dims(1)
      nj_o = dims(2)
   end if
   CALL oasis_pprinti(subname,2,'* matrix dims src x dst      : ',int1=na,int2=nb)
   CALL oasis_pprinti(subname,2,'* number of non-zero elements : ',int1=ns)

 endif
 
   !--- read and load area_a ---
   if (present(areasrc)) then
   if (mytask == 0) then
      call mct_aVect_init(areasrc0,' ',areaAV_field,na)
!     status = nf_inq_varid     (fid,'area_a',vid)
      status = nf_inq_varid     (fid,'src_grid_area',vid)
      if (status /= NF_NOERR) CALL oasis_pprintc(subname,2,' : ',char1=TRIM(nf_strerror(status)))
      status = nf_get_var_double(fid, vid, areasrc0%rAttr)
      if (status /= NF_NOERR) CALL oasis_pprintc(subname,2,' : ',char1=TRIM(nf_strerror(status)))
   endif
   call mct_aVect_scatter(areasrc0, areasrc, SgsMap, 0, mpicom, status)
   if (status /= 0) call mct_die(subname,"Error on scatter of areasrc0")
   if (mytask == 0) then
!      if (present(dbug)) then
!         if (dbug > 2) then
!            write(nulprt,*) subName,'Size of src ',mct_aVect_lSize(areasrc0)
!            write(nulprt,*) subName,'min/max src ',minval(areasrc0%rAttr(1,:)),maxval(areasrc0%rAttr(1,:))
!         endif
!      end if
      call mct_aVect_clean(areasrc0)
   end if
   end if

   !--- read and load area_b ---
   if (present(areadst)) then
   if (mytask == 0) then
      call mct_aVect_init(areadst0,' ',areaAV_field,nb)
!     status = nf_inq_varid     (fid,'area_b',vid)
      status = nf_inq_varid     (fid,'dst_grid_area',vid)
      if (status /= NF_NOERR) CALL oasis_pprintc(subname,2,' : ',char1=TRIM(nf_strerror(status)))
      status = nf_get_var_double(fid, vid, areadst0%rAttr)
      if (status /= NF_NOERR) CALL oasis_pprintc(subname,2,' : ',char1=TRIM(nf_strerror(status)))
   endif
   call mct_aVect_scatter(areadst0, areadst, DgsMap, 0, mpicom, status)
   if (status /= 0) call mct_die(subname,"Error on scatter of areadst0")
   if (mytask == 0) then
!      if (present(dbug)) then
!         if (dbug > 2) then
!            write(nulprt,*) subName,'Size of dst ',mct_aVect_lSize(areadst0)
!            write(nulprt,*) subName,'min/max dst ',minval(areadst0%rAttr(1,:)),maxval(areadst0%rAttr(1,:))
!         endif
!      end if
      call mct_aVect_clean(areadst0)
   endif
   endif

   if (present(ni_i) .and. present(nj_i) .and. present(ni_o) .and. present(nj_o)) then
      call prism_mpi_bcast(ni_i,mpicom,subName//" MPI in ni_i bcast")
      call prism_mpi_bcast(nj_i,mpicom,subName//" MPI in nj_i bcast")
      call prism_mpi_bcast(ni_o,mpicom,subName//" MPI in ni_o bcast")
      call prism_mpi_bcast(nj_o,mpicom,subName//" MPI in nj_o bcast")
   end if

   call prism_mpi_bcast(ns,mpicom,subName//" MPI in ns bcast")
   call prism_mpi_bcast(na,mpicom,subName//" MPI in na bcast")
   call prism_mpi_bcast(nb,mpicom,subName//" MPI in nb bcast")

   !--- setup local seg map, sorted
   if (newdom == 'src') then
      mygsmap => DgsMap
   elseif (newdom == 'dst') then
      mygsmap => SgsMap
   else
      CALL oasis_pprintc(subname,2,'ERROR: invalid newdom value = ',char1=newdom)
      CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
      CALL oasis_pprintc(subname,2,' error :',char1=' invalid newdom value')
      call prism_sys_abort()
   endif
   lsize = 0
   do n = 1,size(mygsmap%start)
      if (mygsmap%pe_loc(n) == mytask) then
         lsize=lsize+1
      endif
   enddo
   allocate(lsstart(lsize),lscount(lsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Lsstart',status)

   lsize = 0
   do n = 1,size(mygsmap%start)
      if (mygsmap%pe_loc(n) == mytask) then  ! on my pe
         lsize=lsize+1
         found = .false.
         l1 = 1
         do while (.not.found .and. l1 < lsize)         ! bubble sort copy
            if (mygsmap%start(n) < lsstart(l1)) then
               do l2 = lsize, l1+1, -1
                  lsstart(l2) = lsstart(l2-1)
                  lscount(l2) = lscount(l2-1)
               enddo
               found = .true.
            else
               l1 = l1 + 1
            endif
         enddo
         lsstart(l1) = mygsmap%start(n)
         lscount(l1) = mygsmap%length(n)
      endif
   enddo
   do n = 1,lsize-1
      if (lsstart(n) > lsstart(n+1)) then
         CALL oasis_pprinti(subname,2,' abort by model compid ',int1=compid)
         CALL oasis_pprintc(subname,2,' error :',char1=' lsstart not properly sorted')
         CALL prism_sys_abort()
      endif
   enddo

   rsize = min(rbuf_size,ns)                     ! size of i/o chunks
   bsize = ((ns/commsize) + 1 ) * 1.2   ! local temporary buffer size
   if (ns == 0) then
      nread = 0
   else
      nread = (ns-1)/rsize + 1                      ! num of reads to do
   endif

   if (mytask == 0) then
      allocate(remaps(nwgts,rsize),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate remaps',status)
   endif
   allocate(Sbuf(rsize),Rbuf(rsize),Cbuf(rsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Sbuf',status)
   allocate(Snew(bsize),Cnew(bsize),Rnew(bsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Snew1',status)

   cnt = 0
   do n = 1,nread
      start(1) = (n-1)*rsize + 1
      count(1) = min(rsize,ns-start(1)+1)
      start2(1) = 1
      count2(1) = nwgts
      start2(2) = start(1)
      count2(2) = count(1)

      !--- read data on root pe
      if (mytask== 0) then
!        status = nf_inq_varid      (fid,'S'  ,vid)
         status = nf_inq_varid      (fid,'remap_matrix'  ,vid)
!        status = nf_get_vara_double(fid,vid,start,count,Sbuf)
         status = nf_get_vara_double(fid,vid,start2,count2,remaps)
         Sbuf(:) = remaps(1,:)
         if (status /= NF_NOERR) CALL oasis_pprintc(subname,2,' : ',char1=TRIM(nf_strerror(status)))

!        status = nf_inq_varid      (fid,'row',vid)
         status = nf_inq_varid      (fid,'dst_address',vid)
         status = nf_get_vara_int   (fid,vid,start,count,Rbuf)
         if (status /= NF_NOERR) CALL oasis_pprintc(subname,2,' : ',char1=TRIM(nf_strerror(status)))

!        status = nf_inq_varid      (fid,'col',vid)
         status = nf_inq_varid      (fid,'src_address',vid)
         status = nf_get_vara_int   (fid,vid,start,count,Cbuf)
         if (status /= NF_NOERR) CALL oasis_pprintc(subname,2,' : ',char1=TRIM(nf_strerror(status)))
      endif

      !--- send S, row, col to all pes
      call prism_mpi_bcast(Sbuf,mpicom,subName//" MPI in Sbuf bcast")
      call prism_mpi_bcast(Rbuf,mpicom,subName//" MPI in Rbuf bcast")
      call prism_mpi_bcast(Cbuf,mpicom,subName//" MPI in Cbuf bcast")

      !--- now each pe keeps what it should
      do m = 1,count(1)
         !--- should this weight be on my pe
         if (newdom == 'src') then
            mywt = check_myindex(Rbuf(m),lsstart,lscount)
         elseif (newdom == 'dst') then
            mywt = check_myindex(Cbuf(m),lsstart,lscount)
         endif

         if (mywt) then
            cntold = cnt
            cnt = cnt + 1

            !--- new arrays need to be bigger
            if (cnt > bsize) then
               !--- allocate old arrays and copy new into old
               allocate(Sold(cntold),Rold(cntold),Cold(cntold),stat=status)
               if (status /= 0) call mct_perr_die(subName,':: allocate old',status)
               Sold(1:cntold) = Snew(1:cntold)
               Rold(1:cntold) = Rnew(1:cntold)
               Cold(1:cntold) = Cnew(1:cntold)

               !--- reallocate new to bigger size, increase buffer by 50% (arbitrary)
               deallocate(Snew,Rnew,Cnew,stat=status)
               if (status /= 0) call mct_perr_die(subName,':: allocate new',status)
               bsize = 1.5 * bsize
               CALL oasis_pprinti(subname,15,' reallocate bsize to : ',int1=bsize)
               allocate(Snew(bsize),Rnew(bsize),Cnew(bsize),stat=status)
               if (status /= 0) call mct_perr_die(subName,':: allocate old',status)

               !--- copy data back into new
               Snew(1:cntold) = Sold(1:cntold)
               Rnew(1:cntold) = Rold(1:cntold)
               Cnew(1:cntold) = Cold(1:cntold)
               deallocate(Sold,Rold,Cold,stat=status)
               if (status /= 0) call mct_perr_die(subName,':: deallocate old',status)
            endif

            Snew(cnt) = Sbuf(m)
            Rnew(cnt) = Rbuf(m)
            Cnew(cnt) = Cbuf(m)
         endif
      enddo  ! count
   enddo   ! nread

   if (mytask == 0) then
      deallocate(remaps, stat=status)
      if (status /= 0) call mct_perr_die(subName,':: deallocate remaps',status)
   endif
   deallocate(Sbuf,Rbuf,Cbuf, stat=status)
   if (status /= 0) call mct_perr_die(subName,':: deallocate Sbuf',status)

   !----------------------------------------------------------------------------
   ! init the mct sMat data type
   !----------------------------------------------------------------------------
   ! mct_sMat_init must be given the number of rows and columns that
   ! would be in the full matrix.  Nrows= size of output vector=nb.
   ! Ncols = size of input vector = na.
   call mct_sMat_init(sMat, nb, na, cnt)

   igrow = mct_sMat_indexIA(sMat,'grow')
   igcol = mct_sMat_indexIA(sMat,'gcol')
   iwgt  = mct_sMat_indexRA(sMat,'weight')

   if (cnt /= 0) then
      sMat%data%rAttr(iwgt ,1:cnt) = Snew(1:cnt)
      sMat%data%iAttr(igrow,1:cnt) = Rnew(1:cnt)
      sMat%data%iAttr(igcol,1:cnt) = Cnew(1:cnt)
   endif
   deallocate(Snew,Rnew,Cnew, stat=status)
   deallocate(lsstart,lscount,stat=status)
   if (status /= 0) call mct_perr_die(subName,':: deallocate new',status)

   if (mytask == 0) then
      status = nf_close(fid)
      CALL oasis_pprintc(subname,2,' : ',char1='... done reading file')
   endif

  call prism_sys_debug_exit(subname)

end subroutine prism_coupler_sMatReaddnc

!------------------------------------------------------------
! !BOP ===========================================================================
!
! !IROUTINE:  check_myindex - binary search for index in list
!
! !DESCRIPTION: 
!     Do a binary search to see if a value is contained in a list of
!     values.  return true or false.  starti must be monotonically
!     increasing, function does NOT check this.
!
! !INTERFACE:  -----------------------------------------------------------------

logical function check_myindex(index,starti,counti)

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p

! !INPUT/OUTPUT PARAMETERS:

   integer(IN) :: index       ! is this index in start/count list
   integer(IN) :: starti(:)   ! start list
   integer(IN) :: counti(:)   ! count list

! !EOP

   !--- local ---
   integer(IN)    :: nl,nc,nr,ncprev 
   integer(IN)    :: lsize
   logical        :: stopnow

   !--- formats ---
   character(*),parameter :: subName = '(check_myindex) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   call prism_sys_debug_enter(subname)
   check_myindex = .false.

   lsize = size(starti)
   if (lsize < 1) then
!     call prism_sys_debug_exit(subname)
      return
   endif

   nl = 0
   nr = lsize + 1
   nc = (nl+nr)/2
   stopnow = .false.
   do while (.not.stopnow)
      if (index < starti(nc)) then
         nr = nc
      elseif (index > (starti(nc) + counti(nc) - 1)) then
         nl = nc
      else
         check_myindex = .true.
!        call prism_sys_debug_exit(subname)
         return
      endif
      ncprev = nc
      nc = (nl + nr)/2
      if (nc == ncprev .or. nc < 1 .or. nc > lsize) stopnow = .true.
   enddo

   check_myindex = .false.

!   call prism_sys_debug_exit(subname)

end function check_myindex

!===============================================================================
END MODULE mod_prism_coupler


