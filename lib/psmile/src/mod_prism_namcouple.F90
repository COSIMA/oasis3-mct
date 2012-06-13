MODULE mod_prism_namcouple

!     - - - - - - - - - - - - - - - - - - - - - - - - - - -

  USE mod_prism_kinds
  USE mod_prism_data
  USE mod_prism_parameters
  USE mod_prism_sys
  USE mod_prism_mpi
  USE mod_prism_string

  IMPLICIT NONE

  private

  public prism_namcouple_init

! NAMCOUPLE PUBLIC DATA

  INTEGER(kind=ip_i4_p) ,public :: prism_nmodels   ! number of models
  character(len=ic_lvar),public,pointer :: prism_modnam(:)  ! model names

  INTEGER(kind=ip_i4_p)   ,public :: nnamcpl       ! number of namcouple inputs
  INTEGER(kind=ip_i4_p)   ,public :: namruntim     ! namcouple runtime
  INTEGER(kind=ip_i4_p)   ,public :: namlogprt     ! namcouple nlogprt value
 
  character(len=ic_lvar)  ,public,pointer :: namsrcfld(:)  ! list of src fields
  character(len=ic_lvar)  ,public,pointer :: namdstfld(:)  ! list of dst fields
  character(len=ic_lvar)  ,public,pointer :: namsrcgrd(:)  ! src grid name
  integer(kind=ip_i4_p)   ,public,pointer :: namsrc_nx(:)  ! src nx grid size
  integer(kind=ip_i4_p)   ,public,pointer :: namsrc_ny(:)  ! src ny grid size
  character(len=ic_lvar)  ,public,pointer :: namdstgrd(:)  ! dst grid name
  integer(kind=ip_i4_p)   ,public,pointer :: namdst_nx(:)  ! dst nx grid size
  integer(kind=ip_i4_p)   ,public,pointer :: namdst_ny(:)  ! dst ny grid size
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namfldseq(:)  ! SEQ value
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namfldops(:)  ! operation, ip_expout,...
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namflddti(:)  ! coupling period (secs)
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namfldlag(:)  ! coupling lag (secs)
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namfldtrn(:)  ! fields transform, ip_instant,...
  integer(kind=ip_i4_p)   ,public,pointer :: namfldcon(:)  ! conserv fld operation
  character(len=ic_long)  ,public,pointer :: nammapfil(:)  ! mapping file name
  character(len=ic_med)   ,public,pointer :: nammaploc(:)  ! mapping location (src or dst pes)
  character(len=ic_med)   ,public,pointer :: nammapopt(:)  ! mapping option (bfb, sum, or opt)
  character(len=ic_med)   ,public,pointer :: namrstfil(:)  ! restart file name
  character(len=ic_med)   ,public,pointer :: naminpfil(:)  ! input file name
  logical                 ,public,pointer :: namchecki(:)  ! checkin flag
  logical                 ,public,pointer :: namchecko(:)  ! checkout flag
  REAL (kind=ip_realwp_p) ,public,pointer :: namfldsmu(:)  ! src multiplier term
  REAL (kind=ip_realwp_p) ,public,pointer :: namfldsad(:)  ! src additive term
  REAL (kind=ip_realwp_p) ,public,pointer :: namflddmu(:)  ! dst multipler term
  REAL (kind=ip_realwp_p) ,public,pointer :: namflddad(:)  ! dst additive term

  character(len=ic_med)   ,public,pointer :: namscrmet(:)  ! scrip method (CONSERV, DISTWGT, BILINEAR, BICUBIC, GAUSWGT)
  character(len=ic_med)   ,public,pointer :: namscrnor(:)  ! scrip conserv normalization (FRACAREA, DESTAREA, FRACNNEI)
  character(len=ic_med)   ,public,pointer :: namscrtyp(:)  ! scrip mapping type (SCALAR, VECTOR, VECTOR_I, VECTOR_J)
  character(len=ic_med)   ,public,pointer :: namscrord(:)  ! scrip conserve order (FIRST, SECOND)
  character(len=ic_med)   ,public,pointer :: namscrres(:)  ! scrip search restriction (LATLON, LATITUDE)
  REAL (kind=ip_realwp_p) ,public,pointer :: namscrvam(:)  ! scrip gauss weight distance weighting for GAUSWGT
  integer(kind=ip_i4_p)   ,public,pointer :: namscrnbr(:)  ! scrip number of neighbors for GAUSWGT and DISTWGT
  integer(kind=ip_i4_p)   ,public,pointer :: namscrbin(:)  ! script number of search bins

  !--- derived ---
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namfldsort(:) ! sorted namcpl list based on seq

!----------------------------------------------------------------
!   LOCAL ONLY BELOW HERE
!----------------------------------------------------------------

  integer(kind=ip_i4_p) :: nulin     ! namcouple IO unit number
  character(len=*),parameter :: cl_namcouple = 'namcouple'

! --- alloc_src
  INTEGER (kind=ip_intwp_p) :: il_err
! --- mod_unitncdf
!  INTEGER (kind=ip_intwp_p) :: nc_grdid, nc_mskid, nc_surid, n_reaty, n_intty
!  INTEGER (kind=ip_intwp_p) :: nc_scpid
!  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE ::  nc_inpid, nc_outid 
!  INTEGER (kind=ip_intwp_p) :: nc_invartimeid
  LOGICAL :: lncdfgrd
  LOGICAL :: lncdfrst
!  LOGICAL :: lncdfana
!  REAL(kind=ip_realwp_p) :: rtime_val
!  CHARACTER(len=20) :: nc_invartime_name
! --- mod_label
  CHARACTER(len=64),ALLOCATABLE::cfldlab(:)
  CHARACTER(len=5), PARAMETER :: cgrdnam = 'grids'  
  CHARACTER(len=5), PARAMETER :: cmsknam = 'masks' 
  CHARACTER(len=5), PARAMETER :: csurnam = 'areas' 
  CHARACTER(len=5), PARAMETER :: crednam = 'maskr'
  CHARACTER(len=4), PARAMETER :: cglonsuf = '.lon'
  CHARACTER(len=4), PARAMETER :: cglatsuf = '.lat'
  CHARACTER(len=4), PARAMETER :: crnlonsuf = '.clo'
  CHARACTER(len=4), PARAMETER :: crnlatsuf = '.cla'
  CHARACTER(len=4), PARAMETER :: cmsksuf = '.msk'
  CHARACTER(len=4), PARAMETER :: csursuf = '.srf'
  CHARACTER(len=4), PARAMETER :: cangsuf = '.ang'
! --- mod_parallel
!  INTEGER (kind=ip_intwp_p), DIMENSION(:,:), ALLOCATABLE :: nparal
  CHARACTER(len=8),DIMENSION(:),ALLOCATABLE :: cparal
! --- mod_rainbow
!  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: nmapp, nsubg
!  REAL (kind=ip_realwp_p), DIMENSION(:),ALLOCATABLE :: amapp, asubg
  LOGICAL,DIMENSION(:),ALLOCATABLE :: lmapp
  LOGICAL,DIMENSION(:),ALLOCATABLE :: lsubg
! --- mod_coast
!  INTEGER (kind=ip_intwp_p), DIMENSION(:,:), ALLOCATABLE :: npcoast
!  INTEGER (kind=ip_intwp_p) :: ncoast
  INTEGER (kind=ip_intwp_p) :: nfcoast
  LOGICAL :: lcoast
! --- mod_calendar
  INTEGER(kind=ip_intwp_p) :: ndate
!  INTEGER(kind=ip_intwp_p) :: njini
!  INTEGER(kind=ip_intwp_p) :: nmini
!  INTEGER(kind=ip_intwp_p) :: naini
!  INTEGER(kind=ip_intwp_p) :: njnow
!  INTEGER(kind=ip_intwp_p) :: njone
!  INTEGER(kind=ip_intwp_p) :: njtwo
!  INTEGER(kind=ip_intwp_p) :: ndtwo, nmnow,nmone, nmtwo, nanow, ndinc, nsrec
!  INTEGER(kind=ip_intwp_p) :: ndone, nmrec
  INTEGER(kind=ip_intwp_p) :: ncaltype
!  INTEGER(kind=ip_intwp_p) :: nddeb
!  INTEGER(kind=ip_intwp_p) :: nadeb
!  INTEGER(kind=ip_intwp_p) :: nmdeb
!  INTEGER(kind=ip_intwp_p) :: njdeb
!  INTEGER(kind=ip_intwp_p) :: ig_date(6)
! --- mod_timestep
  INTEGER (kind=ip_intwp_p) :: ntime
  INTEGER (kind=ip_intwp_p) :: niter
  INTEGER (kind=ip_intwp_p) :: nitfn
  INTEGER (kind=ip_intwp_p) :: nstep
!  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: mstep, mfcpl, mdt
! --- mod_parameter
  INTEGER (kind=ip_intwp_p),PARAMETER :: jpeight = 8, jpfour = 4, jpeighty = 80
  INTEGER (kind=ip_intwp_p) :: ig_nmodel   ! number of models (not including oasis)
  INTEGER (kind=ip_intwp_p) :: ig_nfield   ! number of oasis coupled fields
  INTEGER (kind=ip_intwp_p) :: ig_direct_nfield   ! number of direct coupled fields
  INTEGER (kind=ip_intwp_p) :: ig_total_nfield    ! number of total fields
  LOGICAL :: lg_oasis_field
  INTEGER (kind=ip_intwp_p) :: ig_maxcomb
!  INTEGER (kind=ip_intwp_p) :: ig_nwork, ig_work
!  INTEGER (kind=ip_intwp_p) :: ig_maxold, ig_maxnew
!  INTEGER (kind=ip_intwp_p) :: ig_maxold_grid, ig_maxnew_grid
!  INTEGER (kind=ip_intwp_p) :: ig_maxgrd, ig_total_maxgrd
  INTEGER (kind=ip_intwp_p) :: ig_maxwoa
  INTEGER (kind=ip_intwp_p) :: ig_maxnoa
  INTEGER (kind=ip_intwp_p) :: ig_maxmoa
  INTEGER (kind=ip_intwp_p) :: ig_maxsoa
  INTEGER (kind=ip_intwp_p) :: ig_maxnfm
  INTEGER (kind=ip_intwp_p) :: ig_maxnfg
  INTEGER (kind=ip_intwp_p) :: ig_maxnfp
  INTEGER (kind=ip_intwp_p) :: ig_maxnfs
  INTEGER (kind=ip_intwp_p) :: ig_maxnfn
  INTEGER (kind=ip_intwp_p) :: ig_maxext
  INTEGER (kind=ip_intwp_p) :: ig_maxnbn
! --- mod_printing
  INTEGER(kind=ip_intwp_p) :: nlogprt
! --- mod_hardware
  CHARACTER(len=4) :: cchan
! --- mod_experiment
  INTEGER (kind=ip_intwp_p) :: nmseq
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nbcplproc
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nbtotproc
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: iga_unitmod
  CHARACTER(len=4) :: cjobnam
  CHARACTER(len=6) , DIMENSION(:), ALLOCATABLE :: cmodnam
  CHARACTER(len=80), DIMENSION(:), ALLOCATABLE :: cmpiarg
  LOGICAL :: lmodinf
! --- mod_string
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: numlab
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_numlab
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: nfexch
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_ntrans
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_total_ntrans
!  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: nluinp
!  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: nluout
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: nlonbf
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: nlatbf
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: nlonaf
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: nlataf
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: nseqn
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_total_nseqn
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_freq
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_lag
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: nlagn 
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_invert
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_reverse
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_number_field
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_no_rstfile
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_total_state
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_local_trans
!  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_portin_id, ig_portout_id
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_grid_nbrbf
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_grid_nbraf
!  INTEGER (kind=ip_intwp_p)                          :: ifrqmin
  INTEGER (kind=ip_intwp_p)                          :: ig_nbr_rstfile
  INTEGER (kind=ip_intwp_p)                          :: ig_total_frqmin
  LOGICAL                  ,DIMENSION(:),ALLOCATABLE :: lg_state
  CHARACTER(len=ic_long)   ,DIMENSION(:),ALLOCATABLE :: cnaminp
  CHARACTER(len=ic_long)   ,DIMENSION(:),ALLOCATABLE :: cnamout
  CHARACTER(len=8)         ,DIMENSION(:,:),ALLOCATABLE :: canal 
  CHARACTER(len=8)         ,DIMENSION(:),ALLOCATABLE :: cg_name_rstfile
  CHARACTER(len=8)         ,DIMENSION(:),ALLOCATABLE :: cg_restart_file
  CHARACTER(len=8)         ,DIMENSION(:),ALLOCATABLE :: cficinp
  CHARACTER(len=8)         ,DIMENSION(:),ALLOCATABLE :: cficout
  CHARACTER(len=32)        ,DIMENSION(:),ALLOCATABLE :: cg_input_file
  CHARACTER(len=ic_long)   ,DIMENSION(:),ALLOCATABLE :: cg_input_field
  CHARACTER(len=ic_long)   ,DIMENSION(:),ALLOCATABLE :: cg_output_field
  CHARACTER(len=8)         ,DIMENSION(:),ALLOCATABLE :: cficbf
  CHARACTER(len=8)         ,DIMENSION(:),ALLOCATABLE :: cficaf
  CHARACTER(len=8)         ,DIMENSION(:),ALLOCATABLE :: cstate
  CHARACTER(len=4)         ,DIMENSION(:),ALLOCATABLE :: cga_locatorbf
  CHARACTER(len=4)         ,DIMENSION(:),ALLOCATABLE :: cga_locatoraf
! --- mod_analysis
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: neighbor
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: ntronca
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: ncofld
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: neighborg
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nbofld
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nbnfld
  INTEGER (kind=ip_intwp_p), DIMENSION(:,:), ALLOCATABLE :: nludat
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nlufil
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nlumap 
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nmapfl
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nmapvoi
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nlusub
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nsubfl
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nsubvoi
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nluext
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nextfl
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nosper
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: notper
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  ntinpflx
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  ntoutflx
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  nbins
  INTEGER (kind=ip_intwp_p) :: nlucor
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  nscripvoi
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  ig_assoc_input_field
  REAL (kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: amskval
  REAL (kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: amskvalnew
  REAL (kind=ip_realwp_p), DIMENSION(:,:), ALLOCATABLE :: acocoef
  REAL (kind=ip_realwp_p), DIMENSION(:,:), ALLOCATABLE :: abocoef
  REAL (kind=ip_realwp_p), DIMENSION(:,:), ALLOCATABLE :: abncoef
  REAL (kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: afldcoef
  REAL (kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: afldcobo
  REAL (kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: afldcobn
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cxordbf
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cyordbf
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cxordaf
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cyordaf
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cextmet
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cintmet
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cgrdtyp
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cfldtyp
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cfilfic
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cfilmet 
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cconmet
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cfldcoa
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cfldfin
  CHARACTER(len=8), DIMENSION(:,:),ALLOCATABLE :: ccofld
  CHARACTER(len=8), DIMENSION(:,:),ALLOCATABLE :: cbofld
  CHARACTER(len=8), DIMENSION(:,:),ALLOCATABLE :: cbnfld
  CHARACTER(len=8), DIMENSION(:,:),ALLOCATABLE :: ccofic
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cdqdt
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cgrdmap
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cmskrd
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cgrdsub
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: ctypsub
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cgrdext
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: csper
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: ctper
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cmap_method
  CHARACTER(len=ic_long), DIMENSION(:),ALLOCATABLE :: cmap_file
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cmaptyp
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cmapopt
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: corder
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cnorm_opt
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cfldtype
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: crsttype
  CHARACTER(len=8) :: cfldcor
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cg_assoc_input_field
  LOGICAL, DIMENSION(:),ALLOCATABLE :: lsurf
  LOGICAL, DIMENSION(:),ALLOCATABLE :: lrotate
!  LOGICAL :: lg_areas 
  LOGICAL :: lg_vector  
! --- mod_anais
!  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: ngint,nmint,nmesh
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: naismfl
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: naisgfl
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: naismvoi
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: naisgvoi
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: niwtm
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: niwtg
  REAL (kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: varmul
  LOGICAL, DIMENSION(:), ALLOCATABLE :: linit
! --- mod extrapol
!  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nextra
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: niwtn
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nninnfl 
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: niwtng
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nninnflg
!  INTEGER (kind=ip_intwp_p), DIMENSION(:,:), ALLOCATABLE :: iincre
!  INTEGER (kind=ip_intwp_p), DIMENSION(:,:,:), ALLOCATABLE :: iaddress
!  REAL (kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: aextra
!  REAL (kind=ip_realwp_p), DIMENSION(:,:,:), ALLOCATABLE :: zweights
  LOGICAL, DIMENSION(:), ALLOCATABLE :: lextra
  LOGICAL, DIMENSION(:), ALLOCATABLE :: lweight
!  LOGICAL, DIMENSION(:), ALLOCATABLE :: lextrapdone
!---------------------



!------------------------------------------------------------
CONTAINS
!------------------------------------------------------------
  SUBROUTINE prism_namcouple_init()

  IMPLICIT NONE

  !-----------------------------------------------------------
  integer(kind=ip_i4_p) :: n, nv, n1, loc
  integer(kind=ip_i4_p) :: ja, jf, jc
  integer(kind=ip_i4_p) :: il_iost
  integer(kind=ip_i4_p) :: maxunit
  character(len=*),parameter :: subname='prism_namcouple_init'
  !-----------------------------------------------------------

!  CALL prism_sys_debug_enter(subname)

  CALL prism_sys_unitget(nulin)
  OPEN (UNIT = nulin,FILE =cl_namcouple,STATUS='OLD', &
        FORM ='FORMATTED', IOSTAT = il_iost)

  IF (mpi_rank_global == 0) THEN
      IF (il_iost .NE. 0) THEN
          WRITE(nulprt1,*) subname,' ERROR opening namcouple file ',TRIM(cl_namcouple)
          WRITE (nulprt,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt,'(a)') ' error = ERROR opening namcouple file'
          CALL prism_sys_flush(nulprt1)
          CALL prism_sys_abort()
      ELSE
          WRITE(nulprt1,*) subname,' open namcouple file ',TRIM(cl_namcouple)
      ENDIF
  ENDIF

  call inipar_alloc()
  call alloc()
  call inipar()

  CALL prism_sys_unitfree(nulin)

  IF (mpi_rank_global == 0) THEN
      WRITE(nulprt1,*) subname,' allocating ig_nmodel+1',ig_nmodel+1
      WRITE(nulprt1,*) subname,' allocating ig_total_nfield',ig_total_nfield
  ENDIF

  allocate(prism_modnam(ig_nmodel+1), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "prism_modnam" allocation of experiment module',il_err,1)

  allocate(namsrcfld(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namsrcfld" allocation of experiment module',il_err,1)

  allocate(namdstfld(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namdstfld" allocation of experiment module',il_err,1)

  allocate(namsrcgrd(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namsrcgrd" allocation of experiment module',il_err,1)

  allocate(namsrc_nx(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namsrc_nx" allocation of experiment module',il_err,1)

  allocate(namsrc_ny(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namsrc_ny" allocation of experiment module',il_err,1)

  allocate(namdstgrd(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namdstgrd" allocation of experiment module',il_err,1)

  allocate(namdst_nx(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namdst_nx" allocation of experiment module',il_err,1)

  allocate(namdst_ny(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namdst_ny" allocation of experiment module',il_err,1)

  allocate(namfldseq(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldseq" allocation of experiment module',il_err,1)

  allocate(namfldops(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldops" allocation of experiment module',il_err,1)

  allocate(namfldtrn(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldtrn" allocation of experiment module',il_err,1)

  allocate(namfldcon(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldcon" allocation of experiment module',il_err,1)

  allocate(namflddti(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namflddti" allocation of experiment module',il_err,1)

  allocate(namfldlag(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldlag" allocation of experiment module',il_err,1)

  allocate(nammapfil(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nammapfil" allocation of experiment module',il_err,1)

  allocate(nammaploc(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nammaploc" allocation of experiment module',il_err,1)

  allocate(nammapopt(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nammapopt" allocation of experiment module',il_err,1)

  allocate(namrstfil(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namrstfil" allocation of experiment module',il_err,1)

  allocate(naminpfil(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "naminpfil" allocation of experiment module',il_err,1)

  allocate(namfldsort(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldsort" allocation of experiment module',il_err,1)

  allocate(namchecki(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namchecki" allocation of experiment module',il_err,1)

  allocate(namchecko(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namchecko" allocation of experiment module',il_err,1)

  allocate(namfldsmu(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldsmu" allocation of experiment module',il_err,1)

  allocate(namfldsad(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldsad" allocation of experiment module',il_err,1)

  allocate(namflddmu(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namflddmu" allocation of experiment module',il_err,1)

  allocate(namflddad(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namflddad" allocation of experiment module',il_err,1)

  allocate(namscrmet(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrmet" allocation of experiment module',il_err,1)

  allocate(namscrnor(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrnor" allocation of experiment module',il_err,1)

  allocate(namscrtyp(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrtyp" allocation of experiment module',il_err,1)

  allocate(namscrord(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrord" allocation of experiment module',il_err,1)

  allocate(namscrres(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrres" allocation of experiment module',il_err,1)

  allocate(namscrvam(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrvam" allocation of experiment module',il_err,1)

  allocate(namscrnbr(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrnbr" allocation of experiment module',il_err,1)

  allocate(namscrbin(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrbin" allocation of experiment module',il_err,1)

  prism_modnam(:) = trim(cspval)
  namsrcfld(:) = trim(cspval)
  namdstfld(:) = trim(cspval)
  namsrcgrd(:) = trim(cspval)
  namsrc_nx(:) = 0
  namsrc_ny(:) = 0
  namdstgrd(:) = trim(cspval)
  namdst_nx(:) = 0
  namdst_ny(:) = 0
  namfldseq(:) = -1
  namfldops(:) = -1
  namfldtrn(:) = ip_instant
  namfldcon(:) = ip_cnone
  namflddti(:) = -1
  namfldlag(:) = 0
  nammapfil(:) = "idmap"
  nammaploc(:) = "src"
  nammapopt(:) = "bfb"
  namrstfil(:) = trim(cspval)
  naminpfil(:) = trim(cspval)
  namchecki(:) = .false.
  namchecko(:) = .false.
  namfldsmu(:) = 1.0_ip_realwp_p
  namfldsad(:) = 0.0_ip_realwp_p
  namflddmu(:) = 1.0_ip_realwp_p
  namflddad(:) = 0.0_ip_realwp_p

  namscrmet(:) = trim(cspval)
  namscrnor(:) = trim(cspval)
  namscrtyp(:) = trim(cspval)
  namscrord(:) = trim(cspval)
  namscrres(:) = trim(cspval)
  namscrvam(:) = 1.0_ip_realwp_p
  namscrnbr(:) = -1
  namscrbin(:) = -1

  maxunit = maxval(iga_unitmod)
  IF (mpi_rank_global == 0) THEN
      WRITE(nulprt1,*) subname,' maximum unit number = ',maxunit
  ENDIF

  call prism_sys_unitsetmin(maxunit)

! without oasis coupler
  prism_nmodels = ig_nmodel

! with oasis coupler
!  prism_nmodels = ig_nmodel+1
!  prism_modnam(ig_nmodel+1) = "oasis"

  do n = 1,ig_nmodel
     prism_modnam(n) = trim(cmodnam(n))
  enddo

  IF (mpi_rank_global == 0) THEN
      WRITE(nulprt1,*) subname,' total number of models = ',prism_nmodels
      DO n = 1,prism_nmodels
        WRITE(nulprt1,*) subname,n,TRIM(prism_modnam(n))
      ENDDO
  ENDIF

  nnamcpl = ig_total_nfield
  namruntim = ntime
  namlogprt = nlogprt
  do jf = 1,ig_total_nfield
     namsrcfld(jf) = cg_input_field(jf)
     namdstfld(jf) = cg_output_field(jf)
     namfldseq(jf) = ig_total_nseqn(jf)
     namfldops(jf) = ig_total_state(jf)
     if (namfldops(jf) == ip_auxilary) then
         IF (mpi_rank_global == 0) THEN
             WRITE(nulprt1,*) subname,jf,'WARNING: AUXILARY NOT SUPPORTED'
         ENDIF
     endif
     if (namfldops(jf) == ip_ignored) then
        namfldops(jf) = ip_exported
        IF (mpi_rank_global == 0) THEN
            WRITE(nulprt1,*) subname,jf,'WARNING: IGNORED converted to EXPORTED'
        ENDIF
     endif
     if (namfldops(jf) == ip_ignout) then
        namfldops(jf) = ip_expout
        IF (mpi_rank_global == 0) THEN
            WRITE(nulprt1,*) subname,jf,'WARNING: IGNOUT converted to EXPOUT'
        ENDIF
     endif
     namflddti(jf) = ig_freq(jf)
     namfldlag(jf) = ig_lag(jf)
     namfldtrn(jf) = ig_local_trans(jf)
     namrstfil(jf) = trim(cg_restart_file(jf))
     naminpfil(jf) = trim(cg_input_file(jf))
     if (ig_number_field(jf) > 0) then
        namsrcgrd(jf) = trim(cficbf(ig_number_field(jf)))
        namsrc_nx(jf) = nlonbf(ig_number_field(jf))
        namsrc_ny(jf) = nlatbf(ig_number_field(jf))
        namdstgrd(jf) = trim(cficaf(ig_number_field(jf)))
        namdst_nx(jf) = nlonaf(ig_number_field(jf))
        namdst_ny(jf) = nlataf(ig_number_field(jf))
        do ja = 1, ig_ntrans(ig_number_field(jf))

           if (canal(ja,ig_number_field(jf)) .EQ. 'SCRIPR') then
              namscrmet(jf) = trim(cmap_method(ig_number_field(jf)))
              namscrnor(jf) = trim(cnorm_opt  (ig_number_field(jf)))
              namscrtyp(jf) = trim(cfldtype   (ig_number_field(jf)))
              namscrord(jf) = trim(corder     (ig_number_field(jf)))
              namscrres(jf) = trim(crsttype   (ig_number_field(jf)))
              namscrvam(jf) =      varmul     (ig_number_field(jf))
              namscrnbr(jf) =      nscripvoi  (ig_number_field(jf))
              namscrbin(jf) =      nbins      (ig_number_field(jf))
              IF (TRIM(namscrtyp(jf)) /= 'SCALAR') THEN
                  IF (mpi_rank_global == 0) THEN
                      WRITE(nulprt1,*) subname,jf,'WARNING: SCRIPR weights generation temporarily supported only for SCALAR mapping, not '//TRIM(namscrtyp(jf))
                      WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                      WRITE (nulprt1,'(a)') ' error = ERROR in SCRIPR CFTYP option'
                      CALL prism_sys_flush(nulprt1)
                  ENDIF
                  CALL prism_sys_abort()
              ENDIF

           elseif (canal(ja,ig_number_field(jf)) .EQ. 'MAPPING') then
              nammapfil(jf) = trim(cmap_file(ig_number_field(jf)))
              nammaploc(jf) = trim(cmaptyp(ig_number_field(jf)))
              nammapopt(jf) = trim(cmapopt(ig_number_field(jf)))

           elseif (canal(ja,ig_number_field(jf)) .EQ. 'CONSERV') then
              namfldcon(jf) = ip_cnone
              if (cconmet(ig_number_field(jf)) .EQ. 'GLOBAL') namfldcon(jf) = ip_cglobal
              if (cconmet(ig_number_field(jf)) .EQ. 'GLBPOS') namfldcon(jf) = ip_cglbpos
              if (cconmet(ig_number_field(jf)) .EQ. 'BASBAL') namfldcon(jf) = ip_cbasbal
              if (cconmet(ig_number_field(jf)) .EQ. 'BASPOS') namfldcon(jf) = ip_cbaspos
              if (namfldcon(jf) .EQ. ip_cnone) then
                  IF (mpi_rank_global == 0) THEN
                      WRITE(nulprt1,*) subname,jf,'WARNING: CONSERV option not supported: '//TRIM(cconmet(ig_number_field(jf)))
                      WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                      WRITE (nulprt1,'(a)') ' error = ERROR in CONSERV option'
                      CALL prism_sys_flush(nulprt1)
                  ENDIF
                  CALL prism_sys_abort()
              endif

           elseif (canal(ja,ig_number_field(jf)) .EQ. 'CHECKIN' ) then
              namchecki(jf) = .true.

           elseif (canal(ja,ig_number_field(jf)) .EQ. 'CHECKOUT') then
              namchecko(jf) = .true.

           elseif (canal(ja,ig_number_field(jf)) .EQ. 'BLASOLD') then
              namfldsmu(jf) = afldcobo(ig_number_field(jf))
              do jc = 1, nbofld(ig_number_field(jf))
                 if (trim(cbofld(jc,ig_number_field(jf))) == 'CONSTANT') then
                    namfldsad(jf) = abocoef(jc,ig_number_field(jf))
                 else
                     IF (mpi_rank_global == 0) THEN
                         WRITE(nulprt1,*) subname,jf,'ERROR: BLASOLD only supports CONSTANTS: '//TRIM(cbofld(jc,ig_number_field(jf)))
                         WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                         WRITE (nulprt1,'(a)') ' error = ERROR in BLASOLD option'
                         CALL prism_sys_flush(nulprt1)
                     ENDIF
                    call prism_sys_abort()
                 endif
              enddo

           elseif (canal(ja,ig_number_field(jf)) .EQ. 'BLASNEW') then
              namflddmu(jf) = afldcobn(ig_number_field(jf))
              do jc = 1, nbnfld(ig_number_field(jf))
                 if (trim(cbnfld(jc,ig_number_field(jf))) == 'CONSTANT') then
                    namflddad(jf) = abncoef(jc,ig_number_field(jf))
                 else
                     IF (mpi_rank_global == 0) THEN
                         WRITE(nulprt1,*) subname,jf,'ERROR: BLASNEW only supports CONSTANTS: '//TRIM(cbofld(jc,ig_number_field(jf)))
                         WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                         WRITE (nulprt1,'(a)') ' error = ERROR in BLASNEW option'
                         CALL prism_sys_flush(nulprt1)
                     ENDIF
                    call prism_sys_abort()
                 endif
              enddo

           endif  ! canal
        enddo  ! ig_ntrans
     endif   ! ig_number_field
  enddo   ! ig_total_nfield

  IF (mpi_rank_global == 0) THEN
      WRITE(nulprt1,*) ' '
      DO n = 1,nnamcpl
        WRITE(nulprt1,*) subname,n,'namsrcfld ',TRIM(namsrcfld(n))
        WRITE(nulprt1,*) subname,n,'namdstfld ',TRIM(namdstfld(n))
        WRITE(nulprt1,*) subname,n,'namsrcgrd ',TRIM(namsrcgrd(n))
        WRITE(nulprt1,*) subname,n,'namsrc_nx ',namsrc_nx(n)
        WRITE(nulprt1,*) subname,n,'namsrc_ny ',namsrc_ny(n)
        WRITE(nulprt1,*) subname,n,'namdstgrd ',TRIM(namdstgrd(n))
        WRITE(nulprt1,*) subname,n,'namdst_nx ',namdst_nx(n)
        WRITE(nulprt1,*) subname,n,'namdst_ny ',namdst_ny(n)
        WRITE(nulprt1,*) subname,n,'namfldseq ',namfldseq(n)
        WRITE(nulprt1,*) subname,n,'namfldops ',namfldops(n)
        WRITE(nulprt1,*) subname,n,'namfldtrn ',namfldtrn(n)
        WRITE(nulprt1,*) subname,n,'namfldcon ',namfldcon(n)
        WRITE(nulprt1,*) subname,n,'namflddti ',namflddti(n)
        WRITE(nulprt1,*) subname,n,'namfldlag ',namfldlag(n)
        WRITE(nulprt1,*) subname,n,'nammapfil ',TRIM(nammapfil(n))
        WRITE(nulprt1,*) subname,n,'nammaploc ',TRIM(nammaploc(n))
        WRITE(nulprt1,*) subname,n,'nammapopt ',TRIM(nammapopt(n))
        WRITE(nulprt1,*) subname,n,'namrstfil ',TRIM(namrstfil(n))
        WRITE(nulprt1,*) subname,n,'naminpfil ',TRIM(naminpfil(n))
        WRITE(nulprt1,*) subname,n,'namchecki ',namchecki(n)
        WRITE(nulprt1,*) subname,n,'namchecko ',namchecko(n)
        WRITE(nulprt1,*) subname,n,'namfldsmu ',namfldsmu(n)
        WRITE(nulprt1,*) subname,n,'namfldsad ',namfldsad(n)
        WRITE(nulprt1,*) subname,n,'namflddmu ',namflddmu(n)
        WRITE(nulprt1,*) subname,n,'namflddad ',namflddad(n)
        WRITE(nulprt1,*) subname,n,'namscrmet ',TRIM(namscrmet(n))
        WRITE(nulprt1,*) subname,n,'namscrnor ',TRIM(namscrnor(n))
        WRITE(nulprt1,*) subname,n,'namscrtyp ',TRIM(namscrtyp(n))
        WRITE(nulprt1,*) subname,n,'namscrord ',TRIM(namscrord(n))
        WRITE(nulprt1,*) subname,n,'namscrres ',TRIM(namscrres(n))
        WRITE(nulprt1,*) subname,n,'namscrvam ',namscrvam(n)
        WRITE(nulprt1,*) subname,n,'namscrnbr ',namscrnbr(n)
        WRITE(nulprt1,*) subname,n,'namscrbin ',namscrbin(n)
        WRITE(nulprt1,*) ' '
      ENDDO
  ENDIF

  !--- compute seq sort ---
  namfldsort(:) = -1
  do nv = 1,nnamcpl
     loc = nv    ! default at end
     n1 = 1
     do while (loc == nv .and. n1 < nv)
       if (namfldseq(nv) < namfldseq(namfldsort(n1))) loc = n1
       n1 = n1 + 1
     enddo
     ! nv goes into loc location, shift then set
     do n1 = nv,loc+1,-1
        namfldsort(n1) = namfldsort(n1-1)
     enddo
     namfldsort(loc) = nv
  enddo

  IF (mpi_rank_global == 0) THEN
      DO nv = 1,nnamcpl
        n1 = namfldsort(nv)
        WRITE(nulprt1,*) subname,' sort ',nv,n1,namfldseq(n1)
      ENDDO
  ENDIF


  !--- check they are sorted ---
  do n = 2,nnamcpl
     if (namfldseq(namfldsort(n)) < namfldseq(namfldsort(n-1))) then
         IF (mpi_rank_global == 0) THEN
             WRITE(nulprt1,*) subname,' ERROR in seq sort'
             WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
             WRITE (nulprt1,'(a)') ' error = ERROR in seq sort'
             CALL prism_sys_flush(nulprt1)
         ENDIF
        call prism_sys_abort()
     endif
  enddo

  call dealloc()

!  call prism_sys_debug_exit(subname)

  END SUBROUTINE prism_namcouple_init

!===============================================================================

      SUBROUTINE inipar_alloc()
!****
!               *****************************
!               * OASIS ROUTINE  -  LEVEL 0 *
!               * -------------     ------- *
!               *****************************

!**** *inipar_alloc*  - Get main run parameters to allocate arrays 

!     Purpose:
!     -------
!     Reads out run parameters.

!**   Interface:
!     ---------
!       *CALL*  *inipar_alloc*

!     Input:
!     -----
!     None

!     Output:
!     ------
!     None

!     Workspace:
!     ---------
!     None

!     Externals:
!     ---------
!     parse

!     Reference:
!     ---------
!     See OASIS manual (1995) 

!     History:
!     -------
!     Version Programmer  Date      Description
!     ------- ----------  ----      -----------  
!       2.5   A. Caubel   02/04/02  created from inipar.f

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  IMPLICIT NONE

!* ---------------------------- Local declarations --------------------

      CHARACTER*80 clline, clline_aux, clvari
      CHARACTER*9 clword, clfield, clstring, clmod, clchan
      CHARACTER*3 clind
      CHARACTER*2 cldeb
      CHARACTER*1 clequa
      CHARACTER*8 clwork
      CHARACTER*8 clstrg
      CHARACTER*7 cl_bsend

      CHARACTER(len=8), DIMENSION(:), ALLOCATABLE :: cl_aux
      INTEGER (kind=ip_intwp_p) il_varid, il_len, il_err, il_maxanal 
      INTEGER (kind=ip_intwp_p) nlonbf_notnc, nlatbf_notnc,  &
          nlonaf_notnc, nlataf_notnc
      INTEGER (kind=ip_intwp_p) iind, il_redu, ib, il_aux, il_auxbf, &
           il_auxaf, istatus, il_id
      integer (kind=ip_intwp_p) :: ja,jz,jm,jf,ilen
      integer (kind=ip_intwp_p) :: ig_clim_maxport
      logical :: lg_bsend,endflag
      character(len=*),parameter :: subname='mod_prism_namcouple:inipar_alloc'

!* ---------------------------- Poema verses --------------------------

!  call prism_sys_debug_enter(subname)

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!*    1. Get basic info for the simulation 
!        ---------------------------------

  IF (mpi_rank_global == 0) THEN
      WRITE (UNIT = nulprt1,FMT = *)' '
      WRITE (UNIT = nulprt1,FMT = *)'  ROUTINE inipar_alloc - Level 0'
      WRITE (UNIT = nulprt1,FMT = *)'  ********************   *******'
      WRITE (UNIT = nulprt1,FMT = *)' '
      WRITE (UNIT = nulprt1,FMT = *)'  Initialization of run parameters'
      WRITE (UNIT = nulprt1,FMT = *)' '
      WRITE (UNIT = nulprt1,FMT = *)'  Reading input file namcouple'
      WRITE (UNIT = nulprt1,FMT = *)' '
      WRITE (UNIT = nulprt1,FMT = *)' '
      CALL FLUSH(nulprt1)
  ENDIF

!* Initialization
      cchan = '    '
      ig_direct_nfield = 0
      ig_nfield = 0
      lg_oasis_field = .true.
      lg_vector = .FALSE.
!* Initialize character keywords to locate appropriate input

      clfield  = ' $NFIELDS'
      clchan   = ' $CHANNEL'
      clstring = ' $STRINGS'
      clmod    = ' $NBMODEL'

!* Get number of models involved in this simulation

      REWIND nulin
 100  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 110) clword
      IF (clword .NE. clmod) GO TO 100
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Nothing on input for $NBMODEL '
              WRITE (UNIT = nulprt1,FMT = *) ' Default value will be used '
              WRITE (UNIT = nulprt1,FMT = *) ' '
          ENDIF
      ELSE IF (ilen .GT. 1) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Input variable length is incorrect'
              WRITE (UNIT = nulprt1,FMT = *) ' There are too many models '
              WRITE (UNIT = nulprt1,FMT = *) ' ilen = ', ILEN  
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Check $NBMODEL variable spelling '
              WRITE (UNIT = nulprt1,FMT = *) ' Default value will be used '
          ENDIF
      ELSE
          READ (clvari,FMT = 1003) ig_nmodel
      ENDIF

!* Print out the number of models

      CALL prtout &
          ('The number of models for this run is nmodel =', ig_nmodel, &
          1)
!tcx inline
!      CALL alloc_experiment

       ALLOCATE (nbcplproc(ig_nmodel), stat=il_err)
       IF (il_err.NE.0) CALL prtout & 
          ('Error in "nbcplproc"allocation of experiment module',il_err,1)
       nbcplproc(:)=0
       ALLOCATE (nbtotproc(ig_nmodel), stat=il_err)
       IF (il_err.NE.0) CALL prtout & 
          ('Error in "nbtotproc"allocation of experiment module',il_err,1)
       nbtotproc(:)=0
       ALLOCATE (cmodnam(ig_nmodel), stat=il_err)
       IF (il_err.NE.0) CALL prtout & 
          ('Error in "cmodnam"allocation of experiment module',il_err,1)
       cmodnam(:)=' '
       ALLOCATE (cmpiarg(ig_nmodel), stat=il_err)
       IF (il_err.NE.0) CALL prtout & 
          ('Error in "cmpiarg"allocation of experiment module',il_err,1)
       cmpiarg(:)=' '
       ALLOCATE (iga_unitmod(ig_nmodel), stat=il_err)
       IF (il_err.NE.0) CALL prtout & 
          ('Error in iga_unitmod allocation of experiment module',il_err,1)
       iga_unitmod(:)=0

! --> Get the message passing technique we are using

      REWIND nulin
 120  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 130) clword
      IF (clword .NE. clchan) GO TO 120
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Nothing on input for $CHANNEL '
              WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
              WRITE (nulprt1,'(a)') ' error = STOP in inipar'
              CALL prism_sys_flush(nulprt1)
          ENDIF
          CALL PRISM_SYS_ABORT()
      ELSE IF (ilen .GT. 0 .AND. ilen .NE. 4) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Input variable length is incorrect'
              WRITE (UNIT = nulprt1,FMT = *) ' ilen = ', ILEN  
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Check $CHANNEL variable spelling '
              WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
              WRITE (nulprt1,'(a)') ' error = STOP in inipar'
              CALL prism_sys_flush(nulprt1)
          ENDIF
          CALL PRISM_SYS_ABORT()
      ELSE
          cchan = clvari(1:4)

          IF (cchan .EQ.'MPI1' .OR. cchan .EQ. 'MPI2' &
              .or. cchan .EQ. 'GSIP') THEN
              CALL parse (clline, clvari, 2, jpeighty, ilen)
#if defined use_comm_MPI1 || defined use_comm_MPI2 || defined use_comm_GSIP || (!defined use_comm_MPI1 && !defined use_comm_MPI2 && !defined use_comm_GSIP && !defined use_comm_SIPC && !defined use_comm_GMEM && !defined use_comm_PIPE && !defined use_comm_NONE)
              IF (ilen .eq. 0) THEN
                  lg_bsend = .true.
              ELSE
                  cl_bsend = clvari(1:7)
                  IF (cl_bsend .eq. 'NOBSEND') THEN
                      lg_bsend = .false.
                  ELSE
                      lg_bsend = .true.
                  ENDIF
              ENDIF
#endif
!* Get one additional line for each model
              DO 186 jm = 1, ig_nmodel
                READ (UNIT = nulin,FMT = 1002) clline
!*              Get the total number of processors for the model
                CALL parse (clline, clvari, 1, jpeighty, ilen)
                READ (clvari,FMT = 1004) nbtotproc(jm)
                IF (mpi_rank_global == 0) THEN
                    WRITE (UNIT = nulprt1,FMT = *) ' '
                    WRITE (UNIT=nulprt1,FMT='(''The total number of proc'', &
                       '' for model'', I4, '' is'', I4)') &
                       jm, nbtotproc(jm)
                    WRITE (UNIT = nulprt1,FMT = *) ' '
                ENDIF

!*              Get the nbr of processors involved in the coupling for the model
                CALL parse (clline, clvari, 2, jpeighty, ilen)
                IF (ilen .LE. 0) THEN
                    IF (mpi_rank_global == 0) THEN
                        WRITE (UNIT = nulprt1,FMT = *) '      ***WARNING***'
                        WRITE (UNIT = nulprt1,FMT = *)  &
                           'No input for number of processors involved in the coupling'
                        WRITE (UNIT = nulprt1,FMT = *) 'for model', jm
                        WRITE (UNIT = nulprt1,FMT = *) &
                           'Total number of processors will be used'
                    ENDIF
                    nbcplproc(jm)=nbtotproc(jm)
                ELSE
                    READ (clvari,FMT = 1004) nbcplproc(jm)
                ENDIF
                IF (mpi_rank_global == 0) THEN
                    WRITE (UNIT = nulprt1,FMT = *) ' '
                    WRITE (UNIT = nulprt1,FMT ='(''The number of processors'', &
                       '' involved in the coupling for model'', I8, '' is'',i8)')  &
                          jm, nbcplproc(jm)
                    WRITE (UNIT = nulprt1,FMT = *) ' '
                ENDIF

 186          CONTINUE
     
          ELSE IF(cchan .EQ. 'PVM3') THEN
              IF (mpi_rank_global == 0) THEN
                  WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
                  WRITE (UNIT = nulprt1,FMT = *)  &
                     ' PVM3 no longer supported'
                  WRITE (UNIT = nulprt1,FMT = *)  &
                     ' Please keep on using Oasis 2.4'
                  WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                  WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                  CALL prism_sys_flush(nulprt1)
              ENDIF
              CALL PRISM_SYS_ABORT()
          ENDIF

!* Print out the message passing technique

          CALL prcout &
             (' The message passing used in OASIS is cchan =', cchan, 1)
      ENDIF

!* Formats

 1001 FORMAT(A9)
 1002 FORMAT(A80)
 1003 FORMAT(I1)
 1004 FORMAT(I4)


!*    2. Get field information
!        ---------------------

!* Read total number of fields exchanged by this OASIS process

      REWIND nulin
 200  CONTINUE
      READ (UNIT = nulin,FMT = 2001,END = 210) clword
      IF (clword .NE. clfield) GO TO 200
      READ (UNIT = nulin,FMT = 2002) clline
      CALL parse(clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Nothing on input for $NFIELDS '
              WRITE (UNIT = nulprt1,FMT = *) ' Default value will be used '
              WRITE (UNIT = nulprt1,FMT = *) ' '
          ENDIF
      ELSE
          READ (clvari,FMT = 2003) ig_total_nfield
      ENDIF

!* Print out the total number of fields exchanged by this OASIS process

      CALL prtout &
          ('The number of exchanged fields is nfield =',  &
           ig_total_nfield, 1)

!* Read ig_clim_maxport which should be the maximum number of 
!* prism_def_var_proto called by one component in the coupled model
!* Note: ig_clim_maxport is optional if it is smaller than twice 
!*       the number of fields exchanged by this OASIS process

      CALL parse(clline, clvari, 2, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' No ig_clim_maxport in namcouple'
              WRITE (UNIT = nulprt1,FMT = *) ' '
          ENDIF
          ig_clim_maxport = 0
      ELSE
          READ (clvari,FMT = 2003) ig_clim_maxport

!* Print out the maximum number of fields

          CALL prtout &
              ('The namcouple value of ig_clim_maxport is =',  &
              ig_clim_maxport, 1)
      ENDIF

      IF (ig_clim_maxport .le. 2*ig_total_nfield) &
          ig_clim_maxport = 2*ig_total_nfield

      CALL prtout &
              ('The used value of ig_clim_maxport is =',  &
              ig_clim_maxport, 1)      

!* Alloc field number array 

      ALLOCATE (ig_number_field(ig_total_nfield),stat=il_err)
      IF (il_err.NE.0) CALL prtout  &
          ('Error: ig_number_field allocation of inipar_alloc',il_err,1)
      ig_number_field(:)=0

!* Alloc field status array (logical indicating if the field goes through 
!* Oasis or not)

      ALLOCATE (lg_state(ig_total_nfield), stat=il_err)
      IF (il_err.NE.0) CALL prtout  &
          ('Error: lg_state allocation of inipar_alloc',il_err,1)
      lg_state(:)=.false.

!* Alloc status of all the fields
 
      ALLOCATE (ig_total_state(ig_total_nfield), stat=il_err)
      IF (il_err.NE.0) CALL prtout  &
          ('Error: ig_total_state allocation of inipar_alloc',il_err,1)
      ig_total_state(:)=0

!* Alloc input field name array 

      ALLOCATE (cg_output_field(ig_total_nfield), stat=il_err)
      IF (il_err.NE.0) CALL prtout  &
          ('Error: cg_output_field allocation of inipar_alloc',il_err,1)
      cg_output_field(:)=' ' 

!* Alloc number of analyses array 

      ALLOCATE (ig_total_ntrans(ig_total_nfield),stat=il_err)
      IF (il_err.NE.0) CALL prtout  &
          ('Error: ig_total_ntrans"allocation of inipar_alloc',il_err,1)
      ig_total_ntrans (:) = 0
 
!* Alloc array of restart file names, input and output file names

      ALLOCATE (cg_restart_file(ig_total_nfield),stat=il_err)
      IF (il_err.NE.0) CALL prtout  &
        ('Error: cg_restart_FILE allocation of inipar_alloc',il_err,1)
      cg_restart_file(:)=' '
      ALLOCATE (cg_input_file(ig_total_nfield), stat=il_err)
      IF (il_err.NE.0) CALL prtout  &
       ('Error in "cg_input_file"allocation of inipar_alloc',il_err,1)
      cg_input_file(:)=' '
 
!* Alloc array of source and target locator prefix

      ALLOCATE (cga_locatorbf(ig_total_nfield),stat=il_err)
      IF (il_err.NE.0) CALL prtout  &
        ('Error: cga_locatorbf allocation of inipar_alloc',il_err,1)
      cga_locatorbf(:)=' '

      ALLOCATE (cga_locatoraf(ig_total_nfield),stat=il_err)
      IF (il_err.NE.0) CALL prtout  &
        ('Error: cga_locatoraf allocation of inipar_alloc',il_err,1)
      cga_locatoraf(:)=' '  

!* Get the SSCS for all fields

      REWIND nulin
 220  CONTINUE
      READ (UNIT = nulin,FMT = 2001,END = 230) clword
      IF (clword .NE. clstring) GO TO 220

!* Loop on total number of fields (NoF)

      DO 240 jf = 1, ig_total_nfield

!* First line

        READ (UNIT = nulin,FMT = 2002, END=241) clline
!* Get output field symbolic name
        CALL parse(clline, clvari, 2, jpeighty, ilen)
        cg_output_field(jf) = clvari
!* Get total number of analysis
        CALL parse(clline, clvari, 5, jpeighty, ilen)
        READ (clvari,FMT = 2003) ig_total_ntrans(jf)
!* Get field STATUS for OUTPUT fields
        CALL parse(clline, clvari, 6, jpeighty, ilen)
        IF (clvari(1:6) .eq. 'OUTPUT') THEN
           ig_direct_nfield = ig_direct_nfield + 1
           lg_state(jf) = .false.
           ig_total_state(jf) = ip_output
        ELSE
!* Get field status (direct or through oasis) and the number  
!* of direct and indirect fields if not PIPE nor NONE
            IF (cchan .NE. 'PIPE' .AND. cchan .NE. 'NONE') THEN
                CALL parse(clline, clvari, 7, jpeighty, ilen)
                IF (clvari(1:8).eq.'EXPORTED') THEN
                    ig_nfield = ig_nfield + 1
                    lg_state(jf) = .true.
                    ig_number_field(jf) = ig_nfield
                    ig_total_state(jf) = ip_exported
                    CALL parse(clline, clvari, 6, jpeighty, ilen)
!* Get restart file name               
                    cg_restart_file(jf) = clvari
!* Get restart file name
                ELSEIF (clvari(1:6) .eq. 'OUTPUT' ) THEN
                    ig_direct_nfield = ig_direct_nfield + 1
                    lg_state(jf) = .false.
                    ig_total_state(jf) = ip_output
                    CALL parse(clline, clvari, 6, jpeighty, ilen)
                    cg_restart_file(jf) = clvari
                ELSEIF (clvari(1:7) .eq. 'IGNORED' ) THEN
                    IF (cchan .eq. 'GSIP') THEN
                        IF (mpi_rank_global == 0) THEN
                            WRITE (UNIT = nulprt1,FMT = *) &
                               'Direct exchange of fields between models'
                            WRITE (UNIT = nulprt1,FMT = *) &
                               'not supported with GSIP'
                            WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                            WRITE (nulprt1,'(a)') ' error = STOP in inipar_alloc (GSIP)'
                            CALL prism_sys_flush(nulprt1)
                        ENDIF
                        CALL PRISM_SYS_ABORT()
                    ENDIF
                    ig_direct_nfield = ig_direct_nfield + 1
                    lg_state(jf) = .false.
                    ig_total_state(jf) = ip_ignored
                    CALL parse(clline, clvari, 6, jpeighty, ilen)
!* Get restart file name
                    cg_restart_file(jf) = clvari
                ELSEIF (clvari(1:6) .eq. 'EXPOUT') THEN
                    ig_nfield = ig_nfield + 1
                    lg_state(jf) = .true.
                    ig_number_field(jf) = ig_nfield
                    ig_total_state(jf) = ip_expout
                    CALL parse(clline, clvari, 6, jpeighty, ilen)
!* Get restart file name               
                    cg_restart_file(jf) = clvari
                ELSEIF (clvari(1:6) .eq. 'IGNOUT' ) THEN
                    ig_direct_nfield = ig_direct_nfield + 1
                    lg_state(jf) = .false.
                    ig_total_state(jf) = ip_ignout
                    CALL parse(clline, clvari, 6, jpeighty, ilen)
!* Get restart file name 
                    cg_restart_file(jf) = clvari
                ELSEIF (clvari(1:9).eq. 'AUXILARY') THEN  
                    ig_nfield = ig_nfield + 1
                    lg_state(jf) = .true.
                    ig_number_field(jf) = ig_nfield
                    ig_total_state(jf) = ip_auxilary
                    CALL parse(clline, clvari, 6, jpeighty, ilen)
!* Get restart file name
                    cg_restart_file(jf) = clvari
                ELSEIF (clvari(1:5) .eq. 'INPUT') THEN
                    ig_direct_nfield = ig_direct_nfield + 1
                    lg_state(jf) = .false.
                    ig_total_state(jf) = ip_input
                    CALL parse(clline, clvari, 6, jpeighty, ilen)
!* Get input file name
                    cg_input_file(jf) = clvari
                ENDIF
            ELSE
!*          Get field status if PIPE or NONE
              CALL parse(clline, clvari, 8, jpeighty, ilen)
              IF (clvari .NE. 'EXPORTED' .AND. clvari .NE. 'AUXILARY') THEN
                 CALL prtout  &
                      ('Error in namcouple for status of field',jf,1)
                 IF (mpi_rank_global == 0) THEN
                     WRITE (UNIT = nulprt1,FMT = *)  &
                        '==> Must be EXPORTED or AUXILARY'
                 ENDIF
                 IF (clvari(1:7) .EQ. 'IGNORED')  &
                    THEN
                     IF (mpi_rank_global == 0) THEN
                         WRITE (UNIT = nulprt1,FMT = *) &
                            'Direct communication is only for CLIM/MPI1 or MPI2'
                         WRITE (UNIT = nulprt1,FMT = *)  &
                            'Maybe you forgot the output FILE name which'
                         WRITE (UNIT = nulprt1,FMT = *)  &
                            'is mandatory for PIPE or NONE techniques'
                         WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                         WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                         CALL prism_sys_flush(nulprt1)
                     ENDIF
                 CALL PRISM_SYS_ABORT() 
                 ENDIF
              ELSE IF (clvari .eq. 'EXPORTED') THEN
                 ig_nfield = ig_nfield + 1
                 lg_state(jf) = .true.
                 ig_number_field(jf) = ig_nfield
                 ig_total_state(jf) = ip_exported
!* Get restart file name
                 CALL parse(clline, clvari, 6, jpeighty, ilen)
                 cg_restart_file(jf) = clvari
              ELSE IF (clvari .eq. 'AUXILARY') THEN
                 ig_nfield = ig_nfield + 1
                 lg_state(jf) = .true.
                 ig_number_field(jf) = ig_nfield
                 ig_total_state(jf) = ip_auxilary
!* Get restart file name
                 CALL parse(clline, clvari, 6, jpeighty, ilen)
                 cg_restart_file(jf) = clvari
             ENDIF
         ENDIF
     ENDIF
      IF (lg_state(jf)) THEN
           IF (ig_total_ntrans(jf) .eq. 0) THEN
               IF (mpi_rank_global == 0) THEN
                   WRITE (UNIT = nulprt1,FMT = *) &
                      'If there is no analysis for the field',jf, &
                      'then the status must not be "EXPORTED"' 
                   WRITE (UNIT = nulprt1,FMT = *)' "AUXILARY" or "EXPOUT" '
                   WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                   WRITE (nulprt1,'(a)') ' error = STOP in inipar_alloc'
                   CALL prism_sys_flush(nulprt1)
               ENDIF
              CALL PRISM_SYS_ABORT() 
           ENDIF
           READ (UNIT = nulin,FMT = 2002) clline
           CALL skip(clline, jpeighty)
           READ (UNIT = nulin,FMT = 2002) clline
           CALL skip(clline, jpeighty)
           READ (UNIT = nulin,FMT = 2002)clline_aux
           DO ja=1,ig_total_ntrans(jf)
              CALL parse(clline_aux, clvari, ja, jpeighty, ilen)
              IF (clvari.eq.'CORRECT'.or.clvari.eq.'BLASOLD'.or. &
                   clvari.eq.'BLASNEW') THEN
                 READ (UNIT = nulin,FMT = 2002) clline
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
                 READ(clvari,FMT = 2003) il_aux
                 DO ib = 1, il_aux
                    READ (UNIT = nulin,FMT = 2002) clline
                    CALL skip(clline, jpeighty)
                  ENDDO
              ELSE IF (clvari.eq.'NOINTERP') THEN
                  CONTINUE
              ELSE
                  READ (UNIT = nulin,FMT = 2002) clline
                  CALL skip(clline, jpeighty)
              ENDIF
            ENDDO
        ELSE
           IF (ig_total_state(jf) .ne. ip_input) THEN
              READ (UNIT = nulin,FMT = 2002) clline
              CALL skip(clline, jpeighty)
           ENDIF
           IF (ig_total_state(jf) .ne. ip_input .and.  &
                ig_total_ntrans(jf) .gt. 0 ) THEN
              READ (UNIT = nulin,FMT = 2002) clline
              CALL parse(clline, clvari, 1, jpeighty, ilen)
              IF (clvari(1:8) .ne. 'LOCTRANS') THEN
                  IF (mpi_rank_global == 0) THEN
                      WRITE (UNIT = nulprt1,FMT = *) &
                         'You want a transformation which is not available !'
                      WRITE (UNIT = nulprt1,FMT = *) &
                         'Only local transformations are available for '
                      WRITE (UNIT = nulprt1,FMT = *) &
                         'fields exchanged directly or output fields '
                      WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                      WRITE (nulprt1,'(a)') ' error = STOP in inipar_alloc'
                      CALL prism_sys_flush(nulprt1)
                  ENDIF
                 CALL PRISM_SYS_ABORT() 
              ENDIF
              DO ja=1,ig_total_ntrans(jf)
                 READ (UNIT = nulin,FMT = 2002) clline
                 CALL skip(clline, jpeighty)
              ENDDO
           ENDIF 
       ENDIF
        
 240    CONTINUE
!* Verify we're at the end of the namcouple, if not STOP (tcraig, june 2012)
 243    READ (UNIT = nulin,FMT = 2002, END=242) clline
        CALL skip(clline, jpeighty,endflag)
        if (endflag == .true.) goto 242
        CALL parse(clline, clvari, 1, jpeighty, ilen)
        IF (trim(clvari) .eq. "$END") goto 243
        goto 241       
 242    CONTINUE
        IF (ig_nfield.eq.0) THEN
            lg_oasis_field = .false.
            IF (mpi_rank_global == 0) THEN
                WRITE (nulprt1,*)'==> All the fields are exchanged directly'
            ENDIF
        ENDIF
        

!* Number of different restart files

        allocate (cl_aux(ig_total_nfield))
        cl_aux(:)=' '
        DO jf = 1,ig_total_nfield
          IF (jf.eq.1) THEN
              cl_aux(1) = cg_restart_file(1)
              il_aux = 1
          ELSEIF (jf.gt.1) THEN
              IF (ALL(cl_aux.ne.cg_restart_file(jf))) THEN
                  il_aux = il_aux + 1  
                  cl_aux(il_aux) = cg_restart_file(jf)
              ENDIF
          ENDIF
        ENDDO
        deallocate(cl_aux)
        ig_nbr_rstfile = il_aux

        IF (lg_oasis_field) THEN
 
!*      Alloc array needed for INTERP and initialize them

            ALLOCATE (cintmet(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: cintmet allocation of inipar_alloc',il_err,1)
            ALLOCATE (naismfl(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: naismfl allocation of inipar_alloc',il_err,1)
            ALLOCATE (naismvoi(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: naismvoi allocation of inipar_alloc',il_err,1)
            ALLOCATE (naisgfl(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: naisgfl allocation of inipar_alloc',il_err,1)
            ALLOCATE (naisgvoi(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: naisgvoi allocation of inipar_alloc',il_err,1)
            cintmet(:)=' '
            naismfl(:) = 1
            naismvoi(:) = 1
            naisgfl(:) = 1
            naisgvoi(:) = 1
!     
!*          Alloc arrays needed for EXTRAP and initialize them
!     
            ALLOCATE (cextmet(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: cextmet allocation of inipar_alloc',il_err,1)
            ALLOCATE (nninnfl(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout &
                ('Error: nninnfl allocation of inipar_alloc',il_err,1)
            ALLOCATE (nninnflg(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout &
                ('Error: nninnflg allocation of inipar_alloc',il_err,1)
            ALLOCATE (neighbor(ig_nfield), stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: neighbor allocation of inipar_alloc',il_err,1)
            ALLOCATE (nextfl(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: nextfl allocation of inipar_alloc',il_err,1)
            cextmet(:)=' '
            nninnfl(:) = 1
            nninnflg(:) = 1
            neighbor(:) = 1
            nextfl(:) = 1
!     
!*          Alloc arrays needed for BLAS... analyses and initialize them 
!     
            ALLOCATE (nbofld(ig_nfield), stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: nbofld allocation of inipar_alloc',il_err,1)
            ALLOCATE (nbnfld(ig_nfield), stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: nbnfld allocation of inipar_alloc',il_err,1)
            nbofld(:) = 1
            nbnfld(:) = 1
!     
!*          Alloc arrays needed for MOZAIC and initialize them
!     
            ALLOCATE (nmapvoi(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: nmapvoi allocation of  inipar_alloc',il_err,1)
            ALLOCATE (nmapfl(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: nmapfl allocation of inipar_alloc',il_err,1)
            nmapvoi(:) = 1
            nmapfl(:) = 1
!     
!*          Alloc arrays needed for SUBGRID and initialize them
!     
            ALLOCATE (nsubfl(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: nsubfl allocation of inipar_alloc',il_err,1)
            ALLOCATE (nsubvoi(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: nsubvoi allocation of inipar_alloc',il_err,1)
            nsubfl(:) = 1
            nsubvoi(:) = 1
!     
!*          Alloc arrays needed for GLORED and REDGLO and initialize them 
!     
            ALLOCATE (ntronca(ig_nfield), stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: ntronca allocation of inipar_alloc',il_err,1)
            ntronca(:) = 0

!     
!*          Alloc array needed for analyses parameters 
!     
            ALLOCATE (cficbf(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: cficbf allocation of inipar_alloc',il_err,1)
            cficbf(:)=' '
            ALLOCATE (cficaf(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: cficaf allocation of inipar_alloc',il_err,1)
            cficaf(:)=' '
!     
!*         Alloc arrays needed for grid dimensions of direct fields and 
!*         indirect fields
!     
            ALLOCATE (nlonbf(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: nlonbf allocation of inipar_alloc',il_err,1)
            nlonbf(:)=0
            ALLOCATE (nlatbf(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: nlatbf allocation of inipar_alloc',il_err,1)
            nlatbf(:)=0
            ALLOCATE (nlonaf(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: nlonaf allocation of inipar_alloc',il_err,1)
            nlonaf(:)=0
            ALLOCATE (nlataf(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: nlataf allocation of inipar_alloc',il_err,1)
            nlataf(:)=0
!     
!*         Alloc arrays needed for grid number associated to each field
 
            ALLOCATE (ig_grid_nbrbf(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
            ('Error: ig_grid_nbrbf allocation of inipar_alloc',il_err,1)
            ig_grid_nbrbf(:)=0
            ALLOCATE (ig_grid_nbraf(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
            ('Error: ig_grid_nbraf allocation of inipar_alloc',il_err,1)
            ig_grid_nbraf(:)=0
            
!     
!*          Alloc number of analyses array 
!     
            ALLOCATE (ig_ntrans(ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: ig_ntrans allocation of inipar_alloc',il_err,1)
            ig_ntrans(:)=0
            DO ib = 1, ig_total_nfield
              IF (lg_state(ib)) &
                  ig_ntrans(ig_number_field(ib))=ig_total_ntrans(ib)
            ENDDO
!     
!*          Maximum number of analyses 
!     
            il_maxanal = maxval(ig_ntrans)
!     
!*          Alloc array of restart file names
!     
            ALLOCATE (cficinp(ig_nfield), stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: cficinp allocation of inipar_alloc',il_err,1)
            cficinp(:)=' '
            DO ib = 1, ig_total_nfield
              IF (lg_state(ib))  &
                  cficinp(ig_number_field(ib))=cg_restart_file(ib)
            END DO
#ifdef use_netCDF
!tcx?
!            istatus=NF_OPEN(cg_restart_file(1), NF_NOWRITE, il_id)
!            IF (istatus .eq. NF_NOERR) THEN
!                lncdfrst = .true.
!            ELSE
#endif
                lncdfrst = .false.
#ifdef use_netCDF
!            ENDIF
!            istatus=NF_CLOSE(il_id)
#endif
                IF (mpi_rank_global == 0) THEN
                    WRITE(nulprt1, *) 'lncdfrst =', lncdfrst
                ENDIF
!     
!*          Alloc array needed to get analysis names
 
            ALLOCATE (canal(il_maxanal,ig_nfield),stat=il_err)
            IF (il_err.NE.0) CALL prtout  &
                ('Error: canal allocation of inipar_alloc',il_err,1)
            canal(:,:)=' '
        ENDIF

!*      Get analysis parameters 

        REWIND nulin
 221    CONTINUE
        READ (UNIT = nulin,FMT = 2001,END = 230) clword
        IF (clword .NE. clstring) GO TO 221

!*      Loop on total number of fields (NoF)
!      
        DO 250 jf=1,ig_total_nfield

!*        Initialization

          nlonbf_notnc = 0
          nlatbf_notnc = 0
          nlonaf_notnc = 0
          nlataf_notnc = 0

!*        Skip first line read before

          READ (UNIT = nulin,FMT = 2002) clline
          CALL skip(clline, jpeighty)
!     
!* Second line

!* In the indirect case, reading of second, third, fourth line and analyses 
!* lines

          IF (ig_total_state(jf) .ne. ip_input) THEN
              READ (UNIT = nulin,FMT = 2002) clline
!*            First determine what information is on the line
              CALL parse(clline, clvari, 3, jpeighty, ilen)
              IF (ilen .lt. 0) THEN
!*                  
!*                IF only two words on the line, then they are the locator 
!*                prefixes and the grids file must be in NetCDF format       
                  CALL parse(clline, clvari, 1, jpeighty, ilen)
                  IF (lg_state(jf))  &
                      cficbf(ig_number_field(jf)) = clvari
                  cga_locatorbf(jf) = clvari(1:4)
                  CALL parse(clline, clvari, 2, jpeighty, ilen)
                  IF (lg_state(jf)) &
                      cficaf(ig_number_field(jf)) = clvari
                  cga_locatoraf(jf) = clvari(1:4)
                  lncdfgrd = .true.
              ELSE 
                  READ(clvari,FMT = 2010) clind, clequa, iind
                  IF (clind .EQ. 'SEQ' .or. clind .eq. 'LAG' .and. &!50
                      clequa .EQ. '=') THEN
                      
!*                    If 3rd word is an index, then first two words are 
!*                    locator prefixes and grids file must be NetCDF format
                      CALL parse(clline, clvari, 1, jpeighty, ilen) 
                      IF (lg_state(jf))  &
                          cficbf(ig_number_field(jf)) = clvari 
                      cga_locatorbf(jf) = clvari(1:4)
                      CALL parse(clline, clvari, 2, jpeighty, ilen)
                      IF (lg_state(jf))  &
                          cficaf(ig_number_field(jf)) = clvari
                      cga_locatoraf(jf) = clvari(1:4)
                      lncdfgrd = .true.
                  ELSE
!*              If not, the first 4 words are grid dimensions and next
!*              2 words are locator prefixes, and grids file may be or
!*              not in NetCDF format 
                      CALL parse(clline, clvari, 1, jpeighty, ilen)
!*                    Get number of longitudes for initial field
                      READ(clvari,FMT = 2004) nlonbf_notnc
                      CALL parse(clline, clvari, 2, jpeighty, ilen)
!*                    Get number of latitudes for initial field
                      READ(clvari,FMT = 2004) nlatbf_notnc
                      CALL parse(clline, clvari, 3, jpeighty, ilen)
!*                    Get number of longitudes for final field
                      READ(clvari,FMT = 2004) nlonaf_notnc
                      CALL parse(clline, clvari, 4, jpeighty, ilen)
!*                    Get number of latitudes for final field
                      READ(clvari,FMT = 2004) nlataf_notnc
                      CALL parse(clline, clvari, 5, jpeighty, ilen)
!*                    Get root name grid-related files (initial field)
                      IF (lg_state(jf))  &
                          cficbf(ig_number_field(jf)) = clvari
                      cga_locatorbf(jf) = clvari(1:4)
                      CALL parse(clline, clvari, 6, jpeighty, ilen)
!*                    Get root name for grid-related files (final field)
                      IF (lg_state(jf))  &
                          cficaf(ig_number_field(jf)) = clvari
                      cga_locatoraf(jf) = clvari(1:4)
                      nlonbf(ig_number_field(jf)) = nlonbf_notnc
                      nlatbf(ig_number_field(jf)) = nlatbf_notnc
                      nlonaf(ig_number_field(jf)) = nlonaf_notnc
                      nlataf(ig_number_field(jf)) = nlataf_notnc

                   ENDIF
               ENDIF

!*           Read the P 2 P 0 line for exported, expout or auxilary

             IF (lg_state(jf)) THEN
              READ (UNIT = nulin,FMT = 2002) clline
              CALL skip(clline, jpeighty)
             ENDIF
!     
!*            Read next line of strings
!             --->>> Stuff related to field transformation

             IF (ig_total_ntrans(jf) .gt. 0) then 
              READ (UNIT = nulin,FMT = 2002) clline
              CALL skip(clline, jpeighty)
              DO 260 ja = 1, ig_total_ntrans(jf)
                CALL parse(clline, clvari, ja, jpeighty, ilen)
!*              Get the whole set of analysis to be performed
                IF (lg_state(jf))  &
                    canal(ja,ig_number_field(jf)) = clvari
 260          CONTINUE
!     
!*            Now read specifics for each transformation

           DO 270 ja = 1, ig_total_ntrans(jf)
!     
!*            Read next line unless if analysis is NOINTERP (no input)

             IF (lg_state(jf)) THEN
!            For EXPORTED, AUXILARY, or EXPOUT:
              IF(canal(ja,ig_number_field(jf)) .NE. 'NOINTERP') THEN 
                  READ (UNIT = nulin,FMT = 2002) clline
                  CALL skip(clline, jpeighty)
              ENDIF
              IF (canal(ja,ig_number_field(jf)) .EQ. 'MOZAIC') THEN
                  CALL parse(clline, clvari, 3, jpeighty, ilen)
!*                Get dataset number
                  READ(clvari,FMT = 2005) nmapfl(ig_number_field(jf))
                  CALL parse(clline, clvari, 4, jpeighty, ilen)
!*                Get max nbr of neighbors for the grids associated to current field
                  READ(clvari,FMT = 2003) nmapvoi(ig_number_field(jf))
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'EXTRAP')THEN
                  CALL parse(clline, clvari, 1, jpeighty, ilen)
!*                Get extrapolation method
                  cextmet(ig_number_field(jf)) = clvari
!*                If choice is NINENN, read one more data
                  IF (cextmet(ig_number_field(jf)) .EQ. 'NINENN') THEN
                      CALL parse(clline, clvari, 4, jpeighty, ilen)
!*                    Get NINENN dataset identificator
                      READ(clvari,FMT = 2005) &
                          nninnfl(ig_number_field(jf))
                      IF (nninnfl(ig_number_field(jf)) .EQ. 0) THEN
                          IF (mpi_rank_global == 0) THEN
                              WRITE(UNIT = nulprt1,FMT = *)'  ***WARNING***'
                              WRITE(UNIT = nulprt1,FMT = *)  &
                                 ' **WARNING** The EXTRAP/NINENN dataset id cannot be 0' 
                              WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                              WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                              CALL prism_sys_flush(nulprt1)
                          ENDIF
                          CALL PRISM_SYS_ABORT()
                      ENDIF
!*                    If choice is WEIGHT, read more data
                  ELSE IF (cextmet(ig_number_field(jf)) .EQ. 'WEIGHT')  &
                          THEN
                      CALL parse(clline, clvari, 2, jpeighty, ilen)
!  Get number of neighbors used in extrapolation
!  If extrapolation method is NINENN, next variable is the MINIMUM
!  number of neighbors required (among the 8 closest) to perform
!  the extrapolation (cannot be greater than 4 for convergence). 
!  In case it is WEIGHT, it is the MAXIMUM number
!  of neighbors required by the extrapolation operation.
!     
                      READ(clvari,FMT = 2003)  &
                          neighbor(ig_number_field(jf))
                      CALL parse(clline, clvari, 5, jpeighty, ilen)
!*                    Get dataset number
                      READ(clvari,FMT = 2005)  &
                          nextfl(ig_number_field(jf))
                  ENDIF
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'INTERP')THEN
                  CALL parse(clline, clvari, 1, jpeighty, ilen)
!*                Get interpolation method
                  cintmet(ig_number_field(jf)) = clvari
!*                If interpolation uses ANAIS(G-M), read in more data
                  IF (cintmet(ig_number_field(jf)) .EQ. 'SURFMESH') THEN
                      CALL parse(clline, clvari, 4, jpeighty, ilen)
!*                    Get Anaism dataset identificator
                      READ(clvari,FMT = 2005) &
                          naismfl(ig_number_field(jf))
                      CALL parse(clline, clvari, 5, jpeighty, ilen)
!*                    Get max number of neighbors for the grids related to current field
                      READ(clvari,FMT = 2003)  &
                          naismvoi(ig_number_field(jf))
                  ENDIF 
                  IF (cintmet(ig_number_field(jf)) .EQ. 'GAUSSIAN') THEN
                      CALL parse(clline, clvari, 4, jpeighty, ilen)
!*                    Get Anaisg dataset identificator
                      READ(clvari,FMT = 2005) &
                          naisgfl(ig_number_field(jf))
                      CALL parse(clline, clvari, 5, jpeighty, ilen)
!*                    Get max number of neighbors for the grids related to current field
                      READ(clvari,FMT = 2003)  &
                          naisgvoi(ig_number_field(jf))
                  ENDIF
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'SCRIPR')THEN
!* Get field type (scalar/vector)
                  CALL parse(clline, clvari, 3, jpeighty, ilen)
                  READ(clvari,FMT = 2009) clstrg
                  IF(clstrg .EQ. 'VECTOR_I' .OR. clstrg .EQ. 'VECTOR_J')THEN
                      lg_vector  = .TRUE.
                  ENDIF
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'REDGLO')THEN
                  CALL parse(clline, clvari, 1, jpeighty, ilen)
!*                Get half the nbr of lats for reduced<->global gaussian grid switch
                  READ(clvari,FMT = 2008) cldeb,  &
                      ntronca(ig_number_field(jf))
                  IF (cldeb .NE. 'NO') THEN
                      CALL prcout &
                          ('ERROR in namcouple for analysis',  &
                          canal(ja,ig_number_field(jf)), 1) 
                      IF (mpi_rank_global == 0) THEN
                          WRITE (UNIT = nulprt1,FMT = *)  &
                             'Since version 2.3, the information on the reduced'
                          WRITE (UNIT = nulprt1,FMT = *)  &
                             'grid in namcouple has to be NOxx WHERE xx is half'
                          WRITE (UNIT = nulprt1,FMT = *)  &
                             'the number of latitude lines.'
                          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                          CALL prism_sys_flush(nulprt1)
                      ENDIF
                      CALL prism_sys_abort()
                  ENDIF
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'GLORED')THEN
                  CALL parse(clline, clvari, 1, jpeighty, ilen)
!*                Get gaussian troncature for reduced <-> global gaussian grid switch
                  READ(clvari,FMT = 2008) cldeb,  &
                      ntronca(ig_number_field(jf))
                  IF (cldeb .NE. 'NO') THEN
                      CALL prcout &
                          ('ERROR in namcouple for analysis',  &
                          canal(ja,ig_number_field(jf)), 1) 
                      IF (mpi_rank_global == 0) THEN
                          WRITE (UNIT = nulprt1,FMT = *)  &
                             'Since version 2.3, the information on the reduced'
                          WRITE (UNIT = nulprt1,FMT = *)  &
                             'grid in namcouple has to be NOxx WHERE xx is half'
                          WRITE (UNIT = nulprt1,FMT = *)  &
                             'the number of latitude lines.'
                          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                          CALL prism_sys_flush(nulprt1)
                      ENDIF
                      CALL prism_sys_abort()
                  ENDIF
                  CALL parse(clline, clvari, 4, jpeighty, ilen)
!*                Get NINENN dataset identificator
                  READ(clvari,FMT = 2005) nninnflg(ig_number_field(jf))
                  IF (nninnflg(ig_number_field(jf)) .EQ. 0) THEN
                      IF (mpi_rank_global == 0) THEN
                          WRITE(UNIT = nulprt1,FMT = *)  &
                             '**WARNING** The EXTRAP/NINENN dataset identificator in GLORED' 
                          WRITE(UNIT = nulprt1,FMT = *)  &
                             'cannot be 0'
                          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                          CALL prism_sys_flush(nulprt1)
                      ENDIF
                     CALL PRISM_SYS_ABORT()
                  ENDIF
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'BLASOLD') THEN
                  CALL parse(clline, clvari, 2, jpeighty, ilen)
!*                Get number of additional fields in linear formula
                  READ(clvari,FMT = 2003) nbofld (ig_number_field(jf))
                  DO ib = 1,nbofld (ig_number_field(jf))
                    READ (UNIT = nulin,FMT = 2002) clline
                    CALL skip(clline, jpeighty)
                  ENDDO
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'BLASNEW') THEN
                  CALL parse(clline, clvari, 2, jpeighty, ilen)
!*                Get number of additional fields in linear formula
                  READ(clvari,FMT = 2003) nbnfld (ig_number_field(jf))
                  DO ib = 1,nbnfld (ig_number_field(jf))
                    READ (UNIT = nulin,FMT = 2002) clline
                    CALL skip(clline, jpeighty)
                  ENDDO
!*                Get fields to restore subgrid variability 
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'SUBGRID') THEN
                  CALL parse(clline, clvari, 3, jpeighty, ilen)
!*                Get dataset number
                  READ(clvari,FMT = 2005) nsubfl(ig_number_field(jf))
                  CALL parse(clline, clvari, 4, jpeighty, ilen)
!*                Get max number of neighbors for the grids related to current field
                  READ(clvari,FMT = 2003) nsubvoi(ig_number_field(jf))
              ENDIF
             ELSE
!             For IGNORED, IGNOUT and OUTPUT, only one line for LOCTRANS
              READ (UNIT = nulin,FMT = 2002) clline
              CALL skip(clline, jpeighty)
             ENDIF             
 270        CONTINUE
        ENDIF
      ENDIF

!*      End of loop on NoF
!     
 250  CONTINUE

      IF (lg_oasis_field) THEN 
!     
!*       Search maximum number of fields to be combined in the BLASxxx analyses
!     
          ig_maxcomb = maxval(nbofld)
          IF (maxval(nbnfld).gt.ig_maxcomb) &
              ig_maxcomb = maxval(nbnfld)
!     
!*          Search maximum number of underlying neighbors for SURFMESH interpolation
!     
          ig_maxwoa = maxval(naismvoi)
          IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) &
                 'Max number of underlying neighbors for SURFMESH : ',  &
                 ig_maxwoa
              WRITE(nulprt1,*)' '
          ENDIF
!     
!*          Search maximum number of neighbors for GAUSSIAN interpolation
!     
          ig_maxnoa = maxval(naisgvoi)
          IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) &
                 'Max number of neighbors for GAUSSIAN interp : ', &
                 ig_maxnoa
              WRITE(nulprt1,*)' '
          ENDIF
!     
!*          Search maximum number of underlying neighbors for MOZAIC interpolation
!     
          ig_maxmoa = maxval(nmapvoi)
          IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) &
                 'Maximum number of underlying neighbors for MOZAIC interp: ', &
                 ig_maxmoa
              WRITE(nulprt1,*)' '
          ENDIF
!     
!*          Search maximum number of overlaying neighbors for SUBGRID interpolation
!     
          ig_maxsoa = maxval(nsubvoi)
          IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) &
                 'Maximum number of overlaying neighbors for SUBGRID interp :', &
                 ig_maxsoa
              WRITE(nulprt1,*)' '
          ENDIF
!     
!*          Search maximum number of different SURFMESH interpolations
!     
          ig_maxnfm = maxval(naismfl)
          IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) &
                 'Maximum number of different SURFMESH interpolations : ', &
                 ig_maxnfm
              WRITE(nulprt1,*)' '
          ENDIF
!     
!*          Search maximum number of different GAUSSIAN interpolations
!     
          ig_maxnfg = maxval(naisgfl)
          IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) &
                 'Maximum number of different GAUSSIAN interpolations : ', &
                 ig_maxnfg
              WRITE(nulprt1,*)' '
          ENDIF
!     
!*          Search maximum number of different MOZAIC interpolations
!     
          ig_maxnfp = maxval(nmapfl)
          IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) &
                 'Maximum number of different MOZAIC interpolations : ', &
                 ig_maxnfp
              WRITE(nulprt1,*)' '
          ENDIF
!     
!*          Search maximum number of different SUBGRID interpolations
!     
          ig_maxnfs = maxval(nsubfl)
          IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) &
                 'Maximum number of different SUBGRID interpolations : ', &
                 ig_maxnfs
              WRITE(nulprt1,*)' '
          ENDIF
!     
!*          Search maximum number of different NINENN extrapolations
!     
          ig_maxnfn = maxval(nninnfl)
          IF (maxval(nninnflg).gt.ig_maxnfn) &
              ig_maxnfn = maxval(nninnflg)
          IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) &
                 'Maximum number of different NINENN extrapolations : ', &
                 ig_maxnfn
              WRITE(nulprt1,*)' '
          ENDIF
!     
!*          Search maximum number of neighbors for extrapolation  
!     
          ig_maxext = maxval(neighbor)
          IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) &
                 'Maximum number of neighbors for extrapolation : ', &
                 ig_maxext
              WRITE(nulprt1,*)' '
          ENDIF
!     
!*          Search maximum number of different extrapolation
!     
          ig_maxnbn = maxval(nextfl)
!  
      ENDIF
!*    Formats

 2001 FORMAT(A9)
 2002 FORMAT(A80)
 2003 FORMAT(I4)
 2004 FORMAT(I8)
 2005 FORMAT(I2)
 2008 FORMAT(A2,I4)
 2009 FORMAT(A8)
 2010 FORMAT(A3,A1,I2)

!*    3. End of routine
!        --------------

      IF (mpi_rank_global == 0) THEN
          WRITE(UNIT = nulprt1,FMT = *)' '
          WRITE(UNIT = nulprt1,FMT = *)'-- End of ROUTINE inipar_alloc --'
          CALL FLUSH (nulprt1)
      ENDIF

!      call prism_sys_debug_exit(subname)
      RETURN

!*    Error branch output

 110  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *) &
             ' No active $NBMODEL data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar_alloc'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 130  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' No active $MACHINE data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 210  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' No active $FIELDS data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar_alloc'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 230  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' No active $STRING data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar_alloc'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 241  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' NFIELDS larger or smaller than the number of inputs in namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar_alloc'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()

!      call prism_sys_debug_exit(subname)

      END SUBROUTINE inipar_alloc

!===============================================================================

      SUBROUTINE inipar
!****
!               *****************************
!               * OASIS ROUTINE  -  LEVEL 0 *
!               * -------------     ------- *
!               *****************************

!**** *inipar*  - Get run parameters

!     Purpose:
!     -------
!     Reads and prints out run parameters.

!**   Interface:
!     ---------
!       *CALL*  *inipar*

!     Input:
!     -----
!     None

!     Output:
!     ------
!     None

!     Workspace:
!     ---------
!     None

!     Externals:
!     ---------
!     parse

!     Reference:
!     ---------
!     See OASIS manual (1995) 

!     History:
!     -------
!     Version Programmer  Date      Description
!     ------- ----------  ----      -----------  
!       1.0   L. Terray   94/01/01  created
!       1.1   L. Terray   94/08/01  modified: change in namelist
!                                   nice flag + new case for nmode
!       1.1   L. Terray   94/10/01  modified: change printing
!       2.0b  L. Terray   95/07/24  modified: new structure
!       2.0   L. Terray   96/02/01  modified: lecture of cdqdt for
!                                   subgrid and add mozaic analysis
!                                   Lecture of a unit for filling
!       2.1   L. Terray   96/09/25  Changes to mozaic and subgrid
!                                   analysis, addition of nfend and
!                                   nintflx, check[in-out] analysis
!                                   addition of nointerp case.
!       2.2   L. Terray   97/02/12  Printing bug on analysis sub-
!                                   grid (SOLAR) corrected
!       2.2   L. Terray   97/02/20  Printing bug on analysis ANAIS
!                                   corrected
!       2.2   L. Terray   97/12/14  Add new input: MODINFO and new
!                                   extrapolation technique
!       2.3   S. Valcke   99/03/14  cjobnam with 3 or 4 characters
!       2.3   S. Valcke   99/03/25  troncature as NOxxxx in namcouple
!       2.3   S. Valcke   99/03/30  READ/WRITE flag and dataset index
!                                   for NINENN weights
!       2.3   S. Valcke   99/04/30  NLOGPRT for printing levels
!       2.3   L. Terray   99/09/15  changed periodicity variables
!                                   and input them as field parameters
!       2.4   J. Latour   99/10/28  Add new input: CHATYPE for type of
!      	 	    	      	    message passing : MPI2 or PVM3
!       2.4   S. Valcke   2K/02/04  Additional readings for CLIM/MPI2
!       2.5   S. Valcke   2K/09/04  Remove $MACHINE, clmach, cmach
!       2.5   S. Valcke   2K/09/04  $CHATYPE in $CHANNEL
!       2.5   S. Valcke   2K/09/05  Add input line with integral flag
!                                   for  check[in-out], remove nintflx 
!       2.5   S. Valcke   2K/09/05  Remove fld 3rd input line for CLIM
!       2.5   S. Valcke   2K/09/08  Changed input lines for PVM3&MPI2 
!       2.5   J. Latour   01/11/28  Add MPI1 startup
!       2.5   A. Caubel   02/05/15  Mods for dynamic allocation
!       2.5   S. Valcke   02/06/12  PVM3 no longer supported
!       2.5   V. Gayler   01/09/20  Scrip-Remapping
!       3.0   S. Legutke  03/04/24  proposal of CF compliant cfldlab entries
!                                   added labels for echam5/mpi-om
!                                   grouping into classes
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  IMPLICIT NONE

!* ---------------------------- Local declarations --------------------

      CHARACTER*80 clline, clvari
      CHARACTER*9 clword, clstring, clprint, clcal, clchan
      CHARACTER*9 cljob, clmod, cltime, clseq, cldate, clhead
      CHARACTER*8 cl_print_trans, cl_print_state
      CHARACTER*3 clinfo, clind
      CHARACTER*1 clequa
      CHARACTER*64 cl_cfname,cl_cfunit
      INTEGER (kind=ip_intwp_p) iind, il_aux
      INTEGER (kind=ip_intwp_p) il_file_unit, id_error
      INTEGER (kind=ip_intwp_p) il_max_entry_id, il_no_of_entries
      INTEGER (kind=ip_intwp_p) il_i, il_pos
      LOGICAL llseq, lllag, ll_exist
      INTEGER lastplace
      integer (kind=ip_intwp_p) :: ib,ilind1,ilind2,ilind
      integer (kind=ip_intwp_p) :: ja,jf,jfn,jz,jm,ilen,idum
      integer (kind=ip_intwp_p) :: ifca,ifcb,ilab,jff,jc
      integer (kind=ip_intwp_p) :: icofld,imodel
      character(len=*),parameter :: subname='mod_prism_namcouple:inipar'

!* ---------------------------- Poema verses --------------------------

!  call prism_sys_debug_enter(subname)

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!*    1. Get basic info for the simulation 
!        ---------------------------------

  IF (mpi_rank_global == 0) THEN
      WRITE (UNIT = nulprt1,FMT = *)' '
      WRITE (UNIT = nulprt1,FMT = *)'   ROUTINE inipar  -  Level 0'
      WRITE (UNIT = nulprt1,FMT = *)'   **************     *******'
      WRITE (UNIT = nulprt1,FMT = *)' '
      WRITE (UNIT = nulprt1,FMT = *)'   Initialization of run parameters'
      WRITE (UNIT = nulprt1,FMT = *)'   Reading input file namcouple'
      WRITE (UNIT = nulprt1,FMT = *)' '
      CALL FLUSH(nulprt1)
  ENDIF

!* Initialize character keywords to locate appropriate input

      clstring = ' $STRINGS'
      cljob    = ' $JOBNAME'
      clchan   = ' $CHANNEL'
      clmod    = ' $NBMODEL'
      cltime   = ' $RUNTIME'
      clseq    = ' $SEQMODE'
      cldate   = ' $INIDATE'
      clhead   = ' $MODINFO'
      clprint  = ' $NLOGPRT'
      clcal    = ' $CALTYPE'

!* Initialize some variables 
      ndate = 0 ; nmseq = 1 ; ntime = 432000 ; niter = 5 
      nstep = 86400 ; nitfn=4
      cjobnam = 'DEF'
      lmodinf = .TRUE. 


      if (ig_total_nfield .ne. 0) then
!* CF long names for exchange fields
      INQUIRE (file='cf_name_table.txt', exist=ll_exist)

      IF (ll_exist) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (nulprt1,*) 'inipar: Reading CF name table!'
          ENDIF
          il_file_unit = 99
          OPEN (file='cf_name_table.txt', unit=il_file_unit,  &
              form='formatted', status='old')

          READ (unit=il_file_unit,fmt=*,iostat=id_error)
          READ (unit=il_file_unit,fmt=*,iostat=id_error)  &
              il_max_entry_id, il_no_of_entries

          IF (id_error.ne.0) THEN 
              IF (mpi_rank_global == 0) THEN
                  WRITE (nulprt1,*) 'inipar :cf_name_table.txt:'  &
                     ,' Reading of first record failed!'
                  WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                  WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                  CALL prism_sys_flush(nulprt1)
              ENDIF
              CALL prism_sys_abort()
          ENDIF

          IF (il_max_entry_id.gt.0) THEN 
              allocate (cfldlab(1:il_max_entry_id),STAT=id_error)
              IF (id_error.ne.0) THEN 
                  IF (mpi_rank_global == 0) THEN
                      WRITE(nulprt1,*) 'inipar: Allocation of cfldlab failed!'
                      WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                      WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                      CALL prism_sys_flush(nulprt1)
                  ENDIF
                  CALL prism_sys_abort()
              ENDIF
          ELSE
              IF (mpi_rank_global == 0) THEN
                  WRITE (nulprt1,*) 'inipar: cf_name_table.txt:',  &
                     'The number of entries is less than 0 !'
                  WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                  WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                  CALL prism_sys_flush(nulprt1)
              ENDIF
              CALL prism_sys_abort()                
          ENDIF

          READ (unit=il_file_unit,fmt=*,iostat=id_error)
          DO il_i=1,il_no_of_entries
            READ (unit=il_file_unit,fmt=*,iostat=id_error)  &
                il_pos,cl_cfname,cl_cfunit

            IF (id_error.eq.0) THEN 
                IF (il_pos .le. il_max_entry_id) THEN 
                    cfldlab(il_pos)=trim(cl_cfname)
                ELSE
                    IF (mpi_rank_global == 0) THEN
                        WRITE (nulprt1,*) 'inipar: cf_name_table.txt:', &
                           'Record ',il_i,': numlab =',il_pos,' out of range!'
                        WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                        WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                        CALL prism_sys_flush(nulprt1)
                    ENDIF
                    CALL prism_sys_abort()  
                ENDIF
            ELSE
                IF (mpi_rank_global == 0) THEN
                    WRITE (nulprt1,*) 'inipar: cf_name_table.txt:', &
                       'Reading record ',il_i,' failed!'
                    WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                    WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                    CALL prism_sys_flush(nulprt1)
                ENDIF
                CALL prism_sys_abort() 
            ENDIF
          END DO
      ELSE
          IF (mpi_rank_global == 0) THEN
              WRITE (nulprt1,*) 'inipar: cf_name_table.txt missing'
              WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
              WRITE (nulprt1,'(a)') ' error = STOP in inipar'
              CALL prism_sys_flush(nulprt1)
          ENDIF
          CALL prism_sys_abort() 
      ENDIF
      CLOSE(il_file_unit)
      endif

!* First get experiment name 

      REWIND nulin
 100  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 110) clword
      IF (clword .NE. cljob) GO TO 100
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Nothing on input for $JOBNAME '
              WRITE (UNIT = nulprt1,FMT = *) ' Default value will be used '
              WRITE (UNIT = nulprt1,FMT = *) ' '
          ENDIF
      ELSE IF (ilen .GT. 0 .AND. ilen .NE. 3 .AND. ilen .NE. 4 ) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Input variable length is incorrect'
              WRITE (UNIT = nulprt1,FMT = *) ' ilen = ', ILEN  
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Check $JOBNAME variable spelling '
              WRITE (UNIT = nulprt1,FMT = *) ' Default value will be used '
              WRITE (UNIT = nulprt1,FMT = *) ' '
          ENDIF
      ELSE
          IF (ilen .EQ. 3) THEN
              WRITE (cjobnam,FMT='(A1,A3)') ' ',clvari
          ELSE IF (ilen .EQ. 4) THEN
              WRITE (cjobnam,FMT='(A4)') clvari
          ENDIF
      ENDIF
 
!* Print out experiment name

      CALL prcout &
          ('The experiment name for this run is cjobnam =', cjobnam,1)

!* Get number of models involved in this simulation

      REWIND nulin
 120  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 130) clword
      IF (clword .NE. clmod) GO TO 120
      READ (UNIT = nulin,FMT = 1002) clline

!* Get model names

      DO 140 jm = 1, ig_nmodel
        imodel = jm + 1
        CALL parse (clline, clvari, imodel, jpeighty, ilen)
        cmodnam(jm) = clvari

!* Print out model names

        IF (mpi_rank_global == 0) THEN
            WRITE (UNIT = nulprt1,FMT =' &
               (''   Name for model '',I1,'' is '',A6,/)')  &
               jm, cmodnam(jm)
        ENDIF

 140  CONTINUE

!* Get model maximum unit number used if they appear on the line

      DO 142 jm = 1, ig_nmodel
        imodel = jm + 1 + ig_nmodel
        CALL parse (clline, clvari, imodel, jpeighty, ilen)
        IF (ilen .gt. 0) THEN
            READ (clvari,FMT = 1004) iga_unitmod(jm)

!* Print out model minimum logfile unit number
            IF (mpi_rank_global == 0) THEN
                WRITE (UNIT = nulprt1,FMT = *) ' '
                WRITE (UNIT=nulprt1,FMT='(''The maximum Fortran unit number'', &
                   '' used in model'', I2, '' is '', I2)') &
                   jm, iga_unitmod(jm)
                WRITE (UNIT = nulprt1,FMT = *) ' '
            ENDIF

!* Verify that maximum unit number is larger than 1024; 
!* if not, use 1024.
            IF (iga_unitmod(jm) .lt. 1024) iga_unitmod(jm)=1024
        ELSE
            IF (mpi_rank_global == 0) THEN
                WRITE (UNIT = nulprt1, FMT = *) &
                   ' WARNING: You did not give in the namcouple the maximum', &
                   ' Fortran unit numbers used in your models.', &
                   ' Oasis will suppose that units above 1024 are free !'
            ENDIF
            iga_unitmod(jm)=1024
        ENDIF
 142      CONTINUE

!* Get hardware info for this OASIS simulation

      REWIND nulin
 160  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 170) clword
      IF (clword .NE. clchan) GO TO 160
      READ (UNIT = nulin,FMT = 1002) clline
      CALL skip(clline, jpeighty)
      IF(cchan .EQ. 'MPI2' .OR. cchan .EQ. 'MPI1'  &
          .OR. cchan .EQ. 'GSIP') THEN
!* Get one additional line for each model
          DO 186 jm = 1, ig_nmodel
            READ (UNIT = nulin,FMT = 1002) clline

!*    Get the launching arguments for the model

            CALL parseblk (clline, clvari, 3, jpeighty, ilen)
            IF (ilen .LE. 0) THEN
                IF (mpi_rank_global == 0) THEN
                    WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
                    WRITE (UNIT = nulprt1,FMT = *)  &
                       'No launching argument for model', jm
                    WRITE (UNIT = nulprt1,FMT = *) ' '
                ENDIF
                cmpiarg(jm)=' '
            ELSE
                cmpiarg(jm)=clvari
                IF (mpi_rank_global == 0) THEN
                    WRITE (UNIT = nulprt1,FMT = *) ' '
                    WRITE (UNIT =nulprt1,FMT=' &
                       (''The launching argument for model '', I2, '' is'')') jm
                    WRITE (UNIT = nulprt1,FMT = *) cmpiarg(jm)
                    WRITE (UNIT = nulprt1,FMT = *) ' '
                    WRITE (UNIT = nulprt1,FMT = *) 'ilen ',ILEN
                ENDIF
            ENDIF
            

 186      CONTINUE

         ENDIF

!* Get total time for this simulation

      REWIND nulin
 190  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 191) clword
      IF (clword .NE. cltime) GO TO 190
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Nothing on input for $RUNTIME '
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Default value of 5 days will be used '
              WRITE (UNIT = nulprt1,FMT = *) ' '
          ENDIF
        ELSE
          READ (clvari,FMT = 1004) ntime
      ENDIF

!* Print out total time

      CALL prtout &
          ('The total time for this run is ntime =', ntime, 1)

!* Get initial date for this simulation

      REWIND nulin
 192  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 193) clword
      IF (clword .NE. cldate) GO TO 192
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Nothing on input for $INIDATE '
              WRITE (UNIT = nulprt1,FMT = *) ' Default value will be used '
              WRITE (UNIT = nulprt1,FMT = *) ' '
          ENDIF
        ELSE
          READ (clvari,FMT = 1004) ndate
      ENDIF

!* Print out initial date

      CALL prtout &
          ('The initial date for this run is ndate = ', ndate, 1)

!* Get number of sequential models involved in this simulation

      REWIND nulin
 194  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 195) clword
      IF (clword .NE. clseq) GO TO 194
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Nothing on input for $SEQMODE '
              WRITE (UNIT = nulprt1,FMT = *) ' Default value will be used '
              WRITE (UNIT = nulprt1,FMT = *) ' '
          ENDIF
      ELSE
          READ (clvari,FMT = 1003) nmseq
      ENDIF

!* Print out the number of sequential models

      CALL prtout &
          ('The number of sequential fields is nmseq =', nmseq, 1)

!* Get the information mode for this simulation

      REWIND nulin
 196  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 197) clword
      IF (clword .NE. clhead) GO TO 196
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Nothing on input for $MODINFO '
              WRITE (UNIT = nulprt1,FMT = *) ' Default value will be used '
              WRITE (UNIT = nulprt1,FMT = *) ' '
          ENDIF
        ELSE IF (ilen .GT. 0 .AND. ilen .NE. 3) THEN
            IF (mpi_rank_global == 0) THEN
                WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
                WRITE (UNIT = nulprt1,FMT = *)  &
                   ' Input variable length is incorrect'
                WRITE (UNIT = nulprt1,FMT = *)  &
                   ' Info mode uncorrectly specified'
                WRITE (UNIT = nulprt1,FMT = *) ' ilen = ', ILEN  
                WRITE (UNIT = nulprt1,FMT = *)  &
                   ' Check $MODINFO variable spelling '
                WRITE (UNIT = nulprt1,FMT = *) ' Default value will be used '
            ENDIF
        ELSE
          clinfo = clvari
          IF (clinfo .EQ. 'YES') THEN 
              lmodinf = .TRUE. 
            ELSE 
              lmodinf = .FALSE. 
          ENDIF 
      ENDIF

!* Print out the information mode

      CALL prcout &
          ('The information mode is activated ? ==>', clinfo, 1)

!* Get the printing level for this simulation

      REWIND nulin
 198  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 199) clword
      IF (clword .NE. clprint) GO TO 198
      nlogprt = 0
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Nothing on input for $NLOGPRT '
              WRITE (UNIT = nulprt1,FMT = *) ' Default value 2 will be used '
              WRITE (UNIT = nulprt1,FMT = *) ' '
          ENDIF
      ELSE IF (ilen .gt. 8) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Input variable length is incorrect'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Printing level uncorrectly specified'
              WRITE (UNIT = nulprt1,FMT = *) ' ilen = ', ILEN  
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Check $NLOGPRT variable spelling '
              WRITE (UNIT = nulprt1,FMT = *) ' Default value will be used '
          ENDIF
      ELSE
          READ (clvari,FMT = 1004) nlogprt
      ENDIF

!* Print out the printing level

      CALL prtout &
          ('The printing level is nlogprt =', nlogprt, 1)

!* Get the calendar type for this simulation

      REWIND nulin
 200  CONTINUE
      READ (UNIT = nulin,FMT = 1001,END = 201) clword
      IF (clword .NE. clcal) GO TO 200
      READ (UNIT = nulin,FMT = 1002) clline
      CALL parse (clline, clvari, 1, jpeighty, ilen)
      IF (ilen .LE. 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *)  &
                 ' Nothing on input for $CALTYPE '
              WRITE (UNIT = nulprt1,FMT = *) ' Default value 1 will be used '
              WRITE (UNIT = nulprt1,FMT = *) ' '
          ENDIF
          ncaltype = 1
      ELSE
          READ (clvari,FMT = 1003) ncaltype
      ENDIF

!* Print out the calendar type

      CALL prtout &
         ('The calendar type is ncaltype =', ncaltype, 1)
      IF (ncaltype .EQ. 1) THEN
          CALL prcout &
              ('Gregorian calendar', ' ', 1)
      ELSE IF (ncaltype .EQ. 0) THEN
          CALL prcout &
              ('365 day calendar (no leap years)', ' ', 1)
      ELSE
          CALL prtout &
              ('The number of days per month =', ncaltype, 1)
      ENDIF

!* Formats

 1001 FORMAT(A9)
 1002 FORMAT(A80)
 1003 FORMAT(I3)
 1004 FORMAT(I8)

!*    2. Get field information
!        ---------------------

!* Init. array needed for local transformation  

      ig_local_trans(:) = ip_instant

!* Init. arrays needed for ANAIS(G-M),mapping and subgrid interpolation

      IF (lg_oasis_field) THEN
         lcoast = .TRUE.
         DO 215 jz = 1, ig_nfield
            linit(jz) = .TRUE.
            lmapp(jz) = .TRUE.
            lsubg(jz) = .TRUE.
            lextra(jz) = .TRUE.
            varmul(jz) = 1.
            lsurf(jz) = .FALSE.
 215     CONTINUE 
!     
!* Initialize flag indicating IF EXTRAP/NINENN parameter sets have 
!* already been calculated or read (.TRUE.) or not (.FALSE.)
!     
         DO 217 jfn = 1, ig_maxnfn
            lweight(jfn) = .FALSE.
 217     CONTINUE
      ENDIF

!* Get the SSCS for all fields

      REWIND nulin
 220  CONTINUE
      READ (UNIT = nulin,FMT = 2001,END = 230) clword
      IF (clword .NE. clstring) GO TO 220

!  Initialize restart name index

      il_aux = 0

!* Loop on total number of fields (NoF)

      DO 240 jf = 1, ig_total_nfield

!* Read first two lines of strings for field n = 1,2...,ig_total_nfield
!      --->>> Main characteristics of fields

!* First line

         READ (UNIT = nulin,FMT = 2002) clline
         CALL parse(clline, clvari, 1, jpeighty, ilen)
!* Get output field symbolic name
         cg_input_field(jf) = clvari
         IF (lg_state(jf)) cnaminp(ig_number_field(jf)) = cg_input_field(jf)
         IF (lg_state(jf)) cnamout(ig_number_field(jf)) = cg_output_field(jf)
         CALL parse(clline, clvari, 3, jpeighty, ilen)
!* Get field label number
         READ (clvari,FMT = 2003) ig_numlab(jf)
         IF (lg_state(jf)) numlab(ig_number_field(jf)) = ig_numlab(jf)
         CALL parse(clline, clvari, 4, jpeighty, ilen)
!* Get field exchange frequency
         IF (clvari(1:4) .EQ. 'ONCE') THEN

!* The case 'ONCE' means that the coupling period will be equal to the 
!* time of the simulation

            ig_freq(jf) = ntime
         ELSE
         READ (clvari,FMT = 2004) ig_freq(jf)
         IF (ig_freq(jf) .EQ. 0) THEN
            GOTO 236
         ELSEIF (ig_freq(jf) .gt. ntime) THEN
             IF (mpi_rank_global == 0) THEN
                 WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
                 WRITE (UNIT = nulprt1,FMT = *)  &
                    'The coupling period of the field ',jf
                 WRITE (UNIT = nulprt1,FMT = *)  &
                    'is greater than the time of the simulation '
                 WRITE (UNIT = nulprt1,FMT = *)  &
                    'This field will not be exchanged !'
             ENDIF
         ENDIF
     ENDIF
         IF (lg_state(jf)) nfexch(ig_number_field(jf)) = ig_freq(jf)
!* Fill up restart file number and restart file name arrays
         IF (cg_restart_file(jf).ne.' ') THEN
             IF (jf.eq.1) THEN
                 il_aux = il_aux + 1
                 ig_no_rstfile(jf) = il_aux
                 cg_name_rstfile (ig_no_rstfile(jf)) =  &
                     cg_restart_file(jf)
             ELSEIF (jf.gt.1) THEN
                 IF (ALL(cg_name_rstfile.ne.cg_restart_file(jf))) THEN
                     il_aux = il_aux + 1
                     ig_no_rstfile(jf) = il_aux
                     cg_name_rstfile (ig_no_rstfile(jf))=  &
                         cg_restart_file(jf)
                 ELSE 
                     DO ib = 1, jf - 1 
                       IF(cg_name_rstfile(ig_no_rstfile(ib)).eq. &
                           cg_restart_file(jf)) THEN
                           ig_no_rstfile(jf) = ig_no_rstfile(ib)
                       ENDIF
                     ENDDO
                 ENDIF
             ENDIF
         ENDIF
         CALL parse(clline, clvari, 7, jpeighty, ilen)
!* For all techniques beside PIPE and NONE technique, get eventually
!* the field STATUS
         IF (cchan .ne. 'PIPE' .and. cchan .ne. 'NONE') THEN
             IF (clvari(1:8).eq.'EXPORTED' .or.  &
                 clvari(1:8).eq.'AUXILARY') THEN
                 cstate(ig_number_field(jf)) = clvari
             ELSEIF (clvari(1:6) .eq. 'EXPOUT') THEN
                 cstate(ig_number_field(jf)) = 'EXPORTED'
             ENDIF
!*
         ELSE
            IF (lg_state(jf)) cficout(ig_number_field(jf)) = clvari
!*          Get field status
            CALL parse(clline, clvari, 8, jpeighty, ilen)
            IF (lg_state(jf)) cstate(ig_number_field(jf)) = clvari
            IF (lg_state(jf)) then
            IF (cstate(ig_number_field(jf)) .ne. 'EXPORTED' &
                .and. cstate(ig_number_field(jf)) .ne. 'AUXILARY') THEN
                CALL prtout  &
                    ('Error in namcouple for status of field',jf,1)
                IF (mpi_rank_global == 0) THEN
                    WRITE (UNIT = nulprt1,FMT = *)  &
                       '==> Must be EXPORTED or AUXILARY'
                    WRITE (UNIT = nulprt1,FMT = *)  &
                       'Maybe you forgot the output FILE name which'
                    WRITE (UNIT = nulprt1,FMT = *)  &
                       'is mandatory for PIPE or NONE techniques'
                    WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                    WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                    CALL prism_sys_flush(nulprt1)
                ENDIF
                CALL PRISM_SYS_ABORT() 
            ENDIF
            ENDIF
        ENDIF

!* Second line

        IF (ig_total_state(jf) .ne. ip_input) THEN
           READ (UNIT = nulin,FMT = 2002) clline
!     *      First determine what information is on the line
           CALL parse(clline, clvari, 3, jpeighty, ilen)
           IF (ilen .lt. 0) THEN
!     *          IF only two words on the line, then they are the locator 
!     *          prefixes and the grids file must be in NetCDF format       
              ig_lag(jf)=0
              ig_total_nseqn(jf)=1
              IF (lg_state(jf)) then
                  nseqn(ig_number_field(jf)) = 1
                  nlagn(ig_number_field(jf)) = 0
              ENDIF
              llseq=.FALSE.
              lllag=.FALSE.
              IF (mpi_rank_global == 0) THEN
                  WRITE (UNIT=nulprt1,FMT=3043) jf
              ENDIF
! tcraig, remove nmseq checking, not needed in oasis3-mct
!              IF(nmseq .gt. 1 .and. .not. llseq) GO TO 231
           ELSE 
              READ(clvari,FMT = 2011) clind, clequa, iind
              IF (clind .EQ. 'SEQ' .or. clind .EQ. 'LAG' .and. &
                   clequa .EQ. '=') THEN
!     *              If 3rd word is an index, then first two words are 
!     *              locator prefixes and grids file must be NetCDF format
                 ilind1=3
                 ilind2=6
              ELSE
!     *              If not, the first 4 words are grid dimensions and next
!     *              2 words are locator prefixes, and grids file may be or
!     *              not in NetCDF FORMAT.
                 ilind1=7
                 ilind2=10
              ENDIF
!     *          Get possibly additional indices
              ig_lag(jf)=0
              ig_total_nseqn(jf)=1
              IF (lg_state(jf)) then
                  nseqn(ig_number_field(jf)) = 1
                  nlagn(ig_number_field(jf)) = 0
              ENDIF
              llseq=.FALSE.
              lllag=.FALSE.
!     
              DO 245 ilind=ilind1, ilind2
                 CALL parse(clline, clvari, ilind, jpeighty, ilen)
                 IF(ilen .eq. -1) THEN
                     IF (mpi_rank_global == 0) THEN
                         IF (nlogprt .GE. 0) THEN 
                             IF(.NOT. lllag) WRITE (UNIT=nulprt1,FMT=3043) jf
                         ENDIF
                     ENDIF
!                    IF(nmseq .gt. 1 .and. .not. llseq) GO TO 231
                    GO TO 247
                 ELSE
                    READ(clvari,FMT = 2011) clind, clequa, iind
                    IF (clind .EQ. 'SEQ') THEN
!                       IF (iind .gt. nmseq) THEN
!                          GO TO 232
!                       ELSE IF (iind .eq. 0) THEN
!                          GO TO 234
!                       ELSE
                          ig_total_nseqn(jf)=iind
                          IF (lg_state(jf)) &
                              nseqn(ig_number_field(jf)) = iind
                          llseq=.TRUE.
!                       ENDIF
                    ELSE IF (clind .eq. 'LAG') THEN
                       ig_lag(jf)=iind
                       IF (lg_state(jf)) &
                           nlagn(ig_number_field(jf)) = iind
                       lllag=.TRUE.
                       IF (mpi_rank_global == 0) THEN
                           WRITE (UNIT = nulprt1,FMT = 3044)jf,ig_lag(jf)
                       ENDIF
                    ENDIF              
                 ENDIF
 245          CONTINUE
          ENDIF
       ENDIF


 247    CONTINUE

!* Third line

        IF (lg_state(jf)) THEN
           READ (UNIT = nulin,FMT = 2002) clline
           CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get source grid periodicity type
           csper(ig_number_field(jf)) = clvari
           IF(csper(ig_number_field(jf)) .NE. 'P' .AND.  &
                csper(ig_number_field(jf)) .NE. 'R') THEN
              CALL prtout &
            ('ERROR in namcouple for source grid type of field', jf, 1)
              IF (mpi_rank_global == 0) THEN
                  WRITE (UNIT = nulprt1,FMT = *) '==> must be P or R'
                  WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                  WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                  CALL prism_sys_flush(nulprt1)
              ENDIF
              CALL PRISM_SYS_ABORT()
           ENDIF
!     
           CALL parse(clline, clvari, 2, jpeighty, ilen)
!     * Get nbr of overlapped longitudes for the Periodic type source grid
           READ(clvari,FMT = 2005) nosper(ig_number_field(jf))
           CALL parse(clline, clvari, 3, jpeighty, ilen)
!     * Get target grid periodicity type
           ctper(ig_number_field(jf)) = clvari
           IF(ctper(ig_number_field(jf)) .NE. 'P' .AND.  &
                ctper(ig_number_field(jf)) .NE. 'R') THEN
              CALL prtout &
            ('ERROR in namcouple for target grid type of field', jf, 1)
              IF (mpi_rank_global == 0) THEN
                  WRITE (UNIT = nulprt1,FMT = *) '==> must be P or R'
                  WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                  WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                  CALL prism_sys_flush(nulprt1)
              ENDIF
              CALL PRISM_SYS_ABORT()
           ENDIF
!     
           CALL parse(clline, clvari, 4, jpeighty, ilen)
!     * Get nbr of overlapped longitudes for the Periodic type target grid
           READ(clvari,FMT = 2005) notper(ig_number_field(jf))
!     
!     Define stuff related to parallel decomposition. For now, as oasis
!     is always monoproc, cparal(ig_number_field(jf))='SERIAL'. 
!     
           IF (cchan .EQ. 'MPI2' .OR. cchan .EQ. 'MPI1' &
               .OR. cchan .EQ. 'GSIP') THEN
              cparal(ig_number_field(jf)) = 'SERIAL'
           ENDIF
       ENDIF

!* Get the local transformation

        IF (.not. lg_state(jf)) THEN
           IF (ig_total_state(jf) .ne. ip_input .and.  &
                ig_total_ntrans(jf) .gt. 0 ) THEN
              READ (UNIT = nulin,FMT = 2002) clline
              CALL skip(clline, jpeighty)
              DO ja=1,ig_total_ntrans(jf)
                 READ (UNIT = nulin,FMT = 2002) clline 
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
                 IF (clvari(1:7) .eq. 'INSTANT') THEN 
                    ig_local_trans(jf) = ip_instant
                 ELSEIF (clvari(1:7) .eq. 'AVERAGE') THEN
                    ig_local_trans(jf) = ip_average
                 ELSEIF (clvari(1:7) .eq. 'ACCUMUL') THEN
                    ig_local_trans(jf) = ip_accumul
                 ELSEIF (clvari(1:5) .eq. 'T_MIN') THEN
                    ig_local_trans(jf) = ip_min
                 ELSEIF (clvari(1:5) .eq. 'T_MAX') THEN
                    ig_local_trans(jf) = ip_max   
                 ELSE
                    CALL prtout &
       ('ERROR in namcouple for local transformations of field', jf, 1)
                    IF (mpi_rank_global == 0) THEN
                        WRITE (UNIT = nulprt1,FMT = *)  &
                           '==> Must be INSTANT, AVERAGE, ACCUMUL, T_MIN or T_MAX'
                        WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                        WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                        CALL prism_sys_flush(nulprt1)
                    ENDIF
                    CALL PRISM_SYS_ABORT()  
                 ENDIF
              ENDDO
           ENDIF
       ELSE
         READ (UNIT = nulin,FMT = 2002) clline
              CALL skip(clline, jpeighty)
!     
!     * Now read specifics for each transformation
 
           DO 270 ja = 1, ig_ntrans(ig_number_field(jf))
!     
!     * Read next line unless if analysis is NOINTERP (no input)
!     
              IF(canal(ja,ig_number_field(jf)) .NE. 'NOINTERP') THEN 
                 READ (UNIT = nulin,FMT = 2002) clline
                 CALL skip(clline, jpeighty)
              ENDIF
              IF (canal(ja,ig_number_field(jf)) .EQ. 'LOCTRANS') THEN
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
                 IF (clvari(1:7) .eq. 'INSTANT') THEN 
                    ig_local_trans(jf) = ip_instant
                 ELSEIF (clvari(1:7) .eq. 'AVERAGE') THEN
                    ig_local_trans(jf) = ip_average
                 ELSEIF (clvari(1:7) .eq. 'ACCUMUL') THEN
                    ig_local_trans(jf) = ip_accumul
                 ELSEIF (clvari(1:5) .eq. 'T_MIN') THEN
                    ig_local_trans(jf) = ip_min
                 ELSEIF (clvari(1:5) .eq. 'T_MAX') THEN
                    ig_local_trans(jf) = ip_max   
                 ELSE
                    CALL prtout &
       ('ERROR in namcouple for local transformations of field', jf, 1)
                    IF (mpi_rank_global == 0) THEN
                        WRITE (UNIT = nulprt1,FMT = *)  &
                           '==> Must be INSTANT, AVERAGE, ACCUMUL, T_MIN or T_MAX'
                        WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                        WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                        CALL prism_sys_flush(nulprt1)
                    ENDIF
                    CALL PRISM_SYS_ABORT()  
                 ENDIF
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'MASK') THEN
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get mask value
                 READ(clvari,FMT = 2006) amskval(ig_number_field(jf))
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'MASKP') THEN
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get the Mask value for the output field
                 READ(clvari,FMT = 2006)amskvalnew(ig_number_field(jf))
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'MOZAIC')THEN
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get file name for grid mapping
                 cgrdmap(ig_number_field(jf)) = clvari
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
!     * Get related logical unit 
                 READ(clvari,FMT = 2005) nlumap(ig_number_field(jf))
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'INVERT')THEN
                 ig_invert(jf) = 1
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get lat-lon ordering for initial fields
                 cxordbf(ig_number_field(jf)) = clvari
                 IF(trim(adjustl(clvari)).eq.'NORSUD') &
                          ig_invert(jf)=ig_invert(jf)+1
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
                 cyordbf(ig_number_field(jf)) = clvari
                 IF(trim(adjustl(clvari)).eq.'ESTWST') &
                          ig_invert(jf)=ig_invert(jf)+2
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'CHECKIN')THEN
!     * Get field integral flag
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
                 READ(clvari,FMT = 2010) clind, clequa,  &
                      ntinpflx(ig_number_field(jf))
                 IF(clind .NE. 'INT') GO TO 235
                 IF (ntinpflx(ig_number_field(jf)) .eq. 1)  &
                      lsurf(ig_number_field(jf))= .TRUE. 
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'CHECKOUT')  &
                      THEN
!     *Get field integral flag
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
                 READ(clvari,FMT = 2010) clind, clequa,  &
                      ntoutflx(ig_number_field(jf))
                 IF(clind .NE. 'INT') GO TO 235
                 IF (ntoutflx(ig_number_field(jf)) .eq. 1)  &
                      lsurf(ig_number_field(jf))= .TRUE.
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'NOINTERP')  &
                      THEN
!     * No interpolation case
                 CONTINUE
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'REVERSE')  &
                      THEN
                 ig_reverse(jf) = 1
!     * Get lat-lon ordering for final fields
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
                 cxordaf(ig_number_field(jf)) = clvari
                 IF(trim(adjustl(clvari)).eq.'NORSUD') &
                          ig_reverse(jf)=ig_reverse(jf)+1
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
                 cyordaf(ig_number_field(jf)) = clvari
                 IF(trim(adjustl(clvari)).eq.'ESTWST') &
                          ig_reverse(jf)=ig_reverse(jf)+2
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'EXTRAP')THEN
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get extrapolation method
                 cextmet(ig_number_field(jf)) = clvari
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
!     * Get number of neighbors used in extrapolation
!     If extrapolation method is NINENN, next variable is the MINIMUM
!     number of neighbors required (among the 8 closest) to perform
!     the extrapolation (cannot be greater than 4 for convergence). 
!     In case it is WEIGHT, it is the MAXIMUM number
!     of neighbors required by the extrapolation operation.
!     
                 READ(clvari,FMT = 2003) neighbor(ig_number_field(jf))
                 IF (cextmet(ig_number_field(jf)) .EQ. 'NINENN' .AND.  &
                      neighbor(ig_number_field(jf)) .GT. 4) THEN
                    neighbor(ig_number_field(jf))=4
                    IF (mpi_rank_global == 0) THEN
                        WRITE(UNIT = nulprt1,FMT = *) '        ***WARNING***'
                        WRITE(UNIT = nulprt1,FMT = *)  &
                           'For EXTRAP/NINENN extrapolation' 
                        WRITE(UNIT = nulprt1,FMT = *)  &
                           'the number of neighbors has been set to 4'
                    ENDIF
                 ENDIF
!     * If choice is NINENN, read one more data
                 IF (cextmet(ig_number_field(jf)) .EQ. 'NINENN') THEN
                    CALL parse(clline, clvari, 3, jpeighty, ilen)
!     * Get NINENN weights read/write flag
                    READ(clvari,FMT = 2005) niwtn(ig_number_field(jf))
                 ENDIF
!     * If choice is WEIGHT, read more data
                 IF (cextmet(ig_number_field(jf)) .EQ. 'WEIGHT') THEN
                    CALL parse(clline, clvari, 3, jpeighty, ilen)
!     * Get file name for grid mapping
                    cgrdext(ig_number_field(jf)) = clvari
                    CALL parse(clline, clvari, 4, jpeighty, ilen)
!     * Get related logical unit 
                    READ(clvari,FMT = 2005) nluext(ig_number_field(jf))
                 ENDIF 
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'INTERP')THEN
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get interpolation method
                 cintmet(ig_number_field(jf)) = clvari
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
!     * Get source grid type
                 cgrdtyp(ig_number_field(jf)) = clvari
                 CALL parse(clline, clvari, 3, jpeighty, ilen)
!     * Get field type (scalar or vector)
                 cfldtyp(ig_number_field(jf)) = clvari
!     * If interpolation uses ANAIS(G-M), read in more data
                 IF (cintmet(ig_number_field(jf)) .EQ. 'SURFMESH') THEN
                    CALL parse(clline, clvari, 6, jpeighty, ilen)
!     * Get Anaism weights read/write flag
                    READ(clvari,FMT = 2005) niwtm(ig_number_field(jf))
                 ENDIF 
                 IF (cintmet(ig_number_field(jf)) .EQ. 'GAUSSIAN') THEN
                    CALL parse(clline, clvari, 6, jpeighty, ilen)
!     * Read variance multiplicator for gaussian weights
                    READ(clvari,FMT = 2006) varmul(ig_number_field(jf))
                    CALL parse(clline, clvari, 7, jpeighty, ilen)
!     * Get Anaisg weights read/write flag
                    READ(clvari,FMT = 2005) niwtg(ig_number_field(jf))
                 ENDIF
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'MAPPING') THEN
!* Get mapping filename
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
                 cmap_file(ig_number_field(jf)) = trim(clvari)
!* Get mapping location and/or mapping optimization; src (default), dst; bfb (default), sum, opt
                 cmaptyp(ig_number_field(jf)) = 'src'
                 cmapopt(ig_number_field(jf)) = 'bfb'
                 do idum = 2,3
                    CALL parse(clline, clvari, idum, jpeighty, ilen)
                    if (ilen > 0) then
                       if (trim(clvari) == 'src' .or. trim(clvari) == 'dst') then
                          cmaptyp(ig_number_field(jf)) = trim(clvari)
                       elseif (trim(clvari) == 'opt' .or. trim(clvari) == 'bfb' &
                          .or. trim(clvari) == 'sum') then
                          cmapopt(ig_number_field(jf)) = trim(clvari)
                       else
                          call prtout ('ERROR in namcouple mapping argument',jf,1)
                          IF (mpi_rank_global == 0) THEN
                              WRITE(nulprt1,*) 'ERROR in namcouple mapping argument ',TRIM(clvari)
                              WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                              WRITE (nulprt1,'(a)') ' error = STOP in inipar cmaptyp or loc'
                              CALL prism_sys_flush(nulprt1)
                          ENDIF
                          call prism_sys_abort()
                       endif
                    endif
                 enddo
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'SCRIPR') THEN
!* Get Scrip remapping method
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
                 READ(clvari,FMT = 2009) cmap_method(ig_number_field(jf))
!* Get source grid type
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
                 READ(clvari,FMT = 2009) cgrdtyp(ig_number_field(jf))
                 IF (cmap_method(ig_number_field(jf)) .eq. 'BICUBIC'  &
                    .and. cgrdtyp(ig_number_field(jf)) .ne. 'LR' &
                    .and. cgrdtyp(ig_number_field(jf)) .ne. 'D') THEN
                     IF (mpi_rank_global == 0) THEN
                         WRITE (UNIT = nulprt1,FMT = *) '    '
                     ENDIF
                    CALL prtout &
                      ('ERROR in namcouple for type of field', jf, 1)
                    IF (mpi_rank_global == 0) THEN
                        WRITE (UNIT = nulprt1,FMT = *)  &
                           'BICUBIC interpolation cannot be used if grid is not LR or D'
                        WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                        WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                        CALL prism_sys_flush(nulprt1)
                    ENDIF
                    CALL PRISM_SYS_ABORT() 
                 ENDIF
                 IF (cmap_method(ig_number_field(jf)) .eq. 'BILINEAR'  &
                    .and. cgrdtyp(ig_number_field(jf)) .ne. 'LR' &
                    .and. cgrdtyp(ig_number_field(jf)) .ne. 'D') THEN
                     IF (mpi_rank_global == 0) THEN
                         WRITE (UNIT = nulprt1,FMT = *) '    '
                     ENDIF
                    CALL prtout &
                      ('ERROR in namcouple for type of field', jf, 1)
                    IF (mpi_rank_global == 0) THEN
                        WRITE (UNIT = nulprt1,FMT = *)  &
                           'BILINEAR interpolation cannot be used if grid is not LR or D'
                        WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                        WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                        CALL prism_sys_flush(nulprt1)
                    ENDIF
                    CALL PRISM_SYS_ABORT() 
                 ENDIF
!* Get field type (scalar/vector)
                 CALL parse(clline, clvari, 3, jpeighty, ilen)
                 READ(clvari,FMT = 2009) cfldtype(ig_number_field(jf))
                 IF(cfldtype(ig_number_field(jf)) .NE. 'SCALAR' .AND.  &
                    cfldtype(ig_number_field(jf)) .NE. 'VECTOR' .AND. &
                    cfldtype(ig_number_field(jf)) .NE. 'VECTOR_I' .AND. &
                    cfldtype(ig_number_field(jf)) .NE. 'VECTOR_J') THEN
                     IF (mpi_rank_global == 0) THEN
                         WRITE (UNIT = nulprt1,FMT = *) '    '
                     ENDIF
                    CALL prtout &
                      ('ERROR in namcouple for type of field', jf, 1)
                    IF (mpi_rank_global == 0) THEN
                        WRITE (UNIT = nulprt1,FMT = *)  &
                           '==> must be SCALAR, VECTOR_I or VECTOR_J'
                        WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                        WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                        CALL prism_sys_flush(nulprt1)
                    ENDIF
                    CALL PRISM_SYS_ABORT()
                 ENDIF
!* Get restriction type for SCRIP search
                 CALL parse(clline, clvari, 4, jpeighty, ilen)
                 READ(clvari,FMT = 2009) crsttype(ig_number_field(jf))
                 IF (cgrdtyp(ig_number_field(jf)) .EQ. 'D') THEN
                    IF (cmap_method(ig_number_field(jf)) .EQ. 'BILINEAR' .or. &
                        cmap_method(ig_number_field(jf)) .EQ. 'BICUBIC') THEN
                        IF (crsttype(ig_number_field(jf)) .NE. 'LATITUDE') THEN
                            IF (mpi_rank_global == 0) THEN
                                WRITE (UNIT = nulprt1,FMT = *) '    '
                            ENDIF
                            CALL prtout('ERROR in namcouple for restriction of field',jf,1)
                            IF (mpi_rank_global == 0) THEN
                                WRITE (UNIT = nulprt1,FMT = *)  &
                                   '==> LATITUDE must be chosen for reduced grids (D)'
                                WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                                WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                                CALL prism_sys_flush(nulprt1)
                            ENDIF
                            CALL PRISM_SYS_ABORT()
                        ELSE  
                            crsttype(ig_number_field(jf)) = 'REDUCED'
                        ENDIF
                    ENDIF
                 ENDIF

                 IF(crsttype(ig_number_field(jf)) .NE. 'LATITUDE' .AND.  &
                    crsttype(ig_number_field(jf)) .NE. 'LATLON' .AND. &
                    crsttype(ig_number_field(jf)) .NE. 'REDUCED') THEN
                     IF (mpi_rank_global == 0) THEN
                         WRITE (UNIT = nulprt1,FMT = *) '    '
                     ENDIF
                    CALL prtout('ERROR in namcouple for restriction of field',jf,1)
                    IF (mpi_rank_global == 0) THEN
                        WRITE (UNIT = nulprt1,FMT = *) '==> must be LATITUDE or LATLON'
                        WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                        WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                        CALL prism_sys_flush(nulprt1)
                    ENDIF
                    CALL PRISM_SYS_ABORT()
                 ENDIF
!*
!* Get number of search bins for SCRIP search
                 CALL parse(clline, clvari, 5, jpeighty, ilen)
                 READ(clvari,FMT = 2003) nbins(ig_number_field(jf))
!* Get normalize option for CONSERV
                 IF (cmap_method(ig_number_field(jf)) .EQ. 'CONSERV') THEN
                    CALL parse(clline, clvari, 6, jpeighty, ilen)
                    READ(clvari,FMT = 2009)cnorm_opt(ig_number_field(jf))
                    IF (cnorm_opt(ig_number_field(jf)) .NE. 'FRACAREA' .AND. &
      		        cnorm_opt(ig_number_field(jf)) .NE. 'DESTAREA' .AND.  &
                        cnorm_opt(ig_number_field(jf)) .NE. 'FRACNNEI') THEN
                        IF (mpi_rank_global == 0) THEN
                            WRITE (UNIT = nulprt1,FMT = *) '    '
                        ENDIF
                        CALL prtout &
                          ('ERROR in namcouple for normalize option of field',jf,1)
                        IF (mpi_rank_global == 0) THEN
                            WRITE (UNIT = nulprt1, FMT = *)  &
                               '==> must be FRACAREA, DESTAREA, or FRACNNEI'
                            WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                            WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                            CALL prism_sys_flush(nulprt1)
                        ENDIF
                        CALL PRISM_SYS_ABORT()
                    ENDIF
!* Get order of remapping for CONSERV
                    CALL parse(clline, clvari, 7, jpeighty, ilen)
                    IF (ilen .LE. 0) THEN
                        IF (mpi_rank_global == 0) THEN
                            WRITE (UNIT = nulprt1,FMT = *) '    '
                        ENDIF
                        CALL prtout ('ERROR in namcouple for CONSERV for field',jf,1)
                        IF (mpi_rank_global == 0) THEN
                            WRITE (UNIT = nulprt1,FMT = *)  &
                               '==> SECOND or FIRST must be indicated at end of line'
                            WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                            WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                            CALL prism_sys_flush(nulprt1)
                        ENDIF
                        CALL PRISM_SYS_ABORT()
                    ENDIF
                    READ(clvari,FMT = 2009) corder(ig_number_field(jf))
                 ELSE
                     cnorm_opt(ig_number_field(jf))='NONORM'
                 ENDIF
!* Get number of neighbours for DISTWGT and GAUSWGT
                 IF (cmap_method(ig_number_field(jf)) .EQ. 'DISTWGT' .or. &
                     cmap_method(ig_number_field(jf)) .EQ. 'GAUSWGT') THEN
                    CALL parse(clline, clvari, 6, jpeighty, ilen)
                    IF (ilen .LE. 0) THEN
                        IF (mpi_rank_global == 0) THEN
                            WRITE (UNIT = nulprt1,FMT = *) '    '
                        ENDIF
                        CALL prtout('ERROR in namcouple for field',jf,1)
                        IF (mpi_rank_global == 0) THEN
                            WRITE (UNIT = nulprt1,FMT = *)  &
                               '==> Number of neighbours must be indicated on the line'
                            WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                            WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                            CALL prism_sys_flush(nulprt1)
                        ENDIF
                       CALL PRISM_SYS_ABORT()
                    ELSE
                       READ(clvari,FMT=2003)nscripvoi(ig_number_field(jf))
                    ENDIF 
                 ENDIF
!* Get gaussian variance for GAUSWGT
                 IF (cmap_method(ig_number_field(jf)) .EQ. 'GAUSWGT') THEN
                    CALL parse(clline, clvari, 7, jpeighty, ilen)
                    IF (ilen .LE. 0) THEN
                        IF (mpi_rank_global == 0) THEN
                            WRITE (UNIT = nulprt1,FMT = *) '    '
                        ENDIF
                       CALL prtout('ERROR in namcouple for GAUSWGT for field',jf,1)
                       IF (mpi_rank_global == 0) THEN
                           WRITE (UNIT = nulprt1,FMT = *)  &
                              '==> Variance must be indicated at end of line'
                           WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                           WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                           CALL prism_sys_flush(nulprt1)
                       ENDIF
                       CALL PRISM_SYS_ABORT()
                    ELSE
                       READ(clvari,FMT=2006) varmul(ig_number_field(jf))
                    ENDIF
                 ENDIF
!*Get associated file name and information about rotation to cartesien
                 IF (cfldtype(ig_number_field(jf))=='VECTOR_I' .or. &
                     cfldtype(ig_number_field(jf))=='VECTOR_J') THEN
                    IF(cmap_method(ig_number_field(jf)) .EQ. 'DISTWGT') lastplace=7
                    IF(cmap_method(ig_number_field(jf)) .EQ. 'GAUSWGT') lastplace=8
                    IF(cmap_method(ig_number_field(jf)) .EQ. 'BILINEAR') lastplace=6
                    IF(cmap_method(ig_number_field(jf)) .EQ. 'BICUBIC') lastplace=6
                    IF(cmap_method(ig_number_field(jf)) .EQ. 'CONSERV') lastplace=8
                    CALL parse(clline, clvari, lastplace, jpeighty, ilen)
                    IF (ilen .le. 0) THEN
                        IF (mpi_rank_global == 0) THEN
                            WRITE (UNIT = nulprt1,FMT = *) ' '
                            WRITE (UNIT = nulprt1,FMT = *)  &
                               '==> A field associated must be indicated'
                            WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                            WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                            CALL prism_sys_flush(nulprt1)
                        ENDIF
                       CALL PRISM_SYS_ABORT()
                    ENDIF
                    cg_assoc_input_field(ig_number_field(jf))=clvari
!*Rotation?
                    CALL parse(clline, clvari, lastplace+1, jpeighty, ilen) 
                    IF (ilen .le. 0) THEN
                       lrotate(ig_number_field(jf)) = .false.
                    ELSEIF(clvari .le. 'PROJCART') THEN
                       lrotate(ig_number_field(jf)) = .true.
                       IF (mpi_rank_global == 0) THEN
                           WRITE (UNIT = nulprt1,FMT = *) &
                              'rotation to cartesian for field : ', jf
                       ENDIF
                    ELSE
                        IF (mpi_rank_global == 0) THEN
                            WRITE (UNIT = nulprt1,FMT = *) ' '
                        ENDIF
                        CALL prtout &
                         ('ERROR in namcouple for vector in SCRIPR for field',jf,1) 
                        IF (mpi_rank_global == 0) THEN
                            WRITE (UNIT = nulprt1,FMT = *) &
                               'must be PROJCART or nothing' 
                            WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                            WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                            CALL prism_sys_flush(nulprt1)
                        ENDIF
                       CALL PRISM_SYS_ABORT() 
                    ENDIF
                 END IF

              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'FILLING')  &
                      THEN
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get data file name (used to complete the initial field array)
                 cfilfic(ig_number_field(jf)) = clvari
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
!     * Get logical unit connected to previous file
                 READ(clvari,FMT = 2005) nlufil(ig_number_field(jf))
                 CALL parse(clline, clvari, 3, jpeighty, ilen)
!     * Get filling method
                 cfilmet(ig_number_field(jf)) = clvari
!     * If current field is SST
                 IF(cfilmet(ig_number_field(jf))(4:6) .EQ. 'SST') THEN
                    CALL parse(clline, clvari, 4, jpeighty, ilen)
!     * Get flag for coast mismatch correction
                    READ(clvari,FMT = 2005) nfcoast
                    IF (cfilmet(ig_number_field(jf))(1:3) .EQ. 'SMO') &
                        THEN
                        CALL parse(clline, clvari, 5, jpeighty, ilen)
!     * Get field name for flux corrective term 
                        cfldcor = clvari
                        CALL parse(clline, clvari, 6, jpeighty, ilen)
!     * Get logical unit used to write flux corrective term
                        READ(clvari,FMT = 2005) nlucor
                    ENDIF
                 ENDIF 
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'CONSERV')  &
                      THEN            
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get conservation method
                 cconmet(ig_number_field(jf)) = clvari
                 lsurf(ig_number_field(jf)) = .TRUE.
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'REDGLO') THEN
!     * Get extrapolation flag to go from reduced to global gaussian grid
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
                 cmskrd(ig_number_field(jf)) = clvari
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'GLORED') THEN
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
!     * Get number of neighbors used in EXTRAP/NINENN extrapolation always
!     performed within GLORED (cannot be greater than 4 for convergence).
                 READ(clvari,FMT = 2003) neighborg(ig_number_field(jf))
                 CALL parse(clline, clvari, 3, jpeighty, ilen)
                 IF (neighborg(ig_number_field(jf)) .GT. 4) THEN
                    neighborg(ig_number_field(jf))=4
                    IF (mpi_rank_global == 0) THEN
                        WRITE(UNIT = nulprt1,FMT = *) '        ***WARNING***'
                        WRITE(UNIT = nulprt1,FMT = *)  &
                           'For EXTRAP/NINENN extrapolation in GLORED' 
                        WRITE(UNIT = nulprt1,FMT = *)  &
                           'the number of neighbors has been set to 4'
                    ENDIF
                 ENDIF
!     * Get EXTRAP/NINENN weights read/write flag
                 READ(clvari,FMT = 2005) niwtng(ig_number_field(jf))
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'CORRECT')  &
                      THEN
!     * Get flux correction parameters
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get main field multiplicative coefficient
                 READ(clvari,FMT = 2006) afldcoef(ig_number_field(jf))
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
!     * Get number of auxilary fields in correction formula
                 READ(clvari,FMT = 2003) ncofld (ig_number_field(jf))
!     * Read auxilary field parameters
                 icofld = ncofld(ig_number_field(jf))
                 DO 280 jc = 1, icofld
                    READ (UNIT = nulin,FMT = 2002) clline   
                    CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get symbolic names for additional fields
                    ccofld(jc,ig_number_field(jf)) = clvari
                    CALL parse(clline, clvari, 2, jpeighty, ilen)
!     * Get multiplicative coefficients for  additional fields
                    READ(clvari,FMT = 2006)  &
                         acocoef (jc,ig_number_field(jf))
                    CALL parse(clline, clvari, 3, jpeighty, ilen)
!     * Get file names for external data files 
                    ccofic(jc,ig_number_field(jf)) = clvari
!     * Get related logical units 
                    CALL parse(clline, clvari, 4, jpeighty, ilen)
                    READ(clvari,FMT = 2005)  &
                         nludat(jc,ig_number_field(jf))
 280             CONTINUE
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'BLASOLD')THEN
!     * Get linear combination parameters for initial fields
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get main field multiplicative coefficient
                 READ(clvari,FMT = 2006) afldcobo(ig_number_field(jf))
                 DO 290 jc = 1, nbofld(ig_number_field(jf))
                    READ (UNIT = nulin,FMT = 2002) clline   
                    CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get symbolic names for additional fields
                    cbofld(jc,ig_number_field(jf)) = clvari
                    CALL parse(clline, clvari, 2, jpeighty, ilen)
!     * Get multiplicative coefficients for  additional fields
                    READ(clvari,FMT = 2006)  &
                         abocoef (jc,ig_number_field(jf))
 290             CONTINUE
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'BLASNEW')THEN
!     * Get linear combination parameters for final fields
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get main field multiplicative coefficient
                 READ(clvari,FMT = 2006) afldcobn(ig_number_field(jf))
                 DO 291 jc = 1, nbnfld(ig_number_field(jf))
                    READ (UNIT = nulin,FMT = 2002) clline   
                    CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get symbolic names for additional fields
                    cbnfld(jc,ig_number_field(jf)) = clvari
                    CALL parse(clline, clvari, 2, jpeighty, ilen)
!     * Get multiplicative coefficients for  additional fields
                    READ(clvari,FMT = 2006)  &
                         abncoef (jc,ig_number_field(jf))
 291             CONTINUE
!     * Get fields to restore subgrid variability 
              ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'SUBGRID')THEN
                 CALL parse(clline, clvari, 1, jpeighty, ilen)
!     * Get file name for subgrid interpolation
                 cgrdsub(ig_number_field(jf)) = clvari
                 CALL parse(clline, clvari, 2, jpeighty, ilen)
!     * Get related logical unit 
                 READ(clvari,FMT = 2005) nlusub(ig_number_field(jf))
                 CALL parse(clline, clvari, 5, jpeighty, ilen)
!     * Get type of subgrid interpolation (solar or non solar flux)
                 ctypsub(ig_number_field(jf)) = clvari
                 CALL parse(clline, clvari, 6, jpeighty, ilen)
!     * Get additional field name on coarse grid
                 cfldcoa(ig_number_field(jf)) = clvari
                 CALL parse(clline, clvari, 7, jpeighty, ilen)
!     * Get additional field name on fine grid
                 cfldfin(ig_number_field(jf)) = clvari
                 IF (ctypsub(ig_number_field(jf)) .EQ. 'NONSOLAR') THEN
                    CALL parse(clline, clvari, 8, jpeighty, ilen)
!     * Get coupling ratio on coarse grid
                    cdqdt(ig_number_field(jf)) = clvari
                 ENDIF 
              ELSE 
                  IF (mpi_rank_global == 0) THEN
                      WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
                      WRITE (UNIT = nulprt1,FMT = *) &
                         ' Type of analysis not implemented yet '
                      WRITE (UNIT = nulprt1,FMT = *)  &
                         ' The analysis required in OASIS is :'
                      WRITE (UNIT = nulprt1,FMT = *) ' canal = ',  &
                         canal(ja,ig_number_field(jf))
                      WRITE (UNIT = nulprt1,FMT = *)  &
                         ' with ja = ', ja, ' jf = ', jf
                      WRITE (UNIT = nulprt1,FMT = *) ' '
                      WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                      WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                      CALL prism_sys_flush(nulprt1)
                  ENDIF
                 CALL prism_sys_abort()
              ENDIF
 270       CONTINUE
       ENDIF

!* End of loop on NoF
 
 240  CONTINUE

!*Get the associated number file for case vector

      DO jf = 1, ig_total_nfield
        IF(ig_number_field(jf) > 0) then
         IF (cfldtype(ig_number_field(jf))=='VECTOR_I' .or. &
              cfldtype(ig_number_field(jf))=='VECTOR_J') THEN
            DO jff = 1, ig_total_nfield
              IF (ig_number_field(jff) .gt. 0) then
                  IF(cnaminp(ig_number_field(jff)) .eq. &
                      cg_assoc_input_field(ig_number_field(jf)))THEN
                      ig_assoc_input_field(ig_number_field(jf))= &
                          ig_number_field(jff)
                      exit
                  ENDIF
              ENDIF
            ENDDO

!*Verify if interpolations are the same for the 2 components of the vector field

            IF(cmap_method(ig_number_field(jf)) .ne.  &
                 cmap_method(ig_assoc_input_field( &
                 ig_number_field(jf)))) THEN
                IF (mpi_rank_global == 0) THEN
                    WRITE (UNIT = nulprt1,FMT = *)  &
                       'Interpolations must be the same for the 2'
                    WRITE (UNIT = nulprt1,FMT = *) &
                       'components in vector case'
                    WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
                    WRITE (nulprt1,'(a)') ' error = STOP in inipar'
                    CALL prism_sys_flush(nulprt1)
                ENDIF
               CALL PRISM_SYS_ABORT()
            END IF
         ENDIF
       ENDIF
      ENDDO

!* Minimum coupling period

      ig_total_frqmin = minval(ig_freq)

!* Formats

 2001 FORMAT(A9)
 2002 FORMAT(A80)
 2003 FORMAT(I4)
 2004 FORMAT(I8)
 2005 FORMAT(I2)
 2006 FORMAT(E15.6)
 2008 FORMAT(A2,I4)
 2009 FORMAT(A8)
 2010 FORMAT(A3,A1,I2)
 2011 FORMAT(A3,A1,I8)

!*    3. Printing
!        --------
      IF (mpi_rank_global == 0) THEN
!* Warning: no indentation for the next if (nightmare ...)
      IF (nlogprt .GE. 0) THEN 
      DO 310 jf = 1, ig_total_nfield
         IF (ig_total_state(jf) .eq. ip_exported ) THEN
            cl_print_state = 'EXPORTED'
         ELSEIF (ig_total_state(jf) .eq. ip_ignored ) THEN
            cl_print_state = 'IGNORED'
         ELSEIF (ig_total_state(jf) .eq. ip_ignout ) THEN
            cl_print_state = 'IGNOUT'
         ELSEIF (ig_total_state(jf) .eq. ip_expout ) THEN 
            cl_print_state = 'EXPOUT'
         ELSEIF (ig_total_state(jf) .eq. ip_input ) THEN 
            cl_print_state = 'INPUT'
         ELSEIF (ig_total_state(jf) .eq. ip_output ) THEN 
            cl_print_state = 'OUTPUT'
         ELSEIF (ig_total_state(jf) .eq. ip_auxilary ) THEN 
            cl_print_state = 'AUXILARY'
         ENDIF
         IF (ig_local_trans(jf) .eq. ip_instant) THEN
            cl_print_trans = 'INSTANT'
         ELSEIF (ig_local_trans(jf) .eq. ip_average) THEN
             cl_print_trans = 'AVERAGE'
         ELSEIF (ig_local_trans(jf) .eq. ip_accumul) THEN
            cl_print_trans = 'ACCUMUL'
         ELSEIF (ig_local_trans(jf) .eq. ip_min) THEN
            cl_print_trans = 'T_MIN'
         ELSEIF (ig_local_trans(jf) .eq. ip_max) THEN
            cl_print_trans = 'T_MAX'   
         ENDIF
!* Local indexes
      IF (.NOT. lg_state(jf)) THEN
         ilab = ig_numlab(jf)
         IF (mpi_rank_global == 0) THEN
             WRITE (UNIT = nulprt1,FMT = 3001) jf
             WRITE (UNIT = nulprt1,FMT = 3002)
             WRITE (UNIT = nulprt1,FMT = 3003)
             WRITE (UNIT = nulprt1,FMT = 3004)
         ENDIF
         IF (ig_total_state(jf) .eq. ip_input .or.  &
              ig_total_state(jf) .eq. ip_output) THEN
             IF (mpi_rank_global == 0) THEN
                 WRITE (UNIT = nulprt1,FMT = 3121) &
                    cg_input_field(jf), cg_output_field(jf), cfldlab(ilab),  &
                    ig_freq(jf), cl_print_trans, &
                    cl_print_state, ig_total_ntrans(jf)
             ENDIF
         ELSE  
             IF (mpi_rank_global == 0) THEN
                 WRITE (UNIT = nulprt1,FMT = 3116) &
                    cg_input_field(jf), cg_output_field(jf), cfldlab(ilab),  &
                    ig_freq(jf), cl_print_trans, ig_total_nseqn(jf),  &
                    ig_lag(jf), cl_print_state, ig_total_ntrans(jf)
             ENDIF
         ENDIF
      ELSE
         ilab = numlab(ig_number_field(jf))
         ifcb = len_trim(cficbf(ig_number_field(jf)))
         ifca = len_trim(cficaf(ig_number_field(jf)))
         IF (mpi_rank_global == 0) THEN
             WRITE (UNIT = nulprt1,FMT = 3001) jf
             WRITE (UNIT = nulprt1,FMT = 3002)
             WRITE (UNIT = nulprt1,FMT = 3003)
             WRITE (UNIT = nulprt1,FMT = 3004) 
         ENDIF
         IF (cchan.EQ.'MPI2' .OR. cchan.EQ.'MPI1'  &
             .OR. cchan.EQ.'GSIP') THEN
             IF (mpi_rank_global == 0) THEN
                 WRITE (UNIT = nulprt1,FMT = 3005) &
                    TRIM(cnaminp(ig_number_field(jf))),  &
                    TRIM(cnamout(ig_number_field(jf))), &
                    cfldlab(ilab),  &
                    nfexch(ig_number_field(jf)), &
                    nseqn(ig_number_field(jf)), &
                    ig_lag(jf), &
                    cl_print_state, &
                    ig_ntrans(ig_number_field(jf)),  &
                    cparal(ig_number_field(jf))
             ENDIF
         ELSE
             IF (mpi_rank_global == 0) THEN
                 WRITE (UNIT = nulprt1,FMT = 3115) &
                    TRIM(cnaminp(ig_number_field(jf))),  &
                    TRIM(cnamout(ig_number_field(jf))), &
                    cfldlab(ilab),  &
                    nfexch(ig_number_field(jf)), &
                    nseqn(ig_number_field(jf)),  &
                    cstate(ig_number_field(jf)),  &
                    ig_ntrans(ig_number_field(jf))
             ENDIF
         ENDIF
     ENDIF
!* Warning: no indentation for the next if (nightmare ...)
        IF (nlogprt .GE. 0) THEN
!* Warning: no indentation for the next if (nightmare ...)            
        IF (.not. lg_state(jf)) THEN
           IF (ig_total_state(jf) .eq. ip_ignored .or.  &
               ig_total_state(jf) .eq. ip_ignout ) THEN
              WRITE (UNIT = nulprt1,FMT = 3117) cg_restart_file(jf)
           ELSEIF (ig_total_state(jf) .eq. ip_input) THEN
              WRITE (UNIT = nulprt1,FMT = 3118) cg_input_file(jf)
           ENDIF
        ELSE
           IF (ig_total_state(jf) .eq. ip_exported .or.  &
                ig_total_state(jf) .eq. ip_expout .or.  &
                ig_total_state(jf) .eq. ip_auxilary ) &
                WRITE (UNIT = nulprt1,FMT = 3117) cg_restart_file(jf)
!* Warning: no indentation for the next if (nightmare ...)           
        WRITE (UNIT = nulprt1,FMT = 3007) &
            csper(ig_number_field(jf)), nosper(ig_number_field(jf)),  &
            ctper(ig_number_field(jf)), notper(ig_number_field(jf))
        WRITE (UNIT = nulprt1,FMT = 3008) &
            cficbf(ig_number_field(jf))(1:ifcb)//cglonsuf,  &
            cficbf(ig_number_field(jf))(1:ifcb)//cglatsuf, &
            cficbf(ig_number_field(jf))(1:ifcb)//cmsksuf,  &
            cficbf(ig_number_field(jf))(1:ifcb)//csursuf, &
            cficaf(ig_number_field(jf))(1:ifca)//cglonsuf,  &
            cficaf(ig_number_field(jf))(1:ifca)//cglatsuf, &
            cficaf(ig_number_field(jf))(1:ifca)//cmsksuf,  &
            cficaf(ig_number_field(jf))(1:ifca)//csursuf
        WRITE (UNIT = nulprt1,FMT = 3009) 
        WRITE (UNIT = nulprt1,FMT = 3010)
        DO 320 ja = 1, ig_ntrans(ig_number_field(jf))
          WRITE (UNIT = nulprt1,FMT = 3011) ja,  &
                canal(ja,ig_number_field(jf))
          IF (canal(ja,ig_number_field(jf)) .EQ. 'MASK') THEN
              WRITE(UNIT = nulprt1,FMT = 3012)  &
                  amskval(ig_number_field(jf))
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'MASKP') THEN
              WRITE(UNIT = nulprt1,FMT = 3042)  &
                    amskvalnew(ig_number_field(jf))
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'MOZAIC') THEN
              WRITE(UNIT = nulprt1,FMT = 3013)  &
                    cgrdmap(ig_number_field(jf)),  &
                    nlumap(ig_number_field(jf)), &
                    nmapfl(ig_number_field(jf)),  &
                    nmapvoi(ig_number_field(jf))
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'INVERT') THEN
              WRITE(UNIT = nulprt1,FMT = 3014)  &
                    cxordbf(ig_number_field(jf))
              WRITE(UNIT = nulprt1,FMT = 3015)  &
                   cyordbf(ig_number_field(jf))
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'REVERSE') THEN
              WRITE(UNIT = nulprt1,FMT = 3016)  &
                    cxordaf(ig_number_field(jf))
              WRITE(UNIT = nulprt1,FMT = 3017)  &
                   cyordaf(ig_number_field(jf))
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'EXTRAP') THEN
              WRITE(UNIT = nulprt1,FMT = 3018)  &
                    cextmet(ig_number_field(jf)),  &
                    neighbor(ig_number_field(jf))
              IF (cextmet(ig_number_field(jf)) .EQ. 'WEIGHT') THEN 
                  WRITE(UNIT = nulprt1,FMT = 3019)  &
                      cgrdext(ig_number_field(jf)),  &
                      nluext(ig_number_field(jf)),  &
                      nextfl(ig_number_field(jf))
              ELSE IF (cextmet(ig_number_field(jf)) .EQ. 'NINENN') THEN 
                  WRITE(UNIT = nulprt1,FMT = 3038)  &
                      niwtn(ig_number_field(jf)),  &
                      nninnfl(ig_number_field(jf))
              ENDIF
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'INTERP') THEN
              WRITE(UNIT = nulprt1,FMT = 3020)  &
                    cintmet(ig_number_field(jf)),  &
                    cgrdtyp(ig_number_field(jf)), &
                    cfldtyp(ig_number_field(jf))
              IF (cintmet(ig_number_field(jf)) .EQ. 'SURFMESH') THEN 
                  WRITE(UNIT = nulprt1,FMT = 3021)  &
                      naismfl(ig_number_field(jf)),  &
                      naismvoi(ig_number_field(jf)),  &
                      niwtm(ig_number_field(jf))
              ENDIF 
              IF (cintmet(ig_number_field(jf)) .EQ. 'GAUSSIAN') THEN 
                  WRITE(UNIT = nulprt1,FMT = 3021)  &
                      naisgfl(ig_number_field(jf)),  &
                      naisgvoi(ig_number_field(jf)),  &
                      niwtg(ig_number_field(jf))
                  WRITE(UNIT = nulprt1,FMT = 3022)  &
                       varmul(ig_number_field(jf))
              ENDIF
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'MAPPING') THEN
              write(UNIT = nulprt1,FMT = 3048) &
                    trim(cmap_file(ig_number_field(jf))), &
                    trim(cmaptyp(ig_number_field(jf))), &
                    trim(cmapopt(ig_number_field(jf)))
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'SCRIPR') THEN
              WRITE(UNIT = nulprt1,FMT = 3045)  &
                    cmap_method(ig_number_field(jf)),  &
                    cfldtype(ig_number_field(jf)),  &
                    cnorm_opt(ig_number_field(jf)), &
                    crsttype(ig_number_field(jf)),  &
                    nbins(ig_number_field(jf))
              IF (cmap_method(ig_number_field(jf)) .EQ. 'CONSERV') THEN 
                  WRITE(UNIT = nulprt1,FMT = 3046)  &
                      corder(ig_number_field(jf))
              ENDIF  
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'FILLING') THEN
              WRITE(UNIT = nulprt1,FMT = 3023)  &
                    cfilfic(ig_number_field(jf)),  &
                    nlufil(ig_number_field(jf)), &
                    cfilmet(ig_number_field(jf))
              IF(cfilmet(ig_number_field(jf))(1:6) .EQ. 'SMOSST') &
                  WRITE(UNIT = nulprt1,FMT = 3024)  &
                  nfcoast, cfldcor, nlucor
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'CONSERV') THEN            
              WRITE(UNIT = nulprt1,FMT = 3025)  &
                    cconmet(ig_number_field(jf))
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'REDGLO') THEN
              WRITE(UNIT = nulprt1,FMT = 3026)  &
                    ntronca(ig_number_field(jf)),  &
                    cmskrd(ig_number_field(jf))
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'CORRECT') THEN
              WRITE(UNIT = nulprt1,FMT = 3027)  &
                    trim(cnamout(ig_number_field(jf))),  &
                    afldcoef(ig_number_field(jf))
              WRITE(UNIT = nulprt1,FMT=3028) ncofld(ig_number_field(jf))
              icofld = ncofld(ig_number_field(jf))
              DO 330 jc = 1, icofld
                WRITE(UNIT = nulprt1,FMT = 3029)  &
                    ccofic(jc,ig_number_field(jf)), &
                      nludat(jc,ig_number_field(jf))
                WRITE (UNIT = nulprt1,FMT = 3030)  &
                    ccofld(jc,ig_number_field(jf)),  &
                     acocoef(jc,ig_number_field(jf))
 330          CONTINUE
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'BLASOLD') THEN
              WRITE(UNIT = nulprt1,FMT = 3027)  &
                    trim(cnaminp(ig_number_field(jf))),  &
                    afldcobo(ig_number_field(jf))
              WRITE(UNIT = nulprt1,FMT=3028) nbofld(ig_number_field(jf))
              DO 340 jc = 1, nbofld(ig_number_field(jf))
                WRITE (UNIT = nulprt1,FMT = 3030)  &
                    cbofld(jc,ig_number_field(jf)),  &
                      abocoef (jc,ig_number_field(jf))
 340          CONTINUE
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'BLASNEW') THEN
              WRITE(UNIT = nulprt1,FMT = 3027)  &
                    trim(cnamout(ig_number_field(jf))),  &
                    afldcobn(ig_number_field(jf))
              WRITE(UNIT = nulprt1,FMT=3028) nbnfld(ig_number_field(jf))
              DO 350 jc = 1, nbnfld(ig_number_field(jf))
                WRITE (UNIT = nulprt1,FMT = 3030)  &
                    cbnfld(jc,ig_number_field(jf)),  &
                      abncoef (jc,ig_number_field(jf))
 350          CONTINUE
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'SUBGRID') THEN
              WRITE(UNIT = nulprt1,FMT = 3031)  &
                    cgrdsub(ig_number_field(jf)),  &
                    nlusub(ig_number_field(jf)), &
                    nsubfl(ig_number_field(jf)),  &
                    nsubvoi(ig_number_field(jf)),  &
                    ctypsub(ig_number_field(jf))
              IF (ctypsub(ig_number_field(jf)) .EQ. 'NONSOLAR') THEN 
                  WRITE(UNIT = nulprt1,FMT = 3032)  &
                      cdqdt(ig_number_field(jf)), &
                      cfldcoa(ig_number_field(jf)),  &
                      cfldfin(ig_number_field(jf))
                ELSE IF (ctypsub(ig_number_field(jf)) .EQ. 'SOLAR') THEN
                  WRITE(UNIT = nulprt1,FMT = 3033) &
                      cfldfin(ig_number_field(jf)),  &
                        cfldcoa(ig_number_field(jf))
              ENDIF
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'CHECKIN') THEN
                WRITE(UNIT = nulprt1,FMT = 3034)  &
                    ntinpflx(ig_number_field(jf))
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'CHECKOUT') THEN
                WRITE(UNIT = nulprt1,FMT = 3035)  &
                    ntoutflx(ig_number_field(jf)) 
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'GLORED') THEN
              WRITE(UNIT = nulprt1,FMT = 3036)  &
                    ntronca(ig_number_field(jf)) 
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'NOINTERP') THEN
                WRITE(UNIT = nulprt1,FMT = 3037)
            ELSE IF (canal(ja,ig_number_field(jf)) .EQ. 'LOCTRANS') THEN
               WRITE(UNIT = nulprt1,FMT = 3047) cl_print_trans
            ELSE 
              WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
              WRITE (UNIT = nulprt1,FMT = *) &
                  ' Type of analysis not implemented yet '
              WRITE (UNIT = nulprt1,FMT = *)  &
                  ' The analysis required in OASIS is :'
              WRITE (UNIT = nulprt1,FMT = *) ' canal = ',  &
                   canal(ja,ig_number_field(jf))
              WRITE (UNIT = nulprt1,FMT = *)  &
                  ' with ja = ', ja, ' jf = ', jf
              WRITE (UNIT = nulprt1,FMT = *) ' '
              WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
              WRITE (nulprt1,'(a)') ' error = STOP in inipar'
              CALL prism_sys_flush(nulprt1)
              CALL prism_sys_abort()
          ENDIF
 320    CONTINUE
      ENDIF
      ENDIF
 310  CONTINUE
     ENDIF
 ENDIF

!* Formats

 3001 FORMAT(//,15X,'  FIELD NUMBER ',I3)
 3002 FORMAT(15X,'  ************  ')
 3003 FORMAT(/,10X,'  Field parameters ')
 3004 FORMAT(10X,'  ****************  ',/)
 3005 FORMAT(/,10X,'  Input field symbolic name       = ',A, &
             /,10X,'  Output field symbolic name      = ',A, &
             /,10X,'  Field long name                 = ', &
             /,18X,A53, &
             /,10X,'  Field exchange frequency        = ',I8, &
             /,10X,'  Model sequential index          = ',I2, &
             /,10X,'  Field Lag                       = ',I8, &
!             /,10X,'  Model delay flag                = ',I2, &
!             /,10X,'  Extra time step flag            = ',I2, &
             /,10X,'  Field I/O status                = ',A8, &
             /,10X,'  Number of basic operations      = ',I4, &
             /,10X,'  Parallel decomposition strategy = ',A8,/)
 3115 FORMAT(/,10X,'  Input field symbolic name       = ',A8, &
             /,10X,'  Output field symbolic name      = ',A8, &
             /,10X,'  Field long name                 = ', &
             /,18X,A53, &
             /,10X,'  Field exchange frequency        = ',I8, &
             /,10X,'  Model sequential index          = ',I2, &
!             /,10X,'  Model delay flag                = ',I2, &
!             /,10X,'  Extra time step flag            = ',I2, &
             /,10X,'  Field I/O status                = ',A8, &
             /,10X,'  Number of basic operations      = ',I4,/)
 3116 FORMAT(/,10X,'  Input field symbolic name       = ',A8, &
             /,10X,'  Output field symbolic name      = ',A8, &
             /,10X,'  Field long name                 = ', &
             /,18X,A53, &
             /,10X,'  Field exchange frequency        = ',I8, &
             /,10X,'  Local transformation            = ',A8, &
             /,10X,'  Model sequential index          = ',I2, &
             /,10X,'  Field Lag                       = ',I8,  &
             /,10X,'  Field I/O status                = ',A8, &
             /,10X,'  Number of basic operations      = ',I4,/)
 3117 FORMAT(/,10X,'  Restart file name               = ',A8,/)
 3118 FORMAT(/,10X,'  Input file name                 = ',A32,/)
 3121 FORMAT(/,10X,'  Input field symbolic name       = ',A8, &
             /,10X,'  Output field symbolic name      = ',A8, &
             /,10X,'  Field long name                 = ', &
             /,18X,A53, &
             /,10X,'  Field exchange frequency        = ',I8, &
             /,10X,'  Local transformation            = ',A8, &
             /,10X,'  Field I/O status                = ',A8, &
             /,10X,'  Number of basic operations      = ',I4,/)
 3006 FORMAT(/,10X,'  Input file name                 = ',A8, &
             /,10X,'  Output file name                = ',A8,/)
 3007 FORMAT( &
             /,10X,'  Source grid periodicity type is      = ',A8, &
             /,10X,'  Number of overlapped grid points is  = ',I2, &
             /,10X,'  Target grid periodicity type is      = ',A8, &
             /,10X,'  Number of overlapped grid points is  = ',I2,/)
 3008 FORMAT(/,10X,'  Source longitude file string    = ',A8, &
             /,10X,'  Source latitude file string     = ',A8, &
             /,10X,'  Source mask file string         = ',A8, &
             /,10X,'  Source surface file string      = ',A8, &
             /,10X,'  Target longitude file string    = ',A8, &
             /,10X,'  Target latitude file string     = ',A8, &
             /,10X,'  Target mask file string         = ',A8, &
             /,10X,'  Target surface file string      = ',A8,/)
 3009 FORMAT(/,10X,'  ANALYSIS PARAMETERS ')
 3010 FORMAT(10X,'  ******************* ',/)
 3011 FORMAT(/,5X,'  ANALYSIS number ',I2,' is ',A8, &
             /,5X,'  ***************  ',/)
 3012 FORMAT(5X,' Value for masked points is        = ',E15.6)
 3013 FORMAT(5X,' Grid mapping file = ',A8,' linked to unit = ',I2, &
           /,5X,' Dataset identificator number      = ',I2, &
           /,5X,' Maximum number of neighbors is    = ',I4)
 3014 FORMAT(5X,' Source grid latitude order is    = ',A8)
 3015 FORMAT(5X,' Source grid longitude order is     = ',A8)
 3016 FORMAT(5X,' Target grid latitude order is    = ',A8)
 3017 FORMAT(5X,' Target grid longitude order is     = ',A8)
 3018 FORMAT(5X,' Extrapolation method is           = ',A8, &
           /,5X,' Number of neighbors used is       = ',I2)
 3019 FORMAT(5X,' Extrapolation file = ',A8,' linked to unit = ',I2, &
           /,5X,' Dataset identificator number      = ',I2)
 3020 FORMAT(5X,' Interpolation method is           = ',A8, &
           /,5X,' Source grid type is               = ',A8, &
           /,5X,' Field type is                     = ',A8)
 3021 FORMAT(5X,' Pointer for ANAIS storage is      = ',I2, &
           /,5X,' Maximum number of neighbors is    = ',I4, &
           /,5X,' Write/Read flag for weights is    = ',I2)
 3022 FORMAT(5X,' Variance multiplicator for ANAISG = ',E15.6)
 3023 FORMAT(5X,' Data to fill up field is in file  = ',A8, &
           /,5X,' Connected to logical unit number  = ',I2, &
           /,5X,' Filling method to blend field is  = ',A8)
 3024 FORMAT(5X,' Flag for coasts mismatch is       = ',I2,  &
           /,5X,' Name for flux correction field is = ',A8, &
           /,5X,' It is written on logical unit     = ',I2)
 3025 FORMAT(5X,' Conservation method for field is  = ',A8)
 3026 FORMAT(5X,' Half number of latitudes for gaussian grid is = ',I3, &
           /,5X,' Extrapolation flag is             = ',A8)
 3027 FORMAT(5X,' Field ',A,' is multiplied by Cst = ',E15.6)
 3028 FORMAT(5X,' It is combined with N fields    N = ',I2)
 3029 FORMAT(5X,' Data file = ',A8,' linked to unit = ',I2)
 3030 FORMAT(5X,'   With field ',A8,'   coefficient = ',E15.6)
 3031 FORMAT(5X,' Subgrid data file = ',A8,' linked to unit = ',I2, &
           /,5X,' Dataset identificator number      = ',I2, &
           /,5X,' Maximum number of neighbors is    = ',I4, &
           /,5X,' Type of subgrid interpolation is  = ',A8)
 3032 FORMAT(5X,' Subgrid variability is restored with addition of', &
             /,5X,A8,' x (',A8,' - ',A8,')')
 3033 FORMAT(5X,' Subgrid variability is restored multiplying by', &
             /,5X,'( 1 - ',A8,') / ( 1 - ',A8,')')
 3034 FORMAT(5X,' Integral calculation flag is =', I2)
 3035 FORMAT(5X,' Integral calculation flag is =', I2)
 3036 FORMAT(5X,' Half number of latitudes for gaussian grid is = ',I3)
 3037 FORMAT(5X,' No interpolation for this field ')
 3038 FORMAT(5X,' Write/Read flag for weights is    = ',I2, &
           /,5X,' Dataset identificator number      = ',I2)
 3042 FORMAT(5X,' Value for exported masked points is = ',E15.6)
 3043 FORMAT(/,5X,'No lag in namcouple for the field', I3, &
          /,5X,' Default value LAG=0 will be used ')
 3044 FORMAT(/,5X,'The lag for the field ',I3,3X,'is : ',I8)
 3045 FORMAT(5X,' Remapping method is               = ',A8, &
           /,5X,' Field type is                     = ',A8, &
           /,5X,' Normalization option is           = ',A8, &
           /,5X,' Seach restriction type is         = ',A8, &
           /,5X,' Number of search bins is          = ',I4)
 3046 FORMAT(5X,' Order of remapping is             = ',A8)
 3047 FORMAT(5X,' Local transformation  = ',A8) 
 3048 FORMAT(5X,' Remapping filename is             = ',A, &
           /,5X,' Mapping location is               = ',A8, &
           /,5X,' Mapping optimization is           = ',A8)


!*    4. End of routine
!        --------------

   IF (mpi_rank_global == 0) THEN
       IF (nlogprt .GE. 0) THEN
           WRITE(UNIT = nulprt1,FMT = *)' '
           WRITE(UNIT = nulprt1,FMT = *)'------ End of ROUTINE inipar ----'
           CALL FLUSH (nulprt1)
       ENDIF
   ENDIF
!      call prism_sys_debug_exit(subname)
      RETURN

!* Error branch output

 110  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' No active $JOBNAME data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 130  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *) &
             ' No active $NBMODEL data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 170  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' No active $MACHINE data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
      
 181  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *) &
             ' No active $CHATYPE data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 191  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *) &
             ' No active $RUNTIME data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 193  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *) &
             ' No active $INIDATE data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 195  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *) &
             ' No active $SEQMODE data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 197  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *) &
             ' No active $MODINFO data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 199  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *) &
             ' No active $NLOGPRT found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 201  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *) &
             ' No active $CALTYPE found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 210  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' No active $FIELDS data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 230  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' No active $STRING data found in input file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (UNIT = nulprt1,FMT = *)  &
             ' We STOP!!! Check the file namcouple'
          WRITE (UNIT = nulprt1,FMT = *) ' '
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 231  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) ' '
      ENDIF
      CALL prtout ('ERROR in namcouple for field', jf, 1)
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *)  &
             'NO index of sequential position and $SEQMODE > 1'
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar.f'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 232  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) ' '
      ENDIF
      CALL prtout ('ERROR in namcouple for field', jf, 1)
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *)  &
             'Index of sequential position greater than $SEQMODE'
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar.f'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort() 
 233  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) ' '
      ENDIF
      CALL prtout ('ERROR in namcouple for field', jf, 1)
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *)  &
             'Check the 2nd line for either the index of sequential position, the delay flag, or the extra timestep flag.'
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar.f'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort()
 234  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) ' '
      ENDIF
      CALL prtout ('ERROR in namcouple for field', jf, 1)
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *)  &
             'Index of sequential position equals 0'
          WRITE (UNIT = nulprt1,FMT = *)  &
             '(Should be 1 -default value- IF $SEQMODE=1)'
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar.f'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort() 
 235  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) ' '
      ENDIF
      CALL prtout ('ERROR in namcouple for field', jf, 1)
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *)  &
             'An input line with integral calculation flag' 
          WRITE (UNIT = nulprt1,FMT = *)  &
             '("INT=0" or "INT=1")'
          WRITE (UNIT = nulprt1,FMT = *)  &
             'is now required for analysis CHECKIN or CHECKOUT'
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar.f'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort() 
 236  CONTINUE
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *) ' '
      ENDIF
      CALL prtout ('ERROR in namcouple for field', jf, 1)
      IF (mpi_rank_global == 0) THEN
          WRITE (UNIT = nulprt1,FMT = *)  &
             'The coupling period must not be 0 !'
          WRITE (UNIT = nulprt1,FMT = *)  &
             'If you do not want to exchange this field at all'
          WRITE (UNIT = nulprt1,FMT = *)  &
             'give a coupling period longer than the total run time.'
          WRITE (nulprt1,'(a,i4)') ' abort by model ',compid
          WRITE (nulprt1,'(a)') ' error = STOP in inipar.f'
          CALL prism_sys_flush(nulprt1)
      ENDIF
      CALL prism_sys_abort() 

!      call prism_sys_debug_exit(subname)

      END SUBROUTINE inipar
!===============================================================================
 
  SUBROUTINE alloc()

  IMPLICIT NONE

  character(len=*),parameter :: subname='mod_prism_namcouple:alloc'

!  call prism_sys_debug_enter(subname)

  !--- alloc_anais1
  ALLOCATE (varmul(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "varmul"allocation of anais module',il_err,1)
  varmul(:)=0
  ALLOCATE (niwtm(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "niwtm"allocation of anais module',il_err,1)
  niwtm(:)=0
  ALLOCATE (niwtg(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "niwtg"allocation of anais module',il_err,1)
  niwtg(:)=0
  allocate (linit(ig_nfield), stat=il_err)
  if (il_err.ne.0) call prtout('error in "linit"allocation of anais module',il_err,1)
  linit(:)=.false.

  !--- alloc_analysis
  ALLOCATE (ncofld(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ncofld"allocation of analysis module',il_err,1)
  ncofld(:)=0
  ALLOCATE (neighborg(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "neighborg"allocation of analysis module',il_err,1)
  neighborg(:)=0
  ALLOCATE (nludat(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nludat"allocation of analysis module',il_err,1)
  nludat(:,:)=0
  ALLOCATE (nlufil(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nlufil"allocation of analysis module',il_err,1)
  nlufil(:)=0
  ALLOCATE (nlumap(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nlumap"allocation of analysis module',il_err,1)
  nlumap(:)=0
  ALLOCATE (nlusub(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nlusub"allocation of analysis module',il_err,1)
  nlusub(:)=0
  ALLOCATE (nluext(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nluext"allocation of analysis module',il_err,1)
  nluext(:)=0
  ALLOCATE (nosper(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nosper"allocation of analysis module',il_err,1)
  nosper(:)=0
  ALLOCATE (notper(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "notper"allocation of analysis module',il_err,1)
  notper(:)=0
  ALLOCATE (ntinpflx(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ntinpflx"allocation of analysis module',il_err,1)
  ntinpflx(:)=0
  ALLOCATE (ntoutflx(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ntoutflx"allocation of analysis module',il_err,1)
  ntoutflx(:)=0
  ALLOCATE (amskval(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "amskval"allocation of analysis module',il_err,1)
  amskval(:)=0
  ALLOCATE (amskvalnew(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "amskvalnew"allocation of analysis module',il_err,1)
  amskvalnew(:)=0
  ALLOCATE (acocoef(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "acocoef"allocation of analysis module',il_err,1)
  acocoef(:,:)=0
  ALLOCATE (abocoef(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "abocoef"allocation of analysis module',il_err,1)
  abocoef(:,:)=0
  ALLOCATE (abncoef(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "abncoef"allocation of analysis module',il_err,1)
  abncoef(:,:)=0
  ALLOCATE (afldcoef(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "afldcoef"allocation of analysis module',il_err,1)
  afldcoef(:)=0
  ALLOCATE (afldcobo(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "afldcobo"allocation of analysis module',il_err,1)
  afldcobo(:)=0
  ALLOCATE (afldcobn(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "afldcobn"allocation of analysis module',il_err,1)
  afldcobn(:)=0
  ALLOCATE (cxordbf(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cxordbf"allocation of analysis module',il_err,1)
  cxordbf(:)=' '
  ALLOCATE (cyordbf(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cyordbf"allocation of analysis module',il_err,1)
  cyordbf(:)=' '
  ALLOCATE (cxordaf(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cxordaf"allocation of analysis module',il_err,1)
  cxordaf(:)=' '
  ALLOCATE (cyordaf(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cyordaf"allocation of analysis module',il_err,1)
  cyordaf(:)=' '
  ALLOCATE (cgrdtyp(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cgrdtyp"allocation of analysis module',il_err,1)
  cgrdtyp(:)=' '
  ALLOCATE (cfldtyp(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfldtyp"allocation of analysis module',il_err,1)
  cfldtyp(:)=' '
  ALLOCATE (cfilfic(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfilfic"allocation of analysis module',il_err,1)
  cfilfic(:)=' '
  ALLOCATE (cfilmet(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfilmet"allocation of analysis module',il_err,1)
  cfilmet(:)=' '
  ALLOCATE (cconmet(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cconmet"allocation of analysis module',il_err,1)
  cconmet(:)=' '
  ALLOCATE (cfldcoa(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfldcoa"allocation of analysis module',il_err,1)
  cfldcoa(:)=' '
  ALLOCATE (cfldfin(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfldfin"allocation of analysis module',il_err,1)
  cfldfin(:)=' '
  ALLOCATE (ccofld(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ccofld"allocation of analysis module',il_err,1)
  ccofld(:,:)=' '
  ALLOCATE (cbofld(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cbofld"allocation of analysis module',il_err,1)
  cbofld(:,:)=' '
  ALLOCATE (cbnfld(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cbnfld"allocation of analysis module',il_err,1)
  cbnfld(:,:)=' '
  ALLOCATE (ccofic(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ccofic"allocation of analysis module',il_err,1)
  ccofic(:,:)=' '
  ALLOCATE (cdqdt(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cdqdt"allocation of analysis module',il_err,1)
  cdqdt(:)=' '
  ALLOCATE (cgrdmap(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cgrdmap"allocation of analysis module',il_err,1)
  cgrdmap(:)=' '
  ALLOCATE (cmskrd(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cmskrd"allocation of analysis module',il_err,1)
  cmskrd(:)=' '
  ALLOCATE (cgrdsub(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cgrdsub"allocation of analysis module',il_err,1)
  cgrdsub(:)=' '
  ALLOCATE (ctypsub(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ctypsub"allocation of analysis module',il_err,1)
  ctypsub(:)=' '
  ALLOCATE (cgrdext(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cgrdext"allocation of analysis module',il_err,1)
  cgrdext(:)=' '
  ALLOCATE (csper(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "csper"allocation of analysis module',il_err,1)
  csper(:)=' '
  ALLOCATE (ctper(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ctper"allocation of analysis module',il_err,1)
  ctper(:)=' '
  ALLOCATE (lsurf(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "lsurf"allocation of analysis module',il_err,1)
  lsurf(:)=.false.
  ALLOCATE (nscripvoi(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in nscripvoi allocation of analysis module',il_err,1)
  nscripvoi(:)=0
! 
!* Alloc array needed for SCRIP 
!
  ALLOCATE (cmap_method(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cmap_method" allocation of inipar_alloc',il_err,1)
  cmap_method(:)=' '
  ALLOCATE (cmap_file(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cmap_file" allocation of inipar_alloc',il_err,1)
  cmap_file(:)=' '
  ALLOCATE (cmaptyp(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cmaptyp" allocation of inipar_alloc',il_err,1)
  cmaptyp(:)=' '
  ALLOCATE (cmapopt(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cmapopt" allocation of inipar_alloc',il_err,1)
  cmapopt(:)=' '
  ALLOCATE (cfldtype(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfldtype"allocation of inipar_alloc',il_err,1)
  cfldtype(:)=' '
  ALLOCATE (crsttype(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "crsttype"allocation of inipar_alloc',il_err,1)
  crsttype(:)=' '
  ALLOCATE (nbins(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nbins"allocation of inipar_alloc',il_err,1)
  nbins(:)=0
  ALLOCATE (cnorm_opt(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cnorm_opt"allocation of inipar_alloc',il_err,1)
  cnorm_opt(:)=' '
  ALLOCATE (corder(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "corder"allocation of inipar_alloc',il_err,1)
  corder(:)=' '
!
!Vector case:
!
  IF (lg_vector) THEN
      ALLOCATE (cg_assoc_input_field(ig_total_nfield),stat=il_err)
      IF (il_err.NE.0) CALL prtout ('Error in "cg_assoc_input_field"allocation of inipar_alloc',il_err,1)
      cg_assoc_input_field(:)=' '
      ALLOCATE (ig_assoc_input_field(ig_total_nfield),stat=il_err)
      IF (il_err.NE.0) CALL prtout ('Error in "ig_assoc_input_field"allocation of inipar_alloc',il_err,1)
      ig_assoc_input_field(:)=0
      ALLOCATE (lrotate(ig_total_nfield),stat=il_err)
      IF (il_err.NE.0) CALL prtout ('Error in "lrotate"allocation of inipar_alloc',il_err,1)
  ENDIF

  !--- alloc_extrapol1
  ALLOCATE (niwtn(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "niwtn"allocation of extrapol module',il_err,1)
  niwtn(:)=0
  ALLOCATE (niwtng(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "niwtng"allocation of extrapol module',il_err,1)
  niwtng(:)=0
  ALLOCATE (lextra(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "lextra"allocation of extrapol module',il_err,1)
  lextra(:)=.false.
  ALLOCATE (lweight(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "lweight"allocation of extrapol module',il_err,1)
  lweight(:)=.false.

  !--- alloc_parallel
  ALLOCATE (cparal(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cparal"allocation of parallel module',il_err,1)
  cparal(:)=' '  

  !--- alloc_rainbow1
  ALLOCATE (lmapp(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "lmapp"allocation of rainbow module',il_err,1)
  lmapp(:)=.false.
  ALLOCATE (lsubg(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "lsubg"allocation of rainbow module',il_err,1)
  lsubg(:)=.false.

  !--- alloc_string
  ALLOCATE (cg_name_rstfile(ig_nbr_rstfile), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cg_name_rstfile"allocation of string module',il_err,1)
  cg_name_rstfile(:)=' '
  ALLOCATE (ig_lag(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_lag"allocation of string module',il_err,1) 
  ig_lag(:)=0
  ALLOCATE (ig_no_rstfile(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_no_rstfile"allocation of string module',il_err,1)
  ig_no_rstfile(:)=1
  ALLOCATE (cg_input_field(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cg_input_field"allocation of string module',il_err,1)
  cg_input_field(:)=' '
  ALLOCATE (ig_numlab(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_numlab"allocation of string module',il_err,1)
  ig_numlab(:)=0
  ALLOCATE (ig_freq(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_freq"allocation of string module',il_err,1)
  ig_freq(:)=0
  ALLOCATE (ig_total_nseqn(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_total_nseqn"allocation of string module',il_err,1)
  ig_total_nseqn(:)=0
  ALLOCATE (ig_local_trans(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_local_trans"allocation of string module',il_err,1)
  ig_local_trans(:)=0
  ALLOCATE (ig_invert(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_invert" allocation of string module',il_err,1) 
  ig_invert(:)=0
  ALLOCATE (ig_reverse(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_reverse" allocation of string module',il_err,1) 
  ig_reverse(:)=0
!
!** + Allocate following arrays only if one field (at least) goes
!     through Oasis
!
  IF (lg_oasis_field) THEN
  ALLOCATE (numlab(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "numlab"allocation of string module',il_err,1)
  numlab(:)=0
  ALLOCATE (nfexch(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nfexch"allocation of string module',il_err,1)
  nfexch(:)=0
  ALLOCATE (nseqn(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nseqn"allocation of string module',il_err,1)
  nseqn(:)=0
  ALLOCATE (nlagn(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nlagn" allocation of string module',il_err,1)
  nlagn(:)=0
  ALLOCATE (cnaminp(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cnaminp"allocation of string module',il_err,1)
  cnaminp(:)=' '
  ALLOCATE (cnamout(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cnamout"allocation of string module',il_err,1)
  cnamout(:)=' '
  ALLOCATE (cficout(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cficout"allocation of string module',il_err,1)
  cficout(:)=' '
  ALLOCATE (cstate(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cstate"allocation of string module',il_err,1)
  cstate(:)=' '
  ENDIF

!  call prism_sys_debug_exit(subname)

  END SUBROUTINE alloc
!===============================================================================
  SUBROUTINE dealloc

  IMPLICIT NONE

  character(len=*),parameter :: subname='mod_prism_namcouple:dealloc'

!  call prism_sys_debug_enter(subname)

  deallocate(nbtotproc,stat=il_err)
  if (il_err.ne.0) call prtout('error in "nbtotproc"deallocation of experiment module',il_err,1)

  !--- alloc_anais1
  DEALLOCATE (varmul, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "varmul"deallocation of anais module',il_err,1)
  DEALLOCATE (niwtm, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "niwtm"deallocation of anais module',il_err,1)
  DEALLOCATE (niwtg, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "niwtg"deallocation of anais module',il_err,1)
  deallocate (linit, stat=il_err)
  if (il_err.ne.0) call prtout('error in "linit"deallocation of anais module',il_err,1)

  !--- alloc_analysis
  DEALLOCATE (ncofld, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ncofld"deallocation of analysis module',il_err,1)
  DEALLOCATE (neighborg, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "neighborg"deallocation of analysis module',il_err,1)
  DEALLOCATE (nludat, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nludat"deallocation of analysis module',il_err,1)
  DEALLOCATE (nlufil, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nlufil"deallocation of analysis module',il_err,1)
  DEALLOCATE (nlumap, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nlumap"deallocation of analysis module',il_err,1)
  DEALLOCATE (nlusub, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nlusub"deallocation of analysis module',il_err,1)
  DEALLOCATE (nluext, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nluext"deallocation of analysis module',il_err,1)
  DEALLOCATE (nosper, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nosper"deallocation of analysis module',il_err,1)
  DEALLOCATE (notper, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "notper"deallocation of analysis module',il_err,1)
  DEALLOCATE (ntinpflx, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ntinpflx"deallocation of analysis module',il_err,1)
  DEALLOCATE (ntoutflx, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ntoutflx"deallocation of analysis module',il_err,1)
  DEALLOCATE (amskval, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "amskval"deallocation of analysis module',il_err,1)
  DEALLOCATE (amskvalnew, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "amskvalnew"deallocation of analysis module',il_err,1)
  DEALLOCATE (acocoef, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "acocoef"deallocation of analysis module',il_err,1)
  DEALLOCATE (abocoef, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "abocoef"deallocation of analysis module',il_err,1)
  DEALLOCATE (abncoef, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "abncoef"deallocation of analysis module',il_err,1)
  DEALLOCATE (afldcoef, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "afldcoef"deallocation of analysis module',il_err,1)
  DEALLOCATE (afldcobo, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "afldcobo"deallocation of analysis module',il_err,1)
  DEALLOCATE (afldcobn, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "afldcobn"deallocation of analysis module',il_err,1)
  DEALLOCATE (cxordbf, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cxordbf"deallocation of analysis module',il_err,1)
  DEALLOCATE (cyordbf, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cyordbf"deallocation of analysis module',il_err,1)
  DEALLOCATE (cxordaf, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cxordaf"deallocation of analysis module',il_err,1)
  DEALLOCATE (cyordaf, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cyordaf"deallocation of analysis module',il_err,1)
  DEALLOCATE (cgrdtyp, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cgrdtyp"deallocation of analysis module',il_err,1)
  DEALLOCATE (cfldtyp, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfldtyp"deallocation of analysis module',il_err,1)
  DEALLOCATE (cfilfic, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfilfic"deallocation of analysis module',il_err,1)
  DEALLOCATE (cfilmet, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfilmet"deallocation of analysis module',il_err,1)
  DEALLOCATE (cconmet, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cconmet"deallocation of analysis module',il_err,1)
  DEALLOCATE (cfldcoa, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfldcoa"deallocation of analysis module',il_err,1)
  DEALLOCATE (cfldfin, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfldfin"deallocation of analysis module',il_err,1)
  DEALLOCATE (ccofld, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ccofld"deallocation of analysis module',il_err,1)
  DEALLOCATE (cbofld, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cbofld"deallocation of analysis module',il_err,1)
  DEALLOCATE (cbnfld, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cbnfld"deallocation of analysis module',il_err,1)
  DEALLOCATE (ccofic, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ccofic"deallocation of analysis module',il_err,1)
  DEALLOCATE (cdqdt, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cdqdt"deallocation of analysis module',il_err,1)
  DEALLOCATE (cgrdmap, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cgrdmap"deallocation of analysis module',il_err,1)
  DEALLOCATE (cmskrd, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cmskrd"deallocation of analysis module',il_err,1)
  DEALLOCATE (cgrdsub, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cgrdsub"deallocation of analysis module',il_err,1)
  DEALLOCATE (ctypsub, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ctypsub"deallocation of analysis module',il_err,1)
  DEALLOCATE (cgrdext, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cgrdext"deallocation of analysis module',il_err,1)
  DEALLOCATE (csper, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "csper"deallocation of analysis module',il_err,1)
  DEALLOCATE (ctper, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ctper"deallocation of analysis module',il_err,1)
  DEALLOCATE (lsurf, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "lsurf"deallocation of analysis module',il_err,1)
  DEALLOCATE (nscripvoi, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in nscripvoi deallocation of analysis module',il_err,1)
! 
!* Alloc array needed for SCRIP 
!
  DEALLOCATE (cmap_method,stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cmap_method" deallocation of inipar_alloc',il_err,1)
  DEALLOCATE (cmap_file,stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cmap_file" deallocation of inipar_alloc',il_err,1)
  DEALLOCATE (cmaptyp,stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cmaptyp" deallocation of inipar_alloc',il_err,1)
  DEALLOCATE (cmapopt,stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cmapopt" deallocation of inipar_alloc',il_err,1)
  DEALLOCATE (cfldtype,stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cfldtype"deallocation of inipar_alloc',il_err,1)
  DEALLOCATE (crsttype,stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "crsttype"deallocation of inipar_alloc',il_err,1)
  DEALLOCATE (nbins,stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nbins"deallocation of inipar_alloc',il_err,1)
  DEALLOCATE (cnorm_opt,stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cnorm_opt"deallocation of inipar_alloc',il_err,1)
  DEALLOCATE (corder,stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "corder"deallocation of inipar_alloc',il_err,1)
!
!Vector case:
!
  IF (lg_vector) THEN
      DEALLOCATE (cg_assoc_input_field,stat=il_err)
      IF (il_err.NE.0) CALL prtout ('Error in "cg_assoc_input_field"deallocation of inipar_alloc',il_err,1)
      DEALLOCATE (ig_assoc_input_field,stat=il_err)
      IF (il_err.NE.0) CALL prtout ('Error in "ig_assoc_input_field"deallocation of inipar_alloc',il_err,1)
      DEALLOCATE (lrotate,stat=il_err)
      IF (il_err.NE.0) CALL prtout ('Error in "lrotate"deallocation of inipar_alloc',il_err,1)
  ENDIF

  !--- alloc_extrapol1
  DEALLOCATE (niwtn, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "niwtn"deallocation of extrapol module',il_err,1)
  DEALLOCATE (niwtng, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "niwtng"deallocation of extrapol module',il_err,1)
  DEALLOCATE (lextra, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "lextra"deallocation of extrapol module',il_err,1)
  DEALLOCATE (lweight, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "lweight"deallocation of extrapol module',il_err,1)

  !--- alloc_parallel
  DEALLOCATE (cparal, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cparal"deallocation of parallel module',il_err,1)

  !--- alloc_rainbow1
  DEALLOCATE (lmapp, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "lmapp"deallocation of rainbow module',il_err,1)
  DEALLOCATE (lsubg, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "lsubg"deallocation of rainbow module',il_err,1)

  !--- alloc_string
  DEALLOCATE (cg_name_rstfile, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cg_name_rstfile"deallocation of string module',il_err,1)
  DEALLOCATE (ig_lag, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_lag"deallocation of string module',il_err,1) 
  DEALLOCATE (ig_no_rstfile, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_no_rstfile"deallocation of string module',il_err,1)
  DEALLOCATE (cg_input_field, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cg_input_field"deallocation of string module',il_err,1)
  DEALLOCATE (ig_numlab, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_numlab"deallocation of string module',il_err,1)
  DEALLOCATE (ig_freq, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_freq"deallocation of string module',il_err,1)
  DEALLOCATE (ig_total_nseqn, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_total_nseqn"deallocation of string module',il_err,1)
  DEALLOCATE (ig_local_trans, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_local_trans"deallocation of string module',il_err,1)
  DEALLOCATE (ig_invert, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_invert" deallocation of string module',il_err,1) 
  DEALLOCATE (ig_reverse, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "ig_reverse" deallocation of string module',il_err,1) 
!
!** + Deallocate following arrays only if one field (at least) goes
!     through Oasis
!
  IF (lg_oasis_field) THEN
  DEALLOCATE (numlab, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "numlab"deallocation of string module',il_err,1)
  DEALLOCATE (nfexch, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nfexch"deallocation of string module',il_err,1)
  DEALLOCATE (nseqn, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nseqn"deallocation of string module',il_err,1)
  DEALLOCATE (nlagn, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "nlagn" deallocation of string module',il_err,1)
  DEALLOCATE (cnaminp, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cnaminp"deallocation of string module',il_err,1)
  DEALLOCATE (cnamout, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cnamout"deallocation of string module',il_err,1)
  DEALLOCATE (cficout, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cficout"deallocation of string module',il_err,1)
  DEALLOCATE (cstate, stat=il_err)
  IF (il_err.NE.0) CALL prtout ('Error in "cstate"deallocation of string module',il_err,1)
  ENDIF

!  call prism_sys_debug_exit(subname)

  END SUBROUTINE dealloc
!===============================================================================

  SUBROUTINE prtout(cdtext, kvalue, kstyle)

!****
!               *****************************
!               * OASIS ROUTINE  -  LEVEL 1 *
!               * -------------     ------- *
!               *****************************
!
!**** *prtout*  - Print output
!
!     Purpose:
!     -------
!     Print out character string and one integer value
!
!**   Interface:
!     ---------
!       *CALL*  *prtout (cdtext, kvalue, kstyle)*
!
!     Input:
!     -----
!                cdtext : character string to be printed
!                kvalue : integer variable to be printed
!                kstyle : printing style
!
!     Output:
!     ------
!     None
!
!     Workspace:
!     ---------
!
!     Externals:
!     ---------
!     None
!
!     Reference:
!     ---------
!     See OASIS manual (1995) 
!
!     History:
!     -------
!       Version   Programmer     Date      Description
!       -------   ----------     ----      -----------  
!       2.0       L. Terray      95/10/01  created
!       2.3       L. Terray      99/02/24  modified: X format for NEC
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      IMPLICIT NONE
!
!* ---------------------------- Include files ---------------------------
!
!
!* ---------------------------- Argument declarations ----------------------
!
      CHARACTER(len=*),intent(in) :: cdtext
      INTEGER (kind=ip_intwp_p),intent(in) :: kvalue, kstyle

!* ---------------------------- Local declarations ----------------------

      integer(kind=ip_intwp_p) :: ilen,jl
      CHARACTER*69 cline
      character(len=*),PARAMETER :: cbase = '-'
      character(len=*),PARAMETER :: cprpt = '* ===>>> :'
      character(len=*),PARAMETER :: cdots = '  ------  '
      character(len=*),parameter :: subname='mod_prism_namcouple:prtout'

!* ---------------------------- Poema verses ----------------------------

!  call prism_sys_debug_enter(subname)

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!*    1. Print character string + integer value
!        --------------------------------------

  IF (mpi_rank_global == 0) THEN
      IF ( kstyle .EQ. 1 .OR. kstyle .EQ. 2) THEN
          cline = ' '
          ilen = len(cdtext)
          DO 110 jl = 1, ILEN
            cline(jl:jl) = cbase
 110      CONTINUE
          
          IF ( kstyle .EQ. 2 ) THEN
              WRITE(UNIT = nulprt1,FMT='(/,A,1X,A)') cdots, cline
          ENDIF
          WRITE(UNIT = nulprt1,FMT='(A,1X,A,1X,I18)') cprpt, cdtext, kvalue
          WRITE(UNIT = nulprt1,FMT='(A,1X,A,/)') cdots, cline
        ELSE
          WRITE(UNIT = nulprt1,FMT='(/,A,1X,A,1X,I18,/)') cprpt, cdtext, kvalue
      ENDIF

!*    2. End of routine
!        --------------

      CALL prism_sys_flush(nulprt1)
  ENDIF

!      call prism_sys_debug_exit(subname)

  END SUBROUTINE prtout

!===============================================================================

      SUBROUTINE prcout (cdtext, cdstring, kstyle)
!****
!               *****************************
!               * OASIS ROUTINE  -  LEVEL 1 *
!               * -------------     ------- *
!               *****************************
!
!**** *prcout*  - Print output
!
!     Purpose:
!     -------
!     Print out character string and one character value
!
!**   Interface:
!     ---------
!       *CALL*  *prcout (cdtext, cdstring, kstyle)*
!
!     Input:
!     -----
!                cdtext   : character string to be printed
!                cdstring : character variable to be printed
!                kstyle   : printing style
!
!     Output:
!     ------
!     None
!
!     Workspace:
!     ---------
!     None
!
!     Externals:
!     ---------
!     None
!
!     Reference:
!     ---------
!     See OASIS manual (1995) 
!
!     History:
!     -------
!       Version   Programmer     Date      Description
!       -------   ----------     ----      -----------  
!       2.0       L. Terray      95/10/01  created
!       2.3       L. Terray      99/02/24  modified: X format for NEC
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      IMPLICIT NONE
!
!* ---------------------------- Include files ---------------------------
!
!
!* ---------------------------- Argument declarations ----------------------
!
      CHARACTER(len=*),intent(in) :: cdtext, cdstring
      INTEGER (kind=ip_intwp_p),intent(in) :: kstyle
!
!* ---------------------------- Local declarations ----------------------
!
      integer (kind=ip_intwp_p) :: ilen,jl
      CHARACTER*69 cline
      character(len=*), PARAMETER :: cpbase = '-'
      character(len=*), PARAMETER :: cprpt = '* ===>>> :'
      character(len=*), PARAMETER :: cpdots = '  ------  ' 
      character(len=*),parameter :: subname='mod_prism_namcouple:prcout'
!
!* ---------------------------- Poema verses ----------------------------
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!*    1. Print character string + character value
!        ----------------------------------------
!
!  call prism_sys_debug_enter(subname)

   IF (mpi_rank_global == 0) THEN
      IF ( kstyle .EQ. 1 .OR. kstyle .EQ. 2) THEN
          cline = ' '
          ilen = len(cdtext)
          DO 110 jl = 1, ilen
            cline(jl:jl) = cpbase
 110      CONTINUE
          IF ( kstyle .EQ. 2 ) THEN
              WRITE(UNIT = nulprt1,FMT='(/,A,1X,A)') cpdots, cline
          ENDIF
          WRITE(UNIT = nulprt1,FMT='(A,1X,A,1X,A)') cprpt, cdtext, cdstring
          WRITE(UNIT = nulprt1,FMT='(A,1X,A,/)') cpdots, cline
        ELSE
          WRITE(UNIT = nulprt1,FMT='(/,A,1X,A,1X,A,/)') cprpt, cdtext, cdstring
      ENDIF
!
!
!*    3. End of routine
!        --------------
!
      CALL prism_sys_flush(nulprt1)
  ENDIF

!      call prism_sys_debug_exit(subname)

      END SUBROUTINE prcout
!===============================================================================

  SUBROUTINE parse (cdone, cdtwo, knumb, klen, kleng)
!****
!               *****************************
!               * OASIS ROUTINE  -  LEVEL T *
!               * -------------     ------- *
!               *****************************
!
!**** *parse*  - Parsing routine
!
!     Purpose:
!     -------
!     Find the knumb'th string in cdone and put it in cdtwo.
!     A string is defined as a continuous set of non-blanks characters
!
!**   Interface:
!     ---------
!       *CALL*  *parse (cdone, cdtwo, knumb, klen, kleng)*
!
!     Input:
!     -----
!                cdone : line to be parsed (char string)
!                knumb : rank within the line of the extracted string (integer)
!                klen  : length of the input line (integer)
!
!     Output:
!     ------
!                cdtwo : extracted character string (char string)
!                kleng : length of the extracted string (integer)
!
!     Workspace:
!     ---------
!     None
!
!     Externals:
!     ---------
!
!     Reference:
!     ---------
!     See OASIS manual (1995)
!
!     History:
!     -------
!       Version   Programmer     Date      Description
!       -------   ----------     ----      -----------  
!       2.0       L. Terray      95/09/01  created
!                 O. Marti     2000/11/08  simplify by using F90 
!                                          CHARACTER functions
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      IMPLICIT NONE
!
!* ---------------------------- Include files ---------------------------
!
!
!* ---------------------------- Argument declarations -------------------
!
  INTEGER (kind=ip_intwp_p), INTENT ( in) :: knumb, klen
  CHARACTER (len=klen), INTENT ( inout) :: cdone 
  CHARACTER (len=klen), INTENT ( out) :: cdtwo
  INTEGER (kind=ip_intwp_p), INTENT ( out) :: kleng
!
!* ---------------------------- Local declarations -------------------
!
  integer(kind=ip_intwp_p) :: ii,jl
  CHARACTER (len=klen) :: clline
  CHARACTER (len=klen) :: clwork
  CHARACTER (len=1), SAVE :: clblank = ' ', clcmt = '#'
  character(len=*),parameter :: subname='mod_prism_namcouple:parse'
!
!* ---------------------------- Poema verses ----------------------------

!  call prism_sys_debug_enter(subname)

!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!*    1. Skip line if it is a comment
!        ----------------------------
!
100 IF (cdone(1:1) .NE. clcmt) GO TO 120
  READ (UNIT = nulin, FMT = 1001) clline 
  cdone(1:klen) = clline(1:klen)
  GO TO 100
120 CONTINUE 
1001 FORMAT(A80)
!
!
!*    2. Do the extraction job
!        ---------------------
!
!* - Fill cdtwo with blanks
!
  cdtwo = clblank
!
!* Fill temporary string and remove leading blanks
!
  clwork = ADJUSTL ( cdone)
!
!* - If there are no more characters, kleng=-1
!
  IF ( LEN_TRIM ( clwork) .LE. 0) THEN
      kleng = -1
!      call prism_sys_debug_exit(subname)
      RETURN
  END IF
!
!* - If this is the one we're looking for, skip
!    otherwise go knumb-1 more sets of characters
!
  IF (knumb .GE. 2) THEN
      DO jl = 1, knumb-1
        ii = INDEX ( clwork, clblank) - 1
        clwork ( 1:ii) = clblank
        clwork = ADJUSTL ( clwork)
!
!* - If there are no more characters, kleng=-1
!
        IF (LEN_TRIM ( clwork) .LE. 0) THEN
            kleng = -1
!            call prism_sys_debug_exit(subname)
            RETURN
        END IF
      END DO
  END IF
!
!* - Find the length of this set of characters
!
  kleng = INDEX ( clwork, clblank) - 1
!
!* - Copy to cdtwo
!
  cdtwo ( 1:kleng) = clwork ( 1: kleng)
!
!*    3. End of routine
!        --------------
!
!  call prism_sys_debug_exit(subname)

  END SUBROUTINE parse

!===============================================================================

  SUBROUTINE parseblk (cdone, cdtwo, knumb, klen, kleng)

!****
!               *****************************
!               * OASIS ROUTINE  -  LEVEL T *
!               * -------------     ------- *
!               *****************************
!
!**** *parse*  - Parsing routine
!
!     Purpose:
!     -------
!     Get the rest of the line starting at the knumb'th string.
!     A string is defined as a continuous set of non-blanks characters
!
!**   Interface:
!     ---------
!       *CALL*  *parseblk (cdone, cdtwo, knumb, klen, kleng)*
!
!     Input:
!     -----
!                cdone : line to be parsed (char string)
!                knumb : rank within the line of the starting string (integer)
!                klen  : length of the input line (integer)
!
!     Output:
!     ------
!                cdtwo : extracted rest of line, including blanks (char string)
!                kleng : length of the extracted string (integer)
!
!     Workspace:
!     ---------
!     None
!
!     Externals:
!     ---------
!
!     History:
!     -------
!       Version   Programmer     Date      Description
!       -------   ----------     ----      -----------  
!       2.5       S. Valcke      00/09/08  Adapted from parse.f
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
  IMPLICIT NONE
!
!* ---------------------------- Include files ---------------------------
!
!
!* ---------------------------- Argument declarations -------------------
!
  INTEGER (kind=ip_intwp_p), INTENT ( in) :: knumb, klen
  CHARACTER (len=klen), INTENT ( inout) :: cdone
  CHARACTER (len=klen), INTENT ( out) :: cdtwo
  INTEGER (kind=ip_intwp_p), INTENT ( out) :: kleng
!
!* ---------------------------- Local declarations -------------------
!
  INTEGER (kind=ip_intwp_p) :: ii,jl
  INTEGER (kind=ip_intwp_p) :: il, kleng_aux
  CHARACTER (len=klen) :: clline
  CHARACTER (len=klen) :: clwork
  CHARACTER (len=1), SAVE :: clblank = ' ', clcmt = '#'
  character(len=*),parameter :: subname='mod_prism_namcouple:parseblk'
!
!* ---------------------------- Poema verses ----------------------------

!  call prism_sys_debug_enter(subname)

!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!*    1. Skip line if it is a comment
!        ----------------------------
!
100 IF (cdone(1:1) .NE. clcmt) GO TO 120
  READ (UNIT = nulin, FMT = 1001) clline 
  cdone(1:klen) = clline(1:klen)
  GO TO 100
120 CONTINUE 
1001 FORMAT(A80)
!
!
!*    2. Do the extraction job
!        ---------------------
!
!* - Fill cdtwo with blanks
!
  cdtwo = clblank
!
!* Fill temporary string and remove leading blanks
!
  il = INDEX ( cdone, clblank)
  kleng_aux = 1
  IF (INDEX ( cdone, clblank).EQ.1) THEN
      DO WHILE (cdone(il+1:il+1).EQ.clblank)
        kleng_aux = kleng_aux +1
        il = il+1
        IF (il+1.GT.klen) GO TO 130
      ENDDO
  ENDIF
130 CONTINUE
  clwork = ADJUSTL ( cdone)
!
!* - If there are no more characters, kleng=-1
!
  IF ( LEN_TRIM ( clwork) .LE. 0) THEN
      kleng = -1
!      call prism_sys_debug_exit(subname)
      RETURN
  END IF
!
!* - If this is the one we're looking for, skip
!    otherwise go knumb-1 more sets of characters
!
  IF (knumb .GE. 2) THEN
      DO jl = 1, knumb-1
        ii = INDEX ( clwork, clblank) - 1
        il = ii + 1 
        DO WHILE (clwork(il:il).EQ.clblank)
          kleng_aux = kleng_aux +1
          il = il + 1
          IF (il.GT.klen) GO TO 140
        ENDDO
140 CONTINUE
        kleng_aux = kleng_aux + ii
        clwork ( 1:ii) = clblank
        clwork = ADJUSTL ( clwork)
!
!* - If there are no more characters, kleng=-1
!
        IF (LEN_TRIM ( clwork) .LE. 0) THEN
            kleng = -1
!            call prism_sys_debug_exit(subname)
            RETURN
        END IF
      END DO
  END IF
!
!* - Find the length of the rest of the line
!
  kleng = klen - kleng_aux
!
!* - Copy to cdtwo
!
  cdtwo ( 1:kleng) = clwork ( 1: kleng)
!
!*    3. End of routine
!        --------------
!

!  call prism_sys_debug_exit(subname)

  END SUBROUTINE parseblk
!===============================================================================

  SUBROUTINE skip (cd_one, id_len, endflag)
!
!**** SKIP
!
!     Purpose:
!       Skip line if it is a comment
!
!     Interface: 
!       Call skip (cl_one)
!
!     Method:
!       Read the first caracter of the line and skip line if 
!       it is a comment
!
!     External:
!       none
!
!     Files:
!       none
!   
!     References:
!
!     History:
!     --------
!       Version   Programmer     Date        Description
!       ------------------------------------------------
!       2.5       A.Caubel       2002/04/04  created
!
!*-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!** + DECLARATIONS
!
!
!** ++ Include files
!
!** ++ Argument declarations
!
  INTEGER (kind=ip_intwp_p),intent(in) :: id_len
  CHARACTER(len=*),intent(inout)       :: cd_one
  LOGICAL, optional, intent(inout)     :: endflag
!
!** ++ Local declarations
!
  INTEGER (kind=ip_intwp_p) :: ib
  CHARACTER(len=80) :: cl_line
  CHARACTER(len=1) :: cl_two
  character(len=*),parameter :: subname='mod_prism_namcouple:skip'
!
!*-----------------------------------------------------------------------
!
!  call prism_sys_debug_enter(subname)

  cl_two='#'
100 IF (cd_one(1:1) .NE. cl_two) GO TO 120
  if (present(endflag)) then
     endflag = .false.
     READ (UNIT = nulin, FMT = 1001, END=140) cl_line
  else
     READ (UNIT = nulin, FMT = 1001) cl_line
  endif
  cd_one = trim(cl_line)
  GO TO 100
120 CONTINUE 
  RETURN
140 CONTINUE
  ENDFLAG = .true.
  RETURN
1001 FORMAT(A80)
!
!*-----------------------------------------------------------------------
!
!  call prism_sys_debug_exit(subname)

  END SUBROUTINE skip
!
!*========================================================================
!===============================================================================
!===============================================================================
END MODULE mod_prism_namcouple


