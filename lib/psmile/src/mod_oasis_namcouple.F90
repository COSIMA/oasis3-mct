
!> Reads the namcouple file for use in OASIS

!> This code reads in the namcouple file and sets several variables
!> that are available to the rest of OASIS.  Some of this code
!> is obsolete, and several input settings are deprecated.
!> This code is based on the original Oasis3 version and
!> will be rewritten at some point.

MODULE mod_oasis_namcouple

!     - - - - - - - - - - - - - - - - - - - - - - - - - - -

  USE mod_oasis_kinds
  USE mod_oasis_data
  USE mod_oasis_parameters
  USE mod_oasis_sys
  USE mod_oasis_mpi
  USE mod_oasis_string

  IMPLICIT NONE

  private

  public oasis_namcouple_init

! NAMCOUPLE PUBLIC DATA

  INTEGER (kind=ip_intwp_p),PARAMETER :: jpeighty = 5000 !< max number of characters to be read 
                                                         !< in each line of the file namcouple 
  CHARACTER(len=*),parameter :: rform = '(A5000)'        !< formatted line read format

  CHARACTER(len=*),parameter :: nmapdec_default = 'decomp_1d'        ! decomp_wghtfile or decomp_1d
!  CHARACTER(len=*),parameter :: nmapdec_default = 'decomp_wghtfile' ! decomp_wghtfile or decomp_1d

! CHARACTER(len=*),parameter :: nmatxrd_default = 'orig'   ! ceg or orig
  CHARACTER(len=*),parameter :: nmatxrd_default = 'ceg'    ! ceg or orig

  CHARACTER(len=*),parameter :: nwgtopt_default = 'abort_on_bad_weights'         ! weights handling
!  CHARACTER(len=*),parameter :: nwgtopt_default = 'ignore_bad_weights'          ! weights handling
!  CHARACTER(len=*),parameter :: nwgtopt_default = 'ignore_bad_weights_silently' ! weights handling 
!  CHARACTER(len=*),parameter :: nwgtopt_default = 'use_bad_weights'             ! weights handling

  INTEGER(kind=ip_i4_p)   ,public :: nnamcpl       !< number of namcouple inputs
  INTEGER(kind=ip_i4_p)   ,public :: namruntim     !< namcouple runtime
  INTEGER(kind=ip_i4_p)   ,public :: namlogprt     !< namcouple nlogprt value
  INTEGER(kind=ip_i4_p)   ,public :: namtlogprt    !< namcouple ntlogprt value
  INTEGER(kind=ip_i4_p)   ,public :: namuntmin     !< namcouple min IO unit value
  INTEGER(kind=ip_i4_p)   ,public :: namuntmax     !< namcouple max IO unit value
  LOGICAL                 ,public :: namnorest     !< namcouple allow no restarts
  CHARACTER(len=ic_med)   ,public :: nammapdec     !< namcouple map decomp value
  CHARACTER(len=ic_med)   ,public :: nammatxrd     !< namcouple matrix read option
  CHARACTER(len=ic_med)   ,public :: namwgtopt     !< namcouple weights handling option
 
  CHARACTER(len=jpeighty) ,public,pointer :: namsrcfld(:)  !< list of src fields
  CHARACTER(len=jpeighty) ,public,pointer :: namdstfld(:)  !< list of dst fields
  CHARACTER(len=ic_lvar)  ,public,pointer :: namsrcgrd(:)  !< src grid name
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namsrc_nx(:)  !< src nx grid size
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namsrc_ny(:)  !< src ny grid size
  CHARACTER(len=ic_lvar)  ,public,pointer :: namdstgrd(:)  !< dst grid name
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namdst_nx(:)  !< dst nx grid size
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namdst_ny(:)  !< dst ny grid size
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namfldseq(:)  !< SEQ value
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namfldops(:)  !< operation, ip_expout,...
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namflddti(:)  !< coupling period (secs)
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namfldlag(:)  !< coupling lag (secs)
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namfldtrn(:)  !< fields transform, ip_instant,...
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namfldcon(:)  !< conserv fld operation
  CHARACTER(len=ic_med)   ,public,pointer :: namfldcoo(:)  !< conserv fld option (bfb, opt)
  CHARACTER(len=ic_long)  ,public,pointer :: nammapfil(:)  !< mapping file name
  CHARACTER(len=ic_med)   ,public,pointer :: nammaploc(:)  !< mapping location (src or dst pes)
  CHARACTER(len=ic_med)   ,public,pointer :: nammapopt(:)  !< mapping option (bfb, sum, or opt)
  CHARACTER(len=ic_med)   ,public,pointer :: namrstfil(:)  !< restart file name
  CHARACTER(len=ic_med)   ,public,pointer :: naminpfil(:)  !< input file name
  LOGICAL                 ,public,pointer :: namchecki(:)  !< checkin flag
  LOGICAL                 ,public,pointer :: namchecko(:)  !< checkout flag
  REAL (kind=ip_realwp_p) ,public,pointer :: namfldsmu(:)  !< src multiplier term
  REAL (kind=ip_realwp_p) ,public,pointer :: namfldsad(:)  !< src additive term
  REAL (kind=ip_realwp_p) ,public,pointer :: namflddmu(:)  !< dst multipler term
  REAL (kind=ip_realwp_p) ,public,pointer :: namflddad(:)  !< dst additive term

  CHARACTER(len=ic_med)   ,public,pointer :: namscrmet(:)  !< scrip method (CONSERV, DISTWGT, BILINEAR, BICUBIC, GAUSWGT)
  CHARACTER(len=ic_med)   ,public,pointer :: namscrnor(:)  !< scrip conserv normalization (FRACAREA, DESTAREA, FRACNNEI)
  CHARACTER(len=ic_med)   ,public,pointer :: namscrtyp(:)  !< scrip mapping type (SCALAR, VECTOR)
  CHARACTER(len=ic_med)   ,public,pointer :: namscrord(:)  !< scrip conserve order (FIRST, SECOND)
  CHARACTER(len=ic_med)   ,public,pointer :: namscrres(:)  !< scrip search restriction (LATLON, LATITUDE)
  REAL (kind=ip_realwp_p) ,public,pointer :: namscrvam(:)  !< scrip gauss weight distance weighting for GAUSWGT
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namscrnbr(:)  !< scrip number of neighbors for GAUSWGT and DISTWGT
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namscrbin(:)  !< script number of search bins

  !--- derived ---
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namsort2nn(:) !< sorted namcpl for sort, define nn order, computed later
  INTEGER(kind=ip_i4_p)   ,public,pointer :: namnn2sort(:) !< sorted namcpl for nn, define sort number, computed later

!----------------------------------------------------------------
!   LOCAL ONLY BELOW HERE
!----------------------------------------------------------------

  INTEGER(kind=ip_i4_p) :: nulin     ! namcouple IO unit number
  CHARACTER(len=*),parameter :: cl_namcouple = 'namcouple'

! --- alloc_src
  INTEGER (kind=ip_intwp_p) :: il_err
! --- mod_unitncdf
  LOGICAL :: lncdfgrd
  LOGICAL :: lncdfrst
! --- mod_label
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
! --- mod_rainbow
  LOGICAL,DIMENSION(:),ALLOCATABLE :: lmapp
  LOGICAL,DIMENSION(:),ALLOCATABLE :: lsubg
! --- mod_coast
  INTEGER (kind=ip_intwp_p) :: nfcoast
  LOGICAL :: lcoast
! --- mod_timestep
  INTEGER (kind=ip_intwp_p) :: ntime
  INTEGER (kind=ip_intwp_p) :: niter
  INTEGER (kind=ip_intwp_p) :: nitfn
  INTEGER (kind=ip_intwp_p) :: nstep
! --- mod_parameter
  INTEGER (kind=ip_intwp_p) :: ig_nfield   ! number of oasis coupled fields
  INTEGER (kind=ip_intwp_p) :: ig_direct_nfield   ! number of direct coupled fields
  INTEGER (kind=ip_intwp_p) :: ig_total_nfield    ! estimate of total fields
  INTEGER (kind=ip_intwp_p) :: ig_final_nfield    ! number of final fields
  LOGICAL :: lg_oasis_field
  INTEGER (kind=ip_intwp_p) :: ig_maxcomb
  INTEGER (kind=ip_intwp_p) :: ig_maxnoa
  INTEGER (kind=ip_intwp_p) :: ig_maxnfg
! --- mod_printing
  INTEGER(kind=ip_intwp_p) :: nlogprt
!---- Time statistics level printing
  INTEGER(kind=ip_intwp_p) :: ntlogprt
!---- min and max unit numbers
  INTEGER(kind=ip_intwp_p) :: nuntmin
  INTEGER(kind=ip_intwp_p) :: nuntmax
!---- allow no restart files
  LOGICAL :: nnorest
!---- specify mapping decomp
  CHARACTER(len=ic_med) :: nmapdec
  CHARACTER(len=ic_med) :: nmatxrd
  CHARACTER(len=ic_med) :: nwgtopt
! --- mod_string
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: numlab
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_numlab
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: nfexch
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_ntrans
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_total_ntrans
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
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_grid_nbrbf
  INTEGER (kind=ip_intwp_p),DIMENSION(:),ALLOCATABLE :: ig_grid_nbraf
  INTEGER (kind=ip_intwp_p)                          :: ig_nbr_rstfile
  INTEGER (kind=ip_intwp_p)                          :: ig_total_frqmin
  LOGICAL                  ,DIMENSION(:),ALLOCATABLE :: lg_state
  CHARACTER(len=jpeighty)   ,DIMENSION(:),ALLOCATABLE :: cnaminp
  CHARACTER(len=jpeighty)   ,DIMENSION(:),ALLOCATABLE :: cnamout
  CHARACTER(len=8)         ,DIMENSION(:,:),ALLOCATABLE :: canal
  CHARACTER(len=8)                                   :: cg_c
  CHARACTER(len=32)        ,DIMENSION(:),ALLOCATABLE :: cg_name_rstfile
  CHARACTER(len=32)        ,DIMENSION(:),ALLOCATABLE :: cg_restart_file
  CHARACTER(len=32)        ,DIMENSION(:),ALLOCATABLE :: cficinp
  CHARACTER(len=8)         ,DIMENSION(:),ALLOCATABLE :: cficout
  CHARACTER(len=32)        ,DIMENSION(:),ALLOCATABLE :: cg_input_file
  CHARACTER(len=jpeighty)   ,DIMENSION(:),ALLOCATABLE :: cg_input_field
  CHARACTER(len=jpeighty)   ,DIMENSION(:),ALLOCATABLE :: cg_output_field
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
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  nbins
  INTEGER (kind=ip_intwp_p) :: nlucor
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  nscripvoi
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
  CHARACTER(len=8), DIMENSION(:),ALLOCATABLE :: cconopt
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
  LOGICAL, DIMENSION(:),ALLOCATABLE :: lsurf
! --- mod_anais
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: naismfl
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: naisgfl
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: naismvoi
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: naisgvoi
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: niwtm
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: niwtg
  REAL (kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: varmul
  LOGICAL, DIMENSION(:), ALLOCATABLE :: linit
! --- mod extrapol
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: niwtn
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nninnfl 
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: niwtng
  INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: nninnflg
  LOGICAL, DIMENSION(:), ALLOCATABLE :: lextra
  LOGICAL, DIMENSION(:), ALLOCATABLE :: lweight
!--- local reuse values
  CHARACTER*9, parameter :: &
    clfield  = '$NFIELDS ', &
    clchan   = '$CHANNEL ', &
    clstring = '$STRINGS ', &
    clmod    = '$NBMODEL ', &
    cljob    = '$JOBNAME ', &
    cltime   = '$RUNTIME ', &
    clseq    = '$SEQMODE ', &
    cldate   = '$INIDATE ', &
    clhead   = '$MODINFO ', &
    clprint  = '$NLOGPRT ', &
    clmapdec = '$NMAPDEC ', &
    clmatxrd = '$NMATXRD ', &
    clwgtopt = '$NWGTOPT ', &
    clunit   = '$NUNITNO ', &
    clrest   = '$NNOREST ', &
    clcal    = '$CALTYPE ', &
    clend    = '$END     '
  INTEGER (kind=ip_intwp_p),parameter :: nkeywords = 17
  CHARACTER*9, parameter :: keyword_list(nkeywords) = &
    (/clfield, clchan, clstring, clmod, cljob, cltime, clseq, &
     cldate, clhead, clprint, clmapdec, clmatxrd, clunit, clrest, &
     clcal, clend, clwgtopt /)
  CHARACTER*512 :: tmpstr1, tmpstr2, tmpstr3, tmpstr4


!------------------------------------------------------------
CONTAINS
!------------------------------------------------------------

!> Reads the namcouple

SUBROUTINE oasis_namcouple_init()

  IMPLICIT NONE

  !-----------------------------------------------------------
  INTEGER(kind=ip_i4_p) :: n, nv, n1, n2, loc
  INTEGER(kind=ip_i4_p) :: ja, jf, jc
  INTEGER(kind=ip_i4_p) :: il_iost
  INTEGER(kind=ip_i4_p) :: maxunit
  CHARACTER(len=*),parameter :: subname='(oasis_namcouple_init)'
  !-----------------------------------------------------------

  CALL oasis_unitget(nulin)
  OPEN (nulin,FILE =cl_namcouple,STATUS='OLD', FORM ='FORMATTED', IOSTAT = il_iost)

  IF (mpi_rank_global == 0) THEN
     IF (il_iost .NE. 0) THEN
        WRITE(tmpstr1,*) ' ERROR opening namcouple file ',TRIM(cl_namcouple),' with unit number ',nulin
        CALL namcouple_abort(subname,__LINE__,tmpstr1)
     ELSE
        WRITE(nulprt1,*) subname,' open namcouple file ',TRIM(cl_namcouple),' with unit number ',nulin
     ENDIF
  ENDIF

  CALL inipar_alloc()
  CALL alloc()
  CALL inipar()
  !
  ! Close namcouple unit
  close(nulin)
  
  CALL oasis_unitfree(nulin)

  IF (mpi_rank_global == 0) THEN
      WRITE(nulprt1,*) subname,' allocating ig_final_nfield',ig_final_nfield
      CALL oasis_flush(nulprt1)
  ENDIF

  allocate(namsrcfld(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namsrcfld" allocation of experiment module',il_err,1)

  allocate(namdstfld(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namdstfld" allocation of experiment module',il_err,1)

  allocate(namsrcgrd(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namsrcgrd" allocation of experiment module',il_err,1)

  allocate(namsrc_nx(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namsrc_nx" allocation of experiment module',il_err,1)

  allocate(namsrc_ny(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namsrc_ny" allocation of experiment module',il_err,1)

  allocate(namdstgrd(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namdstgrd" allocation of experiment module',il_err,1)

  allocate(namdst_nx(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namdst_nx" allocation of experiment module',il_err,1)

  allocate(namdst_ny(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namdst_ny" allocation of experiment module',il_err,1)

  allocate(namfldseq(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldseq" allocation of experiment module',il_err,1)

  allocate(namfldops(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldops" allocation of experiment module',il_err,1)

  allocate(namfldtrn(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldtrn" allocation of experiment module',il_err,1)

  allocate(namfldcon(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldcon" allocation of experiment module',il_err,1)

  allocate(namfldcoo(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldcoo" allocation of experiment module',il_err,1)

  allocate(namflddti(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namflddti" allocation of experiment module',il_err,1)

  allocate(namfldlag(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldlag" allocation of experiment module',il_err,1)

  allocate(nammapfil(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nammapfil" allocation of experiment module',il_err,1)

  allocate(nammaploc(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nammaploc" allocation of experiment module',il_err,1)

  allocate(nammapopt(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nammapopt" allocation of experiment module',il_err,1)

  allocate(namrstfil(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namrstfil" allocation of experiment module',il_err,1)

  allocate(naminpfil(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "naminpfil" allocation of experiment module',il_err,1)

  allocate(namsort2nn(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namsort2nn" allocation of experiment module',il_err,1)

  allocate(namnn2sort(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namnn2sort" allocation of experiment module',il_err,1)

  allocate(namchecki(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namchecki" allocation of experiment module',il_err,1)

  allocate(namchecko(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namchecko" allocation of experiment module',il_err,1)

  allocate(namfldsmu(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldsmu" allocation of experiment module',il_err,1)

  allocate(namfldsad(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namfldsad" allocation of experiment module',il_err,1)

  allocate(namflddmu(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namflddmu" allocation of experiment module',il_err,1)

  allocate(namflddad(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namflddad" allocation of experiment module',il_err,1)

  allocate(namscrmet(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrmet" allocation of experiment module',il_err,1)

  allocate(namscrnor(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrnor" allocation of experiment module',il_err,1)

  allocate(namscrtyp(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrtyp" allocation of experiment module',il_err,1)

  allocate(namscrord(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrord" allocation of experiment module',il_err,1)

  allocate(namscrres(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrres" allocation of experiment module',il_err,1)

  allocate(namscrvam(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrvam" allocation of experiment module',il_err,1)

  allocate(namscrnbr(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrnbr" allocation of experiment module',il_err,1)

  allocate(namscrbin(ig_final_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "namscrbin" allocation of experiment module',il_err,1)

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
  namfldcoo(:) = "bfb"
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

!  maxunit = max(maxval(iga_unitmod),1024)
!  maxunit = 1024
!  IF (mpi_rank_global == 0) THEN
!      WRITE(nulprt1,*) subname,' maximum unit number = ',maxunit
!      CALL oasis_flush(nulprt1)
!  ENDIF
!  CALL oasis_unitsetmin(maxunit)

  nnamcpl = ig_final_nfield
  namruntim = ntime
  namlogprt = nlogprt
  namtlogprt = ntlogprt
  namuntmin = nuntmin
  namuntmax = nuntmax
  namnorest = nnorest
  nammapdec = nmapdec
  nammatxrd = nmatxrd
  namwgtopt = nwgtopt
  DO jf = 1,ig_final_nfield
     namsrcfld(jf) = cg_input_field(jf)
     namdstfld(jf) = cg_output_field(jf)
     namfldseq(jf) = ig_total_nseqn(jf)
     namfldops(jf) = ig_total_state(jf)
     IF (namfldops(jf) == ip_auxilary) THEN
        WRITE(tmpstr1,*) jf,' ERROR: AUXILARY NOT SUPPORTED'
        CALL namcouple_abort(subname,__LINE__,tmpstr1)
     ENDIF
     IF (namfldops(jf) == ip_ignored) THEN
        namfldops(jf) = ip_exported
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,jf,'WARNING: IGNORED converted to EXPORTED'
           CALL oasis_flush(nulprt1)
        ENDIF
     ENDIF
     IF (namfldops(jf) == ip_ignout) THEN
        namfldops(jf) = ip_expout
        IF (mpi_rank_global == 0) THEN
            WRITE(nulprt1,*) subname,jf,'WARNING: IGNOUT converted to EXPOUT'
            CALL oasis_flush(nulprt1)
        ENDIF
     ENDIF
     namflddti(jf) = ig_freq(jf)
     namfldlag(jf) = ig_lag(jf)
     namfldtrn(jf) = ig_local_trans(jf)
     namrstfil(jf) = trim(cg_restart_file(jf))
     naminpfil(jf) = trim(cg_input_file(jf))
     IF (ig_number_field(jf) > 0) THEN
        namsrcgrd(jf) = trim(cficbf(ig_number_field(jf)))
        namsrc_nx(jf) = nlonbf(ig_number_field(jf))
        namsrc_ny(jf) = nlatbf(ig_number_field(jf))
        namdstgrd(jf) = trim(cficaf(ig_number_field(jf)))
        namdst_nx(jf) = nlonaf(ig_number_field(jf))
        namdst_ny(jf) = nlataf(ig_number_field(jf))
        DO ja = 1, ig_ntrans(ig_number_field(jf))

           IF (canal(ja,ig_number_field(jf)) .EQ. 'SCRIPR') THEN
              namscrmet(jf) = trim(cmap_method(ig_number_field(jf)))
              namscrnor(jf) = trim(cnorm_opt  (ig_number_field(jf)))
              namscrtyp(jf) = trim(cfldtype   (ig_number_field(jf)))
              namscrord(jf) = trim(corder     (ig_number_field(jf)))
              namscrres(jf) = trim(crsttype   (ig_number_field(jf)))
              namscrvam(jf) =      varmul     (ig_number_field(jf))
              namscrnbr(jf) =      nscripvoi  (ig_number_field(jf))
              namscrbin(jf) =      nbins      (ig_number_field(jf))
              IF (TRIM(namscrtyp(jf)) /= 'SCALAR') THEN
                 WRITE(tmpstr1,*) subname,jf,'WARNING: SCRIPR weights generation &
                   & supported only for SCALAR mapping, not '//TRIM(namscrtyp(jf))
                 CALL namcouple_abort(subname,__LINE__,tmpstr1)
              ENDIF

           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'MAPPING') THEN
              nammapfil(jf) = trim(cmap_file(ig_number_field(jf)))
              nammaploc(jf) = trim(cmaptyp(ig_number_field(jf)))
              nammapopt(jf) = trim(cmapopt(ig_number_field(jf)))

           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'CONSERV') THEN
              namfldcon(jf) = ip_cnone
              namfldcoo(jf) = trim(cconopt(ig_number_field(jf)))
              IF (cconmet(ig_number_field(jf)) .EQ. 'GLOBAL') namfldcon(jf) = ip_cglobal
              IF (cconmet(ig_number_field(jf)) .EQ. 'GLBPOS') namfldcon(jf) = ip_cglbpos
              IF (cconmet(ig_number_field(jf)) .EQ. 'BASBAL') namfldcon(jf) = ip_cbasbal
              IF (cconmet(ig_number_field(jf)) .EQ. 'BASPOS') namfldcon(jf) = ip_cbaspos
              IF (namfldcon(jf) .EQ. ip_cnone) THEN
                 WRITE(tmpstr1,*) subname,jf,'WARNING: CONSERV option not supported: '//&
                                  &TRIM(cconmet(ig_number_field(jf)))
                 CALL namcouple_abort(subname,__LINE__,tmpstr1)
              ENDIF

           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'CHECKIN' ) THEN
              namchecki(jf) = .true.

           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'CHECKOUT') THEN
              namchecko(jf) = .true.

           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'BLASOLD') THEN
              namfldsmu(jf) = afldcobo(ig_number_field(jf))
              DO jc = 1, nbofld(ig_number_field(jf))
                 IF (trim(cbofld(jc,ig_number_field(jf))) == 'CONSTANT') THEN
                    namfldsad(jf) = abocoef(jc,ig_number_field(jf))
                 ELSE
                    WRITE(tmpstr1,*) subname,jf,'ERROR: BLASOLD only supports CONSTANT: '//&
                                     &TRIM(cbofld(jc,ig_number_field(jf)))
                    CALL namcouple_abort(subname,__LINE__,tmpstr1)
                 ENDIF
              ENDDO

           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'BLASNEW') THEN
              namflddmu(jf) = afldcobn(ig_number_field(jf))
              DO jc = 1, nbnfld(ig_number_field(jf))
                 IF (trim(cbnfld(jc,ig_number_field(jf))) == 'CONSTANT') THEN
                    namflddad(jf) = abncoef(jc,ig_number_field(jf))
                 ELSE
                    WRITE(tmpstr1,*) subname,jf,'ERROR: BLASNEW only supports CONSTANTS: '//&
                                     &TRIM(cbofld(jc,ig_number_field(jf)))
                    CALL namcouple_abort(subname,__LINE__,tmpstr1)
                 ENDIF
              ENDDO

           ENDIF  ! canal
        ENDDO  ! ig_ntrans
     ENDIF   ! ig_number_field
  ENDDO   ! ig_final_nfield

  IF (mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) ' '
     WRITE(nulprt1,*) subname,'namlogprt,t   ',namlogprt, namtlogprt
     WRITE(nulprt1,*) subname,'namuntmin,max ',namuntmin, namuntmax
     WRITE(nulprt1,*) subname,'namnorest     ',namnorest
     WRITE(nulprt1,*) subname,'nammapdec     ',trim(nammapdec)
     WRITE(nulprt1,*) subname,'nammatxrd     ',trim(nammatxrd)
     WRITE(nulprt1,*) subname,'namwgtopt     ',trim(namwgtopt)
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
        WRITE(nulprt1,*) subname,n,'namfldcoo ',TRIM(namfldcoo(n))
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
        CALL oasis_flush(nulprt1)
     ENDDO
  ENDIF

  !--- compute seq sort ---
  namsort2nn(:) = -1
  DO nv = 1,nnamcpl
     loc = nv    ! default at end
     n1 = 1
     DO WHILE (loc == nv .and. n1 < nv)
        IF (namfldseq(nv) < namfldseq(namsort2nn(n1))) loc = n1
        n1 = n1 + 1
     ENDDO
     ! nv goes into loc location, shift THEN set
     DO n1 = nv,loc+1,-1
        namsort2nn(n1) = namsort2nn(n1-1)
     ENDDO
     namsort2nn(loc) = nv
  ENDDO

  DO nv = 1,nnamcpl
     namnn2sort(namsort2nn(nv)) = nv
  ENDDO

  IF (mpi_rank_global == 0) THEN
     DO nv = 1,nnamcpl
        n1 = namsort2nn(nv)
        n2 = namnn2sort(nv)
        WRITE(nulprt1,*) subname,' sort ',nv,n1,n2,namfldseq(n1)
        CALL oasis_flush(nulprt1)
     ENDDO
  ENDIF

  !--- check they are sorted ---
  DO n = 2,nnamcpl
     IF (namfldseq(namsort2nn(n)) < namfldseq(namsort2nn(n-1))) THEN
        CALL namcouple_abort(subname,__LINE__,' ERROR in seq sort')
     ENDIF
  ENDDO

  CALL dealloc()

  !  CALL oasis_debug_exit(subname)

END SUBROUTINE oasis_namcouple_init

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
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  IMPLICIT NONE

  !* ---------------------------- Local declarations --------------------
  
  CHARACTER*5000 clline, clline_aux, clvari
  CHARACTER*3 clind
  CHARACTER*2 cldeb
  CHARACTER*1 clequa
  CHARACTER*8 clwork
  CHARACTER*8 clstrg
  CHARACTER*7 cl_bsend

  CHARACTER(len=32), DIMENSION(:), ALLOCATABLE :: cl_aux
  CHARACTER(len=32) :: keyword
  INTEGER (kind=ip_intwp_p) il_varid, il_len, il_err, il_maxanal 
  INTEGER (kind=ip_intwp_p) nlonbf_notnc, nlatbf_notnc,  &
     nlonaf_notnc, nlataf_notnc
  INTEGER (kind=ip_intwp_p) iind, il_redu, ib, il_aux, il_auxbf, &
     il_auxaf, istatus, il_id
  INTEGER (kind=ip_intwp_p) :: ja,jz,jm,jf,ILEN,n,ios
  INTEGER (kind=ip_intwp_p) :: ig_clim_maxport
  LOGICAL :: lg_bsend,endflag
  LOGICAL :: found, readfile
  CHARACTER(len=*),parameter :: subname='(mod_oasis_namcouple:inipar_alloc)'

  !* ---------------------------- Poema verses --------------------------

  !  CALL oasis_debug_enter(subname)

  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  !*    1. Get basic info for the simulation 
  !        ---------------------------------

  IF (mpi_rank_global == 0) THEN
     WRITE(nulprt1,*)' '
     WRITE(nulprt1,*) subname
     WRITE(nulprt1,*)'  ROUTINE inipar_alloc - Level 0'
     WRITE(nulprt1,*)'  ********************   *******'
     WRITE(nulprt1,*)' '
     WRITE(nulprt1,*)'  Initialization of run parameters'
     WRITE(nulprt1,*)' '
     WRITE(nulprt1,*)'  Reading input file namcouple'
     WRITE(nulprt1,*)' '
     WRITE(nulprt1,*)' '
     CALL oasis_flush(nulprt1)
  ENDIF

  !* Initialization
  ig_direct_nfield = 0
  ig_nfield = 0
  lg_oasis_field = .true.

  !* Check for typos in keywords, read all lines until file is at end

  IF (mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) '  Executing valid keyword check '
     CALL oasis_flush(nulprt1)
  ENDIF
  readfile = .true.
  DO WHILE (readfile)
     READ(nulin, FMT=rform, END=501) clline
     CALL skip(clline, jpeighty, ios=ios)
     IF (ios == 0) THEN
         CALL parse(clline, clvari, 1, jpeighty, ILEN, __LINE__)
         IF (clvari(1:1) == "$") THEN
             found = .FALSE.
             DO n = 1, nkeywords
               IF (clvari == keyword_list(n)) found = .TRUE.
             ENDDO
             IF (.NOT. found) THEN
                 IF (mpi_rank_global == 0) THEN
                     WRITE(nulprt1,*) '    Found invalid keyword = '//TRIM(clvari)
                     CALL oasis_flush(nulprt1)
                 ENDIF
                 CALL namcouple_abort(subname,__LINE__,' ERROR: invalid keyword = '//TRIM(clvari))
             ELSE
                 IF (mpi_rank_global == 0) THEN
                     WRITE(nulprt1,*) '    Found valid keyword = '//TRIM(clvari)
                     CALL oasis_flush(nulprt1)
                 ENDIF
             ENDIF
         ENDIF
     ELSE
         GOTO 501
     ENDIF
  ENDDO
501 CONTINUE

  if (mpi_rank_global == 0) WRITE(nulprt1,*) ' '

  !* Get number of models involved in this simulation

  keyword = clmod
  CALL findkeyword (keyword, clline, found)
  IF (found .and. mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) ' ***WARNING*** '//trim(keyword)//' is obsolete in OASIS3-MCT'
     CALL oasis_flush(nulprt1)
  ENDIF

  keyword = clchan
  CALL findkeyword (keyword, clline, found)
  IF (found .and. mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) ' ***WARNING*** '//trim(keyword)//' is obsolete in OASIS3-MCT'
     CALL oasis_flush(nulprt1)
  ENDIF

  !*    2. Get field information
  !        --------------------
  
  !* Read total number of fields exchanged by this OASIS process

  keyword = clfield
  CALL findkeyword (keyword, clline, found)
  IF (found) THEN
     READ(nulin, FMT=rform) clline
     CALL skip(clline, jpeighty, ios=ios)
     CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
     IF (ilen > 0) THEN
        READ(clvari, FMT=2003) ig_total_nfield
     ELSE
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) ' ***WARNING*** Nothing on input for '//trim(keyword)
           WRITE(nulprt1,*) '               Default value will be used '
           WRITE(nulprt1,*) ' '
           CALL oasis_flush(nulprt1)
        ENDIF
     ENDIF
  ELSE
     WRITE(tmpstr1,*) trim(keyword)//' not found in namcouple'
     CALL namcouple_abort(subname,__LINE__,tmpstr1)
  ENDIF

  !* Print out the total number of fields exchanged by this OASIS process

  CALL prtout('The maximum number of exchanged fields set in namcouple is nfield =',ig_total_nfield, 1)

  !* Alloc field number array 

  ALLOCATE (ig_number_field(ig_total_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error: ig_number_field allocation of '//trim(subname),il_err,1)
  ig_number_field(:)=0

  !* Alloc field status array (LOGICAL indicating if the field goes through 
  !* Oasis or not)

  ALLOCATE (lg_state(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error: lg_state allocation of '//trim(subname),il_err,1)
  lg_state(:)=.false.

  !* Alloc status of all the fields

  ALLOCATE (ig_total_state(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error: ig_total_state allocation of '//trim(subname),il_err,1)
  ig_total_state(:)=0

  !* Alloc input field name array 

  ALLOCATE (cg_output_field(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error: cg_output_field allocation of '//trim(subname),il_err,1)
  cg_output_field(:)=' ' 

  !* Alloc number of analyses array 

  ALLOCATE (ig_total_ntrans(ig_total_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error: ig_total_ntrans"allocation of '//trim(subname),il_err,1)
  ig_total_ntrans (:) = 0

  !* Alloc array of restart file names, input and output file names

  ALLOCATE (cg_restart_file(ig_total_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error: cg_restart_FILE allocation of '//trim(subname),il_err,1)
  cg_restart_file(:)=' '
  ALLOCATE (cg_input_file(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cg_input_file"allocation of '//trim(subname),il_err,1)
  cg_input_file(:)=' '

  !* Alloc array of source and target locator prefix

  ALLOCATE (cga_locatorbf(ig_total_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error: cga_locatorbf allocation of '//trim(subname),il_err,1)
  cga_locatorbf(:)=' '

  ALLOCATE (cga_locatoraf(ig_total_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error: cga_locatoraf allocation of '//trim(subname),il_err,1)
  cga_locatoraf(:)=' '  

  !* Get information for all fields

  keyword = clstring
  CALL findkeyword (keyword, clline, found)
  IF (.not.found) THEN
     WRITE(tmpstr1,*) trim(keyword)//' not found in namcouple'
     CALL namcouple_abort(subname,__LINE__,tmpstr1)
  ENDIF

  !* Loop on total number of fields

  ig_final_nfield = 0
  endflag = .false.
  jf = 0
 
  DO WHILE (.not. endflag .and. jf < ig_total_nfield)
     jf = jf + 1

     !* First line

     READ(nulin, FMT=rform, END=241) clline
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,'1 Read line: ',TRIM(clline)
           CALL oasis_flush(nulprt1)
        ENDIF
     CALL skip(clline, jpeighty, ios=ios)
     CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
     IF (trim(clvari) .eq. "$END") endflag = .true.

     IF (TRIM(clvari) .EQ. " ") THEN
        WRITE(tmpstr1,*) ' size clline smaller than the size of the names of the fields on the line'
        WRITE(tmpstr2,*) ' increase jpeighty and change the associated format A(jpeighty) and cline'
        CALL namcouple_abort(subname,__LINE__,tmpstr1,tmpstr2)
     ENDIF

     IF (.not. endflag) THEN

        !* Get output field symbolic name
        CALL parse(clline, clvari, 2, jpeighty, ilen, __LINE__)
        cg_output_field(jf) = clvari

        !* Get total number of analysis
        CALL parse(clline, clvari, 5, jpeighty, ilen, __LINE__)
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,'parsing 1 Read line, clvari in 5 position: ',TRIM(clline),TRIM(clvari)
           CALL oasis_flush(nulprt1) 
        ENDIF
        READ(clvari,FMT=2003) ig_total_ntrans(jf)

        !* Get field STATUS for OUTPUT fields
        CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
        IF (clvari(1:6) .EQ. 'OUTPUT') THEN
           ig_direct_nfield = ig_direct_nfield + 1
           lg_state(jf) = .false.
           ig_total_state(jf) = ip_output
        ELSE
           !* Get field status (direct or through oasis) and the number  
           !* of direct and indirect fields if not PIPE nor NONE
           CALL parse(clline, clvari, 7, jpeighty, ilen, __LINE__)
           IF (clvari(1:8).eq.'EXPORTED') THEN
              ig_nfield = ig_nfield + 1
              lg_state(jf) = .true.
              ig_number_field(jf) = ig_nfield
              ig_total_state(jf) = ip_exported
              CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
              !* Get restart file name               
              cg_restart_file(jf) = clvari
              !* Get restart file name
           ELSEIF (clvari(1:6) .eq. 'OUTPUT' ) THEN
              ig_direct_nfield = ig_direct_nfield + 1
              lg_state(jf) = .false.
              ig_total_state(jf) = ip_output
              CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
              cg_restart_file(jf) = clvari
           ELSEIF (clvari(1:7) .eq. 'IGNORED' ) THEN
              ig_direct_nfield = ig_direct_nfield + 1
              lg_state(jf) = .false.
              ig_total_state(jf) = ip_ignored
              CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
              !* Get restart file name
              cg_restart_file(jf) = clvari
           ELSEIF (clvari(1:6) .eq. 'EXPOUT') THEN
              ig_nfield = ig_nfield + 1
              lg_state(jf) = .true.
              ig_number_field(jf) = ig_nfield
              ig_total_state(jf) = ip_expout
              CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
              !* Get restart file name               
              cg_restart_file(jf) = clvari
           ELSEIF (clvari(1:6) .eq. 'IGNOUT' ) THEN
              ig_direct_nfield = ig_direct_nfield + 1
              lg_state(jf) = .false.
              ig_total_state(jf) = ip_ignout
              CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
              !* Get restart file name 
              cg_restart_file(jf) = clvari
           ELSEIF (clvari(1:9).eq. 'AUXILARY') THEN  
              ig_nfield = ig_nfield + 1
              lg_state(jf) = .true.
              ig_number_field(jf) = ig_nfield
              ig_total_state(jf) = ip_auxilary
              CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
              !* Get restart file name
              cg_restart_file(jf) = clvari
           ELSEIF (clvari(1:5) .eq. 'INPUT') THEN
              ig_direct_nfield = ig_direct_nfield + 1
              lg_state(jf) = .false.
              ig_total_state(jf) = ip_input
              CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
              !* Get input file name
              cg_input_file(jf) = clvari
           ENDIF
       ENDIF

       if (mpi_rank_global == 0) then
           WRITE(nulprt1,*) subname,'field jf : ',jf,' lg_state(jf) : ',lg_state(jf)
           WRITE(nulprt1,*) subname,'field jf : ',jf,' endflag : ',endflag
       endif

        IF (lg_state(jf)) THEN
!           IF (ig_total_ntrans(jf) .eq. 0) THEN
 !             WRITE(tmpstr1,*) 'If there is no analysis for the field',jf, &
 !                   'THEN the status must not be EXPORTED, AUXILIARY, or EXPOUT'
 !             CALL namcouple_abort(subname,__LINE__,tmpstr1)
 !          ENDIF

           READ(nulin, FMT=rform) clline
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,'2 Read line: ',TRIM(clline)
           CALL oasis_flush(nulprt1)
        ENDIF
           CALL skip(clline, jpeighty, ios=ios)
           READ(nulin, FMT=rform) clline
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,'3 Read line: ',TRIM(clline)
           CALL oasis_flush(nulprt1)
        ENDIF
           CALL skip(clline, jpeighty, ios=ios)
! MODIF LC quand il n'y a pas de transformations
           IF (ig_total_ntrans(jf) .GT. 0) THEN
           READ(nulin, FMT=rform) clline_aux
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,'4 Read line_aux: ',TRIM(clline_aux)
           CALL oasis_flush(nulprt1)
        ENDIF
           CALL skip(clline_aux, jpeighty, ios=ios)
           DO ja=1,ig_total_ntrans(jf)
              CALL parse(clline_aux, clvari, ja, jpeighty, ilen, __LINE__)
              IF (clvari.eq.'CORRECT'.or.clvari.eq.'BLASOLD'.or. &
                  clvari.eq.'BLASNEW') THEN
                 READ(nulin, FMT=rform) clline
                 CALL skip(clline, jpeighty, ios=ios)
                 CALL parse(clline, clvari, 2, jpeighty, ilen, __LINE__)
                 READ(clvari, FMT=2003) il_aux
                 DO ib = 1, il_aux
                    READ(nulin, FMT=rform) clline
                    CALL skip(clline, jpeighty, ios=ios)
                 ENDDO
              ELSEIF (clvari.eq.'NOINTERP') THEN
                 CONTINUE
              ELSE
                 READ(nulin, FMT=rform) clline
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,'5 Read line: ',TRIM(clline)
           CALL oasis_flush(nulprt1)
        ENDIF
                 CALL skip(clline, jpeighty, ios=ios)
              ENDIF
           ENDDO
! MODIF LC
        ENDIF
       ELSE
           IF (ig_total_state(jf) .ne. ip_input) THEN
              READ(nulin, FMT=rform) clline
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,'6 Read line : ',TRIM(clline)
           CALL oasis_flush(nulprt1)
        ENDIF
              CALL skip(clline, jpeighty, ios=ios)
           ENDIF
           IF (ig_total_state(jf) .ne. ip_input .and.  &
               ig_total_ntrans(jf) .gt. 0 ) THEN
              READ(nulin, FMT=rform) clline
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,'7 Read line: ',TRIM(clline)
           CALL oasis_flush(nulprt1)
        ENDIF
              CALL skip(clline, jpeighty, ios=ios)
              CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
              IF (clvari(1:8) .ne. 'LOCTRANS') THEN
                 WRITE(tmpstr1,*) 'You want a transformation which is not available !'
                 WRITE(tmpstr2,*) 'Only local transformations are available for '
                 WRITE(tmpstr3,*) 'fields exchanged directly or output fields '
                 CALL namcouple_abort(subname,__LINE__,tmpstr1,tmpstr2,tmpstr3)
              ENDIF
              DO ja=1,ig_total_ntrans(jf)
                 READ(nulin, FMT=rform) clline
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,'8 Read line: ',TRIM(clline)
           CALL oasis_flush(nulprt1)
        ENDIF
                 CALL skip(clline, jpeighty, ios=ios)
              ENDDO
           ENDIF
       ENDIF

        ig_final_nfield = ig_final_nfield + 1
 
    ENDIF ! endflag

  ENDDO  ! DO jf

  !* Verify we're at the end of the namcouple, if not STOP (tcraig, june 2012)
  !* The only thing that should be found is a $END, anything ELSE is wrong

  ios=0
  DO WHILE (ios .eq. 0)
     READ(nulin, FMT=rform, END=241) clline
     CALL skip(clline, jpeighty,ios=ios)
     IF (ios .EQ. 0) THEN 
        CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
        IF (trim(clvari) /= "$END") THEN
           WRITE(tmpstr1,*) ' NFIELDS too small, increase it in namcouple'
           WRITE(nulprt1,*) ' NFIELDS too small, increase it in namcouple'
           CALL oasis_flush(nulprt1)
           CALL namcouple_abort(subname,__LINE__,tmpstr1)
        ENDIF
    ELSE
        GOTO 241
    ENDIF
  ENDDO

241 CONTINUE
  IF (mpi_rank_global == 0) THEN
     WRITE(nulprt1,'(a,i6)') ' found namcouple couplings = ',ig_final_nfield
  ENDIF

  IF (ig_nfield.eq.0) THEN
     lg_oasis_field = .false.
     IF (mpi_rank_global == 0) THEN
        WRITE(nulprt1,*)'==> No fields are exchanged via the coupler'
        CALL oasis_flush(nulprt1)
     ENDIF
  ENDIF

  !* Number of different restart files

  allocate (cl_aux(ig_final_nfield))
  cl_aux(:)=' '
  DO jf = 1,ig_final_nfield
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
     IF (il_err.NE.0) CALL prtout('Error: cintmet allocation of '//trim(subname),il_err,1)
     ALLOCATE (naismfl(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: naismfl allocation of '//trim(subname),il_err,1)
     ALLOCATE (naismvoi(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: naismvoi allocation of '//trim(subname),il_err,1)
     ALLOCATE (naisgfl(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: naisgfl allocation of '//trim(subname),il_err,1)
     ALLOCATE (naisgvoi(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: naisgvoi allocation of '//trim(subname),il_err,1)
     cintmet(:)=' '
     naismfl(:) = 1
     naismvoi(:) = 1
     naisgfl(:) = 1
     naisgvoi(:) = 1

     !*          Alloc arrays needed for EXTRAP and initialize them

     ALLOCATE (cextmet(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: cextmet allocation of '//trim(subname),il_err,1)
     ALLOCATE (nninnfl(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nninnfl allocation of '//trim(subname),il_err,1)
     ALLOCATE (nninnflg(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nninnflg allocation of '//trim(subname),il_err,1)
     ALLOCATE (neighbor(ig_nfield), stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: neighbor allocation of '//trim(subname),il_err,1)
     ALLOCATE (nextfl(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nextfl allocation of '//trim(subname),il_err,1)
     cextmet(:)=' '
     nninnfl(:) = 1
     nninnflg(:) = 1
     neighbor(:) = 1
     nextfl(:) = 1

     !*          Alloc arrays needed for BLAS... analyses and initialize them 

     ALLOCATE (nbofld(ig_nfield), stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nbofld allocation of '//trim(subname),il_err,1)
     ALLOCATE (nbnfld(ig_nfield), stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nbnfld allocation of '//trim(subname),il_err,1)
     nbofld(:) = 1
     nbnfld(:) = 1

     !*          Alloc arrays needed for MOZAIC and initialize them

     ALLOCATE (nmapvoi(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nmapvoi allocation of  '//trim(subname),il_err,1)
     ALLOCATE (nmapfl(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nmapfl allocation of '//trim(subname),il_err,1)
     nmapvoi(:) = 1
     nmapfl(:) = 1

     !*          Alloc arrays needed for SUBGRID and initialize them

     ALLOCATE (nsubfl(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nsubfl allocation of '//trim(subname),il_err,1)
     ALLOCATE (nsubvoi(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nsubvoi allocation of '//trim(subname),il_err,1)
     nsubfl(:) = 1
     nsubvoi(:) = 1

     !*          Alloc arrays needed for GLORED and REDGLO and initialize them 

     ALLOCATE (ntronca(ig_nfield), stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: ntronca allocation of '//trim(subname),il_err,1)
     ntronca(:) = 0

     !*          Alloc array needed for analyses parameters 

     ALLOCATE (cficbf(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: cficbf allocation of '//trim(subname),il_err,1)
     cficbf(:)=' '
     ALLOCATE (cficaf(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: cficaf allocation of '//trim(subname),il_err,1)
     cficaf(:)=' '

      !*         Alloc arrays needed for grid dimensions of direct fields and 
      !*         indirect fields

     ALLOCATE (nlonbf(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nlonbf allocation of '//trim(subname),il_err,1)
     nlonbf(:)=0
     ALLOCATE (nlatbf(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nlatbf allocation of '//trim(subname),il_err,1)
     nlatbf(:)=0
     ALLOCATE (nlonaf(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nlonaf allocation of '//trim(subname),il_err,1)
     nlonaf(:)=0
     ALLOCATE (nlataf(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: nlataf allocation of '//trim(subname),il_err,1)
     nlataf(:)=0

     !*         Alloc arrays needed for grid number associated to each field

     ALLOCATE (ig_grid_nbrbf(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: ig_grid_nbrbf allocation of '//trim(subname),il_err,1)
     ig_grid_nbrbf(:)=0
     ALLOCATE (ig_grid_nbraf(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: ig_grid_nbraf allocation of '//trim(subname),il_err,1)
     ig_grid_nbraf(:)=0

     !*          Alloc number of analyses array 

     ALLOCATE (ig_ntrans(ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: ig_ntrans allocation of '//trim(subname),il_err,1)
     ig_ntrans(:)=0
     DO ib = 1, ig_final_nfield
        IF (lg_state(ib)) ig_ntrans(ig_number_field(ib))=ig_total_ntrans(ib)
     ENDDO

     !*          Maximum number of analyses 

     il_maxanal = maxval(ig_ntrans)

     !*          Alloc array of restart file names

     ALLOCATE (cficinp(ig_nfield), stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: cficinp allocation of '//trim(subname),il_err,1)
     cficinp(:)=' '
     DO ib = 1, ig_final_nfield
        IF (lg_state(ib)) cficinp(ig_number_field(ib))=cg_restart_file(ib)
     ENDDO
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
        CALL oasis_flush(nulprt1)
     ENDIF

     !*          Alloc array needed to get analysis names

     ALLOCATE (canal(il_maxanal,ig_nfield),stat=il_err)
     IF (il_err.NE.0) CALL prtout('Error: canal allocation of '//trim(subname),il_err,1)
     canal(:,:)=' '
  ENDIF

  !*      Get analysis parameters 

  keyword = clstring
  CALL findkeyword (keyword, clline, found)
  IF (.not.found) THEN
     WRITE(tmpstr1,*) trim(keyword)//' not found'
     CALL namcouple_abort(subname,__LINE__,tmpstr1)
  ENDIF

  !*      Loop on total number of fields (NoF)

  DO jf=1,ig_final_nfield

     !*        Initialization

     nlonbf_notnc = 0
     nlatbf_notnc = 0
     nlonaf_notnc = 0
     nlataf_notnc = 0

     !*        Skip first line read before

     READ(nulin, FMT=rform) clline
     CALL skip(clline, jpeighty, ios=ios)

     !* Second line

     !* In the indirect case, reading of second, third, fourth line and analyses 
     !* lines

     IF (ig_total_state(jf) .NE. ip_input) THEN
        READ(nulin, FMT=rform) clline
        !*            First determine what information is on the line
        CALL skip(clline, jpeighty, ios=ios)
        CALL parse(clline, clvari, 3, jpeighty, ILEN, __LINE__)
        IF (ILEN .LT. 0) THEN
           !*                IF only two words on the line, THEN they are the locator 
           !*                prefixes and the grids file must be in NetCDF format       
           CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
           IF (lg_state(jf)) cficbf(ig_number_field(jf)) = clvari
           cga_locatorbf(jf) = clvari(1:4)
           CALL parse(clline, clvari, 2, jpeighty, ilen, __LINE__)
           IF (lg_state(jf)) cficaf(ig_number_field(jf)) = clvari
           cga_locatoraf(jf) = clvari(1:4)
           lncdfgrd = .true.
        ELSE 
           READ(clvari, FMT=2010) clind, clequa, iind
           IF (clind .EQ. 'SEQ' .OR. clind .EQ. 'LAG' .AND. clequa .EQ. '=') THEN
              !*                    If 3rd word is an index, THEN first two words are 
              !*                    locator prefixes and grids file must be NetCDF format
              CALL parse(clline, clvari, 1, jpeighty, ILEN, __LINE__) 
              IF (lg_state(jf)) cficbf(ig_number_field(jf)) = clvari 
              cga_locatorbf(jf) = clvari(1:4)
              CALL parse(clline, clvari, 2, jpeighty, ILEN, __LINE__)
              IF (lg_state(jf)) cficaf(ig_number_field(jf)) = clvari
              cga_locatoraf(jf) = clvari(1:4)
              lncdfgrd = .TRUE.
           ELSE
              !*              If not, the first 4 words are grid dimensions and next
              !*              2 words are locator prefixes, and grids file may be or
              !*              not in NetCDF format 
              CALL parse(clline, clvari, 1, jpeighty, ILEN, __LINE__)
              !*                    Get number of longitudes for initial field
              IF (mpi_rank_global == 0) THEN
                 WRITE(nulprt1,*)'CLVARI=',trim(clvari)
                 CALL oasis_flush(nulprt1) 
              ENDIF
              READ(clvari, FMT=2004) nlonbf_notnc
              CALL parse(clline, clvari, 2, jpeighty, ilen, __LINE__)
              !*                    Get number of latitudes for initial field
              READ(clvari, FMT=2004) nlatbf_notnc
              CALL parse(clline, clvari, 3, jpeighty, ilen, __LINE__)
              !*                    Get number of longitudes for final field
              READ(clvari, FMT=2004) nlonaf_notnc
              CALL parse(clline, clvari, 4, jpeighty, ilen, __LINE__)
              !*                    Get number of latitudes for final field
              READ(clvari, FMT=2004) nlataf_notnc
              CALL parse(clline, clvari, 5, jpeighty, ilen, __LINE__)
              !*                    Get root name grid-related files (initial field)
              IF (lg_state(jf)) cficbf(ig_number_field(jf)) = clvari
              cga_locatorbf(jf) = clvari(1:4)
              CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
              !*                    Get root name for grid-related files (final field)
              IF (lg_state(jf)) cficaf(ig_number_field(jf)) = clvari
              cga_locatoraf(jf) = clvari(1:4)
              nlonbf(ig_number_field(jf)) = nlonbf_notnc
              nlatbf(ig_number_field(jf)) = nlatbf_notnc
              nlonaf(ig_number_field(jf)) = nlonaf_notnc
              nlataf(ig_number_field(jf)) = nlataf_notnc

           ENDIF
        ENDIF
          
        !*           Read the P 2 P 0 line for exported, expout or auxilary
          
        IF (lg_state(jf)) THEN
           READ(nulin, FMT=rform) clline
           CALL skip(clline, jpeighty, ios=ios)
        ENDIF
 
        !*            Read next line of strings
        !             --->>> Stuff related to field transformation

        IF (ig_total_ntrans(jf) .GT. 0) THEN 
           READ(nulin, FMT=rform) clline
           CALL skip(clline, jpeighty, ios=ios)
           DO ja = 1, ig_total_ntrans(jf)
              CALL parse(clline, clvari, ja, jpeighty, ILEN, __LINE__)
              !*              Get the whole set of analysis to be performed
              IF (lg_state(jf)) canal(ja,ig_number_field(jf)) = clvari
           ENDDO

           DO ja = 1, ig_total_ntrans(jf)
              IF (lg_state(jf)) THEN
                 cg_c=canal(ja,ig_number_field(jf))
                 IF (mpi_rank_global == 0) THEN
                    WRITE(nulprt1,*)'LG_STATE cg_c=', trim(clline)
                    CALL oasis_flush(nulprt1)
                 ENDIF
                 IF (cg_c .EQ. 'NOINTERP' .OR. cg_c .EQ. 'REDGLO' .OR. cg_c .EQ. 'INVERT' .OR. &
                     cg_c .EQ. 'MASK' .OR. cg_c .EQ. 'EXTRAP' .OR. cg_c .EQ. 'CORRECT' .OR. &
                     cg_c .EQ. 'REDGLO' .OR. cg_c .EQ. 'INTERP' .OR. cg_c .EQ. 'MOZAIC' .OR. &
                     cg_c .EQ. 'FILLING' .OR. cg_c .EQ. 'MASKP' .OR. cg_c .EQ. 'REVERSE' .OR. &
                     cg_c .EQ. 'GLORED') THEN
                    WRITE(tmpstr1,*)' OBSOLETE OPERATION= ',trim(cg_c),' specified in namcouple'
                    CALL namcouple_abort(subname,__LINE__,tmpstr1)
                 ENDIF
                 READ(nulin, FMT=rform) clline
                 CALL skip(clline, jpeighty, ios=ios)
                 IF (canal(ja,ig_number_field(jf)) .EQ. 'SCRIPR')THEN
                    !* Get field type (scalar/vector)
                    CALL parse(clline, clvari, 3, jpeighty, ILEN, __LINE__)
                    READ(clvari, FMT=2009) clstrg
                 ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'BLASOLD') THEN
                    CALL parse(clline, clvari, 2, jpeighty, ILEN, __LINE__)
                    !* Get number of additional fields in linear formula
                    READ(clvari, FMT=2003) nbofld (ig_number_field(jf))
                    DO ib = 1,nbofld (ig_number_field(jf))
                       READ(nulin, FMT=rform) clline
                       CALL skip(clline, jpeighty, ios=ios)
                    ENDDO
                 ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'BLASNEW') THEN
                    CALL parse(clline, clvari, 2, jpeighty, ILEN, __LINE__)
                    !* Get number of additional fields in linear formula
                    READ(clvari, FMT=2003) nbnfld (ig_number_field(jf))
                    DO ib = 1,nbnfld (ig_number_field(jf))
                       READ(nulin, FMT=rform) clline
                       CALL skip(clline, jpeighty, ios=ios)
                    ENDDO
                 ENDIF
              ELSE
                 ! For IGNORED, IGNOUT and OUTPUT, only one line for LOCTRANS
                 READ(nulin, FMT=rform) clline
                 CALL skip(clline, jpeighty, ios=ios)
                 IF (mpi_rank_global == 0) THEN
                    WRITE(nulprt1,*)'OUTPUT clline=', trim(clline)
                    CALL oasis_flush(nulprt1)
                 ENDIF
              ENDIF
           ENDDO   ! DO ja
        ENDIF   ! IF (ig_total_ntrans(jf) .GT. 0) THEN 
     ENDIF   !IF (ig_total_state(jf) .NE. ip_input) THEN
  ENDDO   ! DO jf

  IF (lg_oasis_field) THEN 

     !*       Search maximum number of fields to be combined in the BLASxxx analyses

     ig_maxcomb = MAXVAL(nbofld)
     IF (MAXVAL(nbnfld).GT.ig_maxcomb) ig_maxcomb = MAXVAL(nbnfld)

     !*          Search maximum number of neighbors for GAUSSIAN interpolation

     ig_maxnoa = MAXVAL(naisgvoi)
     IF (mpi_rank_global == 0) THEN
        WRITE(nulprt1,*) 'Max number of neighbors for GAUSSIAN interp : ',ig_maxnoa
        WRITE(nulprt1,*)' '
        CALL oasis_flush(nulprt1)
     ENDIF

     !*          Search maximum number of different GAUSSIAN interpolations

     ig_maxnfg = MAXVAL(naisgfl)
     IF (mpi_rank_global == 0) THEN
        WRITE(nulprt1,*) 'Maximum number of different GAUSSIAN interpolations : ',ig_maxnfg
        WRITE(nulprt1,*)' '
        CALL oasis_flush(nulprt1)
     ENDIF

  ENDIF

  !*    Formats

2001    FORMAT(A9)
2003    FORMAT(I4)
2004    FORMAT(I8)
2009    FORMAT(A8)
2010    FORMAT(A3,A1,I2)

  !*    3. End of routine
  !        --------------
    
  IF (mpi_rank_global == 0) THEN
     WRITE(nulprt1,*)' '
     WRITE(nulprt1,*) subname,'-- End of ROUTINE --'
     CALL oasis_flush (nulprt1)
  ENDIF

!      CALL oasis_debug_exit(subname)
  RETURN

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
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  IMPLICIT NONE

!* ---------------------------- Local declarations --------------------
  
  CHARACTER*5000 clline, clvari
  CHARACTER*9 clword
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
  INTEGER (kind=ip_intwp_p) :: ib,ilind1,ilind2,ilind
  INTEGER (kind=ip_intwp_p) :: ja,jf,jfn,jz,jm,ilen,idum
  INTEGER (kind=ip_intwp_p) :: ifca,ifcb,ilab,jff,jc
  INTEGER (kind=ip_intwp_p) :: icofld,imodel, ios
  CHARACTER(len=32) :: keyword
  LOGICAL :: found
  CHARACTER(len=*),parameter :: subname='(mod_oasis_namcouple:inipar)'

!* ---------------------------- Poema verses --------------------------

!  CALL oasis_debug_enter(subname)

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!*    1. Get basic info for the simulation 
!        ---------------------------------

  IF (mpi_rank_global == 0) THEN
     WRITE(nulprt1,*)' '
     WRITE(nulprt1,*) trim(subname)
     WRITE(nulprt1,*)'   ROUTINE inipar  -  Level 0'
     WRITE(nulprt1,*)'   **************     *******'
     WRITE(nulprt1,*)' '
     WRITE(nulprt1,*)'   Initialization of run parameters'
     WRITE(nulprt1,*)'   Reading input file namcouple'
     WRITE(nulprt1,*)' '
     CALL oasis_flush(nulprt1)
  ENDIF

!* Initialize CHARACTER keywords to locate appropriate input

  !* Initialize some variables 
  ntime = 0 ; niter = 5 
  nstep = 86400 ; nitfn=4

  !* First get experiment name 

  keyword = cljob
  CALL findkeyword (keyword, clline, found)
  IF (found .and. mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) ' ***WARNING*** '//trim(keyword)//' is obsolete in OASIS3-MCT'
  ENDIF
  
  !* Get number of models involved in this simulation
  
  keyword = clmod
  CALL findkeyword (keyword, clline, found)
  IF (found .and. mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) ' ***WARNING*** '//trim(keyword)//' is obsolete in OASIS3-MCT'
  ENDIF

  !* Get hardware info for this OASIS simulation

  keyword = clchan
  CALL findkeyword (keyword, clline, found)
  IF (found .and. mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) ' ***WARNING*** '//trim(keyword)//' is obsolete in OASIS3-MCT'
  ENDIF

  !* Get total time for this simulation

  keyword = cltime
  CALL findkeyword (keyword, clline, found)
  IF (found) THEN
     READ(nulin, FMT=rform) clline
     CALL skip(clline, jpeighty, ios=ios)
     CALL parse (clline, clvari, 1, jpeighty, ilen, __LINE__)
     IF (ilen > 0) THEN
        READ(clvari, FMT=1004) ntime
     ELSE
        WRITE(tmpstr1,*) ' ERROR with value associated with '//trim(keyword)
        CALL namcouple_abort(subname,__LINE__,tmpstr1)
     ENDIF
  ELSE
     WRITE(tmpstr1,*) trim(keyword)//' not found in namcouple'
     CALL namcouple_abort(subname,__LINE__,tmpstr1)
  ENDIF

  !* Print out total time

  CALL prtout('The total time for this run is ntime =', ntime, 1)

  !* Get initial date for this simulation

  keyword = cldate
  CALL findkeyword (keyword, clline, found)
  IF (found .and. mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) ' ***WARNING*** '//trim(keyword)//' is obsolete in OASIS3-MCT'
  ENDIF

   !* Get number of sequential models involved in this simulation

  keyword = clseq
  CALL findkeyword (keyword, clline, found)
  IF (found .and. mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) ' ***WARNING*** '//trim(keyword)//' is obsolete in OASIS3-MCT'
  ENDIF

  !* Get the information mode for this simulation

  keyword = clhead
  CALL findkeyword (keyword, clline, found)
  IF (found .and. mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) ' ***WARNING*** '//trim(keyword)//' is obsolete in OASIS3-MCT'
  ENDIF

  !* Get the printing level for this simulation

  nlogprt = 2
  ntlogprt=0
  keyword = clprint
  CALL findkeyword (keyword, clline, found)
  IF (found) THEN
     READ(nulin, FMT=rform) clline
     CALL skip(clline, jpeighty, ios=ios)
     CALL parse (clline, clvari, 1, jpeighty, ilen, __LINE__)

     IF (ilen .LE. 0) THEN
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) '        ***WARNING*** Nothing on input for '//trim(keyword)
           WRITE(nulprt1,*) ' Default value 2 will be used '
           WRITE(nulprt1,*) ' '
           CALL oasis_flush(nulprt1)
        ENDIF
     ELSE
        READ(clvari, FMT=1004) nlogprt
        CALL parse (clline, clvari, 2, jpeighty, ilen, __LINE__)
        IF (ILEN > 0) THEN
           READ(clvari, FMT=1004) ntlogprt
        ELSE
           IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) '        ***WARNING*** Nothing on input for time statistic for '//trim(keyword)
              WRITE(nulprt1,*) ' Default value 0 will be used '
              WRITE(nulprt1,*) ' '
              CALL oasis_flush(nulprt1)
           ENDIF
        ENDIF
     ENDIF
  ENDIF   ! found

  !* Print out the printing level

  CALL prtout('The printing level is nlogprt =', nlogprt, 1)
  CALL prtout('The time statistics level is ntlogprt =', ntlogprt, 1)

  !* Get the calendar type for this simulation

  keyword = clcal
  CALL findkeyword (keyword, clline, found)
  IF (found .and. mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) ' ***WARNING*** '//trim(keyword)//' is obsolete in OASIS3-MCT'
  ENDIF

  !* Get the allow no restart flag value

  nnorest = .false.
  keyword = clrest
  CALL findkeyword (keyword, clline, found)
  IF (found) THEN
     READ(nulin, FMT=rform) clline
     CALL skip(clline, jpeighty, ios=ios)
     CALL parse (clline, clvari, 1, jpeighty, ilen, __LINE__)
     IF (ilen .LE. 0) THEN
        nnorest = .false.
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) '        ***WARNING*** Nothing on input for '//trim(keyword)
           WRITE(nulprt1,*) ' Default value false will be used '
           WRITE(nulprt1,*) ' '
           CALL oasis_flush(nulprt1)
        ENDIF
     ELSE
        IF (clvari(1:1) == 't' .or. clvari(1:1) == 'T' .or. &
            clvari(1:2) == '.t' .or. clvari(1:2) == '.T') then
           nnorest = .true.
        ENDIF
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) ' read '//trim(clvari)//' for '//trim(keyword)
           WRITE(nulprt1,*) ' set value to ',nnorest
           WRITE(nulprt1,*) ' '
           CALL oasis_flush(nulprt1)
        ENDIF
     ENDIF
  ENDIF  ! found

  !* Print out the allow no restart flag

  if (nnorest) then
     CALL prtout('The allow_no_restart flag is set to true  ',1,1)
  else
     CALL prtout('The allow_no_restart flag is set to false ',0,1)
  endif

  REWIND nulin

  !* Get the unit map decomp value

  nmapdec = nmapdec_default
  keyword = clmapdec
  CALL findkeyword (keyword, clline, found)
  IF (found) THEN
     READ(nulin, FMT=rform) clline
     CALL skip(clline, jpeighty, ios=ios)
     CALL parse (clline, clvari, 1, jpeighty, ilen, __LINE__)
     IF (ilen .LE. 0) THEN
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) '        ***WARNING*** Nothing on input for '//trim(keyword)
           WRITE(nulprt1,*) ' Default value wght will be used '
           WRITE(nulprt1,*) ' '
           CALL oasis_flush(nulprt1)
        ENDIF
     ELSE
        IF (trim(clvari) == 'decomp_1d'    .or. &
            trim(clvari) == 'decomp_wghtfile') THEN
           nmapdec = clvari
        ELSE
           CALL prtout('ERROR in namcouple '//trim(keyword)//' argument',jf,1)
           WRITE(tmpstr1,*) 'ERROR in namcouple '//trim(keyword)//' argument '//TRIM(clvari)
           CALL namcouple_abort(subname,__LINE__,tmpstr1)
        ENDIF
     ENDIF
  ENDIF   ! found

  !* Print out the mapdec value

  IF (mpi_rank_global == 0) THEN
     write(nulprt1,*) ' The mapdec value is nmapdec = ',trim(nmapdec)
  endif

  !* Get the unit matrix read value

  nmatxrd = nmatxrd_default
  keyword = clmatxrd
  CALL findkeyword (keyword, clline, found)
  IF (found) THEN
     READ(nulin, FMT=rform) clline
     CALL skip(clline, jpeighty, ios=ios)
     CALL parse (clline, clvari, 1, jpeighty, ilen, __LINE__)
     IF (ilen .LE. 0) THEN
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) '        ***WARNING*** Nothing on input for '//trim(keyword)
           WRITE(nulprt1,*) ' Default value wght will be used '
           WRITE(nulprt1,*) ' '
           CALL oasis_flush(nulprt1)
        ENDIF
     ELSE
        IF (trim(clvari) == 'orig'    .or. &
            trim(clvari) == 'ceg') THEN
           nmatxrd = clvari
        ELSE
           CALL prtout('ERROR in namcouple '//trim(keyword)//' argument',jf,1)
           WRITE(tmpstr1,*) 'ERROR in namcouple '//trim(keyword)//' argument '//TRIM(clvari)
           CALL namcouple_abort(subname,__LINE__,tmpstr1)
        ENDIF
     ENDIF
  ENDIF   ! found

  !* Print out the matxrd value

  IF (mpi_rank_global == 0) THEN
     write(nulprt1,*) ' The matxrd value is nmatxrd = ',trim(nmatxrd)
  endif

  !* Get the unit weights handling option

  nwgtopt = nwgtopt_default
  keyword = clwgtopt
  CALL findkeyword (keyword, clline, found)
  IF (found) THEN
     READ(nulin, FMT=rform) clline
     CALL skip(clline, jpeighty, ios=ios)
     CALL parse (clline, clvari, 1, jpeighty, ilen, __LINE__)
     IF (ilen .LE. 0) THEN
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) '        ***WARNING*** Nothing on input for '//trim(keyword)
           WRITE(nulprt1,*) ' Default value wght will be used '
           WRITE(nulprt1,*) ' '
           CALL oasis_flush(nulprt1)
        ENDIF
     ELSE
        IF (trim(clvari) == 'abort_on_bad_weights' .or. &
            trim(clvari) == 'ignore_bad_weights' .or. &
            trim(clvari) == 'ignore_bad_weights_silently' .or. &
            trim(clvari) == 'use_bad_weights') THEN
           nwgtopt = clvari
        ELSE
           CALL prtout('ERROR in namcouple '//trim(keyword)//' argument',jf,1)
           WRITE(tmpstr1,*) 'ERROR in namcouple '//trim(keyword)//' argument '//TRIM(clvari)
           CALL namcouple_abort(subname,__LINE__,tmpstr1)
        ENDIF
     ENDIF
  ENDIF   ! found

  !* Print out the wgtopt value

  IF (mpi_rank_global == 0) THEN
     write(nulprt1,*) ' The wgtopt value is nwgtopt = ',trim(nwgtopt)
  endif

  !* Get the unit min/max values

  nuntmin = 1024
  nuntmax = 9999
  keyword = clunit
  CALL findkeyword (keyword, clline, found)
  IF (found) THEN
     READ(nulin, FMT=rform) clline
     CALL skip(clline, jpeighty, ios=ios)
     CALL parse (clline, clvari, 1, jpeighty, ilen, __LINE__)
     IF (ilen .LE. 0) THEN
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) '        ***WARNING*** Nothing on input for '//trim(keyword)
           WRITE(nulprt1,*) ' Default values 1024 and 9999 will be used '
           WRITE(nulprt1,*) ' '
           CALL oasis_flush(nulprt1)
        ENDIF
     ELSE
        READ(clvari, FMT=1004) nuntmin
        CALL parse (clline, clvari, 2, jpeighty, ilen, __LINE__)
        IF (ILEN > 0) THEN
           READ(clvari, FMT=1004) nuntmax
        ELSE
           IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) '        ***WARNING*** Nothing on input for '//trim(keyword)//' max'
              WRITE(nulprt1,*) ' Default value 9999 will be used '
              WRITE(nulprt1,*) ' '
              CALL oasis_flush(nulprt1)
           ENDIF
        ENDIF
     ENDIF
  ENDIF   ! found

  !* Print out the unit number values

  CALL prtout('The min IO unit number is nuntmin =', nuntmin, 1)
  CALL prtout('The max IO unit number is nuntmax =', nuntmax, 1)

  REWIND nulin

  !* Formats

1004 FORMAT(I12)

  !*    2. Get field information
  !        ---------------------

  !* Init. array needed for local transformation  

  ig_local_trans(:) = ip_instant

!SV More cleaning is needed form here on.

!* Init. arrays needed for ANAIS(G-M),mapping and subgrid interpolation

  IF (lg_oasis_field) THEN
     lcoast = .TRUE.
     DO jz = 1, ig_nfield
        linit(jz) = .TRUE.
        lmapp(jz) = .TRUE.
        lsubg(jz) = .TRUE.
        lextra(jz) = .TRUE.
        varmul(jz) = 1.
        lsurf(jz) = .FALSE.
     ENDDO
  ENDIF

!* Get the SSCS for all fields

  keyword = clstring
  CALL findkeyword (keyword, clline, found)
  IF (.not.found) THEN
     WRITE(tmpstr1,*) trim(keyword)//' not found in namcouple'
     CALL namcouple_abort(subname,__LINE__,tmpstr1)
  ENDIF

!  Initialize restart name index

  il_aux = 0

!* Loop on total number of fields (NoF)

  DO jf = 1, ig_final_nfield

!* Read first two lines of strings for field n = 1,2...,ig_final_nfield
!      --->>> Main CHARACTERistics of fields

!* First line
     READ(nulin, FMT=rform) clline
     CALL skip(clline, jpeighty, ios=ios)
     CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
!* Get output field symbolic name
     cg_input_field(jf) = clvari
     IF (lg_state(jf)) cnaminp(ig_number_field(jf)) = cg_input_field(jf)
     IF (lg_state(jf)) cnamout(ig_number_field(jf)) = cg_output_field(jf)
     CALL parse(clline, clvari, 3, jpeighty, ilen, __LINE__)
!* Get field label number
     READ(clvari, FMT=2003) ig_numlab(jf)
     IF (lg_state(jf)) numlab(ig_number_field(jf)) = ig_numlab(jf)
     CALL parse(clline, clvari, 4, jpeighty, ilen, __LINE__)
!* Get field exchange frequency
     IF (clvari(1:4) .EQ. 'ONCE') THEN

!* The case 'ONCE' means that the coupling period will be equal to the 
!* time of the simulation

        ig_freq(jf) = ntime
     ELSE
        READ(clvari, FMT=2004) ig_freq(jf)
        IF (ig_freq(jf) .EQ. 0) THEN

           IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) ' '
           ENDIF
           CALL prtout('ERROR in namcouple for field', jf, 1)
           WRITE(tmpstr1,*) 'The coupling period must not be 0 !'
           WRITE(tmpstr2,*) 'If you DO not want to exchange this field at all'
           WRITE(tmpstr3,*) 'give a coupling period longer than the total run time.'
           CALL namcouple_abort(subname,__LINE__,tmpstr1,tmpstr2,tmpstr3)

        ELSEIF (ig_freq(jf) .gt. ntime) THEN
           IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) '        ***WARNING*** The coupling period of the field ',jf
              WRITE(nulprt1,*) '  is greater than the time of the simulation '
              WRITE(nulprt1,*) '  This field will not be exchanged !'
              CALL oasis_flush(nulprt1)
           ENDIF
        ENDIF
     ENDIF

     IF (lg_state(jf)) nfexch(ig_number_field(jf)) = ig_freq(jf)
!* Fill up restart file number and restart file name arrays
     IF (cg_restart_file(jf).ne.' ') THEN
        IF (jf.eq.1) THEN
           il_aux = il_aux + 1
           ig_no_rstfile(jf) = il_aux
           cg_name_rstfile (ig_no_rstfile(jf)) = cg_restart_file(jf)
        ELSEIF (jf.gt.1) THEN
           IF (ALL(cg_name_rstfile.ne.cg_restart_file(jf))) THEN
              il_aux = il_aux + 1
              ig_no_rstfile(jf) = il_aux
              cg_name_rstfile (ig_no_rstfile(jf)) = cg_restart_file(jf)
           ELSE 
              DO ib = 1, jf - 1 
                 IF (cg_name_rstfile(ig_no_rstfile(ib)).eq.cg_restart_file(jf)) THEN
                     ig_no_rstfile(jf) = ig_no_rstfile(ib)
                 ENDIF
              ENDDO
           ENDIF
        ENDIF
     ENDIF
     CALL parse(clline, clvari, 7, jpeighty, ilen, __LINE__)
!*
!* Get the field STATUS
     IF (clvari(1:8).eq.'EXPORTED' .or.  &
         clvari(1:8).eq.'AUXILARY') THEN
        cstate(ig_number_field(jf)) = clvari
     ELSEIF (clvari(1:6) .eq. 'EXPOUT') THEN
        cstate(ig_number_field(jf)) = 'EXPORTED'
     ENDIF
!*
!* Second line
! XXX Modif Graham ?

     IF (ig_total_state(jf) .ne. ip_input) THEN
        READ(nulin, FMT=rform) clline
!     *      First determine what information is on the line
        CALL skip(clline, jpeighty, ios=ios)
        CALL parse(clline, clvari, 3, jpeighty, ilen, __LINE__)
        IF (ilen .lt. 0) THEN
!     *          IF only two words on the line, THEN they are the locator 
!     *          prefixes and the grids file must be in NetCDF format       
           ig_lag(jf)=0
           ig_total_nseqn(jf)=1
           IF (lg_state(jf)) THEN
              nseqn(ig_number_field(jf)) = 1
              nlagn(ig_number_field(jf)) = 0
           ENDIF
           llseq=.FALSE.
           lllag=.FALSE.
           IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1, FMT=3043) jf
           ENDIF
        ELSE 
           READ(clvari, FMT=2011) clind, clequa, iind
           IF (clind .EQ. 'SEQ' .or. clind .EQ. 'LAG' .and. &
               clequa .EQ. '=') THEN
!     *              If 3rd word is an index, THEN first two words are 
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
           IF (lg_state(jf)) THEN
              nseqn(ig_number_field(jf)) = 1
              nlagn(ig_number_field(jf)) = 0
           ENDIF
           llseq=.FALSE.
           lllag=.FALSE.

           DO ilind=ilind1, ilind2
              CALL parse(clline, clvari, ilind, jpeighty, ilen, __LINE__)
              IF (ilen .eq. -1) THEN
                 IF (mpi_rank_global == 0) THEN
                    IF (nlogprt .GE. 0) THEN 
                       IF (.NOT. lllag) WRITE(nulprt1, FMT=3043) jf
                    ENDIF
                 ENDIF
                 GO TO 247
              ELSE
                 READ(clvari, FMT=2011) clind, clequa, iind
                 IF (clind .EQ. 'SEQ') THEN
                    ig_total_nseqn(jf)=iind
                    IF (lg_state(jf)) nseqn(ig_number_field(jf)) = iind
                    llseq=.TRUE.
                 ELSEIF (clind .eq. 'LAG') THEN
                    ig_lag(jf)=iind
                    IF (lg_state(jf)) nlagn(ig_number_field(jf)) = iind
                    lllag=.TRUE.
                    IF (mpi_rank_global == 0) THEN
                       WRITE(nulprt1, FMT=3044)jf,ig_lag(jf)
                    ENDIF
                 ENDIF              
              ENDIF
           ENDDO  ! DO ilind
        ENDIF
     ENDIF

 247 CONTINUE

!* Third line

     IF (lg_state(jf)) THEN
        READ(nulin, FMT=rform) clline
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,'9 Read line: ',TRIM(clline)
           CALL oasis_flush(nulprt1)
        ENDIF
        CALL skip(clline, jpeighty, ios=ios)
        CALL parse(clline, clvari, 1, jpeighty, ILEN, __LINE__)
        IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,'9 Read line :',TRIM(clline),'  clvari in 1 position: ',TRIM(clvari)
           CALL oasis_flush(nulprt1)
        ENDIF
        !     * Get source grid periodicity type
        csper(ig_number_field(jf)) = clvari
        IF (csper(ig_number_field(jf)) .NE. 'P' .AND.  &
            csper(ig_number_field(jf)) .NE. 'R') THEN
           CALL prtout('ERROR in namcouple for source grid type of field', jf, 1)
           WRITE(tmpstr1,*) '==> must be P or R'
           CALL namcouple_abort(subname,__LINE__,tmpstr1)
        ENDIF

        CALL parse(clline, clvari, 2, jpeighty, ilen, __LINE__)
!     * Get nbr of overlapped longitudes for the Periodic type source grid
        READ(clvari, FMT=2005) nosper(ig_number_field(jf))
        CALL parse(clline, clvari, 3, jpeighty, ilen, __LINE__)
!     * Get target grid periodicity type
        ctper(ig_number_field(jf)) = clvari
        IF (ctper(ig_number_field(jf)) .NE. 'P' .AND.  &
            ctper(ig_number_field(jf)) .NE. 'R') THEN
           CALL prtout('ERROR in namcouple for target grid type of field', jf, 1)
           WRITE(tmpstr1,*) '==> must be P or R'
           CALL namcouple_abort(subname,__LINE__,tmpstr1)
        ENDIF

        CALL parse(clline, clvari, 4, jpeighty, ilen, __LINE__)
!     * Get nbr of overlapped longitudes for the Periodic type target grid
        READ(clvari, FMT=2005) notper(ig_number_field(jf))
!     
     ENDIF

     !* Get the local transformation

     IF (.NOT. lg_state(jf)) THEN
        IF (ig_total_state(jf) .ne. ip_input .and.  &
            ig_total_ntrans(jf) .gt. 0 ) THEN
           READ(nulin, FMT=rform) clline
           CALL skip(clline, jpeighty, ios=ios)
           DO ja=1,ig_total_ntrans(jf)
              READ(nulin, FMT=rform) clline 
              CALL skip(clline, jpeighty, ios=ios)
              CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
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
                 CALL prtout('ERROR in namcouple for local transformations of field', jf, 1)
                 WRITE(tmpstr1,*) '==> Must be INSTANT, AVERAGE, ACCUMUL, T_MIN or T_MAX'
                 CALL namcouple_abort(subname,__LINE__,tmpstr1)
              ENDIF
           ENDDO  ! ja
        ENDIF
     ELSE
! MODIF LC
         IF (ig_total_ntrans(jf) .GT. 0 ) THEN
        READ(nulin, FMT=rform) clline
        CALL skip(clline, jpeighty, ios=ios)

!     * Now read specifics for each transformation
 
        DO ja = 1, ig_ntrans(ig_number_field(jf))

!     * Read next line unless if analysis is NOINTERP (no input)

           READ(nulin, FMT=rform) clline
           CALL skip(clline, jpeighty, ios=ios)
           IF (canal(ja,ig_number_field(jf)) .EQ. 'LOCTRANS') THEN
              CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
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
                 CALL prtout('ERROR in namcouple for local transformations of field', jf, 1)
                 WRITE(tmpstr1,*) '==> Must be INSTANT, AVERAGE, ACCUMUL, T_MIN or T_MAX'
                 CALL namcouple_abort(subname,__LINE__,tmpstr1)
              ENDIF
           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'CHECKIN')THEN
              CALL parse(clline, clvari, 1, jpeighty, ILEN, __LINE__)
           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'CHECKOUT')  THEN
              CALL parse(clline, clvari, 1, jpeighty, ILEN, __LINE__)
           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'MAPPING') THEN
!* Get mapping filename
              CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
              cmap_file(ig_number_field(jf)) = trim(clvari)
!* Get mapping location and/or mapping optimization; src (default), dst; bfb (default), sum, opt
              cmaptyp(ig_number_field(jf)) = 'src'
              cmapopt(ig_number_field(jf)) = 'bfb'
              DO idum = 2,3
                 CALL parse(clline, clvari, idum, jpeighty, ilen, __LINE__)
                 IF (ilen > 0) THEN
                    IF (trim(clvari) == 'src' .or. trim(clvari) == 'dst') THEN
                       cmaptyp(ig_number_field(jf)) = trim(clvari)
                    ELSEIF (trim(clvari) == 'opt' .or. trim(clvari) == 'bfb' .or. &
                            trim(clvari) == 'sum') THEN
                       cmapopt(ig_number_field(jf)) = trim(clvari)
                    ELSE
                       CALL prtout('ERROR in namcouple mapping argument',jf,1)
                       WRITE(tmpstr1,*) 'ERROR in namcouple mapping argument ',TRIM(clvari),' cmaptyp or loc'
                       CALL namcouple_abort(subname,__LINE__,tmpstr1)
                    ENDIF
                 ENDIF
              ENDDO  ! DO idum
           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'SCRIPR') THEN
!* Get Scrip remapping method
              CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
              READ(clvari, FMT=2009) cmap_method(ig_number_field(jf))
!* Get source grid type
              CALL parse(clline, clvari, 2, jpeighty, ilen, __LINE__)
              READ(clvari, FMT=2009) cgrdtyp(ig_number_field(jf))
              IF (cmap_method(ig_number_field(jf)) .eq. 'BICUBIC'  &
                  .and. cgrdtyp(ig_number_field(jf)) .ne. 'LR' &
                  .and. cgrdtyp(ig_number_field(jf)) .ne. 'D') THEN
                 IF (mpi_rank_global == 0) THEN
                    WRITE(nulprt1,*) '    '
                 ENDIF
                 CALL prtout('ERROR in namcouple for type of field', jf, 1)
                 WRITE(tmpstr1,*) 'BICUBIC interpolation cannot be used if grid is not LR or D'
                 CALL namcouple_abort(subname,__LINE__,tmpstr1)
              ENDIF
              IF (cmap_method(ig_number_field(jf)) .eq. 'BILINEAR'  &
                  .and. cgrdtyp(ig_number_field(jf)) .ne. 'LR' &
                  .and. cgrdtyp(ig_number_field(jf)) .ne. 'D') THEN
                 IF (mpi_rank_global == 0) THEN
                    WRITE(nulprt1,*) '    '
                 ENDIF
                 CALL prtout('ERROR in namcouple for type of field', jf, 1)
                 WRITE(tmpstr1,*) 'BILINEAR interpolation cannot be used if grid is not LR or D'
                 CALL namcouple_abort(subname,__LINE__,tmpstr1)
              ENDIF
!* Get field type (scalar/vector)
              CALL parse(clline, clvari, 3, jpeighty, ilen, __LINE__)
              READ(clvari, FMT=2009) cfldtype(ig_number_field(jf))
              IF (cfldtype(ig_number_field(jf)) .EQ. 'VECTOR') cfldtype(ig_number_field(jf))='SCALAR'
              IF (cfldtype(ig_number_field(jf)) .NE. 'SCALAR') THEN
                 IF (mpi_rank_global == 0) THEN
                    WRITE(nulprt1,*) '    '
                 ENDIF
                 CALL prtout('ERROR in namcouple for type of field', jf, 1)
                 WRITE(tmpstr1,*) '==> must be SCALAR, VECTOR'
                 CALL namcouple_abort(subname,__LINE__,tmpstr1)
              ENDIF
!* Get restriction type for SCRIP search
              CALL parse(clline, clvari, 4, jpeighty, ilen, __LINE__)
              READ(clvari, FMT=2009) crsttype(ig_number_field(jf))
              IF (cgrdtyp(ig_number_field(jf)) .EQ. 'D') THEN
                 IF (cmap_method(ig_number_field(jf)) .EQ. 'BILINEAR' .or. &
                     cmap_method(ig_number_field(jf)) .EQ. 'BICUBIC') THEN
                    IF (crsttype(ig_number_field(jf)) .NE. 'LATITUDE') THEN
                       IF (mpi_rank_global == 0) THEN
                          WRITE(nulprt1,*) '    '
                       ENDIF
                       CALL prtout('ERROR in namcouple for restriction of field',jf,1)
                       WRITE(tmpstr1,*) '==> LATITUDE must be chosen for reduced grids (D)'
                       CALL namcouple_abort(subname,__LINE__,tmpstr1)
                    ELSE  
                       crsttype(ig_number_field(jf)) = 'REDUCED'
                    ENDIF
                 ENDIF
              ENDIF

              IF (crsttype(ig_number_field(jf)) .NE. 'LATITUDE' .AND.  &
                  crsttype(ig_number_field(jf)) .NE. 'LATLON' .AND. &
                  crsttype(ig_number_field(jf)) .NE. 'REDUCED') THEN
                 IF (mpi_rank_global == 0) THEN
                    WRITE(nulprt1,*) '    '
                 ENDIF
                 CALL prtout('ERROR in namcouple for restriction of field',jf,1)
                 WRITE(tmpstr1,*) '==> must be LATITUDE or LATLON'
                 CALL namcouple_abort(subname,__LINE__,tmpstr1)
              ENDIF
!*
!* Get number of search bins for SCRIP search
              CALL parse(clline, clvari, 5, jpeighty, ilen, __LINE__)
              READ(clvari, FMT=2003) nbins(ig_number_field(jf))
!* Get normalize option for CONSERV
              IF (cmap_method(ig_number_field(jf)) .EQ. 'CONSERV') THEN
                 CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
                 READ(clvari, FMT=2009)cnorm_opt(ig_number_field(jf))
                 IF (cnorm_opt(ig_number_field(jf)) .NE. 'FRACAREA' .AND. &
                     cnorm_opt(ig_number_field(jf)) .NE. 'DESTAREA' .AND.  &
                     cnorm_opt(ig_number_field(jf)) .NE. 'FRACNNEI') THEN
                    IF (mpi_rank_global == 0) THEN
                       WRITE(nulprt1,*) '    '
                    ENDIF
                    CALL prtout('ERROR in namcouple for normalize option of field',jf,1)
                    WRITE(tmpstr1, *) '==> must be FRACAREA, DESTAREA, or FRACNNEI'
                    CALL namcouple_abort(subname,__LINE__,tmpstr1)
                 ENDIF
!* Get order of remapping for CONSERV
                 CALL parse(clline, clvari, 7, jpeighty, ilen, __LINE__)
                 IF (ilen .LE. 0) THEN
                    IF (mpi_rank_global == 0) THEN
                       WRITE(nulprt1,*) '    '
                    ENDIF
                    CALL prtout('ERROR in namcouple for CONSERV for field',jf,1)
                    WRITE(tmpstr1,*) '==> FIRST must be indicated at end of line'
                    CALL namcouple_abort(subname,__LINE__,tmpstr1)
                 ENDIF
                 READ(clvari, FMT=2009) corder(ig_number_field(jf))                   
              ELSE
                 cnorm_opt(ig_number_field(jf))='NONORM'
              ENDIF
!* Get number of neighbours for DISTWGT and GAUSWGT
              IF (cmap_method(ig_number_field(jf)) .EQ. 'DISTWGT' .or. &
                  cmap_method(ig_number_field(jf)) .EQ. 'GAUSWGT') THEN
                 CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
                 IF (ilen .LE. 0) THEN
                    IF (mpi_rank_global == 0) THEN
                       WRITE(nulprt1,*) '    '
                    ENDIF
                    CALL prtout('ERROR in namcouple for field',jf,1)
                    WRITE(tmpstr1,*) '==> Number of neighbours must be indicated on the line'
                    CALL namcouple_abort(subname,__LINE__,tmpstr1)
                 ELSE
                    READ(clvari, FMT=2003)nscripvoi(ig_number_field(jf))
                 ENDIF 
              ENDIF
!* Get gaussian variance for GAUSWGT
              IF (cmap_method(ig_number_field(jf)) .EQ. 'GAUSWGT') THEN
                 CALL parse(clline, clvari, 7, jpeighty, ilen, __LINE__)
                 IF (ilen .LE. 0) THEN
                    IF (mpi_rank_global == 0) THEN
                       WRITE(nulprt1,*) '    '
                    ENDIF
                    CALL prtout('ERROR in namcouple for GAUSWGT for field',jf,1)
                    WRITE(tmpstr1,*) '==> Variance must be indicated at end of line'
                    CALL namcouple_abort(subname,__LINE__,tmpstr1)
                 ELSE
                    READ(clvari, FMT=2006) varmul(ig_number_field(jf))
                 ENDIF
              ENDIF

           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'FILLING') THEN
              CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
!     * Get data file name (used to complete the initial field array)
              cfilfic(ig_number_field(jf)) = clvari
              CALL parse(clline, clvari, 2, jpeighty, ilen, __LINE__)
!     * Get logical unit connected to previous file
              READ(clvari, FMT=2005) nlufil(ig_number_field(jf))
              CALL parse(clline, clvari, 3, jpeighty, ilen, __LINE__)
!     * Get filling method
              cfilmet(ig_number_field(jf)) = clvari
!     * If current field is SST
              IF (cfilmet(ig_number_field(jf))(4:6) .EQ. 'SST') THEN
                 CALL parse(clline, clvari, 4, jpeighty, ilen, __LINE__)
!     * Get flag for coast mismatch correction
                 READ(clvari, FMT=2005) nfcoast
                 IF (cfilmet(ig_number_field(jf))(1:3) .EQ. 'SMO') THEN
                    CALL parse(clline, clvari, 5, jpeighty, ilen, __LINE__)
!     * Get field name for flux corrective term 
                    cfldcor = clvari
                    CALL parse(clline, clvari, 6, jpeighty, ilen, __LINE__)
!     * Get logical unit used to write flux corrective term
                    READ(clvari, FMT=2005) nlucor
                 ENDIF
              ENDIF 
           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'CONSERV') THEN            
              CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
!     * Get conservation method
              cconmet(ig_number_field(jf)) = clvari
              lsurf(ig_number_field(jf)) = .TRUE.
              CALL parse(clline, clvari, 2, jpeighty, ilen, __LINE__)
              cconopt(ig_number_field(jf)) = 'bfb'
              IF (ilen > 0) THEN
                 IF (trim(clvari) == 'bfb'    .or. trim(clvari) == 'opt'    .or. &
                     trim(clvari) == 'lsum8'  .or. trim(clvari) == 'lsum16' .or. &
                     trim(clvari) == 'gather' .or. trim(clvari) == 'ddpdd'  .or. &
                     trim(clvari) == 'reprosum') THEN
                    cconopt(ig_number_field(jf)) = clvari
                 ELSE
                    CALL prtout('ERROR in namcouple conserv argument',jf,1)
                    WRITE(tmpstr1,*) 'ERROR in namcouple conserv argument '//TRIM(clvari)
                    CALL namcouple_abort(subname,__LINE__,tmpstr1)
                 ENDIF
              ENDIF
           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'BLASOLD')THEN
!     * Get linear combination parameters for initial fields
              CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
!     * Get main field multiplicative coefficient
              READ(clvari, FMT=2006) afldcobo(ig_number_field(jf))
              DO jc = 1, nbofld(ig_number_field(jf))
                 READ(nulin, FMT=rform) clline   
                 CALL skip(clline, jpeighty)
                 CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
!     * Get symbolic names for additional fields
                 cbofld(jc,ig_number_field(jf)) = clvari
                 CALL parse(clline, clvari, 2, jpeighty, ilen, __LINE__)
!     * Get multiplicative coefficients for  additional fields
                 READ(clvari, FMT=2006) abocoef (jc,ig_number_field(jf))
              ENDDO  ! DO jc
           ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'BLASNEW')THEN
!     * Get linear combination parameters for final fields
              CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
!     * Get main field multiplicative coefficient
              READ(clvari, FMT=2006) afldcobn(ig_number_field(jf))
              DO jc = 1, nbnfld(ig_number_field(jf))
                 READ(nulin, FMT=rform) clline   
                 CALL skip(clline, jpeighty)
                 CALL parse(clline, clvari, 1, jpeighty, ilen, __LINE__)
!     * Get symbolic names for additional fields
                 cbnfld(jc,ig_number_field(jf)) = clvari
                 CALL parse(clline, clvari, 2, jpeighty, ilen, __LINE__)
!     * Get multiplicative coefficients for  additional fields
                 READ(clvari, FMT=2006) abncoef (jc,ig_number_field(jf))
              ENDDO  ! DO jc
           ELSE 
              WRITE(tmpstr1,*) ' Type of analysis not implemented yet '
              WRITE(tmpstr2,*) ' The analysis required in OASIS is :'
              WRITE(tmpstr3,*) ' canal = ', canal(ja,ig_number_field(jf))
              WRITE(tmpstr4,*) ' with ja = ', ja, ' jf = ', jf
              CALL namcouple_abort(subname,__LINE__,tmpstr1,tmpstr2,tmpstr3,tmpstr4)
           ENDIF
        ENDDO  ! DO ja
! MODIF LC
       ENDIF
     ENDIF

!* End of loop on NoF
 
  ENDDO  ! DO jf

!* Minimum coupling period

  ig_total_frqmin = minval(ig_freq)

!* Formats

2001 FORMAT(A9)
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
     IF (nlogprt .GE. 0) THEN 
        DO jf = 1, ig_final_nfield
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
              WRITE(nulprt1, FMT=3001) jf
              WRITE(nulprt1, FMT=3002)
              WRITE(nulprt1, FMT=3003)
              WRITE(nulprt1, FMT=3004)
              IF (ig_total_state(jf) .eq. ip_input .or.  &
                  ig_total_state(jf) .eq. ip_output) THEN
                 WRITE(nulprt1, FMT=3121) &
                    cg_input_field(jf), cg_output_field(jf),  &
                    ig_freq(jf), cl_print_trans, &
                    cl_print_state, ig_total_ntrans(jf)
              ELSE  
                 WRITE(nulprt1, FMT=3116) &
                    cg_input_field(jf), cg_output_field(jf),  &
                    ig_freq(jf), cl_print_trans, ig_total_nseqn(jf),  &
                    ig_lag(jf), cl_print_state, ig_total_ntrans(jf)
              ENDIF
           ELSE
              ilab = numlab(ig_number_field(jf))
              ifcb = len_trim(cficbf(ig_number_field(jf)))
              ifca = len_trim(cficaf(ig_number_field(jf)))
              WRITE(nulprt1, FMT=3001) jf
              WRITE(nulprt1, FMT=3002)
              WRITE(nulprt1, FMT=3003)
              WRITE(nulprt1, FMT=3004) 
              WRITE(nulprt1, FMT=3005) &
                 TRIM(cnaminp(ig_number_field(jf))),  &
                 TRIM(cnamout(ig_number_field(jf))), &
                 nfexch(ig_number_field(jf)), &
                 nseqn(ig_number_field(jf)), &
                 ig_lag(jf), &
                 cl_print_state, &
                 ig_ntrans(ig_number_field(jf))
           ENDIF

           IF (.not. lg_state(jf)) THEN
              IF (ig_total_state(jf) .eq. ip_ignored .or.  &
                  ig_total_state(jf) .eq. ip_ignout ) THEN
                 WRITE(nulprt1, FMT=3117) cg_restart_file(jf)
              ELSEIF (ig_total_state(jf) .eq. ip_input) THEN
                 WRITE(nulprt1, FMT=3118) cg_input_file(jf)
              ENDIF
           ELSE
              IF (ig_total_state(jf) .eq. ip_exported .or.  &
                  ig_total_state(jf) .eq. ip_expout .or.  &
                  ig_total_state(jf) .eq. ip_auxilary ) &
                 WRITE(nulprt1, FMT=3117) cg_restart_file(jf)

              WRITE(nulprt1, FMT=3007) &
                 csper(ig_number_field(jf)), nosper(ig_number_field(jf)),  &
                 ctper(ig_number_field(jf)), notper(ig_number_field(jf))
              WRITE(nulprt1, FMT=3008) &
                 cficbf(ig_number_field(jf))(1:ifcb)//cglonsuf,  &
                 cficbf(ig_number_field(jf))(1:ifcb)//cglatsuf, &
                 cficbf(ig_number_field(jf))(1:ifcb)//cmsksuf,  &
                 cficbf(ig_number_field(jf))(1:ifcb)//csursuf, &
                 cficaf(ig_number_field(jf))(1:ifca)//cglonsuf,  &
                 cficaf(ig_number_field(jf))(1:ifca)//cglatsuf, &
                 cficaf(ig_number_field(jf))(1:ifca)//cmsksuf,  &
                 cficaf(ig_number_field(jf))(1:ifca)//csursuf
              WRITE(nulprt1, FMT=3009) 
              WRITE(nulprt1, FMT=3010)
              DO ja = 1, ig_ntrans(ig_number_field(jf))
                 WRITE(nulprt1, FMT=3011) ja, canal(ja,ig_number_field(jf))
                 IF (canal(ja,ig_number_field(jf)) .EQ. 'MAPPING') THEN
                    write(nulprt1, FMT=3048) &
                       trim(cmap_file(ig_number_field(jf))), &
                       trim(cmaptyp(ig_number_field(jf))), &
                       trim(cmapopt(ig_number_field(jf)))
                 ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'SCRIPR') THEN
                    WRITE(nulprt1, FMT=3045)  &
                       cmap_method(ig_number_field(jf)),  &
                       cfldtype(ig_number_field(jf)),  &
                       cnorm_opt(ig_number_field(jf)), &
                       crsttype(ig_number_field(jf)),  &
                       nbins(ig_number_field(jf))
                    IF (cmap_method(ig_number_field(jf)) .EQ. 'CONSERV') THEN 
                       WRITE(nulprt1, FMT=3046) corder(ig_number_field(jf))
                    ENDIF  
                 ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'CONSERV') THEN            
                    WRITE(nulprt1, FMT=3025)  &
                       cconmet(ig_number_field(jf)),  &
                       cconopt(ig_number_field(jf))
                 ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'BLASOLD') THEN
                    WRITE(nulprt1, FMT=3027)  &
                       trim(cnaminp(ig_number_field(jf))),  &
                       afldcobo(ig_number_field(jf))
                    WRITE(nulprt1, FMT=3028) nbofld(ig_number_field(jf))
                    DO jc = 1, nbofld(ig_number_field(jf))
                       WRITE(nulprt1, FMT=3030)  &
                          cbofld(jc,ig_number_field(jf)),  &
                          abocoef (jc,ig_number_field(jf))
                    ENDDO
                 ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'BLASNEW') THEN
                    WRITE(nulprt1, FMT=3027)  &
                       trim(cnamout(ig_number_field(jf))),  &
                       afldcobn(ig_number_field(jf))
                    WRITE(nulprt1, FMT=3028) nbnfld(ig_number_field(jf))
                    DO jc = 1, nbnfld(ig_number_field(jf))
                       WRITE(nulprt1, FMT=3030)  &
                          cbnfld(jc,ig_number_field(jf)),  &
                          abncoef (jc,ig_number_field(jf))
                    ENDDO
                 ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'CHECKIN') THEN
                    WRITE(nulprt1,*) '   '
                 ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'CHECKOUT') THEN
                    WRITE(nulprt1,*) '   '
                 ELSEIF (canal(ja,ig_number_field(jf)) .EQ. 'LOCTRANS') THEN
                    WRITE(nulprt1, FMT=3047) cl_print_trans
                 ELSE 
                    WRITE(tmpstr1,*) ' Type of analysis not implemented yet '
                    WRITE(tmpstr2,*) ' The analysis required in OASIS is :'
                    WRITE(tmpstr3,*) ' canal = ',canal(ja,ig_number_field(jf))
                    WRITE(tmpstr4,*) ' with ja = ', ja, ' jf = ', jf
                    CALL namcouple_abort(subname,__LINE__,tmpstr1,tmpstr2,tmpstr3,tmpstr4)
                 ENDIF
              ENDDO  ! DO ja
           ENDIF
        ENDDO  ! DO jf
     ENDIF
  ENDIF

!*    4. End of routine
!        --------------

  IF (mpi_rank_global == 0) THEN
     IF (nlogprt .GE. 0) THEN
        WRITE(nulprt1,*)' '
        WRITE(nulprt1,*) subname,'-- End of ROUTINE --'
        CALL oasis_flush (nulprt1)
     ENDIF
  ENDIF
!  CALL oasis_debug_exit(subname)
  RETURN

!* Formats

 3001 FORMAT(//,15X,'  FIELD NUMBER ',I3)
 3002 FORMAT(15X,'  ************  ')
 3003 FORMAT(/,10X,'  Field parameters ')
 3004 FORMAT(10X,'  ****************  ',/)
 3005 FORMAT(/,10X,'  Input field symbolic name       = ',A, &
             /,10X,'  Output field symbolic name      = ',A, &
             /,10X,'  Field exchange frequency        = ',I8, &
             /,10X,'  Model sequential index          = ',I2, &
             /,10X,'  Field Lag                       = ',I8, &
             /,10X,'  Field I/O status                = ',A8, &
             /,10X,'  Number of basic operations      = ',I4, /)
 3116 FORMAT(/,10X,'  Input field symbolic name       = ',A8, &
             /,10X,'  Output field symbolic name      = ',A8, &
             /,10X,'  Field exchange frequency        = ',I8, &
             /,10X,'  Local transformation            = ',A8, &
             /,10X,'  Model sequential index          = ',I2, &
             /,10X,'  Field Lag                       = ',I8,  &
             /,10X,'  Field I/O status                = ',A8, &
             /,10X,'  Number of basic operations      = ',I4,/)
 3117 FORMAT(/,10X,'  Restart file name               = ',A32,/)
 3118 FORMAT(/,10X,'  Input file name                 = ',A32,/)
 3121 FORMAT(/,10X,'  Input field symbolic name       = ',A8, &
             /,10X,'  Output field symbolic name      = ',A8, &
             /,10X,'  Field exchange frequency        = ',I8, &
             /,10X,'  Local transformation            = ',A8, &
             /,10X,'  Field I/O status                = ',A8, &
             /,10X,'  Number of basic operations      = ',I4,/)
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
 3025 FORMAT(5X,' Conservation method for field is  = ',A8, &
           /,5X,' Conservation option is            = ',A8)
 3027 FORMAT(5X,' Field ',A,' is multiplied by Cst = ',E15.6)
 3028 FORMAT(5X,' It is combined with N fields    N = ',I2)
 3030 FORMAT(5X,'   With field ',A8,'   coefficient = ',E15.6)
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

END SUBROUTINE inipar

!===============================================================================
 
SUBROUTINE alloc()

  IMPLICIT NONE

  CHARACTER(len=*),parameter :: subname='(mod_oasis_namcouple:alloc)'

!  CALL oasis_debug_enter(subname)

  !--- alloc_anais1
  ALLOCATE (varmul(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "varmul"allocation of anais module',il_err,1)
  varmul(:)=0
  ALLOCATE (niwtm(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "niwtm"allocation of anais module',il_err,1)
  niwtm(:)=0
  ALLOCATE (niwtg(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "niwtg"allocation of anais module',il_err,1)
  niwtg(:)=0
  allocate (linit(ig_nfield), stat=il_err)
  IF (il_err.ne.0) CALL prtout('error in "linit"allocation of anais module',il_err,1)
  linit(:)=.false.

  !--- alloc_analysis
  ALLOCATE (ncofld(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ncofld"allocation of analysis module',il_err,1)
  ncofld(:)=0
  ALLOCATE (neighborg(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "neighborg"allocation of analysis module',il_err,1)
  neighborg(:)=0
  ALLOCATE (nludat(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nludat"allocation of analysis module',il_err,1)
  nludat(:,:)=0
  ALLOCATE (nlufil(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nlufil"allocation of analysis module',il_err,1)
  nlufil(:)=0
  ALLOCATE (nlumap(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nlumap"allocation of analysis module',il_err,1)
  nlumap(:)=0
  ALLOCATE (nlusub(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nlusub"allocation of analysis module',il_err,1)
  nlusub(:)=0
  ALLOCATE (nluext(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nluext"allocation of analysis module',il_err,1)
  nluext(:)=0
  ALLOCATE (nosper(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nosper"allocation of analysis module',il_err,1)
  nosper(:)=0
  ALLOCATE (notper(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "notper"allocation of analysis module',il_err,1)
  notper(:)=0
  ALLOCATE (amskval(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "amskval"allocation of analysis module',il_err,1)
  amskval(:)=0
  ALLOCATE (amskvalnew(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "amskvalnew"allocation of analysis module',il_err,1)
  amskvalnew(:)=0
  ALLOCATE (acocoef(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "acocoef"allocation of analysis module',il_err,1)
  acocoef(:,:)=0
  ALLOCATE (abocoef(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "abocoef"allocation of analysis module',il_err,1)
  abocoef(:,:)=0
  ALLOCATE (abncoef(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "abncoef"allocation of analysis module',il_err,1)
  abncoef(:,:)=0
  ALLOCATE (afldcoef(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "afldcoef"allocation of analysis module',il_err,1)
  afldcoef(:)=0
  ALLOCATE (afldcobo(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "afldcobo"allocation of analysis module',il_err,1)
  afldcobo(:)=0
  ALLOCATE (afldcobn(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "afldcobn"allocation of analysis module',il_err,1)
  afldcobn(:)=0
  ALLOCATE (cxordbf(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cxordbf"allocation of analysis module',il_err,1)
  cxordbf(:)=' '
  ALLOCATE (cyordbf(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cyordbf"allocation of analysis module',il_err,1)
  cyordbf(:)=' '
  ALLOCATE (cxordaf(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cxordaf"allocation of analysis module',il_err,1)
  cxordaf(:)=' '
  ALLOCATE (cyordaf(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cyordaf"allocation of analysis module',il_err,1)
  cyordaf(:)=' '
  ALLOCATE (cgrdtyp(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cgrdtyp"allocation of analysis module',il_err,1)
  cgrdtyp(:)=' '
  ALLOCATE (cfldtyp(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfldtyp"allocation of analysis module',il_err,1)
  cfldtyp(:)=' '
  ALLOCATE (cfilfic(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfilfic"allocation of analysis module',il_err,1)
  cfilfic(:)=' '
  ALLOCATE (cfilmet(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfilmet"allocation of analysis module',il_err,1)
  cfilmet(:)=' '
  ALLOCATE (cconmet(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cconmet"allocation of analysis module',il_err,1)
  cconmet(:)=' '
  ALLOCATE (cconopt(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cconopt"allocation of analysis module',il_err,1)
  cconopt(:)=' '
  ALLOCATE (cfldcoa(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfldcoa"allocation of analysis module',il_err,1)
  cfldcoa(:)=' '
  ALLOCATE (cfldfin(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfldfin"allocation of analysis module',il_err,1)
  cfldfin(:)=' '
  ALLOCATE (ccofld(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ccofld"allocation of analysis module',il_err,1)
  ccofld(:,:)=' '
  ALLOCATE (cbofld(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cbofld"allocation of analysis module',il_err,1)
  cbofld(:,:)=' '
  ALLOCATE (cbnfld(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cbnfld"allocation of analysis module',il_err,1)
  cbnfld(:,:)=' '
  ALLOCATE (ccofic(ig_maxcomb,ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ccofic"allocation of analysis module',il_err,1)
  ccofic(:,:)=' '
  ALLOCATE (cdqdt(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cdqdt"allocation of analysis module',il_err,1)
  cdqdt(:)=' '
  ALLOCATE (cgrdmap(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cgrdmap"allocation of analysis module',il_err,1)
  cgrdmap(:)=' '
  ALLOCATE (cmskrd(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cmskrd"allocation of analysis module',il_err,1)
  cmskrd(:)=' '
  ALLOCATE (cgrdsub(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cgrdsub"allocation of analysis module',il_err,1)
  cgrdsub(:)=' '
  ALLOCATE (ctypsub(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ctypsub"allocation of analysis module',il_err,1)
  ctypsub(:)=' '
  ALLOCATE (cgrdext(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cgrdext"allocation of analysis module',il_err,1)
  cgrdext(:)=' '
  ALLOCATE (csper(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "csper"allocation of analysis module',il_err,1)
  csper(:)=' '
  ALLOCATE (ctper(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ctper"allocation of analysis module',il_err,1)
  ctper(:)=' '
  ALLOCATE (lsurf(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "lsurf"allocation of analysis module',il_err,1)
  lsurf(:)=.false.
  ALLOCATE (nscripvoi(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in nscripvoi allocation of analysis module',il_err,1)
  nscripvoi(:)=0
! 
!* Alloc array needed for SCRIP 
!
  ALLOCATE (cmap_method(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cmap_method" allocation of '//trim(subname),il_err,1)
  cmap_method(:)=' '
  ALLOCATE (cmap_file(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cmap_file" allocation of '//trim(subname),il_err,1)
  cmap_file(:)=' '
  ALLOCATE (cmaptyp(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cmaptyp" allocation of '//trim(subname),il_err,1)
  cmaptyp(:)=' '
  ALLOCATE (cmapopt(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cmapopt" allocation of '//trim(subname),il_err,1)
  cmapopt(:)=' '
  ALLOCATE (cfldtype(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfldtype"allocation of '//trim(subname),il_err,1)
  cfldtype(:)=' '
  ALLOCATE (crsttype(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "crsttype"allocation of '//trim(subname),il_err,1)
  crsttype(:)=' '
  ALLOCATE (nbins(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nbins"allocation of '//trim(subname),il_err,1)
  nbins(:)=0
  ALLOCATE (cnorm_opt(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cnorm_opt"allocation of '//trim(subname),il_err,1)
  cnorm_opt(:)=' '
  ALLOCATE (corder(ig_nfield),stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "corder"allocation of '//trim(subname),il_err,1)
  corder(:)=' '
!
  !--- alloc_extrapol1
  ALLOCATE (niwtn(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "niwtn"allocation of extrapol module',il_err,1)
  niwtn(:)=0
  ALLOCATE (niwtng(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "niwtng"allocation of extrapol module',il_err,1)
  niwtng(:)=0
  ALLOCATE (lextra(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "lextra"allocation of extrapol module',il_err,1)
  lextra(:)=.false.
  ALLOCATE (lweight(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "lweight"allocation of extrapol module',il_err,1)
  lweight(:)=.false.

  !--- alloc_rainbow1
  ALLOCATE (lmapp(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "lmapp"allocation of rainbow module',il_err,1)
  lmapp(:)=.false.
  ALLOCATE (lsubg(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "lsubg"allocation of rainbow module',il_err,1)
  lsubg(:)=.false.

  !--- alloc_string
  ALLOCATE (cg_name_rstfile(ig_nbr_rstfile), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cg_name_rstfile"allocation of string module',il_err,1)
  cg_name_rstfile(:)=' '
  ALLOCATE (ig_lag(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_lag"allocation of string module',il_err,1) 
  ig_lag(:)=0
  ALLOCATE (ig_no_rstfile(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_no_rstfile"allocation of string module',il_err,1)
  ig_no_rstfile(:)=1
  ALLOCATE (cg_input_field(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cg_input_field"allocation of string module',il_err,1)
  cg_input_field(:)=' '
  ALLOCATE (ig_numlab(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_numlab"allocation of string module',il_err,1)
  ig_numlab(:)=0
  ALLOCATE (ig_freq(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_freq"allocation of string module',il_err,1)
  ig_freq(:)=0
  ALLOCATE (ig_total_nseqn(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_total_nseqn"allocation of string module',il_err,1)
  ig_total_nseqn(:)=0
  ALLOCATE (ig_local_trans(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_local_trans"allocation of string module',il_err,1)
  ig_local_trans(:)=0
  ALLOCATE (ig_invert(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_invert" allocation of string module',il_err,1) 
  ig_invert(:)=0
  ALLOCATE (ig_reverse(ig_total_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_reverse" allocation of string module',il_err,1) 
  ig_reverse(:)=0
!
!** + Allocate following arrays only if one field (at least) goes
!     through Oasis
!
  IF (lg_oasis_field) THEN
  ALLOCATE (numlab(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "numlab"allocation of string module',il_err,1)
  numlab(:)=0
  ALLOCATE (nfexch(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nfexch"allocation of string module',il_err,1)
  nfexch(:)=0
  ALLOCATE (nseqn(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nseqn"allocation of string module',il_err,1)
  nseqn(:)=0
  ALLOCATE (nlagn(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nlagn" allocation of string module',il_err,1)
  nlagn(:)=0
  ALLOCATE (cnaminp(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cnaminp"allocation of string module',il_err,1)
  cnaminp(:)=' '
  ALLOCATE (cnamout(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cnamout"allocation of string module',il_err,1)
  cnamout(:)=' '
  ALLOCATE (cficout(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cficout"allocation of string module',il_err,1)
  cficout(:)=' '
  ALLOCATE (cstate(ig_nfield), stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cstate"allocation of string module',il_err,1)
  cstate(:)=' '
  ENDIF

!  CALL oasis_debug_exit(subname)

END SUBROUTINE alloc

!===============================================================================

SUBROUTINE dealloc

  IMPLICIT NONE

  CHARACTER(len=*),parameter :: subname='(mod_oasis_namcouple:dealloc)'

  !--- alloc_anais1
  DEALLOCATE (varmul, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "varmul"deallocation of anais module',il_err,1)
  DEALLOCATE (niwtm, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "niwtm"deallocation of anais module',il_err,1)
  DEALLOCATE (niwtg, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "niwtg"deallocation of anais module',il_err,1)
  deallocate (linit, stat=il_err)
  IF (il_err.ne.0) CALL prtout('error in "linit"deallocation of anais module',il_err,1)

  !--- alloc_analysis
  DEALLOCATE (ncofld, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ncofld"deallocation of analysis module',il_err,1)
  DEALLOCATE (neighborg, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "neighborg"deallocation of analysis module',il_err,1)
  DEALLOCATE (nludat, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nludat"deallocation of analysis module',il_err,1)
  DEALLOCATE (nlufil, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nlufil"deallocation of analysis module',il_err,1)
  DEALLOCATE (nlumap, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nlumap"deallocation of analysis module',il_err,1)
  DEALLOCATE (nlusub, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nlusub"deallocation of analysis module',il_err,1)
  DEALLOCATE (nluext, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nluext"deallocation of analysis module',il_err,1)
  DEALLOCATE (nosper, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nosper"deallocation of analysis module',il_err,1)
  DEALLOCATE (notper, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "notper"deallocation of analysis module',il_err,1)
  DEALLOCATE (amskval, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "amskval"deallocation of analysis module',il_err,1)
  DEALLOCATE (amskvalnew, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "amskvalnew"deallocation of analysis module',il_err,1)
  DEALLOCATE (acocoef, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "acocoef"deallocation of analysis module',il_err,1)
  DEALLOCATE (abocoef, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "abocoef"deallocation of analysis module',il_err,1)
  DEALLOCATE (abncoef, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "abncoef"deallocation of analysis module',il_err,1)
  DEALLOCATE (afldcoef, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "afldcoef"deallocation of analysis module',il_err,1)
  DEALLOCATE (afldcobo, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "afldcobo"deallocation of analysis module',il_err,1)
  DEALLOCATE (afldcobn, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "afldcobn"deallocation of analysis module',il_err,1)
  DEALLOCATE (cxordbf, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cxordbf"deallocation of analysis module',il_err,1)
  DEALLOCATE (cyordbf, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cyordbf"deallocation of analysis module',il_err,1)
  DEALLOCATE (cxordaf, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cxordaf"deallocation of analysis module',il_err,1)
  DEALLOCATE (cyordaf, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cyordaf"deallocation of analysis module',il_err,1)
  DEALLOCATE (cgrdtyp, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cgrdtyp"deallocation of analysis module',il_err,1)
  DEALLOCATE (cfldtyp, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfldtyp"deallocation of analysis module',il_err,1)
  DEALLOCATE (cfilfic, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfilfic"deallocation of analysis module',il_err,1)
  DEALLOCATE (cfilmet, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfilmet"deallocation of analysis module',il_err,1)
  DEALLOCATE (cconmet, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cconmet"deallocation of analysis module',il_err,1)
  DEALLOCATE (cconopt, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cconopt"deallocation of analysis module',il_err,1)
  DEALLOCATE (cfldcoa, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfldcoa"deallocation of analysis module',il_err,1)
  DEALLOCATE (cfldfin, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfldfin"deallocation of analysis module',il_err,1)
  DEALLOCATE (ccofld, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ccofld"deallocation of analysis module',il_err,1)
  DEALLOCATE (cbofld, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cbofld"deallocation of analysis module',il_err,1)
  DEALLOCATE (cbnfld, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cbnfld"deallocation of analysis module',il_err,1)
  DEALLOCATE (ccofic, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ccofic"deallocation of analysis module',il_err,1)
  DEALLOCATE (cdqdt, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cdqdt"deallocation of analysis module',il_err,1)
  DEALLOCATE (cgrdmap, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cgrdmap"deallocation of analysis module',il_err,1)
  DEALLOCATE (cmskrd, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cmskrd"deallocation of analysis module',il_err,1)
  DEALLOCATE (cgrdsub, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cgrdsub"deallocation of analysis module',il_err,1)
  DEALLOCATE (ctypsub, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ctypsub"deallocation of analysis module',il_err,1)
  DEALLOCATE (cgrdext, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cgrdext"deallocation of analysis module',il_err,1)
  DEALLOCATE (csper, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "csper"deallocation of analysis module',il_err,1)
  DEALLOCATE (ctper, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ctper"deallocation of analysis module',il_err,1)
  DEALLOCATE (lsurf, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "lsurf"deallocation of analysis module',il_err,1)
  DEALLOCATE (nscripvoi, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in nscripvoi deallocation of analysis module',il_err,1)
! 
!* Alloc array needed for SCRIP 
!
  DEALLOCATE (cmap_method,stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cmap_method" deallocation of scrip module',il_err,1)
  DEALLOCATE (cmap_file,stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cmap_file" deallocation of scrip module',il_err,1)
  DEALLOCATE (cmaptyp,stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cmaptyp" deallocation of scrip module',il_err,1)
  DEALLOCATE (cmapopt,stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cmapopt" deallocation of scrip module',il_err,1)
  DEALLOCATE (cfldtype,stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cfldtype"deallocation of scrip module',il_err,1)
  DEALLOCATE (crsttype,stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "crsttype"deallocation of scrip module',il_err,1)
  DEALLOCATE (nbins,stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nbins"deallocation of scrip module',il_err,1)
  DEALLOCATE (cnorm_opt,stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cnorm_opt"deallocation of scrip module',il_err,1)
  DEALLOCATE (corder,stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "corder"deallocation of scrip module',il_err,1)
  !
  !--- alloc_extrapol1
  DEALLOCATE (niwtn, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "niwtn"deallocation of extrapol module',il_err,1)
  DEALLOCATE (niwtng, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "niwtng"deallocation of extrapol module',il_err,1)
  DEALLOCATE (lextra, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "lextra"deallocation of extrapol module',il_err,1)
  DEALLOCATE (lweight, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "lweight"deallocation of extrapol module',il_err,1)

  !--- alloc_rainbow1
  DEALLOCATE (lmapp, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "lmapp"deallocation of rainbow module',il_err,1)
  DEALLOCATE (lsubg, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "lsubg"deallocation of rainbow module',il_err,1)

  !--- alloc_string
  DEALLOCATE (cg_name_rstfile, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cg_name_rstfile"deallocation of string module',il_err,1)
  DEALLOCATE (ig_lag, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_lag"deallocation of string module',il_err,1) 
  DEALLOCATE (ig_no_rstfile, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_no_rstfile"deallocation of string module',il_err,1)
  DEALLOCATE (cg_input_field, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cg_input_field"deallocation of string module',il_err,1)
  DEALLOCATE (ig_numlab, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_numlab"deallocation of string module',il_err,1)
  DEALLOCATE (ig_freq, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_freq"deallocation of string module',il_err,1)
  DEALLOCATE (ig_total_nseqn, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_total_nseqn"deallocation of string module',il_err,1)
  DEALLOCATE (ig_local_trans, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_local_trans"deallocation of string module',il_err,1)
  DEALLOCATE (ig_invert, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_invert" deallocation of string module',il_err,1) 
  DEALLOCATE (ig_reverse, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "ig_reverse" deallocation of string module',il_err,1) 
!
!** + Deallocate following arrays only if one field (at least) goes
!     through Oasis
!
  IF (lg_oasis_field) THEN
  DEALLOCATE (numlab, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "numlab"deallocation of string module',il_err,1)
  DEALLOCATE (nfexch, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nfexch"deallocation of string module',il_err,1)
  DEALLOCATE (nseqn, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nseqn"deallocation of string module',il_err,1)
  DEALLOCATE (nlagn, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "nlagn" deallocation of string module',il_err,1)
  DEALLOCATE (cnaminp, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cnaminp"deallocation of string module',il_err,1)
  DEALLOCATE (cnamout, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cnamout"deallocation of string module',il_err,1)
  DEALLOCATE (cficout, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cficout"deallocation of string module',il_err,1)
  DEALLOCATE (cstate, stat=il_err)
  IF (il_err.NE.0) CALL prtout('Error in "cstate"deallocation of string module',il_err,1)
  ENDIF

!  CALL oasis_debug_exit(subname)

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
!     Print out CHARACTER string and one INTEGER value
!
!**   Interface:
!     ---------
!       *CALL*  *prtout(cdtext, kvalue, kstyle)*
!
!     Input:
!     -----
!                cdtext : CHARACTER string to be printed
!                kvalue : INTEGER variable to be printed
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

  INTEGER(kind=ip_intwp_p) :: ilen,jl
  CHARACTER*69 cline
  CHARACTER(len=*),PARAMETER :: cbase = '-'
  CHARACTER(len=*),PARAMETER :: cprpt = '* ===>>> :'
  CHARACTER(len=*),PARAMETER :: cdots = '  ------  '
  CHARACTER(len=*),parameter :: subname='(mod_oasis_namcouple:prtout)'

!* ---------------------------- Poema verses ----------------------------

!  CALL oasis_debug_enter(subname)

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!*    1. Print CHARACTER string + INTEGER value
!        --------------------------------------

  IF (mpi_rank_global == 0) THEN
     IF ( kstyle .EQ. 1 .OR. kstyle .EQ. 2) THEN
        cline = ' '
        ilen = len(cdtext)
        DO jl = 1, ILEN
           cline(jl:jl) = cbase
        ENDDO
          
        IF ( kstyle .EQ. 2 ) THEN
           WRITE(nulprt1, FMT='(/,A,1X,A)') cdots, cline
        ENDIF
        WRITE(nulprt1, FMT='(A,1X,A,1X,I18)') cprpt, cdtext, kvalue
        WRITE(nulprt1, FMT='(A,1X,A,/)') cdots, cline
     ELSE
        WRITE(nulprt1, FMT='(/,A,1X,A,1X,I18,/)') cprpt, cdtext, kvalue
     ENDIF

!*    2. End of routine
!        --------------

     CALL oasis_flush(nulprt1)
  ENDIF

!  CALL oasis_debug_exit(subname)

END SUBROUTINE prtout

!===============================================================================

SUBROUTINE findkeyword (keyword, line, found)

!****
!               *****************************
!               * OASIS ROUTINE  -  LEVEL T *
!               * -------------     ------- *
!               *****************************
!
!**** *findkeyword*  - Searches for a particular keyword in the namcouple file
!
!     Purpose:
!     -------
!     Find the line with the specified keyword
!
!**   Interface:
!     ---------
!       *CALL*  *findkeyword (keyword, line, found) *
!
!     Input:
!     -----
!                keyword : string to search for (CHARACTER string)
!
!     Output:
!     ------
!                line    : full line that contains the keyword (CHARACTER string)
!                found   : flag indicating whether keyword was found (logical)
!
!     History:
!     -------
!       Version   Programmer     Date      Description
!       -------   ----------     ----      -----------  
!       3.3       T. Craig     2016/08/02  created
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
  CHARACTER (len=*)        , INTENT ( in) :: keyword
  CHARACTER (len=*)        , INTENT (out) :: line
  LOGICAL                  , INTENT (out) :: found
!
!* ---------------------------- Local declarations -------------------
!
  CHARACTER (len=jpeighty) :: clline
  CHARACTER (len=jpeighty) :: clvari
  INTEGER (kind=ip_intwp_p):: ILEN, ios, ios2
  CHARACTER(len=*),parameter :: subname='(mod_oasis_namcouple:findkeyword)'
!
!* ---------------------------- Poema verses ----------------------------

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!  CALL oasis_debug_enter(subname)

  found = .FALSE.
  ios2 = 0 

  REWIND nulin
  DO WHILE (.not.found)
     READ(nulin, FMT=rform, END=110, IOSTAT=ios2) clline
     if (ios2 == 0) then
       CALL skip(clline,jpeighty, ios=ios)
!      write(nulprt1,*) trim(subname),'tcx1: ',trim(clline)
       IF (ios == 0) THEN
         CALL parse(clline, clvari, 1, jpeighty, ILEN, __LINE__)
!        write(nulprt1,*) trim(subname),'tcx2: ',trim(clvari),trim(keyword)
         IF (clvari == ADJUSTL(keyword)) THEN
             line = clline
             found = .TRUE.
         ENDIF
       ENDIF
     else
       goto 110
     endif
  ENDDO

110 CONTINUE

!  CALL oasis_debug_exit(subname)
  return

END SUBROUTINE findkeyword

!===============================================================================

LOGICAL FUNCTION checkcomment (line)

!****
!               *****************************
!               * OASIS ROUTINE  -  LEVEL T *
!               * -------------     ------- *
!               *****************************
!
!**** *checkcomment*  - Checks whether the line is a comment line (starts with # or is blank)
!
!     Purpose:
!     -------
!     Check whether a line is a comment line or a blank line
!
!**   Interface:
!     ---------
!       flag = checkcomment (line) *
!
!     Input:
!     -----
!                line : full line to test
!
!     Output:
!     ------
!                checkcomment : flag indicating whether line is a comment line or not
!
!     History:
!     -------
!       Version   Programmer     Date      Description
!       -------   ----------     ----      -----------  
!       3.3       T. Craig     2016/08/02  created
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
  CHARACTER (len=*)        , INTENT (in) :: line
!
!* ---------------------------- Local declarations -------------------
!
  CHARACTER (len=jpeighty) :: clline
  CHARACTER (len=1), SAVE :: clblank = ' ', clcmt = '#'
  CHARACTER(len=*),parameter :: subname='(mod_oasis_namcouple:checkcomment)'
!
!* ---------------------------- Poema verses ----------------------------

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!  CALL oasis_debug_enter(subname)

  checkcomment = .false.

  clline = adjustl(line)
  IF (clline(1:1) == clcmt .or. len_trim(clline) == 0) THEN
     checkcomment = .true.
  ENDIF

!  CALL oasis_debug_exit(subname)
  return

END FUNCTION checkcomment

!===============================================================================

SUBROUTINE parse (cdone, cdtwo, knumb, klen, kleng, line)

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
!     A string is defined as a continuous set of non-blanks CHARACTERs
!
!**   Interface:
!     ---------
!       *CALL*  *parse (cdone, cdtwo, knumb, klen, kleng)*
!
!     Input:
!     -----
!                cdone : line to be parsed (char string)
!                knumb : rank within the line of the extracted string (INTEGER)
!                klen  : length of the input line (INTEGER)
!                line  : line where parse is called (INTEGER)
!
!     Output:
!     ------
!                cdtwo : extracted CHARACTER string (char string)
!                kleng : length of the extracted string (INTEGER)
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
!       3.3       T. Craig     2016/08/02  updated
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
  INTEGER (kind=ip_intwp_p), INTENT ( in) :: knumb, klen, line
  CHARACTER (len=*), INTENT ( inout) :: cdone 
  CHARACTER (len=*), INTENT ( out) :: cdtwo
  INTEGER (kind=ip_intwp_p), INTENT ( out) :: kleng
!
!* ---------------------------- Local declarations -------------------
!
  INTEGER(kind=ip_intwp_p) :: ii,jl
  CHARACTER (len=klen) :: clline
  CHARACTER (len=klen) :: clwork
  CHARACTER (len=1), SAVE :: clblank = ' ', clcmt = '#'
  CHARACTER(len=*),parameter :: subname='(mod_oasis_namcouple:parse)'
!
!* ---------------------------- Poema verses ----------------------------

!  CALL oasis_debug_enter(subname)
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!* - Abort if line is a comment

  IF (checkcomment(cdone)) THEN
     write(tmpstr1,*) subname,' ERROR comment line found:', trim(cdone)
     write(tmpstr1,*) subname,' ERROR called from :',line
     CALL namcouple_abort(subname,__LINE__,tmpstr1)
  ENDIF

!* - DO the extraction job
!* - Fill temporary string and remove leading blanks

  clwork = ADJUSTL (cdone)

!* - Fill cdtwo with blanks

  cdtwo = clblank

!* - If there are no more CHARACTERs, kleng=-1

  IF ( LEN_TRIM ( clwork) .LE. 0) THEN
     kleng = -1
!     CALL oasis_debug_exit(subname)
     RETURN
  ENDIF

!* - If this is the one we're looking for, skip
!    otherwise go knumb-1 more sets of CHARACTERs

  IF (knumb .GE. 2) THEN
     DO jl = 1, knumb-1
        ii = INDEX ( clwork, clblank) - 1
        clwork ( 1:ii) = clblank
        clwork = ADJUSTL ( clwork)

!* - If there are no more CHARACTERs, kleng=-1

        IF (LEN_TRIM ( clwork) .LE. 0) THEN
           kleng = -1
!           CALL oasis_debug_exit(subname)
           RETURN
        ENDIF
     ENDDO
  ENDIF

!* - Find the length of this set of CHARACTERs

  kleng = INDEX ( clwork, clblank) - 1

!* - Copy to cdtwo

  cdtwo ( 1:kleng) = clwork ( 1: kleng)

!  CALL oasis_debug_exit(subname)

  return

END SUBROUTINE parse

!===============================================================================

SUBROUTINE skip (cd_one, id_len, endflag, ios)

!**** SKIP
!
!     Purpose:
!       Find next non-comment line
!
!     Interface: 
!       CALL skip (cd_one, id_len, endflag)
!
!     Method:
!       Read the first CHARACTER of the line and skip line if 
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
!       3.3       T. Craig       2016/08/02  updated
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
  CHARACTER(len=*),intent(inout)       :: cd_one
  INTEGER (kind=ip_intwp_p),intent(in) :: id_len
  LOGICAL, optional, intent(inout)     :: endflag
  INTEGER (kind=ip_intwp_p), OPTIONAL, INTENT(out) :: ios
!
!** ++ Local declarations
!
  CHARACTER(len=id_len) :: cl_line
  LOGICAL :: found
  CHARACTER(len=*),parameter :: subname='(mod_oasis_namcouple:skip)'
!
!*-----------------------------------------------------------------------
!
!  CALL oasis_debug_enter(subname)

  IF (present(endflag)) endflag = .false.
  IF (present(ios)) ios = 0
  cl_line = cd_one
  found = .false.

  DO WHILE (.not.found)
     IF (checkcomment(cl_line)) THEN
        READ(nulin, FMT=rform, END=140, IOSTAT=ios) cl_line
     ELSE
        found = .true.
     ENDIF
  ENDDO

140 CONTINUE

  IF (found) THEN
     cd_one = cl_line
  ELSE
     IF (present(endflag)) ENDFLAG = .true.
     IF (PRESENT(ios)) ios = -1
  ENDIF

!  CALL oasis_debug_exit(subname)
  RETURN

END SUBROUTINE skip

!*========================================================================

SUBROUTINE namcouple_abort(isubname,lineno,string1,string2,string3,string4)

  ! * Reusable Abort routine

  IMPLICIT NONE
  CHARACTER(len=*),intent(in) :: isubname
  INTEGER         ,intent(in) :: lineno
  CHARACTER(len=*),intent(in),optional :: string1
  CHARACTER(len=*),intent(in),optional :: string2
  CHARACTER(len=*),intent(in),optional :: string3
  CHARACTER(len=*),intent(in),optional :: string4

!** ++ Local declarations

  CHARACTER(len=*),parameter :: subname='(mod_oasis_namcouple:namcouple_abort)'

!*-----------------------------------------------------------------------

!  CALL oasis_debug_enter(subname)

  IF (mpi_rank_global == 0) THEN
     WRITE(nulprt1,*) ' '
     WRITE(nulprt1,*) subname,' calling ABORT'
     WRITE(nulprt1,*) ' **** ABORT from ',trim(isubname),' line number ',lineno
     IF (present(string1)) WRITE(nulprt1,*) ' **** ',trim(isubname),' : ',trim(string1)
     IF (present(string2)) WRITE(nulprt1,*) ' **** ',trim(isubname),' : ',trim(string2)
     IF (present(string3)) WRITE(nulprt1,*) ' **** ',trim(isubname),' : ',trim(string3)
     IF (present(string4)) WRITE(nulprt1,*) ' **** ',trim(isubname),' : ',trim(string4)
     WRITE(nulprt1,*) ' '
     CALL oasis_flush(nulprt1)
  ENDIF
  call oasis_abort(file=__FILE__,line=__LINE__)

!  CALL oasis_debug_enter(subname)

  RETURN

END SUBROUTINE namcouple_abort

!===============================================================================
!===============================================================================

END MODULE mod_oasis_namcouple


