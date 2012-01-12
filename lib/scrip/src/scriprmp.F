C
      SUBROUTINE scriprmp (dst_array, src_array, src_size, dst_size,
     $                     src_mask, dst_mask,
     $                     src_lon, src_lat, nlon_src, nlat_src,
     $                     dst_lon, dst_lat, nlon_dst, nlat_dst,
     $                     map_method, cdgrdtyp,
     $                     id_sper, id_tper, cd_sper, cd_tper,
     $                     src_name, dst_name,
     $                     normalize_opt, order, rst_type, n_srch_bins,
     $                     lextrapdone, rl_varmul, id_scripvoi,
     $                     jf, num_field)
C*****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *scriprmp* - SCRIP remapping
C
C     Purpose:
C     -------
C     Main routine of SCRIP remapping
C        - finds out, whether remapping matrix exists
C        - drives calculation of missing remapping matrices
C        - performs matrix multiplication
C
C     Interface:
C     ---------
C       *CALL*  *scriprmp (dst_array, src_array, src_size, dst_size, 
C                          src_mask, dst_mask, 
C                          src_lon, src_lat, nlon_src, nlat_src,
C                          dst_lon, dst_lat, nlon_dst, nlat_dst, 
C                          map_method, cdgrdtyp, 
C                          id_sper, id_tper, cd_sper, cd_tper, 
C                          src_name, dst_name, 
C                          normalize_opt, order, rst_type, n_srch_bins,
C                          lextrapdone, rl_varmul)
C
C     Called from:
C     -----------
C     interp
C
C     Input:
C     -----
C             src_array : field on source grid(real 1D)
C             src_size  : source grid size (integer)
C             dst_size  : target grid size (integer)
C             src_mask  : source grid mask (INTEGER)
C             dst_mask  : target grid mask (INTEGER)
C             src_lon   : source grid longitudes (real 1D)
C             src_lat   : source grid latitudes (real 1D)
C             nlon_src  : number of source grid longitudes (integer)
C             nlat_src  : number of source grid latitudes (integer)
C             dst_lon   : target grid longitudes (real 1D)
C             dst_lat   : target grid latitudes (real 1D)
C             nlon_dst  : number of destination grid longitudes (integer)
C             nlat_dst  : number of destination grid latitudes (integer)
C             map_method: remapping method (character*8)
C             cdgrdtyp  : source grid type (character*8)
C             id_sper   : number of overlapping for source grid
C             id_tper   : number of overlapping for target grid
C             cd_sper   : source grid periodicity type
C             cd_tper   : target grid periodicity type
C             src_name  : source grid name (character*8)
C             dst_name  : target grid name (character*8)
C             normalize_opt: option for normalization (character*8)
C             order     : order of conservative remapping (character*8)
C             rst_type : type of scrip search restriction (character*8)
C             n_srch_bins : number of seach bins (integer)
C             lextrapdone : logical, true if EXTRAP done on field
C             rl_varmul : Gaussian variance (for GAUSWGT)
C             id_scripvoi : number of neighbour for DISTWGT and GAUSWGT
C
C     Output:
C     ------
C             dst_array : field on target grid (real 1D)
C
C     Externals:
C     ---------
C     corners, scrip, gradient, gradient_bilin
C
C     History:
C     -------
C       Version   Programmer     Date        Description
C       -------   ----------     ----        -----------  
C       2.0       V.Gayler       2001/11/09  created
C       2.5       D.Declat       2002/07/08  completed
C       2.5       D.Declat       2002/08/01  the mask of the tgt grid 
C                                             taken into account
C       2.5       D.Declat       2002/08/09  'NONE' and 'CONSERV' for conserv
C                                            check pole
C       3.3       CMCC           2010        Remapping weights are read once 
C                                            for each field and stored in global
C                                            arrays for further iterations. 
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C* ---------------------------- Modules used ----------------------------
C
      USE grids
      USE remap_vars
      USE mod_parameter
      USE mod_scrip
C
C* ---------------------------- Implicit --------------------------------
C
      IMPLICIT NONE
C
C* ---------------------------- Include files ---------------------------
C
#include <netcdf.inc>
C
C* ---------------------------- Intent In -------------------------------
C
      INTEGER (kind=int_kind) ::
     $     nlon_src,            ! number of source grid longitudes
     $     nlat_src,            ! number of source grid latitudes
     $     nlon_dst,            ! number of destination grid longitudes
     $     nlat_dst,            ! number of destination grid latitudes
     $     src_size,            ! number of source grid cells
     $     dst_size,            ! number of destination grid cells
     $     n_srch_bins,         ! number of search bins fos SCRIP
     $     src_mask(src_size),  ! source grid mask
     $     dst_mask(dst_size),  ! target grid mask
     $     id_sper,             ! number of overlapping points for source grid
     $     id_tper              ! number of overlapping points for target grid
C
      REAL (kind=real_kind) ::
     $     src_array(src_size), ! source grid array
     $     src_lon(src_size),   ! source grid longitudes
     $     src_lat(src_size),   ! source grid latitudes
     $     dst_lon(dst_size),   ! target grid longitudes
     $     dst_lat(dst_size)    ! target grid latitudes
C
      CHARACTER*8 ::
     $     map_method,          ! remapping method
     $     cdgrdtyp,            ! source grid type
     $     src_name,            ! source grid name
     $     dst_name,            ! target grid name
     $     normalize_opt,       ! option for normalization
     $     order,               ! order of conservative remapping
     $     rst_type,            ! type of scrip search restriction
     $     cd_sper,             ! source grid periodicity type
     $     cd_tper              ! target grid periodicity type
C
      LOGICAL ::
     $     lextrapdone          ! logical, true if EXTRAP done on field
C
      REAL (kind=real_kind) ::
     $     rl_varmul            ! Gaussian variance (for GAUSWGT)
C
      INTEGER (kind=int_kind) ::
     $     id_scripvoi          ! number of neighbour for DISTWGT and GAUSWGT
C
C* ---------------------------- Intent Out -------------------------------
C
      REAL (kind=real_kind):: 
     $     dst_array(dst_size)  ! array on destination grid
      REAL (kind=real_kind):: 
     $     weightot(dst_size)  !  sum of weights
C
C* ---------------------------- Local declarations ----------------------
C
      CHARACTER*12 ::
     $     cweight              ! string for weights
C
      CHARACTER*11 ::
     $     csrcadd,             ! string for source grid addresses
     $     cdstadd              ! string for destination grid addresses
C
      CHARACTER*13 ::
     $     cdstare,             ! string for destination grid area
     $     cdstfra              ! string for destination grid frac
C
      CHARACTER (char_len) ::
     $     crmpfile,            ! name of the SCRIP matrix file
     $     cmapping             ! mapping name
C
      INTEGER (kind=int_kind) ::
     $     n,                   ! looping indicee
     $     ncrn_src, ncrn_dst,  ! number of grid cell corners
     $     src_rank, dst_rank,  ! source / target grid rank
     $     src_dims(2),         ! source grid dimensions
     $     dst_dims(2),         ! target grid dimensions
     $     sou_mask(src_size),   ! source grid mask
     $     tgt_mask(dst_size)   ! target grid mask
C
      REAL (kind=real_kind) ::
     $     dst_area(dst_size),  ! target grid area
     $     dst_frac(dst_size),  ! target grid frac
     $     dst_err(dst_size)    ! target grid error
C
C*    netCDF-declarations
      INTEGER (kind=int_kind) ::
     $     stat,                ! netCDF error status
     $     nc_scpid,             ! file id
     $     dimid,               ! dimension id
     $     varid                ! variable id
C
      LOGICAL ::
     $     lcalc,               ! calculate matrix?
     $     ll_weightot          ! grid point with no interpolated value = 1.0E+20
C
      REAL (kind=real_kind), DIMENSION(:), ALLOCATABLE :: 
     $     gradient_lat,        ! latitudinal gradient (conservative rmp.)
     $     gradient_lon,        ! longitudinal gradient (conservative rmp.)
     $     gradient_i,          ! gradient in i direction (bilinear rmp.)
     $     gradient_j,          ! gradient in j direction (bilinear rmp.)
     $     gradient_ij          ! cross gradient (bilinear rmp.)
C
      REAL (kind=real_kind), DIMENSION (:,:), ALLOCATABLE ::
     $     src_corner_lon,      ! longitudes of source grid corners
     $     src_corner_lat,      ! latitudes of source grid corners
     $     dst_corner_lon,      ! longitudes of destination grid corners
     $     dst_corner_lat       ! latitudes of destination grid corners
C
      CHARACTER*8 :: cl_tgt     ! indicates target grid 
C
#if defined use_oasis_para || defined use_oasis_cmcc_para
      CHARACTER*3 :: cl_indexoa
#endif
      INTEGER jf
      INTEGER num_field
C
C*    pole contribution
      REAL (kind=real_kind) ::
     $     moy_tmp_S, moy_tmp_N, ! field average at the pole
     $     moy_err_S, moy_err_N, ! error average at the pole
     $     latpol_N, latpol_S     
C
      INTEGER (kind=int_kind) ::
     $     compt_S, compt_N      ! number of cells representating a pole
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *)' '
          WRITE (UNIT = nulou,FMT = *)'   Entering ROUTINE scriprmp '
          WRITE (UNIT = nulou,FMT = *)'   ****************  ******* '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' SCRIP remapping'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL FLUSH(nulou)
      ENDIF
C
C* -- get the name of the file containig the remapping matrix
C
#if defined use_oasis_para || defined use_oasis_cmcc_para
      IF (ig_indexoa .le. 9) THEN
          WRITE(cl_indexoa,FMT='(I1)') ig_indexoa
      ELSE IF (ig_indexoa .le. 99) THEN
          WRITE(cl_indexoa,FMT='(I2)') ig_indexoa
      ELSE IF (ig_indexoa .le. 999) THEN
          WRITE(cl_indexoa,FMT='(I3)') ig_indexoa
      ENDIF
#endif
C
      SELECT CASE (map_method)
      CASE ('CONSERV')          ! conservative remapping
       cmapping = 
     $     src_name(1:4)//' to '//dst_name(1:4)//' '//map_method//
     $     ' '//normalize_opt(1:4)//' remapping'
#if defined use_oasis_para || defined use_oasis_cmcc_para
      IF (ig_indexoa .le. 9) THEN
          crmpfile = 
     $        'rmp_'//src_name(1:4)//'_to_'//dst_name(1:4)//'_'//
     $        map_method(1:7)//'_'//normalize_opt(1:8)//
     $        '_'//cl_indexoa(1:1)//'.nc'
      ELSE IF (ig_indexoa .le. 99) THEN
          crmpfile = 
     $        'rmp_'//src_name(1:4)//'_to_'//dst_name(1:4)//'_'//
     $        map_method(1:7)//'_'//normalize_opt(1:8)//
     $        '_'//cl_indexoa(1:2)//'.nc'
      ELSE IF (ig_indexoa .le. 999) THEN
          crmpfile = 
     $        'rmp_'//src_name(1:4)//'_to_'//dst_name(1:4)//'_'//
     $        map_method(1:7)//'_'//normalize_opt(1:8)//
     $        '_'//cl_indexoa(1:3)//'.nc'
      ENDIF
#else
       crmpfile = 
     $      'rmp_'//src_name(1:4)//'_to_'//dst_name(1:4)//'_'//
     $      map_method(1:7)//'_'//normalize_opt(1:8)//'.nc'
#endif
C
      CASE DEFAULT
       cmapping = 
     $     src_name(1:4)//' to '//dst_name(1:4)//' '//map_method//
     $     ' remapping'
#if defined use_oasis_para || defined use_oasis_cmcc_para
      IF (ig_indexoa .le. 9) THEN
          crmpfile = 
     $        'rmp_'//src_name(1:4)//'_to_'//dst_name(1:4)//'_'//
     $        map_method(1:7)//'_'//cl_indexoa(1:1)//'.nc'
      ELSE IF (ig_indexoa .le. 99) THEN
          crmpfile = 
     $        'rmp_'//src_name(1:4)//'_to_'//dst_name(1:4)//'_'//
     $        map_method(1:7)//'_'//cl_indexoa(1:2)//'.nc'
      ELSE IF (ig_indexoa .le. 999) THEN
          crmpfile = 
     $        'rmp_'//src_name(1:4)//'_to_'//dst_name(1:4)//'_'//
     $        map_method(1:7)//'_'//cl_indexoa(1:3)//'.nc'
      ENDIF
#else
      crmpfile = 
     $    'rmp_'//src_name(1:4)//'_to_'//dst_name(1:4)//'_'//
     $    map_method(1:7)//'.nc'
#endif
      END SELECT
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) 
     $    ' SCRIP filename : ', crmpfile
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF

C****
C
C* -- character strings of weights and addresses
C
      csrcadd = 'src_address'
      cdstadd = 'dst_address'
      cweight = 'remap_matrix'
      cdstare = 'dst_grid_area'
      cdstfra = 'dst_grid_frac'
C
C* -- logical to calculate matrix
C
      lcalc = .TRUE.
C
C* -- find out whether remapping file exists
C
      stat = NF_OPEN(crmpfile, NF_NOWRITE, nc_scpid)
      IF (stat == NF_NOERR) THEN 
         lcalc = .FALSE.
C
         IF (nlogprt .GE. 2) THEN
             WRITE (UNIT = nulou,FMT = *) 
     $       ' SCRIP file opened - no matrix calculation needed' 
             WRITE (UNIT = nulou,FMT = *) ' '
             CALL flush(nulou)
         ENDIF
      ENDIF
C
C*    -- setting of the mask for the source AND the target grid
C
      WHERE (src_mask .eq. 1)
          sou_mask = 0
      END WHERE
      WHERE (src_mask .eq. 0)
          sou_mask = 1
      END WHERE
C
      WHERE (dst_mask .eq. 1)
          tgt_mask = 0
      END WHERE
      WHERE (dst_mask .eq. 0)
          tgt_mask = 1
      END WHERE

      IF (lcalc) THEN           
C
C*    2. Calculate SCRIP remapping matrix
C        --------------------------------
C
          write(nulou,*) "scriprmp tcx1 generating weights"
          SELECT CASE (normalize_opt)
          CASE ('FRACNNEI')
              normalize_opt = 'FRACAREA'
              lfracnnei = .true.
          END SELECT
C
          IF (nlogprt .GE. 2) THEN
              WRITE (UNIT = nulou,FMT = *) 
     $            ' Calculation of SCRIP remapping matrix: method = ',
     $            map_method
              WRITE (UNIT = nulou,FMT = *) ' '
              CALL FLUSH(nulou)
          ENDIF

C*    -- get grid cell corners for conservative remapping
C
         ncrn_src = 4.
         ncrn_dst = 4.
C
         ALLOCATE(src_corner_lon(ncrn_src,src_size),
     $       src_corner_lat(ncrn_src,src_size),
     $       dst_corner_lon(ncrn_dst,dst_size),
     $       dst_corner_lat(ncrn_dst,dst_size))

         IF (map_method == 'CONSERV') THEN
C
             CALL corners(nlon_src, nlat_src, ncrn_src, 
     $           src_lon, src_lat,
     $           src_name, cdgrdtyp, id_sper, cd_sper,
     $           src_corner_lon, src_corner_lat)
             cl_tgt='TARGETGR' 
             CALL corners(nlon_dst, nlat_dst, ncrn_dst, 
     $           dst_lon, dst_lat,
     $           dst_name, cl_tgt, id_tper, cd_tper,
     $           dst_corner_lon, dst_corner_lat)
         ENDIF
C
C*    -- initialization of grid arrays for SCRIP
C
         src_dims(1) = nlon_src
         src_dims(2) = nlat_src
         dst_dims(1) = nlon_dst
         dst_dims(2) = nlat_dst
         src_rank = 2
         dst_rank = 2
! Modifier car src_rank et dst_rank n'est pas toujours =2.
C
C
         CALL grid_init(map_method, rst_type, n_srch_bins,
     $                  src_size, dst_size, src_dims, dst_dims,
     $                  src_rank, dst_rank, ncrn_src, ncrn_dst,
     $                  sou_mask, tgt_mask, src_name, dst_name,
     $                  src_lat, src_lon, dst_lat, dst_lon,
     $                  src_corner_lat, src_corner_lon,
     $                  dst_corner_lat, dst_corner_lon)
C
C*    -- calculation of weights and addresses using SCRIP-library
C
         CALL scrip(crmpfile, cmapping, map_method, normalize_opt,
     $              lextrapdone, rl_varmul, id_scripvoi)
C
         IF (map_method == 'CONSERV') THEN
             DEALLOCATE(src_corner_lon, src_corner_lat,
     $              dst_corner_lon, dst_corner_lat)
         ENDIF
C
C*    -- open just created SCRIP matrix file
C         
         CALL hdlerr(NF_OPEN
     $              (crmpfile, NF_NOWRITE, nc_scpid), 'scriprmp')
C
         IF (nlogprt .GE. 2) THEN
             WRITE (UNIT = nulou,FMT = *) 
     $       ' SCRIP file created and opened' 
             WRITE (UNIT = nulou,FMT = *) ' '
             CALL FLUSH(nulou)
         ENDIF
      ENDIF
C
C*    3. Read weights and addresses
C        --------------------------
C
C* -- get matrix size
C
c* CMCC - allocation of remapping weights arrays
      IF (.not.allocated(snum_links)) THEN
          ALLOCATE (snum_links(num_field))
      ENDIF
      IF (.not.allocated(snum_wgts)) THEN
          ALLOCATE (snum_wgts(num_field))
      ENDIF
      IF (.not.allocated(sweight_flag)) THEN
          ALLOCATE (sweight_flag(num_field))
          sweight_flag(:) = .FALSE.
      ENDIF
      IF (.not.allocated(sweigth)) THEN
          ALLOCATE (sweigth(num_field))
      END IF
      IF (.not.allocated(ssrc_addr)) THEN
          ALLOCATE (ssrc_addr(num_field))
      END IF
      IF (.not.allocated(sdst_addr)) THEN
          ALLOCATE (sdst_addr(num_field))
      END IF

c* CMCC - flag for managing read from netcdf input files.
c*        'sweight_flag' flag is set to TRUE after the first
c*        read
      IF (.NOT. sweight_flag(jf)) THEN
        write(nulou,*) 'scriprmp tcx2 sweight_flag false'

        CALL hdlerr(NF_INQ_DIMID
     $           (nc_scpid, 'num_links', dimid), 'scriprmp')
c        CALL hdlerr(NF_INQ_DIMLEN
c     $           (nc_scpid, dimid, num_links), 'scriprmp')
        CALL hdlerr(NF_INQ_DIMLEN
     $           (nc_scpid, dimid, snum_links(jf)), 'scriprmp')
C
C* -- get number of weights
C
        SELECT CASE (map_method)
        CASE ('CONSERV')          ! conservative remapping
           snum_wgts(jf) = 3
        CASE ('BILINEAR')         ! bilinear remapping
           snum_wgts(jf) = 1
        CASE ('BICUBIC')          ! bicubic remapping
           IF (cdgrdtyp .eq. 'D') THEN
              snum_wgts(jf) = 1
           ELSE
              snum_wgts(jf) = 4
           ENDIF
        CASE ('DISTWGT')          ! distance weighted averaging
           snum_wgts(jf) = 1
        CASE ('GAUSWGT')          ! distance gaussian weighted averaging
           snum_wgts(jf) = 1
        END SELECT
C
C* -- array allocation
C
        ALLOCATE (sweigth(jf)%warray(snum_wgts(jf), snum_links(jf)))
        ALLOCATE (ssrc_addr(jf)%srcarray(snum_links(jf)))
        ALLOCATE (sdst_addr(jf)%dstarray(snum_links(jf)))
C
C* -- read source grid addresses and weights
C
        CALL hdlerr(NF_INQ_VARID
     $           (nc_scpid, csrcadd, varid), 'scriprmp')
        CALL hdlerr(NF_GET_VAR_INT
     $           (nc_scpid, varid, ssrc_addr(jf)%srcarray), 'scriprmp')
        CALL hdlerr(NF_INQ_VARID
     $           (nc_scpid, cdstadd, varid), 'scriprmp')
        CALL hdlerr(NF_GET_VAR_INT
     $           (nc_scpid, varid, sdst_addr(jf)%dstarray), 'scriprmp')
        CALL hdlerr(NF_INQ_VARID
     $           (nc_scpid, cweight, varid), 'scriprmp')
        IF (ll_single) THEN
            CALL hdlerr(NF_GET_VAR_REAL
     $        (nc_scpid, varid, sweigth(jf)%warray), 'scriprmp')
        ELSE
            CALL hdlerr(NF_GET_VAR_DOUBLE
     $        (nc_scpid, varid, sweigth(jf)%warray), 'scriprmp')
        ENDIF
        CALL hdlerr(NF_INQ_VARID
     $           (nc_scpid, cdstare, varid), 'scriprmp')
        IF (ll_single) THEN
            CALL hdlerr(NF_GET_VAR_REAL
     $        (nc_scpid, varid, dst_area), 'scriprmp')
        ELSE
            CALL hdlerr(NF_GET_VAR_DOUBLE
     $        (nc_scpid, varid, dst_area), 'scriprmp')
        ENDIF
        CALL hdlerr(NF_INQ_VARID
     $           (nc_scpid, cdstfra, varid), 'scriprmp')
        IF (ll_single) THEN    
            CALL hdlerr(NF_GET_VAR_REAL
     $        (nc_scpid, varid, dst_frac), 'scriprmp')
        ELSE
            CALL hdlerr(NF_GET_VAR_DOUBLE
     $        (nc_scpid, varid, dst_frac), 'scriprmp')
        ENDIF
        sweight_flag(jf) = .TRUE.
      END IF
C
C*    4. Do the matrix multiplication
C        ----------------------------
C
      ll_weightot  = .false.
      weightot(:)  = 0.0
      dst_array(:) = 0.0
      write(nulou,*) 'scriprmp tcx3 matrix mult'
      SELECT CASE (map_method)
C
      CASE ('CONSERV')     ! conservative remapping 
C
          SELECT CASE (order)
C
          CASE ('FIRST')        ! first order remapping 
C
              DO n = 1, snum_links(jf)
                IF ((ssrc_addr(jf)%srcarray(n)) .ne. 0) THEN
                 dst_array(sdst_addr(jf)%dstarray(n)) =
     $                   dst_array(sdst_addr(jf)%dstarray(n))
     $                  + sweigth(jf)%warray(1,n)
     $                  * src_array(ssrc_addr(jf)%srcarray(n))
                  weightot(sdst_addr(jf)%dstarray(n)) =
     $                  weightot(sdst_addr(jf)%dstarray(n))
     $                    + sweigth(jf)%warray(1,n)
                ENDIF
              END DO
C
          CASE ('SECOND')       ! second order remapping 
C                               ! (including gradients)
              IF (cdgrdtyp .ne. 'LR') THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $            'Field gradient cannot be calculated'
                  WRITE (UNIT = nulou,FMT = *) 
     $            'by Oasis as grid is not logically rectangular'
                  CALL HALTE('STOP in scriprmp (CONSERV)')
              ENDIF
              ALLOCATE(gradient_lat(src_size), gradient_lon(src_size))
C
              call gradient(nlon_src, nlat_src, src_array, sou_mask,
     $                       src_lat, src_lon, id_sper, cd_sper,
     $                       gradient_lat, gradient_lon)
C
              DO n = 1, snum_links(jf)
                IF ((ssrc_addr(jf)%srcarray(n)) .ne. 0) THEN
                 dst_array(sdst_addr(jf)%dstarray(n)) =
     $                 dst_array(sdst_addr(jf)%dstarray(n))
     $              + sweigth(jf)%warray(1,n)
     $              * src_array(ssrc_addr(jf)%srcarray(n))
     $              + sweigth(jf)%warray(2,n)
     $              * gradient_lat(ssrc_addr(jf)%srcarray(n))
     $              + sweigth(jf)%warray(3,n)
     $              * gradient_lon(ssrc_addr(jf)%srcarray(n))
                 weightot(sdst_addr(jf)%dstarray(n)) =
     $             weightot(sdst_addr(jf)%dstarray(n))
     $          + sweigth(jf)%warray(1,n) + sweigth(jf)%warray(2,n)
     $          + sweigth(jf)%warray(3,n)
                ENDIF
              END DO
              DEALLOCATE(gradient_lat, gradient_lon)
C
           END SELECT           ! order
C
       CASE ('BILINEAR')        ! bilinear remapping 
C
           DO n = 1, snum_links(jf)
             IF ((ssrc_addr(jf)%srcarray(n)) .ne. 0) THEN
                dst_array(sdst_addr(jf)%dstarray(n)) =
     $             dst_array(sdst_addr(jf)%dstarray(n))
     $               + sweigth(jf)%warray(1,n)
     $               * src_array(ssrc_addr(jf)%srcarray(n))
                 weightot(sdst_addr(jf)%dstarray(n)) =
     $            weightot(sdst_addr(jf)%dstarray(n))
     $               + sweigth(jf)%warray(1,n)
             ENDIF
           END DO
C
       CASE ('BICUBIC')         ! bicubic remapping
C
           SELECT CASE (cdgrdtyp) !
           CASE ('LR')          ! logically rectangular

               ALLOCATE(gradient_i(src_size), gradient_j(src_size),
     $             gradient_ij(src_size))
C
               CALL gradient_bicubic(nlon_src,nlat_src,src_array,
     $             sou_mask, src_lat, src_lon, id_sper, cd_sper,
     $             gradient_i, gradient_j, gradient_ij)
C
               DO n = 1, snum_links(jf)
                 IF ((ssrc_addr(jf)%srcarray(n)) .ne. 0) THEN
                 dst_array(sdst_addr(jf)%dstarray(n)) =
     $             dst_array(sdst_addr(jf)%dstarray(n))
     $                   + sweigth(jf)%warray(1,n)
     $               * src_array(ssrc_addr(jf)%srcarray(n))
     $                  + sweigth(jf)%warray(2,n)
     $                   * gradient_i(ssrc_addr(jf)%srcarray(n))
     $                  + sweigth(jf)%warray(3,n)
     $                  * gradient_j(ssrc_addr(jf)%srcarray(n))
     $                 + sweigth(jf)%warray(4,n)
     $                * gradient_ij(ssrc_addr(jf)%srcarray(n))
                  weightot(sdst_addr(jf)%dstarray(n)) =
     $              weightot(sdst_addr(jf)%dstarray(n))
     $          + sweigth(jf)%warray(1,n) + sweigth(jf)%warray(2,n)
     $           + sweigth(jf)%warray(3,n)
     $                   + sweigth(jf)%warray(4,n)
                 ENDIF
               END DO
C
               DEALLOCATE(gradient_i, gradient_j, gradient_ij)
C
           CASE ('D')           !reduced
C
               DO n = 1, snum_links(jf)
                 IF ((ssrc_addr(jf)%srcarray(n)) .ne. 0) THEN
                dst_array(sdst_addr(jf)%dstarray(n)) =
     $                dst_array(sdst_addr(jf)%dstarray(n))
     $                   + sweigth(jf)%warray(1,n)
     $              * src_array(ssrc_addr(jf)%srcarray(n))
                  weightot(sdst_addr(jf)%dstarray(n)) =
     $              weightot(sdst_addr(jf)%dstarray(n))
     $                   + sweigth(jf)%warray(1,n)
                 ENDIF
               ENDDO
C
           END SELECT
C
       CASE ('DISTWGT')         ! distance weighted average
C
           DO n = 1, snum_links(jf)
             IF ((ssrc_addr(jf)%srcarray(n)) .ne. 0) THEN
                 dst_array(sdst_addr(jf)%dstarray(n)) =
     $            dst_array(sdst_addr(jf)%dstarray(n))
     $               + sweigth(jf)%warray(1,n)
     $           * src_array(ssrc_addr(jf)%srcarray(n))
                 weightot(sdst_addr(jf)%dstarray(n)) =
     $             weightot(sdst_addr(jf)%dstarray(n))
     $               + sweigth(jf)%warray(1,n)
             ENDIF
           ENDDO
C
      CASE ('GAUSWGT')          ! distance gaussian weighted average
C
          DO n = 1, snum_links(jf)
            IF ((ssrc_addr(jf)%srcarray(n)) .ne. 0) THEN
                dst_array(sdst_addr(jf)%dstarray(n)) =
     $              dst_array(sdst_addr(jf)%dstarray(n))
     $              + sweigth(jf)%warray(1,n)
     $          * src_array(ssrc_addr(jf)%srcarray(n))
                weightot(sdst_addr(jf)%dstarray(n)) =
     $              weightot(sdst_addr(jf)%dstarray(n))
     $              + sweigth(jf)%warray(1,n)
            ENDIF
          END DO
C
      END SELECT                ! remapping method
C
      IF (ll_weightot) THEN
          DO n = 1, dst_size
            IF (weightot(n) .lt. epsilon(1.)) dst_array(n) = 1.0E+20
          END DO
      ENDIF
C
c$$$C*    5. Check the cells on the poles
c$$$C        ----------------------------
c$$$C
c$$$C* -- For the north pole _N and the south pole _S
c$$$C
c$$$!SV081107      latpol_N = pi*half
c$$$!SV081107      latpol_S = -pi*half
c$$$      latpol_N = 90.0_real_kind
c$$$      latpol_S = 90.0_real_kind
c$$$      compt_N = 0
c$$$      moy_tmp_N = 0d0
c$$$      moy_ERR_N = 0d0
c$$$      compt_S = 0
c$$$      moy_tmp_S = 0d0
c$$$      moy_ERR_S = 0d0
c$$$C
c$$$      DO n = 1, dst_size
c$$$        IF (dst_lat(n) == latpol_N) THEN
c$$$            moy_tmp_N = moy_tmp_N + dst_array(n)
c$$$            moy_err_N = moy_err_N + dst_err(n)
c$$$            compt_N = compt_N + 1
c$$$        ELSE IF (dst_lat(n) == latpol_S) THEN
c$$$            moy_tmp_S = moy_tmp_S + dst_array(n)
c$$$            moy_err_S = moy_err_S + dst_err(n)
c$$$            compt_S = compt_S + 1
c$$$        END IF
c$$$      ENDDO
c$$$C
c$$$      IF (compt_N/=0) THEN
c$$$          moy_tmp_N = moy_tmp_N/compt_N
c$$$          moy_err_N = moy_err_N/compt_N
c$$$ CMCC optimization >>
c$$$          DO n = 1, snum_links(jf)
c$$$            IF (dst_lat(sdst_addr(jf)%dstarray(n)) == latpol_N) THEN
c$$$                dst_array(sdst_addr(jf)%dstarray(n)) = moy_tmp_N
c$$$                dst_err(sdst_addr(jf)%dstarray(n)) = moy_err_N
c$$$            END IF
c$$$          END DO
c$$$c          DO n = 1, num_links
c$$$c            IF (dst_lat(dst_addr(n)) == latpol_N) THEN
c$$$c                dst_array(dst_addr(n)) = moy_tmp_N
c$$$c                dst_err(dst_addr(n)) = moy_err_N
c$$$c            END IF
c$$$c          END DO
c$$$ CMCC optimization <<
c$$$C #slo - Output reduced for nlogprt=0
c$$$          IF (nlogprt .GE. 1) THEN
c$$$            WRITE(nulou,*), 'Points at the pole N'
c$$$            WRITE(nulou,*), 'Average value at the pole : ', moy_tmp_N
c$$$            WRITE(nulou,*), 'Average error at the pole : ', moy_err_N
c$$$          END IF
c$$$      ELSE
c$$$          IF (nlogprt .GE. 1) WRITE(nulou,*), 'No point at the pole N'
c$$$      END IF
c$$$C
c$$$      IF (compt_S/=0) THEN
c$$$          moy_tmp_S = moy_tmp_S/compt_S
c$$$          moy_err_S = moy_err_S/compt_S
c$$$ CMCC optimization >>
c$$$          DO n = 1, snum_links(jf)
c$$$            IF (dst_lat(sdst_addr(jf)%dstarray(n)) == latpol_S) THEN
c$$$                dst_array(sdst_addr(jf)%dstarray(n)) = moy_tmp_S
c$$$                dst_err(sdst_addr(jf)%dstarray(n)) = moy_err_S
c$$$            END IF
c$$$          END DO
c$$$c          DO n = 1, num_links
c$$$c            IF (dst_lat(dst_addr(n)) == latpol_S) THEN
c$$$c                dst_array(dst_addr(n)) = moy_tmp_S
c$$$c                dst_err(dst_addr(n)) = moy_err_S
c$$$c            END IF
c$$$c          END DO
c$$$ CMCC optimization <<
c$$$C #slo - Output reduced for nlogprt=0
c$$$          IF (nlogprt .GE. 1) THEN
c$$$            PRINT*, 'Points at the pole S'
c$$$            PRINT*, 'Average value at the pole : ', moy_tmp_S
c$$$            PRINT*, 'Average error at the pole : ', moy_err_S
c$$$          END IF
c$$$      ELSE
c$$$          IF (nlogprt .GE. 1) PRINT*, 'No point at the pole S'
c$$$      END IF
C
C*    6. Close remapping file
C        --------------------
C
      CALL hdlerr(NF_CLOSE(nc_scpid), 'scriprmp')
C
C*    7. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine scriprmp ---------'
          CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
