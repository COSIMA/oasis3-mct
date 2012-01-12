SUBROUTINE scriprmp_vector(              &
     dst_arrayI_dstA, dst_arrayJ_dstB,   &
     src_arrayI_srcA, src_arrayJ_srcB,   &
     grd_name_srcA,   grd_name_srcB,     &
     grd_name_dstA,   grd_name_dstB,     &
     src_size,        dst_size,          &     
     msk_srcA,        msk_srcB,          &
     msk_dstA,        msk_dstB,          &
     lon_srcA,        lat_srcA,          &
     lon_srcB,        lat_srcB,          &
     lon_dstA,        lat_dstA,          &
     lon_dstB,        lat_dstB,          &
     nlon_src,        nlat_src,          &
     nlon_dst,        nlat_dst,          &
     map_method,      cdgrdtyp,          &
     id_sper,         id_tper,           &
     cd_sper,         cd_tper,           &
     normalize_opt,   order,             &
     rst_type,        n_srch_bins,       &
     lextrapdone,     lprojcart,         &
     rl_varmul,       id_scripvoi        )

!C   Input:
!C   -----
!C             src_arrayI_srcA : field on source grid defined as VECTOR_I
!C             src_arrayJ_srcB : field on source grid defined as VECTOR_J
!C             grd_name_srcA   : name for the source grid where VECTOR_I is defined
!C             grd_name_srcB   : name for the source grid where VECTOR_J is defined
!C             grd_name_dstA   : name for the target grid where VECTOR_I is defined
!C             grd_name_dstB   : name for the target grid where VECTOR_J is defined
!C             src_size        : source grid size 
!C             dst_size        : target grid size 
!C             msk_srcA        : grid mask for srcA 
!C             msk_srcB        : grid mask for srcB
!C             msk_dstA        : grid mask for dstA 
!C             msk_dstB        : grid mask for dstB 
!C             lon_srcA        : longitudes for grid srcA
!C             lon_srcB        : longitudes for grid srcB
!C             lon_dstA        : longitudes for grid dstA
!C             lon_dstB        : longitudes for grid dstB
!C             lat_srcA        : latitudes for grid srcA
!C             lat_srcB        : latitudes for grid srcB
!C             lat_dstA        : latitudes for grid dstA
!C             lat_dstB        : latitudes for grid dstB
!C             nlon_src        : number of longitudes for source grid
!C             nlat_src        : number of latitudes for source grid
!C             nlon_dst        : number of longitudes for target grid
!C             nlat_dst        : number of latitudes for target grid
!C             map_method      : remapping method
!C             cdgrdtyp        : grid type for source grid
!C             id_sper         : number of overlapping for source grid
!C             id_tper         : number of overlapping for target grid
!C             cd_sper         : source grid periodicity type
!C             cd_tper         : target grid periodicity type
!C             normalize_opt   : option for normalization
!C             order           : order of conservative remapping
!C             rst_type        : type of scrip search restriction
!C             n_srch_bins     : number of seach bins 
!C             lextrapdone     : logical, true if EXTRAP done on field
!C             lprojcart       : logical, true for rotation to cartesian refernetial 
!C             rl_varmul       : Gaussian variance (for GAUSWGT)
!C             id_scripvoi     : number of neighbour for DISTWGT and GAUSWGT
!C
!C     Output:
!C     ------
!C             dst_arrayI_dstA : VECTOR_I field on target grid as defined in namcouple
!C             dst_arrayJ_dstB : VECTOR_J field on target grid as defined in namcouple
!C
!C     Externals:
!C     ---------
!C     corners, scrip, gradient, gradient_bicubic, 
!C     from module rotations : loc2spher, sher2car, car2spher, spher2loc
!C     from module vector    : remap_vector_comps, write_src_array_spheric, 
!C                             write_dst_array_spheric, write_cartesian_components,
!C                             check_points_at_poles
!C 
!C     History:
!C     --------
!C     E. Rapaport     2004/03     Created
!C     J. Ghattas      2006/02     Rewritten
!
!
! Description :
! -------------
! This routine interpolates a vector field, described as 2 scalar components I and J.
! The 2 components are described on different source grids and the resulting 2 
! components can be calculated on 2 differents target grids. The source grids 
! are named grd_srcA which is the original grid for the component I, and grd_srcB
! which is the original grid for component J. The target grid defined for the component
! I are dstA and for for the component J dstB. The two source grids have to be identical 
! in number of cells and their mask must be the same; for different target grids the same 
! criteria are imposed.
!
! Steps:
! ------
! -> If the source grid differs a first interpolation will be done from grd_srcB 
!    towards grd_srcA scalarly for the 2 components 
! -> Rotation towards spherical referentials on source grid srcA 
! -> If desired rotation towards cartesian referentials 
! -> Interpolation of the components scalarly from srcA towards target grid dstA 
! -> Rotation towards local referentials. 
! -> If target grids differ, the same procedure will be done from grd_srcA towards grd_dstB.
!
!C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C* ---------------------------- Modules used ----------------------------
  USE grids
  USE rotations
  USE remap_vars
  USE vector
!
!* ---------------------------- Implicit --------------------------------
!
  IMPLICIT NONE
!
!* ---------------------------- Include files ---------------------------
!
!* ---------------------------- Intent In -------------------------------
!
  INTEGER(KIND=int_kind), INTENT(in) :: &
       nlon_src, nlat_src, & ! number of longitudes and latitudes on source grid
       nlon_dst, nlat_dst, & ! number of longitudes and latitudes on target grid
       src_size, dst_size, & ! number of source/target grid cells
       n_srch_bins,        & ! number of search bins for SCRIP
       msk_srcA(src_size), & ! grid mask for srcA
       msk_srcB(src_size), & ! grid mask for srcB
       msk_dstA(dst_size), & ! grid mask for dstA
       msk_dstB(dst_size), & ! grid mask for dstB
       id_sper, &            ! number of overlapping points for source grid
       id_tper, &            ! number of overlapping points for target grid
       id_scripvoi           ! number of neighbour for DISTWGT and GAUSWGT

  REAL (KIND=real_kind), INTENT(in) :: &
       src_arrayI_srcA(src_size),& ! field component I on original source grid srcA
       src_arrayJ_srcB(src_size),& ! field component J on original source grid srcB 
       lon_srcA(src_size), lat_srcA(src_size), & ! lon-/latitudes for source grid srcA
       lon_srcB(src_size), lat_srcB(src_size), & ! lon-/latitudes for source grid srcB
       lon_dstA(dst_size), lat_dstA(dst_size), & ! lon-/latitudes for target grid dstA
       lon_dstB(dst_size), lat_dstB(dst_size), & ! lon-/latitudes for target grid dstB
       rl_varmul                                 ! Gaussian variance (for GAUSWGT)

  CHARACTER(LEN=8), INTENT(in) :: &
       grd_name_srcA, grd_name_srcB,  &   ! grid name for the 2 sources grids
       grd_name_dstA, grd_name_dstB,  &   ! grid name for the 2 target grids
       map_method, &                      ! remapping method
       cdgrdtyp, &                        ! source grid type
       normalize_opt, &                   ! option for normalization
       order, &                           ! order of conservative remapping
       rst_type, &                        ! type of scrip search restriction
       cd_sper, &                         ! source grid periodicity type
       cd_tper                            ! target grid periodicity type

  LOGICAL, INTENT(in) :: &
       lextrapdone, &        ! logical, true if EXTRAP done on field
       lprojcart             ! logical, ture if projection to cartesian should be done

!
!* ---------------------------- Intent Out -------------------------------
!
  REAL (KIND=real_kind), INTENT(out):: &
       dst_arrayI_dstA(dst_size), &  ! VECTOR_I field on target grid dstA
       dst_arrayJ_dstB(dst_size)     ! VECTOR_J field on target grid dstB

!
!* ---------------------------- Local declarations ----------------------
!
  INTEGER(KIND=int_kind) :: nbr_dst, nbr_comp, ii, jj, icount, kcount, stat, &
                            nc_gridsid, angid, nc_fileid, var_id
  INTEGER(KIND=int_kind) :: ilenstr
  INTEGER (KIND=int_kind), DIMENSION(dst_size)   :: msk_dst
  REAL (KIND=real_kind), DIMENSION(src_size)     :: src_arrayJ_srcA
  REAL (KIND=real_kind), DIMENSION(src_size,3)   :: src_array, src_array_buf 
  REAL (KIND=real_kind), DIMENSION(dst_size)     :: lon_dst, lat_dst
  REAL (KIND=real_kind), DIMENSION(src_size,2,2) :: mat_loc2sph
  REAL (KIND=real_kind), DIMENSION(src_size,3,2) :: mat_sph2car
  REAL (KIND=real_kind), DIMENSION(dst_size,3,3) :: mat_car2sph
  REAL (KIND=real_kind), DIMENSION(dst_size,2,2) :: mat_sph2loc
  REAL (KIND=real_kind), DIMENSION(dst_size,3)   :: dst_array, dst_array_buf

  CHARACTER(LEN=8)     :: grd_name_dst
  CHARACTER (char_len) :: cmapping, crmpfile  
  CHARACTER(LEN=12)    :: ang_name 
#if defined use_oasis_para || defined use_oasis_cmcc_para
  CHARACTER*3 :: cl_indexoa
#endif

  LOGICAL :: ll_loc2spher_srcA, ll_spher2loc_dst  

!
!* ---------------------------- Poema verses ----------------------------
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!*** O. Initialization
!       --------------
!       --------------
  IF (nlogprt .GE. 2) THEN
     WRITE (UNIT = nulou,FMT = *) ' '
     WRITE (UNIT = nulou,FMT = *) &
          '    Entering ROUTINE scriprmp_vector  -  Level 3'
     WRITE (UNIT = nulou,FMT = *) &
          '           ***********************     *******'
     WRITE (UNIT = nulou,FMT = *) ' '
     WRITE (UNIT = nulou,FMT = *) 'SCRIP remapping for vector'
     WRITE (UNIT = nulou,FMT = *) ' '
     call flush(nulou)
  ENDIF
  
!
!*** 1. Interpolation towards the same source grid
!       ------------------------------------------
!       ------------------------------------------
!
!* If the source grids of the two vector components are different  
!* -> interpolation of field J on grd_srcB towards the same grid as for 
!* the field I grd_srcA.
! 
! 
!* After this step the two components are on the same source grid:
!* -> src_arrayI_srcA, src_arrayJ_srcA
!

  IF (grd_name_srcA .NE. grd_name_srcB) THEN
     ! Interpolate the field src_arrayJ_srcB from grid srcB towards
     ! grid srcA. Resulting field on srcA is src_arrayJ_srcA
     CALL scriprmp( &
          src_arrayJ_srcA,src_arrayJ_srcB, src_size, src_size, &
          msk_srcB, msk_srcA, &
          lon_srcB, lat_srcB, nlon_src, nlat_src, &
          lon_srcA, lat_srcA, nlon_src, nlat_src, &
          map_method, cdgrdtyp, &
          id_sper, id_sper, cd_sper, cd_sper, &
          grd_name_srcB, grd_name_srcA, & 
          normalize_opt, order, rst_type, n_srch_bins, &
          lextrapdone, rl_varmul, id_scripvoi)
  ELSE
     ! grid srcA equal grid srcB, no interpolation necessary
     src_arrayJ_srcA = src_arrayJ_srcB
  ENDIF

! 
!*** 2. Check if the target grids are different
!       If different the interpolation will be done twice
!       -------------------------------------------------
!       -------------------------------------------------

  IF (grd_name_dstA .EQ. grd_name_dstB) THEN
     nbr_dst=1
  ELSE
     nbr_dst=2
  ENDIF


  DO ii=1, nbr_dst

     IF (ii == 1) THEN
        grd_name_dst = grd_name_dstA
        lon_dst(:)   = lon_dstA(:)
        lat_dst(:)   = lat_dstA(:)
        msk_dst(:)   = msk_dstA(:)
     ELSE
        grd_name_dst = grd_name_dstB
        lon_dst(:)   = lon_dstB(:)
        lat_dst(:)   = lat_dstB(:)
        msk_dst(:)   = msk_dstB(:)
     ENDIF

     IF (nlogprt .GE. 2) THEN
        WRITE (UNIT = nulou,FMT = *) ''
        WRITE (UNIT = nulou,FMT = *) &
             ' Now making interpolation towards target grid : ', grd_name_dst
        CALL FLUSH(nulou)
     ENDIF

!
!*** 3. Calculate and/or read weights, remapping and rotation matrix
!       ------------------------------------------------------------
!       ------------------------------------------------------------
!
!* -- Get the name of the file containing the remapping matrix.
! 
#if defined use_oasis_para || defined use_oasis_cmcc_para
      IF (ig_indexoa .le. 9) THEN
          WRITE(cl_indexoa,FMT='(I1)') ig_indexoa
      ELSE IF (ig_indexoa .le. 99) THEN
          WRITE(cl_indexoa,FMT='(I2)') ig_indexoa
      ELSE IF (ig_indexoa .le. 999) THEN
          WRITE(cl_indexoa,FMT='(I3)') ig_indexoa
      ENDIF
#endif
! 
     icount = ilenstr(grd_name_srcA,jpeight)
     kcount = ilenstr(grd_name_dst, jpeight)
     
     SELECT CASE (map_method)
     CASE ('CONSERV')          ! conservative remapping
        cmapping = &
             grd_name_srcA(1:icount)//' to '//grd_name_dst(1:kcount)//' '&
             //map_method//' '//normalize_opt(1:4)//' remapping'
#if defined use_oasis_para || defined use_oasis_cmcc_para
        IF (ig_indexoa .LE. 9) THEN       
            crmpfile = &
               'rmp_'//grd_name_srcA(1:icount)//'_to_'//grd_name_dst(1:kcount)&
               //'_'//map_method(1:7)//'_'//normalize_opt(1:4)// &
               '_'//cl_indexoa(1:1)//'.nc'
        ELSE IF (ig_indexoa .LE. 99) THEN 
            crmpfile = &
               'rmp_'//grd_name_srcA(1:icount)//'_to_'//grd_name_dst(1:kcount)&
               //'_'//map_method(1:7)//'_'//normalize_opt(1:4)// &
               '_'//cl_indexoa(1:2)//'.nc'
        ELSE IF (ig_indexoa .LE. 999) THEN
             crmpfile = &
               'rmp_'//grd_name_srcA(1:icount)//'_to_'//grd_name_dst(1:kcount)&
               //'_'//map_method(1:7)//'_'//normalize_opt(1:4)// &
               '_'//cl_indexoa(1:3)//'.nc'
         ENDIF
#else
         crmpfile = &
            'rmp_'//grd_name_srcA(1:icount)//'_to_'//grd_name_dst(1:kcount)&
            //'_'//map_method(1:7)//'_'//normalize_opt(1:4)//'.nc' 
#endif
         !       
     CASE DEFAULT
        cmapping = &
          grd_name_srcA(1:icount)//' to '//grd_name_dst(1:kcount)//' '&
          //map_method//' remapping'
#if defined use_oasis_para || defined use_oasis_cmcc_para
      IF (ig_indexoa .LE. 9) THEN     
          crmpfile = &
             'rmp_'//grd_name_srcA(1:icount)//'_to_'//grd_name_dst(1:kcount)&
             //'_'//map_method(1:7)//'_'//cl_indexoa(1:1)//'.nc'
      ELSE IF (ig_indexoa .le. 99) THEN
          crmpfile = &
             'rmp_'//grd_name_srcA(1:icount)//'_to_'//grd_name_dst(1:kcount)&
             //'_'//map_method(1:7)//'_'//cl_indexoa(1:2)//'.nc'
      ELSE IF (ig_indexoa .le. 999) THEN
          crmpfile = &
             'rmp_'//grd_name_srcA(1:icount)//'_to_'//grd_name_dst(1:kcount)&
             //'_'//map_method(1:7)//'_'//cl_indexoa(1:3)//'.nc'
      ENDIF
#else
      crmpfile = &
         'rmp_'//grd_name_srcA(1:icount)//'_to_'//grd_name_dst(1:kcount)&
         //'_'//map_method(1:7)//'.nc'  
#endif
     END SELECT

     IF (nlogprt .GE. 2) THEN
        WRITE (UNIT = nulou,FMT = *) &
             ' SCRIP filename : ', crmpfile
        CALL FLUSH(nulou)
     ENDIF
     
!
!*** 3.1 Create and/or open the file with remapping matrix
!        -------------------------------------------------
!              
     stat = NF_OPEN(crmpfile, NF_WRITE, nc_fileid) 
     IF (stat .NE. NF_NOERR) THEN 
!
!* -- The file does not exist. The remapping matrix has to be created
!     for remapping from source grd_name_srcA to target grd_name_dst
!
        CALL calc_remap_matrix( &
             msk_srcA(:),  msk_dst(:), &
             src_size,     dst_size, &
             lon_srcA(:),  lat_srcA(:),  lon_dst(:),  lat_dst(:), &
             nlon_src, nlat_src, nlon_dst, nlat_dst, &
             grd_name_srcA, grd_name_dst, &
             map_method,   normalize_opt, cdgrdtyp, &
             id_sper,      cd_sper,      &
             id_tper,      cd_tper,      &
             rst_type,                   &
             n_srch_bins,  crmpfile,     &
             cmapping,     lextrapdone,  &
             rl_varmul,    id_scripvoi   )
    
        stat = NF_OPEN(crmpfile, NF_WRITE, nc_fileid) 

     ELSE

!* -- The file with remapping matrix existe already
!
        IF (nlogprt .GE. 2) THEN
           WRITE (UNIT = nulou,FMT = *) &
                'SCRIP file exists with remapping matrix src to dst grid'
           WRITE (UNIT = nulou,FMT = *) ' '
           CALL FLUSH(nulou)
        ENDIF
   
     END IF !IF (stat .NE. NF_NOERR)


!*** 3.2. If necessary calculate matrix for rotation on srcA grid local to sperical
!         --------------------------------------------------------------------------
!
!* Get angles from file grids.nc. 
!  If they exist a rotation from local to spherical referntial will be done, 
!  if no angles the grid is considered as a spheric referential 
!
     CALL hdlerr(NF_OPEN &
          ('grids.nc',NF_NOWRITE, nc_gridsid), 'scriprmp_vector')
     ang_name = grd_name_srcA(1:icount)//'.ang'
     stat = NF_INQ_VARID(nc_gridsid, ang_name, var_id)
     
     CALL hdlerr(NF_CLOSE(nc_gridsid), 'scriprmp_vector')   

     IF(stat == NF_NOERR) THEN
        ll_loc2spher_srcA = .true.
!
!* Rotaion from local to spherical referential on grd_srcA is needed
!* Check if the rotation matrix Mat_src_loc2spher exists in the remapping file
!
        stat = NF_INQ_VARID(nc_fileid, 'Mat_src_loc2sph', var_id)
  
        IF (stat == NF_NOERR) THEN
           !* Read the matrix already existing
           IF (ll_single) THEN
              CALL hdlerr(NF_GET_VAR_REAL &
                (nc_fileid, var_id, mat_loc2sph(:,:,:)), 'scriprmp_vector')
           ELSE
              CALL hdlerr(NF_GET_VAR_DOUBLE &
                   (nc_fileid, var_id, mat_loc2sph(:,:,:)), 'scriprmp_vector')
           ENDIF
        ELSE
           !* The matrix has to be calculated and written to the file
           !* CALL loc2spher() writes the matrix to netcdf file nc_fileid 
           !  and returns the matrix mat_loc2sph
           CALL loc2spher(grd_name_srcA, src_size, mat_loc2sph(:,:,:), nc_fileid)
        ENDIF

     ELSE ! The angles does not exist in grids.nc
        ll_loc2spher_srcA = .false.
        WRITE (UNIT = nulou,FMT = *) &
             '    WARNING'
        WRITE (UNIT = nulou,FMT = *) &
             '***************'
        WRITE (UNIT = nulou,FMT = *) &
             'file grids.nc contains no angle information'
        WRITE (UNIT = nulou,FMT = *) &
             'source grid '// grd_name_srcA// &
             ' will be considered as a spheric referential'
        CALL FLUSH(nulou)
     END IF
!
!*** 3.3 Calculate matrix for projection to cartesian if demanded by user option PROJCART
! 
!      ->  calculate matrix from spherical to cartesian referntials on source grid srcA
!      ->  calculate matrix from cartesian to spherical referntials on target grid dst
!
     IF ( lprojcart ) THEN

        !* On source grid

        stat = NF_INQ_VARID(nc_fileid, 'Mat_src_sph2car', var_id)
        IF (stat == NF_NOERR) THEN
           !* Read the matrix already existing
           IF (ll_single) THEN
              CALL hdlerr(NF_GET_VAR_REAL &
                   (nc_fileid, var_id, mat_sph2car(:,:,:)), 'scriprmp_vector')
           ELSE
              CALL hdlerr(NF_GET_VAR_DOUBLE &
                   (nc_fileid, var_id, mat_sph2car(:,:,:)), 'scriprmp_vector')
           ENDIF
        ELSE
           !* Calculate the matrix mat_sph2car and write to file nc_fileid 
           !
           CALL spher2car(lon_srcA(:), lat_srcA(:), src_size, mat_sph2car(:,:,:), nc_fileid)

        ENDIF
        
        !* On target grid

        stat = NF_INQ_VARID(nc_fileid, 'Mat_dst_car2sph', var_id)
        IF (stat == NF_NOERR) THEN
           !* Read the matrix already existing
           IF (ll_single) THEN
              CALL hdlerr(NF_GET_VAR_REAL &
                   (nc_fileid, var_id, mat_car2sph(:,:,:)), 'scriprmp_vector')
           ELSE
              CALL hdlerr(NF_GET_VAR_DOUBLE &
                   (nc_fileid, var_id, mat_car2sph(:,:,:)), 'scriprmp_vector')
           ENDIF
        ELSE
           !* Calculate the matrix mat_car2sph and write to file nc_fileid 
           !
           CALL car2spher(lon_dst(:), lat_dst(:), dst_size, mat_car2sph(:,:,:), nc_fileid)

        ENDIF
        
     ENDIF !IF ( lprojcart )

!
!*** 3.4 If necessary calculate matrix for rotation on target grid sperical to local
!        ---------------------------------------------------------------------------
!
!* Get angles from file grids.nc. 
!  If they exist a rotation from spherical to local referntial will be done, 
!  if no angles the grid is considered as a spheric referential 
!
     CALL hdlerr(NF_OPEN &
          ('grids.nc',NF_NOWRITE, nc_gridsid), 'scriprmp_vector')
     ang_name = grd_name_dst(1:kcount)//'.ang'
     stat = NF_INQ_VARID(nc_gridsid, ang_name, angid)
     
     CALL hdlerr(NF_CLOSE(nc_gridsid), 'scriprmp_vector')   
     
     IF (stat == NF_NOERR) THEN
        ll_spher2loc_dst = .true.
!
!* Rotaion from spherical to local referential on grd_dst is needed
!  Check if the rotation matrix Mat_dst_sph2loc exists in the remapping file
!
        stat = NF_INQ_VARID(nc_fileid, 'Mat_dst_sph2loc', var_id)
     
        IF (stat == NF_NOERR) THEN
           !* Read the matrix already existing
           IF (ll_single) THEN
              CALL hdlerr(NF_GET_VAR_REAL &
                   (nc_fileid, var_id, mat_sph2loc(:,:,:)), 'scriprmp_vector')
           ELSE
              CALL hdlerr(NF_GET_VAR_DOUBLE &
                   (nc_fileid, var_id, mat_sph2loc(:,:,:)), 'scriprmp_vector')
           ENDIF
        ELSE
           !* The matrix has to be calculated and written to the file
           !* CALL spher2loc() writes the matrix to netcdf file nc_fileid 
           !  and returns the matrix mat_sph2loc
           CALL spher2loc(grd_name_dst, dst_size, mat_sph2loc(:,:,:), nc_fileid)
        
        ENDIF

     ELSE ! The angles does not exist in grids.nc
        ll_spher2loc_dst = .false.
        WRITE (UNIT = nulou,FMT = *) &
             '    WARNING'
        WRITE (UNIT = nulou,FMT = *) &
             '***************'
        WRITE (UNIT = nulou,FMT = *) &
             'file grids.nc contains no angle information'
        WRITE (UNIT = nulou,FMT = *) &
             'target grid '// grd_name_dst// &
             ' will be considered as a spheric referential'
        CALL FLUSH(nulou)
        
     END IF ! IF (stat == NF_NOERR)

!
!* End of calculation of weight and transformation matrix
!* All matrix do now exist
!

!
!*** 4. Rotation and interpolation of the two components on grd_srcA towards 
!       the target grid grd_dst
!       -------------------------------------------------------------------
!       -------------------------------------------------------------------
!
!* save the both source components in src_array(:,:)  
     src_array(:,1) = src_arrayI_srcA(:)
     src_array(:,2) = src_arrayJ_srcA(:)
     src_array(:,3) = 0
     nbr_comp=2   
!
!*** 4.1 Rotate the the vector field components on srcA to spherical referential 
!    
     IF ( ll_loc2spher_srcA ) THEN

        src_array(:,1) = mat_loc2sph(:,1,1) * src_arrayI_srcA(:) +  &
                         mat_loc2sph(:,1,2) * src_arrayJ_srcA(:)
        
        src_array(:,2) = mat_loc2sph(:,2,1) * src_arrayI_srcA(:) +  &
                         mat_loc2sph(:,2,2) * src_arrayJ_srcA(:)

     ENDIF
#ifdef __DEBUG     
     ! The components in spherical referential are saved to vector_debugXXXXXX.nc file. 
     CALL write_src_array_spheric(src_array(:,1:2), src_size, nlon_src, nlat_src, &
          grd_name_srcA, grd_name_dst)
#endif
     
!     
!*** 4.2 If PROJCART, rotate the the vector field components to cartesian referential 
!    
     IF (lprojcart) THEN

        nbr_comp=3

        src_array_buf(:,1) = &
             mat_sph2car(:,1,1) * src_array(:,1) + &  
             mat_sph2car(:,1,2) * src_array(:,2)
        
        src_array_buf(:,2) = &
             mat_sph2car(:,2,1) * src_array(:,1) + &  
             mat_sph2car(:,2,2) * src_array(:,2)

        src_array_buf(:,3) = &
             mat_sph2car(:,3,1) * src_array(:,1) + &  
             mat_sph2car(:,3,2) * src_array(:,2)

        src_array(:,1)= src_array_buf(:,1)
        src_array(:,2)= src_array_buf(:,2)
        src_array(:,3)= src_array_buf(:,3)
        
     ENDIF
!
!*** 4.3 Interpolate scalar the 2 or 3 compontents src_array(:,:)
!        from the source grd_srcA to target grd_dst, result in dst_array(:,:)
!        --------------------------------------------------------------------

     CALL remap_vector_comps (dst_array(:,1:nbr_comp), src_array(:,1:nbr_comp), &
          nbr_comp, nc_fileid, &
          map_method, cdgrdtyp, order, &
          msk_srcA(:), &
          id_sper, cd_sper, &
          nlon_src, nlat_src, lon_srcA(:), lat_srcA(:), src_size, dst_size)
     
!  
!** Close remapping file
!
     CALL hdlerr(NF_CLOSE(nc_fileid), 'scriprmp')

!
!*** 4.4 If PROJCART, rotate dst_array from cartesian to spherical referential
!        ---------------------------------------------------------------------

     IF (lprojcart) THEN

#ifdef __DEBUG     
        ! The components in cartesian referential are saved to vector_debugXXXXXX.nc file
        CALL write_cartesian_components(src_array(:,1:3), dst_array(:,1:3), src_size, &
             dst_size, nlon_src, nlat_src, nlon_dst, nlat_dst, grd_name_srcA, &
             grd_name_dst)
#endif

        dst_array_buf(:,1) = &
             mat_car2sph(:,1,1) * dst_array(:,1) + &
             mat_car2sph(:,1,2) * dst_array(:,2) + &
             mat_car2sph(:,1,3) * dst_array(:,3)
        
        dst_array_buf(:,2) = &
             mat_car2sph(:,2,1) * dst_array(:,1) + &
             mat_car2sph(:,2,2) * dst_array(:,2) + &
             mat_car2sph(:,2,3) * dst_array(:,3)
        
        dst_array_buf(:,3) = &
             mat_car2sph(:,3,1) * dst_array(:,1) + &
             mat_car2sph(:,3,2) * dst_array(:,2) + &
             mat_car2sph(:,3,3) * dst_array(:,3)
        
        dst_array(:,1) = dst_array_buf(:,1)
        dst_array(:,2) = dst_array_buf(:,2)
        dst_array(:,3) = dst_array_buf(:,3)

     ENDIF

!
!*** 4.5 If the target grid is not in a spherical referential
!        then rotate the spherical components towords locals 
!        ----------------------------------------------------

     IF (ll_spher2loc_dst) THEN

#ifdef __DEBUG     
        ! The components in spherical referential are saved to vector_debugXXXXXX.nc file
        ! The field K dst_array(:,3) should be close to zero
        CALL write_dst_array_spheric(dst_array(:,1:2), dst_size, nlon_dst, nlat_dst, &
             grd_name_srcA, grd_name_dst)
#endif
        
        dst_array_buf(:,1) = &
             mat_sph2loc(:,1,1) * dst_array(:,1) + &
             mat_sph2loc(:,1,2) * dst_array(:,2)
        
        dst_array_buf(:,2) = &
             mat_sph2loc(:,2,1) * dst_array(:,1) + &
             mat_sph2loc(:,2,2) * dst_array(:,2)

        dst_array(:,1) = dst_array_buf(:,1)
        dst_array(:,2) = dst_array_buf(:,2)
        
     ENDIF

!* The dst_array is now the I and J components in the local referential 
!* on target grid grd_dst 
!


!
!*** 5. Save the the components that correspond to VECTOR_I and VECTOR_J grid 
!       ---------------------------------------------------------------------
!       ---------------------------------------------------------------------

     IF (ii == 1 .AND. nbr_dst == 1) THEN
        ! There is only one target grid. Save both components calculated above.
        dst_arrayI_dstA(:) = dst_array(:,1)
        dst_arrayJ_dstB(:) = dst_array(:,2) 
     ELSEIF (ii == 1) THEN
        ! There are 2 target grid and the one just calculated is the grid for VECTOR_I
        dst_arrayI_dstA(:) = dst_array(:,1)
     ELSE
        ! There are 2 target grids and the one just calculated is the grid for VECTOR_J
        dst_arrayJ_dstB(:) = dst_array(:,2)
     ENDIF

     IF (nlogprt .GE. 2) THEN
         write(nulou,*)' src_size = ',src_size,'dst_size = ',dst_size
         write(nulou,*)' Now finish interpolation towards target grid : ', grd_name_dst
         call flush(nulou)
     ENDIF

  END DO

!
!*** 6. Test if there are points on the latitudes 90deg north and 90deg south.
!       If there are points on these latitudes, the averege for these points 
!       are calculated and distributed.
!       -----------------------------------------------------------------------
!       -----------------------------------------------------------------------

  CALL check_points_at_poles (dst_arrayI_dstA(:), dst_arrayJ_dstB(:), lat_dstA(:), &
       lat_dstB(:), grd_name_dstA, grd_name_dstB, dst_size)

!
!*  End of routine 
!  
  IF (nlogprt .GE. 2) THEN
     WRITE (UNIT = nulou,FMT = *) ' '
     WRITE (UNIT = nulou,FMT = *) &
          '          --------- End of routine scriprmp_vector ---------'
     CALL FLUSH (nulou)
  ENDIF
  
END SUBROUTINE scriprmp_vector
