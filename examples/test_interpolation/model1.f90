!------------------------------------------------------------------------
! Copyright 2017, CERFACS, Toulouse, France.
! All rights reserved. Use is subject to OASIS3 license terms.
!=============================================================================
!
PROGRAM model1
  !
  USE netcdf
  USE mod_oasis
  USE read_all_data
  !
  IMPLICIT NONE
  !
  INCLUDE 'mpif.h'
  !
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307) ! double
  !
  CHARACTER(len=30), PARAMETER   :: data_gridname='grids.nc' ! file with the grids
  CHARACTER(len=30), PARAMETER   :: data_maskname='masks.nc' ! file with the masks
CHARACTER(len=30)              :: data_filename, field_name
  !
  ! Component name (6 characters) same as in the namcouple
  CHARACTER(len=6)   :: comp_name = 'model1'
  CHARACTER(len=128) :: comp_out ! name of the output log file
  CHARACTER(len=11)   :: cl_remap      ! type of remapping
  CHARACTER(len=4)   :: cl_grd_src ! name of the source grid
  CHARACTER(len=2)   :: cl_type_src ! type of the source grid
  CHARACTER(len=8)   :: cl_period_src ! Periodicity of grid (P=periodic or R=regional)
  INTEGER            :: il_overlap_src
  NAMELIST /grid_source_characteristics/cl_grd_src
  NAMELIST /grid_source_characteristics/cl_remap
  NAMELIST /grid_source_characteristics/cl_type_src
  NAMELIST /grid_source_characteristics/cl_period_src
  NAMELIST /grid_source_characteristics/il_overlap_src
  !
  ! Global grid parameters : 
  INTEGER :: nlon, nlat, ntot    ! dimensions in the 2 directions of space + total size
  REAL (kind=wp), DIMENSION(:,:), POINTER  :: gg_lon,gg_lat ! lon, lat of the points
  INTEGER, DIMENSION(:,:), POINTER         :: gg_mask ! mask, 0 == valid point, 1 == masked point 
  !
  INTEGER :: localComm  ! local MPI communicator and Initialized
  INTEGER :: comp_id    ! component identification
  !
  INTEGER, DIMENSION(3) :: il_paral ! Decomposition for each proc
  !
  INTEGER :: w_unit=100
  INTEGER :: ierror, i, j
  INTEGER :: FILE_Debug=2
  !
  ! Names of exchanged Fields
  CHARACTER(len=8), PARAMETER :: var_name = 'FSENDANA' ! 8 characters field sent by model1
  !
  ! Used in oasis_def_var and oasis_def_var
  INTEGER                       :: var_id
  INTEGER                       :: var_nodims(2) 
  INTEGER                       :: var_type
  !
  REAL (kind=wp), PARAMETER    :: coef = 2.
  REAL (kind=wp), PARAMETER    :: dp_pi=3.14159265359
  REAL (kind=wp), PARAMETER    :: dp_length= 1.2*dp_pi
  REAL (kind=wp), PARAMETER    :: dp_conv = dp_pi/180.
  !
  ! Grid parameters definition
  INTEGER                       :: part_id  ! use to connect the partition to the variables
  INTEGER                       :: var_sh(4) ! local dimensions of the arrays; 2 x rank (=4)
  !
  ! Exchanged local fields arrays
  REAL (kind=wp),   POINTER     :: field_send(:,:)
  REAL (kind=wp),   POINTER     :: gradient_i(:,:), gradient_j(:,:), gradient_ij(:,:)
  REAL (kind=wp),   POINTER     :: grad_lat(:,:), grad_lon(:,:)
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  INITIALISATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL oasis_init_comp (comp_id, comp_name, ierror )
  IF (ierror /= 0) THEN
      WRITE(0,*) 'oasis_init_comp abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 100')
  ENDIF
  !
  CALL oasis_get_localcomm ( localComm, ierror )
  IF (ierror /= 0) THEN
      WRITE (0,*) 'oasis_get_localcomm abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 116')
  ENDIF
  !
  IF (FILE_Debug >= 2) THEN
      comp_out=comp_name//'.out'
      OPEN(w_unit,file=TRIM(comp_out),form='formatted')
      WRITE(w_unit,*) '-----------------------------------------------------------'
      WRITE(w_unit,*) TRIM(comp_name), ' Running with reals compiled as kind =',wp
      WRITE(w_unit,*) '----------------------------------------------------------'
      WRITE(w_unit,*) 'I am ', TRIM(comp_name), ' ', comp_id
      CALL FLUSH(w_unit)
  ENDIF
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  GRID DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Reading global grids.nc and masks.nc netcdf files
  ! Get arguments giving source grid acronym and field type
  ! 
  OPEN(UNIT=70,FILE='name_grids.dat',FORM='FORMATTED')
  READ(UNIT=70,NML=grid_source_characteristics)
  CLOSE(70)
  !
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'Source grid name :',cl_grd_src
      WRITE(w_unit,*) 'Source grid type :',cl_type_src
      CALL flush(w_unit)
  ENDIF
  !
  ! Reading dimensions of the global grid
  CALL read_dimgrid(nlon,nlat,data_gridname,cl_grd_src,w_unit,FILE_Debug)
  !
  ! Allocate grid arrays
  ALLOCATE(gg_lon(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating gg_lon'
  ALLOCATE(gg_lat(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating gg_lat'
  ALLOCATE(gg_mask(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating indice_mask'
  !
  ! Read global grid longitudes, latitudes, corners, mask 
  CALL read_grid(nlon,nlat, data_gridname, cl_grd_src, w_unit, FILE_Debug, gg_lon,gg_lat)
  CALL read_mask(nlon,nlat, data_maskname, cl_grd_src, w_unit, FILE_Debug, gg_mask)
  !
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'After grid and mask reading'
      CALL FLUSH(w_unit)
  ENDIF
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  PARTITION DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !
  il_paral(1)=0
  il_paral(2)=0
  il_paral(3)=nlon*nlat
  !
  CALL oasis_def_partition (part_id, il_paral, ierror)
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'After oasis_def_partition'
      CALL FLUSH(w_unit)
  ENDIF
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  COUPLING FIELD DECLARATION  
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  var_nodims(1) = 2    ! Rank of the field array is 2
  var_nodims(2) = 1    ! Bundles always 1 for OASIS3
  var_type = OASIS_Real
  !
  var_sh(1) = 1
  var_sh(2) = nlon
  var_sh(3) = 1 
  var_sh(4) = nlat
  !
  CALL oasis_def_var (var_id,var_name, part_id, &
     var_nodims, OASIS_Out, var_sh, var_type, ierror)
  IF (ierror /= 0) THEN
      WRITE(w_unit,*) 'oasis_def_var abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 256')
  ENDIF
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'After def_var'
      CALL FLUSH(w_unit)
  ENDIF
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  TERMINATION OF DEFINITION PHASE 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL oasis_enddef ( ierror )
  IF (ierror /= 0) THEN
      WRITE(w_unit,*) 'oasis_enddef abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 272')
  ENDIF
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  SEND ARRAYS 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocate the fields send and received by the model1
  !
  ALLOCATE(field_send(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field1_send'
  !
  DO j=1,nlat
   DO i=1,nlon
      field_send(i,j) =  coef - COS(dp_pi*(ACOS(COS(gg_lon(i,j)* dp_conv)* &
         COS(gg_lat(i,j)* dp_conv))/dp_length))
    ENDDO
  ENDDO
  !
  ! Calculate the gradients and send them for bicubic only if grid is not gaussian reduced
  IF (cl_remap == 'bicu') THEN
     IF ( trim(cl_type_src) == 'LR') THEN
        ALLOCATE(gradient_i(nlon,nlat), STAT=ierror )
        IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating gradient_i'
        ALLOCATE(gradient_j(nlon,nlat), STAT=ierror )
        IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating gradient_j'
        ALLOCATE(gradient_ij(nlon,nlat), STAT=ierror )
        IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating gradient_ij'
        call gradient_bicubic(nlon, nlat, field_send, gg_mask, gg_lat, gg_lon, il_overlap_src,  &
                                  cl_period_src, gradient_i, gradient_j, gradient_ij)
        IF (FILE_Debug >= 2) THEN
           WRITE(w_unit,*) 'Bicubic_gradient calculated '
           CALL FLUSH(w_unit)
        ENDIF
        ! For BICUBIC, need to transfer three extra arguments :
        call oasis_put(var_id, 0, field_send, ierror, gradient_i, gradient_j, gradient_ij)
     ELSE IF ( trim(cl_type_src) == 'D') THEN
        call oasis_put(var_id, 0, field_send, ierror)
     ELSE
        WRITE(w_unit,*) 'Cannot perform bicubic interpolation for type of grid ',cl_type_src
        CALL oasis_abort(comp_id,comp_name,'Bicubic interpolation impossible for that grid')
     ENDIF

  ! Calculate the gradients and send them for conserv 2nd only if grid is not gaussian reduced
  ELSE IF (cl_remap == 'fracarea2nd') THEN
     IF ( trim(cl_type_src) == 'LR') THEN
        ALLOCATE(grad_lat(nlon,nlat), STAT=ierror )
        IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating gradient_i'
        ALLOCATE(grad_lon(nlon,nlat), STAT=ierror )
        IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating gradient_j'
        call gradient_conserv(nlon, nlat, field_send, gg_mask, gg_lat, gg_lon, &
                    & il_overlap_src, cl_period_src, grad_lat, grad_lon)
        IF (FILE_Debug >= 2) THEN
           WRITE(w_unit,*) 'Conservative gradient calculated '
           CALL FLUSH(w_unit)
        ENDIF
        ! For CONSERV/SECOND, need to transfer two extra arguments :
        call oasis_put(var_id, 0, field_send, ierror, grad_lat, grad_lon)
     ELSE
        WRITE(w_unit,*) 'Cannot perform second order conserv interpolation for type of grid ',cl_type_src
        CALL oasis_abort(comp_id,comp_name,'Second order conserv interpolation impossible for that grid')
     ENDIF

  ELSE
     call oasis_put(var_id, 0, field_send, ierror)
  ENDIF
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'End of the program, before oasis_terminate'
      CALL FLUSH(w_unit)
  ENDIF
  !
  CALL oasis_terminate (ierror)
  IF (ierror /= 0) THEN
      WRITE(w_unit,*) 'oasis_terminate abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 332')
  ENDIF
  !
  !
END PROGRAM MODEL1
!
