!------------------------------------------------------------------------
! Copyright 2010, CERFACS, Toulouse, France.
! All rights reserved. Use is subject to OASIS3 license terms.
!=============================================================================
!
!
PROGRAM model1
  !
  ! Use for netCDF library
  USE netcdf
  ! Use for OASIS communication library
  USE mod_oasis
  !
  ! Use module to read the data
  USE read_all_data
  !
  ! Use module to write the data
  USE write_all_fields
  !
  IMPLICIT NONE
  !
  INCLUDE 'mpif.h'
  !
  ! By default OASIS3-MCT exchanges data in double precision,
  ! but conversion to or from single precision data is supported in the interface
#ifdef NO_USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(6,37)   ! real
#elif defined USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307) ! double
#endif
  !
  CHARACTER(len=30), PARAMETER   :: data_gridname='grids.nc' ! file with the grids
  CHARACTER(len=30), PARAMETER   :: data_maskname='masks.nc' ! file with the masks
  CHARACTER(len=30), PARAMETER   :: data_areaname='areas.nc' ! file with the areas of the cells
  CHARACTER(len=30)              :: data_filename, field_name
  !
  ! Component name (6 characters) same as in the namcouple
  CHARACTER(len=6)   :: comp_name = 'model1'
  CHARACTER(len=128) :: comp_out ! name of the output log file 
  CHARACTER(len=3)   :: chout
  CHARACTER(len=4)   :: cl_grd_src ! name of the source grid
  CHARACTER(len=4)   :: cl_grd_tgt ! name of the target grid
  NAMELIST /grid_source_characteristics/cl_grd_src
  NAMELIST /grid_target_characteristics/cl_grd_tgt
  !
  ! Global grid parameters : 
  INTEGER :: nlon, nlat, ntot    ! dimensions in the 2 directions of space + total size
  INTEGER :: il_size
  INTEGER :: nc             ! number of corners
  REAL (kind=wp), DIMENSION(:,:), POINTER    :: globalgrid_lon,globalgrid_lat ! lon, lat of the points
  REAL (kind=wp), DIMENSION(:,:,:), POINTER  :: globalgrid_clo,globalgrid_cla ! lon, lat of the corners
  REAL (kind=wp), DIMENSION(:,:), POINTER    :: globalgrid_srf ! surface of the grid meshes
  INTEGER, DIMENSION(:,:), POINTER           :: globalgrid_mask ! mask, 0 == valid point, 1 == masked point 
  !
  INTEGER :: mype, npes ! rank and  number of pe
  INTEGER :: localComm  ! local MPI communicator and Initialized
  INTEGER :: comp_id    ! component identification
  !
  INTEGER, DIMENSION(:), ALLOCATABLE :: il_paral ! Decomposition for each proc
  !
  INTEGER :: ierror, rank, w_unit
  INTEGER :: i, j
  INTEGER :: FILE_Debug=1
  !
  ! Names of exchanged Fields
  CHARACTER(len=8), PARAMETER      :: var_name = 'FSENDANA' ! 8 characters field sent by model1 to model2
  !
  ! Used in oasis_def_var and oasis_def_var
  INTEGER                       :: var_id
  INTEGER                       :: var_nodims(2) 
  INTEGER                       :: var_type
  !
  REAL (kind=wp), PARAMETER     :: field_ini = -1. ! initialisation of received fields
  !
  INTEGER               ::  ib
  INTEGER, PARAMETER    ::  il_nb_time_steps = 1 ! number of time steps
  INTEGER, PARAMETER    ::  delta_t = 3600     ! time step
  !
  ! Centers arrays of the local grid
  ! used to calculate the field field1_send sent by the model
  REAL (kind=wp), POINTER :: localgrid_lon (:,:)
  REAL (kind=wp), POINTER :: localgrid_lat (:,:)
  !
  INTEGER                       :: il_flag  ! Flag for grid writing by proc 0
  !
  INTEGER                       :: itap_sec ! Time used in oasis_put/get
  !
  ! Grid parameters definition
  INTEGER                       :: part_id  ! use to connect the partition to the variables
                                            ! in oasis_def_var
  INTEGER                       :: var_actual_shape(4) ! local dimensions of the arrays to the pe
                                                       ! 2 x field rank (= 4 because fields are of rank = 2)
  !
  ! Exchanged local fields arrays
  ! used in routines oasis_put and oasis_get
  REAL (kind=wp),   POINTER     :: field_send(:,:)
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !   INITIALISATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!! OASIS_INIT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL oasis_init_comp (comp_id, comp_name, ierror )
  IF (ierror /= 0) THEN
      WRITE(0,*) 'oasis_init_comp abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 103')
  ENDIF
  !
  ! Unit for output messages : one file for each process
  CALL MPI_Comm_Rank ( MPI_COMM_WORLD, rank, ierror )
  IF (ierror /= 0) THEN
      WRITE(0,*) 'MPI_Comm_Rank abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 110')
  ENDIF
  !
  !!!!!!!!!!!!!!!!! OASIS_GET_LOCALCOMM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL oasis_get_localcomm ( localComm, ierror )
  IF (ierror /= 0) THEN
      WRITE (w_unit,*) 'oasis_get_localcomm abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 118')
  ENDIF
  !
  ! Get MPI size and rank
  CALL MPI_Comm_Size ( localComm, npes, ierror )
  IF (ierror /= 0) THEN
      WRITE(w_unit,*) 'MPI_comm_size abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 125')
  ENDIF
  !
  CALL MPI_Comm_Rank ( localComm, mype, ierror )
  IF (ierror /= 0) THEN
      WRITE (w_unit,*) 'MPI_Comm_Rank abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 131')
  ENDIF
  !
  IF ((FILE_Debug == 1) .AND. (mype == 0)) FILE_Debug=2
  !
  IF (FILE_Debug <= 1) THEN
      IF (mype == 0) THEN
          w_unit = 100 + rank
          WRITE(chout,'(I3)') w_unit
          comp_out=comp_name//'.root_'//chout
          OPEN(w_unit,file=TRIM(comp_out),form='formatted')
      ELSE
          w_unit = 15
          comp_out=comp_name//'.notroot'
          OPEN(w_unit,file=TRIM(comp_out),form='formatted',position='append')
      ENDIF
  ELSE
      w_unit = 100 + rank
      WRITE(chout,'(I3)') w_unit
      comp_out=comp_name//'.out_'//chout
      OPEN(w_unit,file=TRIM(comp_out),form='formatted')
  ENDIF
  !
  IF (FILE_Debug >= 2) THEN
      OPEN(w_unit,file=TRIM(comp_out),form='formatted')
      WRITE (w_unit,*) '-----------------------------------------------------------'
      WRITE (w_unit,*) TRIM(comp_name), ' Running with reals compiled as kind =',wp
      WRITE (w_unit,*) 'I am component ', TRIM(comp_name), ' rank :',rank
      WRITE (w_unit,*) '----------------------------------------------------------'
      WRITE(w_unit,*) 'I am the', TRIM(comp_name), ' ', 'comp', comp_id, 'local rank', mype
      CALL FLUSH(w_unit)
  ENDIF
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  GRID DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Reading global grids.nc, masks.nc and areas.nc netcdf files
  ! Get arguments giving source grid acronym and field type
  ! 
  OPEN(UNIT=70,FILE='name_grids.dat',FORM='FORMATTED')
  READ(UNIT=70,NML=grid_source_characteristics)
  READ(UNIT=70,NML=grid_target_characteristics)
  CLOSE(70)
  !
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'Source grid name :',cl_grd_src
      CALL flush(w_unit)
  ENDIF
  !
  ! Reading dimensions of the global grid
  CALL read_dimgrid(nlon,nlat,data_gridname,cl_grd_src,w_unit,FILE_Debug)
  nc=4
  !
  ! Allocation
  ALLOCATE(globalgrid_lon(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_lon'
  ALLOCATE(globalgrid_lat(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_lat'
  ALLOCATE(globalgrid_clo(nlon,nlat,nc), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_clo'
  ALLOCATE(globalgrid_cla(nlon,nlat,nc), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_cla'
  ALLOCATE(globalgrid_srf(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_srf'
  ALLOCATE(globalgrid_mask(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating indice_mask'
  !
  ! Reading of the longitudes, latitudes, longitude and latitudes of the corners, mask 
  ! and areas of the global grid
  CALL read_grid(nlon,nlat,nc, data_gridname, cl_grd_src, w_unit, FILE_Debug, &
                 globalgrid_lon,globalgrid_lat, &
                 globalgrid_clo,globalgrid_cla)
  CALL read_mask(nlon,nlat, data_maskname, cl_grd_src, w_unit, FILE_Debug, &
                 globalgrid_mask)
  CALL read_area(nlon,nlat, data_areaname, cl_grd_src, w_unit, FILE_Debug, &
                 globalgrid_srf)
  !
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'After grids, masks and areas reading'
      CALL FLUSH(w_unit)
  ENDIF
  !
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  PARTITION DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !
  ! Definition of the partition of the grid 
  ntot=nlon*nlat
#ifdef DECOMP_APPLE
  il_size = 3
#elif defined DECOMP_BOX
  il_size = 5
#endif
  ALLOCATE(il_paral(il_size))
  WRITE(w_unit,*) 'After allocate il_paral, il_size', il_size
  call flush(w_unit)
  !
  CALL decomp_def (part_id,il_paral,il_size,nlon,nlat,mype,npes,w_unit)
  WRITE(w_unit,*) 'After decomp_def, il_paral = ', il_paral(:)
  call flush(w_unit)
  !
  CALL oasis_def_partition (part_id, il_paral, ierror)
  !
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! DEFINITION OF THE LOCAL FIELDS  
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!! !!!!!!!!! OASIS_DEF_VAR !!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  Define transient variables
  !
  var_nodims(1) = 2    ! Rank of the field array is 2
  var_nodims(2) = 1    ! Bundles always 1 for OASIS3
  var_type = OASIS_Real
  !
  var_actual_shape(1) = 1
  var_actual_shape(2) = il_paral(3)
  var_actual_shape(3) = 1 
#ifdef DECOMP_APPLE
  var_actual_shape(4) = 1
#elif defined DECOMP_BOX
  var_actual_shape(4) = il_paral(4)
#endif
  !
  ! Declaration of the field associated with the partition
  CALL oasis_def_var (var_id,var_name, part_id, &
     var_nodims, OASIS_Out, var_actual_shape, var_type, ierror)
  IF (ierror /= 0) THEN
      WRITE (w_unit,*) 'oasis_def_var abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 258')
  ENDIF
  !
  DEALLOCATE(il_paral)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION OF DEFINITION PHASE 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  All processes involved in the coupling must call oasis_enddef; 
  !  here all processes are involved in coupling
  !
  !!!!!!!!!!!!!!!!!! OASIS_ENDDEF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL oasis_enddef ( ierror )
  IF (ierror /= 0) THEN
      WRITE (w_unit,*) 'oasis_enddef abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 274')
  ENDIF
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! SEND ARRAYS 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocate the fields send and received by the model1
  !
  ALLOCATE(field_send(var_actual_shape(2), var_actual_shape(4)), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field1_send'
  !
  ALLOCATE ( localgrid_lon(var_actual_shape(2), var_actual_shape(4)), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_lon'
  !
  ALLOCATE ( localgrid_lat(var_actual_shape(2), var_actual_shape(4)), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_lat'
  !
  ! Calculate the local grid to the process for OASIS3
  !
  CALL oasis3_local_grid(mype, npes, nlon, nlat, var_actual_shape, &
                         localgrid_lon, localgrid_lat,             &
                         globalgrid_lon, globalgrid_lat, w_unit)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!OASIS_PUT/OASIS_GET !!!!!!!!!!!!!!!!!!!!!! 
  !
  ! Data exchange 
  ! 
  ib = 1
  itap_sec = delta_t * (ib-1) ! Time
  !
  CALL function_ana(var_actual_shape(2), &
                    var_actual_shape(4), &
                    localgrid_lon,localgrid_lat, &
                    field_send,ib)
  !
  ! Send FSENDANA
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'tcx sendf ',itap_sec,MINVAL(field_send),MAXVAL(field_send)
  ENDIF
  CALL oasis_put(var_id,itap_sec, field_send, ierror)
  IF ( ierror .NE. OASIS_Ok .AND. ierror .LT. OASIS_Sent) THEN
      WRITE (w_unit,*) 'oasis_put abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 330')
  ENDIF
  !
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  IF (FILE_Debug >= 2) THEN
      WRITE (w_unit,*) 'End of the program, before oasis_terminate'
      CALL FLUSH(w_unit)
  ENDIF
  !
  !!!!!!!!!!!!!!!!!! OASIS_ENDDEF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Collective call to terminate the coupling exchanges
  !
  CALL oasis_terminate (ierror)
  IF (ierror /= 0) THEN
      WRITE (w_unit,*) 'oasis_terminate abort by model1 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 334')
  ENDIF
  !
  !
END PROGRAM MODEL1
!
