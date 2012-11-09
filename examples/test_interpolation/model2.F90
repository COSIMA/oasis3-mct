!------------------------------------------------------------------------
! Copyright 2010, CERFACS, Toulouse, France.
! All rights reserved. Use is subject to OASIS3 license terms.
!=============================================================================
!
PROGRAM model2
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
  CHARACTER(len=6)   :: comp_name = 'model2'
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
  INTEGER :: nc             ! number of corners in the (i,j) plan
  !
  REAL (kind=wp), DIMENSION(:,:), POINTER    :: globalgrid_lon,globalgrid_lat
  REAL (kind=wp), DIMENSION(:,:,:), POINTER  :: globalgrid_clo,globalgrid_cla
  REAL (kind=wp), DIMENSION(:,:), POINTER    :: globalgrid_srf
  INTEGER, DIMENSION(:,:), POINTER           :: globalgrid_mask ! mask, 0 == valid point, 1 == masked point 
  !
  INTEGER :: mype, npes ! rank and number of pe
  INTEGER :: localComm  ! local MPI communicator and Initialized
  INTEGER :: comp_id    ! component identification
  !
  INTEGER, DIMENSION(:), ALLOCATABLE :: il_paral ! Decomposition for each proc
  !
  INTEGER :: ierror, rank, w_unit
  INTEGER :: i, j, ij, ic_msk
  INTEGER :: FILE_Debug=2
  !
  ! Names of exchanged Fields
  CHARACTER(len=8), PARAMETER :: var_name = 'FRECVANA' ! 8 characters field received by the atmosphere from the ocean
  !
  ! Used in oasis_def_var and oasis_def_var
  INTEGER                       :: var_id 
  INTEGER                       :: var_nodims(2) 
  INTEGER                       :: var_type
  !
  REAL (kind=wp), PARAMETER     :: field_ini = -1. ! initialisation of received fields
  INTEGER,        PARAMETER     :: err_msk = -10000
  !
  INTEGER               ::  ib
  INTEGER, PARAMETER    ::  il_nb_time_steps = 1 ! number of time steps
  INTEGER, PARAMETER    ::  delta_t = 3600      ! time step
  !
  ! Centers arrays of the local grid
  ! used to calculate the field sent by the model
  REAL (kind=wp), POINTER :: localgrid_lon (:,:)
  REAL (kind=wp), POINTER :: localgrid_lat (:,:)
  !
  INTEGER                       :: il_flag          ! Flag for grid writing
  !
  INTEGER                       :: itap_sec ! Time
  !
  ! Grid parameter definition
  INTEGER                       :: part_id  ! use to connect the partition to the variables
                                            ! in oasis_def_var
  INTEGER                       :: var_actual_shape(4) ! local dimensions of the arrays to the pe
                                                       ! 2 x field rank (= 4 because fields are of rank = 2)
  !
  ! Exchanged local fields arrays
  ! used in routines oasis_put and oasis_get
  REAL (kind=wp), POINTER       :: field_recv(:,:), field_ana(:,:), error(:,:)
  !
  ! Global array to plot the error by proc 0 and calculate min and max
  REAL (kind=wp), POINTER       :: global_error(:)
  INTEGER, POINTER              :: global_shapes(:,:)
  INTEGER, POINTER              :: displs(:),rcounts(:)
  !
  ! Min and Max of the error of interpolation
  REAL (kind=wp)             :: min,max
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !   INITIALISATION
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL mpi_init(ierror)
  !!!!!!!!!!!!!!!!! OASIS_INIT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL oasis_init_comp (comp_id, comp_name, ierror )
  IF (ierror /= 0) THEN
      WRITE(0,*) 'oasis_init_comp abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 108')
  ENDIF
  !
  ! Unit for output messages : one file for each process
  CALL MPI_Comm_Rank ( MPI_COMM_WORLD, rank, ierror )
  IF (ierror /= 0) THEN
      WRITE(0,*) 'MPI_Comm_Rank abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 115')
  ENDIF
  !
  !!!!!!!!!!!!!!!!! OASIS_GET_LOCALCOMM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL oasis_get_localcomm ( localComm, ierror )
  IF (ierror /= 0) THEN
      WRITE (w_unit,*) 'oasis_get_localcomm abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 123')
  ENDIF
  !
  ! Get MPI size and rank
  CALL MPI_Comm_Size ( localComm, npes, ierror )
  IF (ierror /= 0) THEN
      WRITE(w_unit,*) 'MPI_comm_size abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 130')
  ENDIF
  !
  CALL MPI_Comm_Rank ( localComm, mype, ierror )
  IF (ierror /= 0) THEN
      WRITE (w_unit,*) 'MPI_Comm_Rank abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 136')
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
      WRITE(w_unit,*) 'Target grid name :',cl_grd_tgt
      CALL flush(w_unit)
  ENDIF
  !
  ! Reading dimensions of the grid
  CALL read_dimgrid(nlon,nlat,data_gridname,cl_grd_tgt,w_unit,FILE_Debug)
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
  !
  ! Reading of the longitudes, latitudes, longitude and latitudes of the corners, mask 
  ! and areas of the global grid
  ! Attention peut poser pb de lire toute la grille par tous les procs en HR ...
  CALL read_grid(nlon,nlat,nc, data_gridname, cl_grd_tgt, w_unit, FILE_Debug, &
                 globalgrid_lon,globalgrid_lat, &
                 globalgrid_clo,globalgrid_cla)
  CALL read_mask(nlon,nlat, data_maskname, cl_grd_tgt, w_unit, FILE_Debug, &
                 globalgrid_mask)
  CALL read_area(nlon,nlat, data_areaname, cl_grd_tgt, w_unit, FILE_Debug, &
                 globalgrid_srf)
  !
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'After grids writing'
      CALL FLUSH(w_unit)
  ENDIF
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
  !!!!!!!!!!!!!!!!!! OASIS_DEF_VAR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Define transient variables
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
  ! Declaration of the field associated with the partition of the grid
  CALL oasis_def_var (var_id,var_name, part_id, &
     var_nodims, OASIS_In, var_actual_shape, var_type, ierror)
  IF (ierror /= 0) THEN
      WRITE (w_unit,*) 'oasis_def_var abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 264')
  ENDIF
  !
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
      WRITE (w_unit,*) 'oasis_enddef abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 280')
  ENDIF
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! RECEIVE ARRAYS AND CALCULATE THE ERROR 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocate the fields send and received by the model
  !
  ALLOCATE(field_recv(var_actual_shape(2), var_actual_shape(4)), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field_recv'
  ALLOCATE(field_ana(var_actual_shape(2), var_actual_shape(4)),STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field_ana'
  ALLOCATE(error(var_actual_shape(2), var_actual_shape(4)),STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating error'
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
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!OASIS_PUT/OASIS_GET !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Data exchange
  !
  ! Time loop
  ib=1
  itap_sec = delta_t * (ib-1) ! Time
  !
  CALL function_ana(var_actual_shape(2),&
                    var_actual_shape(4), &
                    globalgrid_lon,globalgrid_lat, &
                    field_ana,ib)
  !
  ! Get the field FRECVANA
  field_recv=field_ini
  CALL oasis_get(var_id,itap_sec, field_recv, ierror)
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'tcx recvf1 ',itap_sec,MINVAL(field_recv),MAXVAL(field_recv)
  ENDIF
  IF ( ierror .NE. OASIS_Ok .AND. ierror .LT. OASIS_Recvd) THEN
      WRITE (w_unit,*) 'oasis_get abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 317')
  ENDIF
  !
  !
  IF (FILE_Debug >= 2) THEN
      WRITE (w_unit,*) 'Calculate the error of interpolation'
      CALL FLUSH(w_unit)
  ENDIF
  !
  error = ABS(((field_ana - field_recv)/field_recv))*100
  !
  !!!!!!!!!!!!!!!!!!!!!!! Write the error and the field in a NetCDF file by proc 0 !!!!!!!!!!!!!!!!!!!!
  ! field_recv is 0 on masked points => put error = err_msk on these points
  !
  IF (mype == 0) THEN
      !
      ALLOCATE(global_error(ntot),STAT=ierror )
      IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating global_error'
      ALLOCATE(global_shapes(4,npes),STAT=ierror )
      IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating global_shapes'
      ALLOCATE(displs(npes),STAT=ierror )
      IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating displs'
      ALLOCATE(rcounts(npes),STAT=ierror )
      IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating rcounts'
      !  
      global_error=0.
      global_shapes=0
      displs=0
      rcounts=0
      !
  ENDIF
  !      
  !
  CALL MPI_GATHER(var_actual_shape,4,MPI_INTEGER,global_shapes,4,MPI_INTEGER,0,localComm,ierror)
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error collecting shapes'
  IF (mype == 0) THEN
      IF (FILE_Debug >= 2) THEN
          WRITE (w_unit,*) 'Shapes of all the processes :',global_shapes
          CALL FLUSH(w_unit)
      ENDIF
  ENDIF
      !
  IF (mype == 0) THEN
      displs(1)=0
      DO i=2,npes
        displs(i) = displs(i-1) + global_shapes(2,i) * global_shapes(4,i)
      ENDDO
      DO i=1,npes
        rcounts(i) = global_shapes(2,i) * global_shapes(4,i)
      ENDDO
      !
      IF (FILE_Debug >= 2) THEN
          WRITE(w_unit,*) 'displs :', displs
          WRITE(w_unit,*) 'rcounts :', rcounts
          CALL FLUSH(w_unit)
      ENDIF
  ENDIF
      !
  PRINT*, 'Laure  var_actual_shape(2)*var_actual_shape(4):',mype,var_actual_shape(2)*var_actual_shape(4)
  CALL flush()
#ifdef NO_USE_DOUBLE_PRECISION
      CALL MPI_gatherv(error, var_actual_shape(2)*var_actual_shape(4),MPI_REAL,&
                       global_error,rcounts,displs,MPI_REAL,0,localComm,ierror)
      IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error collecting errors'
      !
#elif defined USE_DOUBLE_PRECISION
      CALL MPI_gatherv(error, var_actual_shape(2)*var_actual_shape(4),MPI_DOUBLE_PRECISION,&
                       global_error,rcounts,displs,MPI_DOUBLE_PRECISION,0,localComm,ierror)
      IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error collecting errors'
      !
#endif
      !
  IF (mype == 0) THEN
      DO i=1,nlon
        DO j=1,nlat
          ij=i+(j-1)*(nlon)
          IF ( globalgrid_mask(i,j) == 1 ) THEN
              global_error(ij) = -err_msk
          ENDIF
        ENDDO
      ENDDO
      data_filename='error_'//cl_grd_tgt//'.nc'
      field_name='error'
!      CALL write_field(nlon,nlat, &
!                       data_filename, field_name,  &
!                       w_unit, FILE_Debug, &
!                       globalgrid_lon, globalgrid_lat, global_error)
  ENDIF
  !
  !!!!!!!!!!!!!!!!!!!! Min and Max of the error on non masked points calculated by proc 0 !!!!!!!!!!!!!!!!
  !
  IF (mype == 0) THEN
      PRINT*, 'Laure global_error :', global_error
  ENDIF

  IF (mype == 0) THEN
      ic_msk=0
      DO i=1,nlon
        DO j=1,nlat
          ij=i+(j-1)*(nlon)
          IF ( globalgrid_mask(i,j) == 1 ) THEN
              global_error(ij) = -err_msk
              ic_msk = ic_msk + 1
          ENDIF
        ENDDO
      ENDDO
      min=MINVAL(REAL(global_error))
      IF (FILE_Debug >= 2) THEN
          WRITE(w_unit,*) 'Min and its location in the error field : ',min
          WRITE(w_unit,*) MINLOC(REAL(global_error))
      ENDIF
      DO i=1,nlon
        DO j=1,nlat
          ij=i+(j-1)*(nlon)
          IF ( globalgrid_mask(i,j) == 1 ) THEN
              global_error(ij) = err_msk
          ENDIF
        ENDDO
      ENDDO
      max=MAXVAL(REAL(global_error))
      IF (FILE_Debug >= 2) THEN
          WRITE(w_unit,*)'Max and its location in the error field : ',max
          WRITE(w_unit,*) MAXLOC(REAL(global_error))
      ENDIF
      DO i=1,nlon
        DO j=1,nlat
          ij=i+(j-1)*(nlon)
          IF ( globalgrid_mask(i,j) == 1 ) THEN
              global_error(ij) = 0.
          ENDIF
        ENDDO
      ENDDO
      IF (FILE_Debug >= 2) THEN
          WRITE(w_unit,*) 'Number of masked points :',ic_msk
          WRITE(w_unit,*) 'Error mean on non masked points: ', &
             SUM(ABS(global_error))/((ntot)-ic_msk)
          WRITE(w_unit,*) 'End calculation of stat on the error'
      ENDIF
  ENDIF
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
      WRITE (w_unit,*) 'oasis_terminate abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 407')
  ENDIF
  !
  CALL mpi_finalize(ierror)
  !
END PROGRAM MODEL2
!
