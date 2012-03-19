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
  IMPLICIT NONE

  INCLUDE 'mpif.h'

  !
  ! By default OASIS3 exchanges data in double precision.
  ! To exchange data in single precision with OASIS3, 
  ! the coupler has to be compiled with CPP key "use_realtype_single" 
  ! and the model with CPP key "NO_USE_DOUBLE_PRECISION"
#ifdef NO_USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(6,37)   ! real
#else
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307) ! double
#endif
  !
  CHARACTER(len=30), PARAMETER :: data_filename='grid_model2.nc'
  ! Component name (6 characters) same as in the namcouple
  CHARACTER(len=6)   :: comp_name = 'toyatm'
  CHARACTER(len=128) :: comp_out ! name of the output log file
  CHARACTER(len=3)   :: chout
  !
  ! Global grid parameters : 
  INTEGER :: nlon, nlat     ! dimensions in the 2 directions of space
  INTEGER :: ntot           ! total dimension
  INTEGER :: il_size
  INTEGER :: nbr_corners_ij ! number of corners in the (i,j) plan
  !
  DOUBLE PRECISION, DIMENSION(:), POINTER      :: globalgrid_lon,globalgrid_lat
  DOUBLE PRECISION, DIMENSION(:,:), POINTER    :: globalgrid_clo,globalgrid_cla
  INTEGER, DIMENSION(:,:), POINTER           :: indice_mask ! mask, 0 == valid point, 1 == masked point 
  !
  INTEGER :: mype, npes ! rank and number of pe
  INTEGER :: localComm  ! local MPI communicator and Initialized
  INTEGER :: comp_id    ! component identification
  !
  INTEGER, DIMENSION(:), ALLOCATABLE :: il_paral ! Decomposition for each proc
  !
  INTEGER :: ierror, rank, w_unit
  INTEGER :: i, j
  !
  ! Names of exchanged Fields
  CHARACTER(len=8), PARAMETER :: var_name1 = 'FRECVATM' ! 8 characters field received by the atmosphere from the ocean
  CHARACTER(len=8), PARAMETER :: var_name2 = 'FSENDATM' ! 8 characters field sent by the atmosphere to the ocean
  !
  ! Used in oasis_def_var and oasis_def_var
  INTEGER                       :: var_id(3) 
  INTEGER                       :: var_nodims(2) 
  INTEGER                       :: var_type
  !
  REAL (kind=wp), PARAMETER     :: field_ini = -1. ! initialisation of received fields
  !
  INTEGER               ::  ib
  INTEGER, PARAMETER    ::  il_nb_time_steps = 6 ! number of time steps
  INTEGER, PARAMETER    ::  delta_t = 3600       ! time step
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
  ! Centers and corners of the global grid for writing the
  ! files grids.nc and masks.nc by proc 0
  ! and used to calculate the field field2_send sent by the model
  DOUBLE PRECISION, POINTER     :: lon(:,:),lat(:,:)
  DOUBLE PRECISION, POINTER     :: clo(:,:,:),cla(:,:,:)
  !
  ! Exchanged local fields arrays
  ! used in routines oasis_put and oasis_get
  REAL (kind=wp), POINTER       :: field1_recv(:,:)
  REAL (kind=wp), POINTER       :: field2_send(:,:)
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !   INITIALISATION
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!! OASIS_INIT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL oasis_init_comp (comp_id, comp_name, ierror )
  IF (ierror /= 0) CALL oasis_abort(comp_id, 'oasis_init_comp', 'Pb in model2')
  !
  ! Unit for output messages : one file for each process
  CALL MPI_Comm_Rank ( MPI_COMM_WORLD, rank, ierror )
  IF (ierror /= 0) CALL oasis_abort(comp_id, 'MPI_Comm_Rank','Pb in model2')
  !
  !
  w_unit=100+rank
  WRITE(chout,'(I3)') w_unit
  comp_out=comp_name//'.out_'//chout
  !
  OPEN(w_unit,file=TRIM(comp_out),form='formatted')
  WRITE (w_unit,*) '-----------------------------------------------------------'
  WRITE (w_unit,*) TRIM(comp_name), ' Running with reals compiled as kind =',wp
  WRITE (w_unit,*) 'I am component ', TRIM(comp_name), ' rank :',rank
  WRITE (w_unit,*) '----------------------------------------------------------'
  CALL flush(w_unit)
  !
  !!!!!!!!!!!!!!!!! OASIS_GET_LOCALCOMM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL oasis_get_localcomm ( localComm, ierror )
  IF (ierror /= 0) CALL oasis_abort(comp_id,'oasis_get_localcomm','Pb in model2')
  !
  ! Get MPI size and rank
  CALL MPI_Comm_Size ( localComm, npes, ierror )
  IF (ierror /= 0) CALL oasis_abort(comp_id, 'MPI_Comm_Size','Pb in model2')
  !
  CALL MPI_Comm_Rank ( localComm, mype, ierror )
  IF (ierror /= 0) CALL oasis_abort(comp_id, 'MPI_Comm_Rank','Pb in model2')
  !
  WRITE(w_unit,*) 'I am the', TRIM(comp_name), ' ',' comp', comp_id, ' local rank ', mype
  CALL flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  GRID DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Reading netcdf file with pre-defined variable names
  !
  ! Reading dimensions of the grid
  CALL read_dim_reg(nlon,nlat,nbr_corners_ij,data_filename,w_unit)
  !
  ! Allocation
  ALLOCATE(globalgrid_lon(nlon), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_lon'
  ALLOCATE(globalgrid_lat(nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_lat'
  ALLOCATE(globalgrid_clo(nlon,nbr_corners_ij), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_clo'
  ALLOCATE(globalgrid_cla(nlat,nbr_corners_ij), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_cla'
  ALLOCATE(indice_mask(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating indice_mask'
  !
  ! Reading of the longitudes, latitudes, longitude and latitudes of the corners, mask of the grid
  CALL read_grid_reg(nlon,nlat,nbr_corners_ij,data_filename,w_unit, &
                              globalgrid_lon,globalgrid_lat, &
                              globalgrid_clo,globalgrid_cla, &
                              indice_mask)
  !
  !
  ! (Global) grid definition for OASIS3
  ! Writing of the file grids.nc and masks.nc by the processor 0 from the grid read in 
  ALLOCATE(lon(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating lon'
  ALLOCATE(lat(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating lat'
  ALLOCATE(clo(nlon,nlat,4), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating clo'
  ALLOCATE(cla(nlon,nlat,4), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating cla'
  !
  DO j=1,nlat
    DO i=1,nlon
      lon(i,j) = globalgrid_lon(i)
      lat(i,j) = globalgrid_lat(j)
    ENDDO
  ENDDO
  DO j=1,nlat
    DO i=1,nlon
      clo(i,j,1:2) = globalgrid_clo(i,1:2)
      cla(i,j,1:2) = globalgrid_cla(j,1:2)
      clo(i,j,3) = globalgrid_clo(i,2)
      clo(i,j,4) = globalgrid_clo(i,1)
      cla(i,j,3) = globalgrid_cla(j,2)
      cla(i,j,4) = globalgrid_cla(j,1)
    ENDDO
  ENDDO
  ! This functionality is indeed not covered by OASIS3-MCT yet.
!!$  IF (mype == 0) THEN
!!$      !
!!$      CALL oasis_start_grids_writing(il_flag)
!!$      CALL oasis_write_grid('lmdz', nlon, nlat, lon, lat)
!!$      CALL oasis_write_corner('lmdz', nlon, nlat, 4, clo, cla)
!!$      CALL oasis_write_mask('lmdz', nlon, nlat, indice_mask(:,:))
!!$      CALL oasis_terminate_grids_writing()
!!$  ENDIF
!!$  WRITE(w_unit,*) 'After grids writing'
!!$  call flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  PARTITION DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !
  ! Definition of the partition of the grid (calling oasis_def_partition)
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
  CALL oasis_def_partition (part_id, il_paral, ierror)
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
  ! Declaration of the field associated with the partition of the grid
  CALL oasis_def_var (var_id(1),var_name1, part_id, &
     var_nodims, OASIS_In, var_actual_shape, var_type, ierror)
  IF (ierror /= 0) CALL oasis_abort(comp_id, 'oasis_def_var','Pb in model2')
  !
  CALL oasis_def_var (var_id(2),var_name2, part_id, &
     var_nodims, OASIS_Out, var_actual_shape, var_type, ierror)
  IF (ierror /= 0) CALL oasis_abort(comp_id, 'oasis_def_var','Pb in model2')
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
  IF (ierror /= 0) CALL oasis_abort(comp_id, 'oasis_enddef','Pb in model2')
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! SEND AND RECEIVE ARRAYS 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocate the fields send and received by the model
  !
  ALLOCATE(field1_recv(var_actual_shape(2), var_actual_shape(4)), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field1_recv'
  !
  ALLOCATE(field2_send(var_actual_shape(2), var_actual_shape(4)),STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field2_send'
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
                         lon, lat, w_unit)
  !
  DEALLOCATE(il_paral)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!OASIS_PUT/OASIS_GET !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Data exchange
  !
  ! Time loop
  DO ib=1, il_nb_time_steps
    itap_sec = delta_t * (ib-1) ! Time
    !
    CALL function_sent(var_actual_shape(2),&
                       var_actual_shape(4), &
                       localgrid_lon,localgrid_lat, &
                       field2_send,ib)
    !
    ! Get the field FRECVATM
    field1_recv=field_ini
    CALL oasis_get(var_id(1),itap_sec, field1_recv, ierror)
    write(w_unit,*) 'tcx recvf1 ',itap_sec,minval(field1_recv),maxval(field1_recv)
    IF ( ierror .NE. OASIS_Ok .AND. ierror .LT. OASIS_Recvd) &
    CALL oasis_abort(comp_id, 'oasis_get','Pb in model2')
    !
    ! Send the field FSENDATM
    write(w_unit,*) 'tcx sendf2 ',itap_sec,minval(field2_send),maxval(field2_send)
    CALL oasis_put(var_id(2),itap_sec, field2_send, ierror)
    IF ( ierror .NE. OASIS_Ok .AND. ierror .LT. OASIS_Sent) &
    CALL oasis_abort(comp_id, 'oasis_put','Pb in model2')
    !
  ENDDO
  !
  WRITE (w_unit,*) 'End of the program, before oasis_terminate'
  CALL flush(w_unit)
  CLOSE(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!!! OASIS_ENDDEF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Collective call to terminate the coupling exchanges
  !
  CALL oasis_terminate (ierror)
  IF (ierror /= 0) CALL oasis_abort(comp_id, 'oasis_terminate', 'Pb in model1')
  !
END PROGRAM MODEL2
!
