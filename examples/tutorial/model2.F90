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
#elif defined USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307) ! double
#endif
  !
  CHARACTER(len=30), PARAMETER :: data_filename='grid_model2.nc'
  ! Component name (6 characters) same as in the namcouple
  CHARACTER(len=6)   :: comp_name = 'model2'
  CHARACTER(len=128) :: comp_out ! name of the output log file
  CHARACTER(len=3)   :: chout
  !
  ! Global grid parameters : 
  INTEGER :: nlon, nlat     ! dimensions in the 2 directions of space
  INTEGER :: ntot           ! total dimension
  INTEGER :: il_paral_size
  INTEGER :: nc ! number of corners in the (i,j) plan
  INTEGER :: indi_beg, indi_end, indj_beg, indj_end
  !
  DOUBLE PRECISION, DIMENSION(:,:), POINTER   :: globalgrid_lon,globalgrid_lat
  DOUBLE PRECISION, DIMENSION(:,:), POINTER   :: localgrid_lon,localgrid_lat
  DOUBLE PRECISION, DIMENSION(:,:,:), POINTER :: globalgrid_clo,globalgrid_cla
  DOUBLE PRECISION, DIMENSION(:,:), POINTER   :: globalgrid_srf
  INTEGER, DIMENSION(:,:), POINTER            :: indice_mask ! mask, 0 == valid point, 1 == masked point 
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
  INTEGER                   :: var_id(2) 
  INTEGER                   :: var_nodims(2) 
  INTEGER                   :: var_type
  !
  REAL (kind=wp), PARAMETER :: field_ini = -1. ! initialisation of received fields
  !
  INTEGER               ::  ib
  INTEGER, PARAMETER    ::  il_nb_time_steps = 12 ! number of time steps
  INTEGER, PARAMETER    ::  delta_t = 1800       ! time step
  !
  INTEGER                :: il_flag          ! Flag for grid writing
  !
  INTEGER                :: itap_sec ! Time
  !
  ! Grid parameter definition
  INTEGER                :: part_id  ! use to connect the partition to the variables
                                     ! in oasis_def_var
  INTEGER                :: var_actual_shape(4) ! local dimensions of the arrays to the pe
                                                ! 2 x field rank (= 4 because fields are of rank = 2)
  !
  ! Exchanged local fields arrays
  ! used in routines oasis_put and oasis_get
  REAL (kind=wp), POINTER :: field1_recv(:,:)
  REAL (kind=wp), POINTER :: field2_send(:,:)
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !   INITIALISATION
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL MPI_Init(ierror)
  !!!!!!!!!!!!!!!!! OASIS_INIT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! TOCOMPLETE - Put here OASIS initialisation call !
  !
  !
  ! Unit for output messages : one file for each process
  CALL MPI_Comm_Rank ( MPI_COMM_WORLD, rank, ierror )
  IF (ierror /= 0) THEN
      WRITE(0,*) 'MPI_Comm_Rank abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 102')
  ENDIF
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
  localComm = MPI_COMM_WORLD
  ! TOCOMPLETE - Put here OASIS call to get local MPI communicator !
  !
  ! Get MPI size and rank
  CALL MPI_Comm_Size ( localComm, npes, ierror )
  IF (ierror /= 0) THEN
      WRITE(w_unit,*) 'MPI_comm_size abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 127')
  ENDIF
  !
  CALL MPI_Comm_Rank ( localComm, mype, ierror )
  IF (ierror /= 0) THEN
      WRITE (w_unit,*) 'MPI_Comm_Rank abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 133')
  ENDIF
  !
  WRITE(w_unit,*) 'I am the ', TRIM(comp_name), ' local rank ', mype
  WRITE (w_unit,*) 'Number of processors :',npes
  CALL flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  GRID DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Reading netcdf file with pre-defined variable names
  !
  ! Reading dimensions of the grid
  CALL read_dimgrid(nlon,nlat,data_filename,w_unit)
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
  ALLOCATE(indice_mask(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating indice_mask'
  !
  ! Reading of the longitudes, latitudes, longitude and latitudes of the corners, mask of the grid
  CALL read_grid(nlon,nlat,nc,data_filename,w_unit, &
                 globalgrid_lon,globalgrid_lat, &
                 globalgrid_clo,globalgrid_cla, &
                 globalgrid_srf, &
                 indice_mask)
  !
  ! (Global) grid definition for OASIS
  ! Writing of the file grids.nc and masks.nc by the processor 0 from the grid read in 
  !
  IF (mype == 0) THEN
      !
      ! Mask inversion to follow (historical) OASIS convention (0=not masked;1=masked)
      WHERE(indice_mask == 1) 
          indice_mask=0
      ELSEWHERE
          indice_mask=1
      END WHERE
      !
      ! TOCOMPLETE - Put here OASIS grid, corner, areas and mask writing calls !
      !
  ENDIF
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  PARTITION DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !
  ! Definition of the partition of the grid (calling oasis_def_partition)
  ntot=nlon*nlat
#ifdef DECOMP_APPLE
  il_paral_size = 3
#elif defined DECOMP_BOX
  il_paral_size = 5
#endif
  ALLOCATE(il_paral(il_paral_size))
  WRITE(w_unit,*) 'After allocate il_paral, il_paral_size', il_paral_size
  call flush(w_unit)
  !
  CALL decomp_def (il_paral,il_paral_size,nlon,nlat,mype,npes,w_unit)
  WRITE(w_unit,*) 'After decomp_def, il_paral = ', il_paral(:)
  call flush(w_unit)
  !
  ! TOCOMPLETE - Put here OASIS call to define local partition !
  ! The data are exchanged in the global grid so you do not need to pass 
  ! isize to oasis_def_partition
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
  ! Declaration of the field associated with the partition of the grid
  !
  ! TOCOMPLETE - Put here OASIS call to declare the 2 coupling field
  !              FRECVATM, FSENDATM !
  ! var_name1 = 'FRECVATM'
  ! var_name2 = 'FSENDATM'
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION OF DEFINITION PHASE 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  All processes involved in the coupling must call oasis_enddef; 
  !  here all processes are involved in coupling
  !
  !!!!!!!!!!!!!!!!!! OASIS_ENDDEF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! TOCOMPLETE - Put here the OASIS call to end the definition phase
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
  ALLOCATE(localgrid_lon(var_actual_shape(2), var_actual_shape(4)), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_lon'
  !
  ALLOCATE(localgrid_lat(var_actual_shape(2), var_actual_shape(4)), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_lat'
  !
  DEALLOCATE(il_paral)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!OASIS_PUT/OASIS_GET !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  indi_beg=1 ; indi_end=nlon
  indj_beg=((nlat/npes)*mype)+1 
  !
  IF (mype .LT. npes - 1) THEN
      indj_end = (nlat/npes)*(mype+1)
  ELSE
      indj_end = nlat 
  ENDIF
  !
  ! Local grid
  !
  localgrid_lon=RESHAPE(globalgrid_lon(indi_beg:indi_end,indj_beg:indj_end),&
                               (/ var_actual_shape(2), var_actual_shape(4) /))
  localgrid_lat=RESHAPE(globalgrid_lat(indi_beg:indi_end,indj_beg:indj_end),&
                               (/ var_actual_shape(2), var_actual_shape(4) /))
  !
  ! Data exchange
  !
  ! Time loop
  DO ib=1, il_nb_time_steps
    itap_sec = delta_t * (ib-1) ! Time
    !
    !
    ! Get the field FRECVATM
    field1_recv=field_ini
    ! TOCOMPLETE - Put here the OASIS call to receive FRECVATM (field1_recv)
    ! Let's suppose here that FRECVATM contains BC needed for the timestep
    !
    ! Here the model computes its timestep
    !
    CALL function_sent(var_actual_shape(2), var_actual_shape(4), &
                       localgrid_lon, localgrid_lat, field2_send,ib)
    !
    ! Send the field FSENDATM
    ! TOCOMPLETE - Put here the OASIS call to send FSENDATM (field2_send)
    !
    !
  ENDDO
  !
  WRITE (w_unit,*) 'End of the program'
  CALL flush(w_unit)
  CLOSE(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!!! OASIS_TERMINATE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Collective call to terminate the coupling exchanges
  !
  ! TOCOMPLETE - Put here the OASIS call to terminate the coupling
  !
  !
  call mpi_finalize(ierror)
END PROGRAM MODEL2
!
