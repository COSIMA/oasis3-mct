!------------------------------------------------------------------------
! Copyright 2010, CERFACS, Toulouse, France.
! All rights reserved. Use is subject to OASIS3 license terms.
!=============================================================================
!
PROGRAM model1

  ! Use for netCDF library
  USE netcdf
  ! Use for OASIS communication library
  USE mod_prism

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
  CHARACTER(len=30), PARAMETER   :: data_filename='grid_model1.nc'
  ! Component name (6 characters) same as in the namcouple
  CHARACTER(len=6)   :: comp_name = 'model1'
  CHARACTER(len=128) :: comp_out ! name of the output log file 
  CHARACTER(len=3)   :: chout

  ! Global grid parameters : 
  INTEGER :: nlon, nlat     ! dimensions in the 2 directions of space
  INTEGER :: ntot           ! total dimension
  INTEGER :: il_size
  INTEGER :: nbr_corners_ij ! number of corners in the (i,j) plan
  DOUBLE PRECISION, DIMENSION(:,:)  , POINTER :: globalgrid_lon,globalgrid_lat ! lon, lat of the points
  DOUBLE PRECISION, DIMENSION(:,:,:), POINTER :: globalgrid_clo,globalgrid_cla ! lon, lat of the corners
  INTEGER         , DIMENSION(:,:)  , POINTER :: indice_mask ! mask, 0 == valid point, 1 == masked point 
  DOUBLE PRECISION, DIMENSION(:,:)  , POINTER :: globalgrid_area ! area

  INTEGER :: mype, npes ! rank and  number of pe
  INTEGER :: localComm  ! local MPI communicator and Initialized
  INTEGER :: comp_id    ! component identification

  INTEGER, DIMENSION(:), ALLOCATABLE :: il_paral ! Decomposition for each proc

  INTEGER :: ierror, rank, w_unit
  INTEGER :: i, j, n, m, mm1, mp1, nm1, np1
  DOUBLE PRECISION :: cla1,cla2,cla3,cla4,clo1,clo2,clo3,clo4
  DOUBLE PRECISION :: dlon,dlat
  DOUBLE PRECISION,parameter :: pi = 3.14159265358979323846
  DOUBLE PRECISION,parameter :: eradius = 6371229.    ! meters

  character(len=*),parameter :: F01 = '(a,i6,i12,3g15.8)'

  ! Used in prism_def_var and prism_def_var_proto
  integer, parameter :: mvar = 10
  integer, parameter :: nvar = 7
  character(len=8),parameter :: var_name(mvar) =  &
     (/'M1FLD01 ','M1FLD02 ','M1FLD03 ','M1FLD04 ','M1FLD05 ', &
       'M1FLD06 ','M1FLD07 ','M1FLD08 ','M1FLD09 ','M1FLD10 '/)
  logical, parameter :: var_out(mvar) = &
     (/.true.,.true.,.false.,.false.,.true., &
       .true.,.false.,.false.,.true.,.true./)

  INTEGER                       :: var_id(mvar) 
  INTEGER                       :: var_nodims(2) 
  INTEGER                       :: var_type
  !
  REAL (kind=wp), PARAMETER     :: field_ini = -1. ! initialisation of received fields
  !
  character(len=*),parameter ::  timefile = 'model1.timefile.txt'
  logical               ::  exists
  INTEGER               ::  ib
  INTEGER               ::  start_t = 0
  INTEGER               ::  il_nb_time_steps = 12 ! number of time steps
  INTEGER               ::  delta_t = 3600       ! time step
  !
  ! Centers arrays of the local grid
  ! used to calculate the field field1_send sent by the model
  REAL (kind=wp), POINTER :: localgrid_lon (:,:)
  REAL (kind=wp), POINTER :: localgrid_lat (:,:)
  !
  INTEGER                       :: il_flag  ! Flag for grid writing by proc 0
  !
  INTEGER                       :: itap_sec ! Time used in prism_put/get_proto
  INTEGER                       :: mod_sec  ! Model time
  !
  ! Grid parameters definition
  INTEGER                       :: part_id  ! use to connect the partition to the variables
                                            ! in prism_def_var_proto
  INTEGER                       :: var_actual_shape(4) ! local dimensions of the arrays to the pe
                                                       ! 2 x field rank (= 4 because fields are of rank = 2)
  !
  !
  ! Exchanged local fields arrays
  ! used in routines prism_put_proto and prism_get_proto
  REAL (kind=wp),   POINTER     :: field(:,:)
  REAL (kind=wp) :: fmin,fmax,fsum
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !   INITIALISATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! read restart file for timing information
  inquire(file=trim(timefile),exist=exists)
  if (exists) then
     open(21,file=trim(timefile))
     read(21,*) start_t
     read(21,*) il_nb_time_steps
     read(21,*) delta_t
     close(21)
  endif

  !!!!!!!!!!!!!!!!! PRISM_INIT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL prism_init_comp_proto (comp_id, comp_name, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_init_comp_proto', 'Pb in model1')
  !
  ! Unit for output messages : one file for each process
  CALL MPI_Comm_Rank ( MPI_COMM_WORLD, rank, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'MPI_Comm_Rank', 'Pb in model1')
  !
  w_unit = 100 + rank
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
  !!!!!!!!!!!!!!!!! PRISM_GET_LOCALCOMM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL prism_get_localcomm_proto ( localComm, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id,'prism_get_localcomm_proto','Pb in model1')
  !
  ! Get MPI size and rank
  CALL MPI_Comm_Size ( localComm, npes, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'MPI_Comm_Size','Pb in model1')
  !
  CALL MPI_Comm_Rank ( localComm, mype, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'MPI_Comm_Rank','Pb in model1')
  !
  WRITE(w_unit,*) 'I am the', TRIM(comp_name), ' ', 'comp', comp_id, 'local rank', mype
  CALL flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  GRID DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Reading global grid netcdf file
  !
  ! Reading dimensions of the global grid
  CALL read_dim_irreg(nlon,nlat,nbr_corners_ij,data_filename,w_unit)
  !
  ! Allocation
  ALLOCATE(globalgrid_lon(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_lon'
  ALLOCATE(globalgrid_lat(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_lat'
  ALLOCATE(globalgrid_clo(nlon,nlat,nbr_corners_ij), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_clo'
  ALLOCATE(globalgrid_cla(nlon,nlat,nbr_corners_ij), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_cla'
  ALLOCATE(indice_mask(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating indice_mask'
  ALLOCATE(globalgrid_area(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_area'
  !
  ! Reading of the longitudes, latitudes, longitude and latitudes of the corners, mask of the global grid
  CALL read_grid_irreg(nlon,nlat,nbr_corners_ij,data_filename, w_unit, &
                                 globalgrid_lon,globalgrid_lat, &
                                 globalgrid_clo,globalgrid_cla, &
                                 indice_mask)
  call flush(w_unit)

  do n = 1,nlat
  do m = 1,nlon
     mm1 = max(m-1,1)
     mp1 = min(m+1,nlon)
     nm1 = max(n-1,1)
     np1 = min(n+1,nlat)
!     dlon = abs((globalgrid_lon(mp1,n) - globalgrid_lon(mm1,n)) / float(mp1-mm1))
!     !--- correct for wraparound points, users guide say valid range is -360 to 720
!     !--- so need to check for 360 diff up to 3x
!     !--- this assumes there are never grids with sizes greater than 180 degrees
!     !--- assumes lon/lat in degrees, compute area in m2
!     if (dlon > 180.) dlon = abs(dlon - 360.0)
!     if (dlon > 180.) dlon = abs(dlon - 360.0)
!     if (dlon > 180.) dlon = abs(dlon - 360.0)
!     !---
!     dlat = abs((globalgrid_lat(m,np1) - globalgrid_lat(m,nm1)) / float(np1-nm1))
!     globalgrid_area(m,n) = abs(dlon*dlat*(pi/180.)*(pi/180.)*cos(globalgrid_lat(m,n)*pi/180.)*eradius*eradius)
     ! great circle area estimate
     cla1 = globalgrid_cla(m,n,1)*pi/180.
     cla2 = globalgrid_cla(m,n,2)*pi/180.
     cla3 = globalgrid_cla(m,n,3)*pi/180.
     cla4 = globalgrid_cla(m,n,4)*pi/180.
     clo1 = globalgrid_clo(m,n,1)*pi/180.
     clo2 = globalgrid_clo(m,n,2)*pi/180.
     clo3 = globalgrid_clo(m,n,3)*pi/180.
     clo4 = globalgrid_clo(m,n,4)*pi/180.
     ! great circle approximation with many assumptions build-in
     !   4 corner points exactly, orthogonal parallel grid lines, etc
     dlon = 0.5 *(abs(acos(sin(cla1)*sin(cla2)+cos(cla1)*cos(cla2)*cos(clo2-clo1))) + &
                  abs(acos(sin(cla3)*sin(cla4)+cos(cla3)*cos(cla4)*cos(clo4-clo3))))
     dlat = 0.5 *(abs(acos(sin(cla2)*sin(cla3)+cos(cla2)*cos(cla3)*cos(clo3-clo2))) + &
                  abs(acos(sin(cla4)*sin(cla1)+cos(cla4)*cos(cla1)*cos(clo1-clo4))))
     globalgrid_area(m,n) = abs(dlon*dlat*eradius*eradius)
  enddo
  enddo

  !
  ! (Global) grid definition for OASIS3
  ! Writing of the file grids.nc and masks.nc by the processor 0 from the grid read in 
  IF (mype == 0) THEN
      !
      ! Half of the line j=148 is masked 
      DO i=92,182
        indice_mask(i,148)=1
      ENDDO
      !
      CALL prism_start_grids_writing(il_flag)
      CALL prism_write_grid('torc', nlon, nlat, globalgrid_lon, globalgrid_lat)
      CALL prism_write_corner('torc', nlon, nlat, 4, globalgrid_clo, globalgrid_cla)
      CALL prism_write_mask('torc', nlon, nlat, indice_mask(:,:))
      CALL prism_write_area('torc', nlon, nlat, globalgrid_area)
      CALL prism_terminate_grids_writing()
  ENDIF
  WRITE(w_unit,*) 'After grids writing'
  call flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  PARTITION DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !
  ! Definition of the partition of the grid (calling prism_def_partition_proto)
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
  CALL prism_def_partition_proto (part_id, il_paral, ierror)
  WRITE(w_unit,*) 'After prism_def_partition_proto = ', il_paral(:)
  call flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! DEFINITION OF THE LOCAL FIELDS  
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!! !!!!!!!!! PRISM_DEF_VAR !!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !  Define transient variables
  !
  var_nodims(1) = 2    ! Rank of the field array is 2
  var_nodims(2) = 1    ! Bundles always 1 for OASIS3
  var_type = PRISM_Real
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

  do n = 1,nvar
     if (var_out(n)) then
        CALL prism_def_var_proto (var_id(n),var_name(n), part_id, &
           var_nodims, PRISM_Out, var_actual_shape, var_type, ierror)
     else
        CALL prism_def_var_proto (var_id(n),var_name(n), part_id, &
           var_nodims, PRISM_In, var_actual_shape, var_type, ierror)
     endif
     IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_def_var_proto', 'Pb in model1')
  enddo

  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION OF DEFINITION PHASE 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  All processes involved in the coupling must call prism_enddef_proto; 
  !  here all processes are involved in coupling
  !
  !!!!!!!!!!!!!!!!!! PRISM_ENDDEF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL prism_enddef_proto ( ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_enddef_proto', 'Pb in model1')
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! SEND AND RECEIVE ARRAYS 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocate the fields send and received by the model
  !

  ALLOCATE(field(var_actual_shape(2), var_actual_shape(4)), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field'

  ALLOCATE ( localgrid_lon(var_actual_shape(2), var_actual_shape(4)), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_lon'
  !
  ALLOCATE ( localgrid_lat(var_actual_shape(2), var_actual_shape(4)), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_lat'

  ! Calculate the local grid to the process for OASIS3

  CALL oasis3_local_grid(mype, npes, nlon, nlat, var_actual_shape, &
                         localgrid_lon, localgrid_lat,             &
                         globalgrid_lon, globalgrid_lat, w_unit)
  !
  DEALLOCATE(il_paral)
  !
  !!!!!!!!!!!!!!!!!!!!!!!!PRISM_PUT/PRISM_GET !!!!!!!!!!!!!!!!!!!!!! 
  !
  ! Data exchange 
  ! 
  ! Time loop
  DO ib=1, il_nb_time_steps
    itap_sec =           delta_t * (ib-1) ! Coupling Time
    mod_sec  = start_t + delta_t * (ib-1) ! Model Time
    write(w_unit,*) 'advance time ',ib,itap_sec,mod_sec
    !
!    CALL function_sent(var_actual_shape(2), &
!                       var_actual_shape(4), &
!                       localgrid_lon,localgrid_lat, &
!                       field,ib)

     do n = 1,nvar
        if (var_out(n)) then
           DO j=1,var_actual_shape(4)
           DO i=1,var_actual_shape(2)
              field(i,j) =   100 + (n-1)*10 + float(mod_sec)/float(delta_t) + &
                             (cos(float(i)/10.)*sin(float(j)/10.))
           ENDDO
           ENDDO
           call flddiag(field,fmin,fmax,fsum,localcomm,var_actual_shape(2),var_actual_shape(4))
           if (mype == 0) write(w_unit,F01) 'tcx send ',n,mod_sec,fmin,fmax,fsum
           CALL prism_put_proto(var_id(n),itap_sec, field, ierror)
           IF ( ierror .NE. PRISM_Ok .AND. ierror .LT. PRISM_Sent) &
              CALL prism_abort_proto(comp_id, 'prism_put_proto', 'Pb in model1')
        else
           field=field_ini
           CALL prism_get_proto(var_id(n),itap_sec, field, ierror)
           call flddiag(field,fmin,fmax,fsum,localcomm,var_actual_shape(2),var_actual_shape(4))
           if (mype == 0) write(w_unit,F01) 'tcx recv ',n,mod_sec,fmin,fmax,fsum
           IF ( ierror .NE. PRISM_Ok .AND. ierror .LT. PRISM_Recvd) &
              CALL prism_abort_proto(comp_id, 'prism_get_proto', 'Pb in model1')
        endif
     enddo

  ENDDO
  !
  ! write restart
  mod_sec  = start_t + delta_t * (il_nb_time_steps) ! Model Time
  if (mype == 0) then
     write(w_unit,*) 'write timefile ',trim(timefile),mod_sec,il_nb_time_steps,delta_t
     open(21,file=trim(timefile))
     write(21,*) mod_sec
     write(21,*) il_nb_time_steps
     write(21,*) delta_t
     close(21)
  endif
  !
  CLOSE (w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!!! PRISM_ENDDEF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Collective call to terminate the coupling exchanges
  !
  CALL prism_terminate_proto (ierror)
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_terminate_proto', 'Pb in model1')
  !
END PROGRAM MODEL1
!
