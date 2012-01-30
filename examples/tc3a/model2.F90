!------------------------------------------------------------------------
! Copyright 2010, CERFACS, Toulouse, France.
! All rights reserved. Use is subject to OASIS3 license terms.
!=============================================================================
!
PROGRAM model2

  ! Use for netCDF library
  USE netcdf
  ! Use for OASIS communication library
  USE mod_oasis

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
  CHARACTER(len=30), PARAMETER   :: data_filename='grid_model2.nc'
  ! Component name (6 characters) same as in the namcouple
  CHARACTER(len=6)   :: comp_name = 'model2'
  CHARACTER(len=128) :: comp_out ! name of the output log file 
  CHARACTER(len=3)   :: chout

  ! Global grid parameters : 
  INTEGER :: nlon, nlat     ! dimensions in the 2 directions of space
  INTEGER :: ntot           ! total dimension
  INTEGER :: il_size
  INTEGER :: nbr_corners_ij ! number of corners in the (i,j) plan
  DOUBLE PRECISION, DIMENSION(:), POINTER      :: globalgrid_lon,globalgrid_lat
  DOUBLE PRECISION, DIMENSION(:,:), POINTER    :: globalgrid_clo,globalgrid_cla
  DOUBLE PRECISION, DIMENSION(:,:), POINTER    :: globalgrid_area
  INTEGER, DIMENSION(:,:), POINTER           :: indice_mask ! mask, 0 == valid point, 1 == masked point 

  INTEGER :: mype, npes ! rank and  number of pe
  INTEGER :: localComm  ! local MPI communicator and Initialized
  INTEGER :: comp_id    ! component identification

  INTEGER, DIMENSION(:), ALLOCATABLE :: il_paral ! Decomposition for each proc

  INTEGER :: ierror, rank, w_unit
  INTEGER :: i, j, n, m, mm1, mp1, nm1, np1
  DOUBLE PRECISION :: dlon,dlat
  DOUBLE PRECISION :: cla1,cla2,cla3,cla4,clo1,clo2,clo3,clo4
  DOUBLE PRECISION,parameter :: pi = 3.14159265358979323846
  DOUBLE PRECISION,parameter :: eradius = 6371229.    ! meters

  character(len=*),parameter :: F01 = '(a,i6,i12,3g15.8)'

  ! Used in oasis_def_var and oasis_def_var_proto
  integer, parameter :: mvar = 10
  integer, parameter :: nvar = 5
  character(len=8),parameter :: var_name(mvar) =  &
     (/'M2FLD01 ','M2FLD02 ','M2FLD03 ','M2FLD04 ','M2FLD05 ', &
       'M2FLD06 ','M2FLD07 ','M2FLD08 ','M2FLD09 ','M2FLD10 '/)
  logical, parameter :: var_out(mvar) = &
     (/.true.,.true.,.false.,.false.,.false., &
       .false.,.false.,.false.,.false.,.false./)
  INTEGER                       :: var_id(mvar) 
  INTEGER                       :: var_nodims(2) 
  INTEGER                       :: var_type
  !
  REAL (kind=wp), PARAMETER     :: field_ini = -1. ! initialisation of received fields
  !
  INTEGER               ::  ib
  INTEGER, PARAMETER    ::  il_nb_time_steps = 12 ! number of time steps
  INTEGER, PARAMETER    ::  delta_t = 3600       ! time step
  !
  ! Centers arrays of the local grid
  ! used to calculate the field field1_send sent by the model
  REAL (kind=wp), POINTER :: localgrid_lon (:,:)
  REAL (kind=wp), POINTER :: localgrid_lat (:,:)
  ! Centers and corners of the global grid for writing the
  ! files grids.nc and masks.nc by proc 0
  ! and used to calculate the field field2_send sent by the model
  DOUBLE PRECISION, POINTER     :: lon(:,:),lat(:,:)
  DOUBLE PRECISION, POINTER     :: clo(:,:,:),cla(:,:,:)
  !
  INTEGER                       :: il_flag  ! Flag for grid writing by proc 0
  !
  INTEGER                       :: itap_sec ! Time used in oasis_put/get_proto
  !
  ! Grid parameters definition
  INTEGER                       :: part_id  ! use to connect the partition to the variables
                                            ! in oasis_def_var_proto
  INTEGER                       :: var_actual_shape(4) ! local dimensions of the arrays to the pe
                                                       ! 2 x field rank (= 4 because fields are of rank = 2)
  !
  !
  ! Exchanged local fields arrays
  ! used in routines oasis_put_proto and oasis_get_proto
  REAL (kind=wp),   POINTER     :: field(:,:)
  REAL (kind=wp) :: fmin,fmax,fsum
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !   INITIALISATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!! OASIS_INIT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL oasis_init_comp_proto (comp_id, comp_name, ierror )
  IF (ierror /= 0) CALL oasis_abort_proto(comp_id, 'oasis_init_comp_proto', 'Pb in model2')
  !
  ! Unit for output messages : one file for each process
  CALL MPI_Comm_Rank ( MPI_COMM_WORLD, rank, ierror )
  IF (ierror /= 0) CALL oasis_abort_proto(comp_id, 'MPI_Comm_Rank', 'Pb in model2')
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
  !!!!!!!!!!!!!!!!! OASIS_GET_LOCALCOMM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL oasis_get_localcomm_proto ( localComm, ierror )
  IF (ierror /= 0) CALL oasis_abort_proto(comp_id,'oasis_get_localcomm_proto','Pb in model2')
  !
  ! Get MPI size and rank
  CALL MPI_Comm_Size ( localComm, npes, ierror )
  IF (ierror /= 0) CALL oasis_abort_proto(comp_id, 'MPI_Comm_Size','Pb in model2')
  !
  CALL MPI_Comm_Rank ( localComm, mype, ierror )
  IF (ierror /= 0) CALL oasis_abort_proto(comp_id, 'MPI_Comm_Rank','Pb in model2')
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
  ! Reading dimensions of the grid
  CALL read_dim_reg(nlon,nlat,nbr_corners_ij,data_filename,w_unit)
  !
  ! Allocation
  ALLOCATE(globalgrid_lon(nlon), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating gglon'
  ALLOCATE(globalgrid_lat(nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating gglat'
  ALLOCATE(globalgrid_clo(nlon,nbr_corners_ij), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating ggclo'
  ALLOCATE(globalgrid_cla(nlat,nbr_corners_ij), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating ggcla'
  ALLOCATE(indice_mask(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating indice_mask'
  ALLOCATE(globalgrid_area(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating ggcla'
  ! Reading of the longitudes, latitudes, longitude and latitudes of the corners, mask of the grid
  CALL read_grid_reg(nlon,nlat,nbr_corners_ij,data_filename,w_unit, &
                              globalgrid_lon,globalgrid_lat, &
                              globalgrid_clo,globalgrid_cla, &
                              indice_mask)

  call flush(w_unit)
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
      clo(i,j,1) = globalgrid_clo(i,1)
      clo(i,j,2) = globalgrid_clo(i,2)
      clo(i,j,3) = globalgrid_clo(i,2)
      clo(i,j,4) = globalgrid_clo(i,1)
      cla(i,j,1) = globalgrid_cla(j,1)
      cla(i,j,2) = globalgrid_cla(j,1)
      cla(i,j,3) = globalgrid_cla(j,2)
      cla(i,j,4) = globalgrid_cla(j,2)
    ENDDO
  ENDDO

!  write(w_unit,*) 'tcx gg_clo',minval(globalgrid_clo),maxval(globalgrid_clo)
!  write(w_unit,*) 'tcx gg_cla',minval(globalgrid_cla),maxval(globalgrid_cla)
!  write(w_unit,*) 'tcx01 ',globalgrid_cla(1,1),globalgrid_cla(1,2)
!  write(w_unit,*) 'tcx40 ',globalgrid_cla(40,1),globalgrid_cla(1,2)
!  write(w_unit,*) 'tcx clo',minval(clo),maxval(clo)
!  write(w_unit,*) 'tcx cla',minval(cla),maxval(cla)

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
     cla1 = cla(m,n,1)*pi/180.
     cla2 = cla(m,n,2)*pi/180.
     cla3 = cla(m,n,3)*pi/180.
     cla4 = cla(m,n,4)*pi/180.
     clo1 = clo(m,n,1)*pi/180.
     clo2 = clo(m,n,2)*pi/180.
     clo3 = clo(m,n,3)*pi/180.
     clo4 = clo(m,n,4)*pi/180.
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
      CALL oasis_start_grids_writing(il_flag)
      CALL oasis_write_grid('lmdz', nlon, nlat, lon, lat)
      CALL oasis_write_corner('lmdz', nlon, nlat, 4, clo, cla)
      CALL oasis_write_mask('lmdz', nlon, nlat, indice_mask(:,:))
      CALL oasis_write_area('lmdz', nlon, nlat, globalgrid_area)
      CALL oasis_terminate_grids_writing()
  ENDIF
  WRITE(w_unit,*) 'After grids writing'
  call flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  PARTITION DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !
  ! Definition of the partition of the grid (calling oasis_def_partition_proto)
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
  CALL oasis_def_partition_proto (part_id, il_paral, ierror)
  WRITE(w_unit,*) 'After oasis_def_partition_proto = ', il_paral(:)
  call flush(w_unit)
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

  do n = 1,nvar
     if (var_out(n)) then
        CALL oasis_def_var_proto (var_id(n),var_name(n), part_id, &
           var_nodims, OASIS_Out, var_actual_shape, var_type, ierror)
     else
        CALL oasis_def_var_proto (var_id(n),var_name(n), part_id, &
           var_nodims, OASIS_In, var_actual_shape, var_type, ierror)
     endif
     IF (ierror /= 0) CALL oasis_abort_proto(comp_id, 'oasis_def_var_proto', 'Pb in model2')
  enddo

  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION OF DEFINITION PHASE 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  All processes involved in the coupling must call oasis_enddef_proto; 
  !  here all processes are involved in coupling
  !
  !!!!!!!!!!!!!!!!!! OASIS_ENDDEF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL oasis_enddef_proto ( ierror )
  IF (ierror /= 0) CALL oasis_abort_proto(comp_id, 'oasis_enddef_proto', 'Pb in model2')
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
  !!!!!!!!!!!!!!!!!!!!!!!!OASIS_PUT/OASIS_GET !!!!!!!!!!!!!!!!!!!!!! 
  !
  ! Data exchange 
  ! 
  ! Time loop
  DO ib=1, il_nb_time_steps
    itap_sec = delta_t * (ib-1) ! Time
    !
!    CALL function_sent(var_actual_shape(2), &
!                       var_actual_shape(4), &
!                       localgrid_lon,localgrid_lat, &
!                       field,ib)

     do n = 1,nvar
        if (var_out(n)) then
           DO j=1,var_actual_shape(4)
           DO i=1,var_actual_shape(2)
              field(i,j) =  200 + (n-1)*10 + ib + (cos(float(i)/10.)*sin(float(j)/10.))
           ENDDO
           ENDDO
           call flddiag(field,fmin,fmax,fsum,localcomm,var_actual_shape(2),var_actual_shape(4))
           if (mype == 0) write(w_unit,F01) 'tcx send ',n,itap_sec,fmin,fmax,fsum
           CALL oasis_put_proto(var_id(n),itap_sec, field, ierror)
           IF ( ierror .NE. OASIS_Ok .AND. ierror .LT. OASIS_Sent) &
              CALL oasis_abort_proto(comp_id, 'oasis_put_proto', 'Pb in model2')
        else
           field=field_ini
           CALL oasis_get_proto(var_id(n),itap_sec, field, ierror)
           call flddiag(field,fmin,fmax,fsum,localcomm,var_actual_shape(2),var_actual_shape(4))
           if (mype == 0) write(w_unit,F01) 'tcx recv ',n,itap_sec,fmin,fmax,fsum
           IF ( ierror .NE. OASIS_Ok .AND. ierror .LT. OASIS_Recvd) &
              CALL oasis_abort_proto(comp_id, 'oasis_get_proto', 'Pb in model2')
        endif
     enddo

  ENDDO
  !
  CLOSE (w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!!! OASIS_ENDDEF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Collective call to terminate the coupling exchanges
  !
  CALL oasis_terminate_proto (ierror)
  IF (ierror /= 0) CALL oasis_abort_proto(comp_id, 'oasis_terminate_proto', 'Pb in model2')
  !
END PROGRAM MODEL2
!
