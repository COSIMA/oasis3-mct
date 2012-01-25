!!------------------------------------------------------------------------
! Copyright 2011, CERFACS, Toulouse, France.
! All rights reserved. Use is subject to OASIS3 license terms.
!============================================================================
!
PROGRAM model1
  !
!!!#define _DEBUG
  ! Use for netCDF library
  USE netcdf
  !
  ! Use for OASIS communication library
  USE mod_prism
#ifdef TMG_PERF
  USE mod_prism_timer
#endif
  !
  IMPLICIT NONE
  !
#ifndef TMG_PERF
  INCLUDE 'mpif.h'
#endif
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
  CHARACTER(len=6)   :: comp_name = 'toyocn'
  CHARACTER(len=128) :: comp_out ! name of the output log file 
  CHARACTER(len=3)   :: chout
  !
  integer :: ib_b, ib_pe               ! General loop index
  !
  ! Grid parameters : 
  INTEGER :: nlon, nlat         ! global dimensions in the 2 directions of space
  INTEGER :: il_lon, il_lat     ! local dimensions in the 2 directions of space
  INTEGER :: il_npetot, il_mypei, il_mypej, il_begi, il_endi, il_begj, il_endj 
  INTEGER :: ip_npei
  INTEGER :: ip_npej
  INTEGER :: n,ipi,ipj,iprat,iprat0
  INTEGER :: il_fileid, il_lonid, il_latid, il_varid
  INTEGER, DIMENSION(2)          :: ila_dim, ila_st
  !
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: localgrid_mask ! mask, 0 == valid point, 1 == masked point 
  !
  INTEGER :: mype, npes ! rank and  number of pe
  INTEGER :: localComm  ! local MPI communicator and Initialized
  INTEGER :: comp_id    ! component identification
  !
  INTEGER, DIMENSION(5) :: il_paral ! Decomposition for each proc
  !
  INTEGER :: ierror, rank, w_unit
  !
  ! Names of exchanged Fields
  CHARACTER(len=8), PARAMETER      :: var_name1 = 'FSENDOCN' ! 8 characters field sent by model1 to model2
  CHARACTER(len=8), PARAMETER      :: var_name2 = 'FRECVOCN' ! 8 characters field received by model1 from model2
  !
  ! Used in prism_def_var and prism_def_var_proto
  INTEGER                       :: var_id(2) 
  INTEGER                       :: var_nodims(2) 
  INTEGER                       :: var_type
  !
  REAL (kind=wp), PARAMETER     :: field_ini = -1. ! initialisation of received fields
  !
  INTEGER               ::  il_c
  INTEGER, PARAMETER    ::  il_nb_time_steps = 240 ! 2 or 240 number of time steps
  INTEGER, PARAMETER    ::  delta_t = 3600       ! time step
  !
  ! Centers arrays of the local grid
  REAL (kind=wp), ALLOCATABLE :: localgrid_lon (:,:)
  REAL (kind=wp), ALLOCATABLE :: localgrid_lat (:,:)
  !
  INTEGER                 :: itap_sec ! Time used in prism_put/get_proto
  !
  ! Grid parameters definition
  INTEGER                 :: part_id  ! use to connect the partition to the variables
                                      ! in prism_def_var_proto
  INTEGER                 :: var_actual_shape(4) ! local dimensions of the arrays to the pe
                                                 ! 2 x field rank
  !
  ! Exchanged local fields arrays
  ! used in routines prism_put_proto and prism_get_proto
  REAL (kind=wp),   ALLOCATABLE     :: field1_send(:,:)
  REAL (kind=wp),   ALLOCATABLE     :: field2_recv(:,:)
  !
  INTEGER  :: idim, jdim, ib_i, ib_j
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !   INITIALISATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!! PRISM_INIT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL prism_init_comp_proto (comp_id, comp_name, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_init_comp_proto', 'Pb in model1')
  !
#ifdef TMG_PERF
  call prism_timer_start('In mod, initialization')
#endif
  !	
  ! Unit for output messages : one file for each process
  CALL MPI_Comm_Rank ( MPI_COMM_WORLD, rank, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'MPI_Comm_Rank', 'Pb in model1')
  !
#ifdef _DEBUG
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
#endif
  !
  !!!!!!!!!!!!!!!!! PRISM_GET_LOCALCOM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL prism_get_localcomm_proto ( localComm, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id,'prism_get_localcomm_proto','Pb in model1')
  !
  ! Get MPI size and rank
  CALL MPI_Comm_Size ( localComm, npes, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'MPI_Comm_Size','Pb in model1')
  !
  ip_npei = 0
  ip_npej = 0
  iprat0 = npes + 1
  do n = 1,npes
     ipi = n
     ipj = npes/n
     il_npetot = ipi * ipj
     if (il_npetot == npes) then
        iprat = max(ipi/ipj,ipj/ipi)
        if ((iprat-1) < (iprat0-1)) then
           ip_npei = ipi
           ip_npej = ipj
           iprat0 = iprat
        endif
     endif
  enddo
  il_npetot = ip_npei * ip_npej
  if (il_npetot == 0) then
     CALL prism_abort_proto(comp_id, 'decomp not found', 'Pb in model1')
  endif

#ifdef _DEBUG
  IF (il_npetot /= npes) THEN
      WRITE(w_unit,*) 'The total number of processes assigned :', npes, &
         'is not equal to the number of processes needed for the decompostion',&
         il_npetot
      CALL prism_abort_proto(comp_id, 'Wrong total number of processes', 'Pb in model1')
  ENDIF
#endif
  !
  CALL MPI_Comm_Rank ( localComm, mype, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'MPI_Comm_Rank','Pb in model1')
  !
#ifdef _DEBUG
  WRITE(w_unit,*) 'I am the', TRIM(comp_name), ' ', 'comp', comp_id, 'local rank', mype
  CALL flush(w_unit)
#endif
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  GRID DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Reading local part of the grid in the global grid netcdf file
  !
  ! Reading dimensions of the global grid
  CALL read_dim_irreg(nlon,nlat,data_filename)
#ifdef _DEBUG
  WRITE(w_unit,*) 'Read input file ',data_filename
  WRITE(w_unit,*) 'Global dimensions nlon=',nlon,' nlat=',nlat
  CALL flush(w_unit)
#endif
  !
  ! Calculating the local dimensions and local index in i and j (BOX decomposition)
  il_mypei = MOD(mype,ip_npei)
  il_begi = ((nlon/ip_npei)*il_mypei)+1
  IF (il_mypei .LT. ip_npei - 1) THEN
      il_endi = (nlon/ip_npei)*(il_mypei+1)
  ELSE
      il_endi = nlon 
  ENDIF
  !
  il_mypej = mype/ip_npei
  il_begj = ((nlat/ip_npej)*il_mypej)+1
  IF (il_mypej .LT. ip_npej - 1) THEN
      il_endj = (nlat/ip_npej)*(il_mypej+1)
  ELSE
      il_endj = nlat 
  ENDIF
  il_lon = il_endi - il_begi + 1
  il_lat = il_endj - il_begj + 1
  !
#ifdef _DEBUG
  WRITE(w_unit,101) il_mypei, il_begi, il_endi, il_lon
  WRITE(w_unit,102) il_mypej, il_begj, il_endj, il_lat
  call flush(w_unit)
101 FORMAT('il_mypei, il_begi, il_endi, il_lon = ', 4I6)
102 FORMAT('il_mypej, il_begj, il_endj, il_lat = ', 4I6)
#endif
  !
  ! Allocation
  ALLOCATE(localgrid_lon(il_lon,il_lat), STAT=ierror )
#ifdef _DEBUG
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_lon'
#endif
  ALLOCATE(localgrid_lat(il_lon,il_lat), STAT=ierror )
#ifdef _DEBUG
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_lat'
#endif
  ALLOCATE(localgrid_mask(il_lon,il_lat), STAT=ierror )
#ifdef _DEBUG
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_mask'
#endif
#ifdef _DEBUG
  WRITE(w_unit,*) 'After allocation of local grid arrays'
  call flush(w_unit)
#endif
  !
  ! Reading of the longitudes, latitudes, mask of the local grid
  CALL read_grid_irreg(nlon, nlat, il_begi, il_begj, il_lon, il_lat, &
                       data_filename, w_unit,                   &
                       localgrid_lon, localgrid_lat, localgrid_mask)
#ifdef _DEBUG
  ! Write the lon, lat, mask field
  IF (mype == 0) THEN
      CALL hdlerr(nf90_create('grid_model1_out.nc', NF90_CLOBBER, il_fileid), __LINE__ )
      CALL hdlerr(nf90_def_dim(il_fileid, "x", nlon, il_lonid), __LINE__ )
      CALL hdlerr(nf90_def_dim(il_fileid, "y", nlat, il_latid), __LINE__ )
      CALL hdlerr(nf90_def_var(il_fileid, "lon", NF90_DOUBLE, (/il_lonid,il_latid/), il_varid), __LINE__ )
      CALL hdlerr(nf90_def_var(il_fileid, "lat", NF90_DOUBLE, (/il_lonid,il_latid/), il_varid), __LINE__ )
      CALL hdlerr(nf90_enddef(il_fileid), __LINE__ )
      CALL hdlerr(nf90_close(il_fileid), __LINE__ )
  ENDIF
  ila_st(1) = il_begi
  ila_st(2) = il_begj
  ila_dim(1) = il_lon
  ila_dim(2) = il_lat
  !
  DO ib_b = 0, npes - 1
    CALL MPI_Barrier(localComm, ierror)
    IF (mype == ib_b) THEN
        CALL hdlerr( nf90_open('grid_model1_out.nc', NF90_WRITE, il_fileid), __LINE__ )
        CALL hdlerr( nf90_inq_varid(il_fileid, "lon" , il_varid), __LINE__ )
        CALL hdlerr( nf90_put_var(il_fileid, il_varid, localgrid_lon, ila_st, ila_dim), __LINE__ )
        CALL hdlerr( nf90_inq_varid(il_fileid, "lat" , il_varid), __LINE__ )
        CALL hdlerr( nf90_put_var(il_fileid, il_varid, localgrid_lat, ila_st, ila_dim), __LINE__ )
        CALL hdlerr( nf90_close(il_fileid), __LINE__ )
    ENDIF
  ENDDO
#endif !_DEBUG
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  PARTITION DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !
  ! Definition of the BOX partition of the grid (calling prism_def_partition_proto)
  !
  il_paral(1) = 2
  il_paral(2) = (il_begj-1)*nlon + il_begi - 1
  il_paral(3) = il_lon
  il_paral(4) = il_lat
  il_paral(5) = nlon
  !
  CALL prism_def_partition_proto (part_id, il_paral, ierror)
#ifdef _DEBUG
  WRITE(w_unit,*) 'After prism_def_partition_proto = ', il_paral(:)
  call flush(w_unit)
#endif
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
  var_actual_shape(2) = il_lon
  var_actual_shape(3) = 1 
  var_actual_shape(4) = il_lat
  !
#ifdef _DEBUG
  write(w_unit,*) ' var_actual_shape', var_actual_shape(:)
  call flush(w_unit)
#endif
  ! Declaration of the field associated with the partition
  CALL prism_def_var_proto (var_id(1),var_name1, part_id, &
     var_nodims, PRISM_Out, var_actual_shape, var_type, ierror)
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_def_var_proto', 'Pb in model1')
  !
  CALL prism_def_var_proto (var_id(2),var_name2, part_id, &
     var_nodims, PRISM_In, var_actual_shape, var_type, ierror)
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_def_var_proto', 'Pb in model1')
#ifdef _DEBUG
  write(w_unit,*)'After prism_def_var_proto'
  call flush(w_unit)
#endif
  !
#ifdef TMG_PERF
  call prism_timer_stop('In mod, initialization')
#endif
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
#ifdef _DEBUG
  write(w_unit,*)'After prism_enddef_proto'
  call flush(w_unit)
#endif
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! SEND AND RECEIVE ARRAYS 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocate the fields send and received by the model
  !
  idim=var_actual_shape(2)
  jdim=var_actual_shape(4)
  !
  ALLOCATE(field1_send(idim, jdim), STAT=ierror )
#ifdef _DEBUG
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field1_send'
#endif
  !
  ALLOCATE(field2_recv(idim, jdim), STAT=ierror )
#ifdef _DEBUG
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field2_recv'
#endif
  !
  !!!!!!!!!!!!!!!!!!!!!!!!PRISM_PUT/PRISM_GET !!!!!!!!!!!!!!!!!!!!!! 
  !
  ! Data exchange 
  ! 
  ! Time loop
  DO ib_b=1, il_nb_time_steps
    itap_sec = delta_t * (ib_b-1) ! Time
    field1_send(:,:) =  ib_b*(localgrid_lon(:,:) + localgrid_lat(:,:)) 
    !
    ! Send FSENDOCN
#ifdef _DEBUG
    write(w_unit,*) 'tcx sendf1 ',itap_sec,minval(field1_send),maxval(field1_send)
    call flush(w_unit)
#endif
    !
#ifdef TMG_PERF
    call mpi_barrier(MPI_COMM_WORLD, ierror)
    call prism_timer_start('Ping-pong exchange')
#endif
    CALL prism_put_proto(var_id(1),itap_sec, field1_send, ierror)
    IF ( ierror .NE. PRISM_Ok .AND. ierror .LT. PRISM_Sent) &
        CALL prism_abort_proto(comp_id, 'prism_put_proto', 'Pb in model1')
    !
    ! call sleep(1)
    !
    ! Get FRECVOCN
    field2_recv=field_ini
    !
    CALL prism_get_proto(var_id(2),itap_sec, field2_recv, ierror)
#ifdef _DEBUG
    call prism_timer_start('Write field')
    IF (ib_b == 1) then
       IF (mype == 0) THEN
       	  CALL hdlerr(nf90_create('field2_recv.nc', NF90_CLOBBER, il_fileid), __LINE__ )
       	  CALL hdlerr(nf90_def_dim(il_fileid, "x", nlon, il_lonid), __LINE__ )
       	  CALL hdlerr(nf90_def_dim(il_fileid, "y", nlat, il_latid), __LINE__ )
       	  CALL hdlerr(nf90_def_var(il_fileid, "field", NF90_DOUBLE, (/il_lonid,il_latid/), il_varid), __LINE__ )
       	  CALL hdlerr(nf90_enddef(il_fileid), __LINE__ )
       	  CALL hdlerr(nf90_close(il_fileid), __LINE__ )
       ENDIF
	  ila_st(1) = il_begi
          ila_st(2) = il_begj
          ila_dim(1) = il_lon
          ila_dim(2) = il_lat
  !
       DO ib_pe = 0, npes - 1
       	  CALL MPI_Barrier(localComm, ierror)
    	  IF (mype == ib_pe) THEN
             CALL hdlerr( nf90_open('field2_recv.nc', NF90_WRITE, il_fileid), __LINE__ )
             CALL hdlerr( nf90_inq_varid(il_fileid, "field" , il_varid), __LINE__ )
             CALL hdlerr( nf90_put_var(il_fileid, il_varid, field2_recv, ila_st, ila_dim), __LINE__ )
             CALL hdlerr( nf90_close(il_fileid), __LINE__ )
    	  ENDIF
       ENDDO
    ENDIF
    call prism_timer_stop('Write field')
#endif
    IF ( ierror .NE. PRISM_Ok .AND. ierror .LT. PRISM_Recvd) &
        CALL prism_abort_proto(comp_id, 'prism_get_proto', 'Pb in model1')		
    !
#ifdef TMG_PERF
    call prism_timer_stop('Ping-pong exchange')
#endif 
  ENDDO
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!!! PRISM_TERMINATE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Collective call to terminate the coupling exchanges
  !
  CALL prism_terminate_proto (ierror)
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_terminate_proto', 'Pb in model1')
  !
#ifdef _DEBUG	
  CLOSE (w_unit)
#endif
  !
END PROGRAM MODEL1
!
