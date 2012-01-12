!------------------------------------------------------------------------
! Copyright 2010, CERFACS, Toulouse, France.
! All rights reserved. Use is subject to OASIS3 license terms.
!============================================================================
!
PROGRAM model2
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
  CHARACTER(len=30), PARAMETER :: data_filename='rmp_A400_to_Ot25_GAUSWGT.nc'
  ! Component name (6 characters) same as in the namcouple
  CHARACTER(len=6)   :: comp_name = 'toyatm'
  CHARACTER(len=128) :: comp_out ! name of the output log file
  CHARACTER(len=3)   :: chout
  !
  INTEGER :: ib_b, ib_pe               ! General loop index
  !
  ! Global grid parameters : 
  INTEGER :: npt_hor    ! dimension of the global 2D grid
  INTEGER :: il_nptloc  ! dimension of the local 2D grid
  INTEGER :: il_fileid, il_lonid, il_varid
  !
  INTEGER, DIMENSION(:), POINTER :: localgrid_mask ! mask, 0 == valid point, 1 == masked point 
  !
  INTEGER :: mype, npes ! rank and number of pe
  INTEGER :: localComm  ! local MPI communicator and Initialized
  INTEGER :: comp_id    ! component identification
  INTEGER :: il_offset
  !
#ifdef DECOMP_ORANGE
  INTEGER :: il_c, il_len
  INTEGER, PARAMETER :: ip_nseg = 10
  INTEGER, DIMENSION(2+ip_nseg*2) :: il_paral ! Decomposition for each proc
#elif defined DECOMP_APPLE
  INTEGER, DIMENSION(3) :: il_paral ! Decomposition for each proc
#endif
  !
  INTEGER :: ierror, rank, w_unit
  !
  ! Names of exchanged Fields
  CHARACTER(len=8), PARAMETER :: var_name1 = 'FRECVATM' ! 8 characters field received by the atmosphere from the ocean
  CHARACTER(len=8), PARAMETER :: var_name2 = 'FSENDATM' ! 8 characters field sent by the atmosphere to the ocean
  !
  ! Used in prism_def_var and prism_def_var_proto
  INTEGER                       :: var_id(2) 
  INTEGER                       :: var_nodims(2) 
  INTEGER                       :: var_type
  !
  REAL (kind=wp), PARAMETER     :: field_ini = -1. ! initialisation of received fields
  !
  INTEGER, PARAMETER    ::  il_nb_time_steps = 240 ! 2 or 240 number of time steps
  INTEGER, PARAMETER    ::  delta_t = 3600       ! time step
  !
  ! Centers arrays of the local grid used to calculate the field sent by the model
  REAL (kind=wp), ALLOCATABLE :: localgrid_lon (:), localgrid_lat (:)
  INTEGER, ALLOCATABLE        :: localgrid_msk (:)
  !
  INTEGER                       :: itap_sec ! Time used in prism_put/get_proto
  !
  ! Grid parameter definition
  INTEGER                       :: part_id  ! use to connect the partition to the variables
                                            ! in prism_def_var_proto
  INTEGER                       :: il_part
  INTEGER                       :: var_actual_shape(2) ! local dimensions of the arrays to the pe
                                                       ! 2 x field rank        
  !
  ! Exchanged local fields arrays
  ! used in routines prism_put_proto and prism_get_proto
  REAL (kind=wp), ALLOCATABLE       :: field1_recv(:)
  REAL (kind=wp), ALLOCATABLE       :: field2_send(:)
  !
  INTEGER  :: idim
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !   INITIALISATION
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!! PRISM_INIT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL prism_init_comp_proto (comp_id, comp_name, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_init_comp_proto', 'Pb in model2')
  !
#ifdef TMG_PERF
  call prism_timer_start('In mod, initialization')
#endif
  !
  ! Unit for output messages : one file for each process
  CALL MPI_Comm_Rank ( MPI_COMM_WORLD, rank, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'MPI_Comm_Rank','Pb in model2')
  !
  !
#ifdef _DEBUG
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
#endif
  !
  !!!!!!!!!!!!!!!!! PRISM_GET_LOCALCOMM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL prism_get_localcomm_proto ( localComm, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id,'prism_get_localcomm_proto','Pb in model2')
  !
  ! Get MPI size and rank
  CALL MPI_Comm_Size ( localComm, npes, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'MPI_Comm_Size','Pb in model2')
  !
  CALL MPI_Comm_Rank ( localComm, mype, ierror )
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'MPI_Comm_Rank','Pb in model2')
  !
#ifdef _DEBUG
  WRITE(w_unit,*) 'I am the', TRIM(comp_name), ' ',' comp', comp_id, ' local rank ', mype
  CALL flush(w_unit)
#endif
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  GRID DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Reading local part of the grid in the global grid netcdf file
  !
  ! Reading global dimensions of the grid
  CALL read_dim_gauss(data_filename,npt_hor)
#ifdef _DEBUG
  WRITE(w_unit,*) 'After read_dim_gauss'
  CALL flush(w_unit)
#endif
  !
  ! Defining local number of points
  il_nptloc = npt_hor/npes
  il_offset = mype*il_nptloc
  !
  IF (mype == (npes-1)) THEN
      il_nptloc = npt_hor-(mype*il_nptloc)
  ENDIF
  !
  ! Allocation for local grid
  ALLOCATE(localgrid_lon(il_nptloc), STAT=ierror )
#ifdef _DEBUG
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_lon'
#endif
  ALLOCATE(localgrid_lat(il_nptloc), STAT=ierror )
#ifdef _DEBUG
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_lat'
#endif
  ALLOCATE(localgrid_msk(il_nptloc), STAT=ierror )
#ifdef _DEBUG
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating localgrid_msk'
#endif
#ifdef _DEBUG
  WRITE(w_unit,*) 'After allocation of local grid arrays'
  CALL flush(w_unit)
#endif
  !
  ! Reading of the longitudes, latitudes, mask of the grid
  CALL read_grid_gauss(data_filename,il_nptloc,il_offset,   &
                       localgrid_lon,localgrid_lat,localgrid_msk)
#ifdef _DEBUG
  WRITE(w_unit,*) 'After read_grid_gauss'
  CALL flush(w_unit)
#endif
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  PARTITION DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !
  ! Definition of the partition of the grid (calling prism_def_partition_proto)
  !
#ifdef DECOMP_ORANGE
  il_paral (1) = 3
  il_paral (2) = ip_nseg
  il_len = il_nptloc/ip_nseg
  il_c = 0
  do ib_b = 3,ip_nseg*2+1,2
	il_paral (ib_b) = mype*(npt_hor/npes) + il_c*il_len
        il_c = il_c+1
  enddo
  il_paral (4:ip_nseg*2:2) = il_len 
  il_paral (2+ip_nseg*2) = il_nptloc - (il_len*(ip_nseg-1))
  !
#elif defined DECOMP_APPLE
  il_paral(1) = 1
  il_paral(2) = il_offset
  il_paral(3) = il_nptloc
  !
#endif
  !
#ifdef _DEBUG
  WRITE(w_unit,*) 'After decomp_def,il_nptloc, il_len = ', il_nptloc, il_len 
  WRITE(w_unit,*) 'After decomp_def, il_paral = ',il_paral(:)
  call flush(w_unit)
#endif
  CALL prism_def_partition_proto (part_id, il_paral, ierror)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! DEFINITION OF THE LOCAL FIELDS  
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!!! PRISM_DEF_VAR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Define transient variables
  !
  var_nodims(1) = 1    ! Rank of the field array is 1
  var_nodims(2) = 1    ! Bundles always 1 for OASIS3
  var_type = PRISM_Real
  !
  var_actual_shape(1) = 1
  var_actual_shape(2) = il_nptloc
  !
  ! Declaration of the field associated with the partition of the grid
  CALL prism_def_var_proto (var_id(1),var_name1, part_id, &
     var_nodims, PRISM_In, var_actual_shape, var_type, ierror)
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_def_var_proto','Pb in model2')
  !
  CALL prism_def_var_proto (var_id(2),var_name2, part_id, &
     var_nodims, PRISM_Out, var_actual_shape, var_type, ierror)
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_def_var_proto','Pb in model2')
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
  IF (ierror /= 0) CALL prism_abort_proto(comp_id, 'prism_enddef_proto','Pb in model2')
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
  !
  ALLOCATE(field1_recv(idim), STAT=ierror )
#ifdef _DEBUG
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field1_recv'
#endif
  !
  ALLOCATE(field2_send(idim),STAT=ierror )
#ifdef _DEBUG
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field2_send'
#endif
  !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!PRISM_PUT/PRISM_GET !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Data exchange
  !
  ! Time loop
  DO ib_b=1, il_nb_time_steps
    itap_sec = delta_t * (ib_b-1) ! Time
    !
    ! Get the field FRECVATM
    field1_recv=field_ini
    !
#ifdef TMG_PERF   
    call mpi_barrier(MPI_COMM_WORLD, ierror)
    call prism_timer_start('Ping-pong exchange')
#endif 
    !    
    CALL prism_get_proto(var_id(1),itap_sec, field1_recv, ierror)
    IF ( ierror .NE. PRISM_Ok .AND. ierror .LT. PRISM_Recvd) &
        CALL prism_abort_proto(comp_id, 'prism_get_proto','Pb in model2')
#ifdef _DEBUG
    call prism_timer_start('Write field')   
    IF (ib_b == 1) THEN
         IF (mype == 0) THEN
      	     CALL hdlerr(nf90_create('field1_recv.nc', NF90_CLOBBER, il_fileid), __LINE__ )
             CALL hdlerr(nf90_def_dim(il_fileid, "x", npt_hor, il_lonid), __LINE__ )
      	     CALL hdlerr(nf90_def_var(il_fileid, "field", NF90_DOUBLE, (/il_lonid/), il_varid), __LINE__ )
             CALL hdlerr(nf90_enddef(il_fileid), __LINE__ )
      	     CALL hdlerr(nf90_close(il_fileid), __LINE__ )
  	 ENDIF
  !
         DO ib_pe = 0, npes - 1
    	    CALL MPI_Barrier(localComm, ierror)
    	    IF (mype == ib_pe) THEN
               CALL hdlerr( nf90_open('field1_recv.nc', NF90_WRITE, il_fileid), __LINE__ )
               CALL hdlerr( nf90_inq_varid(il_fileid, "field" , il_varid), __LINE__ )
               CALL hdlerr( nf90_put_var(il_fileid, il_varid, field1_recv, (/il_offset+1/), (/il_nptloc/)), __LINE__ )
               CALL hdlerr( nf90_close(il_fileid), __LINE__ )
   	    ENDIF
         ENDDO
    ENDIF
    call prism_timer_stop('Write field')
#endif 
    !
    ! Send FSENDATM
    field2_send(:) =  ib_b*(localgrid_lon(:) + localgrid_lat(:))
    !
    CALL prism_put_proto(var_id(2),itap_sec, field2_send, ierror)
    IF ( ierror .NE. PRISM_Ok .AND. ierror .LT. PRISM_Sent) &
       CALL prism_abort_proto(comp_id, 'prism_put_proto','Pb in model2')	
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
  CLOSE(w_unit)
#endif
  ! 
END PROGRAM MODEL2
!
