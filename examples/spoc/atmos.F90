PROGRAM atmos
  !
  ! Use for netCDF library
  USE netcdf
  !
  USE def_parallel_decomposition
  !
  IMPLICIT NONE
  !
  INCLUDE 'mpif.h'   ! Include for MPI
  !
  INTEGER :: mype, npes ! rank and number of pe
  INTEGER :: localComm  ! local communicator for atmos processes
  CHARACTER(len=128) :: comp_out ! name of the output log file 
  CHARACTER(len=3)   :: chout
  INTEGER :: ierror, w_unit
  !
  ! Global grid parameters
  INTEGER, PARAMETER :: nlon = 96, nlat = 72    ! dimensions in the 2 spatial directions
  INTEGER, PARAMETER :: nc = 4 ! number of grid cell corners in the (i,j) plan
  !
  ! Local grid dimensions and arrays
  INTEGER :: il_extentx, il_extenty, il_offsetx, il_offsety
  INTEGER :: il_size, il_offset
  DOUBLE PRECISION, DIMENSION(:,:),   POINTER   :: grid_lon, grid_lat ! lon, lat of the cell centers
  DOUBLE PRECISION, DIMENSION(:,:,:), POINTER   :: grid_clo, grid_cla ! lon, lat of the cell corners
  DOUBLE PRECISION, DIMENSION(:,:),   POINTER   :: grid_srf ! surface of the grid meshes
  INTEGER, DIMENSION(:,:),            POINTER   :: grid_msk ! mask, 0 == valid point, 1 == masked point
  !
  ! For time step loop
  INTEGER               ::  ib
  INTEGER, PARAMETER    ::  il_nb_time_steps = 8 ! number of time steps
  INTEGER, PARAMETER    ::  delta_t = 1800       ! time step
  INTEGER               ::  itap_sec ! time in seconds
  DOUBLE PRECISION, PARAMETER    :: dp_pi=3.14159265359
  DOUBLE PRECISION, PARAMETER    :: dp_length= 1.2*dp_pi
  !
  ! Local coupling fields arrays exchanged via oasis_get and oasis_put
  DOUBLE PRECISION, POINTER :: field_recv(:,:)
  DOUBLE PRECISION, POINTER :: field_send(:,:)
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  INITIALISATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  call MPI_Init(ierror)
  !
  localComm =  MPI_COMM_WORLD
  !
  !!!!!!!!!!!!!!!!! OASIS_INIT_COMP !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !!!!!!!!!!!!!!!!! OASIS_GET_LOCALCOMM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  ! Get rank in local communicator
  CALL MPI_Comm_Size ( localComm, npes, ierror )
  CALL MPI_Comm_Rank ( localComm, mype, ierror )
  !  
  ! Unit for output messages : one file for each process
  w_unit = 100 + mype
  WRITE(chout,'(I3)') w_unit
  comp_out='atmos.out_'//chout
  !
  OPEN(w_unit,file=TRIM(comp_out),form='formatted')
  WRITE (w_unit,*) '-----------------------------------------------------------'
  WRITE (w_unit,*) 'I am atmos process with rank :', mype
  WRITE (w_unit,*) 'in my local communicator gathering ', npes, 'processes'
  WRITE (w_unit,*) '----------------------------------------------------------'
  CALL flush(w_unit)
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  PARTITION DEFINITION
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ !
  !
  ! Definition of the local partition
  call def_local_partition(nlon, nlat, npes, mype, &
  	     		 il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset)
  WRITE(w_unit,*) 'Local partition definition'
  WRITE(w_unit,*) 'il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset = ', &
                   il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset
  !
  !!!!!!!!!!!!!!!!! OASIS_DEF_PARTITION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  GRID DEFINITION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocation of local grid arrays
  ALLOCATE(grid_lon(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(grid_lat(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(grid_clo(il_extentx, il_extenty, nc), STAT=ierror )
  ALLOCATE(grid_cla(il_extentx, il_extenty, nc), STAT=ierror )
  ALLOCATE(grid_srf(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(grid_msk(il_extentx, il_extenty), STAT=ierror )
  !
  ! Reading local grid arrays from input file atmos_mesh.nc
  CALL read_grid(nlon, nlat, nc, il_offsetx+1, il_offsety+1, il_extentx, il_extenty, &
                'atmos_mesh.nc', w_unit, &
                 grid_lon, grid_lat, grid_clo, grid_cla, grid_srf, grid_msk)
  !
  !!!!!!!!!!!!!!!!! OASIS_WRITE_GRID  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  DEFINITION OF THE LOCAL FIELDS  
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocate local coupling fields
  ALLOCATE(field_send(il_extentx, il_extenty), STAT=ierror )
  ALLOCATE(field_recv(il_extentx, il_extenty), STAT=ierror )
  !
  !!!!!!!!!!!!!!!!!! OASIS_DEF_VAR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION OF DEFINITION PHASE 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  WRITE(w_unit,*) 'End of initialisation phase'
  call flush(w_unit)
  !
  !!!!!!!!!!!!!!!!!! OASIS_ENDDEF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  TIME STEP LOOP
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  write(w_unit,*) 'Timestep, field min and max value'
  call flush(w_unit)
  DO ib = 1,il_nb_time_steps
    !
    itap_sec = delta_t * (ib-1) ! time in seconds
    field_recv=-1.0
    !
    !!!!!!!!!!!!!!!!!!!!!!!! OASIS_GET !!!!!!!!!!!!!!!!!!!!!!
    ! 
    ! Definition of field produced by the component
    field_send(:,:) =  ib*(2.-COS(dp_pi*(ACOS(COS(grid_lat(:,:)*dp_pi/180.)* &
                           COS(grid_lon(:,:)*dp_pi/180.))/dp_length)))
    write(w_unit,*) itap_sec,minval(field_send),maxval(field_send)
    !
    !!!!!!!!!!!!!!!!!!!!!!!! OASIS_PUT !!!!!!!!!!!!!!!!!!!!!! 
    !
  ENDDO
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!!! OASIS_TERMINATE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  WRITE (w_unit,*) 'End of the program'
  CALL flush(w_unit)
  CLOSE(w_unit)
  !
  CALL MPI_Finalize(ierror)
  !
END PROGRAM atmos
!
