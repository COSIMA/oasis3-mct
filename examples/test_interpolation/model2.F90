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
  REAL (kind=wp), DIMENSION(:,:), POINTER    :: gg_lon,gg_lat
  INTEGER, DIMENSION(:,:), POINTER           :: gg_mask ! mask, 0 == valid point, 1 == masked point 
  !
  INTEGER :: mype, npes ! rank and number of pe
  INTEGER :: localComm  ! local MPI communicator and Initialized
  INTEGER :: comp_id    ! component identification
  !
  INTEGER, DIMENSION(:), ALLOCATABLE :: il_paral ! Decomposition for each proc
  !
  INTEGER :: ierror, rank, w_unit
  INTEGER :: i, j, ij, ic_msk
  INTEGER :: FILE_Debug=1
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
  INTEGER                       :: il_flag          ! Flag for grid writing
  !
  INTEGER                       :: itap_sec ! Time
  !
  ! Grid parameter definition
  INTEGER                       :: part_id  ! use to connect the partition to the variables
                                            ! in oasis_def_var
  INTEGER                       :: var_sh(4) ! local dimensions of the arrays to the pe
                                             ! 2 x field rank (= 4 because fields are of rank = 2)
  INTEGER :: indi_beg, indi_end, indj_beg, indj_end
  !
  ! Local fields arrays used in routines oasis_put and oasis_get
  REAL (kind=wp), POINTER       :: field_recv(:,:), field_ana(:,:), error(:,:)
  !
  ! Global array to plot the error by proc 0 and calculate min and max
  REAL (kind=wp), POINTER       :: global_error(:)
  INTEGER, POINTER              :: global_shapes_2D(:,:)
  INTEGER, POINTER              :: displs(:),rcounts(:),global_shapes(:)
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
      CALL oasis_abort(comp_id,comp_name,'Problem at line 109')
  ENDIF
  !
  ! Unit for output messages : one file for each process
  CALL MPI_Comm_Rank ( MPI_COMM_WORLD, rank, ierror )
  IF (ierror /= 0) THEN
      WRITE(0,*) 'MPI_Comm_Rank abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 116')
  ENDIF
  !
  !
  !!!!!!!!!!!!!!!!! OASIS_GET_LOCALCOMM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  CALL oasis_get_localcomm ( localComm, ierror )
  IF (ierror /= 0) THEN
      WRITE (0,*) 'oasis_get_localcomm abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 125')
  ENDIF
  !
  ! Get MPI size and rank
  CALL MPI_Comm_Size ( localComm, npes, ierror )
  IF (ierror /= 0) THEN
      WRITE(0,*) 'MPI_comm_size abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 132')
  ENDIF
  !
  CALL MPI_Comm_Rank ( localComm, mype, ierror )
  IF (ierror /= 0) THEN
      WRITE (0,*) 'MPI_Comm_Rank abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 138')
  ENDIF
  !
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
      WRITE (w_unit,*) 'Number of porcessors :',npes
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
  ALLOCATE(gg_lon(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating gg_lon'
  ALLOCATE(gg_lat(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating gg_lat'
  ALLOCATE(gg_mask(nlon,nlat), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating indice_mask'
  !
  ! Read global grid longitudes, latitudes and mask 
  !
  CALL read_grid(nlon,nlat, data_gridname, cl_grd_tgt, w_unit, FILE_Debug, &
                 gg_lon,gg_lat)
  CALL read_mask(nlon,nlat, data_maskname, cl_grd_tgt, w_unit, FILE_Debug, &
                 gg_mask)
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
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'After allocate il_paral, il_size', il_size
      CALL FLUSH(w_unit)
  ENDIF
  !
  CALL decomp_def (il_paral,il_size,nlon,nlat,mype,npes,w_unit)
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'After decomp_def, il_paral = ', il_paral(:)
      CALL FLUSH(w_unit)
  ENDIF
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
  var_sh(1) = 1
  var_sh(2) = il_paral(3)
  var_sh(3) = 1 
#ifdef DECOMP_APPLE
  var_sh(4) = 1
#elif defined DECOMP_BOX
  var_sh(4) = il_paral(4)
#endif
  !
  ! Declaration of the field associated with the partition of the grid
  CALL oasis_def_var (var_id,var_name, part_id, &
     var_nodims, OASIS_In, var_sh, var_type, ierror)
  IF (ierror /= 0) THEN
      WRITE (w_unit,*) 'oasis_def_var abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 266')
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
      CALL oasis_abort(comp_id,comp_name,'Problem at line 281')
  ENDIF
  !
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'After oasis_enddef'
      CALL FLUSH(w_unit)
  ENDIF
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! RECEIVE ARRAYS AND CALCULATE THE ERROR 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Allocate the fields send and received by the model
  !
  ALLOCATE(field_recv(var_sh(2), var_sh(4)), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field_recv'
  ALLOCATE(field_ana(var_sh(2), var_sh(4)),STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating field_ana'
  ALLOCATE(error(var_sh(2), var_sh(4)),STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating error'
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
  ! Data exchange in time loop
  !
  ib=1
  itap_sec = delta_t * (ib-1) ! Time
  !
  CALL function_ana(var_sh(2),&
                    var_sh(4), &
                    RESHAPE(gg_lon(indi_beg:indi_end,indj_beg:indj_end),(/ var_sh(2), var_sh(4) /)), &
                    RESHAPE(gg_lat(indi_beg:indi_end,indj_beg:indj_end),(/ var_sh(2), var_sh(4) /)), &
                    field_ana,ib)
  !
  ! Get the field FRECVANA
  field_recv=field_ini
  CALL oasis_get(var_id,itap_sec, field_recv, ierror)
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'tcx recvf1 ',itap_sec,MINVAL(field_recv),MAXVAL(field_recv)
      CALL FLUSH(w_unit)
  ENDIF
  IF ( ierror .NE. OASIS_Ok .AND. ierror .LT. OASIS_Recvd) THEN
      WRITE (w_unit,*) 'oasis_get abort by model2 compid ',comp_id
      CALL oasis_abort(comp_id,comp_name,'Problem at line 327')
  ENDIF
  !
  !
  IF (FILE_Debug >= 2) THEN
      WRITE (w_unit,*) 'Calculate the error of interpolation only if npes = 1'
      CALL FLUSH(w_unit)
  ENDIF
  !
  IF (npes == 1) THEN
  !
  error=err_msk
  !
  ALLOCATE(global_error(ntot), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating global_error'
  !
  WHERE (RESHAPE(gg_mask(indi_beg:indi_end,indj_beg:indj_end),(/ var_sh(2), var_sh(4) /)) == 0)
      error = ABS(((field_ana - field_recv)/field_recv))*100
  END WHERE
  !
  !
  !!!!!!!!!!!!!!!!!!!!!!! Write the error and the field in a NetCDF file by proc 0 !!!!!!!!!!!!!!!!!!!!
  ! field_recv is 0 on masked points => put error = err_msk on these points
  !
  !
  !
  data_filename='error.nc'
  field_name='error'
  CALL write_field(nlon,nlat, &
                       data_filename, field_name,  &
                       w_unit, FILE_Debug, &
                       gg_lon, gg_lat, error)
  !
  !!!!!!!!!!!!!!!!!!!! Min and Max of the error on non masked points calculated by proc 0 !!!!!!!!!!!!!!!!
  !
      ic_msk=0
      DO i=1,nlon
        DO j=1,nlat
          ij=i+(j-1)*(nlon)
          IF ( gg_mask(i,j) == 1 ) THEN
              global_error(ij) = -err_msk
              ic_msk = ic_msk + 1
          ENDIF
        ENDDO
      ENDDO
      min=MINVAL(REAL(global_error))
      IF (FILE_Debug >= 2) THEN
          WRITE(w_unit,*) 'Min and its location in the error field : ',min
          WRITE(w_unit,*) MINLOC(REAL(global_error))
          CALL FLUSH(w_unit)
      ENDIF
      DO i=1,nlon
        DO j=1,nlat
          ij=i+(j-1)*(nlon)
          IF ( gg_mask(i,j) == 1 ) THEN
              global_error(ij) = err_msk
          ENDIF
        ENDDO
      ENDDO
      max=MAXVAL(REAL(global_error))
      IF (FILE_Debug >= 2) THEN
          WRITE(w_unit,*)'Max and its location in the error field : ',max
          WRITE(w_unit,*) MAXLOC(REAL(global_error))
          CALL FLUSH(w_unit)
      ENDIF
      DO i=1,nlon
        DO j=1,nlat
          ij=i+(j-1)*(nlon)
          IF ( gg_mask(i,j) == 1 ) THEN
              global_error(ij) = 0.
          ENDIF
        ENDDO
      ENDDO
      IF (FILE_Debug >= 2) THEN
          WRITE(w_unit,*) 'Number of masked points :',ic_msk
          WRITE(w_unit,*) 'Error mean on non masked points: ', &
             SUM(ABS(global_error))/((ntot)-ic_msk)
          WRITE(w_unit,*) 'End calculation of stat on the error'
          CALL FLUSH(w_unit)
      ENDIF
   ELSE
     WRITE (w_unit,*) 'Pas de calcul d''erreur possible en multiprocs'
     CALL FLUSH(w_unit)
   ENDIF !if npes = 1
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
      CALL oasis_abort(comp_id,comp_name,'Problem at line 477')
  ENDIF
  !
  CALL mpi_finalize(ierror)
  !
END PROGRAM MODEL2
!
