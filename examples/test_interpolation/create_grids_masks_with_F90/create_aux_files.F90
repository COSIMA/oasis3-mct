!------------------------------------------------------------------------
! Copyright 2010, CERFACS, Toulouse, France.
! All rights reserved. Use is subject to OASIS3 license terms.
!=============================================================================
!
!
PROGRAM create_aux_files
  !
  ! Use for netCDF library
  USE netcdf
  !
  IMPLICIT NONE
  !
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307) ! double
  !
  CHARACTER(len=30)   :: data_filename_g1
  CHARACTER(len=30)   :: data_filename_g2
  ! 
  CHARACTER(len=128) :: comp_out                 ! name of the output log file 
  CHARACTER(len=2)   :: chout
  CHARACTER(len=4)   :: src_grd_ini, src_grd_end ! Source grid names
  CHARACTER(len=4)   :: tgt_grd_ini, tgt_grd_end ! Source grid names
  !
  ! Global grid parameters : 
  INTEGER :: nlon1, nlat1     ! dimensions in the 2 directions of space
  INTEGER :: nlon2, nlat2     ! dimensions in the 2 directions of space
  INTEGER :: ntot             ! total dimension
  INTEGER :: nc               ! number of corners
  !
  DOUBLE PRECISION, DIMENSION(:,:), POINTER    :: ggrid1_lon,ggrid1_lat ! lon, lat of the points
  DOUBLE PRECISION, DIMENSION(:,:,:), POINTER  :: ggrid1_clo,ggrid1_cla ! lon, lat of the corners
  INTEGER, DIMENSION(:,:), POINTER             :: g1_mask ! 
  DOUBLE PRECISION, DIMENSION(:,:), POINTER    :: ggrid2_lon,ggrid2_lat ! lon, lat of the points
  DOUBLE PRECISION, DIMENSION(:,:,:), POINTER  :: ggrid2_clo,ggrid2_cla ! lon, lat of the corners
  INTEGER, DIMENSION(:,:), POINTER             :: g2_mask ! 
  !
  INTEGER :: ierror
  INTEGER :: i, j
  !
  INTEGER, PARAMETER    :: w_unit = 15
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  GET THE NAME OF THE INITIAL GRIDS AND FINAL GRIDS 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  CALL getarg(1,data_filename_g1)
  PRINT *, 'Source grid file = ', data_filename_g1
  CALL getarg(2,data_filename_g2)
  PRINT *, 'Target grid file = ', data_filename_g2
  CALL getarg(3,src_grd_ini)
  PRINT *, 'Initial Source grid acronym = ', src_grd_ini
  CALL getarg(4,tgt_grd_ini)
  PRINT *, ' Initial Target grid acronym = ', tgt_grd_ini
  CALL getarg(5,src_grd_end)
  PRINT *, 'Final Source grid acronym = ', src_grd_end
  CALL getarg(6,tgt_grd_end)
  PRINT *, ' Final Target grid acronym = ', tgt_grd_end

  comp_out='create_aux_files.out'
  OPEN(w_unit,file=TRIM(comp_out),form='formatted')
  !
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  READ GRIDS OF THE TWO MODELS 
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  ! Reading dimensions of the first global grid
  CALL read_dimgrid(nlon1,nlat1,data_filename_g1,w_unit,src_grd_ini)
  nc=4
  !
  ! Allocation
  ALLOCATE(ggrid1_lon(nlon1,nlat1), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_lon'
  ALLOCATE(ggrid1_lat(nlon1,nlat1), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_lat'
  ALLOCATE(ggrid1_clo(nlon1,nlat1,nc), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_clo'
  ALLOCATE(ggrid1_cla(nlon1,nlat1,nc), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_cla'
  ALLOCATE(g1_mask(nlon1,nlat1), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating indice_mask'
  !
  ! Reading of the longitudes, latitudes, longitude and latitudes of the corners, mask of the global grid
  CALL read_grid_mask(nlon1,nlat1,nc, data_filename_g1, w_unit, src_grd_ini,  &
                      ggrid1_lon,ggrid1_lat, &
                      ggrid1_clo,ggrid1_cla, &
                      g1_mask)

  ! Mask inversion to follow (historical) OASIS3 convention (0=not masked;1=masked)
  WHERE(g1_mask == 1) 
      g1_mask=0
  ELSEWHERE
      g1_mask=1
  END WHERE
  !
  ! Reading dimensions of the second global grid
  CALL read_dimgrid(nlon2,nlat2,data_filename_g2,w_unit,tgt_grd_ini)
  nc=4
  !
  ! Allocation
  ALLOCATE(ggrid2_lon(nlon2,nlat2), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_lon'
  ALLOCATE(ggrid2_lat(nlon2,nlat2), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_lat'
  ALLOCATE(ggrid2_clo(nlon2,nlat2,nc), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_clo'
  ALLOCATE(ggrid2_cla(nlon2,nlat2,nc), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating globalgrid_cla'
  ALLOCATE(g2_mask(nlon2,nlat2), STAT=ierror )
  IF ( ierror /= 0 ) WRITE(w_unit,*) 'Error allocating indice_mask'
  !
  ! Reading of the longitudes, latitudes, longitude and latitudes of the corners, mask of the global grid
  CALL read_grid_mask(nlon2,nlat2,nc, data_filename_g2, w_unit, tgt_grd_ini,  &
                      ggrid2_lon,ggrid2_lat, &
                      ggrid2_clo,ggrid2_cla, &
                      g2_mask)

  ! Mask inversion to follow (historical) OASIS3 convention (0=not masked;1=masked)
  WHERE(g2_mask == 1) 
      g2_mask=0
  ELSEWHERE
      g2_mask=1
  END WHERE
  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !  WRITE grids.nc and masks.nc with both source and target grids
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL write_grids_masks(nlon1,nlat1,nlon2,nlat2,nc, &
                         src_grd_end, tgt_grd_end, w_unit, &
                         ggrid1_lon,ggrid1_lat,ggrid1_clo,ggrid1_cla,g1_mask, &
                         ggrid2_lon,ggrid2_lat,ggrid2_clo,ggrid2_cla,g2_mask)
  !
  CLOSE (w_unit)
  !
END PROGRAM create_aux_files

