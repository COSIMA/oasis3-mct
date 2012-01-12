!****************************************************************************************
SUBROUTINE read_dim_reg (nlon,nlat,corners_ij_lus,data_filename,w_unit)
  !**************************************************************************************
  USE netcdf
  IMPLICIT NONE
  !
  INTEGER                  :: i,j,k,w_unit
  !
  INTEGER                  :: il_file_id,il_grid_id,il_lon_id, &
     il_lat_id,il_clo_id,il_cla_id,il_indice_id, &
     lon_dims,lat_dims,clo_dims,cla_dims,imask_dims
  !
  INTEGER, DIMENSION(NF90_MAX_VAR_DIMS) :: lon_dims_ids,lat_dims_ids,clo_dims_ids,&
     cla_dims_ids,imask_dims_ids,lon_dims_len,&
     lat_dims_len,clo_dims_len,cla_dims_len,imask_dims_len  
  !               
  INTEGER, INTENT(out)     :: nlon,nlat,corners_ij_lus
  !
  CHARACTER(len=30)        :: data_filename
  !
  ! Dimensions
  !
  CALL hdlerr(NF90_OPEN(data_filename, NF90_NOWRITE, ncid=il_file_id), __LINE__ )
  !
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'lon' ,  il_lon_id),    __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'lat' ,  il_lat_id),    __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'clo' ,  il_clo_id),    __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'cla' ,  il_cla_id),    __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'imask', il_indice_id), __LINE__ )
  !
  CALL hdlerr( NF90_INQUIRE_VARIABLE(ncid=il_file_id, varid=il_lon_id, ndims=lon_dims, dimids=lon_dims_ids), __LINE__ )
  !
  ! The value lon_dims_len(i) is obtained thanks to the lon_dims_ids ID
  ! already obtained from the file
  DO i=1,lon_dims
    CALL hdlerr( NF90_INQUIRE_DIMENSION(ncid=il_file_id,dimid=lon_dims_ids(i),len=lon_dims_len(i)), __LINE__ )
  ENDDO
  !
  nlon=lon_dims_len(1)
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL hdlerr( NF90_INQUIRE_VARIABLE(ncid=il_file_id, varid=il_lat_id, ndims=lat_dims, dimids=lat_dims_ids), __LINE__ )
  !
  ! The value lat_dims_len(i) is obtained thanks to the lat_dims_ids ID already obtained from the file
  DO i=1,lat_dims
    CALL hdlerr( NF90_INQUIRE_DIMENSION(ncid=il_file_id,dimid=lat_dims_ids(i),len=lat_dims_len(i)), __LINE__ )
  ENDDO
  !
  nlat=lat_dims_len(1)
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL hdlerr( NF90_INQUIRE_VARIABLE(ncid=il_file_id, varid=il_clo_id, ndims=clo_dims, dimids=clo_dims_ids), __LINE__ )
  !
  ! The value clo_dims_len(i) is obtained thanks to the clo_dims_ids ID already obtained from the file
  DO i=1,clo_dims
    CALL hdlerr( NF90_INQUIRE_DIMENSION(ncid=il_file_id,dimid=clo_dims_ids(i),len=clo_dims_len(i)), __LINE__ )
  ENDDO
  !
  corners_ij_lus=clo_dims_len(2)
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL hdlerr( NF90_INQUIRE_VARIABLE(ncid=il_file_id, varid=il_cla_id, ndims=cla_dims, dimids=cla_dims_ids), __LINE__ )
  !
  ! The value cla_dims_len(i) is obtained thanks to the cla_dims_ids ID already obtained from the file
  DO i=1,cla_dims
    CALL hdlerr( NF90_INQUIRE_DIMENSION(ncid=il_file_id,dimid=cla_dims_ids(i),len=cla_dims_len(i)), __LINE__ )
  ENDDO
  !
  IF (cla_dims_len(1) .NE. lat_dims_len(1)) THEN
      WRITE(w_unit,*) 'Problem model2 in read_dim_reg'
      WRITE(w_unit,*) 'Dimension of the latitude of the corners is not the same as the one of the latitude'
      CALL flush(w_unit)
      STOP
  ENDIF
  !
  IF (cla_dims_len(2) .NE. clo_dims_len(2)) THEN
      WRITE(w_unit,*) 'Problem model2 in read_dim_reg'
      WRITE(w_unit,*) 'Problem of the dimension of the corners in the (i,j) plan'
      WRITE(w_unit,*) 'Number of corners for the longitude :',clo_dims_len(2)
      WRITE(w_unit,*) 'Number of corners for the latitude :',cla_dims_len(2)
      CALL flush(w_unit)
      STOP
  ENDIF
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL hdlerr( NF90_INQUIRE_VARIABLE(ncid=il_file_id, varid=il_indice_id, ndims=imask_dims, dimids=imask_dims_ids), __LINE__ )
  !
  ! The value imask_dims_len(i) is obtained thanks to the imask_dims_ids ID already obtained from the file
  DO i=1,imask_dims
    CALL hdlerr( NF90_INQUIRE_DIMENSION(ncid=il_file_id,dimid=imask_dims_ids(i),len=imask_dims_len(i)), __LINE__ )
  ENDDO
  !
  CALL hdlerr(NF90_CLOSE(il_file_id), __LINE__ )
  !
  WRITE(w_unit,*) 'Reading input file ',data_filename
  WRITE(w_unit,*) 'Global dimensions nlon=',nlon,' nlat=',nlat
  WRITE(w_unit,*) 'Corners in the (i,j) plan corners_ij_lus =',corners_ij_lus
  CALL flush(w_unit)
  !
END SUBROUTINE read_dim_reg
