!****************************************************************************************
SUBROUTINE read_dim_irreg (nlon,nlat,data_filename)
  !**************************************************************************************
  USE netcdf
  IMPLICIT NONE
  !
  INTEGER, INTENT(out)                  :: nlon,nlat
  CHARACTER(len=30), INTENT(in)         :: data_filename
  INTEGER                               :: i, il_file_id, il_lon_id, lon_dims
  INTEGER, DIMENSION(NF90_MAX_VAR_DIMS) :: lon_dims_ids, lon_dims_len
  !               
  CALL hdlerr(NF90_OPEN(data_filename, NF90_NOWRITE, il_file_id), __LINE__ )
  !
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'lon' ,  il_lon_id),    __LINE__ )
  CALL hdlerr( NF90_INQUIRE_VARIABLE(il_file_id, varid=il_lon_id, ndims=lon_dims, dimids=lon_dims_ids), __LINE__ )
  !
  ! The value lon_dims_len(i) is obtained thanks to the lon_dims_ids ID already obtained from the file
  DO i=1,lon_dims
    CALL hdlerr( NF90_INQUIRE_DIMENSION(ncid=il_file_id,dimid=lon_dims_ids(i),len=lon_dims_len(i)), __LINE__ )
  ENDDO
  !
  nlon=lon_dims_len(1)
  nlat=lon_dims_len(2)
  !
  CALL hdlerr(NF90_CLOSE(il_file_id), __LINE__ )
  !
END SUBROUTINE read_dim_irreg
