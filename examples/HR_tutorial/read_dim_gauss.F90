!****************************************************************************************
SUBROUTINE read_dim_gauss(data_filename, npt_hor)
  !**************************************************************************************
  USE netcdf
  IMPLICIT NONE
  CHARACTER(len=30), INTENT(in)  :: data_filename
  INTEGER, INTENT(out)           :: npt_hor
  !
  INTEGER                        :: il_file_id, il_lon_id
  INTEGER                        :: lon_dims
  !
  INTEGER, DIMENSION(NF90_MAX_VAR_DIMS) :: lon_dims_ids
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL hdlerr(NF90_OPEN(data_filename, NF90_NOWRITE, il_file_id), __LINE__ )
  !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'src_grid_center_lon' , il_lon_id), __LINE__ )
  CALL hdlerr( NF90_INQUIRE_VARIABLE(ncid=il_file_id, varid=il_lon_id, ndims=lon_dims, dimids=lon_dims_ids), __LINE__ )
  CALL hdlerr( NF90_INQUIRE_DIMENSION(ncid=il_file_id,dimid=lon_dims_ids(1),len=npt_hor), __LINE__ )
  !
  CALL hdlerr(NF90_CLOSE(il_file_id), __LINE__ )
  !
END SUBROUTINE read_dim_gauss
