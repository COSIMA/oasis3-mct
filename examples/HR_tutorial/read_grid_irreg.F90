  !****************************************************************************************
  SUBROUTINE read_grid_irreg (nlon, nlat, id_begi, id_begj, id_lon, id_lat, &
                              data_filename, w_unit,                       &
                              dda_lon, dda_lat, ida_mask)
  !**************************************************************************************
  !
  USE netcdf
  IMPLICIT NONE
  !
  INTEGER, INTENT(in)             :: nlon, nlat, id_begi, id_begj, id_lon, id_lat
  CHARACTER(len=30), INTENT(in)   :: data_filename
  INTEGER, INTENT(in)             :: w_unit
  DOUBLE PRECISION, DIMENSION(id_lon, id_lat), INTENT(out)       :: dda_lon,dda_lat
  INTEGER, DIMENSION(id_lon, id_lat), INTENT(out)                :: ida_mask
  !
  INTEGER                         :: il_file_id, il_lon_id, il_lat_id, il_indice_id
  !               
  INTEGER,  DIMENSION(3)          :: ila_dim, ila_st
  !
  CALL hdlerr(NF90_OPEN(data_filename, NF90_NOWRITE, il_file_id), __LINE__ )
  !
  !**************************************************************************************
  !
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'lon' , il_lon_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'lat' , il_lat_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'imask' , il_indice_id), __LINE__ )
  !
  ila_st(1) = id_begi
  ila_st(2) = id_begj
  ila_st(3) = 1
  !
  ila_dim(1) = id_lon
  ila_dim(2) = id_lat
  ila_dim(3) = 1
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_lon_id, dda_lon, &
     ila_st(1:2), ila_dim(1:2)), __LINE__ )
  !
#ifdef _DEBUG
  WRITE(w_unit,*) 'Local grid longitudes dda_lon read from file'
  CALL flush(w_unit)
#endif
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_lat_id, dda_lat, &
     ila_st(1:2), ila_dim(1:2)), __LINE__ )
  !
#ifdef _DEBUG
  WRITE(w_unit,*) 'Local grid latitudes dda_lat read from file'
  CALL flush(w_unit)
#endif
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_indice_id, ida_mask, &
     ila_st(1:2), ila_dim(1:2)), __LINE__ )
  !
#ifdef _DEBUG
  WRITE(w_unit,*) 'Local grid mask ida_mask_mask read from file'
  CALL flush(w_unit)
#endif
  !
  CALL hdlerr( NF90_CLOSE(il_file_id), __LINE__ )
  !
  ! OASIS3 mask convention (1=masked, 0=not masked) is opposite to usual one)
  !
  WHERE (ida_mask == 0)
      ida_mask = 1
  ELSEWHERE
      ida_mask = 0
  END WHERE
  !
#ifdef _DEBUG
  WRITE(w_unit,*) 'End of routine read_grid_irreg'
  CALL flush(w_unit)
#endif
  !
END SUBROUTINE read_grid_irreg
