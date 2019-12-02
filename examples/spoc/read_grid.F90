  !****************************************************************************************
  SUBROUTINE read_grid (nlon, nlat, nc, id_begi, id_begj, id_lon, id_lat, &
                              data_filename, w_unit,                       &
                              dda_lon, dda_lat, dda_clo, dda_cla, dda_srf, ida_mask)
  !**************************************************************************************
  !
  USE netcdf
  IMPLICIT NONE
  !
  INTEGER, INTENT(in)             :: nlon, nlat, nc, id_begi, id_begj, id_lon, id_lat
  CHARACTER(len=*), INTENT(in)   :: data_filename
  INTEGER, INTENT(in)             :: w_unit
  DOUBLE PRECISION, DIMENSION(id_lon, id_lat), INTENT(out)       :: dda_lon, dda_lat, dda_srf
  DOUBLE PRECISION, DIMENSION(id_lon, id_lat, nc), INTENT(out)   :: dda_clo, dda_cla
  INTEGER, DIMENSION(id_lon, id_lat), INTENT(out)                :: ida_mask
  !
  INTEGER :: il_file_id, il_lon_id, il_lat_id, il_clo_id, il_cla_id, il_srf_id, il_msk_id
  !               
  INTEGER,  DIMENSION(3)          :: ila_dim, ila_st
  !
#ifdef _DEBUG
  WRITE(w_unit,*) 'Starting read_grid'
  CALL flush(w_unit)
#endif
  CALL hdlerr (NF90_OPEN(data_filename, NF90_NOWRITE, il_file_id), __LINE__ )
  !
  !**************************************************************************************
  !
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'lon' , il_lon_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'lat' , il_lat_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'clo' , il_clo_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'cla' , il_cla_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'srf' , il_srf_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'imask' , il_msk_id), __LINE__ )
  !
  CALL flush(w_unit)
  ila_st(1) = id_begi
  ila_st(2) = id_begj
  ila_st(3) = 1
  !
  ila_dim(1) = id_lon
  ila_dim(2) = id_lat
  ila_dim(3) = nc
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_lon_id, dda_lon, ila_st(1:2), ila_dim(1:2)), __LINE__ )
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_lat_id, dda_lat, ila_st(1:2), ila_dim(1:2)), __LINE__ )
#ifdef _DEBUG
  WRITE(w_unit,*) 'Local grid longitudes and latitudes read from file'
  CALL flush(w_unit)
#endif
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_clo_id, dda_clo, ila_st(1:3), ila_dim(1:3)), __LINE__ )
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_cla_id, dda_cla, ila_st(1:3), ila_dim(1:3)), __LINE__ )
#ifdef _DEBUG
  WRITE(w_unit,*) 'Local grid corner longitudes and latitudes read from file'
  CALL flush(w_unit)
#endif
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_srf_id, dda_srf, ila_st(1:2), ila_dim(1:2)), __LINE__ )
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_msk_id, ida_mask, ila_st(1:2), ila_dim(1:2)), __LINE__ )
  !
#ifdef _DEBUG
  WRITE(w_unit,*) 'Local grid areas and mask read from file'
  CALL flush(w_unit)
#endif
  !
  CALL hdlerr( NF90_CLOSE(il_file_id), __LINE__ )
  !
  ! OASIS3 mask convention (1=masked, 0=not masked) is opposite to usual one)
  !
  WHERE (ida_mask == 0) ; ida_mask = 1; ELSEWHERE ; ida_mask = 0; END WHERE
  !
#ifdef _DEBUG
  WRITE(w_unit,*) 'End of routine read_grid'
  CALL flush(w_unit)
#endif
  !
END SUBROUTINE read_grid
!
!*********************************************************************************
SUBROUTINE hdlerr(istatus, line)
  !*********************************************************************************
  use netcdf
  implicit none
  !
  INCLUDE 'mpif.h'
  !
  ! Check for error message from NetCDF call
  !
  integer, intent(in) :: istatus, line
  integer             :: ierror
  !
  IF (istatus .NE. NF90_NOERR) THEN
      write ( * , * ) 'NetCDF problem at line',line
      write ( * , * ) 'Stopped '
      call MPI_Abort ( MPI_COMM_WORLD, 1, ierror )
  ENDIF
  !
  RETURN
END SUBROUTINE hdlerr
