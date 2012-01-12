SUBROUTINE read_grid_gauss(data_filename,id_nptloc,id_offset, &
                           dla_gred_lon,dla_gred_lat,ila_gred_msk)
  USE netcdf
  IMPLICIT NONE
  CHARACTER(len=30),                       INTENT(in)  :: data_filename
  INTEGER,                                 INTENT(in)  :: id_nptloc, id_offset
  DOUBLE PRECISION, DIMENSION(id_nptloc,1),  INTENT(out) :: dla_gred_lon,dla_gred_lat
  INTEGER, DIMENSION(id_nptloc,1),           INTENT(out) :: ila_gred_msk 
!
  INTEGER                        :: il_file_id, il_var_id, il_start
  INTEGER,  DIMENSION(2)         :: ila_dim, ila_st
  !
  ! Dimensions
  !
  CALL hdlerr(NF90_OPEN(data_filename, NF90_NOWRITE, il_file_id), __LINE__ )
  !
  il_start = id_offset+1
  ila_st(1) = id_offset+1
  ila_st(2) = 1
  ila_dim(1) = id_nptloc
  ila_dim(2) = 1
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'src_grid_center_lon' , il_var_id), __LINE__ )
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_var_id, dla_gred_lon, ila_st(1:2), ila_dim(1:2)), __LINE__ )
  !
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'src_grid_center_lat' , il_var_id), __LINE__ )
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_var_id, dla_gred_lat, ila_st(1:2), ila_dim(1:2)), __LINE__ )
  !
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'src_grid_imask' , il_var_id), __LINE__ )
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_var_id, ila_gred_msk, ila_st(1:2), ila_dim(1:2)), __LINE__ )
  !
  ! Inverse mask to fit OASIS3 convention
  !
!!$  WHERE (ila_gred_msk == 0)
!!$      ila_gred_msk = 1
!!$  ELSEWHERE
!!$      ila_gred_msk = 0
!!$  END WHERE
  !
  CALL hdlerr( NF90_CLOSE(il_file_id), __LINE__ )
  !
END SUBROUTINE read_grid_gauss
