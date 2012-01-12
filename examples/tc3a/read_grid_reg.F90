!****************************************************************************************
SUBROUTINE read_grid_reg (nlon,nlat,corners_ij_lus, &
                                    data_filename, w_unit, &
                                    reg_lon,reg_lat, &
                                    reg_clo,reg_cla, &
                                    indice_mask)
  !**************************************************************************************
  USE netcdf
  IMPLICIT NONE
  !
  INTEGER                  :: i,j,k,w_unit
  !
  INTEGER                  :: il_file_id,il_grid_id,il_lon_id, &
                              il_lat_id,il_clo_id,il_cla_id,il_indice_id, &
                              lon_dims,lat_dims,clo_dims,cla_dims,&
                              imask_dims
  !
  INTEGER, DIMENSION(NF90_MAX_VAR_DIMS) :: lon_dims_ids,lat_dims_ids,clo_dims_ids,&
                                           cla_dims_ids,imask_dims_ids,lon_dims_len,&
                                           lat_dims_len,clo_dims_len,cla_dims_len,&
                                           imask_dims_len  
  !               
  INTEGER, INTENT(in)     :: nlon,nlat,corners_ij_lus
  !
  CHARACTER(len=30)        :: data_filename
  !
  INTEGER,  DIMENSION(3)       :: ila_dim,ila_what
  INTEGER,  DIMENSION(2)       :: ila_corners
  !
  !
  DOUBLE PRECISION, DIMENSION(nlon)                     :: reg_lon
  DOUBLE PRECISION, DIMENSION(nlat)                     :: reg_lat
  DOUBLE PRECISION, DIMENSION(nlon,corners_ij_lus)      :: reg_clo
  DOUBLE PRECISION, DIMENSION(nlat,corners_ij_lus)      :: reg_cla
  INTEGER, DIMENSION(nlon,nlat)                      :: indice_mask, indice_mask_inv
  !
  !
  ! Dimensions
  !
  CALL hdlerr(NF90_OPEN(data_filename, NF90_NOWRITE, ncid=il_file_id), __LINE__ )
  !
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'lon' , il_lon_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'lat' , il_lat_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'clo' , il_clo_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'cla' , il_cla_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'imask' , il_indice_id), __LINE__ )
  !
  !
  ila_what(:)=1
  !
  ila_dim(1)=nlon
  ila_dim(2)=nlat
  ila_dim(3)=1
  !
  CALL hdlerr( NF90_OPEN(data_filename, NF90_NOWRITE, il_file_id), __LINE__ )
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_lon_id, reg_lon), __LINE__ )
  !
  WRITE(w_unit,*) 'We read globalgrid_lon'
  CALL flush(w_unit)
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_lat_id, reg_lat), __LINE__ )
  !
  WRITE(w_unit,*) 'We read globalgrid_lat'
  CALL flush(w_unit)
  !
  ila_corners(1)=nlon
  ila_corners(2)=corners_ij_lus
  !
  CALL hdlerr( NF90_GET_VAR(il_file_id, il_clo_id, reg_clo, &
     ila_what(1:2), ila_corners(1:2)), __LINE__ )
  !
  WRITE(w_unit,*) 'We read globalgrid_clo'
  CALL flush(w_unit)
  !
  ila_corners(1)=nlat
  ila_corners(2)=corners_ij_lus
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_cla_id, reg_cla, &
     ila_what(1:2), ila_corners(1:2)), __LINE__ )
  !
  WRITE(w_unit,*) 'We read globalgrid_cla'
  CALL flush(w_unit)
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_indice_id, indice_mask_inv, &
     ila_what, ila_dim), __LINE__ )
  !
  WRITE(w_unit,*) 'We read globalgrid_indice_mask_inv'
  CALL flush(w_unit)
  !
  !
  CALL hdlerr( NF90_CLOSE(il_file_id), __LINE__ )
  !
  DO i = 1,ila_dim(1)
    DO j = 1,ila_dim(2)
        IF ( indice_mask_inv(i,j) == 0 ) indice_mask(i,j) = 1
        IF ( indice_mask_inv(i,j) == 1 ) indice_mask(i,j) = 0
      ENDDO
  ENDDO
  !
  WRITE(w_unit,*) 'End of routine read_grid_reg'
  CALL flush(w_unit)
  !
END SUBROUTINE read_grid_reg
