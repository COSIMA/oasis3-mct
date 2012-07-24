  !****************************************************************************************
  SUBROUTINE read_grid (nlon,nlat,corners_ij_lus, &
                                       data_filename, w_unit, &
                                       gridlon,gridlat, &
                                       gridclo,gridcla, &
                                       indice_mask)
  !**************************************************************************************
  !
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
  INTEGER,  DIMENSION(3)   :: ila_dim
  INTEGER,  DIMENSION(3)   :: ila_corners,ila_what
  !
  DOUBLE PRECISION, DIMENSION(nlon,nlat)                :: gridlon,gridlat
  DOUBLE PRECISION, DIMENSION(nlon,nlat,corners_ij_lus) :: gridclo,gridcla
  INTEGER, DIMENSION(nlon,nlat)                      :: indice_mask
  !
  !
  ! Dimensions
  !
  CALL hdlerr(NF90_OPEN(data_filename, NF90_NOWRITE, il_file_id), __LINE__ )
  !
  !
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'lon' , il_lon_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'lat' , il_lat_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'clo' , il_clo_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'cla' , il_cla_id), __LINE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'imask' , il_indice_id), __LINE__ )
  !
  ila_what(:)=1
  !
  ila_dim(1)=nlon
  ila_dim(2)=nlat
  ila_dim(3)=1
  !
  ila_corners(1)=nlon
  ila_corners(2)=nlat
  ila_corners(3)=corners_ij_lus
  !
  ! Data
  !
  CALL hdlerr( NF90_OPEN(data_filename, NF90_NOWRITE, il_file_id), __LINE__ )
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_lon_id, gridlon, &
     ila_what(1:2), ila_dim(1:2)), __LINE__ )
  !
  WRITE(w_unit,*) 'We read globalgrid_lon'
  CALL flush(w_unit)
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_lat_id, gridlat, &
     ila_what(1:2), ila_dim(1:2)), __LINE__ )
  !
  WRITE(w_unit,*) 'We read globalgrid_lat'
  CALL flush(w_unit)
  WRITE(w_unit,*) 'We read globalgrid_lat 2'
  WRITE(w_unit,*) 'ila_corners', ila_corners(:)
  CALL flush(w_unit)
  !
  CALL hdlerr( NF90_GET_VAR(il_file_id, il_clo_id, gridclo, &
     ila_what, ila_corners), __LINE__ )
  !
  WRITE(w_unit,*) 'We read globalgrid_clo'
  CALL flush(w_unit)
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_cla_id, gridcla, &
     ila_what, ila_corners), __LINE__ )
  !
  WRITE(w_unit,*) 'We read globalgrid_cla'
  CALL flush(w_unit)
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_indice_id, indice_mask, &
     ila_what, ila_dim), __LINE__ )
  !
  WRITE(w_unit,*) 'We read globalgrid_indice_mask'
  CALL flush(w_unit)
  !
  CALL hdlerr( NF90_CLOSE(il_file_id), __LINE__ )
  !
  WRITE(w_unit,*) 'End of routine read_grid'
  CALL flush(w_unit)
  !
END SUBROUTINE read_grid
