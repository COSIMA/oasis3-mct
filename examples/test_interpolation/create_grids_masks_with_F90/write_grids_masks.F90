  !****************************************************************************************
  SUBROUTINE write_grids_masks (nlon1,nlat1,nlon2,nlat2,corners_ij, &
                               grids_name, gridt_name, w_unit, &
                               gridlon1,gridlat1, gridclo1,gridcla1, indice_mask1, &
                               gridlon2,gridlat2, gridclo2,gridcla2, indice_mask2)
  !**************************************************************************************
  !
  USE netcdf
  IMPLICIT NONE
  !
  INTEGER                  :: i,j,k,w_unit
  !
  INTEGER                  :: LONID1, LATID1, LONID2, LATID2, CC1, CC2
  INTEGER                  :: il_file_id,il_lon_id1, &
                              il_lat_id1,il_clo_id1,il_cla_id1,il_indice_id1, &
                              il_lon_id2,il_lat_id2,il_clo_id2,il_cla_id2,il_indice_id2
  !               
  INTEGER, INTENT(in)     :: nlon1,nlat1,nlon2,nlat2,corners_ij
  !
  CHARACTER(len=4)         :: grids_name,gridt_name
  CHARACTER(len=8)         :: cl1_nam_lon, cl1_nam_lat, cl1_nam_msk, cl1_nam_clo, cl1_nam_cla
  CHARACTER(len=8)         :: cl2_nam_lon, cl2_nam_lat, cl2_nam_msk, cl2_nam_clo, cl2_nam_cla
  !
  INTEGER,  DIMENSION(3)   :: ila_dim
  INTEGER,  DIMENSION(3)   :: ila_corners,ila_what
  !
  DOUBLE PRECISION, DIMENSION(nlon1,nlat1)                :: gridlon1,gridlat1
  DOUBLE PRECISION, DIMENSION(nlon1,nlat1,corners_ij) :: gridclo1,gridcla1
  INTEGER,          DIMENSION(nlon1,nlat1)                :: indice_mask1
  DOUBLE PRECISION, DIMENSION(nlon2,nlat2)                :: gridlon2,gridlat2
  DOUBLE PRECISION, DIMENSION(nlon2,nlat2,corners_ij) :: gridclo2,gridcla2
  INTEGER,          DIMENSION(nlon2,nlat2)                :: indice_mask2
  !
  !
  ! Dimensions
  !
  cl1_nam_lon=grids_name//".lon"
  cl1_nam_clo=grids_name//".clo"
  cl1_nam_lat=grids_name//".lat"
  cl1_nam_cla=grids_name//".cla"
  cl1_nam_msk=grids_name//".msk"
  !
  cl2_nam_lon=gridt_name//".lon"
  cl2_nam_clo=gridt_name//".clo"
  cl2_nam_lat=gridt_name//".lat"
  cl2_nam_cla=gridt_name//".cla"
  cl2_nam_msk=gridt_name//".msk"
  !
  !
  CALL hdlerr(NF90_CREATE('grids.nc', NF90_CLOBBER, il_file_id), __LINE__ )
  !
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "lon1", nlon1, LONID1), __LINE__ )
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "lat1", nlat1, LATID1), __LINE__ )
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "crn1", corners_ij, CC1), __LINE__ )
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "lon2", nlon2, LONID2), __LINE__ )
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "lat2", nlat2, LATID2), __LINE__ )
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "crn2", corners_ij, CC2), __LINE__ )
  !
  CALL hdlerr( NF90_DEF_VAR(il_file_id, cl1_nam_lon, NF90_REAL, (/LONID1, LATID1/), il_lon_id1), __LINE__ )
  CALL hdlerr( NF90_DEF_VAR(il_file_id, cl1_nam_lat, NF90_REAL, (/LONID1, LATID1/), il_lat_id1), __LINE__ )
  CALL hdlerr( NF90_DEF_VAR(il_file_id, cl1_nam_clo, NF90_REAL, (/LONID1, LATID1, CC1/), il_clo_id1), __LINE__ )
  CALL hdlerr( NF90_DEF_VAR(il_file_id, cl1_nam_cla, NF90_REAL, (/LONID1, LATID1, CC1/), il_cla_id1), __LINE__ )
  !
  CALL hdlerr( NF90_DEF_VAR(il_file_id, cl2_nam_lon, NF90_REAL, (/LONID2, LATID2/), il_lon_id2), __LINE__ )
  CALL hdlerr( NF90_DEF_VAR(il_file_id, cl2_nam_lat, NF90_REAL, (/LONID2, LATID2/), il_lat_id2), __LINE__ )
  CALL hdlerr( NF90_DEF_VAR(il_file_id, cl2_nam_clo, NF90_REAL, (/LONID2, LATID2, CC2/), il_clo_id2), __LINE__ )
  CALL hdlerr( NF90_DEF_VAR(il_file_id, cl2_nam_cla, NF90_REAL, (/LONID2, LATID2, CC2/), il_cla_id2), __LINE__ )
  !
  CALL hdlerr( NF90_ENDDEF(il_file_id), __LINE__ )
  !
  ila_what(:)=1
  !
  ila_dim(1)=nlon1
  ila_dim(2)=nlat1
  ila_dim(3)=1
  !
  ila_corners(1)=nlon1
  ila_corners(2)=nlat1
  ila_corners(3)=corners_ij
  !
  ! Write Data in grids.nc 
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_lon_id1, gridlon1, &
     ila_what(1:2), ila_dim(1:2)), __LINE__ )
  WRITE(w_unit,*) 'Global source grid longitudes writing done'
  CALL FLUSH(w_unit)
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_lat_id1, gridlat1, &
     ila_what(1:2), ila_dim(1:2)), __LINE__ )
  WRITE(w_unit,*) 'Global source grid latitudes writing done'
  CALL FLUSH(w_unit)
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_clo_id1, gridclo1, &
     ila_what, ila_corners), __LINE__ )
  WRITE(w_unit,*) 'Global source grid clo writing done'
  CALL FLUSH(w_unit)
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_cla_id1, gridcla1, &
     ila_what, ila_corners), __LINE__ )
  WRITE(w_unit,*) 'Global source grid cla writing done'
  CALL FLUSH(w_unit)
  !
  ila_what(:)=1
  !
  ila_dim(1)=nlon2
  ila_dim(2)=nlat2
  ila_dim(3)=1
  !
  ila_corners(1)=nlon2
  ila_corners(2)=nlat2
  ila_corners(3)=corners_ij
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_lon_id2, gridlon2, &
     ila_what(1:2), ila_dim(1:2)), __LINE__ )
  WRITE(w_unit,*) 'Global target grid longitudes writing done'
  CALL FLUSH(w_unit)
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_lat_id2, gridlat2, &
     ila_what(1:2), ila_dim(1:2)), __LINE__ )
  WRITE(w_unit,*) 'Global target grid latitudes writing done'
  CALL FLUSH(w_unit)
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_clo_id2, gridclo2, &
     ila_what, ila_corners), __LINE__ )
  WRITE(w_unit,*) 'Global target grid clo writing done'
  CALL FLUSH(w_unit)
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_cla_id2, gridcla2, &
     ila_what, ila_corners), __LINE__ )
  WRITE(w_unit,*) 'Global target grid cla writing done'
  CALL FLUSH(w_unit)
  !
  CALL hdlerr( NF90_CLOSE(il_file_id), __LINE__ )
  !
  !
  CALL hdlerr(NF90_CREATE('masks.nc', NF90_CLOBBER, il_file_id), __LINE__ )
  !
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "lon1", nlon1, LONID1), __LINE__ )
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "lat1", nlat1, LATID1), __LINE__ )
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "lon2", nlon2, LONID2), __LINE__ )
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "lat2", nlat2, LATID2), __LINE__ )
  !
  CALL hdlerr( NF90_DEF_VAR(il_file_id, cl1_nam_msk, NF90_INT, (/LONID1, LATID1/), il_indice_id1), __LINE__ )
  CALL hdlerr( NF90_DEF_VAR(il_file_id, cl2_nam_msk, NF90_INT, (/LONID2, LATID2/), il_indice_id2), __LINE__ )
  !
  CALL hdlerr( NF90_ENDDEF(il_file_id), __LINE__ )
  !
  ila_what(:)=1
  !
  ila_dim(1)=nlon1
  ila_dim(2)=nlat1
  ila_dim(3)=1
  !
  ! Write Data in masks.nc 
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_indice_id1, indice_mask1, &
     ila_what(1:2), ila_dim(1:2)), __LINE__ )
  WRITE(w_unit,*) 'Global source grid mask writing done'
  CALL FLUSH(w_unit)
  !
  ila_what(:)=1
  !
  ila_dim(1)=nlon2
  ila_dim(2)=nlat2
  ila_dim(3)=1
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_indice_id2, indice_mask2, &
     ila_what(1:2), ila_dim(1:2)), __LINE__ )
  WRITE(w_unit,*) 'Global target grid mask writing done'
  CALL FLUSH(w_unit)
  !
  CALL hdlerr( NF90_CLOSE(il_file_id), __LINE__ )
  !
  WRITE(w_unit,*) 'End of routine write_grids_masks'
  CALL FLUSH(w_unit)
  !
END SUBROUTINE write_grids_masks
