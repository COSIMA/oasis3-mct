MODULE write_all_fields  
  !
  USE netcdf
  IMPLICIT NONE
  !
  ! 
  CONTAINS
    !
 !****************************************************************************************
  SUBROUTINE write_field (nlon,nlat, &
                          data_filename, field_name, w_unit, FILE_Debug, &
                          gridlon, gridlat, array)
  !**************************************************************************************
  !
#ifdef NO_USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(6,37)   ! real
#elif defined USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307) ! double
#endif
  !
  !
  INTEGER                  :: i,j,k,w_unit,FILE_Debug
  !
  INTEGER                  :: il_file_id, il_array_id, il_lon_id, il_lat_id 
  INTEGER                  :: LONID, LATID
  !               
  INTEGER, INTENT(in)     :: nlon,nlat
  !
  CHARACTER(len=30)        :: data_filename,field_name
  !
  INTEGER,  DIMENSION(2)   :: ila_dim
  INTEGER,  DIMENSION(2)   :: ila_what
  !
  REAL (kind=wp), DIMENSION(nlon,nlat)  :: gridlon,gridlat
  REAL (kind=wp), DIMENSION(nlon,nlat)  :: array
  !
  !
  ! Dimensions
  !
  CALL hdlerr(NF90_CREATE(data_filename, NF90_CLOBBER, il_file_id), __LINE__ )
  !
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "lon", nlon, LONID), __LINE__ )
  CALL hdlerr( NF90_DEF_DIM(il_file_id, "lat", nlat, LATID), __LINE__ )
  !
#ifdef NO_USE_DOUBLE_PRECISION
  CALL hdlerr( NF90_DEF_VAR(il_file_id, "lon", NF90_REAL, (/LONID, LATID/), il_lon_id), __LINE__ )
  CALL hdlerr( NF90_PUT_ATT(il_file_id, il_lon_id, "units", "degrees_east"), __LINE__ )
  CALL hdlerr( NF90_PUT_ATT(il_file_id, il_lon_id, "standard_name", "longitude"), __LINE__ )
  CALL hdlerr( NF90_DEF_VAR(il_file_id, "lat", NF90_REAL, (/LONID, LATID/), il_lat_id), __LINE__ )
  CALL hdlerr( NF90_PUT_ATT(il_file_id, il_lat_id, "units", "degrees_north"), __LINE__ )
  CALL hdlerr( NF90_PUT_ATT(il_file_id, il_lat_id, "standard_name", "latitude"), __LINE__ )
  !
  CALL hdlerr( NF90_DEF_VAR(il_file_id, TRIM(field_name), NF90_REAL, (/LONID, LATID/), il_array_id), __LINE__ )
#elif defined USE_DOUBLE_PRECISION
  CALL hdlerr( NF90_DEF_VAR(il_file_id, "lon", NF90_DOUBLE, (/LONID, LATID/), il_lon_id), __LINE__ )
  CALL hdlerr( NF90_PUT_ATT(il_file_id, il_lon_id, "units", "degrees_east"), __LINE__ )
  CALL hdlerr( NF90_PUT_ATT(il_file_id, il_lon_id, "standard_name", "longitude"), __LINE__ )
  CALL hdlerr( NF90_DEF_VAR(il_file_id, "lat", NF90_DOUBLE, (/LONID, LATID/), il_lat_id), __LINE__ )
  CALL hdlerr( NF90_PUT_ATT(il_file_id, il_lat_id, "units", "degrees_north"), __LINE__ )
  CALL hdlerr( NF90_PUT_ATT(il_file_id, il_lat_id, "standard_name", "latitude"), __LINE__ )
  !
  CALL hdlerr( NF90_DEF_VAR(il_file_id, TRIM(field_name), NF90_DOUBLE, (/LONID, LATID/), il_array_id), __LINE__ )
#endif
  !
  CALL hdlerr( NF90_ENDDEF(il_file_id), __LINE__ )
  !
  ila_what(:)=1
  !
  ila_dim(1)=nlon
  ila_dim(2)=nlat
  !
  ! Data
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_lon_id, gridlon, &
     ila_what, ila_dim), __LINE__ )
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_lat_id, gridlat, &
     ila_what, ila_dim), __LINE__ )
  !
  CALL hdlerr( NF90_PUT_VAR (il_file_id, il_array_id, array, &
     ila_what, ila_dim), __LINE__ )
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'Global grid mask reading done'
      CALL FLUSH(w_unit)
  ENDIF
  !
  CALL hdlerr( NF90_CLOSE(il_file_id), __LINE__ )
  !
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'End of routine write field'
      CALL FLUSH(w_unit)
  ENDIF
  !
END SUBROUTINE write_field

END MODULE write_all_fields
