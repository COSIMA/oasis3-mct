!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
SUBROUTINE oasis3_local_grid(mype, npes, nlon, nlat, var_shape_oasis3, &
                             localgrid_lon, localgrid_lat, local_mask,  &
                             globalgrid_lon, globalgrid_lat, indice_mask, w_unit, FILE_Debug)
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  IMPLICIT NONE
  !
#ifdef NO_USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(6,37)   ! real
#elif defined USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307) ! double
#endif
  !
  INTEGER, INTENT(in)    :: nlon,nlat,mype,npes, w_unit,FILE_Debug
  INTEGER, INTENT(in)    :: var_shape_oasis3(4)
  !
  DOUBLE PRECISION, INTENT(in) :: globalgrid_lon(nlon,nlat),globalgrid_lat(nlon,nlat)
  INTEGER, INTENT(in)          :: indice_mask(nlon,nlat)
  !
  REAL (kind=wp), INTENT(out)  :: localgrid_lon(   &
     var_shape_oasis3(1):var_shape_oasis3(2),var_shape_oasis3(3):var_shape_oasis3(4))
  REAL (kind=wp), INTENT(out)  :: localgrid_lat(   &
     var_shape_oasis3(1):var_shape_oasis3(2), var_shape_oasis3(3):var_shape_oasis3(4))
  INTEGER, INTENT(out)         :: local_mask(   &
     var_shape_oasis3(1):var_shape_oasis3(2), var_shape_oasis3(3):var_shape_oasis3(4))
  !
  INTEGER :: indi_beg, indi_end, indj_beg, indj_end, ij,i,j
  !
  !
  indi_beg=1 ; indi_end=nlon
  indj_beg=((nlat/npes)*mype)+1 
  !
  IF (mype .LT. npes - 1) THEN
      indj_end = (nlat/npes)*(mype+1)
  ELSE
      indj_end = nlat 
  ENDIF
  !
  IF (FILE_Debug >= 2) THEN
      WRITE(w_unit,*) 'indi_beg, indi_end,indj_beg, indj_end',indi_beg, indi_end,indj_beg, indj_end 
  ENDIF
  !
#ifdef DECOMP_APPLE
  ! Apple decomposition
  !
  ij=0
  !
  DO j=indj_beg,indj_end
    DO i=indi_beg,indi_end
      ij=i+(j-indj_beg)*(nlon)
!        WRITE(w_unit,*) 'ij = ', ij
!        call flush(w_unit)
#ifdef NO_USE_DOUBLE_PRECISION
      localgrid_lon(ij,1) = REAL(globalgrid_lon(i,j))
      localgrid_lat(ij,1) = REAL(globalgrid_lat(i,j))
#elif defined USE_DOUBLE_PRECISION
      localgrid_lon(ij,1) = globalgrid_lon(i,j)
      localgrid_lat(ij,1) = globalgrid_lat(i,j)
#endif
      !
      local_mask(ij,1) = indice_mask(i,j)
      !
    ENDDO
  ENDDO
  !
#elif defined DECOMP_BOX
  ! Box decomposition
  !
#ifdef NO_USE_DOUBLE_PRECISION
      localgrid_lon(var_shape_oasis3(1):var_shape_oasis3(2),var_shape_oasis3(3): &
         var_shape_oasis3(4)) = REAL(globalgrid_lon(indi_beg:indi_end,&
                                indj_beg:indj_end))
      
      localgrid_lat(var_shape_oasis3(1):var_shape_oasis3(2),var_shape_oasis3(3): &
         var_shape_oasis3(4)) = REAL(globalgrid_lat(indi_beg:indi_end,&
                                indj_beg:indj_end))
#elif defined USE_DOUBLE_PRECISION
      localgrid_lon(var_shape_oasis3(1):var_shape_oasis3(2),var_shape_oasis3(3): &
         var_shape_oasis3(4)) = globalgrid_lon(indi_beg:indi_end,&
                                indj_beg:indj_end)
      
      localgrid_lat(var_shape_oasis3(1):var_shape_oasis3(2),var_shape_oasis3(3): &
         var_shape_oasis3(4)) = globalgrid_lat(indi_beg:indi_end,&
                                indj_beg:indj_end)
#endif
      !
      local_mask(var_shape_oasis3(1):var_shape_oasis3(2),var_shape_oasis3(3): &
         var_shape_oasis3(4)) = indice_mask(indi_beg:indi_end,&
                                indj_beg:indj_end)
      !
#endif
  !
END SUBROUTINE oasis3_local_grid
