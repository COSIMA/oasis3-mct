MODULE rotations
!-----------------------------------------------------------------------

  USE kinds_mod             ! defines common data types      
  USE constants             ! defines common constants      
  USE grids
  USE vector

  IMPLICIT NONE

!!#include <netcdf.inc>  ! include done in module vector
  
  REAL (KIND=dbl_kind), PARAMETER :: &
     rp_deg2rad = pi/180.            ! Conversion from degrees to radians

!
!  This module contains necessary routines for transformations between 
!  the referentials Cartesian, spheric and streched spheric.
!

!  History:
!  --------
!  E. Rapaport     2004/03     Created
!  J. Ghattas      2006/02     Modified and added write of matrix to remapping file
!                              

CONTAINS

!-----------------------------------------------------------------------
 
   SUBROUTINE angel(cd_grd_name,id_grd_size,rda_sin_alpha,rda_cos_alpha)
     
      CHARACTER (LEN=8), INTENT(in) :: cd_grd_name        ! grid name
     
      INTEGER (KIND=int_kind), INTENT(in) :: id_grd_size  ! grid size
      
      CHARACTER(LEN=8) ::  name_ang                       ! name of the angle valew in grids.nc
    
      REAL (KIND=dbl_kind), DIMENSION (:), ALLOCATABLE :: rla_alpha    
      ! the angel between the direction j in streched and the 
      ! north-south in spheric coordonate system
    
    INTEGER (KIND=int_kind) :: &
         il_stat, il_varid, il_vtype, &     ! netcdf variables
         icount, ilenstr, i, nc_grdid
    
!RETURN VALUE:

    REAL (KIND=dbl_kind), DIMENSION(id_grd_size), INTENT(out) :: &
         rda_sin_alpha,&     ! the sinus and cosinus for the angel between the 
         rda_cos_alpha       ! direction j in streched and the north-south 
                             ! in spheric
    
! !DESCRIPTION:
!
!  This routine finds the cosinus and sinus of the angle between the
!  north-south direction in a spheric referential and the direction
!  of gridlines j in a streched spheric referential. 
!  If there is no angel information in file grids.nc we stop.
!

    IF (nlogprt .GE. 2) THEN
       WRITE (UNIT = nulou,FMT = *) ' '
       WRITE (UNIT = nulou,FMT = *) '     Entering ROUTINE angel'
       WRITE (UNIT = nulou,FMT = *) ' '
       CALL FLUSH(nulou)
    ENDIF

    IF (lncdfgrd) THEN
       icount = ilenstr(cd_grd_name,jpeight)
       name_ang = cd_grd_name(1:icount)//'.ang'

       CALL hdlerr &
            (NF_OPEN('grids.nc',NF_NOWRITE,nc_grdid),'angel')

       il_stat = NF_INQ_VARID(nc_grdid, name_ang, il_varid)

       IF (il_stat < 0) THEN
          WRITE (UNIT = nulou,FMT = *) &
	       'File grids.nc contains no angle information,'
          WRITE (UNIT = nulou,FMT = *) &
	       'OASIS will stop'        
          STOP
       ELSE IF (il_stat == 0) THEN
!
!** Get angle information from file grids.nc
!
          ALLOCATE(rla_alpha(id_grd_size))
       
          CALL hdlerr(NF_GET_VAR_DOUBLE &
               (nc_grdid, il_varid, rla_alpha),'angel') 

          rda_sin_alpha(:) = SIN(rp_deg2rad*rla_alpha(:))	 
          rda_cos_alpha(:) = COS(rp_deg2rad*rla_alpha(:))
       
          DEALLOCATE(rla_alpha)
       END IF

       CALL hdlerr (NF_CLOSE(nc_grdid),'angel')
    ELSE
       il_stat = -1
    END IF

    IF (nlogprt .GE. 2) THEN
       WRITE (UNIT = nulou,FMT = *) ' '
       WRITE (UNIT = nulou,FMT = *) '  Leaving routine angel '
       CALL FLUSH (nulou)
    ENDIF

  END SUBROUTINE angel
!
!-----------------------------------------------------------------------
!
     SUBROUTINE loc2spher(cd_grd_name, id_grd_size, rda_trans, nc_fileid)
          
       CHARACTER (LEN=8),INTENT(in) :: &
            cd_grd_name      ! grid name      
    
       INTEGER (KIND=int_kind), INTENT(in) :: &
            id_grd_size, &      ! grid size
            nc_fileid            ! id of open netcdf remapping file

! !RETURN VALUE:

       REAL (KIND=dbl_kind), DIMENSION(id_grd_size,2,2), INTENT(out) :: &
            rda_trans

! Local variables:
       REAL (KIND=dbl_kind), DIMENSION(id_grd_size) :: &
            rla_sin_alpha, &    ! the sinus and cosinus for the angel between the 
            rla_cos_alpha       ! direction j in streched and the north-south 
                                ! in spheric

       INTEGER(KIND=int_kind) :: dimid_src, dim2_id, var_id, stat
       INTEGER(KIND=int_kind), DIMENSION(3) :: dim_id

! !DESCRIPTION:
!
!  This routine finds the transformation matrix between a local stretched 
!  spheric referential and a regulier spheric referential.
!  The matrix is returned and written to open file with id nc_fileid
!

       IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) '   Entering ROUTINE loc2spher '
          CALL FLUSH(nulou)
       ENDIF
!
!*Find the angle between the two referentials
!
       CALL angel(cd_grd_name, id_grd_size, &
            rla_sin_alpha, rla_cos_alpha)	     

       rda_trans(:,1,1) =  rla_cos_alpha 
       rda_trans(:,2,1) =  rla_sin_alpha
       rda_trans(:,1,2) =  -rla_sin_alpha 
       rda_trans(:,2,2) =  rla_cos_alpha
	

!
!*Write matrix to remapping file
!

      CALL hdlerr(NF_REDEF(nc_fileid), 'loc2spher')

      CALL hdlerr(NF_INQ_DIMID &
	 (nc_fileid,'src_grid_size',dimid_src), 'loc2spher')

      stat=NF_INQ_DIMID(nc_fileid,'dim_2',dim2_id)

      IF (stat .NE. NF_NOERR) THEN
         CALL hdlerr(NF_DEF_DIM &
              (nc_fileid,'dim_2',2,dim2_id), 'loc2spher')
      ENDIF

      dim_id(1) = dimid_src 
      dim_id(2) = dim2_id
      dim_id(3) = dim2_id

      CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Mat_src_loc2sph',NF_DOUBLE,3,dim_id,var_id), 'loc2spher')     

      CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,var_id, 'matrix',&
	 55,'Transformation matrix for source grid local to spheric'),&
	 'loc2spher')

      CALL hdlerr(NF_ENDDEF(nc_fileid), 'loc2spher')

      CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,var_id,rda_trans),'loc2spher')


       IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) &
               '          --------- Leaving routine loc2spher ---------'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL FLUSH(nulou)
       ENDIF

     END SUBROUTINE loc2spher
! 
!-----------------------------------------------------------------------
! 
     SUBROUTINE spher2loc(cd_grd_name, id_grd_size, rda_trans, nc_fileid)
     
!* Input variables
       CHARACTER (LEN=8),INTENT(in) :: &
            cd_grd_name       ! grid name      
   
       INTEGER (KIND=int_kind), INTENT(in) :: &
            id_grd_size, &       ! grid size
            nc_fileid            ! id of open netcdf remapping file

!*RETURN VALUE:
       REAL (KIND=dbl_kind), DIMENSION(id_grd_size,2,2), INTENT(out) :: rda_trans
      
!*Local variables
       REAL (KIND=dbl_kind), DIMENSION(id_grd_size) :: &
            rla_sin_alpha, &  ! the sinus and cosinus for the angel between the 
            rla_cos_alpha     ! direction j in streched and the north-south in spheric
   
       INTEGER(KIND=int_kind) :: dimid_dst, dim2_id, var_id, stat
       INTEGER(KIND=int_kind), DIMENSION(3) :: dim_id

!*DESCRIPTION:
!  Finds the transformation matrix between a spheric and a stretched 
!  spheric referential. 
!

       IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) '  Entering ROUTINE spher2loc '
          CALL FLUSH(nulou)
       ENDIF
!
!*Find the angle between the two referentials
!     
       CALL angel(cd_grd_name, id_grd_size, &
            rla_sin_alpha, rla_cos_alpha)	  

       rda_trans(:,1,1) =  rla_cos_alpha 
       rda_trans(:,2,1) =  -rla_sin_alpha
       rda_trans(:,1,2) = rla_sin_alpha
       rda_trans(:,2,2) =  rla_cos_alpha

!
!*Write matrix to remapping file
!

      CALL hdlerr(NF_REDEF(nc_fileid), 'spher2loc')

      CALL hdlerr(NF_INQ_DIMID &
	 (nc_fileid,'dst_grid_size',dimid_dst), 'spher2loc')

      stat=NF_INQ_DIMID(nc_fileid,'dim_2',dim2_id)

      IF (stat .NE. NF_NOERR) THEN
         CALL hdlerr(NF_DEF_DIM &
              (nc_fileid,'dim_2',2,dim2_id), 'spher2loc')
      ENDIF

      dim_id(1) = dimid_dst 
      dim_id(2) = dim2_id
      dim_id(3) = dim2_id

      CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Mat_dst_sph2loc',NF_DOUBLE,3,dim_id,var_id), 'spher2loc')     

      CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,var_id, 'matrix',&
	 55,'Transformation matrix for target grid spheric to local'),&
	 'spher2loc')

      CALL hdlerr(NF_ENDDEF(nc_fileid), 'spher2loc')

      CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,var_id,rda_trans),'spher2loc')



       IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) &
               '          --------- Leaving routine spher2loc ---------'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL FLUSH(nulou)
       ENDIF

     END SUBROUTINE spher2loc    
!
!-----------------------------------------------------------------------
! 
     SUBROUTINE spher2car(rda_grd_lon, rda_grd_lat, id_grd_size, rda_trans, nc_fileid)

       INTEGER (KIND=int_kind), INTENT(in) :: &
            id_grd_size, &                 ! grid size
            nc_fileid                      ! id of open netcdf remapping file
       
       REAL (KIND=dbl_kind), DIMENSION(id_grd_size),INTENT(in) :: &
            rda_grd_lon, &                 ! grid longitudes
            rda_grd_lat                    ! grid latitudes
    
! !RETURN VALUE:
  
       REAL (KIND=dbl_kind), DIMENSION(id_grd_size,3,2), INTENT(out) :: rda_trans 
   
! Local variables

       REAL (KIND=dbl_kind), DIMENSION(id_grd_size) :: &
            rla_sin_lon, rla_cos_lon, &    ! sinus and cosinus for longitudes
            rla_sin_lat, rla_cos_lat       ! sinus and cosinus for latitudes

       INTEGER(KIND=int_kind) :: dimid_src, dim2_id, dim3_id, var_id, stat
       INTEGER(KIND=int_kind), DIMENSION(3) :: dim_id



! !DESCRIPTION:
!  Finds the rotation matrix between a spheric and cartesien referentails.
!

    IF (nlogprt .GE. 2) THEN
	WRITE (UNIT = nulou,FMT = *) ' '
	WRITE (UNIT = nulou,FMT = *) ' Entering ROUTINE - spher2car'
	WRITE (UNIT = nulou,FMT = *) ' '
        CALL FLUSH(nulou)
    ENDIF

    rla_sin_lon(:) = SIN(rda_grd_lon(:) * rp_deg2rad)
    rla_cos_lon(:) = COS(rda_grd_lon(:) * rp_deg2rad)
    rla_sin_lat(:) = SIN(rda_grd_lat(:) * rp_deg2rad)
    rla_cos_lat(:) = COS(rda_grd_lat(:) * rp_deg2rad)

    rda_trans(:,1,1) = -rla_sin_lon(:)
    rda_trans(:,1,2) = -rla_sin_lat(:) * rla_cos_lon(:)
    rda_trans(:,2,1) =  rla_cos_lon(:)
    rda_trans(:,2,2) = -rla_sin_lat(:) * rla_sin_lon(:)
    rda_trans(:,3,1) =  0.
    rda_trans(:,3,2) =  rla_cos_lat(:)

!
!*Write matrix to remapping file
!

      CALL hdlerr(NF_REDEF(nc_fileid), 'spher2car')

      CALL hdlerr(NF_INQ_DIMID &
	 (nc_fileid,'src_grid_size',dimid_src), 'spher2car')

      stat=NF_INQ_DIMID(nc_fileid,'dim_2',dim2_id)

      IF (stat .NE. NF_NOERR) THEN
         CALL hdlerr(NF_DEF_DIM &
              (nc_fileid,'dim_2',2,dim2_id), 'spher2car')
      ENDIF

      stat=NF_INQ_DIMID(nc_fileid,'dim_3',dim3_id)

      IF (stat .NE. NF_NOERR) THEN
         CALL hdlerr(NF_DEF_DIM &
              (nc_fileid,'dim_3',3,dim3_id), 'spher2car')
      ENDIF

      dim_id(1) = dimid_src
      dim_id(2) = dim3_id
      dim_id(3) = dim2_id

      CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Mat_src_sph2car',NF_DOUBLE,3,dim_id,var_id), 'spher2car')     

      CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,var_id, 'matrix',&
	 60,'Transformation matrix for source grid spheric to cartesian'),&
	 'spher2car')

      CALL hdlerr(NF_ENDDEF(nc_fileid), 'spher2car')

      CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,var_id,rda_trans),'spher2car')



    IF (nlogprt .GE. 2) THEN
	WRITE (UNIT = nulou,FMT = *) ' '
	WRITE (UNIT = nulou,FMT = *) &
	   '          --------- Leaving routine spher2car ---------'
	CALL FLUSH (nulou)
    ENDIF

  END SUBROUTINE spher2car
!
!-----------------------------------------------------------------------
! 
  SUBROUTINE car2spher(rda_grd_lon, rda_grd_lat, id_grd_size, rda_trans, nc_fileid)

    INTEGER (KIND=int_kind), INTENT(in) :: &
         id_grd_size, &                 ! grid size
         nc_fileid                      ! id of open netcdf remapping file
    
    REAL (KIND=dbl_kind), DIMENSION(id_grd_size),INTENT(in) :: &
         rda_grd_lon, &                 ! grid longitudes
         rda_grd_lat                    ! grid latitudes

! !RETURN VALUE:

    REAL (KIND=dbl_kind), DIMENSION(id_grd_size,3,3), INTENT(out) :: rda_trans

! Local variables

    REAL (KIND=dbl_kind), DIMENSION(id_grd_size) :: &
         rla_sin_lon, rla_cos_lon, &    ! sinus and cosinu for longitudes
         rla_sin_lat, rla_cos_lat       ! sinus and cosinu for latitudes
    
    INTEGER(KIND=int_kind) :: dimid_dst, dim3_id, var_id, stat
    INTEGER(KIND=int_kind), DIMENSION(3) :: dim_id
    
! !DESCRIPTION:
!  Finds the rotation matrix between a cartesien and a spheric referential.
!
  
    IF (nlogprt .GE. 2) THEN
	WRITE (UNIT = nulou,FMT = *) ' '
	WRITE (UNIT = nulou,FMT = *) '  Entering ROUTINE   - car2spher'
	WRITE (UNIT = nulou,FMT = *) ' '
        CALL FLUSH(nulou)
    ENDIF
 
    rla_sin_lon(:) = SIN(rda_grd_lon(:) * rp_deg2rad)
    rla_cos_lon(:) = COS(rda_grd_lon(:) * rp_deg2rad)
    rla_sin_lat(:) = SIN(rda_grd_lat(:) * rp_deg2rad)
    rla_cos_lat(:) = COS(rda_grd_lat(:) * rp_deg2rad)
    
    rda_trans(:,1,1) = -rla_sin_lon(:)
    rda_trans(:,1,2) =  rla_cos_lon(:)
    rda_trans(:,1,3) =  0.
    rda_trans(:,2,1) = -rla_sin_lat(:) * rla_cos_lon(:)
    rda_trans(:,2,2) = -rla_sin_lat(:) * rla_sin_lon(:)
    rda_trans(:,2,3) =  rla_cos_lat(:)
    rda_trans(:,3,1) =  rla_cos_lat(:) * rla_cos_lon(:)
    rda_trans(:,3,2) =  rla_cos_lat(:) * rla_sin_lon(:)
    rda_trans(:,3,3) =  rla_sin_lat(:)

!
!*Write matrix to remapping file
!

    CALL hdlerr(NF_REDEF(nc_fileid), 'car2spher')
    
    CALL hdlerr(NF_INQ_DIMID &
	 (nc_fileid,'dst_grid_size',dimid_dst), 'car2spher')
    
    stat=NF_INQ_DIMID(nc_fileid,'dim_3',dim3_id)
    
    IF (stat .NE. NF_NOERR) THEN
       CALL hdlerr(NF_DEF_DIM &
            (nc_fileid,'dim_3',3,dim3_id), 'car2spher')
    ENDIF
    
    dim_id(1) = dimid_dst
    dim_id(2) = dim3_id
    dim_id(3) = dim3_id
    
    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Mat_dst_car2sph',NF_DOUBLE,3,dim_id,var_id), 'car2spher')     
    
    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,var_id, 'matrix',&
	 60,'Transformation matrix for target grid cartesian to spheric'),&
	 'car2spher')
    
    CALL hdlerr(NF_ENDDEF(nc_fileid), 'car2spher')
    
    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,var_id,rda_trans),'car2spher')
    
    
    IF (nlogprt .GE. 2) THEN
	WRITE (UNIT = nulou,FMT = *) ' '
	WRITE (UNIT = nulou,FMT = *) &
	   '          --------- Leaving routine car2spher ---------'
	CALL FLUSH (nulou)
    ENDIF

  END SUBROUTINE car2spher



END MODULE rotations
