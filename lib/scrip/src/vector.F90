MODULE vector
!-----------------------------------------------------------------------
!
!** Description: 
!   ------------
!   This module contains subroutines needed for scriprmp_vector
!
!**  History:
!    --------
!    Created   02/2006   J. Ghattas
!
!
  USE kinds_mod 
  USE constants 
  USE mod_parameter
  USE grids
  USE mod_unitncdf
  
  IMPLICIT NONE

#include <netcdf.inc>


CONTAINS

!
!** -----------------------------------------------------------------------
!** -----------------------------------------------------------------------
! 

  SUBROUTINE calc_remap_matrix( &
       src_mask, dst_mask, &
       src_size, dst_size, &
       lon_srcA, lat_srcA,  lon_dst,  lat_dst, &
       nlon_src, nlat_src, nlon_dst, nlat_dst, &
       grd_name_srcA, grd_name_dst, &
       map_method,   n_opt, cdgrdtyp, &
       id_sper,      cd_sper,      &
       id_tper,      cd_tper,      &
       rst_type,                   &
       n_srch_bins,  crmpfile,     &
       cmapping,     lextrapdone,  &
       rl_varmul,    id_scripvoi   )
    
!
!** Description :
!   -------------
! Calcutation of remapping matrix for interpolation between qource grid grd_name_src 
! and target grid grd_name_dst. Remapping matrix written to file crmpfile which must 
! not exist when the this subroutine is called.
!
    IMPLICIT NONE
!
!** Input variables
!   ---------------
    INTEGER(KIND=int_kind), INTENT(in) :: src_size, dst_size, &
         nlon_src, nlat_src, nlon_dst, nlat_dst, &
         id_sper, id_tper, n_srch_bins, id_scripvoi
    INTEGER(KIND=int_kind), DIMENSION(src_size), INTENT(in) :: src_mask
    INTEGER(KIND=int_kind), DIMENSION(dst_size), INTENT(in) :: dst_mask
    REAL(KIND=real_kind), DIMENSION(src_size), INTENT(in) :: lon_srcA,  lat_srcA
    REAL(KIND=real_kind), DIMENSION(dst_size), INTENT(in) :: lon_dst,  lat_dst
    REAL(KIND=real_kind), INTENT(in) :: rl_varmul
    CHARACTER(LEN=8), INTENT(in) :: grd_name_srcA, grd_name_dst, n_opt, rst_type
    CHARACTER(LEN=8), INTENT(in) :: map_method, cdgrdtyp, cd_sper, cd_tper
    CHARACTER(char_len), INTENT(in) :: crmpfile, cmapping
    LOGICAL, INTENT(in) :: lextrapdone
 
!
!** Local variables
!   ---------------
    INTEGER(KIND=int_kind), DIMENSION(src_size) :: sou_mask
    INTEGER(KIND=int_kind), DIMENSION(dst_size) :: tgt_mask
    CHARACTER(LEN=8) :: normalize_opt
    LOGICAL :: lfracnnei
    INTEGER(KIND=int_kind) :: ncrn, src_rank, dst_rank, src_dims(2), dst_dims(2)
    REAL (KIND=real_kind), DIMENSION(:,:), ALLOCATABLE :: src_corner_lon, src_corner_lat, &
         dst_corner_lon, dst_corner_lat
!
    CHARACTER(LEN=8) :: cl_tgt     ! indicates target grid 
!
!** ----------------------------------------------------------------------------
!
!
    IF (nlogprt .GE. 2) THEN
        WRITE (UNIT = nulou,FMT = *)' '
        WRITE (UNIT = nulou,FMT = *)'Entering routine calc_remap_matrix'
        CALL FLUSH(nulou)
    ENDIF
!
!  
!** -- Setting of the mask for the source and the target grid
!       

    sou_mask(:) = 0
    tgt_mask(:) = 0
    WHERE (src_mask .eq. 1)
       sou_mask = 0
    END WHERE
    WHERE (src_mask .eq. 0)
       sou_mask = 1
    END WHERE
    
    WHERE (dst_mask .eq. 1)
       tgt_mask = 0
    END WHERE
    WHERE (dst_mask .eq. 0)
       tgt_mask = 1
    END WHERE
    
!
!** -- Calculate SCRIP remapping matrix : source to destination grid
!
    normalize_opt = n_opt
    SELECT CASE (normalize_opt)
    CASE ('FRACNNEI')
       normalize_opt = 'FRACAREA'
       lfracnnei = .true.
    END SELECT
    
    IF (nlogprt .GE. 2) THEN 
       WRITE (UNIT = nulou,FMT = *) & 
            ' Calculation of SCRIP remapping matrix: method = ', &
            map_method
       WRITE (UNIT = nulou,FMT = *) ' '
       call flush(nulou)
    ENDIF
!
!** -- Get grid cell corners for conservative remapping
!
    ncrn = 4.
    ALLOCATE(src_corner_lon(ncrn,src_size), &
       src_corner_lat(ncrn,src_size),&
       dst_corner_lon(ncrn,dst_size),&
       dst_corner_lat(ncrn,dst_size))
    src_corner_lon(:,:)= 0.0
    src_corner_lat(:,:)= 0.0
    dst_corner_lon(:,:)= 0.0
    dst_corner_lat(:,:)= 0.0

    IF (map_method == 'CONSERV') THEN	  
       CALL corners(nlon_src, nlat_src, ncrn,  &
            lon_srcA, lat_srcA,                    &
            grd_name_srcA, cdgrdtyp, id_sper, cd_sper, &
            src_corner_lon, src_corner_lat)
       cl_tgt='TARGETGR' 
       CALL corners(nlon_dst, nlat_dst, ncrn,    &
            lon_dst, lat_dst,                      &
            grd_name_dst, cl_tgt, id_tper, cd_tper,  &
            dst_corner_lon, dst_corner_lat)
    ENDIF
!
!** -- Initialization of grid arrays for SCRIP
!
    src_dims(1) = nlon_src
    src_dims(2) = nlat_src
    dst_dims(1) = nlon_dst
    dst_dims(2) = nlat_dst
    src_rank    = 2
    dst_rank    = 2

    CALL grid_init(map_method, rst_type, n_srch_bins,&
       src_size, dst_size, src_dims(:), dst_dims(:),&
       src_rank, dst_rank, ncrn, ncrn,&
       sou_mask(:), tgt_mask(:), grd_name_srcA, grd_name_dst,&
       lat_srcA(:), lon_srcA(:), lat_dst(:), lon_dst(:),&
       src_corner_lat(:,:), src_corner_lon(:,:),&
       dst_corner_lat(:,:), dst_corner_lon(:,:))
!        
!** -- Calculation of weights and addresses using SCRIP-library
!
    CALL scrip(crmpfile, cmapping, map_method, normalize_opt, &
         lextrapdone, rl_varmul, id_scripvoi)
    
    IF (map_method == 'CONSERV') THEN
       DEALLOCATE(src_corner_lon, src_corner_lat,&
            dst_corner_lon, dst_corner_lat)
    ENDIF
        
!
    IF (nlogprt .GE. 2) THEN
        WRITE (UNIT = nulou,FMT = *)' '
        WRITE (UNIT = nulou,FMT = *)'Leaving routine calc_remap_matrix'
        CALL FLUSH(nulou)
    ENDIF
!
  END SUBROUTINE calc_remap_matrix

!
!** -----------------------------------------------------------------------
!** -----------------------------------------------------------------------
! 

  SUBROUTINE remap_vector_comps (dst_array, src_array, &
       nbr_comp, nc_fileid, &
       map_method, cdgrdtyp, order, &
       src_mask, &
       id_sper, cd_sper, &
       nlon_src, nlat_src, src_lon, src_lat, src_size, dst_size)

!
!** Description :
!   -------------
!   Interpolation of  2 or 3 components in src_array(:,:) on source grid using 
!   remapping matrix allready existing in remappingile. Resulting fields in 
!   dst_array on target grid grd_name_dst. Remapping matrix is read from file 
!   already opend with id nc_fileid.
!
    IMPLICIT NONE
!
!** Input variables
!   ---------------
    INTEGER (KIND=int_kind), INTENT(in) :: nbr_comp, nc_fileid, id_sper, nlon_src, &
         nlat_src, src_size, dst_size
    REAL (KIND=real_kind), DIMENSION (src_size, nbr_comp), INTENT(in) :: src_array
    REAL (KIND=real_kind), DIMENSION (src_size), INTENT(in) :: src_lon, src_lat
    CHARACTER (LEN=8), INTENT(in) :: map_method, cdgrdtyp, order, cd_sper
    INTEGER (KIND=int_kind), DIMENSION (src_size), INTENT(in) :: src_mask
!
!** Output variables
!   ----------------
    REAL (KIND=real_kind), DIMENSION (dst_size, nbr_comp), INTENT(out) :: dst_array
!
!** Local variables
!   ---------------
    INTEGER (KIND=int_kind) :: num_wgts, dimid1, num_links, ib, n, varid
    
    INTEGER (KIND=int_kind), DIMENSION (:), ALLOCATABLE :: src_addr, dst_addr
    INTEGER (KIND=int_kind), DIMENSION (src_size) :: sou_mask
    
    REAL (KIND=real_kind), DIMENSION (:,:), ALLOCATABLE :: weights
    
    REAL (KIND=real_kind), DIMENSION (:), ALLOCATABLE :: gradient_lat, gradient_lon, &
         gradient_i, gradient_j, gradient_ij
    
    REAL (KIND=real_kind), DIMENSION (dst_size) :: dst_area, dst_frac
    
    CHARACTER(LEN=11) :: &
         csrcadd, &            ! string for source grid addresses
         cdstadd               ! string for destination grid addresses
    CHARACTER(LEN=13) :: &
         cdstare, &            ! string for destination grid area
         cdstfra               ! string for destination grid frac
    CHARACTER(LEN=12) :: &
         cweight               ! string for weights
    REAL (KIND=real_kind), DIMENSION (dst_size, nbr_comp) :: weightot
    LOGICAL  ::   ll_weightot
    
!
!** ----------------------------------------------------------------------------
!
    IF (nlogprt .GE. 2) THEN
        WRITE (UNIT = nulou,FMT = *)' '
        WRITE (UNIT = nulou,FMT = *)'Entering routine remap_vector_comps'
        CALL FLUSH(nulou)
    ENDIF
!  
!* Read weights and addresses
! 
!** Get number of weights
!
    SELECT CASE (map_method)
    CASE ('CONSERV')          ! conservative remapping
       num_wgts = 3.
    CASE ('BILINEAR')         ! bilinear remapping
       num_wgts = 1.
    CASE ('BICUBIC')          ! bicubic remapping   
       IF (cdgrdtyp .eq. 'LR') THEN ! logically rectangular
          num_wgts = 4.
       ELSE                  ! reduced  
          num_wgts=1.
       ENDIF
    CASE ('DISTWGT')          ! distance weighted averaging
       num_wgts = 1.
    CASE ('GAUSWGT')          ! distance gaussian weighted averaging
       num_wgts = 1.
    END SELECT
  
!  
!** Setting of the mask for the source grid
!       
    sou_mask(:) = 0

    WHERE (src_mask .eq. 1)
       sou_mask = 0
    END WHERE
    WHERE (src_mask .eq. 0)
       sou_mask = 1
    END WHERE

!  
!** Character strings of weights and addresses
!
    csrcadd = 'src_address'
    cdstadd = 'dst_address'
    cweight = 'remap_matrix'
    cdstare = 'dst_grid_area'
    cdstfra = 'dst_grid_frac'
!
!** Get matrix size
! 
    CALL hdlerr(NF_INQ_DIMID &
         (nc_fileid, 'num_links', dimid1), 'scriprmp_vector')
    CALL hdlerr(NF_INQ_DIMLEN &
         (nc_fileid, dimid1, num_links), 'scriprmp_vector')
!
!** Array allocation
!
    ALLOCATE (src_addr(num_links), dst_addr(num_links), &
         weights(num_wgts,num_links))
!
!** Read source grid addresses and weights
!
    CALL hdlerr(NF_INQ_VARID &
         (nc_fileid, csrcadd, varid), 'scriprmp_vector')
    CALL hdlerr(NF_GET_VAR_INT &
         (nc_fileid, varid, src_addr), 'scriprmp_vector')
    CALL hdlerr(NF_INQ_VARID &
         (nc_fileid, cdstadd, varid), 'scriprmp_vector')
    CALL hdlerr(NF_GET_VAR_INT &
         (nc_fileid, varid, dst_addr), 'scriprmp_vector')
    CALL hdlerr(NF_INQ_VARID &
         (nc_fileid, cweight, varid), 'scriprmp_vector')
    IF (ll_single) THEN
       CALL hdlerr(NF_GET_VAR_REAL &
            (nc_fileid, varid, weights), 'scriprmp_vector')
    ELSE 
       CALL hdlerr(NF_GET_VAR_DOUBLE &
            (nc_fileid, varid, weights), 'scriprmp_vector') 
    ENDIF
    CALL hdlerr(NF_INQ_VARID &
         (nc_fileid, cdstare, varid), 'scriprmp_vector')
    IF (ll_single) THEN
       CALL hdlerr(NF_GET_VAR_REAL &
            (nc_fileid, varid, dst_area), 'scriprmp_vector')
    ELSE
       CALL hdlerr(NF_GET_VAR_DOUBLE &
            (nc_fileid, varid, dst_area), 'scriprmp_vector') 
    ENDIF
    CALL hdlerr(NF_INQ_VARID &
         (nc_fileid, cdstfra, varid), 'scriprmp_vector')
    IF (ll_single) THEN
       CALL hdlerr(NF_GET_VAR_REAL &
            (nc_fileid, varid, dst_frac), 'scriprmp_vector') 
    ELSE
       CALL hdlerr(NF_GET_VAR_DOUBLE &
            (nc_fileid, varid, dst_frac), 'scriprmp_vector') 
    ENDIF

!
!** Do the matrix multiplication for the 2 or 3 components
!
    ll_weightot  = .false.
    weightot(:,:)  = 0.0
    dst_array(:,:) = 0.
  
    SELECT CASE (map_method)
     
    CASE ('CONSERV')     ! conservative remapping       
       SELECT CASE (order)	  
       CASE ('FIRST')        ! first order remapping 
          DO ib=1,nbr_comp
             DO n = 1, num_links
               IF (src_addr(n) .NE. 0) THEN
                dst_array(dst_addr(n),ib) = dst_array(dst_addr(n),ib)&
                     + weights(1,n) * src_array(src_addr(n),ib)
                weightot(dst_addr(n),ib) = weightot(dst_addr(n),ib) &
                         + weights(1,n)
               ENDIF
             END DO
          END DO
       CASE ('SECOND')       ! second order remapping (including gradients)
          IF (cdgrdtyp .ne. 'LR') THEN
             WRITE (UNIT = nulou,FMT = *) &
                  'Field gradient cannot be calculated'
             WRITE (UNIT = nulou,FMT = *) &
                  'by Oasis as grid is not logically rectangular'
             CALL HALTE('STOP in scriprmp (CONSERV)')
          ENDIF
          ALLOCATE(gradient_lat(src_size), gradient_lon(src_size))
          
          DO ib=1, nbr_comp
             CALL gradient(nlon_src, nlat_src, src_array(:,ib), &
                  sou_mask, src_lat, src_lon, id_sper, cd_sper, &
                  gradient_lat, gradient_lon)
             
             DO n = 1, num_links
               IF (src_addr(n) .NE. 0) THEN
                dst_array(dst_addr(n),ib) = dst_array(dst_addr(n),ib)&
                     + weights(1,n) * src_array(src_addr(n),ib) &
                     + weights(2,n) * gradient_lat(src_addr(n)) &
                     + weights(3,n) * gradient_lon(src_addr(n))
                weightot(dst_addr(n),ib) = weightot(dst_addr(n),ib) & 
                     + weights(1,n) + weights(2,n) + weights(3,n)
               ENDIF
             ENDDO
          END DO
          DEALLOCATE(gradient_lat, gradient_lon)	  
       END SELECT! order      
    CASE ('BILINEAR')        ! bilinear remapping 
       DO ib=1, nbr_comp
          DO n = 1, num_links
            IF (src_addr(n) .NE. 0) THEN
             dst_array(dst_addr(n),ib) = dst_array(dst_addr(n),ib) &
                  + weights(1,n) * src_array(src_addr(n),ib)
             weightot(dst_addr(n),ib) = weightot(dst_addr(n),ib) & 
                  + weights(1,n)
            ENDIF
          ENDDO
       END DO
    CASE ('BICUBIC')         ! bicubic remapping	
       SELECT CASE (cdgrdtyp) !
       CASE ('LR')          ! logically rectangular 	    
          ALLOCATE(gradient_i(src_size), gradient_j(src_size), &
               gradient_ij(src_size))
          
          DO ib=1, nbr_comp
             CALL gradient_bicubic(nlon_src,nlat_src,src_array(:,ib), &
                  sou_mask, src_lat, src_lon, id_sper, cd_sper, &
                  gradient_i, gradient_j, gradient_ij)
             
             DO n = 1, num_links
               IF (src_addr(n) .NE. 0) THEN
                dst_array(dst_addr(n),ib) = dst_array(dst_addr(n),ib) &
                     + weights(1,n) * src_array(src_addr(n),ib) &
                     + weights(2,n) * gradient_i(src_addr(n)) &
                     + weights(3,n) * gradient_j(src_addr(n)) &
                     + weights(4,n) * gradient_ij(src_addr(n))
                weightot(dst_addr(n),ib) = weightot(dst_addr(n),ib) &
                     + weights(1,n) + weights(2,n) + weights(3,n) + weights(4,n)
               ENDIF
             ENDDO
          END DO
          
          DEALLOCATE(gradient_i, gradient_j, gradient_ij)	  
       CASE ('D')           !reduced	  
          DO ib=1, nbr_comp 
             DO n = 1, num_links
               IF (src_addr(n) .NE. 0) THEN
                dst_array(dst_addr(n),ib) =  dst_array(dst_addr(n),ib) &
                     + weights(1,n) * src_array(src_addr(n),ib)
                weightot(dst_addr(n),ib) = weightot(dst_addr(n),ib) & 
                     + weights(1,n)
               ENDIF
             ENDDO
          ENDDO
       END SELECT
    CASE ('DISTWGT')         ! distance weighted average
       DO ib=1, nbr_comp
          DO n = 1, num_links
            IF (src_addr(n) .NE. 0) THEN
             dst_array(dst_addr(n),ib) = dst_array(dst_addr(n),ib) &
                  + weights(1,n) * src_array(src_addr(n),ib)
             weightot(dst_addr(n),ib) = weightot(dst_addr(n),ib) &
                  + weights(1,n)
            ENDIF
          ENDDO
       ENDDO
    CASE ('GAUSWGT')          ! distance gaussian weighted average
       DO ib=1, nbr_comp
          DO n = 1, num_links
            IF (src_addr(n) .NE. 0) THEN
             dst_array(dst_addr(n),ib) = dst_array(dst_addr(n),ib) &
                  + weights(1,n) * src_array(src_addr(n),ib)
             weightot(dst_addr(n),ib) = weightot(dst_addr(n),ib) &
                  + weights(1,n) 
            ENDIF
          ENDDO
       ENDDO
    END SELECT  ! remapping method
   
    IF (ll_weightot) THEN
        DO ib=1, nbr_comp
          DO n = 1, dst_size
            IF (weightot(n,ib) .LT. EPSILON(1.)) dst_array(n,ib) = 1.0E+20
          ENDDO
        ENDDO
    ENDIF
    
    DEALLOCATE (src_addr, dst_addr, weights)
    
!
    IF (nlogprt .GE. 2) THEN
        WRITE (UNIT = nulou,FMT = *)' '
        WRITE (UNIT = nulou,FMT = *)'Leaving routine remap_vector_comps'
        CALL FLUSH(nulou)
    ENDIF
! 
  END SUBROUTINE remap_vector_comps
 
!
!** -----------------------------------------------------------------------
!** -----------------------------------------------------------------------
! 

  SUBROUTINE check_points_at_poles (arrayA, arrayB, latA, latB, grd_nameA, &
       grd_nameB, dst_size)
!
!** Description :
!   -------------
!   Test if there are points on the latitudes 90deg north and 90deg south.
!   If there are points on these latitudes, the averege for these points 
!   in arrayA and arrayB are calculated and distributed. arrayA and arrayB
!   are modified if necessary.
!   
    IMPLICIT NONE
!
!** Input variables
!   ----------------
    INTEGER(KIND=int_kind), INTENT(in) :: dst_size
    REAL (KIND=real_kind), DIMENSION(dst_size), INTENT(in):: latA, latB
    CHARACTER(LEN=8), INTENT(in) :: grd_nameA, grd_nameB
!
!** OutInput variables
!   ------------------
    REAL (KIND=real_kind), DIMENSION(dst_size), INTENT(inout):: &
         arrayA, &  ! VECTOR_I field on target grid dstA
         arrayB     ! VECTOR_J field on target grid dstB
!
!** Local variables
!   ----------------
    INTEGER(KIND=int_kind) :: nbr_N, nbr_S, n
    REAL(kind=real_kind) :: ave_N, ave_S, latpolN, latpolS
!
!** ----------------------------------------------------------------------------
!
    IF (nlogprt .GE. 2) THEN
        WRITE (UNIT = nulou,FMT = *)' '
        WRITE (UNIT = nulou,FMT = *)'Entering routine check_points_at_poles'
        CALL FLUSH(nulou)
    ENDIF
!     
    latpolN = pi*half
    latpolS = -pi*half
    nbr_N = 0
    nbr_S = 0
    ave_N = 0.0
    ave_S = 0.0
    
    DO n = 1, dst_size
       IF (latA(n) == latpolN) THEN
          nbr_N = nbr_N + 1
          ave_N= ave_N + arrayA(n)
       ELSE IF (latA(n) == latpolS) THEN
          nbr_S = nbr_S + 1
          ave_S= ave_S + arrayA(n)
       END IF
    END DO
    
    IF (nbr_N .NE. 0) THEN
       ave_N = ave_N/nbr_N
       WHERE (latA == latpolN)
          arrayA = ave_N
       END WHERE
    END IF
    
    IF (nbr_S .NE. 0) THEN
       ave_S = ave_S/nbr_S
       WHERE (latA .EQ. latpolS)
          arrayA = ave_S
       END WHERE
    END IF
!
    IF (nlogprt .GE. 2) THEN    
        WRITE (UNIT = nulou,FMT = *) ' '
        WRITE (UNIT = nulou,FMT = *) 'For target grid : ', grd_nameA
        WRITE (UNIT = nulou,FMT = *) nbr_N,' points at the north pole '
        WRITE (UNIT = nulou,FMT = *) nbr_S,' points at the south pole '
        WRITE (UNIT = nulou,FMT = *) ' '
        IF (nbr_N/=0 .OR. nbr_S/=0) THEN
            WRITE (UNIT = nulou,FMT = *) 'Average of field component I at north pole : ', ave_N
            WRITE (UNIT = nulou,FMT = *) 'Average of field component I at south pole : ', ave_S
        ENDIF
        CALL FLUSH (nulou)
    ENDIF
!     
!** The same calculation for the second target grid 
!
    nbr_N = 0
    nbr_S = 0
    ave_N = 0.0
    ave_S = 0.0
  
    DO n = 1, dst_size
       IF (latB(n) == latpolN) THEN
          nbr_N = nbr_N + 1
          ave_N= ave_N + arrayB(n)
       ELSE IF (latB(n) == latpolS) THEN
          nbr_S = nbr_S + 1
          ave_S= ave_S + arrayB(n)
       END IF
    END DO
    
    IF (nbr_N .NE. 0) THEN
       ave_N = ave_N/nbr_N
       WHERE (latB == latpolN)
          arrayB = ave_N
       END WHERE
    END IF
    
    IF (nbr_S .NE. 0) THEN
       ave_S = ave_S/nbr_S
       WHERE (latB .EQ. latpolS)
          arrayB = ave_S
       END WHERE
    END IF
!
    IF (nlogprt .GE. 2) THEN    
        IF (grd_nameA .NE. grd_nameB) THEN
            WRITE (UNIT = nulou,FMT = *) ' '
            WRITE (UNIT = nulou,FMT = *) 'For target grid : ', grd_nameB
            WRITE (UNIT = nulou,FMT = *) nbr_N,' points at the north pole '
            WRITE (UNIT = nulou,FMT = *) nbr_S,' points at the south pole '
            WRITE (UNIT = nulou,FMT = *) ' '
        ENDIF
        IF (nbr_N/=0 .OR. nbr_S/=0) THEN
            WRITE (UNIT = nulou,FMT = *) 'Average of field component J at north pole : ', ave_N
            WRITE (UNIT = nulou,FMT = *) 'Average of field component J at south pole : ', ave_S
        ENDIF
!
        WRITE (UNIT = nulou,FMT = *)' '
        WRITE (UNIT = nulou,FMT = *)'Leaving routine check_points_at_poles'
        CALL FLUSH(nulou)
    ENDIF
!         
  END SUBROUTINE check_points_at_poles

!
!** -----------------------------------------------------------------------
!** -----------------------------------------------------------------------
! 

  SUBROUTINE write_src_array_spheric(array, array_size, nlon, nlat, &
       grd_name_src, grd_name_dst)
!
!** Description
!   -----------
!   This routine will write the 2 spherical components given in 'array' in 
!   netcdf file vector_debug.nc. The file will first be created.
!
    IMPLICIT NONE
!
!** Input variables
!  
    INTEGER (KIND=int_kind), INTENT(in) :: array_size, nlon, nlat
    REAL (KIND=real_kind), DIMENSION(array_size,2), INTENT(in) :: array
    CHARACTER(LEN=8), INTENT(in) :: grd_name_src, grd_name_dst
!
!** Local variables
!
    INTEGER(KIND=int_kind) :: dim_id(2), nc_fileid, dimI_id, dimJ_id, stat, &
         varI_id, varJ_id, icount, kcount
    INTEGER(KIND=int_kind) :: ilenstr
    CHARACTER(char_len) :: text, filename
!
!** --------------------------------------------------------------------------
!
    IF (nlogprt .GE. 2) THEN
        WRITE(nulou,*) '   '
        WRITE(nulou,*) ' Entering routine write_src_array_spheric'
        call flush(nulou)
    ENDIF
!  
!
!** Create the file vector_debug.nc and define the dimensions
!

    icount = ilenstr(grd_name_src,jpeight)
    kcount = ilenstr(grd_name_dst, jpeight)
    filename='vector_debug_'//grd_name_src(1:icount)//'_to_'//grd_name_dst(1:kcount)//'.nc'
    CALL hdlerr(NF_CREATE(filename, 0, nc_fileid), 'write_src_array_spheric')
!
    IF (nlogprt .GE. 2) THEN
        WRITE (UNIT = nulou,FMT = *) ' '
        WRITE (UNIT = nulou,FMT = *) filename,' is created containg fields in spheric and'
        WRITE (UNIT = nulou,FMT = *) ' eventually cartesian referentials'
        WRITE (UNIT = nulou,FMT = *) ' '
        CALL FLUSH(nulou)
    ENDIF
!    
    CALL hdlerr(NF_DEF_DIM &
         (nc_fileid,'src_grid_dim_I',nlon,dimI_id), 'write_src_array_spheric')
    
    CALL hdlerr(NF_DEF_DIM &
         (nc_fileid,'src_grid_dim_J',nlat,dimJ_id), 'write_src_array_spheric')
    
    dim_id(1) = dimI_id
    dim_id(2) = dimJ_id
!    
!** Define variable for component I    
!
    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Comp_I_src_spheric',NF_DOUBLE,2,dim_id,varI_id), &
         'write_src_array_spheric')     

    text = 'Component I in spherical referential on source grid '//grd_name_src

    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,varI_id, 'array',&
	 LEN(text),text),'write_src_array_spheric')
!
!** Define variable for component J   
!
    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Comp_J_src_spheric',NF_DOUBLE,2,dim_id,varJ_id), &
         'write_src_array_spheric')     

    text = 'Component J in spherical referential on source grid '//grd_name_src

    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,varJ_id, 'array',&
	 LEN(text),text),'write_src_array_spheric')
    
    CALL hdlerr(NF_ENDDEF(nc_fileid), 'write_src_array_spheric')
    
!
!** Put the component I and J into file
!
    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,varI_id,array(:,1)),'write_src_array_spheric')

    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,varJ_id,array(:,2)),'write_src_array_spheric')

    CALL hdlerr(NF_CLOSE(nc_fileid),'write_src_array_spheric')
!
    IF (nlogprt .GE. 2) THEN
        WRITE(nulou,*) '   '
        WRITE(nulou,*) ' Entering routine write_src_array_spheric'
        call flush(nulou)
    ENDIF
!    
  END SUBROUTINE write_src_array_spheric

!
!** -----------------------------------------------------------------------
!** -----------------------------------------------------------------------
! 

  SUBROUTINE write_dst_array_spheric(array, array_size, nlon, nlat, &
       grd_name_src, grd_name_dst)
!
!** Description
!   -----------
!   This routine will write the 3 spherical components given in 'array' in 
!   netcdf file vector_debug_XXX_to_YYY.nc. The file has to existe(it is created by 
!   call to write_src_array_spheric).
! 
    IMPLICIT NONE
!  
!** Input variables
!   ---------------
    INTEGER (KIND=int_kind), INTENT(in) :: array_size, nlon, nlat
    REAL (KIND=real_kind), DIMENSION(array_size,3), INTENT(in) :: array
    CHARACTER(LEN=8), INTENT(in) :: grd_name_src, grd_name_dst
!
!** Local variables
!   ---------------
    INTEGER(KIND=int_kind) :: dim_id(2), nc_fileid, dimI_id, dimJ_id, &
         stat, varI_id, varJ_id, varK_id, icount, kcount
    INTEGER(KIND=int_kind) :: ilenstr
    CHARACTER(char_len) :: text, filename
!
!** -----------------------------------------------------------------------
!
    IF (nlogprt .GE. 2) THEN
        WRITE(nulou,*) '   '
        WRITE(nulou,*) ' Entering routine write_dst_array_spheric'
        call flush(nulou)
    ENDIF
!
!** Open the file vector_debug_XXX_to_YYY.nc and define the dimensions
!  
    icount = ilenstr(grd_name_src,jpeight)
    kcount = ilenstr(grd_name_dst, jpeight)
    filename='vector_debug_'//grd_name_src(1:icount)//'_to_'//grd_name_dst(1:kcount)//'.nc'
    CALL hdlerr(NF_OPEN(filename, NF_WRITE, nc_fileid), 'write_dst_array_spheric')
    
    CALL hdlerr(NF_REDEF(nc_fileid), 'write_dst_array_spheric')

    stat=NF_INQ_DIMID(nc_fileid,'dst_grid_dim_I',dimI_id)
    IF (stat .NE. NF_NOERR) CALL hdlerr(NF_DEF_DIM &
         (nc_fileid,'dst_grid_dim_I',nlon,dimI_id), 'write_dst_array_spheric')

    stat=NF_INQ_DIMID(nc_fileid,'dst_grid_dim_J',dimJ_id)
    IF (stat .NE. NF_NOERR) CALL hdlerr(NF_DEF_DIM &
         (nc_fileid,'dst_grid_dim_J',nlat,dimJ_id), 'write_dst_array_spheric')
    
    dim_id(1) = dimI_id
    dim_id(2) = dimJ_id
!    
!** Define variable for component I 
!
    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Comp_I_dst_spheric',NF_DOUBLE,2,dim_id,varI_id), &
         'write_dst_array_spheric')     

    text = 'Component I in spherical referential on target grid '//grd_name_dst

    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,varI_id, 'array',&
	 LEN(text),text),'write_dst_array_spheric')
!
!* Defining variable for component J   
!
    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Comp_J_dst_spheric',NF_DOUBLE,2,dim_id,varJ_id), &
         'write_dst_array_spheric')     

    text = 'Component J in spherical referential on target grid '//grd_name_dst
    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,varJ_id, 'array',&
	 LEN(text),text),'write_dst_array_spheric')
!    
!* Defining variable for component K
!
    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Comp_K_dst_spheric',NF_DOUBLE,2,dim_id,varK_id), &
         'write_dst_array_spheric')     

    text = 'Component K in spherical referential on target grid '//grd_name_dst
    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,varK_id, 'array',&
	 LEN(text),text),'write_dst_array_spheric')
    
    CALL hdlerr(NF_ENDDEF(nc_fileid), 'write_src_array_spheric')
!
!* Putting the fields into the file
!
    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,varI_id,array(:,1)),'write_dst_array_spheric')

    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,varJ_id,array(:,2)),'write_dst_array_spheric')

    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,varK_id,array(:,3)),'write_dst_array_spheric')

    CALL hdlerr(NF_CLOSE(nc_fileid),'write_src_array_spheric')
!
    IF (nlogprt .GE. 2) THEN
        WRITE(nulou,*) '   '
        WRITE(nulou,*) ' Leaving routine write_dst_array_spheric'
        call flush(nulou)
    ENDIF
!    
  END SUBROUTINE write_dst_array_spheric

!
!** -----------------------------------------------------------------------
!** -----------------------------------------------------------------------
! 

  SUBROUTINE  write_cartesian_components(src_array, dst_array, src_size, dst_size, &
       nlon_src, nlat_src, nlon_dst, nlat_dst, grd_name_src, grd_name_dst)
!
!** Description
!   -----------
! This routine will write the 3 cartesian components on soure and target grid in 
! netcdf file vector_debug_XXX_to_YYY.nc. The file has to existe(it is created by 
! call to write_src_array_spheric).
!
    IMPLICIT NONE
!  
!** Input variables
!   ---------------    
    INTEGER (KIND=int_kind), INTENT(in) :: src_size, dst_size, &
         nlon_src, nlat_src, nlon_dst, nlat_dst
    REAL (KIND=real_kind), DIMENSION(src_size,3), INTENT(in) :: src_array
    REAL (KIND=real_kind), DIMENSION(dst_size,3), INTENT(in) :: dst_array
    CHARACTER(LEN=8), INTENT(in) :: grd_name_src, grd_name_dst
!
!** Local variables
!   ---------------
!
    INTEGER(KIND=int_kind) :: dimsrc_id(2), dimdst_id(2), nc_fileid, &
         dimI_id, dimJ_id, stat, &
         varXsrc_id, varXdst_id, varYsrc_id, varYdst_id, varZsrc_id, varZdst_id, &
         icount, kcount
    INTEGER(KIND=int_kind) :: ilenstr
    CHARACTER(char_len) :: text, filename
!
!** ----------------------------------------------------------------------------
!
    IF (nlogprt .GE. 2) THEN
        WRITE(nulou,*) '   '
        WRITE(nulou,*) ' Entering routine write_cartesian_components'
        call flush(nulou)
    ENDIF
!
!** Open the file vector_debug_XXX_to_YYY.nc and define the dimensions
!
    
    icount = ilenstr(grd_name_src,jpeight)
    kcount = ilenstr(grd_name_dst, jpeight)
    filename='vector_debug_'//grd_name_src(1:icount)//'_to_'//grd_name_dst(1:kcount)//'.nc'
    CALL hdlerr(NF_OPEN(filename, NF_WRITE, nc_fileid), 'write_cartesian_components')
    
    CALL hdlerr(NF_REDEF(nc_fileid), 'write_cartesian_components')
!
!** Define dimension for src grid
!
    stat=NF_INQ_DIMID(nc_fileid,'src_grid_dim_I',dimI_id)
    IF (stat .NE. NF_NOERR) CALL hdlerr(NF_DEF_DIM &
         (nc_fileid,'src_grid_dim_I',nlon_src,dimI_id), 'write_cartesian_components')

    stat=NF_INQ_DIMID(nc_fileid,'src_grid_dim_J',dimJ_id)
    IF (stat .NE. NF_NOERR) CALL hdlerr(NF_DEF_DIM &
         (nc_fileid,'src_grid_dim_J',nlat_src,dimJ_id), 'write_cartesian_components')
    
    dimsrc_id(1) = dimI_id
    dimsrc_id(2) = dimJ_id
!
!** Define dimension for dst grid
!
    stat=NF_INQ_DIMID(nc_fileid,'dst_grid_dim_I',dimI_id)
    IF (stat .NE. NF_NOERR) CALL hdlerr(NF_DEF_DIM &
         (nc_fileid,'dst_grid_dim_I',nlon_dst,dimI_id), 'write_cartesian_components')

    stat=NF_INQ_DIMID(nc_fileid,'dst_grid_dim_J',dimJ_id)
    IF (stat .NE. NF_NOERR) CALL hdlerr(NF_DEF_DIM &
         (nc_fileid,'dst_grid_dim_J',nlat_dst,dimJ_id), 'write_cartesian_components')
    
    dimdst_id(1) = dimI_id
    dimdst_id(2) = dimJ_id

!    
!* Define src and dst variables for component X
!
    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Comp_X_cart_src',NF_DOUBLE,2,dimsrc_id, varXsrc_id), &
         'write_cartesian_components')     
    text = 'Component X in cartesian referential on source grid '//grd_name_src
    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,varXsrc_id, 'array',&
	 LEN(text),text),'write_cartesian_components')

    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Comp_X_cart_dst',NF_DOUBLE,2,dimdst_id, varXdst_id), &
         'write_cartesian_components')     
    text = 'Component X in cartesian referential on target grid '//grd_name_dst
    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,varXdst_id, 'array',&
	 LEN(text),text),'write_cartesian_components')


!    
!* Define src and dst variables for component Y
!
    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Comp_Y_cart_src',NF_DOUBLE,2,dimsrc_id, varYsrc_id), &
         'write_cartesian_components')     
    text = 'Component Y in cartesian referential on source grid '//grd_name_src
    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,varYsrc_id, 'array',&
	 LEN(text),text),'write_cartesian_components')

    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Comp_Y_cart_dst',NF_DOUBLE,2,dimdst_id, varYdst_id), &
         'write_cartesian_components')     
    text = 'Component Y in cartesian referential on target grid '//grd_name_dst
    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,varYdst_id, 'array',&
	 LEN(text),text),'write_cartesian_components')


!    
!* Define src and dst variables for component Z
!
    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Comp_Z_cart_src',NF_DOUBLE,2,dimsrc_id, varZsrc_id), &
         'write_cartesian_components')     
    text = 'Component Z in cartesian referential on source grid '//grd_name_src
    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,varZsrc_id, 'array',&
	 LEN(text),text),'write_cartesian_components')

    CALL hdlerr(NF_DEF_VAR &
	 (nc_fileid,'Comp_Z_cart_dst',NF_DOUBLE,2,dimdst_id, varZdst_id), &
         'write_cartesian_components')     
    text = 'Component Z in cartesian referential on target grid '//grd_name_dst
    CALL hdlerr(NF_PUT_ATT_TEXT(nc_fileid,varZdst_id, 'array',&
	 LEN(text),text),'write_cartesian_components')

    
    CALL hdlerr(NF_ENDDEF(nc_fileid), 'write_src_array_spheric')
    
!
!* Putting the fields into the file
!
    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,varXsrc_id,src_array(:,1)),'write_cartesian_components')
    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,varXdst_id,dst_array(:,1)),'write_cartesian_components')

    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,varYsrc_id,src_array(:,2)),'write_cartesian_components')
    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,varYdst_id,dst_array(:,2)),'write_cartesian_components')

    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,varZsrc_id,src_array(:,3)),'write_cartesian_components')
    CALL hdlerr(NF_PUT_VAR_DOUBLE &
	 (nc_fileid,varZdst_id,dst_array(:,3)),'write_cartesian_components')


    CALL hdlerr(NF_CLOSE(nc_fileid),'write_src_array_spheric')
!
    IF (nlogprt .GE. 2) THEN
        WRITE(nulou,*) '   '
        WRITE(nulou,*) ' Leaving routine write_cartesian_components'
        call flush(nulou)
    ENDIF
!
  END SUBROUTINE write_cartesian_components

!
!** -----------------------------------------------------------------------
!** -----------------------------------------------------------------------
! 
    
END MODULE vector
