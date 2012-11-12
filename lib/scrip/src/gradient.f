      subroutine gradient(NX1, NY1, src_array, sou_mask,
     $                    src_latitudes, src_longitudes,
     $                    id_per, cd_per,
     $                    grad_lat, grad_lon)

C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL ? *
C               * -------------     ------- *
C               *****************************
C
C**** *gradient*  - calculate gradients for conservative remapping
C
C     Purpose:
C     -------
C     Calculation of gradients in latitudinal and longitudinal direction.
C     In a first step the gradients in direction of source-grid rows  
C     and lines are calculated. Then they are rotated to longitudinal 
C     and latitudinal direction, using the scalar product.
C     This routine works for logically rectangular grids, only.
C
C**   Interface:
C     ---------
C       *CALL*  *gradient*(NX1, NY1, src_array, sou_mask, src_latitudes,
C     $          src_longitudes, grad_lat, grad_lon)
C
C     Input:
C     -----
C          NX1            : grid dimension in x-direction (integer)
C          NY1            : grid dimension in y-direction (integer)
C          src_array      : array on source grid (real 2D)
C          sou_mask       : source grid mask (integer 2D)
C          src_latitudes  : latitudes on source grid (real 2D)
C          src_longitudes : longitudes on source grid (real 2D)
C          id_per         : number of overlapping points for source grid
C          cd_per         : grip periodicity type
C 
C     Output:
C     ------
C          grad_lat       : gradient in latitudinal direction (real 2D)
C          grad_lon       : gradient in longitudinal direction (real 2D)
C
C     History:
C     -------
C       Version   Programmer     Date        Description
C       -------   ----------     ----        -----------  
C       2.5       V. Gayler      2001/09/20  created
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      USE constants
      USE kinds_mod
      USE mod_oasis_flush

      IMPLICIT NONE
!-----------------------------------------------------------------------
!     INTENT(IN)
!-----------------------------------------------------------------------
      INTEGER (int_kind), INTENT(IN) ::
     $    NX1, NY1,             ! source grid dimensions
     $    id_per                ! nbr of overlapping grid points

      CHARACTER*8, INTENT(IN) ::
     $    cd_per                ! grip periodicity type     

      REAL (kind = real_kind), DIMENSION(NX1,NY1), INTENT(IN) ::
     $    src_array             ! array on source grid

      INTEGER (int_kind), DIMENSION(NX1,NY1), INTENT(IN) ::
     $    sou_mask              ! source grid mask

      REAL (kind = real_kind), DIMENSION(NX1,NY1), INTENT(IN) ::
     $    src_latitudes,        ! source grid latitudes
     $    src_longitudes        ! source grid longitudes

!-----------------------------------------------------------------------
!     INTENT(OUT)
!-----------------------------------------------------------------------
      REAL (kind = real_kind), DIMENSION(NX1,NY1), INTENT(OUT) ::
     $     grad_lat,            ! gradient in latitudinal direction
     $     grad_lon             ! gradient in longitudinal direction

!-----------------------------------------------------------------------
!     LOCAL VARIABLES
!-----------------------------------------------------------------------
      INTEGER (int_kind) ::
     $     i, j,                ! looping indicees
     $     ip1, jp1, im1, jm1

      REAL (kind = real_kind) ::
     $     distance             ! distance in rad
     
      REAL (kind = real_kind) ::
     $     dVar_i, dVar_j,      ! difference of Var in i / j direction
     $     dlat_i, dlat_j,      ! difference in lat in i / j direction
     $     dlon_i, dlon_j,      ! difference in lon in i / j direction
     $     dist_i, dist_j,      ! distance in i / j direction
     $     grad_i, grad_j,      ! gradient in i / j direction
     $     ABSold, ABSnew, lat_factor

      REAL (kind = real_kind), DIMENSION(NX1,NY1) ::
     $     src_lon,             ! source grid longitudes [radiants]
     $     src_lat,             ! source grid latitudes [radiants]
     $     pi180                ! conversion factor: deg -> rad

      INTEGER (int_kind) ::  il_maskval= 0_int_kind

!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *)' '
          WRITE (UNIT = nulou,FMT = *)' Entering routine gradient   '
          WRITE (UNIT = nulou,FMT = *)' '
          CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!     Transformation from degree to radiant
!     -------------------------------------
      pi180 = 1.74532925199432957692e-2 ! =PI/180

      src_lon = src_longitudes * pi180
      src_lat = src_latitudes * pi180

!-----------------------------------------------------------------------

      DO i = 1, NX1

         DO j = 1, NY1
                   
            IF (sou_mask(i,j) /= il_maskval) THEN

               ip1 = i + 1
               im1 = i - 1
               IF (i == NX1) THEN
                   IF (cd_per == 'P') ip1 = 1 + id_per ! the 0-meridian
                   IF (cd_per == 'R') ip1 = NX1
               ENDIF
               IF (i == 1 )  THEN
                   IF (cd_per == 'P') im1 = NX1 - id_per
                   IF (cd_per == 'R') im1 = 1
               ENDIF
               jp1 = j + 1
               jm1 = j - 1
               IF (j == NY1) jp1 = NY1 ! treatment of the last..
               IF (j == 1 )  jm1 = 1   ! .. and the first grid-row

               IF (sou_mask(ip1,j) == il_maskval)  ip1 = i
               IF (sou_mask(im1,j) == il_maskval)  im1 = i
               IF (sou_mask(i,jp1) == il_maskval)  jp1 = j
               IF (sou_mask(i,jm1) == il_maskval)  jm1 = j          

!              difference between neighbouring datapoints
               dVar_i = src_array(ip1,j) - src_array(im1,j)
               dVar_j = src_array(i,jp1) - src_array(i,jm1)

!              difference in latitudes
               dlat_i = src_lat(ip1,j) - src_lat(im1,j)
               dlat_j = src_lat(i,jp1) - src_lat(i,jm1)

!              difference in longitudes
               dlon_i = src_lon(ip1,j) - src_lon(im1,j)
               IF (dlon_i >   PI)  dlon_i = dlon_i - PI2
               IF (dlon_i < (-PI)) dlon_i = dlon_i + PI2
               dlon_j = src_lon(i,jp1) - src_lon(i,jm1)
               IF (dlon_j >   PI)  dlon_j = dlon_j - PI2
               IF (dlon_j < (-PI)) dlon_j = dlon_j + PI2
               lat_factor = cos(src_lat(i,j))
               dlon_i = dlon_i * lat_factor
               dlon_j = dlon_j * lat_factor
 
!              distance
               dist_i = distance(src_lon(ip1,j), src_lat(ip1,j),
     $                           src_lon(im1,j), src_lat(im1,j))
               dist_j = distance(src_lon(i,jp1), src_lat(i,jp1),
     $                           src_lon(i,jm1), src_lat(i,jm1))

!              gradients: dVar / distance (= vector lenght)
               IF (dist_i /= 0.) THEN
                  grad_i = dVar_i / dist_i
               ELSE
                  grad_i = 0
               ENDIF
               IF (dist_j /= 0.) THEN
                  grad_j = dVar_j / dist_j
               ELSE
                  grad_j = 0
               ENDIF

!              projection by scalar product
!              ----------------------------
               grad_lon(i,j) = grad_i * dlon_i + grad_j * dlat_i
               grad_lat(i,j) = grad_i * dlon_j + grad_j * dlat_j

               if (dist_i /= 0) then
                  grad_lon(i,j) = grad_lon(i,j) / dist_i
               else
                  grad_lon(i,j) = 0
               endif
               if (dist_j /= 0) then
                  grad_lat(i,j) = grad_lat(i,j) / dist_j
               else
                  grad_lat(i,j) = 0.
               endif
              
!              correct skale
!              -------------
               ABSold = SQRT(grad_i**2 + grad_j**2)
               ABSnew = SQRT(grad_lon(i,j)**2 + grad_lat(i,j)**2)
               IF (ABSnew > 1.E-10) THEN
C                  grad_lon(i,j) = grad_lon(i,j)*ABSold/ABSnew
                  grad_lon(i,j) = grad_lon(i,j)
               ELSE
                  grad_lon(i,j) = 0.0
               ENDIF

!              test orthogonality
!              ------------------
               IF ((dlon_i*dlon_j+dlat_j*dlat_i) > 0.1) THEN
                  print*, 'ORTHOGONAL? ', i, j,
     $                    (dlon_i*dlon_j+dlat_j*dlat_i)
               ENDIF

            ELSE
           
               grad_lat(i,j) = 0.
               grad_lon(i,j) = 0. 
            
            ENDIF

         ENDDO
      ENDDO
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *)' '
          WRITE (UNIT = nulou,FMT = *)' Leaving routine gradient   '
          WRITE (UNIT = nulou,FMT = *)' '
          CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF 
      RETURN

      END SUBROUTINE gradient







