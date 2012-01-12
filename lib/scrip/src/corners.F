      SUBROUTINE corners(IX, IY, IC, src_lon, src_lat,
     $    grd_name, cdgrdtyp, id_per, cd_per,
     $    grid_corner_lon_2D, grid_corner_lat_2D)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL ? *
C               * -------------     ------- *
C               *****************************
C
C**** *corners*  - read or calculate grid cell corners
C
C     Purpose:
C     -------
C     Read grid cell corners from grids file, if awailable.
C     If not, calculate them from grid center longitudes and latitudes.
C     This works for logically rectangular grids, only.
C
C**   Interface:
C     ---------
C       *CALL*  *corners*(IX, IY, IC, grid_center_lon, grid_center_lat,
C          grd_name, cdgrdtyp, grid_corner_lon_2D, grid_corner_lat_2D)
C
C     Input:
C     -----
C          IX              : number of longitudes
C          IY              : number of latitudes
C          IC              : number of corners (=4)
C          grid_center_lon : grid center longitudes
C          grid_center_lat : grid center latitudes
C          grd_name        : string to identify model grid
C          cdgrdtyp        : string to identify model grid TYPE
C          id_per          : number of overlapping grid points
C          cp_per          : grid periodicity type (P or R)
C
C     Output:
C     ------
C          grid_corner_lon_2D : grid corner longitudes (real 2D)
C          grid_corner_lat_2D : grid corner latitudes  (real 2D)
C
C     History:
C     -------
C       Version   Programmer     Date        Description
C       -------   ----------     ----        -----------  
C       2.5       V. Gayler      2001/09/20  created
C       2.5       D. Declat      2002/06/27  MODULE modif.
C                 J. Ghattas     2006/02/10  changed src_lon/lat to intent(in), 
C                                            added case for ymean==0.
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Modules --------------------------------
C
      USE kinds_mod
      USE mod_parameter
      USE mod_unitncdf
      USE mod_label
      USE mod_printing
      USE mod_unit
C
C* ---------------------------- Implicit --------------------------------
C
      IMPLICIT NONE
C
C* ---------------------------- Include files ---------------------------
C
#include <netcdf.inc>
C
C* ---------------------------- Intent In -------------------------------
C
      INTEGER (kind=int_kind), INTENT(IN) ::
     $     IX,                  ! number of longitudes
     $     IY,                  ! number of latitudes
     $     IC,                  ! number of grid cell corners
     $     id_per               ! number of overlapping grid points
C
      REAL (kind=dbl_kind), INTENT(IN) ::
     $     src_lon(IX,IY), ! grid center longitudes
     $     src_lat(IX,IY)  ! grid center latitudes
C
      CHARACTER*8, INTENT(IN) ::
     $     grd_name,             ! string to specify model grid
     $     cdgrdtyp,             ! string to identify model grid type 
     $     cd_per                ! grip periodicity type
C
C* ---------------------------- Intent Out -------------------------------
C
      REAL (kind=dbl_kind), INTENT(OUT) :: 
     $     grid_corner_lon_2D(IC,IX*IY), ! grid corner longitudes
     $     grid_corner_lat_2D(IC,IX*IY)  ! grid corner latitudes
C
C* ---------------------------- Local declarations ----------------------
C   
      REAL (kind=dbl_kind) ::
     $     grid_center_lon(IX,IY), ! grid center longitudes
     $     grid_center_lat(IX,IY) ! grid center latitudes
C
      INTEGER (kind=int_kind) ::
     $     i, j, c, ij,         ! looping indicees: lon, lat, corner
     $     ip1, im1, jp1, jm1,  !
     $     c1, c2, c3, c4,
     $     icount,               ! number of characters in string
     $     ilenstr              ! external function
C
      REAL (kind=dbl_kind)  :: 
     $     aux(IC,IX,IY),
     $     grid_corner_lon(IX,IY,IC), ! grid corner longitudes (3D)
     $     grid_corner_lat(IX,IY,IC), ! grid corner latitudes  (3D)
     $     lon1, lon2, lon3, lon4, ! center lon. of surrounding a corner
     $     lat1, lat2, lat3, lat4, ! center lat. of surrounding a corner
     $     lon21, lon12,        !
     $     x1, x2, x3, x4,      ! transformed to x/y/z coordinates
     $     y1, y2, y3, y4,      !      ''         ''       ''
     $     z1, z2, z3, z4,      !      ''         ''       ''
     $     vp,                  ! vector product
     $     xmean, ymean, zmean, ! mean values   
     $     radius,              ! vector lenght
     $     zconva, zconvb,      ! conversion factors
     $     PI                   ! pi
C
      CHARACTER*8 ::
     $     ccrnlon,             ! string: corner longitude
     $     ccrnlat,             !   ''    corner latitude
     $     clon                 !   ''    longitude
C
C* -- netCDF-declarations
      INTEGER (kind=int_kind) ::
     $     stat,                ! netCDF status
     $     start(3), count(3),  ! start and count vector for NF_PUT_VARA
     $     varid,               ! variable id
     $     idcrnlon,             ! variable id: corner longitudes
     $     idcrnlat,            ! variable id: corner latitudes
     $     idlon,               ! variable id: center longitudes
     $     vtype,               ! variable type
     $     ndims,               ! number of dimensions
     $     vardims(3),          ! corner array dimensions
     $     londims(2),          ! dimensions of longitude array
     $     crndim               ! corner dimension
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '    Entering ROUTINE corners  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ***************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $         ' Get grid cell corners for grid: ', grd_name
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL FLUSH(nulou)
      ENDIF

C
C
C Coping longitudes and latitudes to local variables
C
      grid_center_lon(:,:) = src_lon(:,:)
      grid_center_lat(:,:) = src_lat(:,:)
C
C*    2. Look for corners in grids file
C*       ------------------------------
C
C* -- get locator string
C
      icount = ilenstr(grd_name,jpeight)
      ccrnlon = grd_name(1:icount)//crnlonsuf
      ccrnlat = grd_name(1:icount)//crnlatsuf
C
      start = (/1,1,1/)
      count = (/IX,IY,IC/)
C
      IF (lncdfgrd) THEN
          CALL hdlerr
     $        (NF_OPEN(cgrdnam//'.nc',NF_WRITE,nc_grdid),'corners')
          stat = NF_INQ_VARID(nc_grdid, ccrnlon, varid)
          IF (stat .lt. 0) THEN
              IF (cdgrdtyp .eq. 'TARGETGR') THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $            'File grids.nc must contains corner information'
                  WRITE (UNIT = nulou,FMT = *) 
     $            'for target grids. If needed, do another remapping'
                  WRITE (UNIT = nulou,FMT = *) 
     $            'in which the current target grid is a source grid.'
                  CALL HALTE('STOP in corners.f')
              ELSE IF (cdgrdtyp .ne. 'LR') THEN
                  WRITE (UNIT = nulou,FMT = *) 
     $            'File grids.nc contains no corner information,'
                  WRITE (UNIT = nulou,FMT = *) 
     $            'and grid is not logically rectangular.'
                  CALL HALTE('STOP in corners.f')
              ELSE
                  WRITE (UNIT = nulou,FMT = *) 
     $   'WARNING: File grids.nc contains no source corner information'
                  WRITE (UNIT = nulou,FMT = *) 
     $           'OASIS will calculate it with basic algorithm' 
                  WRITE (UNIT = nulou,FMT = *) 
     $           'See corner.f'  
              ENDIF
          ENDIF
      ELSE
          IF (cdgrdtyp .eq. 'TARGETGR') THEN
              WRITE (UNIT = nulou,FMT = *) 
     $            'File grids.nc must exist with corner information'
              WRITE (UNIT = nulou,FMT = *) 
     $            'for target grids. If needed, do another remapping'
              WRITE (UNIT = nulou,FMT = *) 
     $            'in which the current target grid is a source grid.'
              CALL HALTE('STOP in corners.f')
          ELSE IF (cdgrdtyp .ne. 'LR') THEN
              WRITE (UNIT = nulou,FMT = *) 
     $            'File grids.nc does not exist'
              WRITE (UNIT = nulou,FMT = *) 
     $            'and grid is not logically rectangular.'
              CALL HALTE('STOP in corners.f')
          ELSE
              stat = -1
              WRITE (UNIT = nulou,FMT = *) 
     $            'WARNING: as NetCDF FILE grids.nc does not exists,'
              WRITE (UNIT = nulou,FMT = *) 
     $           'OASIS will calculate corner information' 
              WRITE (UNIT = nulou,FMT = *) 
     $           'with basic algorithm. See corner.f'
          ENDIF
      ENDIF
C
      IF (stat == 0) THEN
C*    -- read corner longitudes
          CALL hdlerr
     $        (NF_INQ_VARTYPE(nc_grdid, varid, vtype), 'corners')
         IF (vtype .eq. NF_FLOAT) THEN
            CALL hdlerr(NF_GET_VARA_REAL (nc_grdid, varid, 
     $           start, count, grid_corner_lon),'corners')
         ELSE IF (vtype .eq. NF_DOUBLE) THEN
            CALL hdlerr(NF_GET_VARA_DOUBLE (nc_grdid, varid, 
     $           start, count, grid_corner_lon),'corners')
         ELSE
            CALL prcout 
     $           ('Problem with type of corner array =', ccrnlon, 1) 
            CALL HALTE('STOP in corners')
         ENDIF
C*    -- read corner latitudes
         ccrnlat = grd_name(1:icount)//crnlatsuf
         CALL hdlerr(NF_INQ_VARID(nc_grdid, ccrnlat, varid), 'corners')
         CALL hdlerr
     $        (NF_INQ_VARTYPE(nc_grdid, varid, vtype), 'corners')
         IF (vtype .eq. NF_FLOAT) THEN
            CALL hdlerr(NF_GET_VARA_REAL (nc_grdid, varid, 
     $           start, count, grid_corner_lat),'corners')
         ELSE IF (vtype .eq. NF_DOUBLE) THEN
            CALL hdlerr(NF_GET_VARA_DOUBLE (nc_grdid, varid, 
     $           start, count, grid_corner_lat),'corners')
         ELSE
            CALL prcout
     $           ('Problem with type of corner array =', ccrnlat, 1) 
            CALL HALTE('STOP in corners')
         ENDIF
         IF (nlogprt .GE. 2) THEN
            WRITE (UNIT = nulou,FMT = *) 
     $            ' Read corner longitudes and latitudes from grids.nc'
         ENDIF 
C     
      ELSE
         IF (cdgrdtyp .ne. 'LR') THEN
            WRITE (UNIT = nulou,FMT = *) 
     $            'Corner longitudes and latitudes are not'
            WRITE (UNIT = nulou,FMT = *) 
     $            'defined in grids.nc and cannot be calculated'
            WRITE (UNIT = nulou,FMT = *) 
     $            'by Oasis as grid is not logically rectangular'
            CALL HALTE('STOP in corners')
         ENDIF
C
C*   3. Calculate corners from center latitudes and longitudes
C*      ------------------------------------------------------
        IF (nlogprt .GE. 2) THEN
            WRITE (UNIT = nulou,FMT = *) ' '
            WRITE (UNIT = nulou,FMT = *) 
     $          ' Calculate corner longitudes and latitudes'
        ENDIF 
C   
C*   -- convert longitudes/latitudes to radians
C
        zconva = 1.74532925199432957692e-2 ! =PI/180
        zconvb = 1/zconva 
        PI = zconva * 180
C
        grid_center_lon = grid_center_lon * zconva
        grid_center_lat = grid_center_lat * zconva
C
        WHERE (grid_center_lon < -PI)
            grid_center_lon = grid_center_lon + 2*PI
        END WHERE
        WHERE (grid_center_lon > PI)
            grid_center_lon = grid_center_lon - 2*PI
        END WHERE
C
C*   -- Calculate vectorproduct to find out the grid order
C
        lon12 = grid_center_lon(1,2)
        lon21 = grid_center_lon(2,1)
        IF ((lon21 - grid_center_lon(1,1)) >  PI) THEN
            lon21 = lon21 + 2*PI
        ELSE IF ((lon21 - grid_center_lon(1,1)) < -PI) THEN
            lon21 = lon21 + 2*PI
        ELSE IF ((lon12 - grid_center_lon(1,1)) >  PI) THEN
            lon12 = lon12 - 2*PI
        ELSE IF ((lon12 - grid_center_lon(1,1)) < -PI) THEN
            lon12 = lon12 + 2*PI
        ENDIF
C
        vp =  ((lon21                - grid_center_lon(1,1)) 
     $        * (grid_center_lat(1,2) - grid_center_lat(1,1)))
     $       - ((lon12                - grid_center_lon(1,1)) 
     $        * (grid_center_lat(2,1) - grid_center_lat(1,1)))
        IF (vp < 0) THEN
            c1 = 1
            c2 = 2
            c3 = 3
            c4 = 4
        ELSE
            c1 = 4
            c2 = 3
            c3 = 2
            c4 = 1
        ENDIF
C
C*    -- Grid corner latitudes and longitudes
C
	DO i = 1, IX            ! loop longitudes
          im1 = i-1
          IF (i == 1)  THEN
              IF (cd_per == 'P') im1 = IX - id_per
              IF (cd_per == 'R') im1 = 1
          ENDIF
          ip1 = i+1
          IF (i == IX) THEN
              IF (cd_per == 'P') ip1 = 1 + id_per
              IF (cd_per == 'R') ip1 = IX
          ENDIF
c           DO j = 2, IY-1          ! loop latitudes
          DO j = 1, IY            ! loop latitudes
            jm1 = j-1
            IF (j == 1) jm1 = 1
            jp1 = j+1
            IF (j == IY) jp1 = IY
            DO c = 1, IC        ! loop corners
              IF (c == c1) THEN
                  lon1 = grid_center_lon(ip1,jm1)
                  lon2 = grid_center_lon(i  ,jm1)
                  lon3 = grid_center_lon(i  ,j  )
                  lon4 = grid_center_lon(ip1,j  )
                  lat1 = grid_center_lat(ip1,jm1)
                  lat2 = grid_center_lat(i  ,jm1)
                  lat3 = grid_center_lat(i  ,j  )
                  lat4 = grid_center_lat(ip1,j  )
              ELSE IF (c == c2) THEN
                  lon1 = grid_center_lon(i  ,jm1)
                  lon2 = grid_center_lon(im1,jm1)
                  lon3 = grid_center_lon(im1,j  )
                  lon4 = grid_center_lon(i  ,j  )
                  lat1 = grid_center_lat(i  ,jm1)
                  lat2 = grid_center_lat(im1,jm1)
                  lat3 = grid_center_lat(im1,j  )
                  lat4 = grid_center_lat(i  ,j  )
              ELSE IF (c == c3) THEN
                  lon1 = grid_center_lon(i  ,j  )
                  lon2 = grid_center_lon(im1,j  )
                  lon3 = grid_center_lon(im1,jp1)
                  lon4 = grid_center_lon(i  ,jp1)
                  lat1 = grid_center_lat(i  ,j  )
                  lat2 = grid_center_lat(im1,j  )
                  lat3 = grid_center_lat(im1,jp1)
                  lat4 = grid_center_lat(i  ,jp1)
              ELSE IF (c == c4) THEN
                  lon1 = grid_center_lon(ip1,j  )
                  lon2 = grid_center_lon(i  ,j  )
                  lon3 = grid_center_lon(i  ,jp1)
                  lon4 = grid_center_lon(ip1,jp1)
                  lat1 = grid_center_lat(ip1,j  )
                  lat2 = grid_center_lat(i  ,j  )
                  lat3 = grid_center_lat(i  ,jp1)
                  lat4 = grid_center_lat(ip1,jp1)
              ENDIF
C
C*             -- Transformation to x/y/z coordinates
C
              x1 = cos(lat1)*cos(lon1)
              x2 = cos(lat2)*cos(lon2)
              x3 = cos(lat3)*cos(lon3)
              x4 = cos(lat4)*cos(lon4)
C
              y1 = cos(lat1)*sin(lon1)
              y2 = cos(lat2)*sin(lon2)
              y3 = cos(lat3)*sin(lon3)
              y4 = cos(lat4)*sin(lon4)
C
              z1 = sin(lat1)
              z2 = sin(lat2)
              z3 = sin(lat3)
              z4 = sin(lat4)
C
C*             -- Interpolation
C
              xmean = (x1 + x2 + x3 + x4) / 4.
              ymean = (y1 + y2 + y3 + y4) / 4.
              zmean = (z1 + z2 + z3 + z4) / 4.
C
              radius = SQRT(xmean**2 + ymean**2 + zmean**2)
C
              xmean = xmean/radius
              ymean = ymean/radius
              zmean = zmean/radius
C     
C*             -- Transformation to lat/lon coordinates
C
              grid_corner_lat(i,j,c) = ASIN(zmean)
              IF (ymean >= 0. .AND. xmean > 0.) THEN
                  grid_corner_lon(i,j,c) = ATAN(ymean/xmean)
              ELSE IF (ymean >= 0. .AND. xmean < 0.) THEN
                  grid_corner_lon(i,j,c) = PI + ATAN(ymean/xmean)
              ELSE IF (ymean < 0. .AND. xmean < 0.) THEN
                  grid_corner_lon(i,j,c) = -PI + ATAN(ymean/xmean)
              ELSE IF (ymean < 0. .AND. xmean > 0.) THEN
                  grid_corner_lon(i,j,c) = ATAN(ymean/xmean)
              ENDIF
C
            ENDDO               ! corners
          ENDDO                 ! latitudes
        ENDDO                   ! longitudes
C
C*    -- Transformation from radians to degrees
C
        grid_center_lon = grid_center_lon * zconvb
        grid_center_lat = grid_center_lat * zconvb
        grid_corner_lon = grid_corner_lon * zconvb
        grid_corner_lat = grid_corner_lat * zconvb
C
C*    -- write corner longitudes and latitudes to grids-file
C
        IF ( .not. lncdfgrd) THEN
            WRITE (UNIT = nulou,FMT = *) 
     $          'WARNING: corner longitudes and latitudes'
            WRITE (UNIT = nulou,FMT = *) 
     $          'cannot be written to file grids'
            WRITE (UNIT = nulou,FMT = *) 
     $          'as it is not in NetCDF FORMAT.'
        ELSE
            clon = grd_name(1:icount)//cglonsuf
            CALL hdlerr(NF_INQ_VARID
     $          (nc_grdid, clon, idlon), 'corners')
            CALL hdlerr(NF_INQ_VARDIMID
     $          (nc_grdid, idlon, londims), 'corners')
            CALL hdlerr(NF_REDEF(nc_grdid), 'corners')
            stat = NF_INQ_DIMID(nc_grdid, 'corners', crndim)
            IF (stat .NE. NF_NOERR) THEN
                CALL hdlerr(NF_DEF_DIM
     $              (nc_grdid, 'corners', IC, crndim), 'corners')
            ENDIF
            ndims = 3
            vardims(1) = londims(1)
            vardims(2) = londims(2)
            vardims(3) = crndim
            IF ( .not. ll_single) THEN
                CALL hdlerr(NF_DEF_VAR(nc_grdid, ccrnlon,
     $              NF_DOUBLE, ndims, vardims, idcrnlon), 'corners')
                CALL hdlerr(NF_DEF_VAR(nc_grdid, ccrnlat,
     $              NF_DOUBLE, ndims, vardims, idcrnlat), 'corners')
                CALL hdlerr(NF_ENDDEF(nc_grdid), 'corners')
                CALL hdlerr(NF_PUT_VARA_DOUBLE(nc_grdid, idcrnlon, 
     $              start, count, grid_corner_lon), 'corners')
                CALL hdlerr(NF_PUT_VARA_DOUBLE(nc_grdid, idcrnlat, 
     $              start, count, grid_corner_lat), 'corners')
            ELSE
                CALL hdlerr(NF_DEF_VAR(nc_grdid, ccrnlon,
     $              NF_REAL, ndims, vardims, idcrnlon), 'corners')
                CALL hdlerr(NF_DEF_VAR(nc_grdid, ccrnlat,
     $              NF_REAL, ndims, vardims, idcrnlat), 'corners')
                CALL hdlerr(NF_ENDDEF(nc_grdid), 'corners')
                CALL hdlerr(NF_PUT_VARA_REAL(nc_grdid, idcrnlon, 
     $              start, count, grid_corner_lon), 'corners')
                CALL hdlerr(NF_PUT_VARA_REAL(nc_grdid, idcrnlat,
     $              start, count, grid_corner_lat), 'corners')
            ENDIF
C
            IF (nlogprt .GE. 2) THEN
                WRITE (UNIT = nulou,FMT = *) ' '
                WRITE (UNIT = nulou,FMT = *) 
     $              ' Wrote corner longitudes and latitudes'
            ENDIF 
C
        ENDIF
      ENDIF
C
C*    4. Resize corner arrays
C*       --------------------
C
      DO i = 1, IX
        DO j = 1, IY
          ij = ((j-1)*IX)+i
          DO c = 1, IC
            grid_corner_lon_2D(c,ij) = grid_corner_lon(i,j,c)
            grid_corner_lat_2D(c,ij) = grid_corner_lat(i,j,c)
          ENDDO
        ENDDO
      ENDDO
C
C*    5. End of routine
C*       --------------
C
      IF ( lncdfgrd) CALL hdlerr(NF_CLOSE(nc_grdid), 'corners')
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine corners ---------'
          CALL FLUSH (nulou)
      ENDIF
      RETURN
      END SUBROUTINE corners

