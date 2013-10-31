      subroutine fracnnei (src_size, dst_size,
     $    ld_srcmask, ld_dstmask,
     $    src_lon, src_lat, dst_lon, dst_lat,
     $    num_links, num_wgts, num_neigh, lnnei,
     $    weights_temp, src_addr_temp, dst_addr_temp,
     $    weights, src_addr, dst_addr)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 4 *
C               * -------------     ------- *
C               *****************************
C
C**** *fracnnei* - SCRIP remapping
C
C     Purpose:
C     -------
C     Treatment of the tricky points in an interpolation
C
C     Interface:
C     ---------
C       *CALL*  *
C
C     Called from:
C     -----------
C     scriprmp
C
C     Input:
C     -----
C             src_size    : source grid size (integer)
C             dst_size    : target grid size (integer)
C             ld_srcmask  : mask of the source grid
C             ld_dstmask  : mask of the target grid
C             src_lon     : longitudes of the source grid
C             src_lat     : latitudes of the source grid
C             dst_lon     : longitudes of the target grid
C             dst_lat     : latitudes of the target grid
C             num_links   : total number of links
C             num_wgts    : number of weights for each link
C     InOut
C     -----
C             weights     : remapping weights
C             src_addr    : remapping source addresses
C             dst_addr    : remapping target addresses
C
C     History:
C     -------
C       Version   Programmer     Date        Description
C       -------   ----------     ----        -----------  
C       2.5       D.Declat       2002/08/20  adapted from S. Valcke ptmsq
C       3.0       S. Valcke      2002/10/30  test and corrections
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C* ---------------------------- Modules used ----------------------------
C
      use kinds_mod     ! defines common data types
      use constants     ! defines common constants
      use grids         ! module containing grid information
      use remap_vars    ! module containing remap information
      USE mod_oasis_flush
C
C* ---------------------------- Implicit --------------------------------
C
      implicit none
C
C* ---------------------------- Include files ---------------------------
C

C      INCLUDE 'netcdf.inc'
C
C* ---------------------------- Intent In -------------------------------
C
      INTEGER (kind=int_kind) ::
     $    src_size,             ! size of the source grid
     $    dst_size              ! size of the destination grid
C
      REAL (kind=dbl_kind) ::
     $    src_lat(src_size), src_lon(src_size),
     $    dst_lat(dst_size), dst_lon(dst_size)

C
      LOGICAL ::
     $    ld_srcmask(src_size),   ! source grid mask
     $    ld_dstmask(dst_size)    ! target grid mask
C
      INTEGER (kind=int_kind) ::
     $    num_links,       ! number of links between src and tgt
     $    num_wgts,        ! number of weights
     $    num_neigh        ! number of Vmm points

      logical (kind=log_kind) ::
     $    lnnei(dst_size)       ! flag for tricky points


      REAL (kind=dbl_kind) ::
     $    weights_temp(num_wgts, num_links) ! oldsize remapping weights
C
      INTEGER (kind=int_kind) ::
     $    src_addr_temp(num_links), ! oldsize remapping source addresses
     $    dst_addr_temp(num_links)  ! oldsize remapping target addresses

     
C
C* ---------------------------- Intent Out ------------------------------
C
      REAL (kind=dbl_kind)  ::
     $    weights(num_wgts, num_links+num_neigh ) ! remapping weights
C
      INTEGER (kind=int_kind)  ::
     $    src_addr(num_links+num_neigh), ! remapping source addresses
     $    dst_addr(num_links+num_neigh)  ! remapping target addresses
C
C* ---------------------------- Local declarations ----------------------
C
C

C
      INTEGER (kind=int_kind) :: 
     $    ila_nneiadd           ! Nearest-neighbor address
C
      INTEGER (kind=int_kind) ::
     $    ib_dst,               ! INDEX loop for the distance grid
     $    ib_src,               ! INDEX loop for the source grid
     $    ib_links              ! INDEX loop for the links
C
      REAL (kind=dbl_kind) ::
     $    coslat,               ! cosinus of the latitude
     $    sinlat,               ! sinus of the latitude
     $    coslon,               ! cosinus of the longitude
     $    sinlon,               ! sinus of the longitude
     $    distance, 
     $    dist_min,
     $    arg
C
      INTEGER (kind=int_kind) :: n, il
C
      INTEGER (kind=int_kind) :: counter_Vmm
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
     $        '   Entering ROUTINE fracnnei  -  Level 4'
          WRITE (UNIT = nulou,FMT = *) 
     $        '           ****************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Treating the tricky points of the remapping'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL FLUSH(nulou)
      ENDIF
C
C *----------------------------------------------------------------------
C
C*    2. Treating Vmm points   V
C        -------------------  m m
C     The target point is a non-masked Valid point while the source points 
C         are all masked points. Use of the non-masked nearest neighbours.
C
 
C  -- store the weights, src_addr, dst_addr from temporary array
      weights(1:num_wgts,1:num_links) =
     $                  weights_temp(1:num_wgts,1:num_links)
      src_addr(1:num_links) = src_addr_temp(1:num_links)
      dst_addr(1:num_links) = dst_addr_temp(1:num_links)

C* -- Find the nearest neighbours and store weights and address 
      counter_Vmm = 0
      DO ib_dst = 1, dst_size
        IF ( lnnei(ib_dst) .eqv. .true. ) THEN

          counter_Vmm = counter_Vmm+1    
          dst_addr(num_links+counter_Vmm) = ib_dst

      IF (nlogprt .GE. 2) THEN
          write(nulou,*) 'ib_dst for true=',ib_dst 
          write(nulou,*) 'counter_Vmm =',counter_Vmm
          write(nulou,*) 'num_links+counter_Vmm =',
     $                   num_links+counter_Vmm 
          write(nulou,*) 'dst_addr =',dst_addr(num_links+counter_Vmm)
      ENDIF

                    coslat = cos(dst_lat(ib_dst))
                    sinlat = sin(dst_lat(ib_dst))
                    coslon = cos(dst_lon(ib_dst))
                    sinlon = sin(dst_lon(ib_dst))

                    dist_min = bignum
                    ila_nneiadd = 0
                    DO ib_src = 1, src_size
                      IF (ld_srcmask(ib_src)) THEN
                          arg = 
     &                        coslat*cos(src_lat(ib_src))*
     &                       (coslon*cos(src_lon(ib_src)) +
     &                        sinlon*sin(src_lon(ib_src)))+
     &                        sinlat*sin(src_lat(ib_src))
                          IF (arg < -1.0d0) THEN
                              arg = -1.0d0
                          ELSE IF (arg > 1.0d0) THEN
                              arg = 1.0d0
                          END IF
                          distance = acos(arg)
                          IF (distance < dist_min) THEN
                              ila_nneiadd = ib_src
                              dist_min = distance
                          ENDIF
                      ENDIF
                    END DO
                    src_addr(num_links+counter_Vmm) = ila_nneiadd
                    weights(1,num_links+counter_Vmm) = 1.0
        IF (nlogprt .GE. 2) THEN
            write(nulou,*) 'src_addr =',src_addr(num_links+counter_Vmm)
            WRITE(nulou,*) 
     $               '*************** Nearest source neighbour is ', 
     $                  ila_nneiadd          
        ENDIF          
      ENDIF
      END DO


C
C
C *----------------------------------------------------------------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $        '   Leaving ROUTINE fracnnei  -  Level 4'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL FLUSH(nulou)
      ENDIF

      END SUBROUTINE fracnnei

!***********************************************************************

  
