      subroutine fracnnei_vmm (dst_size,ld_dstmask,num_links, 
     $           dst_addr,src_addr,num_neigh,lnnei) 
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 4 *
C               * -------------     ------- *
C               *****************************
C
C**** *fracnnei* - SCRIP remapping
C
C
C     Purpose:
C     -------
C     Calculate the number of the tricky points in an interpolation
C
C     Interface:
C     ---------
C       *CALL*  *
C
C     Called from:
C     -----------
C     scrip
C
C     Input:
C     -----
C             dst_size    : target grid size (integer)
C             ld_dstmask  : mask of the target grid
C             num_links   : total number of links
C             dst_addr    : remapping target addresses       
C     Output 
C     -----
C             num_neigh   : number of tricky points  
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
      use remap_vars            ! module containing remap information
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
      INTEGER (kind=int_kind), intent(in) ::
     $    dst_size              ! size of the destination grid
C
      LOGICAL, intent(in) :: 
     $    ld_dstmask(dst_size)    ! target grid mask
C
      INTEGER (kind=int_kind) ::
     $    num_links      ! number of links between src and tgt

      INTEGER (kind=int_kind) ::
     $    dst_addr(num_links),  ! remapping target addresses
     $    src_addr(num_links)


C* ---------------------------- Intent Out ------------------------------

      INTEGER (kind=int_kind)  :: num_neigh
      logical (kind=log_kind)  ::
     $    lnnei(dst_size)       ! flag for tricky points
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER (kind=int_kind) :: 
     $    ila_nneiadd           ! Nearest-neighbor address
C
      INTEGER (kind=int_kind) ::
     $    ib_dst,               ! INDEX loop for the distance grid
     $    ib_links              ! INDEX loop for the links
C
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
     $        '   Entering ROUTINE fracnnei_vmm  -  Level 4'
          WRITE (UNIT = nulou,FMT = *) 
     $        '           ****************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $        '  Countering the number of the tricky points'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
C
C *----------------------------------------------------------------------
C
C*    2. Countering Vmm points   V
C        -------------------  m m
C    A non-masked Valid target point is either with link or without link
C    If without link, find the non-masked nearest neighbours.
C    and countering the number of Vmm points. 
C
      lnnei(:) = .false.
C* -- Loop all other the target points
      DO ib_dst = 1, dst_size
C* -- If the point is a sea point
        IF (ld_dstmask(ib_dst)) THEN

            lnnei(ib_dst) = .true.

            DO ib_links = 1, num_links
              IF (dst_addr(ib_links) .eq. ib_dst) THEN
                  lnnei(ib_dst) = .false. 
                  exit
              ENDIF
            END DO 
        
        END IF 
      END DO  
 
C* -- Count the number of Vmm points, i.e. the number of element in
C     lnnei which are true     
      counter_Vmm = 0
      DO ib_dst = 1, dst_size
        IF ( lnnei(ib_dst) .eqv. .true. ) THEN
          counter_Vmm = counter_Vmm+1
         IF (nlogprt .GE. 2) THEN
          WRITE(nulou,*)
     $               '********* Will do FRACNNEI for point',ib_dst
          CALL OASIS_FLUSH_SCRIP(nulou)
         ENDIF
        END IF 
      END DO

      num_neigh = counter_Vmm

       IF (nlogprt .GE. 2) THEN
          WRITE(nulou,*)
     $       '************ There are',num_neigh,'Vmm points in all'
       ENDIF
C
C *----------------------------------------------------------------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $        '   Leaving ROUTINE fracnnei vmm -  Level 4'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF

      END SUBROUTINE fracnnei_vmm

!***********************************************************************

  
