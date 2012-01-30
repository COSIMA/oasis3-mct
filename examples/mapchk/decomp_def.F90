
module mod_decomp_def

   implicit none

   public :: decomp_def

contains

!****************************************************************************************
SUBROUTINE decomp_def(type,id_paral,id_im,id_jm,id_rank,id_npes,id_unit)
  !
  IMPLICIT NONE
  INTEGER, INTENT(in)  :: type        ! type 1=apple, 2=box, 3=orange
  INTEGER, allocatable, INTENT(out) :: id_paral(:)
  INTEGER, INTENT(in)  :: id_im       ! Grid dimension in i
  INTEGER, INTENT(in)  :: id_jm       ! Grid dimension in j
  INTEGER, INTENT(in)  :: id_rank     ! Rank of process
  INTEGER, INTENT(in)  :: id_npes     ! Number of processes involved in the coupling
  INTEGER, INTENT(in)  :: id_unit     ! Unit of log file
  INTEGER              :: il_imjm, il_partj, il_size
  INTEGER              :: m,n, nsegs, nsegst, offset, length, segcnt

  il_imjm = id_im*id_jm
  il_partj = id_jm/id_npes  ! Nbr of latitude circles in the partition

  !--- apple -------

  if (type == 1) then
  ! Each process is responsible for a part of field defined by
  ! the number of grid points and the offset of the first point

  WRITE (id_unit,*) 'APPLE partitioning'
  il_size=3
  allocate(id_paral(il_size))

  IF (id_rank .LT. (id_npes-1)) THEN
      id_paral (1) = 1
      id_paral (2) = id_rank*(il_partj * id_im)
      id_paral (3) = il_partj * id_im
  ELSE
      id_paral (1) = 1
      id_paral (2) = id_rank*(il_partj * id_im)
      id_paral (3) = il_imjm-(id_rank*(il_partj * id_im))
  ENDIF

  endif  ! type = 1

  !--- box -------

  if (type == 2) then
  !
  WRITE (id_unit,*) 'BOX partitioning'
  il_size=5
  allocate(id_paral(il_size))

  ! Each process is responsible for a rectangular box of 32x48 points

  id_paral (1) = 2
  id_paral (5) = id_im
  id_paral (2) = id_rank*il_partj*id_im
  id_paral (3) = id_im
  IF (id_rank .LT. (id_npes-1)) THEN
      id_paral (4) = il_partj
  ELSE
      id_paral (4) = id_jm-(id_rank*il_partj)
  ENDIF

  endif  ! type = 2

  !--- orange -------

  if (type == 3) then
  ! Each process is responsible for a bunch of segments

  WRITE (id_unit,*) 'ORANGE partitioning'
  nsegst = max(8*id_npes,2*id_jm)   ! max of 8 segs/pe or 2 segs/lat line
  nsegst = min(nsegst,il_imjm)       ! but not more than the number of gridcells

  nsegs = nsegst/id_npes            ! now compute nsegs/pe from nsegst, same on all pes
  nsegst = nsegs*id_npes            ! and recompute the total number of segs (int math)

  il_size=2+2*nsegs
  allocate(id_paral(il_size))

  id_paral (1) = 3
  id_paral (2) = nsegs

  offset = 0
  segcnt = 0

  do m = 1,nsegs
  do n = 0,id_npes-1
     length = (il_imjm-offset)/(nsegst-segcnt)
     if (id_rank == n) then
        id_paral(1+2*m) = offset
        id_paral(2+2*m) = length
     endif
!     write(id_unit,*) 'tcxdd1 ',m,n,offset,length
     offset = offset + length
     segcnt = segcnt + 1
  enddo
  enddo

  endif  ! type = 3


END SUBROUTINE decomp_def

end module mod_decomp_def