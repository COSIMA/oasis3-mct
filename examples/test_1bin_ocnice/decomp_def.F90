!****************************************************************************************
SUBROUTINE decomp_def(id_paral,id_size,id_im,id_jm,id_rank,id_npes,id_unit)

  IMPLICIT NONE
  INTEGER, DIMENSION(id_size), INTENT(out) :: id_paral(id_size)    
  INTEGER, INTENT(in)  :: id_size
  INTEGER, INTENT(in)  :: id_im       ! Grid dimension in i
  INTEGER, INTENT(in)  :: id_jm       ! Grid dimension in j
  INTEGER, INTENT(in)  :: id_rank     ! Rank of process
  INTEGER, INTENT(in)  :: id_npes     ! Number of processes involved in the coupling
  INTEGER, INTENT(in)  :: id_unit     ! Unit of log file
  INTEGER              :: il_imjm, il_partj, il_parti, il_partij, npes2

  if (id_rank < 0 .or. id_rank > id_npes) then
    write(*,*) 'decomp_def ABORT invalid rank',id_rank,id_npes
    stop
  endif

  il_imjm = id_im*id_jm
  il_partj = id_jm/id_npes  ! Nbr of latitude circles in the partition
  il_parti = id_im/id_npes  ! Nbr of lon in partition
  il_partij = il_imjm/id_npes
  id_paral(:) = 0

  if (id_size == 3) then    ! APPLE (this is simple 1d decomp)
      id_paral (1) = 1
      id_paral (2) = id_rank*(il_partij)
      IF (id_rank .LT. (id_npes-1)) THEN
          id_paral (3) = il_partij
      ELSE
          id_paral (3) = il_imjm - ((id_npes-1)*(il_partij))
      ENDIF

  elseif (id_size == 4) then    ! GRAPE (this is doing a decomp in y)
      id_paral (1) = 1
      id_paral (2) = id_rank*(il_partj * id_im)
      IF (id_rank .LT. (id_npes-1)) THEN
          id_paral (3) = il_partj * id_im
      ELSE
          id_paral (3) = il_imjm-(id_rank*(il_partj * id_im))
      ENDIF

  elseif (id_size == 5) then   ! BOX
      id_paral (1) = 2
      id_paral (5) = id_im

      ! full X boxes
!      id_paral (2) = id_rank*il_partj*id_im
!      id_paral (3) = id_im
!      IF (id_rank .LT. (id_npes-1)) THEN
!          id_paral (4) = il_partj
!      ELSE
!          id_paral (4) = id_jm-(id_rank*il_partj)
!      ENDIF

      ! full Y boxes
      id_paral (2) = id_rank*il_parti
      id_paral (4) = id_jm
      IF (id_rank .LT. (id_npes-1)) THEN
          id_paral (3) = il_parti
      ELSE
          id_paral (3) = id_im-(id_rank*il_parti)
      ENDIF

  else
      write(*,*) 'decomp_def ABORT unknown decomp'
      stop
  endif

END SUBROUTINE decomp_def

