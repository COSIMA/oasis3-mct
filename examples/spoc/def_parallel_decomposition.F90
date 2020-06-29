module def_parallel_decomposition
!
! The global grid is split in npes rectangle partitions with local extent in x = global extent
!
contains
   SUBROUTINE def_local_partition (nlon, nlat, npes, mype, &
  	     		 il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset)
  IMPLICIT NONE
  INTEGER, INTENT(in)  :: nlon, nlat, npes, mype
  INTEGER, INTENT(out) :: il_extentx, il_extenty, il_size, il_offsetx, il_offsety, il_offset
  !
  il_extentx = nlon
  il_extenty = nlat/npes ; IF (mype == npes-1)  il_extenty = nlat - (nlat/npes * mype)
  il_size = il_extentx * il_extenty
  il_offsetx = 0
  il_offsety = (nlat/npes * mype)
  il_offset = nlon * il_offsety
  ! 
END SUBROUTINE def_local_partition
!
SUBROUTINE def_paral_size (il_paral_size)
  IMPLICIT NONE
  INTEGER, INTENT(out) :: il_paral_size  
#ifdef DECOMP_APPLE
  il_paral_size = 3
#elif defined DECOMP_BOX
  il_paral_size = 5
#endif
  ! 
END SUBROUTINE def_paral_size
!
SUBROUTINE def_paral(il_offset, il_size, il_extentx, il_extenty, nlon, il_paral_size, il_paral)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: il_offset, il_size, il_extentx, il_extenty, nlon, il_paral_size
  INTEGER, INTENT(OUT) :: il_paral(il_paral_size) 
#ifdef DECOMP_APPLE
  il_paral(1) = 1
  il_paral(2) = il_offset
  il_paral(3) = il_size
#elif defined DECOMP_BOX
  il_paral(1) = 2
  il_paral(2) = il_offset
  il_paral(3) = il_extentx
  il_paral(4) = il_extenty
  il_paral(5) = nlon
#endif
  ! 
END SUBROUTINE def_paral
end module def_parallel_decomposition 