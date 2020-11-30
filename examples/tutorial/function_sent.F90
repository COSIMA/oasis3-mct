SUBROUTINE function_sent(ni,nj, &
                         coords_1,coords_2, &
                         fnc_ana,ib,w_unit)
  !*********************************************************************************************************************
  !
  IMPLICIT NONE
  !
#ifdef NO_USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(6,37)   ! real
#elif USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307) ! double
#endif
  !
  ! Constants
  !
  REAL (kind=wp), PARAMETER    :: coef = 2.
  !
  REAL (kind=wp), PARAMETER    :: dp_pi=3.14159265359
  REAL (kind=wp), PARAMETER    :: dp_length= 1.2*dp_pi
  REAL (kind=wp), PARAMETER    :: dp_conv = dp_pi/180.
  !
  INTEGER, INTENT(in) :: ni,nj,ib,w_unit
  !
  INTEGER             :: i,j,k,l
  INTEGER,PARAMETER   :: tdim=100
  !
  REAL (kind=wp), INTENT(out)            :: fnc_ana(ni,nj)
  !
  REAL (kind=wp)                         :: coords_1(ni,nj)
  REAL (kind=wp)                         :: coords_2(ni,nj)
  !
  REAL (kind=wp)                         :: a(tdim,tdim),b(tdim,tdim),c(tdim,tdim)
  !
  DO j=1,nj
    DO i=1,ni
      fnc_ana(i,j) =  ib*(coef - COS(dp_pi*(ACOS(COS(coords_2(i,j)* dp_conv)* &
         COS(coords_1(i,j)* dp_conv))/dp_length)))
    ENDDO
  ENDDO
  !
  ! Additional computations to test load balancing functions
  !
  call random_number(a)
  call random_number(b)

  DO j=1,ni
    DO l=1,tdim
       DO i=1,tdim
          c(l,i)=0.
          DO k=1,tdim
             c(l,i)=a(l,k)*b(k,i)+j+ib
          ENDDO
       ENDDO
    ENDDO
    !write(w_unit,*) 'Indice j end loop:',j
    !call flush(w_unit)
  ENDDO
  !
  !
END SUBROUTINE function_sent
!
