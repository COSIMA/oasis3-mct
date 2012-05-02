MODULE mod_oasis_print

   USE mod_prism_kinds
   USE mod_prism_data

   IMPLICIT NONE

   PUBLIC

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Print separately the integers, the reals, the characters or logical
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 CONTAINS
   SUBROUTINE oasis_pprinti(cd_routine,nv_prism_debug,cd_char,int1,int2,&
                            int3)

     CHARACTER(len=*), INTENT(in)       :: cd_routine,cd_char
     INTEGER(kind=ip_i4_p),INTENT(in)	:: nv_prism_debug
     !
     INTEGER(kind=ip_i4_p), INTENT(in),OPTIONAL :: int1, int2,int3
     !
     !--- local ---
     INTEGER(kind=ip_i4_p) :: l_int1, l_int2, l_int3
     !
     l_int1=0
     l_int2=0
     l_int3=0

     IF (PRISM_Debug == 0) THEN
         IF (mpi_rank_local == 0) THEN
         IF (PRESENT(int1)) THEN
             IF (PRESENT(int2)) THEN
                 IF (PRESENT(int3)) THEN
                     l_int1=int1
                     l_int2=int2
                     l_int3=int3
                     WRITE(nulprt,'(A,1x,A,1x,3i8)') cd_routine,cd_char,l_int1,l_int2,l_int3
                 ELSE
                     l_int1=int1
                     l_int2=int2
                     WRITE(nulprt,'(A,1x,A,1x,2i8)') cd_routine,cd_char,l_int1,l_int2
                 ENDIF
             ELSE
                 l_int1=int1
                 WRITE(nulprt,'(A,1x,A,1x,i8)') cd_routine,cd_char,l_int1
             ENDIF
         ELSE
             WRITE(nulprt,*) 'ERROR : With this routine you can only print integers'
         ENDIF
         ENDIF

     ELSE IF (PRISM_Debug > 0) THEN

     IF (PRISM_Debug >= nv_prism_debug) THEN
!
         IF (PRESENT(int1)) THEN
             IF (PRESENT(int2)) THEN
                 IF (PRESENT(int3)) THEN
                     l_int1=int1
                     l_int2=int2
                     l_int3=int3
                     WRITE(nulprt,'(A,1x,A,1x,3i8)') cd_routine,cd_char,l_int1,l_int2,l_int3
                 ELSE
                     l_int1=int1
                     l_int2=int2
                     WRITE(nulprt,'(A,1x,A,1x,2i8)') cd_routine,cd_char,l_int1,l_int2
                 ENDIF
             ELSE
                 l_int1=int1
                 WRITE(nulprt,'(A,1x,A,1x,i8)') cd_routine,cd_char,l_int1
             ENDIF
         ELSE
             WRITE(nulprt,*) 'ERROR : With this routine you can only print integers'
         ENDIF
!
     ENDIF  !nv_prism_debug
     ENDIF

     CALL FLUSH(nulprt)

   END SUBROUTINE oasis_pprinti
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   SUBROUTINE oasis_pprintr(cd_routine,nv_prism_debug,cd_char,r1,r2,r3)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     CHARACTER(len=*), INTENT(in)       :: cd_routine,cd_char
     INTEGER(kind=ip_i4_p),INTENT(in)	:: nv_prism_debug

     REAL(kind=ip_r8_p), INTENT(in),OPTIONAL :: r1, r2, r3

     !--- local ---
     REAL(kind=ip_r8_p)    :: rr1, rr2,rr3

     rr1=0.
     rr2=0.
     rr3=0.

     IF (PRISM_Debug == 0) THEN
         IF (mpi_rank_local == 0) THEN
         IF (PRESENT(r1)) THEN
             IF (PRESENT(r2)) THEN
                 IF (PRESENT(r3)) THEN
                     rr1=r1
                     rr2=r2
                     rr3=r3
                     WRITE(nulprt,'(A,1x,A,3(3x,1E12.6))') cd_routine,cd_char,rr1,rr2,rr3
                 ELSE
                     rr1=r1
                     rr2=r2
                     WRITE(nulprt,'(A,1x,A,2(3x,1E12.6))') cd_routine,cd_char,rr1,rr2
                 ENDIF
             ELSE
                 rr1=r1
                 WRITE(nulprt,'(A,1x,A,1(3x,1E12.6))') cd_routine,cd_char,rr1
             ENDIF
         ELSE
             WRITE(nulprt,*) 'ERROR : With this routine you can only print reals'
         ENDIF
         ENDIF

     ELSEIF (PRISM_Debug > 0) THEN

     IF (PRISM_Debug >= nv_prism_debug) THEN

         IF (PRESENT(r1)) THEN
             IF (PRESENT(r2)) THEN
                 IF (PRESENT(r3)) THEN
                     rr1=r1
                     rr2=r2
                     rr3=r3
                     WRITE(nulprt,'(A,1x,A,3(3x,1E12.6))') cd_routine,cd_char,rr1,rr2,rr3
                 ELSE
                     rr1=r1
                     rr2=r2
                     WRITE(nulprt,'(A,1x,A,2(3x,1E12.6))') cd_routine,cd_char,rr1,rr2
                 ENDIF
             ELSE
                 rr1=r1
                 WRITE(nulprt,'(A,1x,A,1(3x,1E12.6))') cd_routine,cd_char,rr1
             ENDIF
         ELSE
             WRITE(nulprt,*) 'ERROR : With this routine you can only print reals'
         ENDIF

     ENDIF ! nv_prism_debug
     ENDIF

     CALL FLUSH(nulprt)

   END SUBROUTINE oasis_pprintr
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   SUBROUTINE oasis_pprintc(cd_routine,nv_prism_debug,cd_char,char1,char2,char3)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     CHARACTER(len=*), INTENT(in)       :: cd_routine,cd_char
     INTEGER(kind=ip_i4_p),INTENT(in)	:: nv_prism_debug

     CHARACTER(len=*), INTENT(in),OPTIONAL :: char1,char2,char3

     !--- local ---
     CHARACTER(len=128)    :: cchar1,cchar2,cchar3

     cchar1=''
     cchar2=''
     cchar3=''

     IF (PRISM_Debug == 0) THEN
         IF (mpi_rank_local == 0) THEN
         IF (PRESENT(char1)) THEN
             IF (PRESENT(char2)) THEN
                 IF (PRESENT(char3)) THEN
                     cchar1=char1
                     cchar2=char2
                     cchar3=char3
                     WRITE(nulprt,'2(A,1x),3(A,3x)') cd_routine,cd_char,TRIM(cchar1),TRIM(cchar2),TRIM(cchar3)
                 ELSE
                     cchar1=char1
                     cchar2=char2
                     WRITE(nulprt,'2(A,1x),2(A,3x)') cd_routine,cd_char,TRIM(cchar1),TRIM(cchar2)
                 ENDIF
             ELSE
                 cchar1=char1
                 WRITE(nulprt,'2(A,1x),(A,3x)') cd_routine,cd_char,TRIM(cchar1)
             ENDIF
         ELSE
             WRITE(nulprt,*) 'ERROR : With this routine you can only print characters'
         ENDIF
         ENDIF

     ELSEIF (PRISM_Debug > 0) THEN

     IF (PRISM_Debug >= nv_prism_debug) THEN

         IF (PRESENT(char1)) THEN
             IF (PRESENT(char2)) THEN
                 IF (PRESENT(char3)) THEN
                     cchar1=char1
                     cchar2=char2
                     cchar3=char3
                     WRITE(nulprt,'2(A,1x),3(A,3x)') cd_routine,cd_char,TRIM(cchar1),TRIM(cchar2),TRIM(cchar3)
                 ELSE
                     cchar1=char1
                     cchar2=char2
                     WRITE(nulprt,'2(A,1x),2(A,3x)') cd_routine,cd_char,TRIM(cchar1),TRIM(cchar2)
                 ENDIF
             ELSE
                 cchar1=char1
                 WRITE(nulprt,'2(A,1x),(A,3x)') cd_routine,cd_char,TRIM(cchar1)
             ENDIF
         ELSE
             WRITE(nulprt,*) 'ERROR : With this routine you can only print characters'
         ENDIF
     ENDIF !nv_prism_debug
     ENDIF

     CALL FLUSH(nulprt)

   END SUBROUTINE oasis_pprintc
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   SUBROUTINE oasis_pprintl(cd_routine,nv_prism_debug,cd_char,log1,log2,log3)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     CHARACTER(len=*), INTENT(in)       :: cd_routine,cd_char
     INTEGER(kind=ip_i4_p),INTENT(in)	:: nv_prism_debug

     LOGICAL, INTENT(in),OPTIONAL :: log1,log2,log3

     !--- local ---
     LOGICAL               :: llog1,llog2,llog3

     llog1=.FALSE.
     llog2=.FALSE.
     llog3=.FALSE.

     IF (PRISM_Debug == 0) THEN
         IF (mpi_rank_local == 0) THEN
         IF (PRESENT(log1)) THEN
             IF (PRESENT(log2)) THEN
                 IF (PRESENT(log3)) THEN
                     llog1=log1
                     llog2=log2
                     llog3=log3
                     WRITE(nulprt,'(A,1x,A,3(3x,L))') cd_routine,cd_char,llog1,llog2,llog3
                 ELSE
                     llog1=log1
                     llog2=log2
                     WRITE(nulprt,'(A,1x,A,2(3x,L))') cd_routine,cd_char,llog1,llog2
                 ENDIF
             ELSE
                 llog1=log1
                 WRITE(nulprt,'(A,1x,A,(3x,L))') cd_routine,cd_char,llog1
             ENDIF
         ELSE
             WRITE(nulprt,*) 'ERROR : With this routine you can only print logicals'
         ENDIF
         ENDIF

     ELSEIF (PRISM_Debug > 0) THEN

     IF (PRISM_Debug >= nv_prism_debug) THEN

         IF (PRESENT(log1)) THEN
             IF (PRESENT(log2)) THEN
                 IF (PRESENT(log3)) THEN
                     llog1=log1
                     llog2=log2
                     llog3=log3
                     WRITE(nulprt,'(A,1x,A,3(3x,L))') cd_routine,cd_char,llog1,llog2,llog3
                 ELSE
                     llog1=log1
                     llog2=log2
                     WRITE(nulprt,'(A,1x,A,2(3x,L))') cd_routine,cd_char,llog1,llog2
                 ENDIF
             ELSE
                 llog1=log1
                 WRITE(nulprt,'(A,1x,A,(3x,L))') cd_routine,cd_char,llog1
             ENDIF
         ELSE
             WRITE(nulprt,*) 'ERROR : With this routine you can only print logicals'
         ENDIF

     ENDIF !nv_prism_debug
     ENDIF

     CALL FLUSH(nulprt)

   END SUBROUTINE oasis_pprintl
   !
 END MODULE mod_oasis_print
