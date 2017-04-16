        SUBROUTINE PROD(ZA,ZB,ZC,N)
* Product of matrix
        COMPLEX*8 ZA(N,N),ZB(N,N),ZC(N,N)
       DO 1 J1=1,N
       DO 1 J2=1,N
        ZC(J1,J2)=0
         DO 1 J=1,N
1       ZC(J1,J2)=ZC(J1,J2)+ZA(J1,J)*ZB(J,J2)
       END
