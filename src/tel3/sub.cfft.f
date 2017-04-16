      SUBROUTINE CFFT(X,N,ID)
* <  FFT for complex variables >
      IMPLICIT COMPLEX*8 (X-Z)
      DIMENSION X(N)
       PI=SIGN(3.141593,ID*1.)
        N2=N/2
        L=N
1      NI=L/2
        Z=CEXP(CMPLX(0.,PI/NI))
        ZK=1
      DO 3 K=1,NI
       DO 2 M=1,N/L
          J=K+2*(M-1)*NI
          XJ=X(J)
          X(J)=XJ+X(J+NI)
2         X(J+NI)=(XJ-X(J+NI))*ZK
3      ZK=ZK*Z
        L=NI
      IF(L.GT.1) GOTO 1
      DO 10 I=1,N
         J=I-1
         M=N2
         L=1
5       L=L+M*(J-2*(J/2))
         M=M/2
         J=J/2
       IF(J.GE.1) GOTO 5
       IF(I.GT.L) GOTO 10
         XI=X(I)
         X(I)=X(L)
         X(L)=XI
10    CONTINUE
      END
