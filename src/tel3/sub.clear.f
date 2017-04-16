       SUBROUTINE CLEAR(Z,N)
       COMPLEX Z(N,N)
       DO 1 I=1,N
       DO 1 J=1,N
1      Z(I,J)=0
       END
