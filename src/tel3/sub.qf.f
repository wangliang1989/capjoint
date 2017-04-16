      SUBROUTINE QF(Z,N,TQ,DF)
*  < Q-filter >
      COMPLEX Z(N),Z1
      FN=DF*N/2
      DO 1 I=2,N/2
         F=DF*(I-1)
         Z1=CMPLX(0.,2*F*TQ)*LOG(CMPLX(0.,F/FN))
         Z(I)=EXP(Z1)
1        Z(N+2-I)=CONJG(Z(I))
      Z(1)=1
      Z(N/2+1)=0
      END
