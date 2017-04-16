      SUBROUTINE INSTG(Z,N,DW,ID,ZP,ZZ,IZP,IZZ,A0,IP)
*===============================================
*   << Response of GDSN instrument >>          *
*     ID = 0/1/2 for delta/step/ramp function  *
*===============================================
      IMPLICIT COMPLEX*8 (Z)
      DIMENSION Z(N),ZP(50),ZZ(50)
        M2=N/2
        M22=N+2
        DO 3 I=2,M2
          ZW0=CMPLX(0.,DW*(I-1))
          ZZZ=1.
         DO 1 J=1,IZP
1         ZZZ=ZZZ/(ZW0-ZP(J))
         DO 2 J=1,IZZ
2         ZZZ=ZZZ*(ZW0-ZZ(J))
         Z(I)=A0*ZZZ*ZW0**(IP-ID)
3        Z(M22-I)=CONJG(Z(I))
       Z(1)=0.
       Z(M2+1)=0.
      END
