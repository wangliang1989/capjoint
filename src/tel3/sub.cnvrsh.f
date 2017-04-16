      SUBROUTINE CNVRSH(ZRC,DW,N,P,VS,DEN,DEP,NL,TR)
*  < Displacement-to-displacement conversion near the free surface >
*      P  = Ray parameter: sin(ih)/v
*  --------------------------------------
*      ZRC = v(0)/vSH(nl) (SH to Horizontal component)
*  --------------------------------------
*      TR = S-wave travel time from nl-th layer to top
*  --------------------------------------
      IMPLICIT COMPLEX*8 (Z)
       REAL VS(NL),DEN(NL),DEP(NL)
       DIMENSION ZRC(N),ZA(2,2),ZAA(2,2),ZA1(2,2)
           CALL CLEAR(ZA,2)
           CALL CLEAR(ZAA,2)
           CALL CLEAR(ZA1,2)
           P2=P**2
           TR=0
       DO 10 M=1,NL-1
          IF(VS(M).EQ.0.) GOTO 10
          TR=TR+DEP(M)*SQRT(1/VS(M)**2-P2)
10     CONTINUE
      DO 100 I=1,N/2
         W=(I-1)*DW
         DO 1 J1=1,2
         DO 1 J2=1,2
1        ZAA(J1,J2)=0
         DO 2 J=1,2
2        ZAA(J,J)=1
3      DO 110 M=1,NL-1
           VS2=1/VS(M)**2
           Y2=SQRT(VS2-P2)
           RGB=DEN(M)*VS(M)**2*Y2/P
           QM=Y2*W*DEP(M)
           CQM=COS(QM)
           ZSQM=CMPLX(0.,SIN(QM))
* A-matrix
          ZA(1,1)=CQM
          ZA(1,2)=1/RGB*ZSQM
          ZA(2,1)=RGB*ZSQM
          ZA(2,2)=CQM
*
       DO 101 J1=1,2
       DO 101 J2=1,2
101       ZA1(J1,J2)=ZAA(J1,J2)
        CALL PROD(ZA,ZA1,ZAA,2)
110     CONTINUE
*
          VS2=1/VS(NL)**2
          Y2=SQRT(VS2-P2)
          RGB=DEN(NL)*VS(NL)**2*Y2/P
          ZDET=ZAA(1,1)+ZAA(2,1)/RGB
         ZRC(I)=2/ZDET
161    IF(I.EQ.1) GOTO 100
         ZRC(N+2-I)=CONJG(ZRC(I))
100   CONTINUE
         ZRC(N/2+1)=0
      END
