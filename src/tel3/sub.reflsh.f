      SUBROUTINE REFLSH(ZRSU,ZRSD,DW,N,P,VS,DEN,DEP,NL,L,TR)
*  < Haskell(1953,BSSA;1960,JGR) matrix for SH wave >
*      P  = Ray parameter: sin(ih)/v
*  --------------------------------------
*     ZRSU = v(l)' / v(nl)''
*     ZRSD = v(l)''/ v(nl)''
*  v is coefficient of <<displacement>>
*  --------------------------------------
*     TR = S-wave travel time from l to nl layer
*  --------------------------------------
      IMPLICIT COMPLEX*8 (Z)
       REAL VS(NL),DEN(NL),DEP(NL)
       DIMENSION ZRSU(N),ZRSD(N)
     -  ,ZA(2,2),ZAA(2,2),ZA1(2,2),ZAL(2,2)
           CALL CLEAR(ZA,2)
           CALL CLEAR(ZAA,2)
           CALL CLEAR(ZA1,2)
           CALL CLEAR(ZAL,2)
           P2=P**2
           TR=0
       DO 10 M=L,NL-1
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
        IF(L.NE.1) GOTO 3
         ZAL(1,1)=1.
         ZAL(2,2)=1.
         ZAL(1,2)=0.
         ZAL(2,1)=0.
3      DO 110 M=1,NL-1
        IF(VS(M).NE.0.) THEN
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
* Water layer
         ELSE
          ZA(1,1)=1
          ZA(1,2)=0
          ZA(2,1)=0
          ZA(2,2)=0
       ENDIF
*
       DO 101 J1=1,2
       DO 101 J2=1,2
101       ZA1(J1,J2)=ZAA(J1,J2)
        CALL PROD(ZA,ZA1,ZAA,2)
* J-matrix for l layers
        IF(M.NE.L-1) GOTO 110
       DO 102 J1=1,2
       DO 102 J2=1,2
102       ZAL(J1,J2)=ZAA(J1,J2)
110     CONTINUE
*
          VS2=1/VS(NL)**2
          Y2=SQRT(VS2-P2)
          RGB=DEN(NL)*VS(NL)**2*Y2/P
          ZDET=ZAA(1,1)+ZAA(2,1)/RGB
          VS2=1/VS(L)**2
          Y2=SQRT(VS2-P2)
          RGBL=DEN(L)*VS(L)**2*Y2/P
* Propagator coefficients
         ZRSU(I)=(ZAL(1,1)-ZAL(2,1)/RGBL)/ZDET
         ZRSD(I)=(ZAL(1,1)+ZAL(2,1)/RGBL)/ZDET
161    IF(I.EQ.1) GOTO 100
         ZRSU(N+2-I)=CONJG(ZRSU(I))
         ZRSD(N+2-I)=CONJG(ZRSD(I))
100   CONTINUE
         ZRSU(N/2+1)=0
         ZRSD(N/2+1)=0
      END
