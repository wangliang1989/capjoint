      SUBROUTINE REFL(ZRPU,ZRPD,ZRSU,ZRSD,DW,N,
     -               P,VP,VS,DEN,DEP,NL,L,IB,TR)
*  < Haskell(1953,BSSA;1962;JGR)'s matrix >
*      IB = 1/2 for P/SV
*      P  = Ray parameter: sin(ih)/v
*  --------------------------------------
*   For IB=1/4(incident P)
*     ZRPU = delta(l)' /delta(nl)''
*     ZRPD = delta(l)''/delta(nl)''
*     ZRSU =2omega(l)' /delta(nl)''
*     ZRSD =2omega(l)''/delta(nl)''
*   For IB=2(incident SV)
*     ZRPU = delta(l)' /2omega(nl)''
*     ZRPD = delta(l)''/2omega(nl)''
*     ZRSU = omega(l)' / omega(nl)''
*     ZRSD = omega(l)''/ omega(nl)''
*  --------------------------------------
*   delta,omega are coefficients of <<potentials>>
*  --------------------------------------
*     TR = P-wave travel time from l to nl layer
*  --------------------------------------
      IMPLICIT COMPLEX*8 (Z)
       REAL VP(NL),VS(NL),DEN(NL),DEP(NL)
       DIMENSION ZRPU(N),ZRPD(N),ZRSU(N),ZRSD(N)
     -,  ZEL(4,4),ZE(4,4),ZA(4,4),ZAA(4,4),ZA1(4,4),ZJ(4,4),ZJL(4,4)
           CALL CLEAR(ZEL,4)
           CALL CLEAR(ZE,4)
           CALL CLEAR(ZA,4)
           CALL CLEAR(ZAA,4)
           CALL CLEAR(ZA1,4)
           CALL CLEAR(ZJ,4)
           CALL CLEAR(ZJL,4)
           P2=P**2
           TR=0
       DO 10 M=L,NL-1
10        TR=TR+DEP(M)*SQRT(1/VP(M)**2-P2)
* E-1 matrix for l layers
           GM=2*(P*VS(L))**2
           VP2=1/VP(L)**2
           Y1=SQRT(VP2-P2)
           RA=Y1/P
           VS2=1/VS(L)**2
           Y2=SQRT(VS2-P2)
           RB=Y2/P
          ZEL(1,1)=-2*(VS(L)/VP(L))**2
          ZEL(1,3)=1/(DEN(L)*VP(L)**2)
          ZEL(2,2)=(GM-1)/(VP(L)**2*RA*P2)
          ZEL(2,4)=1/(DEN(L)*VP(L)**2*RA)
          ZEL(3,1)=(GM-1)/(GM*RB)
          ZEL(3,3)=-P2/(DEN(L)*GM*RB)
          ZEL(4,2)=1
          ZEL(4,4)=P2/(DEN(L)*GM)
* E-1 matrix for nl layers
           GM=2*(P*VS(NL))**2
           VP2=1/VP(NL)**2
           Y1=SQRT(VP2-P2)
           RA=Y1/P
           VS2=1/VS(NL)**2
           Y2=SQRT(VS2-P2)
           RB=Y2/P
          ZE(1,1)=-2*(VS(NL)/VP(NL))**2
          ZE(1,3)=1/(DEN(NL)*VP(NL)**2)
          ZE(2,2)=(GM-1)/(VP(NL)**2*RA*P2)
          ZE(2,4)=1/(DEN(NL)*VP(NL)**2*RA)
          ZE(3,1)=(GM-1)/(GM*RB)
          ZE(3,3)=-P2/(DEN(NL)*GM*RB)
          ZE(4,2)=1
          ZE(4,4)=P2/(DEN(NL)*GM)
*
      DO 100 I=1,N/2
         W=(I-1)*DW
         DO 1 J1=1,4
         DO 1 J2=1,4
1        ZAA(J1,J2)=0
         DO 2 J=1,4
2        ZAA(J,J)=1
        IF(L.EQ.1) CALL PROD(ZEL,ZAA,ZJL,4)
       DO 110 M=1,NL-1
           VP2=1/VP(M)**2
           Y1=SQRT(VP2-P2)
           PM=Y1*W*DEP(M)
           CPM=COS(PM)
           ZSPM=CMPLX(0.,SIN(PM))
           RA=Y1/P
           RMC2=DEN(M)/P2
         IF(VS(M).NE.0.) THEN
           VS2=1/VS(M)**2
           Y2=SQRT(VS2-P2)
           GM=2*(P*VS(M))**2
           QM=Y2*W*DEP(M)
           CQM=COS(QM)
           ZSQM=CMPLX(0.,SIN(QM))
           RB=Y2/P
* A-matrix
          ZA(1,1)=GM*CPM-(GM-1)*CQM
          ZA(1,2)=(GM-1)/RA*ZSPM+GM*RB*ZSQM
          ZA(1,3)=-(1/RMC2)*(CPM-CQM)
          ZA(1,4)=(1/RMC2)*(ZSPM/RA+RB*ZSQM)
          ZA(2,1)=-(GM*RA*ZSPM+(GM-1)/RB*ZSQM)
          ZA(2,2)=-(GM-1)*CPM+GM*CQM
          ZA(2,3)=(1/RMC2)*(RA*ZSPM+ZSQM/RB)
          ZA(2,4)=ZA(1,3)
          ZA(3,1)=RMC2*GM*(GM-1)*(CPM-CQM)
          ZA(3,2)=RMC2*( (GM-1)**2/RA*ZSPM+GM**2*RB*ZSQM)
          ZA(3,3)=ZA(2,2)
          ZA(3,4)=ZA(1,2)
          ZA(4,1)=RMC2*(GM**2*RA*ZSPM+(GM-1)**2/RB*ZSQM)
          ZA(4,2)=ZA(3,1)
          ZA(4,3)=ZA(2,1)
          ZA(4,4)=ZA(1,1)
* Water layer
         ELSE
          ZA(1,1)=1
          ZA(1,2)=-ZSPM/RA
          ZA(1,3)=-(1/RMC2)*CPM
          ZA(1,4)=(1/RMC2)*ZSPM/RA
          ZA(2,1)=0
          ZA(2,2)=CPM
          ZA(2,3)=RA/RMC2*ZSPM
          ZA(2,4)=ZA(1,3)
          ZA(3,1)=0
          ZA(3,2)=RMC2/RA*ZSPM
          ZA(3,3)=ZA(2,2)
          ZA(3,4)=ZA(1,2)
          ZA(4,1)=0
          ZA(4,2)=0
          ZA(4,3)=0
          ZA(4,4)=0
       ENDIF
*
       DO 101 J1=1,4
       DO 101 J2=1,4
101       ZA1(J1,J2)=ZAA(J1,J2)
        CALL PROD(ZA,ZA1,ZAA,4)
* J-matrix for l layers
        IF(M.EQ.L-1) CALL PROD(ZEL,ZAA,ZJL,4)
110     CONTINUE
*
* J-matrix for nl layers
         CALL PROD(ZE,ZAA,ZJ,4)
*
         ZJ1=ZJ(4,2)-ZJ(3,2)
         ZJ2=ZJ(3,1)-ZJ(4,1)
         ZJ3=ZJ(2,2)-ZJ(1,2)
         ZJ4=ZJ(1,1)-ZJ(2,1)
            ZDET=ZJ4*ZJ1-ZJ3*ZJ2
* Propagator coefficients
       IF(IB.EQ.1.OR.IB.EQ.4) THEN
         ZRPU(I)=(ZJ1*(ZJL(1,1)+ZJL(2,1))+ZJ2*(ZJL(1,2)+ZJL(2,2)))/ZDET
         ZRPD(I)=(ZJ1*(ZJL(1,1)-ZJL(2,1))+ZJ2*(ZJL(1,2)-ZJL(2,2)))/ZDET
       ZRSU(I)=2*(ZJ1*(ZJL(3,1)+ZJL(4,1))+ZJ2*(ZJL(3,2)+ZJL(4,2)))/ZDET
       ZRSD(I)=2*(ZJ1*(ZJL(4,1)-ZJL(3,1))+ZJ2*(ZJL(4,2)-ZJL(3,2)))/ZDET
       ELSE
       ZRPU(I)=(ZJ3*(ZJL(1,1)+ZJL(2,1))+ZJ4*(ZJL(1,2)+ZJL(2,2)))/ZDET/2
       ZRPD(I)=(ZJ3*(ZJL(1,1)-ZJL(2,1))+ZJ4*(ZJL(1,2)-ZJL(2,2)))/ZDET/2
         ZRSU(I)=(ZJ3*(ZJL(3,1)+ZJL(4,1))+ZJ4*(ZJL(3,2)+ZJL(4,2)))/ZDET
         ZRSD(I)=(ZJ3*(ZJL(4,1)-ZJL(3,1))+ZJ4*(ZJL(4,2)-ZJL(3,2)))/ZDET
       ENDIF
161    IF(I.EQ.1) GOTO 100
         ZRPU(N+2-I)=CONJG(ZRPU(I))
         ZRPD(N+2-I)=CONJG(ZRPD(I))
         ZRSU(N+2-I)=CONJG(ZRSU(I))
         ZRSD(N+2-I)=CONJG(ZRSD(I))
100   CONTINUE
         ZRPU(N/2+1)=0
         ZRPD(N/2+1)=0
         ZRSU(N/2+1)=0
         ZRSD(N/2+1)=0
      END
