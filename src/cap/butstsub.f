      SUBROUTINE  BUTPAS(H,M,GN,N,FL,FH,FS,AP,AS)
      COMPLEX  R(2),OJ,CQ
      DIMENSION  H(*)
      DATA  PI/3.141593/,HP/1.570796/
C
C      BUTTERWORTH BAND PASS FILTER COEFFICIENT
C
C      ARGUMENTS
C        H      : FILTER COEFFICIENTS
C        M      : ORDER OF FILTER
C        GN     : GAIN FACTOR
C        N      : ORDER OF BUTTERWORTH FUNCTION
C        FL     : LOW  FREQUENCY CUT-OFF  (NON-DIMENSIONAL)
C        FH     : HIGH FREQUENCY CUT-OFF
C        FS     : STOP BAND FREQUENCY
C        AP     : MAX. ATTENUATION IN PASS BAND
C        AS     : MIN. ATTENUATION IN STOP BAND
C
C      M. SAITO  (7/I/76)
C
      WL = AMIN1(ABS(FL),ABS(FH))*PI
      WH = AMAX1(ABS(FL),ABS(FH))*PI
      WS = ABS(FS)*PI
      IF( WL.EQ.0. .OR. WL.EQ.WH .OR. WH.GE.HP .OR. WS.EQ.0. .OR.
     *    WS.GE.HP .OR. (WS-WL)*(WS-WH).LE.0. )  GO TO  100
C****  DETERMINE N & C
      CLH= 1./(COS(WL)*COS(WH))
      OP = SIN(WH-WL)*CLH
      WW = TAN(WL)*TAN(WH)
      TS = TAN(WS)
      OS = ABS(TS-WW/TS)
      PA = AMIN1(ABS(AP),ABS(AS))
      SA = AMAX1(ABS(AP),ABS(AS))
      IF( PA.EQ.0. )  PA = 0.5
      IF( SA.EQ.0. )  SA = 5.
      N  = MAX0(2,IFIX(ABS(ALOG(PA/SA)/ALOG(OP/OS))+0.5))
      CC = EXP(ALOG(PA*SA)/FLOAT(N))/(OP*OS)
      C  = SQRT(CC)
      WW = WW*CC
C
      DP = HP/FLOAT(N)
      K  = N/2
      M  = K*2
      L  = 0
      G  = 1.
      FJ = 1.
C
      DO  2  J=1,K
        OJ = CMPLX(COS(DP*FJ),SIN(DP*FJ))*0.5
        FJ = FJ+2.
        CQ = CSQRT(OJ**2+WW)
        R(1) = OJ+CQ
        R(2) = OJ-CQ
        G  = G*CC
C
        DO  1  I=1,2
          RE =  REAL(R(I))**2
          RI = AIMAG(R(I))
          A  = 1./((C+RI)**2+RE)
          G  = G*A
            H(L+1) = 0.
            H(L+2) =-1.
            H(L+3) = 2.*((RI-C)*(RI+C)+RE)*A
            H(L+4) = ((RI-C)**2+RE)*A
            L = L+4
    1   CONTINUE
C
    2 CONTINUE
C****  EXIT
      GN = G
      IF( N.EQ.M )  RETURN
C****  FOR ODD N
      M = M+1
      WPC = CC*COS(WH-WL)*CLH
      WMC =-CC*COS(WH+WL)*CLH
          A  = 1./(WPC+C)
          GN = G*C*A
            H(L+1) = 0.
            H(L+2) =-1.
            H(L+3) = 2.*WMC*A
            H(L+4) = (WPC-C)*A
      RETURN
C****  ERROR
  100 WRITE(6,101)  FL,FH,FS
  101  FORMAT(/1X,5('?'),'   (BUTPAS)   INVALID INPUT   FL =',
     *        1PE14.6,3X,'FH =',E14.6,3X,'FS =',E14.6,3X,5('?')//)
      RETURN
      END
     
      SUBROUTINE  RECRES(H,M,GN,FM,DF,G,P,N)
      DIMENSION  H(*),G(N),P(N)
      DATA  PI/3.141593/,PI2/6.283185/
C
C      RECURSIVE FILTER FREQUENCY RESPONSE FUNCTION
C
C      ARGUMENTS
C        H      : COEFFICIENTS OF FILTER
C        M      : ORDER OF FILTER
C        GN     : GAIN FACTOR
C        FM     : MIN. FREQUENCY  (NON-DIMENSIONAL)
C        DF     : STEP OF FREQUENCY ; FREQ = FM,FM+DF,...,FM+(N-1)DF
C        G      : POWER TRANSFER FUNCTION
C        P      : PHASE DELAY
C        N      : NUMBER OF G & P
C
C      M. SAITO  (13/XII/75)
C
      IF( M.LE.0 .OR. N.LE.0 )  GO TO  4
      K  = M*4
      C  = GN**2
      CS = COS(FM*PI2)
      SN = SIN(FM*PI2)
      DC =-2.*SIN(DF*PI)**2
      DS = SIN(DF*PI2)
C
      DO  3  I=1,N
        GG = C
        PP = 0.
C
        DO  1  J=1,K,4
C****  NUMERATOR
          A  = (H(J+1)+1.)*CS+H(J)
          B  = (H(J+1)-1.)*SN
          GG = GG*(A**2+B**2)
          IF( GG.LE.0. )  GO TO  2
          PP = PP+ATAN2(B,A)
C****  DENOMINATOR
          A  = (H(J+3)+1.)*CS+H(J+2)
          B  = (H(J+3)-1.)*SN
          GG = GG/(A**2+B**2)
          PP = PP-ATAN2(B,A)
C****  REDUCE PHASE
          IF( ABS(PP).GT.PI )  PP = PP-SIGN(PI2,PP)
    1   CONTINUE
C
    2   G(I) = GG
        P(I) = PP
C****  STEP UP FREQUENCY
        W  = CS
        CS = CS*DC-SN*DS+CS
        SN = W *DS+SN*DC+SN
        W  = (1.-CS**2-SN**2)*0.5
        CS = CS*W+CS
        SN = SN*W+SN
    3 CONTINUE
C
      RETURN
C****  ERROR
    4 WRITE(6,5)  M,N
    5  FORMAT(//1X,5('?'),3X,'(RECRES)',3X,'INVALID INPUT',3X,'M =',I5,
     *        3X,'N =',I5,3X,5('?')//)
      RETURN
      END

      
      SUBROUTINE  BUTHIP(H,M,GN,N,FP,FS,AP,AS)
      DIMENSION  H(*)
      DATA  PI/3.141593/,HP/1.570796/
C
C      BUTTERWORTH HIGH PASS FILTER COEFFICIENT
C
C      ARGUMENTS
C        H      : FILTER COEFFICIENTS
C        M      : ORDER OF FILTER  (M=(N+1)/2)
C        GN     : GAIN FACTOR
C        N      : ORDER OF BUTTERWORTH FUNCTION
C        FP     : PASS BAND FREQUENCY  (NON-DIMENSIONAL)
C        FS     : STOP BAND FREQUENCY
C        AP     : MAX. ATTENUATION IN PASS BAND
C        AS     : MIN. ATTENUATION IN STOP BAND
C
C      M. SAITO  (7/I/76)
C
      WP = AMAX1(ABS(FP),ABS(FS))*PI
      WS = AMIN1(ABS(FP),ABS(FS))*PI
      IF( WS.EQ.0. .OR. WS.EQ.WP .OR. WP.GE.HP )  GO TO  100
C****  DETERMINE N & C
      TP = TAN(WP)
      TS = TAN(WS)
      PA = AMIN1(ABS(AP),ABS(AS))
      SA = AMAX1(ABS(AP),ABS(AS))
      IF( PA.EQ.0. )  PA = 0.5
      IF( SA.EQ.0. )  SA = 5.
      N  = MAX0(2,IFIX(ABS(ALOG(SA/PA)/ALOG(TP/TS))+0.5))
      CC = EXP(ALOG(PA*SA)/FLOAT(N))*TP*TS
      C  = SQRT(CC)
C
      DP = HP/FLOAT(N)
      M  = N/2
      K  = M*4
      G  = 1.
      FJ = 1.
      C2 =-2.*(1.-C)*(1.+C)
C
      DO  1  J=1,K,4
        SJ = COS(DP*FJ)**2
        TJ = SIN(DP*FJ)
        FJ = FJ+2.
        A  = 1./((C+TJ)**2+SJ)
        G  = G*A
          H( J ) =-2.
          H(J+1) = 1.
          H(J+2) = C2*A
          H(J+3) = ((C-TJ)**2+SJ)*A
    1 CONTINUE
C****  EXIT
      GN = G
      IF( MOD(N,2).EQ.0 )  RETURN
C****  FOR ODD N
      M = M+1
        GN = G/(C+1.)
          H(K+1) =-1.
          H(K+2) = 0.
          H(K+3) = (C-1.)/(C+1.)
          H(K+4) = 0.
      RETURN
C****  ERROR
  100 WRITE(6,101)  FP,FS
  101  FORMAT(/1X,5('?'),'   (BUTHIP)   INVALID INPUT   FP =',
     *        1PE14.6,3X,'FS =',E14.6,3X,5('?')//)
      RETURN
      END

      SUBROUTINE  BUTLOP(H,M,GN,N,FP,FS,AP,AS)
      DIMENSION  H(*)
      DATA  PI/3.141593/,HP/1.570796/
C
C      BUTTERWORTH LOW PASS FILTER COEFFICIENT
C
C      ARGUMENTS
C        H      : FILTER COEFFICIENTS
C        M      : ORDER OF FILTER  (M=(N+1)/2)
C        GN     : GAIN FACTOR
C        N      : ORDER OF BUTTERWORTH FUNCTION
C        FP     : PASS BAND FREQUENCY  (NON-DIMENSIONAL)
C        FS     : STOP BAND FREQUENCY
C        AP     : MAX. ATTENUATION IN PASS BAND
C        AS     : MIN. ATTENUATION IN STOP BAND
C
C      M. SAITO  (17/XII/75)
C
      WP = AMIN1(ABS(FP),ABS(FS))*PI
      WS = AMAX1(ABS(FP),ABS(FS))*PI
      IF( WP.EQ.0. .OR. WP.EQ.WS .OR. WS.GE.HP )  GO TO  100
C****  DETERMINE N & C
      TP = TAN(WP)
      TS = TAN(WS)
      PA = AMIN1(ABS(AP),ABS(AS))
      SA = AMAX1(ABS(AP),ABS(AS))
      IF( PA.EQ.0. )  PA = 0.5
      IF( SA.EQ.0. )  SA = 5.
      N  = MAX0(2,IFIX(ABS(ALOG(PA/SA)/ALOG(TP/TS))+0.5))
      CC = EXP(ALOG(PA*SA)/FLOAT(N))/(TP*TS)
      C  = SQRT(CC)
C
      DP = HP/FLOAT(N)
      M  = N/2
      K  = M*4
      G  = 1.
      FJ = 1.
      C2 = 2.*(1.-C)*(1.+C)
C
      DO  1  J=1,K,4
        SJ = COS(DP*FJ)**2
        TJ = SIN(DP*FJ)
        FJ = FJ+2.
        A  = 1./((C+TJ)**2+SJ)
        G  = G*A
          H( J ) = 2.
          H(J+1) = 1.
          H(J+2) = C2*A
          H(J+3) = ((C-TJ)**2+SJ)*A
    1 CONTINUE
C****  EXIT
      GN = G
      IF( MOD(N,2).EQ.0 )  RETURN
C****  FOR ODD N
      M  = M+1
        GN = G/(1.+C)
          H(K+1) = 1.
          H(K+2) = 0.
          H(K+3) = (1.-C)/(1.+C)
          H(K+4) = 0.
      RETURN
C****  ERROR
  100 WRITE(6,101)  FP,FS
  101  FORMAT(/1X,5('?'),'   (BUTLOP)   INVALID INPUT   FP =',
     *        1PE14.6,3X,'FS =',E14.6,3X,5('?')//)
      RETURN
      END

      SUBROUTINE  BUTSTP(H,M,GN,N,FL,FH,FS,AP,AS)
      COMPLEX  R(2),OJ,CQ
      DIMENSION  H(*)
      DATA  PI/3.141593/,HP/1.570796/
C
C      BUTTERWORTH BAND STOP FILTER COEFFICIENT
C
C      ARGUMENTS
C        H      : FILTER COEFFICIENTS
C        M      : ORDER OF FILTER
C        GN     : GAIN FACTOR
C        N      : ORDER OF BUTTERWORTH FUNCTION
C        FL     : LOW  FREQUENCY CUT-OFF  (NON-DIMENSIONAL)
C        FH     : HIGH FREQUENCY CUT-OFF
C        FS     : STOP BAND FREQUENCY
C        AP     : MAX. ATTENUATION IN PASS BAND
C        AS     : MIN. ATTENUATION IN STOP BAND
C
C      M. SAITO  (7/I/76)
C
      WL = AMIN1(ABS(FL),ABS(FH))*PI
      WH = AMAX1(ABS(FL),ABS(FH))*PI
      WS = ABS(FS)*PI
      IF( WL.EQ.0. .OR. WL.EQ.WH .OR. WH.GE.HP .OR.
     *   (WS-WL)*(WS-WH).GE.0. )  GO TO  100
C****  DETERMINE N & C
      CLH= 1./(COS(WL)*COS(WH))
      OP = SIN(WH-WL)*CLH
      WW = TAN(WL)*TAN(WH)
      TS = TAN(WS)
      OS = ABS(TS-WW/TS)
      PA = AMIN1(ABS(AP),ABS(AS))
      SA = AMAX1(ABS(AP),ABS(AS))
      IF( PA.EQ.0. )  PA = 0.5
      IF( SA.EQ.0. )  SA = 5.
      N  = MAX0(2,IFIX(ABS(ALOG(SA/PA)/ALOG(OP/OS))+0.5))
      CC = EXP(-ALOG(PA*SA)/FLOAT(N))/(OP*OS)
      C  = SQRT(CC)
C
      WW = WW*CC
      DP = HP/FLOAT(N)
      K  = N/2
      M  = K*2
      L  = 0
      G  = 1.
      FJ = 1.
      WPC= CC*COS(WH-WL)*CLH
      WMC=-CC*COS(WH+WL)*CLH
      A0 = WPC
      A2 = A0**2
      A1 = 2.*WMC/A0
C
      DO  2  J=1,K
        OJ = CMPLX(-COS(DP*FJ),SIN(DP*FJ))*0.5
        FJ = FJ+2.
        CQ = CSQRT(OJ**2+WW)
        R(1) = OJ+CQ
        R(2) = OJ-CQ
        G  = G*A2
C
        DO  1  I=1,2
          RE =  REAL(R(I))**2
          RI = AIMAG(R(I))
          A  = 1./((C+RI)**2+RE)
          G  = G*A
            H(L+1) = A1
            H(L+2) = 1.
            H(L+3) = 2.*((RI-C)*(RI+C)+RE)*A
            H(L+4) = ((RI-C)**2+RE)*A
            L = L+4
    1   CONTINUE
C
    2 CONTINUE
C****  EXIT
      GN = G
      IF( N.EQ.M )  RETURN
C****  FOR ODD N
      M = M+1
          A  = 1./(WPC+C)
          GN = G*A0*A
            H(L+1) = A1
            H(L+2) = 1.
            H(L+3) = 2.*WMC*A
            H(L+4) = (WPC-C)*A
C
      RETURN
C****  ERROR
  100 WRITE(6,101)  FL,FH,FS
  101  FORMAT(/1X,5('?'),'   (BUTSTP)   INVALID INPUT   FL =',
     *        1PE14.6,3X,'FH =',E14.6,3X,'FS =',E14.6,3X,5('?')//)
      RETURN
      END

      SUBROUTINE  TANDEM(X,Y,N,H,M,GN,NML)
      DIMENSION  X(N),Y(N),H(*)
C
C      RECURSIVE FILTERING IN SERIES
C
C      ARGUMENTS
C        X      : INPUT TIME SERIES
C        Y      : OUTPUT TIME SERIES  (MAY BE EQUIVALENT TO X)
C        N      : LENGTH OF X & Y
C        H      : COEFFICIENTS OF FILTER
C        M      : ORDER OF FILTER
C        NML    : >0 ; FOR NORMAL  DIRECTION FILTERING
C                 <0 ;     REVERSE DIRECTION FILTERING
C
C      SUBROUTINE REQUIRED : RECFIL
C
C      M. SAITO  (6/XII/75)
C
      IF( N.LE.0 .OR. M.LE.0 )  GO TO  2
C****  1-ST CALL
      CALL  RECFIL(X,Y,N,H,NML)
      IF( M.GT.1 )  THEN
C****  2-ND AND AFTER
         DO  I=2,M
           CALL  RECFIL(Y,Y,N,H(I*4-3),NML)
         ENDDO
      ENDIF
C
      DO I=1,N
         Y(I) = GN*Y(I)
      ENDDO
      RETURN
C****  ERROR
    2 WRITE(6,3)  N,M
    3  FORMAT(//1X,5('?'),3X,'(TANDEM)',3X,'INVALID INPUT',3X,'N =',I5,
     *        3X,'M =',I5,5('?')//)
      RETURN
      END

      SUBROUTINE  RECFIL(X,Y,N,H,NML)
      DIMENSION  X(N),Y(N),H(*)
C
C      RECURSIVE FILTERING : F(Z) = (1+A*Z+AA*Z**2)/(1+B*Z+BB*Z**2)
C
C      ARGUMENTS
C        X      : INPUT TIME SERIES
C        Y      : OUTPUT TIME SERIES  (MAY BE EQUIVALENT TO X)
C        N      : LENGTH OF X & Y
C        H      : FILTER COEFFICIENTS ; H(1)=A, H(2)=AA, H(3)=B, H(4)=BB
C        NML    : >0 ; FOR NORMAL  DIRECTION FILTERING
C                 <0 ; FOR REVERSE DIRECTION FILTERING
C
C      M. SAITO  (6/XII/75)
C
      IF(   N.LE.0 )  GO TO  4
      IF( NML.GE.0 )  GO TO  1
C****  REVERSE FILTERING
      J  = N
      JD =-1
        GO TO  2
C****  NORMAL FILTERING
    1 J  = 1
      JD = 1
C
    2 A  = H(1)
      AA = H(2)
      B  = H(3)
      BB = H(4)
      U1 = 0.
      U2 = 0.
      V1 = 0.
      V2 = 0.
C****  FILTERING
      DO  3  I=1,N
        U3 = U2
        U2 = U1
        U1 = X(J)
        V3 = V2
        V2 = V1
        V1 = U1+A*U2+AA*U3-B*V2-BB*V3
        Y(J) = V1
        J  = J+JD
    3 CONTINUE
C****  EXIT
      RETURN
C****  ERROR
    4 WRITE(6,5)  N
    5  FORMAT(//1X,5('?'),3X,'(RECFIL)',3X,'INVALID INPUT',3X,'N =',I5,
     *        3X,5('?')//)
      RETURN
      END
