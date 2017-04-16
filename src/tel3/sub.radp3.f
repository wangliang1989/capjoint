      SUBROUTINE RADP3(iFault,AZ,D0,A0,P,PD,PU,SVD,SVU,SHD,SHU,VP,VS)
*  < Radiation pattern >
*  -----------------------------------
*        PD,PU for P,p
*        SD,SU for S,s(SV&SH)
*     Polarity
*        SH : Clockwise
*        SV : SH x P
*  -----------------------------------
*  May 29,1990
*    adding radiation pattern for isotropic moment-tensor
*        for D0 > 360
*  -----------------------------------
      IF(D0.GT.360.) THEN
         PD=1
         PU=1
         SVD=0
         SVU=0
         SHD=0
         SHU=0
      RETURN
      ENDIF
      RAD=.0174533
      DR0=D0*RAD
      AR0=A0*RAD
      THET=AZ*RAD
      SIH=P*VP
      IF(SIH.GE.1.) CIH=0.
      IF(SIH.LT.1.) CIH=SQRT(1-SIH**2)
      SIH2=P*VS
      CIH2=SQRT(1-SIH2**2)
C     P2=P*P
C   *** C PARAMETERS
      C1=SIH**2
      C2=-2.*SIH*CIH
      C3=2-3*SIH**2
C    *** SV PARAMETERS
      SV1=SIH2*CIH2
      SV2=2*SIH2**2-1
      SV3=-3*SIH2*CIH2
C    *** SH PARAMETERS
      SH1=SIH2
      SH2=CIH2
C    ***** A PARAMETERS
      THE2 =THET*2.
      DR02=DR0*2.
      STH=SIN(THET)
      CTH=COS(THET)
      STH2=SIN(THE2 )
      CTH2=COS(THE2 )
      SD0=SIN(DR0)
      CD0=COS(DR0)
      SD02=SIN(DR02)
      CD02=COS(DR02)
      CA0=COS(AR0)
      SA0=SIN(AR0)
      A1=STH2*CA0*SD0+.5*CTH2*SA0*SD02
      A2=CTH*CA0*CD0-STH*SA0*CD02
      A3=.5*SA0*SD02
      A4=CTH2*CA0*SD0-.5*STH2*SA0*SD02
      A5=STH*CA0*CD0+CTH*SA0*CD02
*
      PD =A1*C1+A2*C2+A3*C3
      PU =A1*C1-A2*C2+A3*C3
      SVD =A1*SV1+A2*SV2+A3*SV3
      SVU =-A1*SV1+A2*SV2-A3*SV3
      SHD =A4*SH1+A5*SH2
      SHU =A4*SH1-A5*SH2
c	in order to generate 3 types of wave A1(R,Z),A4(T);
c	A2(R,Z),A5
C	A3(R,Z)
	if(iFault.eq. 3) then
	PD =  -1*C1
	PU =  -1*C1
	SVD=  -1*SV1
	SVU= SV1
	SHD=SH1
	SHU=SH1
	endif
	if(iFault.eq. 2) then
	PD = C2
	PU = -C2
	SVD=SV2
	SVU= SV2
	SHD=SH2
	SHU=-SH2
	endif
	if(iFault.eq. 1) then
	PD =  C3
	PU =  C3
	SVD= SV3
	SVU= -1* SV3
	SHD=0
	SHU=0
	endif

      END
