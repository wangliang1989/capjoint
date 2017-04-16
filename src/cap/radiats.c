/****************************************************************
   compute horizontal radiation patten for
	double-couple	specified by az,dip,rake
	single force	specified by az and dip
	dc/sf		specified by moment tensor and az
  az is the station azimuth measured from the strike

  rad[3][3] order is rad[45,DD,SS][Z,R,T]
*****************************************************************/
#include<math.h>

#define RAD  1.745329252e-2  /*degree to rad*/

/*******************************************************************
horizontal radiation coefficients of a double-couple, Haskell'64
with tangential corrected

Algorithm:
 V/R = f3n3*Z0
     +((f1n3+f3n1)*cos(theta)+(f2n3+f3n2)*sin(theta))*Z1
     +((f2n2-f1n1)*cos(2theta)+(-f1n2-f2n1)*sin(2theta))*Z2
  T =-((f1n3+f3n1)*sin(theta)-(f2n3+f3n2)*cos(theta))*T1
     -((f2n2-f1n1)*sin(2theta)-(-f1n2-f2n1)*cos(2theta))*T2

where theta=pi/2-az and az is the azimuth of the station measured from
the strike of the fault clockwise
  n = (sin(delta),0,cos(delta))
  F = (-sin(lamda)*cos(delta), cos(lamda), sin(lamda)*sin(delta))
where delta is dip from the horizontal, lambda is the rake from the
strike CCW.

********************************************************************/
void	dc_radiat(float stk,float dip,float rak,float rad[3][3]) {
   float sstk,sdip,srak,sstk2,sdip2;
   float cstk,cdip,crak,cstk2,cdip2;
   stk*=RAD; dip*=RAD; rak*=RAD;
   sstk=sin(stk);cstk=cos(stk);
   sdip=sin(dip);cdip=cos(dip);
   srak=sin(rak);crak=cos(rak);
   sstk2=2*sstk*cstk; cstk2=cstk*cstk-sstk*sstk;
   sdip2=2*sdip*cdip; cdip2=cdip*cdip-sdip*sdip;
   rad[0][0]=0.5*srak*sdip2;
   rad[0][1]=rad[0][0];
   rad[0][2]=0.;
   rad[1][0]=-sstk*srak*cdip2+cstk*crak*cdip;
   rad[1][1]=rad[1][0];
   rad[1][2]= cstk*srak*cdip2+sstk*crak*cdip;
   rad[2][0]=-sstk2*crak*sdip-0.5*cstk2*srak*sdip2;
   rad[2][1]=rad[2][0];
   rad[2][2]=cstk2*crak*sdip-0.5*sstk2*srak*sdip2;
}

/******************************************************
horizontal radiation coefficients of a single-force
   In:
	stk: strike_of_obs w.r.t to strike of the force
		 measured clockwise
	dip: dip of the force, from horizontal down

   algorithm:
	vertical (UP) = f3*Z0 + (f1*cos(theta)+f2*sin(theta))*Z1
	radial  (OUT) = f3*R0 + (f1*cos(theta)+f2*sin(theta))*R1
	tangen   (CW) =       - (f1*sin(theta)-f2*cos(theta))*T1
    where F = (0,cos(dip),-sin(dip))
******************************************************/
void	sf_radiat(float stk,float dip,float rad[3][3]) {
   float sstk,sdip,cstk,cdip;
   stk*=RAD; dip*=RAD;
   sstk=sin(stk);cstk=cos(stk);
   sdip=sin(dip);cdip=cos(dip);
   rad[0][0]=-sdip;
   rad[0][1]=rad[0][0];
   rad[0][2]=0.;
   rad[1][0]=cdip*cstk;
   rad[1][1]=rad[1][0];
   rad[1][2]=cdip*sstk;
   rad[2][0]=0.;
   rad[2][1]=0.;
   rad[2][2]=0.;
}

/* horizontal radiation coefficients from moment m at azimuth az*/
void	mt_radiat(float az, float m[3][3], float rad[3][3]) {
   float saz, caz, saz2, caz2;
   az *= RAD;
   saz = sin(az); caz = cos(az);
   saz2 = 2*saz*caz; caz2 = caz*caz-saz*saz;
   rad[2][0] = rad[2][1] = -0.5*(m[0][0]-m[1][1])*caz2 - m[0][1]*saz2; 
   rad[1][0] = rad[1][1] = -m[0][2]*caz - m[1][2]*saz;
   rad[0][0] = rad[0][1] =  (2.*m[2][2]-m[0][0]-m[1][1])/6.;
   rad[2][2] = -0.5*(m[0][0]-m[1][1])*saz2 + m[0][1]*caz2;
   rad[1][2] = -m[0][2]*saz + m[1][2]*caz;
   rad[0][2] = 0.;
}
