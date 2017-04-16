#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "cap.h"

/* from fault plane to moment tensor, A&R P 117 */
void fdtensor(MECA meca,float tensor[3][3])
{
  float str,dip,rake;
  float cstr,cdip,crak;
  float sstr,sdip,srak;
  
  str=meca.stk*RAD;dip=meca.dip*RAD;rake=meca.rak*RAD;
  sstr=sin(str);cstr=cos(str);
  sdip=sin(dip);cdip=cos(dip);
  crak=cos(rake);srak=sin(rake);
  tensor[0][0]=-sdip*crak*sin(2*str)-sin(2*dip)*srak*sstr*sstr;
  tensor[0][1]=tensor[1][0]=sdip*crak*cos(2*str)+0.5*sin(2*dip)*srak*sin(2*str);
  tensor[0][2]=tensor[2][0]=-cdip*crak*cstr-cos(2*dip)*srak*sstr;
  tensor[1][1]=sdip*crak*sin(2*str)-sin(2*dip)*srak*cstr*cstr;
  tensor[1][2]=tensor[2][1]=cos(2*dip)*srak*cstr-cdip*crak*sstr;
  tensor[2][2]=sin(2*dip)*srak;
}

/* radiation pattern from a double couple,  A & R, P118*/
float radpmt(float mom[3][3], FM fm)
{
  float	dir[3], wave[3], sth, cth, cphi, sphi;
  int	m,n;
  
  sth=sin(fm.alpha*RAD);
  cth=cos(fm.alpha*RAD);
  cphi=cos(fm.az*RAD);
  sphi=sin(fm.az*RAD);
  dir[0]=sth*cphi;
  dir[1]=sth*sphi;
  dir[2]=cth;
  switch(abs(fm.type)) {
  case 1:
    wave[0]=dir[0];
    wave[1]=dir[1];
    wave[2]=dir[2];
    break;
  case 2:
    wave[0]=cth*cphi;
    wave[1]=cth*sphi;
    wave[2]=-sth;
    break;
  case 3:
    wave[0]=-sphi;
    wave[1]=cphi;
    wave[2]=0;
  break;
  default:
    fprintf(stderr,"wrong phase type %d\n",fm.type);
    return 0.;
  }
  
  sth=0;
  for(m=0;m<3;m++){
    for(n=0;n<3;n++){
      sth+=mom[m][n]*dir[m]*wave[n];
    }
  }
  return sth;
}
