#include <stdio.h> 
#include <math.h> 

/***************************************************************
  forward transform from x (North), y (East), z (Down) to
  x' (Slip, n2), y', z' (-n1) :
 crak*cstr+cdip*srak*sstr,-cdip*cstr*srak + crak*sstr,-sdip*srak
 cstr*srak-cdip*crak*sstr, cdip*crak*cstr + srak*sstr, crak*sdip
 sdip*sstr,               -cstr*sdip,                  cdip

 inverse transform 
 crak*cstr+cdip*srak*sstr, cstr*srak-cdip*crak*sstr, sdip*sstr
 crak*sstr-cdip*cstr*srak, cdip*crak*cstr+srak*sstr,-cstr*sdip
 -sdip*srak,               crak*sdip,                cdip

 rake mesured anticlockwise from the strike direction
****************************************************************/

double  dg=0.0174532925199433;
double	pi=3.141592653589793;
double  pi2=1.570796326794897;

main (int argc, char **argv) {
int	i;
double	s2,str,dip,rak,sstr,sdip,srak,cstr,cdip,crak,p[3],t[3],n1[3],n2[3];
void	fault(double *, double *),angle(double *),vector(double *);

s2 = 1./sqrt(2.);
if (argc == 4) {
      sscanf(argv[1],"%lf",&str);
      sscanf(argv[2],"%lf",&dip);
      sscanf(argv[3],"%lf",&rak);
      str=str*dg;dip=dip*dg;rak=rak*dg;
      sstr=sin(str);cstr=cos(str);
      sdip=sin(dip);cdip=cos(dip);
      crak=cos(rak);srak=sin(rak);
      /* n1 is the normal vector of fault plane */
      n1[0]=-sstr*sdip;
      n1[1]= cstr*sdip;
      n1[2]=-cdip;
      /* n2 is the normal vector of axu. plane, also the slip vector */
      n2[0]= crak*cstr+srak*sstr*cdip;
      n2[1]= crak*sstr-srak*cstr*cdip;
      n2[2]=-srak*sdip;
      fault(n1,n2);
      for (i=0;i<3;i++) {
          p[i] = s2*(n1[i]-n2[i]);
          t[i] = s2*(n1[i]+n2[i]);
      }
      angle(p);
      angle(t);
}
else if (argc==5) {
      for(i=0;i<2;i++) {
         sscanf(argv[1+i],"%lf",&p[i]);p[i]*=dg; 
         sscanf(argv[3+i],"%lf",&t[i]);t[i]*=dg; 
      }
      vector(p);
      vector(t);
      for (i=0;i<3;i++) {
	  n1[i] = (p[i] + t[i])*s2;
	  n2[i] = (t[i] - p[i])*s2;
      }
      fault(n1,n2);
      fault(n2,n1);
}
else
      puts("Input: strike dip rake or p_azimuth p_plunge t_azimuth t_plunge");
puts(" ");
}

void	vector( double *a) {
  double aaaa;
  a[2] = sin(a[1]);
  aaaa = cos(a[1]);
  a[1] = aaaa*sin(a[0]);
  a[0] = aaaa*cos(a[0]);
}

void	angle( double *p) {
      int	i;
      double aaaa;
      if(p[2]<0.) for(i=0;i<3;i++) p[i]=-p[i];
      p[2]=asin(p[2]);
      aaaa=cos(p[2]);
      p[0]=atan2(p[1]/aaaa,p[0]/aaaa)/dg;
      p[1]=p[2]/dg;
      printf("%7.1f%7.1f",p[0],p[1]);
}

void	fault(double *n1, double *n2) {
      int	i;
      double str,dip,rak,sdip;
      if (n2[2]>0.) for(i=0;i<3;i++) {n1[i]=-n1[i];n2[i]=-n2[i];}
      dip= acos(-n2[2]);
      sdip = sin(dip);
      str = atan2(-n2[0]/sdip, n2[1]/sdip);if (str<0.) str+=2*pi;
      rak = atan2(-n1[2]/sdip,n1[0]*cos(str)+n1[1]*sin(str));
      printf("%7.1f%7.1f%7.1f",str/dg,dip/dg,rak/dg);
}
