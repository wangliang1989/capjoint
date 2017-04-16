/****************************************************************
				cap.c
  grid-search for source mechanisms, The code uses 5 segments of
  3-component waveforms to determined the focal mechanics (mw,
  strike, dip, rake). It applies different filter to Pnl and surface waves.

  requires:
	Green's function -- has P and S arrival time set (t1 and t2 in SAC header)
  optional:
	Data		 -- P pick (A in SAC header) --> align with t1
			 -- Pnl window (t1 and t2)
			 -- P-SV, SH window (t3 and t4)

  Modify history:
  June 19, 1998	Lupei Zhu	modified from srct.c
  July  9, 1998 Lupei Zhu	use different velocities for love and rayleigh 
  July 16, 1998 Lupei Zhu	improve m0 estimation using parabola near mimimum
  July 19, 1998 Lupei Zhu	allow inputs for time shifts
  July 26, 1998 Lupei Zhu	taper waveforms (w=0.4)
  Jan. 29, 1998 Lupei Zhu	add option of repeat inversion after discard bad comp.
  Nov.  9, 1999 Lupei Zhu	absorb shft_pnl into constant shift
				use rint() to convert float (t/dt) to int
  Dec   2, 1999 Lupei Zhu	taper waveform before conv() use w=0.3
  Dec  27, 1999 Lupei Zhu	compute windows using apparent Vp, Vs
  June 28, 2000 Lupei Zhu	switch to new greens function format
  Feb  15, 2001 Lupei Zhu	taper waveform after conv.
  June 27, 2001 Lupei Zhu	add fm_thr (firt-motion threshold)
  July 16, 2001 Lupei Zhu	add directivity option
  Jan. 02, 2002 Lupei Zhu	add an input for number of freedom per sec (nof_per_samp)
  Oct. 31, 2002 Lupei Zhu	use abolute time in the output sac files
  July 18, 2003 Lupei Zhu	use Butterworth filters to band-pass data
  July 30, 2003 Lupei Zhu	not absorb shft_pnl into constant shift
  Aug. 18, 2003 Lupei Zhu	normalize L2 of misfit by # of points
  Aug. 21, 2003 Lupei Zhu	tie SH and SV using variable tie (0-0.5)
  Sep. 28, 2003 Lupei Zhu	use P and S take-off angles in hd.user1/user2
  Oct. 12, 2003 Lupei Zhu	output other local minimums whose
				misfit-min is less than mltp*sigma.
  Apr. 06, 2004 Lupei Zhu	allow inputing a SAC source time function src.sac
  Sep. 06, 2004 Lupei Zhu	make cap to run above the event_dir level

  Known bugs:
  free(data) and free(green) can fail if data or green is not allocated in read_sac()

****************************************************************/
#include <stdio.h> 
#include <stdlib.h>
#include <math.h> 
#include <string.h>
#include <float.h>
#include "cap.h"
#include "sac.h"
#include "Complex.h"

#define NRANSI
#include "nr.h"
#include "nrutil.h"

#define MAXFLEN 100

char    cm[3]={'t','r','z'};    /* component names, so 0->SH,1->R,2->Z */
int     kd[5]={0,1,2,1,2};      /* SH, SVr, SVz, Pnlr, Pnlz */
int     kk[5]={0,2,5,2,5};      /* index of 1st Greens' fn for each segment */
int	discard_bad_data(int,DATA *,SOLN,float,float *);
char	grn_com[8] = {'8','5','7','4','1','6','3','0'};

int main (int argc, char **argv) {
  int 	i,j,k,k1,l,m,nda,npt,plot,output,kc,nfm,useDisp,dof;
  int	m_pnl,m_sw,wndw_size,ns,fwd=1,mltp;
  int	nup, up[3], total_n, n_shft;
  int	shft_pnl, shft_love, shft_rayleigh, shft_max, shft_min, shft0[STN][5];
  int	t0,te,t1,t2,t3,t4,n1,n2,mm[2],t[5],n[5],max_shft[5];
  int	repeat = 0;
  int   hi,hj,hk,index;
  char	dst0[10],dst[10],tmp[80],tmp1[80],path[80],*c_pt;
  float	x,x1,x2,y,y1,amp,dt,rad[5],arad[3][3],m0,dm,fm_thr,tie;
  float	rms_cut[5];
  float strike,dip,rake,solution;
  float *data[3], *green[8];
  int	n_data, n_green;
  float	bs_body,bs_surf,bs[5],weight,nof_per_samp;
  float	w_pnl[5];
  float	distance,dmin=100.,vp,vs1,vs2,depSqr=25,con_shft[STN];
  float	*syn,*f_pt,*f_pt0,*f_pt1;
  GRID	grid;
  COMP	*spt;
  DATA	*obs, *obs0;
  FM	*fm, *fm0;
  SOLN	sol, sol1, sol2;
  SACHEAD hd;
  FILE 	*f_out,*ff_out;
  float tau0, riseTime, *src;
  float f1_pnl, f2_pnl, f1_sw, f2_sw;
  float g_pnl, g_sw, h_pnl[MAXFLEN], h_sw[MAXFLEN];
  void  buttbp(float *,int *,float *,float,float);
  void  tandem_(float *,float *,int *,float *,int *,float *,int *);
  void  principal_values(float *);
#ifdef DIRECTIVITY
  int ns_pnl, ns_sw;
  float *src_pnl, *src_sw;
  float tau, faultStr, faultDip, rupDir, rupV, pVel, sVel, temp;
  scanf("%f%f%f%f%f",&pVel,&sVel,&riseTime,&tau0,&rupDir);
  rupDir *= RAD;
  rupV = 0.8*sVel;
#endif
  
  /****** input control parameters *************/
  scanf("%f%f%f%f%d%f%f",&x1,&y1,&x,&y,&repeat,&fm_thr,&tie);
//  fprintf(stderr,"x1,y1, etc %f %f %f %f %d %f %f\n", x1, y1, x, y, repeat, fm_thr, tie);
  if (repeat) for(j=0;j<5;j++) scanf("%f",rms_cut+4-j);
  scanf("%f%f%f",&vp,&vs1,&vs2);
  scanf("%f%f%f%f",&bs_body,&bs_surf,&x2,&nof_per_samp);
  scanf("%d%d",&plot,&output);
  scanf("%d%d",&useDisp,&mltp);
  scanf("%s",path); strcat(strcat(path,argv[2]),"/");

  fprintf(stderr,"%d %d %d %d\n",plot,output,useDisp,mltp);
  /*** input source functions and filters for pnl and sw ***/
  scanf("%f",&dt);
  if (dt>0.) {
     scanf("%f%f",&tau0,&riseTime);
     src = trap(tau0, riseTime, dt, &ns);
  } else {
     scanf("%s",tmp);
     if ((src = read_sac(tmp,&hd)) == NULL) {
        fprintf(stderr,"fail to read in source time: %s\n",tmp);
        return -1;
     }
     dt = hd.delta;
     ns = hd.npts;
  }
  scanf("%f%f%f%f",&f1_pnl,&f2_pnl,&f1_sw,&f2_sw);
  buttbp(h_pnl,&m_pnl,&g_pnl,dt*f1_pnl,dt*f2_pnl);
  buttbp(h_sw ,&m_sw ,&g_sw ,dt*f1_sw ,dt*f2_sw );

  /** max. window length, shift, and weight for Pnl portion **/
  mm[0]=rint(x1/dt);
  max_shft[3]=max_shft[4]=2*rint(x/dt);
  w_pnl[3]=w_pnl[4]=x2;
  /** max. window length, shift, and weight for P-SV, SH **/
  mm[1]=rint(y1/dt);
  max_shft[0]=max_shft[1]=max_shft[2]=2*rint(y/dt);
  w_pnl[0]=w_pnl[1]=w_pnl[2]=1.;
  /** and tie of time shifts between SH and P-SV **/

  /** input grid-search range **/
  scanf("%f%f",&m0,&dm);
  for(j=0;j<3;j++) {
    scanf("%f%f%f",&x1,&x2,&grid.step[j]);
    grid.n[j] = rint((x2-x1)/grid.step[j]) + 1;
    grid.x0[j] = x1;
  }
  grid.err = (float *) malloc(grid.n[0]*grid.n[1]*grid.n[2]*sizeof(float));
  if (grid.err == NULL ) {
     fprintf(stderr,"fail to allocate memory for storing misfit errors\n");
     return -1;
  }

#ifdef DIRECTIVITY
  faultStr = grid.x0[0]*RAD;
  faultDip = grid.x0[1]*RAD;
#endif

  /** input number of stations **/
  scanf("%d",&nda);
  if (nda > STN) {
     fprintf(stderr,"number of station, %d, exceeds max., some stations are discarded\n",nda);
     nda = STN;
  }
  obs = obs0 = (DATA *) malloc(nda*sizeof(DATA));
  fm = fm0 = (FM *) malloc(3*nda*sizeof(FM));
  if (obs == NULL || fm == NULL) {
     fprintf(stderr,"fail to allocate memory for data\n");
     return -1;
  }
  
  /**** loop over stations *****/
  total_n = 0;
  n_shft = 0;
  nfm = 0;
  for(i=0;i<nda;i++) {

    n_data=0; n_green=0;

    /***** input station name and weighting factor ******/
    scanf("%s%s%s",tmp,dst0,dst);
    /*fprintf(stderr,"tmp %s dst0  %s  dst %s\n",tmp,dst0,dst); */
    for(nup=0,j=0;j<5;j++) {
       scanf("%d",&obs->com[4-j].on_off);
       nup += obs->com[4-j].on_off;
    }
    scanf("%f%f%f",&x,&x1,&x2);
//	fprintf(stderr,"x x1 x2 %f %f %f\n",x,x1,x2); 
/*    if (nup==0) goto next_sta;*/
    shft_pnl = rint(x/dt); shft_rayleigh = rint(x1/dt); shft_love = rint(x2/dt);
    shft_min=shft_love;if(shft_min>shft_rayleigh)shft_min=shft_rayleigh;
    shft_max=shft_love;if(shft_max<shft_rayleigh)shft_max=shft_rayleigh;
    nup = sscanf(tmp,"%[^/]/%d/%d/%d",obs->stn,&up[0],&up[1],&up[2]);
    if ( fm_thr > 1 ) nup = 1;

    /**************input waveforms************/
    strcat(strcat(strcat(strcpy(tmp,argv[1]),"/"),obs->stn), ".t");
    c_pt = strrchr(tmp,(int) 't');
    for(n_data=0;n_data<NRC;n_data++){
      *c_pt = cm[n_data];
      if ((data[n_data] = read_sac(tmp,&hd)) == NULL) goto next_sta;
    }
    obs->az = hd.az;
//   fprintf(stderr," az = %f\n",obs->az);
    obs->dist = distance = hd.dist;
    bs[3]=bs[4]=bs_body;
    bs[0]=bs[1]=bs[2]=bs_surf;
    if (strchr(dst,(int) '.')) bs[0]=bs[1]=bs[2]=bs_body;
    strcat(strcat(obs->stn,"_"),dst0);
    
    x = hd.b-hd.o;
    y = hd.e-hd.o;
    x1 = hd.a-hd.o;
    t1 = rint((hd.t1-hd.b)/dt);
    t2 = rint((hd.t2-hd.b)/dt);
    t3 = rint((hd.t3-hd.b)/dt);
    t4 = rint((hd.t4-hd.b)/dt);

    /**************compute source time function***********/
#ifdef DIRECTIVITY
    temp = hd.az*RAD-faultStr;
    temp = rupV*cos(temp)*cos(rupDir)-sin(temp)*sin(rupDir)*cos(faultDip);
    tau = tau0*(1-temp/pVel);
    src_pnl = trap(tau, riseTime, dt, &ns_pnl);
    tau = tau0*(1-temp/sVel);
    fprintf(stderr,"station %s %5.1f tauS %5.1f\n",obs->stn,hd.az,tau);
    src_sw  = trap(tau, riseTime, dt, &ns_sw);
#endif
    
    /************input green's functions***********/
    strcat(strcat(strcpy(tmp,path),dst),".grn.0");
    c_pt = strrchr(tmp,(int) '0');
    for(n_green=0;n_green<NGR;n_green++){
      *c_pt = grn_com[n_green];
      if ((green[n_green] = read_sac(tmp,&hd)) == NULL) goto next_sta;
      conv(src, ns, green[n_green], hd.npts);
    }

    /* generate first-motion polarity data */
    if (nup>1 && (hd.user1<0. || hd.user2<0.)) {
      fprintf(stderr,"No P/S take-off angle in Greens' function %s\n",tmp);
    } else {
      obs->alpha = hd.user1;
      for(j=1;j<nup;j++) {
        fm->type = up[j-1];
        fm->az = obs->az;
        if (abs(fm->type)==1)	fm->alpha = hd.user1;
	else 			fm->alpha = hd.user2;
        nfm++;
        fm++;
      }
    }

    /*** calculate time shift needed to align data and syn approximately ****/
    /* positive shift means synthetic is earlier */
    con_shft[i] = 0.; /*shft_pnl*dt;*/		/* shift the whole trace */
    if ( x1 > 0.) {			/* if first-arrival is picked in SAC */
       con_shft[i] += x1 - hd.t1;	/* use it to align with greens' fn*/
    }
/*    shft_pnl = 0;*/

    /** calculate time windows for Pnl and Surface wave portions **/
    /** Here we use beginning of greens' function traces as the ***/
    /** origin of time axis **/

    t0=rint((x-hd.b-con_shft[i])/dt) + 1;	/* beginning time of data */
    te=rint((y-hd.b-con_shft[i])/dt) - 1;	/* ending time of data */

    /* for Pnl portion */
 //   fprintf(stderr,"t0 %d  te= %d  shift=%f \n",t0,te,con_shft[i]);
    if (t1 > 0 && t2 > 0 ) {	/* using picked time window in data */
      t1+=t0;
      t2+=t0;
    } else {			/* using default time window in syn */
      if (vp > 0.)
	 t1=rint((sqrt(distance*distance+depSqr)/vp-hd.b)/dt - 0.3*mm[0]);	/* use vp to compute t1 */
      else
	 t1=rint((hd.t1-hd.b)/dt - 0.2*mm[0]);	/* use tp as t1 */
      t2=rint((hd.t2-hd.b)/dt+0.2*mm[0]);		/* ts plus some delay */
    }
    /* now make sure that t1 > start of green (0) && data (t0) */
    if (t1<t0) t1=t0;   if (t1<shft_pnl) t1=shft_pnl;
    /* and t2 < end of green (npts) && data (te) */
    if (t2>te) t2=te;   if(t2>hd.npts+shft_pnl) t2=hd.npts+shft_pnl;

    /* do same for surface wave portion */
    if (t3 > 0 && t4 >0 ) {
      t3+=t0;
      t4+=t0;
    } else {
      if (vs1 >0. && vs2 > 0.) {
	 t3=rint((sqrt(distance*distance+depSqr)/vs1-hd.b)/dt - 0.3*mm[1]);
	 t4=rint((sqrt(distance*distance+depSqr)/vs2-hd.b)/dt + 0.7*mm[1]);
	 if (t4>hd.npts) t4=hd.npts;
      }
      else {
         t3=rint((hd.t2-hd.b)/dt - 0.2*mm[1]);
         t4=t3+mm[1];
      }
    }
    if(t3<t1) t3=t1;	if (t3<t1-shft_pnl+shft_max) t3=t1-shft_pnl+shft_max;
    if(t4>te) t4=te;	if (t4>hd.npts+shft_min) t4=hd.npts+shft_min;

    /*calculate the time windows */
    n1 = t2 - t1;	/*Pnl*/
    n2 = t4 - t3;	/*PSV/SH*/
//    fprintf(stderr,"%d %d\n",n1,n2);

    if (n1>mm[0]) n1=mm[0];
    if (n2>mm[1]) n2=mm[1];
//    fprintf(stderr,"n1=%d n2=%d msft=%d %d\n",n1,n2,max_shft[0],max_shft[3]);
    /* if the window is too short, turn this portion off */
    if (n1<max_shft[3] && n2<max_shft[0]) {
      fprintf(stderr,"no time window for %s\n", obs->stn);
      goto next_sta;
    }
    else if (n2<max_shft[0]) {
      fprintf(stderr,"turn off surface wave for %s\n",obs->stn);
      obs->com[0].on_off=obs->com[1].on_off=obs->com[2].on_off=0;
      n2 = n1; t3 = t1; t4 = t2;
    }
    else if (n1<max_shft[3]) {
      fprintf(stderr,"turn off Pnl wave for %s\n",obs->stn);
      obs->com[3].on_off=obs->com[4].on_off=0;
      n1 = n2; t1 = t3; t2 = t4;
    }

    /***window data+Greens, do correlation and L2 norms **/
    t[0]=t3;		/* love wave */
    t[1]=t[2]=t4-n2;	/* rayleigh wave */
    t[3]=t[4]=t1;	/* Pnl */
    n[0]=n[1]=n[2]=n2;	n[3]=n[4]=n1;
//    fprintf(stderr,"%s %f %f\n",obs->stn,hd.b+t[3]*dt,n1*dt);
//    fprintf(stderr,"%s %f %f\n",obs->stn,hd.b+t[0]*dt,n2*dt);
//    fprintf(stderr,"%s %f %f\n",obs->stn,hd.b+t[1]*dt,n2*dt);
    shft0[i][0] = shft_love;
    shft0[i][1] = shft0[i][2] = shft_rayleigh;
    shft0[i][3] = shft0[i][4] = shft_pnl;
    if (obs->com[0].on_off>0) n_shft++;
    if (obs->com[1].on_off>0 || obs->com[2].on_off>0) n_shft++;
    if (obs->com[3].on_off>0 || obs->com[3].on_off>0) n_shft++;
/*    if (dmin<0.) dmin=distance;*/
    for(spt=obs->com,kc=2,j=0;j<NCP;j++,spt++,kc=3) {
      spt->npt = npt = n[j];
      spt->b = t[j]*dt+con_shft[i]+hd.b;
      if (spt->on_off) total_n+=npt;
      weight = w_pnl[j]*pow(distance/dmin,bs[j]);
      wndw_size = npt*sizeof(float);
      spt->rec = (float *) malloc(wndw_size);
      memcpy(spt->rec, data[kd[j]]+t[j]-t0, wndw_size);
      if (j<3) tandem_(spt->rec, spt->rec, &npt, h_sw, &m_sw, &g_sw, &fwd);
      else     tandem_(spt->rec, spt->rec, &npt, h_pnl,&m_pnl,&g_pnl,&fwd);
   /*   taper(spt->rec,npt);  */
      if (useDisp==1) cumsum(spt->rec, npt, dt); /*use displacement data*/
      taper(spt->rec,npt);  
      for(x2=0.,f_pt=spt->rec,l=0;l<npt;l++,f_pt++) {
	*f_pt *= weight;
	x2+=(*f_pt)*(*f_pt);
      }
      spt->rec2 = x2;
      for(m=0,k=0;k<kc;k++) {
	spt->syn[k] = f_pt = (float *) malloc(wndw_size);
	memcpy(f_pt, green[kk[j]+k]+t[j]-shft0[i][j], wndw_size);
	if (j<3) {
#ifdef DIRECTIVITY
		conv(src_sw, ns_sw, f_pt, npt);
#endif
		tandem_(f_pt, f_pt, &npt, h_sw, &m_sw, &g_sw, &fwd);
	} else {
#ifdef DIRECTIVITY
		conv(src_pnl, ns_pnl, f_pt, npt);
#endif
		tandem_(f_pt, f_pt, &npt, h_pnl,&m_pnl,&g_pnl,&fwd);
	}
	/* taper(f_pt,npt); */
	if (useDisp) cumsum(f_pt, npt, dt);
	taper(f_pt,npt);
	for(l=0; l<npt; l++) f_pt[l] *= weight;
	spt->crl[k] = crscrl(npt,spt->rec,f_pt,max_shft[j]);
	for(x=1.,k1=k;k1>=0;k1--,x=2.) {
	  f_pt0=spt->syn[k];
	  f_pt1=spt->syn[k1];
	  for(x2=0.,l=0;l<npt;l++)
	    x2+=(*f_pt0++)*(*f_pt1++);
   //fprintf(stderr,"k %d k1 %d m %d x %f\n",k,k1,m,x);
	  spt->syn2[m++] = x*x2;
	}
      }
    }

    obs++;
    goto normal;

    next_sta:
    nda--; i--;

    normal:
    for(j=0;j<n_data;j++) free(data[j]);
    for(j=0;j<n_green;j++) free(green[j]);

  }	/*********end of loop over stations ********/

  if (nda < 1) {
    fprintf(stderr,"No station available for inversion\n");
    return -1;
  }

  /************grid-search for source mechanism***********/
  INVERSION:
  sol = error(nda,obs0,nfm,fm0,max_shft,m0,grid,fm_thr,tie);
  if (dm>0.001) {	/* grid-search for m0 */
    m0 = m0+dm;
    sol2 = error(nda,obs0,nfm,fm0,max_shft,m0,grid,fm_thr,tie);
    if (sol2.err > sol.err) {	/* this is the wrong direction, turn around */
      dm = -dm;
      sol1 = sol2; sol2 = sol; sol  = sol1; /*swap sol, sol2 */
      m0 = m0+dm;
    }
    while(sol2.err < sol.err) {	/* keep going until passing by the mininum */
      sol1 = sol;
      sol = sol2;
      m0 = m0+dm;
      sol2 = error(nda,obs0,nfm,fm0,max_shft,m0,grid,fm_thr,tie);
    }
    m0 = m0-dm*(0.5+(sol2.err-sol.err)/(sol2.err+sol1.err-2*sol.err));
    sol = error(nda,obs0,nfm,fm0,max_shft,m0,grid,fm_thr,tie);
  }
  dof = nof_per_samp*total_n;
  x2 = sol.err/dof;

  /* repeat grid search if needed */
  if ( repeat && discard_bad_data(nda,obs0,sol,x2,rms_cut) ) {
    repeat--;
    goto INVERSION;
  }

  /**************output the results***********************/
  if (sol.flag) fprintf(stderr,"Warning: flag=%d => the minimum %5.1f/%4.1f/%5.1f is at boundary\n",sol.flag,sol.meca.stk,sol.meca.dip,sol.meca.rak);
  else principal_values(&(sol.dev[0]));
/*  if (sol.meca.dip>90.) {
     fprintf(stderr,"Warning: dip corrected by %f\n",sol.meca.dip-90);
     sol.meca.dip = 90.;
  } */
  strcat(strcat(strcat(strcpy(tmp,argv[1]),"/"),argv[2]),".out");
  f_out=fopen(tmp,"w");
  fprintf(f_out,"Event %s Model %s FM %3d %2d %3d Mw %4.2f rms %9.3e %5d ERR %3d %3d %3d\n",argv[1],argv[2],
	(int) rintf(sol.meca.stk), (int) rintf(sol.meca.dip), (int) rintf(sol.meca.rak),
	m0, sol.err, dof,
	(int) rintf(grid.step[0]*sqrt(2*x2/sol.dev[0])),
	(int) rintf(grid.step[1]*sqrt(2*x2/sol.dev[1])),
	(int) rintf(grid.step[2]*sqrt(2*x2/sol.dev[2])));
  if(output) {
  strcat(strcat(strcat(strcpy(tmp1,argv[1]),"/"),argv[2]),".grid"); ff_out=fopen(tmp1,"w");
//  fprintf(stderr,"%s\n",tmp1);
  for(hi=0;hi<grid.n[2];hi++) {
     for(hj=0;hj<grid.n[1];hj++) {
        for(hk=0;hk<grid.n[0];hk++) {
           strike = grid.x0[0]+hk*grid.step[0];
           dip = grid.x0[1]+hj*grid.step[1];
           rake = grid.x0[2]+hi*grid.step[2];
           index = hi*grid.n[0]*grid.n[1]+hj*grid.n[0]+hk;
           solution = grid.err[index];
    /*       solution = (solution-grid.err[sol.others[0]])/x2/dof*100; */
           solution = (solution-sol.err)/sol.err*100;
        /*   solution = solution/x2/dof;  */
//           fprintf(ff_out,"%3d %3d %3d %5.1f\n",(int) rintf(strike),(int) rintf(dip),(int) rintf(rake),solution);
        }
     }
  }
  fclose(ff_out);
  strcat(strcat(strcat(strcpy(tmp1,argv[1]),"/"),argv[2]),".grid1"); ff_out=fopen(tmp1,"w");
  fprintf(stderr,"gridding output at: %s\n",tmp1);
  for(hi=0;hi<grid.n[2];hi+=2) {
     for(hj=0;hj<grid.n[1];hj+=2) {
        for(hk=0;hk<grid.n[0];hk+=2) {
           strike = grid.x0[0]+hk*grid.step[0];
           dip = grid.x0[1]+hj*grid.step[1];
           rake = grid.x0[2]+hi*grid.step[2];
           index = hi*grid.n[0]*grid.n[1]+hj*grid.n[0]+hk;
           solution = grid.err[index];
    /*       solution = (solution-grid.err[sol.others[0]])/x2/dof*100; */
           solution = (solution-sol.err)/sol.err*100;
        /*   solution = solution/x2/dof;  */
           fprintf(ff_out,"%3d %3d %3d %5.1f\n",(int) rintf(strike),(int) rintf(dip),(int) rintf(rake),solution);
        }
     }
  }
  fclose(ff_out);
  }
  for(i=1;i<sol.ms;i++) {
     j = sol.others[i];
/*     if (grid.err[j]-grid.err[sol.others[0]]<mltp*x2) {   */
     if (grid.err[j]-sol.err<mltp*x2*dof/100) {
	k = j/(grid.n[0]*grid.n[1]);
	k1 = (j-k*grid.n[0]*grid.n[1])/grid.n[0];
	fprintf(f_out,"# %3d %2d %3d %4.2f %9.3e %3.1f\n",
		(int) rintf(grid.x0[0]+(j-k1*grid.n[0]-k*grid.n[0]*grid.n[1])*grid.step[0]),
		(int) rintf(grid.x0[1]+k1*grid.step[1]),
		(int) rintf(grid.x0[2]+k*grid.step[2]),
/*		m0,grid.err[j],(grid.err[j]-grid.err[sol.others[0]])/x2); */
		m0,grid.err[j],(grid.err[j]-sol.err)/x2/dof*100);
     }
  } 

  grid.n[0]=grid.n[1]=grid.n[2]=1;
  grid.x0[0]=sol.meca.stk; grid.x0[1]=sol.meca.dip; grid.x0[2]=sol.meca.rak;
  sol = error(nda,obs0,nfm,fm0,max_shft,m0,grid,fm_thr,tie);
  for(obs=obs0,i=0;i<nda;i++,obs++) {
    fprintf(f_out,"%-9s %5.2f",obs->stn, con_shft[i]);
    for(j=0;j<NCP;j++) {
      k = NCP - 1 - j;
      sol.shft[i][k]=sol.shft[i][k] - max_shft[k]/2;
      kc = sol.cfg[i][k]; if (kc<0) kc = 0;
      fprintf(f_out," %1d %8.2e %2d %5.2f %5.2f",obs->com[k].on_off,sol.error[i][k],kc,sol.ampt[i][k],dt*(shft0[i][k]+sol.shft[i][k]));
    }
    fprintf(f_out,"\n");
  }
  fclose(f_out);

  if ( ! plot ) return 0;

  /***** output waveforms for both data and synthetics ****/
  i = mm[1]; if(mm[0]>i) i=mm[0];
  syn = (float *) malloc(i*sizeof(float));
  if (syn == NULL) {
     fprintf(stderr,"fail to allocate memory for output\n");
     return -1;
  }
  amp = pow(10.,1.5*m0+16.1-20);
  for(obs=obs0,i=0;i<nda;i++,obs++){
    dc_radiat(obs->az-sol.meca.stk,sol.meca.dip,sol.meca.rak,arad);
/*
    fprintf(stderr,"useing az=%f strk=%f dip=%f rake=%f for outputing synthetics\n",obs->az,sol.meca.stk,sol.meca.dip,sol.meca.rak);
*/
    for(k=0;k<3;k++) rad[k]=arad[2-k][0]; for(;k<5;k++) rad[k]=arad[5-k][2];
    strcat(strcat(strcat(strcat(strcat(strcpy(tmp,argv[1]),"/"),argv[2]), "_"),obs->stn),".0");
    c_pt = strrchr(tmp,(int) '0');
    for(kc=2,f_pt=rad+3,spt=obs->com,j=0;j<NCP;j++,spt++,kc=3,f_pt=rad) {
      npt=spt->npt;
      hd = sachdr(dt, npt, spt->b);
      hd.dist = obs->dist; hd.az = obs->az; hd.user1 = obs->alpha;
      hd.a = hd.b;
      for(l=0;l<npt;l++) syn[l] = spt->rec[l]/amp;
      write_sac(tmp,hd,syn);
      (*c_pt)++;
      for(l=0;l<npt;l++) {
	for(x2=0.,k=0;k<kc;k++)x2 += f_pt[k]*spt->syn[k][l];
	syn[l] = x2;
      }
      hd.b -= (shft0[i][j]*dt+con_shft[i]);
      hd.a = hd.b-sol.shft[i][j]*dt;
	hd.az=obs->az;
      write_sac(tmp,hd,syn);
      (*c_pt)++;
    }
  }
  return 0;
}

SOLN	error(	int		nda,
		DATA		*obs0,
		int		nfm,
		FM		*fm,
		const int	*max_shft,
		float		m0,
		GRID		grid,
		float		fm_thr,
		float		tie
	)
{
  int	i, j, k, l, m, k1, kc, z0, z1, z2;
  int	i_stk, i_dip, i_rak;
  float	amp, rad[5], arad[3][3], x, x1, x2, y, y1, y2, cfg[5], s3d[9];
  float	*f_pt0, *f_pt1, *r_pt, *r_pt0, *r_pt1, *z_pt, *z_pt0, *z_pt1, *grd_err;
  DATA	*obs;
  COMP	*spt;
  SOLN	sol;
  int	check_first_motion(MECA, FM *, int, float);

  grd_err = grid.err;
  amp = pow(10.,1.5*m0+16.1-20);
  for(i_rak=0; i_rak<grid.n[2]; i_rak++) {
     sol.meca.rak=grid.x0[2]+i_rak*grid.step[2];
     for(i_dip=0; i_dip<grid.n[1]; i_dip++) {
       sol.meca.dip=grid.x0[1]+i_dip*grid.step[1];
       for(i_stk=0; i_stk<grid.n[0]; i_stk++) {
          sol.meca.stk=grid.x0[0]+i_stk*grid.step[1];
	  if (check_first_motion(sol.meca,fm,nfm,fm_thr)<0) {
		*grd_err++ = sol.err = FLT_MAX;
		continue;
	  }
	  for(obs=obs0,sol.err=0.,i=0;i<nda;i++,obs++){
	    dc_radiat(obs->az - sol.meca.stk,sol.meca.dip,sol.meca.rak,arad);
	    for(k=0;k<3;k++) rad[k]=amp*arad[2-k][0]; for(;k<5;k++) rad[k]=amp*arad[5-k][2];
	    
	    /*******find the time shift*************/
	    /**SH**/
	    spt = obs->com;
	    f_pt0 = spt->crl[0];
	    f_pt1 = spt->crl[1];
	    z0 = spt->on_off>0?1:0;
	    /**PSV*/
	    spt++;
	    r_pt0 = spt->crl[0];
	    r_pt1 = spt->crl[1];
	    r_pt  = spt->crl[2];
	    z1 = spt->on_off>0?1:0;
	    spt++;
	    z_pt0 = spt->crl[0];
	    z_pt1 = spt->crl[1];
	    z_pt  = spt->crl[2];
	    z2 = spt->on_off>0?1:0;
	    for(y1=y2=-FLT_MAX,l=0;l<=max_shft[1];l++) {
	      x =rad[3]*(*f_pt0++)+rad[4]*(*f_pt1++);
	      x1=rad[0]*(*r_pt0++)+rad[1]*(*r_pt1++)+rad[2]*(*r_pt++);
	      x2=rad[0]*(*z_pt0++)+rad[1]*(*z_pt1++)+rad[2]*(*z_pt++);
	      y = (1-tie)*z0*x + tie*(z1*x1 + z2*x2);
	      if (y>y2) {y2=y;cfg[0]=x;sol.shft[i][0]=l;}
	      y = tie*z0*x + (1-tie)*(z1*x1 + z2*x2);
	      if (y>y1) {y1=y;cfg[1]=x1;cfg[2]=x2;m=l;}
	    }
	    sol.shft[i][1]=sol.shft[i][2]=m;
	    /**Pnl*/
	    spt++;
	    r_pt0 = spt->crl[0];
	    r_pt1 = spt->crl[1];
	    r_pt  = spt->crl[2];
	    z1 = spt->on_off>0?1:0;
	    spt++;
	    z_pt0 = spt->crl[0];
	    z_pt1 = spt->crl[1];
	    z_pt  = spt->crl[2];
	    z2 = spt->on_off>0?1:0;
	    for(y1=-FLT_MAX,l=0;l<=max_shft[3];l++) {
	      x1=rad[0]*(*r_pt0++)+rad[1]*(*r_pt1++)+rad[2]*(*r_pt++);
	      x2=rad[0]*(*z_pt0++)+rad[1]*(*z_pt1++)+rad[2]*(*z_pt++);
	      y = z1*x1 + z2*x2;
	      if (y>y1) {y1=y;cfg[3]=x1;cfg[4]=x2;m=l;}
	    }
	    sol.shft[i][3]=sol.shft[i][4]=m;
	    
	    /***error calculation*****/
	    spt -= NCP - 1;
	    for(kc=2,f_pt1=rad+3,j=0;j<NCP;j++,spt++,kc=3,f_pt1=rad) {
	      for(x2=0.,f_pt0=spt->syn2,k=0;k<kc;k++)
		for(k1=k;k1>=0;k1--)
		  x2+=f_pt1[k]*f_pt1[k1]*(*f_pt0++);
	      x1 = spt->rec2+x2-2.*cfg[j];
	      sol.error[i][j] = x1;	/*L2 error for this com.*/
              sol.ampt[i][j] = sqrt(x2/spt->rec2);
	      sol.cfg[i][j] = 100*cfg[j]/sqrt(spt->rec2*x2);
	      sol.err += spt->on_off*x1;
	    }
	  } /*-------------------------end of all stations*/
	  *grd_err++ = sol.err;		/*error for this solution*/

       }
    }
  }
  sol.err = grid3d(grid.err,&(grid.n[0]),s3d,&sol.flag,&sol.ms,sol.others);
  sol.meca.stk = grid.x0[0]+s3d[0]*grid.step[0];
  sol.meca.dip = grid.x0[1]+s3d[1]*grid.step[1];
  sol.meca.rak = grid.x0[2]+s3d[2]*grid.step[2];
  for(i=0;i<6;i++) sol.dev[i]  = s3d[3+i];

  return(sol);

}

int check_first_motion(MECA meca, FM *fm, int n, float fm_thr)
{
  int	i;
  float mt[3][3];
  FM	*pt;

  fdtensor(meca,mt);
  for(pt=fm,i=0;i<n;i++,pt++) {
    if (pt->type*radpmt(mt, *pt)/abs(pt->type)<fm_thr)
      return -1;
  }
  return 0;

}

void    taper(float *aa, int n)
{
  int	i, m;
  float	tt, pi1;
  m = rint(0.3*n);
  pi1 = 3.1415926/m;
  for (i=0; i<m; i++) {
    tt = 0.5*(1.-cos(i*pi1));
    aa[i] *= tt;
    aa[n-i-1] *= tt;
  }
}

int discard_bad_data(int nda, DATA *obs, SOLN sol, float sig, float rms_cut[]) {
   int i, j, n;
   COMP	*spt;
   n = 0;
   for(i=0;i<nda;i++,obs++) {
     spt = obs->com;
     for(j=0; j<5; j++,spt++) {
        if (sol.error[i][j]/sig > rms_cut[j]) {
	   spt->on_off = 0;
	   n++;
	}
     }
   }
   return(n);
}

/* unit sum trapzoid from convolution of two boxes (durations t1 and t2) */
float *trap(float t1, float t2, float dt, int *n) {
    int i, n1, n2;
    float slope, *s;
    n1 = rint(t1/dt); if (n1<1) n1 = 1;
    n2 = rint(t2/dt); if (n2<1) n2 = 1;
    if (n1 > n2) {
	i = n1;
	n1 = n2;
	n2 = i;
    }
    *n = n1+n2;
    s = (float *) malloc((*n)*sizeof(float));
    slope = 1./(n1*n2);
    s[0] = 0;
    for(i=1;i<=n1;i++) s[*n-i] = s[i]=s[i-1] + slope;
    for(;i<n2;i++) s[i]=s[i-1];
/*
    fprintf(stderr,"%d %d \n",n1,n2);
    slope = 0;for(i=0;i<*n;i++) {slope+=s[i];fprintf(stderr,"%d %f %f\n",i,s[i],slope);}
*/
    return s;
}

void buttbp(float *h, int *m, float *g, float fl, float fh) {
    int n;
    float eps, ah, al, ap, as, fs;
/*    SACHEAD hd;
    float gout[512],pout[512];
    void recres_(); */
    void butpas_(float *,int *,float *,int *,float *,float *,float *,float *,float *);
    eps = 0.3;
    ah = 0.1;
    al = 0.1;
    ap=sqrt(ah/(1.-ah));
    as=sqrt((1.-al)/al);
    fs=fh*(1.+eps); if (fs>0.5) fs=0.5;
    butpas_(h,m,g,&n,&fl,&fh,&fs,&ap,&as);
/*    fprintf(stderr,"butterworth filter order = %d\n",*m);
    fl=0;
    n=512;
    fh=0.5/n;
    recres_(h,m,g,&fl,&fh,gout,pout,&n);
    fh=fh/0.2;
    hd = sachdr(fh,n,fl);
    write_sac("am.sac",hd,gout);
    write_sac("ph.sac",hd,pout); */
}

void principal_values(float a[]) {
   int i;
   float **b, **v;
   b = matrix(1,3,1,3);
   v = matrix(1,3,1,3);
   for(i=0;i<3;i++) b[i+1][i+1]=a[i];
   b[1][2]=b[2][1]=a[3];b[1][3]=b[3][1]=a[4];b[2][3]=b[3][2]=a[5];
   jacobi(b,3,a,v,&i);
   eigsrt(a,v,3);
   for(i=0;i<3;i++) {a[i] = a[i+1]; if (a[i]<0.0001*a[1]) a[i]=0.0001*a[1];}
   free_convert_matrix(b,1,3,1,3);
   free_matrix(v,1,3,1,3);
}
