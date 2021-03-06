**===================================================**
**    Calculate  Green's functions W(i;n,k)          **
**     i:time step   n=6:#. base elements k:depth#   **
**        for  j-th station                          **
**   Files:                                          **
**     1 = station parameters (OBS)                  **
**     2 = structure          (STR)                  **
**     3 = Green's function   (G1)                   **
**========================= Ver.900715 ==============**
**  Modifications:                                   **
** 1)The source layer # is defined for each grid(k)  **
**     , not necessarily common to all grids -900715 **
**===================================================**
**  Modifications:                                   **
**  Rewrote the original to be a standalone code to  **
**     calculate teleseismic waveforms in SAC format **
**  R. Chu, 10-25-2009                               **
**     Using Lupei's sacio file for the output       **
**===================================================**
      PARAMETER (ND0=2048,NM0=6,LK0=10,NL0=10,PI=3.141593,RAD=.0174533)
      IMPLICIT COMPLEX*8 (Z)
      CHARACTER NAME*40,NAM*4, prefix*80, adel*3, outfile*80
      character*1 wv(9),cm(9), model*80, astr*4,adip*2,arak*4
      character*1 greenV(9)
      character   sta*10
      real*8 gg(500),pp(500),depth,vp00,gs(500),ps(500),vs00
      DIMENSION W(ND0),ZI(ND0),ZQP(ND0),ZQS(ND0)
     -,     ZZ(50),ZP0(50),S1(NM0),A1(NM0),D1(NM0),h(lk0)
     -,     ib(5),ic(5),p(5),g(5)
      COMMON /STR0/NL ,VP (NL0),VS (NL0),DEN (NL0),DEP (NL0)
      COMMON /STR1/NL1,VP1(NL0),VS1(NL0),DEN1(NL0),DEP1(NL0)
      COMMON /STR2/NL2,VP2(NL0),VS2(NL0),DEN2(NL0),DEP2(NL0)
	    common /sourceRegion/vsrc

* < Base elements >
*   N=1,2,3,4,5,6 for Mxy,Mzz-Myy,Myz,Mzx,Mzz-Mxx,Mxx+Myy+Mzz
            DATA S1/ 0,135,180,90,90,0/
            DATA D1/90, 90, 90,90,45,500/
            DATA A1/ 0,  0, 90,90,90,0/

* < Components and phases >
	    data wv/'P', 'P', 'H','V', 'V', 'V','H','H','P'/
	    data cm/'Z', 'R','T', 'Z', 'R', 'T','Z', 'R', 'T'/
	    data greenV/'0','1','2','3','4','5','6','7','8'/
	    data ib/1, 1, 3, 2, 2/
	    data ic/1, 2, 2, 3, 1/

* < Default structure >
	    tqp=1.0
	    tqs=4.0

	    nl= 4
	    data vp/  5.2, 6.24, 6.58, 7.8, 6*0.0/
	    data vs/  3.0, 3.60, 3.80, 4.4, 6*0.0/
	    data den/ 2.4, 2.67, 2.80, 3.3, 6*0.0/
	    data dep/ 5.5, 9.50, 19.0, 0.0, 6*0.0/

	    nl1=3
	    data vp1/  5.2, 6.40, 8.0, 7*0.0/
	    data vs1/  3.0, 3.75, 4.5, 7*0.0/
	    data den1/ 2.4, 2.70, 3.3, 7*0.0/
	    data dep1/ 5.0, 30.0, 0.0, 7*0.0/

	    nl2=2
	    data vp2/   6.40, 8.0, 8*0.0/
	    data vs2/   3.75, 4.4, 8*0.0/
	    data den2/  2.70, 3.3, 8*0.0/
	    data dep2/ 30.00, 0.0, 8*0.0/

* < Default Green's function parameters >
c	    nt= ND0
c	    dt=0.05
	    nk=7
	    xcorrection = 10**(-5.1)
	    tstart=-5.0
c	    nstart=-tstart/dt
	    data h/ .2, 2.5, 5.0, 10.0, 15.0, 20.0, 25.0, 3*0.0/


c ------------ End initialization ----------------------------
      read(*,'(a80)') model
      if(model(1:4) .ne. 'none') then
c	 print*, model
	 open(2,file=model)
         READ(2,'(a40)') name
         READ(2,*) TQP,TQS,NL ,(VP (L),VS (L),DEN (L),DEP (L),L=1,NL )
         READ(2,*)         NL1,(VP1(L),VS1(L),DEN1(L),DEP1(L),L=1,NL1)
         READ(2,*)         NL2,(VP2(L),VS2(L),DEN2(L),DEP2(L),L=1,NL2)
	 close(2)
      endif
	read(*,*) nt, dt
	nstart=-tstart/dt
	
      DF=1/(DT*NT)
      DW=DF*2*PI
	iFlt=0
c	for three type of faults
* < Q-response >
      CALL QF(ZQP,NT,TQP,DF)
      CALL QF(ZQS,NT,TQS,DF)

* < Set ZI >
      CALL INSTG(ZI,NT,DW,0,ZP0,ZZ,0,0,1.,0)

* -----------------------------------------------
* < Read the prefix for the sacfiles >
c	read(*,'(a80)') prefix
c	idx=index(prefix,' ') -1
c	idxs=index(prefix,' ') -1
      
* < Read the source parameters, str/dip/slip = 0/0/0 calculates >
* < fundamental Green's functions, slip > 360. = explosion >
      read(*,*) depth, strike, dip, slip
      if(depth.ge. 0.0) then
	 nk=1
	 h(1)=depth
      endif
      if (strike .eq. 0.0 .and. dip .eq. 0.0 .and. slip .eq. 0.0) then
	 nm=5
      else
	 nm=1
	 s1(1)=strike
	 d1(1)=dip
	 a1(1)=slip
      endif
      if (dip .gt. 360.) then
	 nc=2
      else
	 nc=5
      endif

* < Geometrical spreading >
      call getgeom(gg,pp,depth,vp00,0,1)
      call getgeom(gs,ps,depth,vs00,0,2)

* < Synthetics >

      do 100, j=1,1000
         READ(*,*,END=999) az, dist, sta
	read(*,'(a80)') prefix
        idx=index(prefix,' ') -1
        idxs=index(prefix,' ') -1
c      idxs=index(sta,' ') -1
	 idelt=dist
         g(1)=gg(idelt)+(dist-idelt)*(gg(idelt+1)-gg(idelt))
         p(1)=pp(idelt)+(dist-idelt)*(pp(idelt+1)-pp(idelt))
	 p(2)=p(1)
	 g(2)=g(1)
         g(3)=gs(idelt)+(dist-idelt)*(gs(idelt+1)-gs(idelt))
         p(3)=ps(idelt)+(dist-idelt)*(ps(idelt+1)-ps(idelt))
	 p(4)=p(3)
	 g(4)=g(3)
	 p(5)=p(3)
	 g(5)=g(3)
         print *,"P",p(1),"S",p(2)
	 write(adel,'(i3.3)') idelt
	  nc=2
c	just for P
	 do 99, l=1,nk
c            DO 98 K=1,nc
            DO 98 K=1,3
c	just for three component of P
               DO 97 N=1,NM
		do 1197 iFlt=1,3

		if(K .lt. 4) then
	          write(astr,'(i4.4)') int(s1(n))
	          write(adip,'(i2.2)') int(d1(n))
	          write(arak,'(i4.4)') int(a1(n))
c		  print*, NT,DT,IB(k),IC(k),S1(N),D1(N),A1(N)
c		  print*,h(l),AZ,P(k),g(k)
                  CALL BODYW13(iFlt,W,NT,DT,IB(k),IC(k),S1(N),D1(N),
     -             A1(N), h(l),AZ,P(k),g(k),ZQP,ZQS,ZI,dely,nt)
c		write(*,*) 'delay=', dely
		endif
c	only name changes, no logic change in bodywave
c                  outfile=prefix(1:idx)//'_'//sta(1:idxs)//
c     -           '.'//'_'//wv(k)//'.'//greenV((iFlt-1)*3+K)//char(0)
		outfile=prefix(1:idx)//greenV((iFlt-1)*3+K)//char(0)
c		write(*,*) 'outfile=',outfile

		  do 96, m=nt,nstart+1,-1
		     w(m)=w(m-nstart)-w(1)
		     w(m)= w(m)*xcorrection
c	syn requires for moment of 10^20
96                continue
		  do 95, m=1,nstart
		     w(m)=0.0
95                continue
                  do 94, m=nt,2,-1
                     w(m) = (w(m)-w(m-1))/dt
94                continue
c		  print*,tstart,dely
c		  call newhdr
c	          call setfhv('AZ',az,nerr)
c	          call setfhv('GCARC',dist,nerr)
cc	          call setfhv('B',tstart+dely,nerr)
c	          call setfhv('B',tstart,nerr)
		  xstart = asin(vsrc*p(1))*90/asin(1.0)
c		  call setfhv('USER1',xstart,nerr)
c		  call setfhv('DELTA',dt,nerr)
c		  call setnhv('NPTS',nt,nerr)
c	          call wsac0(outfile,w,w,nerr)
		  call wrtsac1(outfile,dt,nt,tstart,dist,az,xstart,w)
1197		continue
97             continue
98          continue
99       continue
100   continue

999   CONTINUE
      END
