#!/usr/bin/gawk

# Script for computing conjugate fault plane by a known plane solution

# Modification and annotation made by Chen Weiwen
# Current Version: 2014/10/10
# Contact jiazhe@mail.ustc.edu.cn vincentc@mail.ustc.edu.cn for more info

# Input: focal mechanism with known strike/dip/rake
# Output: Corresponding strike/dip/rake of the conjugate fault plane

BEGIN{ARGC=1; d2r=3.1415926/180;
       PI=atan2(1,1)*4.0;

        phi=ARGV[1]*d2r; delta=ARGV[2]*d2r; lambda=ARGV[3]*d2r;


        l1=-sin(lambda)*sin(delta);
        l2=cos(lambda)*cos(phi)+cos(delta)*sin(lambda)*sin(phi);
        l3=cos(lambda)*sin(phi)-cos(delta)*sin(lambda)*cos(phi);
        l3=-1*l3;

        n2=-1*sin(delta)*sin(phi);
        n3=-1*sin(delta)*cos(phi);
        n1=-cos(delta);

#there are two different angles which will suitable for the solution
        if(l1>0)
        { l1=-1*l1;
          l2=-1*l2;
          l3=-1*l3;
          n1 = -1*n1;
          n2=-1*n2;
	  n3=-1*n3;
	  delta2_1=atan2(sqrt(1-l1*l1),-1*l1);
          delta2_2=-1*delta2_1;
#case 1, delta2 in [0.PI/2]

#	  fai2_1=atan2(-1*l2,-1*l3)+PI;
	  fai2_1=atan2(-1*l2,-1*l3);
        if(fai2_1 <0)fai2_1=fai2_1+2*PI;
        if(fai2_1 >2*PI)fai2_1=fai2_1-2*PI;

        tmp_coslamda=n2*cos(fai2_1)-n3*sin(fai2_1)

       if(delta2_1 != 0.0) tmp_sinlamda=-1*n1/sin(delta2_1);
       lamda2_1=atan2(tmp_sinlamda,tmp_coslamda);
       if(lamda2_1 <-1*PI)lamda2_1+2*PI;


#case2 delta2 in [-PI/2,0]
        fai2_2=atan2(l2,l3);
        if(fai2_2 <0)fai2_2=fai2_2+2*PI;
         tmp_coslamda=n2*cos(fai2_2)-n3*sin(fai2_2)

       if(delta2_2 != 0.0) tmp_sinlamda=-1*n1/sin(delta2_2);
       lamda2_2=atan2(tmp_sinlamda,tmp_coslamda);
       if(lamda2_2 <-1*PI)lamda2_2+2*PI;
      }
      else{

          delta2_1=atan2(sqrt(1-l1*l1),-1*l1);
          delta2_2=-1*delta2_1;
#case 1, delta2 in [0.PI/2]

	  fai2_1=atan2(-1*l2,-1*l3);
        if(fai2_1 <0)fai2_1=fai2_1+2*PI;
        if(fai2_1 >2*PI)fai2_1=fai2_1-2*PI;

        tmp_coslamda=n2*cos(fai2_1)-n3*sin(fai2_1)

       if(delta2_1 != 0.0) tmp_sinlamda=-1*n1/sin(delta2_1);
       lamda2_1=atan2(tmp_sinlamda,tmp_coslamda);

#case2 delta2 in [-PI/2,0]
        fai2_2=atan2(l2,l3);
        if(fai2_2 <0)fai2_2=fai2_2+2*PI;
         tmp_coslamda=n2*cos(fai2_2)-n3*sin(fai2_2)

       if(delta2_2 != 0.0) tmp_sinlamda=-1*n1/sin(delta2_2);
       lamda2_2=atan2(tmp_sinlamda,tmp_coslamda);
      }

#        fai2=fai2/d2r;
#        delta2=delta2/d2r;
#        lamda2=lamda2/d2r;

        fai2_1=fai2_1/d2r;
        delta2_1=delta2_1/d2r;
        lamda2_1=lamda2_1/d2r;

        fai2_2=fai2_2/d2r;
        delta2_2=delta2_2/d2r;
        lamda2_2=lamda2_2/d2r;
       #  print  fai2," ", delta2," ",lamda2;
         print  fai2_1," ", delta2_1," ",lamda2_1;
         print  fai2_2," ", delta2_2," ",lamda2_2;
}
