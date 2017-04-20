#!/usr/bin/sh

# Script for running the CAP program

# Author: Jia Zhe;
# Modification and annotation made by Chen Weiwen
# Current Version: 2014/10/10
# Please feel free to contact "Zhe Jia" via jiazhe@mail.ustc.edu.cn or "Weiwen Chen" via vincentc@mail.ustc.edu.cn / ustc.chenweiwen@gmail.com for more info

# Input I: three-components SAC files(local and teleseismic teleseismic dist)
# Input II: Green's Functions computed by forward programs FK/TEL3
# Input III: inversion input parameters, see below
# Output I : Inversion results: *.out
# Output II: Error grids: *.grid
# Output III: Figure plots, including inversion results and depth-error fit

# --- Usage: ---
# sh LeadCAP.cmd

# before doing inversion, specify the depth grid
# input parameters

# Specify the folder of CRUST2.0 below
  CRUST=$HOME"/Model/crust2/"

# dep1: upper limit; dep2: lower limit; depg: depth grid space
  dep1=1;  	     dep2=30; 		depg=1;

# Now specify weighting parameters:
# teleseimic Vs. Local; teleseismic P Vs. SH; switch of P/SH
# P/SH switch: P only:-1 ; SH only: 1 ; P & SH: 0 (default)
  w_TEL_LOC=50;          w_P_SH=1;             s_P_SH=0;
#w_TEL_LOC=50 远震对近震 50 倍
#w_P_SH=1; 远震的 P 对 SH 1 倍
#s_P_SH=0; P 和 SH 都用
# Specify these parameters below:
# mag: magnitude
# dur: source duration
# fh/fhs: Upper filter limit for P/SH(Surface) waves
# comment these lines below if you want compute them with scaling law
  mag=6.8;     dur=10;      fh=0.1;      fhs=0.08;

# remove the inappropriate or repeated files to the folder "others"

cd data/
cp ../.gmtdefaults .
mkdir others/
#ls -1 *.00.z | gawk ' BEGIN {FS="."} {print "mv "$1"."$2".10.* others"}' | sh
#mv *.[2-9]0.* others
saclst gcarc f *.[rtz] | gawk '{if ($2>90) print "mv "$1" ./others;"}' | sh ;

# compute the favorable parameters from magnitude for inversion

if [ -z "$mag" ]; then
mag=$(ls -1 *.z | gawk ' BEGIN{j=1;} {if(j<=1) print $1;j++;}' | gawk '{print "saclst mag f "$1;}' | sh | gawk '{print $2;}')
fi
if [ -z "$dur" ]; then
dur=$(ls -1 *.z | gawk ' BEGIN{j=1;} {if(j<=1) print $1;j++;}' | gawk '{print "saclst mag f "$1;}' | sh | gawk '{printf ("%.1f",3^($2-4.8));}')
fi
if [ -z "$fh" ]; then
fh=$(ls -1 *.z | gawk ' BEGIN{j=1;} {if(j<=1) print $1;j++;}' | gawk '{print "saclst mag f "$1;}' | sh | gawk '{printf("%.2f",0.25*0.66^($2-5));}')
fhs=$(echo $fh | gawk '{printf("%.2f", $1/1.3);}')
fi

if (echo $fh | awk '!($fh>0.2){exit 1}'); then  fh=0.2; fi
if (echo $fhs | awk '!($fhs>0.1){exit 1}'); then  fhs=0.1; fi
freq="0.01/"$fh"/0.01/"$fhs

# Derive the earth model for teleseismic stations

# copy the crust2.0 folder

ls -1 *.z | gawk ' BEGIN{j=1;} {if(j<=1) print $1;j++;}' | gawk '{print "saclst evla evlo f "$1;}' | sh | gawk '{print $2,$3;}' > lalo.dat

if [ -d "$CRUST" ]; then
cp -r $CRUST .
# caution: cp the CRUST2.0 folder from your own !
cp lalo.dat ./crust2/
cd ./crust2/
cat lalo.dat | gawk '{print $0;} END{print "*";}' | ./getCN2point
cp outcr ../
cd ../
fi

# make the model file proper format

echo -e "#model" > vmodel
cat outcr | gawk 'NR>=6&&$1!=0{print $2,$3,$4,$1;}' > model.tmp
num_layer=$(wc -l model.tmp | gawk '{print $1+1;}')
echo -n "1 5 "$num_layer" " >> vmodel
cat model.tmp | gawk '{print "echo "$0" >> vmodel";}' | sh
cat ../cmds/default_mid_receiver_model >> vmodel

# Computing the Green's functions for teleseismic stations
distlst=`saclst dist f *.z | gawk '{if($2>3000&&$2<10000) printf "%d ",$2} END {printf "\n"}'`
#distlst 的形成机制可能因为LOCAL的问题出现震中距重复
gawk -v dist="$distlst" 'BEGIN {for (dep='$dep1';dep<='$dep2';dep+='$depg') { print " perl ../cmds/tel3.pl -O../greenFuncDir -Mvmodel/"dep, dist }}' | sh
# 如果调用teles不成功，原版perl脚本不报错
cp vmodel vmodel_tel

# Derive the earth model for local stations

cat outcr | gawk 'NR>=6&&$1!=0&&$3!=0{print $1+0.01,$3,$2,$4;}' > vmodel
cat ../cmds/default_loc_uppermost_mantle >> vmodel   ###############   IASP91
cp vmodel ../


# Computing local Green's Functions Using FK method

distlst=`saclst dist f *.z | gawk '$2<600{printf "%s ",$2} END {printf "\n"}'`
gawk -v dist="$distlst" 'BEGIN {for (dep='$dep1';dep<='$dep2';dep+='$depg') { print "fk.pl -Mvmodel/"dep," -N2048/0.05/2 -S2" dist ;print "mv ./vmodel_"dep"/* ../greenFuncDir/vmodel_"dep"/ " } }' | sh
#gawk -v dist="$distlst" 'BEGIN {for (dep='$dep1';dep<='$dep2';dep+='$depg') { print "perl ../cmds/fk.pl   -Mvmodel/"dep," -N2048/0.1/2 " dist ;print "mv ./vmodel_"dep"/* ../greenFuncDir/vmodel_"dep"/ " } }' | sh
cp vmodel vmodel_loc


# generate weight files for joint inversion


saclst dist az f *.z  | gawk ' $2<600 {gsub(".z","",$1); print $1,$2,$2,"1 1 1 1 1 1 0 0"}'  | sort -n --key=3 >weight.dat

saclst dist az f *.z   | gawk '$2>3000{gsub(".z","",$1); printf "%s %d %d %s\n", $1,$2,$2,"1 0 0 0 1 0 0 0"}' | sort -n --key=3 >> weight.dat


rm model.tmp lalo.dat outcr
cp vmodel ../
cd ..
cp cmds/cap_plt_3.pl .

# start CAP inversion
# Inversion Control parameters here, see detailed info in ./cmds/cap_3.pl

#    -C  filters for Pnl and surface waves, specified by the corner
#        frequencies of the band-pass filter. (0.02/0.2/0.02/0.1)
#    -D  weight for Pnl (w1) and distance scaling powers for Pnl (p1) and surface
#        waves (p2). (2/1/0.5)
#    -F  include first-motion data in the search. thr is the threshold (0.01).
#        The first motion is input in the file weight.dat. The polarities
#        can be specified using +-1 for P, +-2 for SV, and +-3 for SH after
#        the station name, e.g. LHSA/+1/-3 means up P and down SH.
#        The Green functions need to have take-off angles stored in the SAC
#        header (user1).
#    -G  green's function location ($home/data/model/region).
#    -H  dt (0.05).
#    -I  search interval in strike/dip/rake and mag (5/0.1).
#    -L  source duration (1 sec)
#    -M  specify velocity model and source depth.
#    -N  repeat the inversion n times and discard bad traces (0)
#    -P  generate waveform-fit plot with plotting scale.
#        Yscale: inch per cm/s at 100 km. (100000)
#        Xscale: seconds per inch. (40)
#        append k if one wants to keep those waveforms.
#    -Q  number of freedom per sample (1)
#    -R  grid-search range for strike/dip/rake (0/360/0/90/-90/90).
#    -S  max. time shifts in sec for Pnl and surface waves (1/5) and
#        tie between SH shift and SV shift:
#         tie=0          shift SV and SH independently,
#         tie=0.5        force the same shift for SH and SV (default)
#    -T  max. time window lengths for Pnl and surface waves (35/70)
#    -V  apparent velocities for Pnl, Love, and Rayleigh waves (off)
#    -U  directivity, specify rupture direction on the fault plane (off)
#    -W  use displacement for inversion; 1=> data in velocity; 2=> data in disp
#    -X  output other local minimums whose misfit-min<n*sigma (0)
#    -Z  specify a different weight file name (weight.dat)
#    -A  specify scalar factor for telseismogram

####Application of a grid search method, with grid search intervals of strike, dip and rake to be 10 degrees.####
echo -e "perl ./cmds/cap_3.pl -D3/0/0 -T60/60 -V-1/-1/-1 -G./greenFuncDir  -O -P0.8e6/90/k   -H0.05 -L"$dur"  -Mvmodel_\$1/"$mag" -W1 -R0/360/0/90/-180/180  -S5/10/0 -C"$freq"  -I10/0.1  data  -Zweight.dat -A"$w_TEL_LOC"/"$w_P_SH"/"$s_P_SH" " > cap_3.cmd

for ((j=$dep1;j<=$dep2;j=j+$depg))
do
 sh cap_3.cmd $j
 rm ./data/*.[0-9]
done

####The focal mechanism result of previous search####
cd data
stk=$(cat *_*.out|grep ERR | gawk '{ gsub($2,""); print $0 }' |gawk '{gsub("_"," ");print $6,$12;}' | gawk 'BEGIN {num=0;temp=9999999999;} {if ($2<=temp) {temp=$2;num=$1;}} END {print num;}')
dip=$(cat *_*.out|grep ERR | gawk '{ gsub($2,""); print $0 }' |gawk '{gsub("_"," ");print $7,$12;}' | gawk 'BEGIN {num=0;temp=9999999999;} {if ($2<=temp) {temp=$2;num=$1;}} END {print num;}')
rak=$(cat *_*.out|grep ERR | gawk '{ gsub($2,""); print $0 }' |gawk '{gsub("_"," ");print $8,$12;}' | gawk 'BEGIN {num=0;temp=9999999999;} {if ($2<=temp) {temp=$2;num=$1;}} END {print num;}')
dep_optimal_1=$(cat *_*.out|grep ERR | gawk '{ gsub($2,""); print $0 }' |gawk '{gsub("_"," ");print $4,$12;}' | gawk 'BEGIN {num=0;temp=9999999999;} {if ($2<=temp) {temp=$2;num=$1;}} END {print num;}')
mag_revised=$(cat *_*.out|grep ERR | gawk '{ gsub($2,""); print $0 }' |gawk '{gsub("_"," ");print $10,$12;}' | gawk 'BEGIN {num=0;temp=9999999999;} {if ($2<=temp) {temp=$2;num=$1;}} END {print num;}')
stk_min=$(echo $stk | gawk '{if ($1-10>=0) {printf ("%d",$1-10);} else {printf ("%d",0);}}')
stk_max=$(echo $stk | gawk '{if ($1+10<=360){printf ("%d",$1+10);} else {printf ("%d",360);}}')
dip_min=$(echo $dip | gawk '{if ($1-10>=0){printf ("%d",$1-10);} else {printf ("%d",0);}}')
dip_max=$(echo $dip | gawk '{if ($1+10<=90){printf ("%d",$1+10);} else {printf ("%d",90);}}')
rak_min=$(echo $rak | gawk '{if ($1-10>=-180) {printf ("%d",$1-10);} else {printf ("%d",-180);}}')
rak_max=$(echo $rak | gawk '{if ($1+10<=180){printf ("%d",$1+10);} else {printf ("%d",180);}}')
dep_min=$(echo $dep_optimal_1 | gawk '{if ($1>10) {printf ("%d",$1-10)} else {if ($1%2==0) printf ("%d",2);if ($1%2==1) printf ("%d",1);};}')
dep_max=$(echo $dep_optimal_1 | gawk '{printf ("%d",$1+10);}')
distlst_1=`saclst dist f *.z | gawk '{if($2>3000&&$2<10000) printf "%d ",$2} END {printf "\n"}'`
cp vmodel_tel vmodel
gawk -v dist="$distlst_1" 'BEGIN {for (dep='$dep_min';dep<='$dep_max';dep+=2) { print " perl ../cmds/tel3.pl -O../greenFuncDir -Mvmodel/"dep, dist }}' | sh
distlst_2=`saclst dist f *.z | gawk '$2<600{printf "%s ",$2} END {printf "\n"}'`
cp vmodel_loc vmodel
gawk -v dist="$distlst_2" 'BEGIN {for (dep='$dep_min';dep<='$dep_max';dep+=2) { print "perl ../cmds/fk.pl   -Mvmodel/"dep," -N2048/0.1/2 " dist ;print "mv ./vmodel_"dep"/* ../greenFuncDir/vmodel_"dep"/ " } }' | sh
mkdir sparse_results
mv vmodel_*  sparse_results
cd ..

####Firstly sparse and secondly dense search intervals are applied in order to reduce the computing time.####
echo -e "perl ./cmds/cap_3.pl -D3/0/0 -T60/60 -V-1/-1/-1 -G./greenFuncDir  -O -P0.8e6/90/k   -H0.05 -L"$dur"  -Mvmodel_\$1/"$mag_revised" -W1 -R"$stk_min"/"$stk_max"/"$dip_min"/"$dip_max"/"$rak_min"/"$rak_max"  -S5/10/0 -C"$freq"  -I1/0.1  data  -Zweight.dat -A"$w_TEL_LOC"/"$w_P_SH"/"$s_P_SH" " > cap_3_rev.cmd

for ((j=$dep_min;j<=$dep_max;j=j+2))
do
 sh cap_3_rev.cmd $j
 rm ./data/*.[0-9]
done


#cat data/*_*.out|grep ERR | gawk '{ gsub($2,""); print $0 }' |gawk '{gsub("_"," ");print $4,$12;}' | awk 'BEGIN {num=0;temp=9999999999;} {if ($2<=temp) {temp=$2;num=$1;}} END {print num;}' > num.tmp
#nd1=$(cat num.tmp | gawk '{print $1-5;}')
#nd2=$(cat num.tmp | gawk '{print $1+5;}')
#gawk -v dist="$distlst" 'BEGIN {for (dep='$nd1';dep<='$nd2';dep+=1) { print "perl tel3.pl -O./greenFuncDir -Mvmodel/"dep, dist }}' | sh
#cat num.tmp | gawk -v feq="$freq" '{bg=$1-5;nd=$1+5;print "rm data/*_*.out";print "sh cap_3.sh "bg" "nd" 1 "'$mag','$dur',feq}' | sh
cp ./cmds/mecherr.cmd data/
cd data/
sh mecherr.cmd
cp mecherr.ps ../
rm mecherr.cmd
rm junk.*
cd ..
