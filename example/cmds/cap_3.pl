#!/usr/bin/perl
#
# A user-friendly PERL interface to the CAP source inversion code cap
#
# written by Lupei Zhu, 3/6/1998, Caltech
#
# revision history
#	6/18/2001	add usage and documentation
#	1/10/2014	small modification
#
use FindBin;

# Specify the folder where the cap exe is located
 $cap= "$FindBin::Bin/cap";

require "$FindBin::Bin/cap_plt_3.pl";	# include plot script

# command line input, [] means optional, see () for default value
$usage =
" ===== CAP source inversion using seismic waveforms ====
	Ref: Zhu and Helmberger, 1996, BSSA 86, 1645-1641

  Data preparation:
     Put all three-component waveforms, named as station.[r,t,z] in
  a single directory named by the event ID. The data should be velocity
  (in cm/s) or displacement (in cm) in the SAC format, with the reference
  time set at the origin time and epicentral distance and azimuth
  set in the SAC header. There should be another file called weight.dat
  in the same directory, in the following format:
	station_name  d1 d2 w1 w2 w3 w4 w5 t0 tsv tsh
    where d1 is the epicentral distance
          d2 is the distance where the Greens functions d2.grn.[0-8] are used
	  w1 to w5 are the weights for 5 segs. of waveforms: PnlZ, PnlR, Z, R, T
	  t0 is the time shift (in sec) for the whole record (positive means
	  that the data is delayed wrt the model).
	  tsv and tsh are the initial time shifts for Rayleigh and Love waves

  The Greens function library:
     The Greens functions are computed using FK, named as xxx.grn.[0-8] where
  xxx is the distance. All Greens functions from one source depth are placed
  in a single directory named as model_depth. They are in SAC format with
  two time marks set: t1 for the first P arrival and t2 for the first S arrival.
  If first-motion data are to be used in the inversion, the Greens functions
  need to have user1 and user2 set as the P and S take-off angles (in degrees).

  Time window determination:
     The inversion breaks the whole record into two windows, the Pnl window
  and the surface wave window. These windows are determined in following way:
    1) If the SAC head has time mark t1, t2 set, the code will use them for
       the Pnl window. The same is true for the surface wave window (t3,t4)
    Otherwise,
    2) If positive apparent velocities are given to the code, it will use
       them to calculate the time windows (see the -V option below):
	  t1 = dist/vp - 0.3*m1, t2 = ts + 0.2*m1
	  t3 = dist/vLove - 0.3*m2, t4 = dist/vRayleigh + 0.7*m2
    Otherwise,
    3) Using the tp, ts in the Greens function header
 	  t1 = tp - 0.2*m1,  t2 = t1+m1
	  t3 = ts - 0.3*m2,  t4 = t3+m2
    Here m1, m2 are the maximum lengths for the Pnl and surface waves windows
    (see the -T options below).

  Usage: cap.pl -Mmodel_depth/mag [-C<f1_pnl/f2_pnl/f1_sw/f2_sw>] [-D<w1/p1/p2>] [-F<thr>] [-Ggreen] [-Hdt] [-Idd[/dm]] [-L<tau>] [-N<n>] [-O] [-P[<Yscale[/Xscale[/k]]]>] [-Qnof] [-R<strike1/strike2/dip1/dip2/rake1/rake2>] [-S<s1/s2[/tie]>] [-T<m1/m2>] [-V<vp/vl/vr>] [-Udir] [-Wi] [-Xn] [-Zstring] event_dirs
    -C  filters for Pnl and surface waves, specified by the corner
	frequencies of the band-pass filter. (0.02/0.2/0.02/0.1)
    -D	weight for Pnl (w1) and distance scaling powers for Pnl (p1) and surface
   	waves (p2). (2/1/0.5)
    -F	include first-motion data in the search. thr is the threshold (0.01).
    	The first motion is input in the file weight.dat. The polarities
	can be specified using +-1 for P, +-2 for SV, and +-3 for SH after
	the station name, e.g. LHSA/+1/-3 means up P and down SH.
	The Green functions need to have take-off angles stored in the SAC
	header (user1).
    -G  green's function location ($home/data/model/region).
    -H  dt (0.2).
    -I  search interval in strike/dip/rake and mag (10/0.1).
    -L  source duration (1 sec)
    -M	specify velocity model and source depth.
    -N  repeat the inversion n times and discard bad traces (0)
    -P	generate waveform-fit plot with plotting scale.
    	Yscale: inch per cm/s at 100 km. (100000)
	Xscale: seconds per inch. (40)
	append k if one wants to keep those waveforms.
    -Q  number of freedom per sample (1)
    -R	grid-search range for strike/dip/rake (0/360/0/90/-90/90).
    -S	max. time shifts in sec for Pnl and surface waves (1/5) and
	tie between SH shift and SV shift:
	 tie=0 		shift SV and SH independently,
	 tie=0.5 	force the same shift for SH and SV (default)
    -T	max. time window lengths for Pnl and surface waves (35/70)
    -V	apparent velocities for Pnl, Love, and Rayleigh waves (off)
    -U  directivity, specify rupture direction on the fault plane (off)
    -W  use displacement for inversion; 1=> data in velocity; 2=> data in disp
    -X  output other local minimums whose misfit-min<n*sigma (0)
    -Z  specify a different weight file name (weight.dat)
    -A  specify scalar factor for telseismogram
";

@ARGV > 1 || die $usage;

#================defaults======================================
$green="/net/dix/scratch01/ytan/model";	#green's function location
$repeat = 0;
$fm_thr = 2;		# >0, do not use fm data even they exist in weight.dat
$dir='';
$disp=0;
$output=0;
$mltp=0;
$weight="weight.dat";
$weight_TEL_LOC=1;           ################################################
$weight_P_SH=1;           ################################################
$switch_P_SH=0;           ################################################
# plotting
$plot = 0;
$amplify = 100000;
$sec_per_inch = 40;
$keep = 0;
$dura = 0;
$rise = 0.45;

# filters and window lengths
($f1_pnl, $f2_pnl, $f1_sw, $f2_sw, $m1, $m2) = (0.02,0.2,0.02,0.1,35,70);

# max. shifts
$max_shft1=1;		# max. shift for Pnl
$max_shft2=5;		# max. shift for surface wave
$tie = 0.5;		# tie between SV and SH

# weights between different portions
$weight_of_pnl=2;		# weight for pnl portions
$power_of_body=1;		# distance scaling power for pnl waves
$power_of_surf=0.5;

# apparent velocities
($vp, $love, $rayleigh) = (7.8, 3.5, 3.1);
#($vp, $love, $rayleigh) = (-1, -1, -1);

# default grid-search ranges
($deg, $dm) = (10, 0.1);
$str1 = 0; $str2 = 360;
$dip1 = 0; $dip2 = 90;
$rak1 = -90; $rak2 = 90;

# number of freedom per sample for estimating uncertainty
$nof = 0.1;
$dt=0.1;

#input options
foreach (grep(/^-/,@ARGV)) {
   $opt = substr($_,1,1);
   @value = split(/\//,substr($_,2));
   if ($opt eq "C") {
     ($f1_pnl, $f2_pnl, $f1_sw, $f2_sw) = @value;
   } elsif ($opt eq "D") {
     ($weight_of_pnl,$power_of_body,$power_of_surf)=@value;
   } elsif ($opt eq "F") {
     $fm_thr = 0.01;
     $fm_thr = $value[0] if $#value >= 0;
   } elsif ($opt eq "G") {
     $green = substr($_,2);
   } elsif ($opt eq "H") {
     $dt = $value[0];
   } elsif ($opt eq "I") {
     $deg = $value[0];
     $dm = $value[1] if $#value > 0;
   } elsif ($opt eq "L") {
     $dura = $value[0];
   } elsif ($opt eq "M") {
     ($model,$mg) = @value;
   } elsif ($opt eq "N") {
     $repeat = $value[0];
   } elsif ($opt eq "O") {
     $output = 1;
     print STDERR "output = $output\n";
   } elsif ($opt eq "P") {
     $plot = 1;
     $amplify = $value[0] if $#value >= 0;
     $sec_per_inch = $value[1] if $#value > 0;
     $keep = 1 if $#value > 1;
   } elsif ($opt eq "Q") {
     $nof = $value[0];
   } elsif ($opt eq "R") {
     ($str1,$str2,$dip1,$dip2,$rak1,$rak2) = @value;
   } elsif ($opt eq "S") {
     ($max_shft1, $max_shft2) = @value;
     $tie = $value[2] if $#value > 1;
   } elsif ($opt eq "T") {
     ($m1, $m2) = @value;
   } elsif ($opt eq "V") {
     ($vp, $love, $rayleigh) = @value;
   } elsif ($opt eq "U") {
     ($rupDir) = @value;
     $pVel = 6.4;
     $sVel = 3.6;
     $riseTime = 0.45;
     $dir = "_dir";
   } elsif ($opt eq "W") {
     $disp = $value[0];
   } elsif ($opt eq "X") {
     $mltp = $value[0];
   } elsif ($opt eq "Z") {
     $weight = $value[0];
   } elsif ($opt eq "A"){
	 $weight_TEL_LOC=$value[0] if $value[0]>0 ;
	 $weight_P_SH=$value[1] if $value[1]>0 ;
	 $switch_P_SH=$value[2]  if $#value==2
   }else {
     printf STDERR $usage;
     exit(0);
   }
}
@event = grep(!/^-/,@ARGV);

if ($dura == 0) {
     $dura = int(10**(($mg-5)/2)+0.5);
     $dura = 1 if $dura < 1;
     $dura = 9 if $dura > 9;
}

foreach $eve (@event) {

  next unless -d $eve;
  ($mmdd, $depth) = split('_',$model);


  print STDERR "$eve $model $dura\n";

  open(WEI, "$eve/$weight") || die "could open $weight\n";
  @wwf=<WEI>;
  close(WEI);

  $wwff_count=0;
  @wwff=();
  foreach(@wwf){
     @line = split;
     if( $line[1] > 1000 ){
	$line[3]=1000*$line[3]*$weight_TEL_LOC*$weight_P_SH;
	$line[7]=1000*$line[7]*$weight_TEL_LOC;
          if ($switch_P_SH>0){
		$line[3]=0;
	  }elsif($switch_P_SH<0){
		$line[7]=0;
	  }

     }
    $wwff[$wwff_count++]="$line[0] $line[1] $line[2] $line[3] $line[4] $line[5] $line[6] $line[7] $line[8] $line[9] $line[10]\n";
  }
#  $cap = "/cygdrive/e/mywork/cap_test/subhir/cap.exe";
#  $cap = "/home/chenweiwen/myopt/cap/subhir/cap";
#  $cap = "/oldhome/stone/bin/cap";
#  $cap = "/home/cww/bin/cap";


  open(SRC, "| $cap $eve $model") || die "can not run $cap\n";                       # cap cap_dir model  <=> argv[1] ; argv[2]
#  printf "<< cap.pl | cap.exe $eve $model >>\n";

  printf SRC "$pVel $sVel %6.2f $dura $rupDir\n",$riseTime*$dura if $dir;            # pvel Svel risetime*dura dura rupDir   <=> #ifdef DIRECTIVITY
#  printf "<< cap.pl | $pVel $sVel %6.2f $dura $rupDir >>\n",$riseTime*$dura if $dir;

  print SRC "$m1 $m2 $max_shft1 $max_shft2 $repeat $fm_thr $tie\n";                  # -T35/70  m1=35 m2=70
#  printf "<< cap.pl | $m1 $m2 $max_shft1 $max_shft2 $repeat $fm_thr $tie >>\n";

  print SRC "$vp $love $rayleigh\n";
#  printf "<< cap.pl | $vp $love $rayleigh >>\n";

  print SRC "$power_of_body $power_of_surf $weight_of_pnl $nof\n";                   # -D2/0/0 power_of_body=0 power_of_surf=0 weight_of_pnl=2
#  printf "<< cap.pl | $power_of_body $power_of_surf $weight_of_pnl $nof >>\n";
  print SRC "$plot $output\n";
#  printf "<< cap.pl | $plot $output >>\n";
  print SRC "$disp $mltp\n";
#  printf "<< cap.pl | $disp $mltp >>\n";
  print SRC "$green/\n";                 #     greenFunc_dir
#  printf "<< cap.pl | $green/ >>\n";
  if (-r "src.sac") {
  print SRC "0. src.sac\n";
#  printf "<< cap.pl | 0. src.sac >>\n";
} else {print SRC "$dt $dura $rise\n";
#	printf "<< cap.pl | $dt $dura $rise >>\n";
}
  print SRC "$f1_pnl $f2_pnl $f1_sw $f2_sw\n";          #     -C0.01/0.1/0.01/0.1
#  printf "<< cap.pl | $f1_pnl $f2_pnl $f1_sw $f2_sw >>\n";
  print SRC "$mg $dm\n";
#  print "<< cap.pl | $mg $dm >>\n";
  print SRC "$str1 $str2 $deg\n";
#  printf "<< cap.pl | $str1 $str2 $deg >>\n";
  print SRC "$dip1 $dip2 $deg\n";
#  printf "<< cap.pl | $dip1 $dip2 $deg >>\n";
  print SRC "$rak1 $rak2 $deg\n";
#  printf "<< cap.pl | $rak1 $rak2 $deg >>\n";
  printf SRC "%d\n",$#wwff + 1;
#  printf "<< cap.pl | %d >>\n",$#wwff + 1;
  print SRC @wwff;
#  printf "<< cap.pl | \n @wwff >>\n"; ########################################
  close(SRC);
  print STDERR "inversion done\n";

  plot:
  if ( $plot > 0 && ($? >> 8) == 0 ) {
     chdir($eve);
     &plot($model, $m1, $m2, $amplify, 5, $sec_per_inch,$weight_TEL_LOC,$weight_P_SH);
     unlink(<${model}_*.?>) unless $keep;
     chdir("../");
  }

}
exit(0);
