#!/usr/bin/perl

# this routine is used to generate the favourable depth inversion result
# by binomial approximation
# Written by Weiwen Chen
# Last modified in 2014/04/02

# Input: misfit versus depth curve in "yyy.dat" file
# Output: value of the central axis of three lowest mifit points

open(RSL,"./yyy.dat") or die "couldn't open $rsl\n";
@aaa=<RSL>;
close(RSL);

$ii=1;
$best=1;
$min=1.0e+9;

foreach $aa (@aaa) {
	@item = split /\s+/,$aa ;
	$dep[$ii] = $item[0];
	$rms[$ii] = $item[1];
	if($min>$rms[$ii]) {
		$best = $ii;
		$min = $rms[$ii];
	}
	$ii++;
}

next unless $ii>1;

  $adj=0.; $adj=0.001*$rms[$best] if $rms[$best-1] eq $rms[$best] and $rms[$best+1] eq $rms[$best];
  $d1 = $dep[$best]-$dep[$best-1];
  $d2 = $dep[$best+1]-$dep[$best];
  $sigma = $d1*$d2*($d1+$d2)/($d2*($rms[$best-1]-$rms[$best])+$d1*($rms[$best+1]-$rms[$best])+$adj*($d1+$d2));
  $depth = 0.5*($rms[$best+1]-$rms[$best-1])*$sigma/($d1+$d2);
  $min = $rms[$best] - $depth*$depth/$sigma;
  $depth = $dep[$best] - $depth;
  $A = ($rms[$best]-$min)/(($dep[$best]-$depth)**2);
  
#  print  "The real depth is %5.2f \n", $depth;
  print  "$A  $depth $min";

