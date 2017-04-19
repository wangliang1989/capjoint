#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(min);
use List::Util qw(max);
require config;

@ARGV >= 1 or die "Usage: perl $0 dirname";
my @dir = @ARGV;

foreach my $event (@dir){
    chdir "$event" or die;

    my ($dep_best, $strike_best, $dip_best, $rake_best, $mag_best, $rms_best);
    foreach (glob ("*.out")) {
        open (IN, "< $_") or die;
        foreach (<IN>) {
            my @info = split m/\s+/;
            next if ($info[0] ne 'Event');
            #Event data Model vmodel_11 FM 327 61 -172 Mw 6.80 rms 8.961e+01  2680 ERR   0   1   1
            #0     1    2     3         4  5   6  7    8  9    10  11
            my ($model_dep, $strike, $dip, $rake, $mag, $rms) = ($info[3], $info[5], $info[6], $info[7], $info[9], $info[11]);
            my (undef, $dep) = split m/_/, $model_dep;
            if (defined($rms_best)) {
                ($dep_best, $strike_best, $dip_best, $rake_best, $mag_best, $rms_best) = ($dep, $strike, $dip, $rake, $mag, $rms) if ($rms <= $rms_best);
            } else {
                ($dep_best, $strike_best, $dip_best, $rake_best, $mag_best, $rms_best) = ($dep, $strike, $dip, $rake, $mag, $rms);
            }
        }
        close (IN);
    }
    print "The Best Result:\n";
    print "DEPTH: $dep_best\nSRIKE: $strike_best DIP: $dip_best RAKE: $rake_best\nMAG: $mag_best RMS: $rms_best\n";

    my $stk_min = max (0, ($strike_best - 10));
    my $stk_max = min (360, ($strike_best + 10));
    my $dip_min = max (0, ($dip_best - 10));
    my $dip_max = min (90, ($dip_best + 10));
    my $rak_min = max (-180, ($rake_best - 10));
    my $rak_max = min (180, ($rake_best + 10));
    print "\nI Suggest You:\n-I1/0.1\n";
    print "-R$stk_min/$stk_max/$dip_min/$dip_max/$rak_min/$rak_max\n";

    my $dep_min = max (0, ($dep_best - 10));
    if ($dep_min <= 0) {
        if ($dep_best % 2 == 0) {
            $dep_min = 2;
        } else {
            $dep_min = 1;
        }
    }
    my $dep_max = $dep_best + 10;
    print "Depth Range: $dep_min $dep_max\n";
}
