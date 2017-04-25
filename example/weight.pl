#!/usr/bin/env perl
use strict;
use warnings;
$ENV{SAC_DISPLAY_COPYRIGHT} = 0;

@ARGV >= 1 or die "Usage: perl $0 dirs\n";
my @dir = @ARGV;

my $ts = 0;
my $ddist = 5;
my $weight = "weight.dat";

foreach my $dir (@dir){
    chdir $dir or die "can not open $dir\n";
    print "===== $dir WEIGHT FILE =====\n";
    open(IN, "saclst dist f *.z | sort -k2n |");
    my @lines = <IN>;
    close(IN);

    open (OUT, "> ./$weight");
    foreach my $line (@lines) {
        my ($fname, $dist) = split m/\s+/, $line;
        my ($net, $sta, $loc) = split m/\./, $fname;
        if ($dist < 600) {
            print OUT "${net}.${sta}.${loc} $dist $dist 1 1 1 1 1 1 0 0\n";
        }elsif ($dist > 3000) {
            $dist = int $dist;# do not delete!
            print OUT "${net}.${sta}.${loc} $dist $dist 1 0 0 0 1 0 0 0\n";
        }
    }
    chdir "..";
}
