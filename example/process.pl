#!/usr/bin/env perl
use strict;
use warnings;

@ARGV >= 1 or die "Usage: \n\tperl $0 event1 event2 ... eventn\n";

my @events = @ARGV;

foreach my $event (@events) {
    print "=== RDSEED ===\n";
    system "perl rdseed.pl $event";
    print "=== EVENTINFO ===\n";
    system "perl eventinfo.pl $event";
    print "=== MARKTIME ===\n";
    system "perl marktime.pl $event";
    print "=== TRANSFER ===\n";
    system "perl transfer.pl $event";
    print "=== ROTATE ===\n";
    system "perl rotate.pl $event";
    print "=== RESSAMPLE ===\n";
    system "perl resample.pl $event";
}
