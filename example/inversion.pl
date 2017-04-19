#!/usr/bin/env perl
use strict;
use warnings;
require config;

@ARGV >= 1 or die "Usage: perl $0 dirname";
my @dir = @ARGV;

foreach my $event (@dir){
    my %pars = read_config($event);
    my $weight = $pars{'-Z'};
    die "no weight file\n" if !-e "$event/$weight";

    # 获取反演的震源深度
    my @depth = split /\s+/, $pars{'DEPTH'};
    my @log;
    system "rm -f $event/*.ps $event/*.out $event/*.grid $event/*.grid1";
    system "cp gmtdefaults $event/.gmtdefaults";
    foreach my $depth (@depth) {
        # deal with -M option
        my $cap_args = "$pars{'cap_args'} -M$pars{'MODEL'}_${depth}/$pars{'MAG'}";
        system "perl cmds/cap_3.pl $cap_args $event";
        #system "perl cmds/cap_3.pl -A50/1/0 -C0.01/0.1/0.01/0.08 -D3/0/0 -G./greenFuncDir -H0.05 -I10/0.1 -L10 -O -P0.8e6/90/k -R0/360/0/90/-180/180 -S5/10/0 -T60/60 -V-1/-1/-1 -W1 -Zweight.dat -Mvmodel_$depth/6.8 $event";
        system "rm -f $event/*.[0-9]";
    }
    chdir "$event" or die;
    system "cp ../cmds/mecherr.cmd .";
    system "sh mecherr.cmd";
    system "rm -f *.grid *.grid1";
    chdir "..";
}
