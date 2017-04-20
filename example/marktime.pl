#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(min);
$ENV{SAC_DISPLAY_COPYRIGHT} = 0;

@ARGV == 1 or die "Usage: perl $0 dir\n";
my ($dir) = @ARGV;

my $taup = `which taup_time`;
exit "Skip marking arrival times in SAC files because Taup isn't install\n" unless (defined($taup));

chdir $dir;

# 标到时
open(SAC, "| sac") or die "Error in opening SAC\n";
print SAC "wild echo off\n";
foreach my $Zfile (glob "*Z.SAC") {
    my ($net, $sta, $loc) = split /\./, $Zfile;
    my (undef, $evdp, $gcarc) = split /\s+/, `saclst evdp gcarc f $Zfile`;

    # 不小于 30 度的台站
    if ($gcarc >= 30) {
        # p 波标在 *.?[LH]Z, t1是P或p的初至
        my @time = split /\s+/, `taup_time -mod prem -ph P,p -h $evdp -deg $gcarc --time`;
        my $t1 = min @time;
        print SAC "r ${net}.${sta}.${loc}.*Z.SAC\n";
        print SAC "ch t1 $t1\n";
        print SAC "wh\n";
        # S 波标在 *?[LH][EN12], t1是S或s的初至
        @time = split /\s+/, `taup_time -mod prem -ph S,s -h $evdp -deg $gcarc --time`;
        $t1 = min @time;
        print SAC "r ${net}.${sta}.${loc}.*[EN12].SAC\n";
        print SAC "ch t1 $t1\n";
        print SAC "wh\n";
    }
}
print SAC "q\n";
close(SAC);

# cut数据
open(SAC, "| sac") or die "Error in opening SAC\n";
print SAC "wild echo off\n";
foreach my $Zfile (glob "*Z.SAC") {
    my ($net, $sta, $loc) = split /\./, $Zfile;
    my (undef, $evdp, $gcarc, $b, $t1) = split /\s+/, `saclst evdp gcarc b t1 f $Zfile`;
    if ($gcarc >= 30) {
        # 不小于 30 度的台站: 使其 a = 10s, 物理意义为 P/p, S/s, t1/t3 = 8s, t2/t4 = 52s
        print SAC "cut t1 -10 t1 50\n";
        print SAC "r ${net}.${sta}.${loc}.*.SAC\n";
        print SAC "ch b 0 a 10 t1 8 t2 52 t3 8 t4 52\n";
        print SAC "w over\n";
    } elsif ($gcarc <= 5) {
        # 不大于 5 度的台站
        print SAC "cut 0 180\n";
        print SAC "r ${net}.${sta}.${loc}.*.SAC\n";
        print SAC "w over\n";
    } else {
        unlink glob "${net}.${sta}.${loc}.*.SAC";
    }
}
print SAC "q\n";
close(SAC);

chdir "..";
