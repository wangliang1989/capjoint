#!/usr/bin/env perl
use warnings;
use strict;
#use Parallel::ForkManager;

my $depth_start = 1;
my $depth_end = 30;
my $depth_step = 1;

my $nt = 2048;
my $dt = 0.1;
my $smooth = 2;
my $qtp = 1;
my $qts = 5;

@ARGV >= 1 or die "Usage: \n\tperl $0 event1 event2 ... eventn\n";

my @events = @ARGV;

foreach my $event (@events) {
    my $glib = "greenFuncDir";
    # 获取台站震中距和震中经纬度
    chdir "$event" or die;

    my %dist_loc;
    my %dist_tel;
    foreach (glob "*.z") {
        my (undef, $dist) = split m/\s+/, `saclst dist f $_`;
        if (($dist > 3000) and ($dist < 10000)){
            $dist = int $dist;
            $dist_tel{$dist} = 1;
        }
        if ($dist < 600) {
            $dist_loc{$dist} = 1;
        }
    }
    my @dist_tel = sort { $a <=> $b } keys %dist_tel;
    my @dist_loc = sort { $a <=> $b } keys %dist_loc;
    open(INFO, "< event.info") or die "Cannot find event info.";
    my $eventinfo = <INFO>;
    my ($origin, $evla, $evlo, $evdp, $mag) = split " ", $eventinfo;
    close(INFO);

    chdir "..";
    # 生成地层模型
    my $model_tel = "model_tel";# 远震模型文件名
    my $model_loc = "model_loc";# 近震模型文件名
    my $model = "model";# 计算用模型文件名

    chdir "../src/crust2" or die;

    open (CRUST, "| ./getCN2point") or die;
    print CRUST "$evla $evlo\n";
    print CRUST "q\n";
    close (CRUST);
    #  远震地层模型
    system "cat outcr | gawk \'NR>=6&&\$1!=0{print \$2,\$3,\$4,\$1;}\' > model.tmp";
    my ($num_layer) = split m/\s+/, `(wc -l model.tmp | gawk \'{print \$1+1;}\')`;
    open (OUT, "> $model_tel");
    print OUT "#$model\n";
    print OUT "$qtp $qts $num_layer ";
    close OUT;
    system "cat model.tmp | gawk \'{print \"echo \"\$0\" >> $model_tel\";}\' | sh";
    system "cat ../../example/cmds/default_mid_receiver_model >> $model_tel";
    unlink "model.tmp";
    #  近震地层模型
    system "cat outcr | gawk \'NR>=6&&\$1!=0&&\$3!=0{print \$1+0.01,\$3,\$2,\$4;}\' > $model_loc";
    system "cat ../../example/cmds/default_loc_uppermost_mantle >> $model_loc";# IASP91

    chdir "../../example";

    # 计算格林函数
    mkdir "$glib";

    chdir "$glib" or die;
    system "mv ../../src/crust2/$model_loc $model_loc";
    system "mv ../../src/crust2/$model_tel $model_tel";
    #  近震格林函数
    die unless (-e $model_loc);
    system "cp $model_loc $model";
    #my ($MAX_PROCESSES) = split /\n/, `cat /proc/cpuinfo |grep "processor"|wc -l`;
    #my $pm = Parallel::ForkManager -> new($MAX_PROCESSES);
    for (my $depth = $depth_start; $depth <= $depth_end; $depth = $depth + $depth_step) {
        #my $pid = $pm -> start and next;
        #system "fk_parallel.pl -M$model/$depth -N$nt/$dt/$smooth -S2 @dist_loc";
        #$pm -> finish;
        system "fk.pl -M$model/$depth -N$nt/$dt/$smooth -S2 @dist_loc";
    }
    #$pm -> wait_all_children;

    #  远震格林函数
    die unless (-e $model_tel);
    system "cp $model_tel $model";
    for(my $depth = $depth_start; $depth <= $depth_end; $depth = $depth + $depth_step) {
        system "perl ../cmds/tel3.pl -O./ -M$model/$depth @dist_tel";
    }

    unlink glob "junk.*";
    unlink "$model";
    chdir "..";
}
