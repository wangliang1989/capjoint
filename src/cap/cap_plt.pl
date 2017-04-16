# this subroutine plots waveform fits produced by source inversion srct

$pssac = "/home/ytan/bin_PC/pssac2";
sub plot {
  local($model, $t1, $t2, $am, $num_com, $sec_per_inch) = @_;
  local($nn,$tt,$plt1,$plt2,$plt3,$plt4,$i,$nam,$com1,$com2,$j,$x,$y,@aa,$rslt,@name,@aztk);
  local $keepBad = 1;
  
  @trace = ("1/255/255/255","3/0/0/0");       # plot data trace
  @name = ("Pnl V","Pnl R","Vertical.","Radial","Tang.");
  
  ($nn,$hight) = (12,10.5);	# 10 rows of traces per 10.5 in.
  
  $sepa = 0.1*$sec_per_inch;
  $tt = 2*$t1 + ($num_com-2)*$t2+($num_com-1)*$sepa;
  $width = 0.1*int(10*$tt/$sec_per_inch+0.5);
  @x0 = ($t1+$sepa, $t1+$sepa, $t2+$sepa, $t2+$sepa, $t2);
  
  $plt1 = "| $pssac -JX$width/$hight -R0/$tt/0/$nn -Y0.2 -Ent-2 -M$am/0 -K -P>> $model.ps";
  $plt2 = "| pstext -JX -R -O -K -N >> $model.ps";
  $plt3 = "| psmeca -JX1/1 -R-1/1/-1/1 -Sa5 -Y9.2 -X-0.8 -O -K >> $model.ps";
  $plt4 = "| psxy -JPa1 -R0/360/0/1 -Sc0.075 -W4/0/0/0 -G255/255/255 -O -N >> $model.ps";
#  $plt22 = "| pstext -JP -R -O -N -G255/0/0 >> $model.ps";
  #$plt1=$plt2=$plt3="|cat";		# for testing

  open(FFF,"$model.out");
  @rslt = <FFF>;
  close(FFF);
  @meca = split('\s+',shift(@rslt));
  @others = grep(/^#/,@rslt); @rslt=grep(!/^#/,@rslt);

  unlink("$model.ps") if -e "$model.ps";
  while (@rslt) {
    open(PLT, $plt1);
    $i = 0; @aztk=();
    @aaaa = splice(@rslt,0,$nn-3);
    foreach (@aaaa) {
      @aa = split;
      $nam = "${model}_$aa[0].";
      $x=0;
      $com1=2*($num_com-1);
      $com2=$com1+1;
      for($j=0;$j<$num_com;$j++) {
        #printf PLT "%s %f %f $trace[$aa[4*$j+2]>0]\n",$nam.$com1,$x,$nn-$i-2;
	if ($aa[5*$j+2]>0) {
	   printf PLT "%s %f %f 5/0/0/0\n",$nam.$com1,$x,$nn-$i-3;
	   printf "$plt1 %s %f %f 5/0/0/0\n",$nam.$com1,$x,$nn-$i-3;
	} elsif ($keepBad) {
	   printf PLT "%s %f %f 3/0/255/0\n",$nam.$com1,$x,$nn-$i-3;
	}
        printf PLT "%s %f %f 3/255/0/0\n",$nam.$com2,$x,$nn-$i-3;
        $x = $x + $x0[$j];
        $com1-=2;
        $com2-=2;
      }
      $aztk[$i] = `saclst az user1 f ${nam}0`;
      $i++;
    }
    close(PLT);
    
    open(PLT, $plt2);
    $y = $nn-3;
    foreach (@aaaa) {
      @aa = split;
      $x = 0;
      printf PLT "%f %f 10 0 0 1 $aa[0]\n",$x-0.6*$sec_per_inch,$y;
      printf PLT "%f %f 10 0 0 1 $aa[1]\n",$x-0.4*$sec_per_inch,$y-0.25;
      for($j=0;$j<$num_com;$j++) {
        printf PLT "%f %f 10 0 0 1 $aa[5*$j+6]\n",$x,$y-0.4;
#        printf PLT "%f %f 10 0 0 1 $aa[5*$j+4] $aa[5*$j+5]\n",$x,$y-0.6;
        printf PLT "%f %f 10 0 0 1 $aa[5*$j+4]\n",$x,$y-0.6;
        $x = $x + $x0[$j];
      }
      $y--;
    }
#    printf PLT "%f %f 11 0 1 LM Event %s  Model_depth %s  FM %-3d %-3d %-3d  Mw %4.2f  Error %s\n",-0.4*$sec_per_inch,$nn-1,@meca[1,3,5,6,7,9,11];
    printf PLT "%f %f 10 0 0 1 @meca\n",0.5*$sec_per_inch,$nn-0.5;
    $x = 0.2*$sec_per_inch;
    for($i=0;$i<$num_com;$i++) {
      printf PLT "%f %f 9 0 0 1 $name[$i]\n",$x-18,$nn-2.65;
      $x = $x+$x0[$i];
    }
    close(PLT);
    if($meca[6] > 90) {
       $meca[5] += 180;
       $meca[6] = 180-$meca[6];
       $meca[7] = -1*$meca[7];
    } 
    open(PLT, $plt3);
    printf PLT "0 0 0 @meca[5,6,7] 1\n";#0.5*$sec_per_inch,$nn-1;
    $x = 2;
    foreach (@others) {
       split;
       if($_[2] > 90) {
          $_[1] += 180;
          $_[2] = 180 - $_[2];
          $_[3] = -1*$_[3];
       }
       printf PLT "%f -0.2 0 @_[1,2,3] 0.5 $_[4]\n",$x; $x+=1.5;
       printf  "%f -0.2 0 @_[1,2,3] 0.5 $_[4]\n",$x; $x+=1.5;
    }
    close(PLT);
    open(PLT, $plt4);
#    open(PLT2,$plt22);
    foreach (@aztk) {
      @aa = split;
      @bb = split(/\_/,$aa[0]);
      if ($aa[2]>90.) {$aa[1] += 180; $aa[2]=180-$aa[2];}
      printf PLT "$aa[1] %f\n",sqrt(2.)*sin($aa[2]*3.14159/360);
#      printf PLT2 "%f %f 8 0 0 1 $bb[2]\n",$aa[1],sqrt(2.)*sin($aa[2]*3.14159/360);
    }
    close(PLT);
#    close(PLT2);
    
  }
  
}
1;
