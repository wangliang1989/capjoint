# this subroutine plots waveform fits produced by source inversion srct
# modified by Weiwen @ Oct 2. 2013
$pssac = "pssac";
sub plot {
  local($model, $t1, $t2, $am, $num_com, $sec_per_inch,$wt_tel_loc,$wt_P_SH,$wt_of_pnl,$dura) = @_;
  local($nn,$tt,$plt1,$plt2,$plt3,$plt4,$i,$nam,$com1,$com2,$j,$x,$y,@aa,$rslt,@name,@aztk,$am1,$am2);
  local $keepBad = 1;
  @trace = ("1/255/255/255","3/0/0/0");       # plot data trace
  @name1 = (" Pnl V"," Pnl R"," Vertical."," Radial"," Tang"); #1:local
  @name2 = ("P","","","","SH");				       #2:teles
#  $sepa = 0.1*$sec_per_inch;
  $tt = 2*$t1 + ($num_com-2)*$t2;  # 2 Pnl and 3 Surface
  $sep = $tt/5;  #total separation, trace plus blank
  $sepa = $tt/(5*5); # sigal separation, blank only
printf "tt sep sepa is $tt $sep $sepa\n";
  $scalar=1; # amplitude between loc and tel
  @segment = ($t1+$sepa, $t1+$sepa, $t2+$sepa, $t2+$sepa, $t2);
#  @segment = ($sepa, $t1+$sepa, $t1+$sepa, $t2+$sepa, $t2);
  $shift_title=$tt/4;
  $Sa=0.8;   # adjust size of beachball
  $Saa=4;  # beach ball for large fig i.e. model_1.fig



  #########################PRINT cap_3.cmd INFO###############################
  	open(FFF,"../cap_3.cmd");
	@cap_linux = <FFF>;
	close(FFF);
	@cap_linux=grep(!/^#/,@cap_linux);  # delete elements start with '#'
	@cap_linux=grep(/cap/,@cap_linux);  # choose the line with 'cap'
	@item = split('\s+',shift(@cap_linux));
	$count_item=0;
	foreach (@item){
		if ( ($item[$count_item]) =~ /^-L/ )  {$item_L=$item[$count_item] ;};
		if ( ($item[$count_item]) =~ /^-C/ )  {$item_C=$item[$count_item] ;};
		if ( ($item[$count_item]) =~ /^-P/ )  {$item_P=$item[$count_item] ;};
		if ( ($item[$count_item]) =~ /^-S/ )  {$item_S=$item[$count_item] ;};
		if ( ($item[$count_item]) =~ /^-T/ )  {$item_T=$item[$count_item] ;};
		if ( ($item[$count_item]) =~ /^-R/ )  {$item_R=$item[$count_item] ;};
		$count_item++
	}
  #################### READ model_?.out parameters ################################
  open(FFF,"$model.out");
  @rslt = <FFF>;
  close(FFF);
  @meca = split('\s+',shift(@rslt)); # only first line
  @others = grep(/^#/,@rslt); @rslt=grep(!/^#/,@rslt);  # 2nd lines to end
  $rms_tel=0;
  $rms_loc=0;
  $rms_total=0;
  $rms_P=0;
  $rms_SH=0;
  $n_tel=0;
  $n_loc=0;
  $n_P=0;
  $n_SH=0;
  ################# to every staions,compute rms#############################
        foreach (@rslt) {
		@aa = split;
		@stna_dist = split(/\_/,$aa[0]);

		if($stna_dist[1]>1000){
			$tel_count++;
			for ($j=0;$j<=4;$j++){
				if ( $aa[$j*5+2]>0 ){
					$rms_tel=$rms_tel+$aa[$j*5+2]*$aa[$j*5+3];
					$n_tel++;
					if($j==0 ){  # first seg is P
						$n_P++;
						$rms_P=$rms_P+$aa[$j*5+2]*$aa[$j*5+3];
					}else{
						$n_SH++;
						$rms_SH=$rms_SH+$aa[$j*5+2]*$aa[$j*5+3];
					}
				}
			}

		}else{      # local waveforms
			$loc_count++;
                        for ($j=0;$j<=4;$j++){
                                if ( $aa[$j*5+2]>0 ){
                                        $rms_loc=$rms_loc+$aa[$j*5+2]*$aa[$j*5+3];
                                        $n_loc++;
				}
                        }

		}
    	}

	$rms_total=$rms_loc+$rms_tel;

#####################################################################################
  	$total_count=$loc_count+$tel_count ;
        $loc_line = $loc_count             ;
	$tel_line = int($tel_count/2+0.5)  ;
   	$total_line = $loc_line+$tel_line  ;
########################################################################################
  #	$width = 0.1*int(10*($tt+$t2+$sepa)/$sec_per_inch+0.5);
        $width = 7;
        $hight = 7.5;

        $Rw=-$sepa*3; $Re=$tt+$sep+$sepa*2 ; $Rs=0; $Rn=$total_line+3;
 #       $Rw=-$sepa*3; $Re=150 ; $Rs=0; $Rn=$total_line+1;

  $vel_grid = ($Rn-$Rs)/$hight;
  $hol_grid = ($Re-$Rw)/$width;

#=========for loc

	$Rwx=0; $Rex=$t1 ; $Rsx=$Rs; $Rnx=$loc_line;
	$widthx = $width*($Rex-$Rwx)/($Re-$Rw);
	$hightx = $hight*($Rnx-$Rsx)/($Rn-$Rs);

	$rightPnl1 = $width*($Rwx-$Rw)/($Re-$Rw);
	$rightPnl2 = $width*$segment[0]/($Re-$Rw);
	$rightSF = $width*$segment[3]/($Re-$Rw);
	$rightLV = $width*$segment[3]/($Re-$Rw);
#================
#=========for tel

	$Rw1=0; $Re1=$t1 ; $Rs1=0; $Rn1=$Rs1+$tel_line;
	$width1 = $width*($Re1-$Rw1)/($Re-$Rw);
	$hight1 = $hight*($Rn1-$Rs1)/($Rn-$Rs);

	$rightP1 = $width*($Rw1-$Rw)/($Re-$Rw);
	$rightSH = $width*$segment[3]/($Re-$Rw);
	$rightP2 = $width*(2*$t1+2*$sepa)/($Re-$Rw);

	$shiftback = -$rightPnl1-$rightPnl2-2*$rightSF-$rightLV;
	$shifttel = $rightP1 + $shiftback;
	$shiftup = $hightx/$loc_line*($loc_line+2);

#================

	$am1=0.000001*$am; $am2=0.000001*$am;
        $shiftx=-$sepa;
        $shifty=$Rn+1.5*$vel_grid; # shiftx,y for beach ball position
	printf "WESN is $Rw $Re $Rs $Rn\n";




#######################################################################################
##plt0:basemap plt1:pssac plt2:pstext plt3:psmeca plt4:psxy
	$plt0  = " psbasemap   -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn  -K -P -Bn  > $model.ps";



#==loc
	$pltPnl1  = " psbasemap   -JX${widthx}i/${hightx}i -R$Rwx/$Rex/$Rsx/$Rnx -Ba15g100:\"Pnl(V)\":/g1S  -K -P -O  -X${rightPnl1}i  >> $model.ps";
	$pltPnl2  = " psbasemap   -JX${widthx}i/${hightx}i -R$Rwx/$Rex/$Rsx/$Rnx -Ba15g100:\"Pnl(R)\":/g1S  -K -P -O  -X${rightPnl2}i  >> $model.ps";
	$pltSF  = " psbasemap   -JX${widthx}i/${hightx}i -R$Rwx/$Rex/$Rsx/$Rnx -Ba15g100:\"Rayleigh(H)\":/g1S  -K -P -O  -X${rightSF}i  >> $model.ps";
	$pltLV  = " psbasemap   -JX${widthx}i/${hightx}i -R$Rwx/$Rex/$Rsx/$Rnx -Ba15g100:\"Love(T)\":/g1S  -K -P -O  -X${rightSF}i  >> $model.ps";

#==tel
	$pltP1  = " psbasemap   -JX${width1}i/${hight1}i -R$Rw1/$Re1/$Rs1/$Rn1 -Ba15g100:.\"P\":/g1S  -K -P -O  -X${shifttel}i -Y${shiftup}i >> $model.ps";
	$pltSH  = " psbasemap   -JX${width1}i/${hight1}i -R$Rw1/$Re1/$Rs1/$Rn1 -Ba15g100:.\"SH\":/g1S  -K -P -O  -X${rightSH}i  >> $model.ps";
	$pltP2  = " psbasemap   -JX${width1}i/${hight1}i -R$Rw1/$Re1/$Rs1/$Rn1 -Ba15g100:.\"P\":/g1S  -K -P -O  -X${rightP2}i  >> $model.ps";

	$plt00  = " psbasemap  -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn  -K -P -G255/255/255 -X4i -Y-3i > ${model}_1.ps";
  	$plt1_loc  = "| $pssac -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn -O -K -Ent-2 -M$am1 >> $model.ps";
  	$plt1_tel  = "| $pssac -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn -O -K -Ent-2 -M$am2 >> $model.ps";
  	$plt2  = "| pstext     -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn  -O -K -P -N >> $model.ps";
  	$plt20  = "| pstext    -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn  -O -K -P -N >> ${model}_1.ps";
  	$plt200  = "| pstext     -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn -O -K -P -N >> $model.ps";

  	$plt3  = "| psmeca     -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn  -O -K -P -N -Sa${Sa}i  -G220/220/220  >> $model.ps";
  	$plt30  = "| psmeca    -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn  -O -K -P -N -Sa${Saa}i  -G220/220/220  >> ${model}_1.ps";
	$plt300  = "| psmeca   -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn  -O -K -P -N -Sa${Saa}i  -T0  >> ${model}_1.ps";
  	$plt4  = "| psxy       -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn  -O -K -P -N -Sc0.075i -W0.005p/255/255/0 -G255/0/0 >> $model.ps";
  	$plt41  = "| psxy       -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn  -O -K -P -N -St0.075i -W0.005p/255/255/0 -G255/0/0 >> $model.ps";
  	$plt40  = "| psxy      -JX${width}i/${hight}i -R$Rw/$Re/$Rs/$Rn  -O -K -P -N -Sc0.15i -W0.005p/255/255/0 -G255/0/0 >> ${model}_1.ps";


##############################################   psbasemap  ######################################################################
      	unlink("$model.ps") if -e "$model.ps";
      	unlink("${model}_1.ps") if -e "${model}_1.ps";
    	system($plt0);
	system($plt00);


##############################################   psmeca   ############################################################################

        if($meca[6] > 90) {
                $meca[5] += 180;
                $meca[6] = 180-$meca[6];
                $meca[7] = -1*$meca[7];
        }

        open(PLT, $plt3);
        printf "$plt3 $shiftx $shifty 0 @meca[5,6,7] 10\n";
        printf  PLT " $shiftx $shifty 0 @meca[5,6,7] 10\n";
        close(PLT);

	open(PLT, $plt30);
        printf "$plt30 $shiftx $shifty 0 @meca[5,6,7] 10\n";
        printf  PLT "  $shiftx $shifty 0 @meca[5,6,7] 10\n";
        close(PLT);

	open(PLT, $plt300);
        printf "$plt300 $shiftx $shifty 0 180 90 0 10\n";
        printf  PLT " $shiftx $shifty 0 180 90 0 10\n";
        close(PLT);



#############################################################################


	@pstext_string=();                          $count_pstext=0;
	@pstext_string1=();                          $count_pstext1=0;
	@pstext_string1_1=();
	@psxy_string=()  ;                          $count_psxy  =0;
	@psxy_string_1=()  ;
 	@pssac_string1=();   			    $count_pssac1=0;
	@pssac_string2=();			    $count_pssac2=0;
#	$pos_tel_top=$Rn-5;
	$pos_tel_top=$Rn-1;
#	$pos_loc_y=$Rn-6-$tel_line;
	$pos_loc_y=$Rn-2-$tel_line;
	$pos_tel_y=$pos_tel_top;
#############################################################################################################################
  $whole=0;
  while (@rslt) { #if rslt exits
	$whole++;
	@aaaa = splice(@rslt,0,$total_count); #give value to aaaa, empty rslt
	@aztk=();
	$count=0;
	$count_loc=0;
        $count_tel=0;
	$az=0;
 	$flag=1;
	$pos_x=0;
    	foreach (@aaaa) {
      		@aa = split;
#		printf "aa0 is $aa[0]\n";
      		$nam = "${model}_$aa[0]."; #$aa[0]: sta_dist #nam is segments
      		@stna_dist = split(/\_/,$aa[0]);
      		@nt_na = split(/\./,$stna_dist[0]);
#		printf "nt na @nt_na\n";
		$st_az=`saclst az f ${stna_dist[0]}.z` ;
		@stna_az= split('\s+',$st_az);

		$com1=2*($num_com-1);             # com1 :: data #num_com=5 for loc
                $com2=$com1+1;                    # com2 :: syn
	# here com1 is 8, com2 is 9	for 1st P wave
		$az_ih=`saclst az user1 f ${nam}0`;  #az and ih
		@aaa=split('\s+',$az_ih);
		if( $aaa[2]>90.) {                   #go up,   ih > 90 -- local
			$aaa[1]+=180;                #go down, ih < 90 -- teles
			$aaa[2]=180-$aaa[2];
		}
		$az=$aaa[1];             # az on the focal sphere, not real az
		$radius=sqrt(2.)*sin($aaa[2]*3.14159/360); # sqrt2*sin(theta/2)
		$rx=($radius*sin($az*3.14159/180))*$Sa*($Re-$Rw)/$width;
		$rx_1=($radius*sin($az*3.14159/180))*$Saa*($Re-$Rw)/$width;
		$ry=($radius*cos($az*3.14159/180))*$Sa*($Rn-$Rs)/$hight;
		$ry_1=($radius*cos($az*3.14159/180))*$Saa*($Rn-$Rs)/$hight;

=cut
		$pos_xy_x=$shiftx+$rx;
		$pos_xy_x_1=$shiftx+$rx_1;
		$pos_xy_y=$shifty+$ry;
		$pos_xy_y_1=$shifty+$ry_1;
		$psxy_string[$count_psxy++]="$pos_xy_x $pos_xy_y\n";
		$psxy_string_1[$count_psxy++]="$pos_xy_x_1 $pos_xy_y_1\n";
=cut


		$pstext_string1[$count_pstext1++]="$pos_xy_x $pos_xy_y    10 0 0 1 $nt_na[1]\n";   # nt_na1 is station name, draw on focal sphere
		$newx = $pos_xy_x_1 +2.5;
		$pstext_string1_1[$count_pstext1++]="$newx $pos_xy_y_1   20 0 0 1 $nt_na[1]\n";
                ######## above: ready to points on the focal sphere############

		if( $stna_dist[1] > 1000){           #teles
			$count_tel++;


		$pos_xy_x_tel=$shiftx+$rx;
		$pos_xy_x_1_tel=$shiftx+$rx_1;
		$pos_xy_y_tel=$shifty+$ry;
		$pos_xy_y_1_tel=$shifty+$ry_1;
		$psxy_string_tel[$count_psxy++]="$pos_xy_x_tel $pos_xy_y_tel\n";
		$psxy_string_1_tel[$count_psxy++]="$pos_xy_x_1_tel $pos_xy_y_1_tel\n";


			$step_j=4; $step_com=8; $offsetx=0; #skip 4 segments
			if ( $count_tel != $tel_line+1 && $count_tel > 1){
				$pos_tel_y--;   # draw downward
		#if not 2 headers , draw under
			}
			if ( $count_tel==$tel_line+1 ){   # second row head
				$pos_tel_y=$pos_tel_top;
				$pos_x=2*$t1+$t2+3*$sepa;
			}
########################## above: set default seg and text positions ###################
			$pos_y=$pos_tel_y;
	                for($j=0;$j<$num_com;) {  # P then SH
				$str=$pos_x+$offsetx ; $pos_corr=$pos_y+0.2; $pos_shift=$pos_y-0.3 ; # set postions for string , CC and time shift
				if ($j==0){
					$pos_name_x=$str-$t2+$sepa; $pos_name_y=$pos_y;
					                            $pos_az_y=$pos_name_y-0.45;
								    $pos_dist_y=$pos_name_y-0.6;
	$intaz = int($stna_az[1]+0.5);
	$intdist = int($stna_dist[1]/111.195+0.5);
					$pstext_string[$count_pstext++]="$pos_name_x $pos_az_y   8 0 0 CM $intaz\217 $intdist\217\n";     # az
					$pstext_string[$count_pstext++]="$pos_name_x $pos_name_y 10 0 16 CM $nt_na[0] $nt_na[1]\n";   # stna
#	$intdist = int($stna_dist[1]/111.195+0.5);
#					$pstext_string[$count_pstext++]="$pos_name_x $pos_dist_y  9 0 0 CM $intdist degree\n";   # dist
				}
                                if ($aa[5*$j+2]>0 )  { # weight is not zero
					$pssac_string2[$count_pssac2++]="$nam$com1 $str $pos_y 5/0/0/0\n";        # com1: data:black
 					$pssac_string2[$count_pssac2++]="$nam$com2 $str $pos_y 5/255/0/0\n";      # com2: green: red
					$pstext_string[$count_pstext++]="$str $pos_corr  10 0 0 RT $aa[5*$j+4]\n";  # cross correlation
					$pstext_string[$count_pstext++]="$str $pos_shift 8 0 2 RT $aa[5*$j+6]\n";  # shift second
                                } elsif ($keepBad>0) {
					$pssac_string2[$count_pssac2++]="$nam$com1 $str $pos_y 20/0/255/0\n";      # com1: data:green
 					$pssac_string2[$count_pssac2++]="$nam$com2 $str $pos_y 20/255/0/0\n";      # com2: green: red
					$pstext_string[$count_pstext++]="$str $pos_corr  10 0 0 1 $aa[5*$j+4]\n";  # cross correlation
					$pstext_string[$count_pstext++]="$str $pos_shift  8 0 0 1 $aa[5*$j+6]\n";  # shift second
                                }
                                $offsetx+=$segment[3];  # then SH
                	        $com1-=$step_com;
                        	$com2-=$step_com;
                        	$j+=$step_j;
        	        } # end for
                 }  # end if  end tel


	         else{   # now do local
			$pos_x=0; $offsetx=0;
			$count_loc++;
			$step_j=1; $step_com=2;  # for local , do not skip
			$pos_loc_y--;
			$pos_y=$pos_loc_y;


		$pos_xy_x=$shiftx+$rx;
		$pos_xy_x_1=$shiftx+$rx_1;
		$pos_xy_y=$shifty+$ry;
		$pos_xy_y_1=$shifty+$ry_1;
		$psxy_string[$count_psxy++]="$pos_xy_x $pos_xy_y\n";
		$psxy_string_1[$count_psxy++]="$pos_xy_x_1 $pos_xy_y_1\n";

=cut
                        if ( $count_loc == 1 ){ # print wave type label
                                $pos_tmp_y=$pos_loc_y+1;
                                $pos_tmp_x=$pos_x;
                                for ($j=0;$j<$num_com;){
                                        $pstext_string[$count_pstext++]="$pos_tmp_x $pos_tmp_y    10 0 0 1 $name1[$j]\n";
                                        $j+=$step_j;
                                        $pos_tmp_x+=$segment[$j];
                                }
                        }

=cut
            	for($j=0;$j<$num_com;) {
				$str=$pos_x+$offsetx ; $pos_corr=$pos_y+0.2; $pos_shift=$pos_y-0.2 ;
                                if ($j==0){
                                        $pos_name_x=$str-$t2+$sepa; $pos_name_y=$pos_y;
                                                                    $pos_az_y=$pos_name_y-0.45;
                                                                    $pos_dist_y=$pos_name_y-0.6;

	$intaz = int($stna_az[1]+0.5);
	$intdist = int($stna_dist[1]+0.5);
					$pstext_string[$count_pstext++]="$pos_name_x $pos_az_y   8 0 0 CM $intaz\217 $intdist km\n";     # az
					$pstext_string[$count_pstext++]="$pos_name_x $pos_name_y 10 0 16 CM $nt_na[0] $nt_na[1]\n";
                                }
                                if ($aa[5*$j+2]>0 )  {  # this is the weight
					$pssac_string1[$count_pssac2++]="$nam$com1 $str $pos_y 5/0/0/0\n";        # com1: data:black
 					$pssac_string1[$count_pssac2++]="$nam$com2 $str $pos_y 5/255/0/0\n";      # com2: green: red
					$pstext_string[$count_pstext++]="$str $pos_corr  10 0 0 RT $aa[5*$j+4]\n";  # cross correlation
					$pstext_string[$count_pstext++]="$str $pos_shift 8 0 2 RT $aa[5*$j+6]\n";  # shift second

                              } elsif ($keepBad>0) {
					$pssac_string1[$count_pssac1++]="$nam$com1 $str $pos_y 20/0/255/0\n";      #com1: data:green
 					$pssac_string1[$count_pssac1++]="$nam$com2 $str $pos_y 20/255/0/0\n";      #com2: green: red
					$pstext_string[$count_pstext++]="$str $pos_corr 10 0 0 1 $aa[5*$j+4]\n";  # cross correlation
					$pstext_string[$count_pstext++]="$str $pos_shift 8 0 0 1 $aa[5*$j+6]\n";  # shift second
                                }
                                $offsetx+=$segment[$j];
				$com1-=$step_com;
                        	$com2-=$step_com;
                        	$j+=$step_j;
                	}  #end for
		} #end else  end loc
	} # end foreach

#printf "@pssac_string2\n";
#########################   pssac_tel  ########################################
        if(@pssac_string2){
                open(PLT, $plt1_tel);
                foreach $tmp (@pssac_string2) {
			printf PLT "$tmp";
			#printf "$plt1_tel $tmp";
                }
                close(PLT);
        }

##########################   pssac_loc   ######################################
        if(@pssac_string1){
                open(PLT, $plt1_loc);
                print "$plt1_loc\n";
                foreach $tmp (@pssac_string1) {
                        printf PLT "$tmp";
                        print "AAAA|$tmp\n"
			#printf "$plt1_loc $tmp";
                }
                close(PLT);
        }
###############################################################################
        if(@pstext_string){
                open(PLT, $plt2);
                foreach $tmp (@pstext_string) {
                        printf PLT "$tmp";
                        #printf "$plt2 $tmp";
                }
                close(PLT);
        }
	if(@pstext_string1){  # focal sphere text
                open(PLT, $plt2);
                foreach $tmp (@pstext_string1) {
 #                       printf PLT "$tmp";
                        #printf "$plt2 $tmp";
	                }
                close(PLT);
         }
	if(@pstext_string1_1){
                open(PLT, $plt20);
                foreach $tmp (@pstext_string1_1) {
                        printf PLT "$tmp";
                        #printf "$plt20 $tmp";
	                }
                close(PLT);
         }
############################ psxy  ###############################################
	if(@psxy_string){
		open(PLT, $plt41);
        	foreach $tmp (@psxy_string) {
                	printf PLT "$tmp";
	                #printf "$plt4 $tmp";
       		}
        	close(PLT);
	}

	if(@psxy_string_tel){
		open(PLT, $plt4);
        	foreach $tmp (@psxy_string_tel) {
                	printf PLT "$tmp";
	                #printf "$plt4 $tmp";
       		}
        	close(PLT);
	}
	if(@psxy_string_1){ # points at focal sphere
		open(PLT, $plt40);
        	foreach $tmp (@psxy_string_1) {
                	printf PLT "$tmp";
	                #printf "$plt4 $tmp";
       		}
        	close(PLT);
	}

######################################################################################
 open(PLT,$plt200);
  printf PLT "%f %f 15 0 0 1 Model_depth %s Mw %4.2f  RMS %s\n",$shift_title,$Rn+1.8*$vel_grid,@meca[3,9,11];
  printf "\nModel_depth %s  FM %d/%d/%-3d  Mw %4.2f  RMS %s\n",@meca[3,5,6,7,9,11];
  my @meca_conjugate=`gawk -f ~/bin/to_conjugate.awk $meca[5] $meca[6] $meca[7]`;
  printf "%6.3f  %6.3f  %6.3f\n", @meca[5,6,7];
  printf "@meca_conjugate[0]\n";
  printf "$item_R\n";
  @meca1 = split('\s+',@meca_conjugate[0]);
  printf PLT "%f %f 15 0 0 1  FM1 %d/%d/%d  FM2 %d/%d/%-3d  \n",$shift_title,$Rn+1.5*$vel_grid,@meca[5,6,7],$meca1[0],$meca1[1],$meca1[2];
  printf PLT "%f %f 15 0 0 1 %s \n",$shift_title,$Rn+1.2*$vel_grid,$item_C;
#  printf PLT "%f %f 15 0 0 1 %s %s  %s %s\n",$shift_title,$Rn+0.9*$vel_grid,$item_L,$item_S,$item_P,$item_T;
#  printf PLT "%f %f 15 0 0 1 Weight_of_Pnl: %.1f\n",$shift_title, $Rn+0.6,$wt_of_pnl;

  close(PLT);



#==loc
	system($pltPnl1);
	system($pltPnl2);
	system($pltSF);
	system($pltSF);
	system($pltLV);

#==tel
	system($pltP1);
	system($pltSH);
	system($pltP2);
	system($pltSH);



  printf "\n              total              tel             loc           P           SH\n";
  printf "rms:          $rms_total         $rms_tel        $rms_loc         $rms_P         $rms_SH\n";
  printf "  n:          %d                 %d              %d               %d              %d\n",$n_tel+$n_loc,$n_tel,$n_loc,$n_P,$n_SH;
  if($n_tel==0){

	printf "rms_average:  %f               NAN              %f            NAN         NAN\n\n\n",$rms_total/($n_tel+$n_loc), $rms_loc/$n_loc;

   }elsif($n_loc==0){

  	printf "rms_average:  %f               %f              NAN            %f           %f\n\n\n",$rms_total/($n_tel+$n_loc), $rms_tel/$n_tel,$rms_P/$n_P,$rms_SH/$n_SH;

	$tmp=($rms_SH*$n_P)/($rms_P*$n_SH);
  	printf "        rms_SH    n_P \n";
  	printf "scalar: ------- *  ----- = %f \n",$tmp;
  	printf "        rms_P     n_SH\n\n";
  	printf "weight:P/SH=%f\n",$tmp*$wt_P_SH;

   }else{
  	printf "rms_average:  %f %f %f %f %f\n\n\n",$rms_total/($n_tel+$n_loc), $rms_tel/$n_tel, $rms_loc/$n_loc,$rms_P/$n_P,$rms_SH/$n_SH;
  	$tmp1=($rms_loc*$n_tel)/($rms_tel*$n_loc);
	$tmp2=($rms_SH*$n_P)/($rms_P*$n_SH);
  	printf "        rms_loc    n_tel \n";
  	printf "scalar: ------- *  ----- = %f \n",$tmp1;
  	printf "        rms_tel    n_loc\n\n";

  	printf "        rms_SH    n_P \n";
  	printf "scalar: ------- *  ----- = %f \n",$tmp2;
  	printf "        rms_P     n_SH\n\n";

  	printf "weight: tel/loc = %f\n",$tmp1*$wt_tel_loc;

  	printf "weight:   P/ SH = %f\n",$tmp2*$wt_P_SH;

  }

#########################################################################################################
  }
  printf "\n";
}
