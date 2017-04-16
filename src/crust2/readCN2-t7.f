c layer one and two are flipped now
      implicit real*8(a-h,o-z)

      parameter(ntyp=360)
      parameter(nla=90,nlo=180)
      dimension amapt(nlo),amapth(nlo),amaps(nlo),amapi(nlo)
      dimension amapl(6,nlo),flev(ntyp,7),flev2(7)
      dimension elev(nlo,nla),ilon(nlo),il(nla)

      character*2 ctype(ntyp),dum*1,dum0*5,line*506
      character*2 types(nlo)


c... read in elevation map
c......................... 
      open(3,file='CNelevatio2.txt')
      open(19,file='map2.topo')
      read(3,*)ilon
      do 200 j=1,nla
         read(3,*)il(j),(elev(i,j),i=1,nlo)
         write(19,902)(elev(i,j)/1000.,i=nla+1,nlo),
     +   (elev(i,j)/1000.,i=1,nla)
 200  continue
      close(3)
      close(19)

      open(2,file='CNtype2_key.txt')
      open(7,file='CNtype2.txt')
      open(8,file='map2.t7')
      open(16,file='map2.t0')
      open(9,file='map2.t1')
      open(10,file='map2.t2')
      open(11,file='map2.t3')
      open(12,file='map2.t4')
      open(13,file='map2.t5')
      open(14,file='map2.t6')
      open(15,file='map2.thick')
      open(17,file='map2.sed')
      open(18,file='map2.ice')

c... read in key for crust types
c...............................
      read(2,890)dum
      do 101 i=1,ntyp
         read(2,899)ctype(i)
         print 899,ctype(i)
         read(2,891)dum
         read(2,899)line
         read(line,*)(flev(i,l),l=1,7),dum0,fthick
c. switch layer 1 and 2
c. new layer 1 is water
c. new layer 2 is ice
         aux=flev(i,2)
         flev(i,2)=flev(i,1)
         flev(i,1)=aux
         felev=0.
         do 102 l=1,7
 102     felev=felev+flev(i,l)
         if(felev.ne.fthick)then
           print*,' read error in thickness of crust'
           print 905,' type: ',i,ctype(i)
           print*,felev,fthick 
           stop
         endif
 101  continue
      print*,'reading types completed'
      close(2)

c... read CNtype file
c...............................
      read(7,*)flons
      do 40 j=1,nla
         read(7,901)ilat,types
         print*,ilat
         do 10 i=1,nlo
            do 20 l=1,ntyp
            if(types(i).eq.ctype(l))then
              do 104 m=1,7
 104          flev2(m)=flev(l,m)
              amaps(i)=flev2(3)+flev2(4)
              amapi(i)=flev2(2)
              if(elev(i,j).lt.0.)then
                 flev2(1)=-elev(i,j)/1000.
                 elev(i,j)=0.      
              endif
              do 103 k=2,7
 103          flev2(k)=flev2(k)+flev2(k-1)
              amapt(i)=elev(i,j)/1000.-flev2(7) 
              amapth(i)=flev2(7) 
c uncomment next line if thickness without water is desired
              if(elev(i,j).eq.0.)amapth(i)=amapth(i)-flev2(1)
              do 30 k=1,6
 30           amapl(k,i)=elev(i,j)/1000.-flev2(k)
              goto 10
            endif
 20         continue
            print*,' crust type code not found: ',types(i)
            print*,' latitude: ',ilat,' long index: ',i
 10      continue
         write(8,902)(amapt(l),l=nla+1,nlo),(amapt(l),l=1,nla)
         write(15,902)
     +        (amapth(l),l=nla+1,nlo),(amapth(l),l=1,nla)
         write(16,902)(elev(l,j)/1000.,l=nla+1,nlo)
     +        ,(elev(l,j)/1000.,l=1,nla)
         write(17,902)
     +        (amaps(l),l=nla+1,nlo),(amaps(l),l=1,nla)
         write(18,902)
     +        (amapi(l),l=nla+1,nlo),(amapi(l),l=1,nla)
         do 50 k=1,6
 50      write(8+k,902)
     +        (amapl(k,l),l=nla+1,nlo),(amapl(k,l),l=1,nla)
 40   continue

 890  format(////a)
 891  format(//a)
 899  format(a)
 901  format(i4,1x,180(2x,a2,1x))
 902  format(30(8e15.5/))
 905  format(a,i6,1x,a2)
 99   continue
      close(7)
      end
