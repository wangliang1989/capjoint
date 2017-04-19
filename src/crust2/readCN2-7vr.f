c layer one and two flipped, after the read statement!
c layer 1: water
c layer 2: ice

      parameter(ityp=360)
      parameter(nla=90,nlo=180)

      dimension fvel(ityp,8),fvels(ityp,8),frho(ityp,8)
      dimension amapvp(8,nlo,nla),amaprho(8,nlo,nla),
     +          amapvs(8,nlo,nla)
      
      character*2 ctype(ityp),dum*1,line*506
      character*2 types(nlo)

      open(2,file='CNtype2_key.txt')
      open(7,file='CNtype2.txt')

c... read in key for crust types
c...............................
      read(2,890)dum
      do 101 i=1,ityp
         read(2,899)ctype(i)
         print 899,ctype(i)
         read(2,899)line
         read(line,*)(fvel(i,l),l=1,8)
         read(2,899)line
         read(line,*)(fvels(i,l),l=1,8)
         read(2,899)line
         read(line,*)(frho(i,l),l=1,8)
         read(2,899)dum
c flip layers
         aux=fvel(i,1)
         fvel(i,1)=fvel(i,2)
         fvel(i,2)=aux
         aux=fvels(i,1)
         fvels(i,1)=fvels(i,2)
         fvels(i,2)=aux
         aux=frho(i,1)
         frho(i,1)=frho(i,2)
         frho(i,2)=aux
 101  continue

c... read CNtype file
c...............................
      read(7,899)line
      read(line,*)flons
      do 40 j=1,nla
         read(7,901)ilat,types
         print*,ilat
         do 10 i=1,nlo
            do 20 l=1,ityp
            if(types(i).eq.ctype(l))then
              do 31 k=1,8
              amapvp(k,i,j)=fvel(l,k)
              amapvs(k,i,j)=fvels(l,k)
              amaprho(k,i,j)=frho(l,k)
 31           continue
              goto 10
            endif
 20         continue
            print*,' crust type code not found: ',types(i)
            print*,' latitude: ',ilat,' long index: ',i
 10      continue
 40   continue
      do 50 k=1,8
         write(dum,'(i1)')k
         open(8+k,file='map2.vp'//dum)
         open(18+k,file='map2.vs'//dum)
         open(28+k,file='map2.rho'//dum)
         do 60 i=1,nla
         write(8+k,902)(amapvp(k,l,i),l=nla+1,nlo),
     +                 (amapvp(k,l,i),l=1,nla)
         write(18+k,902)(amapvs(k,l,i),l=nla+1,nlo),
     +                 (amapvs(k,l,i),l=1,nla)
         write(28+k,902)(amaprho(k,l,i),l=nla+1,nlo),
     +                 (amaprho(k,l,i),l=1,nla)
 60      continue
         close(8+k)
         close(18+k)
         close(28+k)
 50   continue

 890  format(////a)
 899  format(a)
 901  format(i4,1x,180(2x,a2,1x))
 902  format(30(8e15.5/))
 99   continue
      close(7)
      close(2)
      end
