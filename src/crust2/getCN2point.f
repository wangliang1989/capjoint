c layer one and two flipped, after the read statement!
c layer 1: water
c layer 2: ice

      parameter(ityp=360)
      parameter(nla=90,nlo=180)

      dimension fvel(ityp,8),fvels(ityp,8),frho(ityp,8)
      dimension fthi(ityp,7)
      dimension amapvp(8,nlo,nla),amaprho(8,nlo,nla),
     +          amapvs(8,nlo,nla),amapthi(7,nlo,nla),
     +          amapele(nlo,nla)
      character*2 ctype(ityp),line*506,dum*1,dum0*5
      character*2 types(nlo),nsta*4,ntyp*2,atype(nlo,nla)
      character*12 names(7)
      data names/'water','ice','soft sed.','hard sed.',
     +         'upper crust','middle crust','lower crust'/
      open(2,file='CNtype2_key.txt')
      open(7,file='CNtype2.txt')
      open(8,file='CNelevatio2.txt')

      dx=360/nlo
c... read in key for crust types
c...............................
      read(2,890)dum
      print*,' ... reading key file ...'
      do 101 i=1,ityp
         read(2,899)ctype(i)
c        print 899,ctype(i)
         read(2,899)line
         read(line,*)(fvel(i,l),l=1,8)
         read(2,899)line
         read(line,*)(fvels(i,l),l=1,8)
         read(2,899)line
         read(line,*)(frho(i,l),l=1,8)
         read(2,899)line
         read(line,*)(fthi(i,l),l=1,7)
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
         aux=fthi(i,1)
         fthi(i,1)=fthi(i,2)
         fthi(i,2)=aux
 101  continue

c... read CNtype file
c...............................
      read(7,*)flons
      print*,' ... reading model ...'
      read(8,899)line
      do 40 j=1,nla
         read(8,*)ilat,(amapele(i,j),i=1,nlo)
         read(7,901)ilat,types
         do 10 i=1,nlo
            do 20 l=1,ityp
            if(types(i).eq.ctype(l))then
              atype(i,j)=ctype(l)
              do 30 k=1,8
              amapvp(k,i,j)=fvel(l,k)
              amapvs(k,i,j)=fvels(l,k)
              amaprho(k,i,j)=frho(l,k)
 30           continue
              do 31 k=1,7
 31           amapthi(k,i,j)=fthi(l,k)
              goto 10
            endif
 20         continue
            print*,' crust type code not found: ',types(i)
            print*,' latitude: ',ilat,' long index: ',i
 10      continue
 40   continue

*-------------------
c     now look up coordinates
     
      open(66,file='outcr') 
      print*,' the output file is outcr'
      print*,' '
 60   continue
      print*,' enter lat, lon  (* quits)'
      read(*,'(a)')line
      if(line(1:1).eq.' '.or.line(1:1).eq.'*')goto 99
      read(line,*,err=99)flat,flon
      cola=90.-flat
      if(flon.gt.180.)flon=flon-360.
      ilat=int(cola/dx)+1
      ilon=int((flon+180.)/dx)+1
      print 999,ilat,ilon,atype(ilon,ilat)
 999  format(2i5,1x,a2)
      vthi=0.
      vvp=0.
      vvs=0.
      vrho=0.
      do 50 i=2,7
         vthi=vthi+amapthi(i,ilon,ilat)
         vvp=vvp+amapthi(i,ilon,ilat)/amapvp(i,ilon,ilat)
         vvs=vvs+amapthi(i,ilon,ilat)/amapvs(i,ilon,ilat)
         vrho=vrho+amapthi(i,ilon,ilat)*amaprho(i,ilon,ilat)
 50   continue
      vvp=vthi/vvp
      vvs=vthi/vvs
      vrho=vrho/vthi
      write(66,793)'type, latitude, longitude, elevation: ',
     +     atype(ilon,ilat),flat,flon,amapele(ilon,ilat)
      write(66,794)'crustal thickness, ave. vp, vs, rho:  ',
     +     '  ',vthi,vvp,vvs,vrho                                
      write(66,793)'Mantle below Moho: ave. vp, vs, rho:  ',
     +     '  ',amapvp(8,ilon,ilat),amapvs(8,ilon,ilat),
     +     amaprho(8,ilon,ilat)
 793  format(a,2x,a2,2x,11x,4f11.4)
 794  format(a,2x,a2,2x,4f11.4)
      write(66,*)' ' 
      write(66,'(a)')' 7-layer crustal model (thickness, vp,vs,rho)'
      if(amapele(ilon,ilat).lt.0..and.amapthi(1,ilon,ilat).ne.0)   
     +  amapthi(1,ilon,ilat)=-amapele(ilon,ilat)/1000.
      do 70 i=1,7
         write(66,792)amapthi(i,ilon,ilat),amapvp(i,ilon,ilat),
     +        amapvs(i,ilon,ilat),amaprho(i,ilon,ilat),names(i)
 70   continue
 791  format(a2,2x,2f11.4,7f6.2)
 792  format(4f10.4,2x,a12)
      goto 60

 890  format(////a)
 899  format(a)
 901  format(i4,1x,180(2x,a2,1x))

 99   continue
      close(7)
      close(66)

      end
