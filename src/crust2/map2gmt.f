      parameter(nlo=180,nla=90)
      dimension amap(nlo,nla)
      character*80 name
      real*4 lon,lat

      dx=360./nlo
 1    print*,'enter map file name'
      read(*,'(a)')name
      open(8,file=name,status='old',iostat=iret)
      if(iret.ne.0)goto 1
      ic=ilen(name)
      write(name(ic+1:ic+4),'(a)')'.gmt'
      open(9,file=name)
      read(8,*)((amap(i,j),i=1,nlo),j=1,nla)
      do 20 j=1,nla
         lat=90.-((j-0.5)*dx)
         do 20 i=1,nlo
            lon=(i-0.5)*dx        
            write(9,*)lon,lat,amap(i,j)
 20   continue 
      end  

      function ilen(name)
      character*(*) name

      ilen=0
 1    continue
      il=ilen+1
      if(name(il:il).eq.' ')goto 99
      ilen=ilen+1
      goto 1
 99   return
      end
      
