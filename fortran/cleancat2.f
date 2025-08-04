
      parameter(nmax=2000000)
      real xp(nmax)
      integer idl(nmax),idh(nmax)
      integer*8 i1,ib(nmax),id6,id7,ishot,ip(nmax)
      character cdet*10,c1*14,ca(nmax)*14,cdum*38

      open(unit=1,file='baddet.use',status='old')
      nb=0
      do i=1,nmax
         read(1,*,end=667) i1
         nb=nb+1
         ib(nb)=i1
      enddo
 667  continue
      close(1)

      open(unit=1,file='badamp.use',status='old')
      na=0
      do i=1,nmax
         read(1,*,end=668) c1,i2,i3
         na=na+1
         ca(na)=c1
         idl(na)=i2
         idh(na)=i3
      enddo
 668  continue
      close(1)

      open(unit=1,file='j1',status='old')
      open(unit=11,file='outj',status='unknown')

      n=0
      n1=0
      n2=0
      do i=1,1000000
         read(1,*,end=666) x1,x2,x3,x4,x5,id6,
     $        id7,x14,ra,dec,wave,ishot,xlw,fobs
     $        ,xdum,cdet,cdum
         read(cdum(1:8),*) idate
         do j=1,nb
            if(id7.eq.ib(j)) then
               n1=n1+1
               goto 888
            endif
         enddo
         do j=1,na
            if(cdum(21:34).eq.ca(j).and.
     $           idate.ge.idl(j).and.idate.le.idh(j)) then
               n2=n2+1
               goto 888
            endif
         enddo
         write(11,1101) x1,x2,x3,x4,x5,id6,
     $        id7,x14,ra,dec,wave,xlw,fobs
 888     continue
      enddo
 666  continue
      close(1)
      close(11)

      print *,n1,n2

 1101 format(5(1x,f11.4),1x,i14,1x,i14,1(1x,f8.3),2(1x,f9.5),
     $     2(1x,f7.2),1x,f11.4)

      end
