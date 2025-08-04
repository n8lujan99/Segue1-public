
      parameter(nmax=300000)
      real x(nmax),y(nmax),sig(nmax),xsig(nmax),ysig(nmax)
      real xin(nmax),yin(nmax),xl(10000),yl(10000)
      real xout(nmax),yout(nmax)
      integer ic(nmax)
      character c1*40,c2*40

      nbin=17

      ifit=0
      call qc1('Input datafile ','plotxy.def',c2)
      open(unit=1,file=c2,status='old')
      
      ilog=0
      call qr2('Xmin and Xmax ','plotxy.def',xmin,xmax)
      call qr2('Ymin and Ymax ','plotxy.def',ymin,ymax)
      call savdef
      
      call pgbegin(0,'?',1,1)
      call pgpap(0.,1.)
      call pgscf(2)
      call pgsch(1.5)
      call pgslw(2)

      do i=1,nmax
         read(1,*,end=666) x1,x2,i3
         n=n+1
         x(n)=x1
         y(n)=x2
         ic(n)=i3
      enddo
 666  continue
      close(1)

      ybit=(ymax-ymin)/10.
      call pgenv(xmin,xmax,ymin,ymax,0,0)
      call pgsls(1)
      call pgsch(1.2)

      do j=1,10
         nin=0
         do i=1,n
            if(ic(i).eq.j) then
               nin=nin+1
               xin(nin)=x(i)
               yin(nin)=y(i)
               call pgsci(ic(i))
               xp=x(i)
c               call pgpt1(xp,y(i),17)
            endif
         enddo
         if(nin.gt.5) then
            call getline(nbin,xmin,xmax,nin,xin,yin,nout,xout,yout)
            call pgsci(j)
            call pgslw(5)
            call pgline(nout,xout,yout)
            call pgslw(2)
         endif
      enddo

      call pgsci(1)

      call pgsch(1.5)
      open(unit=11,file='labels.dat',status='old',err=866)
      read(11,*,err=866,end=866) c1
      call pgmtxt('B',2.5,0.5,0.5,c1)
      read(11,*,err=866,end=866) c1
      call pgmtxt('L',2.0,0.5,0.5,c1)
      read(11,*,err=866,end=866) c1
      call pgmtxt('T',1.5,0.5,0.5,c1)
 866  close(11)

      call pgend
      end

      subroutine getline(nbin,xmin,xmax,nin,xin,yin,nout,xout,yout)
      parameter(nmax=300000)
      real xin(nmax),yin(nmax),xout(nmax),yout(nmax)
      real y(nmax)

      xdiff0=1./float(nbin-1)*(xmax-xmin)-xmin
      nout=0
      ymino=1e10
      do i=1,nbin
         xp=xmin+(xmax-xmin)*float(i-1)/float(nbin-1)
         n=0
         if(i.le.1) then
            xdiff=xdiff0/5.
         else
            xdiff=xdiff0
         endif
         do j=1,nin
            if(abs(xp-xin(j)).lt.xdiff) then
               n=n+1
               y(n)=yin(j)
            endif
         enddo
         if(n.gt.0) then
            call biwgt(y,n,xb,xs)
            nout=nout+1
            xout(nout)=xp
            yout(nout)=xb
            if(xb.lt.ymino) then
               ymino=xb
               xmino=xp
            endif
         endif
      enddo
      print *,xmino,ymino

      return
      end
