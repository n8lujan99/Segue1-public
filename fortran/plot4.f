      
      parameter(nmax=10000)
      real xml(nmax),xbh(nmax),xvd(nmax),xrd(nmax)
      real xch(nmax),xal(nmax)

      open(unit=1,file='plimits.dat',status='old')
      read(1,*) cmin,cmax
      read(1,*) xmmin,xmmax
      read(1,*) bmin,bmax
      read(1,*) vmin,vmax
      read(1,*) rmin,rmax
      close(1)

      open(unit=1,file='chi.dat',status='old')

      n=0
      do i=1,nmax
         read(1,*,end=666) x1,x2,x3,x4,x5,x6
         n=n+1
         xml(n)=x1
         xbh(n)=x2
         xvd(n)=x3
         xrd(n)=x4
         xch(n)=x5
         xal(n)=x6
      enddo
 666  continue
      close(1)

      call pgbegin(0,'?',2,2)
      call pgpap(0.,1.)
      call pgscf(2)
      call pgsch(1.5)
      call pgslw(2)

      call plotxy(n,xml,xch,xmmin,xmmax,cmin,cmax,0)
      call plotxy(n,xbh,xch,bmin,bmax,cmin,cmax,0)
      call plotxy(n,xvd,xch,vmin,vmax,cmin,cmax,0)
      call plotxy(n,xrd,xch,rmin,rmax,cmin,cmax,0)

      call pgend

      end

      subroutine plotxy(n,x,y,xmin,xmax,ymin,ymax,ilog)
      parameter(nmax=10000)
      real x(nmax),y(nmax)

      call pgenv(xmin,xmax,ymin,ymax,ilog,0)

      do i=1,n
         call pgpt1(x(i),y(i),17)
      enddo

      return
      end
