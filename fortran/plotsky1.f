
      parameter(nmax=10000)
      real x(nmax),y(nmax),z(nmax)
      character file1*80,file2*80

      call pgbegin(0,'?',1,1)
      call pgpap(0.,1.)
      call pgsch(1.5)
      call pgscf(2)
      call pgsls(1)
      call pgslw(2)

      xmin=3500.
      xmax=5500.
      ymin=-50.
      ymax=420.

      open(unit=1,file='list',status='old')

      ia=0
      ns1=0
      do il=1,1000
         read(1,*,end=666) file1
         open(unit=2,file=file1,status='old')
         if(il.eq.1) then
            call pgsci(1)
            call pgenv(xmin,xmax,ymin,ymax,0,0)
            call pglabel('Wavelength (\(2078))','Counts','')
         endif
         n=0
         do i=1,10000
            read(2,*,end=668) x1,x2,x3
            n=n+1
            x(n)=x1
            y(n)=x2
            z(n)=x3
         enddo
 668     continue
      enddo
 666  continue
      close(1)

      call pgsci(1)
      call pgslw(6)
      call pgline(n,x,y)
      call pgsci(2)
      call pgslw(2)
      call pgline(n,x,z)

      call pgend

      end

      subroutine xlinint(xp,n,x,y,yp)
      real x(n),y(n)
      do j=1,n-1
         if(xp.ge.x(j).and.xp.lt.x(j+1)) then
            yp=y(j)+(y(j+1)-y(j))*(xp-x(j))/(x(j+1)-x(j))
            return
         endif
      enddo
      if(xp.lt.x(1)) yp=y(1)
      if(xp.gt.x(n)) yp=y(n)
      return
      end
