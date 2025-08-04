
      parameter(nmax=10000)
      real x(nmax),yo(nmax),ya(nmax)
      real yoh(nmax),yol(nmax)
      real yah(nmax),yal(nmax)

      call pgbegin(0,'?',1,1)
      call pgpap(0.,1.)
      call pgsch(1.5)
      call pgscf(2)
      call pgslw(3)

      xmin=log10(0.3)
      xmax=log10(1150.)
      ymin=log10(3.8)
      ymax=log10(410.)
      call pgsls(1)
      call pgslw(2)
      call pgsci(1)
      call pgsch(1.5)
      call pgenv(xmin,xmax,ymin,ymax,0,30)
      call pglabel('R["]','stars/"\U2','')

      open(unit=1,file='fritz.txt',status='old')
      read(1,*)

      n=0
      do il=1,10000
         read(1,*,end=667) x1,x2,x3
         n=n+1
         x(n)=log10(x1)
         yo(n)=log10(x2)
         if(x1.lt.1) ye=0.18
         if(x1.gt.1) ye=0.10
         if(x1.gt.2) ye=0.08
         if(x1.gt.25) ye=0.03
         yoh(n)=yo(n)+ye
         yol(n)=yo(n)-ye
         ya(n)=log10(x3)
         yah(n)=ya(n)+ye
         yal(n)=ya(n)-ye
         write(*,1001) x(n),yo(n),ya(n),ye
      enddo
 667  continue
      close(1)
      call pgsci(4)
      call pgline(n,x,ya)
      call pgerry(n,x,yah,yal,1.)
      call pgsci(2)
      call pgline(n,x,yo)
      call pgerry(n,x,yoh,yol,1.)

      call pgend
 1001 format(4(1x,f8.3))
      end
