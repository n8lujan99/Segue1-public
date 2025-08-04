
      parameter(nmax=2e6)
      real xfac(nmax),xback(nmax),xscale(nmax),xwoff(nmax)
      real xp(nmax),yp1(nmax),yp2(nmax),yp3(nmax),yp4(nmax)
      character af1(nmax)*18,af2(nmax)*14,aa(4)*3
      character a1*18,a2*14,amp*14,amp0*11

      call pgbegin(0,'?',1,1)
c      call pgpap(0.,1.)
      call pgscf(2)
      call pgsch(1.5)
      call pgslw(2)
      call pgask(.true.)

      open(unit=1,file='all.dat',status='old')
      n=0
      do i=1,nmax
         read(1,*,end=666) a1,a2,x3,x4,x5,x6,x7
         n=n+1
         af1(n)=a1
         af2(n)=a2
         xfac(n)=x3
         xback(n)=x5
         xscale(n)=x6
         xwoff(n)=x7
      enddo
 666  continue
      close(1)
      print *,n

      open(unit=1,file='listin',status='old')
      open(unit=11,file='out',status='unknown')

      aa(1)="_LL"
      aa(2)="_LU"
      aa(3)="_RL"
      aa(4)="_RU"

      do j=1,10000
         read(1,*,end=667) amp0
         do k=1,4
            amp=amp0//aa(k)
            np=0
            do i=1,nmax
               if(af2(i)(1:14).eq.amp) then
                  np=np+1
                  xp(np)=float(np)
                  yp1(np)=xfac(i)
                  yp2(np)=xback(i)
                  yp3(np)=xscale(i)
                  yp4(np)=xwoff(i)
               endif
            enddo
            xmin=xp(1)
            xmax=xp(np)
            print *,k,np
            ymin=0.7
            ymax=1.4
            call pgpage
            call pgsch(0.8)
            call pgvport(0.10,0.49,0.55,0.95)
            call pgwindow(xmin,xmax,ymin,ymax)
            call pgbox('bcst',0.,0,'bcnst',0.,0)
            call pgsch(1.8)
            call pgmtxt('T',0.2,1.03,0.5,amp)
            call pgsch(0.8)
            call pgmtxt('T',-1.5,0.5,0.5,"Normalization")
            call plotpt(np,xp,yp1,ymin,ymax,xb1,xs1)
            ymin=-0.8
            ymax=1.5
            call pgvport(0.51,0.90,0.55,0.95)
            call pgwindow(xmin,xmax,ymin,ymax)
            call pgbox('bcst',0.,0,'bcmst',0.,0)
            call pgsch(0.8)
            call pgmtxt('T',-1.5,0.5,0.5,"Background")
            call plotpt(np,xp,yp2,ymin,ymax,xb2,xs2)
            ymin=0.
            ymax=50.
            call pgvport(0.10,0.49,0.13,0.53)
            call pgwindow(xmin,xmax,ymin,ymax)
            call pgbox('bcnst',0.,0,'bcnst',0.,0)
            call pgmtxt('T',-1.5,0.5,0.5,"Scale")
            call plotpt(np,xp,yp3,ymin,ymax,xb3,xs3)
            ymin=-1.2
            ymax=1.2
            call pgvport(0.51,0.90,0.13,0.53)
            call pgwindow(xmin,xmax,ymin,ymax)
            call pgbox('bcnst',0.,0,'bcmst',0.,0)
            call pgmtxt('T',-1.5,0.5,0.5,"Wave offset")
            call plotpt(np,xp,yp4,ymin,ymax,xb4,xs4)
            write(11,1101) xb1,xs1,xb2,xs2,xb3,xs3,xb4,xs4,amp
         enddo
      enddo
 667  continue
      close(1)

      close(11)
 1101 format(8(1x,f8.2),1x,a14)
      end

      subroutine plotpt(n,x,y,ymin,ymax,xb,xs)
      real x(n),y(n)
      do i=1,n
         yp=y(i)
         call pgsci(1)
         if(yp.lt.ymin) then
            call pgsci(2)
            yp=ymin
         endif
         if(yp.gt.ymax) then
            call pgsci(2)
            yp=ymax
         endif
         call pgpt1(x(i),yp,17)
      enddo
      call pgsci(1)
      call biwgt(y,n,xb,xs)
      return
      end
