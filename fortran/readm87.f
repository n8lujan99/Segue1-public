
      parameter (narrm=70,narrm3=2040,nmax=120000)
      real xd(narrm,narrm,narrm3),x(nmax),y(nmax)
      real xa(nmax,2040),w(nmax),win(10000),xin(10000)
      real xin2(2040,nmax),sky(3000),wsky(3000)
      real xp1(10000),xp2(10000),xp3(10000),xp4(10000)
      real wc(nmax),cs(nmax),cr(nmax),co(nmax)
      real wpost(nmax),post(nmax)
      integer naxes(3),ita(10000)
      character file1*80,cdum*80,af*20,cfile(10000)*20,file2*20
      logical simple,extend,anyf


      w0=20007.0
      w1=2.1335
      nw=2040
      do i=1,nw
         w(i)=w0+float(i-1)*w1
      enddo

      open(unit=1,file='pos.dat',status='old')
      np=0
      do i=1,10000
         read(1,*,end=669) itype,x1,x2,x3,x4,af
         np=np+1
         ita(np)=itype
         xp1(np)=x1
         xp2(np)=x2
         xp3(np)=x3
         xp4(np)=x4
         do j=1,20
            if(af(j:j).eq." ") then
               jc=j-1
               goto 555
            endif
         enddo
 555     continue
         cfile(np)=af(1:jc)//".spec"
      enddo
 669  continue
      close(1)

      open(unit=1,file='sky.dat',status='old')
      nsky=0
      do i=1,10000
         read(1,*,end=777) x1,x2
         nsky=nsky+1
         wsky(nsky)=x1
c         sky(nsky)=x2
         sky(i)=0.
      enddo
 777  continue
      close(1)

      npost=0
      open(unit=1,file='postcor',status='old',err=778)
      do i=1,10000
         read(1,*,end=778) x1,x2
         npost=npost+1
         wpost(npost)=x1
         post(npost)=x2
      enddo
 778  continue
      close(1)

      iext=2

      open(unit=1,file='listin',status='old')

      nt=0
      ntall=0
      do i=1,30
         read(1,*,end=666) file1,x0,y0,xnorm0
         im1=0
         ier=0
         call ftgiou(im1,ier)
         iread=0
         call ftopen(im1,file1,iread,iblock,ier)
         call ftmahd(im1,iext,ihd,ier)
         call ftghpr(im1,3,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
         ncol=naxes(1)
         nrow=naxes(2)
         num=naxes(3)
         call ftg3de(im1,igc,0.,narrm,narrm,ncol,nrow,num,xd,anyf,ier)
         call ftgkye(im1,'CRVAL3',wave0,cdum,ier)
         call ftgkye(im1,'CD3_3',wave1,cdum,ier)
         print *,file1(1:30),wave0,wave1,ntall,ier
         call ftclos(im1,ier)

         file2(6:9)=".cor"
         file2(1:5)=file1(18:22)
         nc=0
         open(unit=2,file=file2,status='old',err=556)
         do ic=1,nmax
            read(2,*,end=556) x1,x2,x3,x4
            nc=nc+1
            wc(nc)=x1
            cs(nc)=x2
            cr(nc)=x3
            co(nc)=x4
         enddo
 556     continue
         close(2)

         sum=0.
         do ix=1,ncol
            do iy=1,nrow
               ntall=ntall+1
               x(ntall)=float(ix)-x0
               y(ntall)=float(iy)-y0
               do iz=1,num
                  win(iz)=wave0+float(iz-1)*wave1
                  xin(iz)=xd(ix,iy,iz)
               enddo
               jin=1
               if(nc.gt.0) then
                  do iz=1,num
                     call xlinint2(win(iz),nc,wc,cs,x1,jin,jout)
                     jin=jout
                     call xlinint2(win(iz),nc,wc,cr,x2,jin,jout)
                     jin=jout
                     call xlinint2(win(iz),npost,wpost,post,x3,jin,jout)
                     jin=jout
                     xin(iz)=(xin(iz)-x1)/x2/x3
c                     xin(iz)=xin(iz)/x2
                  enddo
               endif
               jin=1
               do iz=1,nw
                  call xlinint2(w(iz),num,win,xin,xout,jin,jout)
c                  xa(ntall,iz)=xout/xnorm0-sky(iz)
                  xa(ntall,iz)=xout/xnorm0
                  jin=jout
               enddo
               jin=1
               do iz=1,nw
                  call xlinint2(w(iz),nsky,wsky,sky,xout,jin,jout)
                  xa(ntall,iz)=xa(ntall,iz)-xout
                  jin=jout
               enddo
               rad=sqrt(x(ntall)**2+y(ntall)**2)
               if(rad.lt.5) then
                  do iz=500,800
                     sum=sum+xa(ntall,iz)
                  enddo
               endif
            enddo
         enddo
c         print *,sum
      enddo
 666  continue
      close(1)
      print *,ntall,nw

      do ip=1,np
         if(ita(ip).eq.0) then
            radmin=xp1(ip)
            radmax=xp2(ip)
            nt=0
            sumr=0.
            do i=1,ntall
               x1=x(i)
               y1=y(i)
               rad=sqrt(x1*x1+y1*y1)
               if(rad.ge.radmin.and.rad.lt.radmax) then
                  nt=nt+1
                  do j=1,nw
                     xin2(j,nt)=xa(i,j)
                  enddo
                  sumr=sumr+rad
               endif
            enddo
         else
            nt=0
            sumr=0.
            do i=1,ntall
               x1=x(i)
               y1=y(i)
               if(x1.ge.xp1(ip).and.x1.lt.xp2(ip).and.
     $              y1.ge.xp3(ip).and.y1.lt.xp4(ip)) then
                  nt=nt+1
                  do j=1,nw
                     xin2(j,nt)=xa(i,j)
                  enddo
                  sumr=sumr+sqrt(x1*x1+y1*y1)
               endif
            enddo
         endif
         if(nt.gt.0.) sumr=sumr/float(nt)
         print *,nt

         open(unit=11,file=cfile(ip),status='unknown')
         do j=1,nw
            do i=1,nt
               xin(i)=xin2(j,i)
            enddo
            call biwgt(xin,nt,xb,xs)
            write(11,*) w(j),xb,xs,nt,sumr
         enddo
         close(11)
      enddo

 706  continue
      end

      subroutine xlinint2(xp,n,x,y,yp,jin,jout)
      real x(n),y(n)
      do j=jin,n-1
         if(xp.ge.x(j).and.xp.le.x(j+1)) then
            yp=y(j)+(y(j+1)-y(j))*(xp-x(j))/(x(j+1)-x(j))
            jout=j
            return
         endif
      enddo
      if(xp.lt.x(1)) yp=y(1)
      if(xp.gt.x(n)) yp=y(n)
      jout=1
      return
      end
