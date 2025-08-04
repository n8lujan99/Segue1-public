
      parameter(nmax=1000000)
      real x1(nmax),x2(nmax),x3(nmax),x4(nmax),x5(nmax),x8(nmax)
      real f(nmax),x9(nmax),x10(nmax),x11(nmax),x12(nmax),x13(nmax)
      real x14(nmax),ra(nmax),dec(nmax),wave(nmax),wc(nmax),xlw(nmax)
      real fobs(nmax),zspec(nmax)
      integer icheck(nmax),ica(nmax)
      integer*8 id6(nmax),id7(nmax),id(nmax),ishot(nmax)
      character cdet(nmax)*10

      xlya=1215.67

c - ish=0 is unique source, =1 is keep individual repeats
      ish=1

      open(unit=1,file='j1',status='old')

      n=0
      do i=1,nmax
         read(1,*,end=666) x1(i),x2(i),x3(i),x4(i),x5(i),id6(i),
c     $        id7(i),x8(i),x9(i),x10(i),x11(i),x12(i),x13(i),x14(i)
     $        id7(i),x14(i),ra(i),dec(i),wave(i),ishot(i),xlw(i),fobs(i)
     $        ,xdum,cdet(i)
c     $        ,zspec(i)
         icheck(i)=0
         n=n+1
c         x14(i)=x14(i)*1.e17
      enddo
 666  continue
      close(1)
      print *,n

      do i=1,n-1
         nc=1
         id(nc)=id7(i)
         f(nc)=x1(i)
         wc(nc)=wave(i)
         ica(nc)=i
         if(icheck(i).eq.0) then
            if(ish.eq.0) then
               do j=i+1,n
                  if(id6(i).eq.id6(j)) then
                     nc=nc+1
                     id(nc)=id7(j)
                     f(nc)=x1(j)
                     wc(nc)=wave(j)
                     ica(nc)=j
                  endif
               enddo
            else
               do j=i+1,n
                  if(id6(i).eq.id6(j).and.ishot(i).eq.ishot(j)) then
                     nc=nc+1
                     id(nc)=id7(j)
                     f(nc)=x1(j)
                     wc(nc)=wave(j)
                     ica(nc)=j
                  endif
               enddo
            endif
            if(nc.gt.1) then
               wavec=(1.+x4(i))*xlya
               if(nc.gt.3) print *,i,id6(i),nc,wavec
               fmax=-1e10
               do ic=1,nc
                  icheck(ica(ic))=1
                  wdiff=abs(wc(ic)-wavec)
                  if(nc.gt.3) print *,ic,id(ic),f(ic),wdiff,wc(ic),wavec
                  if(f(ic).gt.fmax.and.wdiff.lt.10.) then
                     fmax=f(ic)
                     imax=ica(ic)
                  endif
               enddo
               icheck(imax)=0
            endif
         endif
      enddo

      open(unit=11,file='outj',status='unknown')
      open(unit=12,file='outj2',status='unknown')
      nr=0
      do i=1,n
         if(icheck(i).eq.0) then
            write(11,1101) x1(i),x2(i),x3(i),x4(i),x5(i),id6(i),
     $           id7(i),x14(i),ra(i),dec(i),wave(i),xlw(i),fobs(i)
     $           ,cdet(i)
         else
            write(12,1101) x1(i),x2(i),x3(i),x4(i),x5(i),id6(i),
     $           id7(i),x14(i),ra(i),dec(i),wave(i),xlw(i),fobs(i)
     $           ,cdet(i)
         endif
      enddo
      close(11)
      close(12)

 1101 format(5(1x,f11.4),1x,i14,1x,i14,1(1x,f8.3),2(1x,f9.5),
     $     2(1x,f7.2),1x,f11.4,1x,a6)

      end
