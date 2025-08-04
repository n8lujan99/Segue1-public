
      parameter (narrm=10000)
      real xd(narrm,narrm),xin(narrm*narrm)
      integer naxes(2)
      character file1*40
      logical simple,extend,anyf

      open(unit=1,file='listin',status='old')
      do i=1,10000
         read(1,*,end=777) file1
         
      iext=1

      im1=0
      ier=0
      call ftgiou(im1,ier)
      iread=0
      call ftopen(im1,file1,iread,iblock,ier)
      if(ier.ne.0) then
         write(*,*) 'Error opening image : ',file1
         goto 706
      endif
      call ftmahd(im1,iext,ihd,ier)
      naxis=2
      call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
      if(naxis.eq.1) naxes(2)=1
      if(naxes(1).gt.narrm.or.naxes(2).gt.narrm) then
         write(*,"('Arrays too small - make narrm bigger')")
         goto 706
      endif
      ncol=naxes(1)
      nrow=naxes(2)
      call ftg2de(im1,igc,0.,narrm,ncol,nrow,xd,anyf,ier)
      call ftclos(im1,ier)

      icen=550
      x1=xd(icen,1)
      do j=2,112
         xdiff=xd(icen,j)-x1
         x1=xd(icen,j)
         if(x1.gt.0.) then
            if(abs(xdiff).lt.5.) write(*,1001) file1(1:25),j,xdiff,x1
         endif
      enddo
      enddo
 777  continue
      close(1)

 1001 format(a25,1x,i3,1x,f5.2,1x,f7.2)
 706  continue
      end
