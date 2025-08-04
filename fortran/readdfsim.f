
      parameter (narrm1=2000,narrm2=100000)
      real*8 dx1,dx2,dx3
      character file1*180,ttype(3)*10,class*6
      character nullstr*1,name*8,cname(narrm2)*24,cspec*15
      logical simple,extend,anyf

      file1="ouchi_lf_test_flim_1e-17.fits"

      im1=0
      ier=0
      iread=0
      call ftgiou(im1,ier)
      call ftopen(im1,file1,iread,iblock,ier)

      call ftmahd(im1,2,ihd,ier)
c      call ftgkns(im1,'TTYPE',1,3,ttype,nfound,ier)
      open(unit=11,file='out',status='unknown')
      do i=1,35232172
         call ftgcvd(im1,1,i,1,1,0.,dx1,anyf,ier)
         call ftgcvd(im1,2,i,1,1,0.,dx2,anyf,ier)
         call ftgcvd(im1,3,i,1,1,0.,dx3,anyf,ier)
         write(11,*) i,real(dx1),real(dx2/1.d42),real(dx3)/1.e-17
      enddo
      close(11)

      call ftclos(im1,ier)

 706  continue
 1001 format(i1)
 1002 format(i2)
 1003 format(i3)
 1004 format(i4)
 1005 format(i5)
 1101 format(i8,3(1x,f10.5),1x,a5,1x,a4,1x,a15)
      end
