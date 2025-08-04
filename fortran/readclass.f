
      parameter (narrm2=900000)
      real xa(16384)
      real*8 dx,dxa(16384)
      integer*8 idd
      character file1*180,ttype(16)*10,class*6
c      character nullstr*1,name*8,cname(narrm2)*18
      character nullstr*1,name*8,cname(narrm2)*24,cspec*15
      logical simple,extend,anyf

      file1="in.fits"
      iext=2

      im1=51
      ier=0
      iread=0
      call ftopen(im1,file1,iread,iblock,ier)

      call ftmahd(im1,iext,ihd,ier)
c      call ftgkns(im1,'TTYPE',1,16,ttype,nfound,ier)
c      print *,ttype,ier
      open(unit=11,file='out',status='unknown')
      i=2
      call ftgcvj(im1,3,i,1,1,0.,ix,anyf,ier)
      call ftgcvd(im1,4,i,1,16384,0.,dxa,anyf,ier)
      call ftgcve(im1,5,i,1,16384,0.,xa,anyf,ier)
      do i=1,16384
c         print *,i,dxa(i),xa(i)
         write(11,*) dxa(i),xa(i)
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
