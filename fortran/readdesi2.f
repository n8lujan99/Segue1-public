
      parameter (narrm1=2000,narrm2=100000)
      integer*8 idd
      character file1*180,ttype(3)*10,class*6
c      character nullstr*1,name*8,cname(narrm2)*18
      character nullstr*1,name*8,cname(narrm2)*24,cspec*15
      logical simple,extend,anyf

      file1="desi-hetdex.fits"

      im1=51
      ier=0
      iread=0
      call ftopen(im1,file1,iread,iblock,ier)

      call ftmahd(im1,2,ihd,ier)
c      call ftgkns(im1,'TTYPE',1,3,ttype,nfound,ier)
      open(unit=11,file='out',status='unknown')
      do i=1,1959
c         call ftgcve(im1,7,i,1,1,0.,ra,anyf,ier)
c         call ftgcve(im1,8,i,1,1,0.,dec,anyf,ier)
         call ftgcvk(im1,8,i,1,1,0.,idd,anyf,ier)
         call ftgcve(im1,19,i,1,1,0.,zdex,anyf,ier)
         call ftgcve(im1,114,i,1,1,0.,zdesi,anyf,ier)
         call ftgcve(im1,14,i,1,1,0.,sn,anyf,ier)
         call ftgcvj(im1,14,i,1,1,0.,ifib,anyf,ier)
         call ftgcvj(im1,28,i,1,1,0.,ifibstat,anyf,ier)
         call ftgcve(im1,87,i,1,1,0.,dx,anyf,ier)
         call ftgcve(im1,89,i,1,1,0.,dy,anyf,ier)
         del=sqrt(dx*dx+dy*dy)
         write(11,*) idd,zdex,zdesi,sn,ifib,ifibstat,del
c         print *,idd,zdex,zdesi,ier
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
