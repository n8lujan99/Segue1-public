
      parameter (narrm1=2000,narrm2=100000)
      character file1*180,ttype(3)*10,class*6
c      character nullstr*1,name*8,cname(narrm2)*18
      character nullstr*1,name*8,c1*20
      logical simple,extend,anyf

      file1="ocooper_ssa22.fits"

      im1=0
      ier=0
      iread=0
      call ftgiou(im1,ier)
      call ftopen(im1,file1,iread,iblock,ier)

      call ftmahd(im1,2,ihd,ier)
c      call ftgkns(im1,'TTYPE',1,3,ttype,nfound,ier)
      open(unit=11,file='out',status='unknown')
      do i=1,4000
c         call ftgcvs(im1,1,i,1,1,nullstr,c1,anyf,ier)
         call ftgcvj(im1,1,i,1,1,0.,j1,anyf,ier)
         call ftgcve(im1,14,i,1,1,0.,wave,anyf,ier)
         call ftgcve(im1,46,i,1,1,0.,p1,anyf,ier)
         call ftgcve(im1,50,i,1,1,0.,p2,anyf,ier)
c         call ftgcve(im1,2,i,1,1,0.,ra,anyf,ier)
c         call ftgcve(im1,3,i,1,1,0.,dec,anyf,ier)
c         call ftgcve(im1,8,i,1,1,0.,flux,anyf,ier)
         if(ier.ne.0) goto 706
         print *,j1,wave,p2
         write(11,*) j1,wave,p2
      enddo
      close(11)
 706  continue

      call ftclos(im1,ier)

 1001 format(i1)
 1002 format(i2)
 1003 format(i3)
 1004 format(i4)
 1005 format(i5)
 1101 format(i8,3(1x,f10.5),1x,a5,1x,a4,1x,a15)
      end
