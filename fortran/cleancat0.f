
      parameter(nmax=200000)
      integer*8 i1,ib(nmax),i6,i7,i12
      character c16*5,c17*38,c20*3

      open(unit=1,file='baddet.use',status='old')
      nb=0
      do i=1,nmax
         read(1,*,end=667) i1
         nb=nb+1
         ib(nb)=i1
      enddo
 667  continue
      close(1)

      open(unit=1,file='j1',status='old')
      open(unit=11,file='out',status='unknown')

      n=0
      nt=0
      do i=1,2000000
         read(1,*,end=666) x1,x2,x3,x4,x5,i6,i7,x8,x9,x10,x11,i12,x13,
     $        x14,x15,c16,c17,i18,i19,c20,i21,x22,x23,x24,i25
         nt=nt+1
         do j=1,nb
            if(i7.eq.ib(j)) then
               goto 567
            endif
         enddo
c         write(*,1101) x1,x2,x3,x4,x5,i6,i7,x8,x9,x10,x11,i12,x13,
c     $        x14,x15,c16,c17,i18,i19,c20
         write(11,1101) x1,x2,x3,x4,x5,i6,i7,x8,x9,x10,x11,i12,x13,
     $        x14,x15,c16,c17,i18,i19,c20,i21,x22,x23,x24,i25
         n=n+1
 567     continue
      enddo
 666  continue
      close(1)
      print *,nt,n,nt-n

 1101 format(5(1x,f11.5),1x,i14,1x,i14,3(1x,f9.5),1x,f9.3,1x,i12,
     $     3(1x,f10.5),1x,a5,1x,a38,2(1x,i1),1x,a3,1x,i1,1x,f4.2,
     $     1x,f7.3,1x,f5.3,1x,i1)

      end
