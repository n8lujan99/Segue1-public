
      parameter (narrm=2000)
      integer naxes(2)
      character file1*40,cspec*100,campname*2
      logical simple,extend,anyf

      read *,file1
      read *,campname
      iext=1

      im1=51
      ier=0
      iread=1
      call ftopen(im1,file1,iread,iblock,ier)
      if(ier.ne.0) then
         write(*,*) 'Error opening image : ',file1
         goto 706
      endif
      call ftmahd(im1,iext,ihd,ier)
      call ftghpr(im1,2,simple,ibit,naxis,naxes,ipc,igc,extend,ier)
      call ftpkys(im1,'AMPNAME',campname,"Amplifier in use",ier)

      call ftclos(im1,ier)

 706  continue
      end

