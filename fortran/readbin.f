
      open(unit=1,file="spec",form="unformatted",access='direct')
      do i=1,100
         read(1,*,end=666) x1
         print *,x1
      enddo
 666  continue

      close(1)

      end
      
