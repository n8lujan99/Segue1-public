
      real r1
      real*8 d1

      r1=250.23456789
      d1=250.23456789d0
      r1=0.6666666666
      d1=0.6666666666d0
      print *,r1
      print *,d1
      print *,3600.d0*(dble(r1)-d1)

      end
