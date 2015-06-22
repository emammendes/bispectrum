      subroutine number(string,sn,num,io)
c*******************************************
c  Integer from character integer in string at pointer ip
c  sn - field for integer num
c===========================================
      integer sn,num
      character*7 string
      intent(in) string,sn,io
      intent(out) num
c********************************************
      select case(sn)
       case(1)
        num=ichar(string(1:1))-48
       case(2)
        n1=ichar(string(1:1))-48
        n2=ichar(string(2:2))-48
        num=n1*10+n2
       case(3)
        n1=ichar(string(1:1))-48
        n2=ichar(string(2:2))-48
        n3=ichar(string(3:3))-48
        num=n1*100+n2*10+n3
       case(4)
        n1=ichar(string(1:1))-48
        n2=ichar(string(2:2))-48
        n3=ichar(string(3:3))-48
        n4=ichar(string(4:4))-48
        num=n1*1000+n2*100+n3*10+n4
       case(5)
        n1=ichar(string(1:1))-48
        n2=ichar(string(2:2))-48
        n3=ichar(string(3:3))-48
        n4=ichar(string(4:4))-48
        n5=ichar(string(5:5))-48
        num=n1*10000+n2*1000+n3*100+n4*10+n5
       case(6)
        n1=ichar(string(1:1))-48
        n2=ichar(string(2:2))-48
        n3=ichar(string(3:3))-48
        n4=ichar(string(4:4))-48
        n5=ichar(string(5:5))-48
        n6=ichar(string(6:6))-48
        num=n1*100000+n2*10000+n3*1000+n4*100+n5*10+n6
       case(7)
        n1=ichar(string(1:1))-48
        n2=ichar(string(2:2))-48
        n3=ichar(string(3:3))-48
        n4=ichar(string(4:4))-48
        n5=ichar(string(5:5))-48
        n6=ichar(string(6:6))-48
        n7=ichar(string(7:7))-48
        num=n1*1000000+n2*100000+n3*10000+n4*1000+n5*100+n6*10+n7
      endselect
c-------
      return
      end
