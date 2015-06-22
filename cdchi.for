      subroutine cdchi(x,df,prob,io)
c********************************************
c  Calculates cumulative distribution function of chi square (df)
c  Input: x - level, df - degrees of freedom, io - output file no.
c  Output: prob - probability that chi**2(ndf) < x
c  Version : 8-18-99
c============================================
      real x,df,prob,tx
      parameter(tx=1.e-4)
      intent(in) x,df,io
      intent(out) prob
c********************************************
c Trap x
      if(x<0.) then
       write(io,*) 'x in cdchi ',x,' is not positive'
       return
      endif
      if(x<tx) then
       prob=0.
       return
      endif
c Trap df
      if(df<0.) then
       write(io,*) 'df ',df,' value is not positive in call to cdchi'
       return
      endif
      if(df<tx) then
       y=x/2
       if(y<=11) then
        prob=1.-exp(-y)
         else
          prob=1.
       endif
       return
      endif
      prob=gammp(0.5*df,0.5*x,io)
      if(prob<tx) then
       prob=0.
      endif
c-------
      return
      end
c
c==========================
c
      function gammp(a,x,io)
c******************************************
c  Incomplete gamma function. Uses gcf,gser
c==========================================
      real a,gammp,x
      real*8 gamser,gammcf,gln
      intent(in) a,x,io
c===========================
      if(x<0..or.a<=0.) then
       write(io,*) 'Bad argument in gammp'
       return
      endif
      if(x<a+1.)then
       call gser(gamser,a,x,gln,io)
       gammp=sngl(gamser)
        else
         call gcf(gammcf,a,x,gln,io)
         gammp=sngl(1.d0-gammcf)
      endif
      return
      end
c
c=========================================
c
      subroutine gcf(gammcf,a,x,gln,io)
c***********************************
c  Uses gammln
c===================================
      parameter(itmax=1000,eps=3.d-7,fpmin=1.d-30)
      real a,x
      real*8 gammcf,gln,dx,an,b,c,d,del,h,di,gammln
      intent(in) a,x,io
      intent(out) gammcf,gln
c================================
      gln=gammln(dble(a))
      dx=dble(x)
      b=dx+1.d0-dble(a)
      c=1.d0/fpmin
      d=1.d0/b
      h=d
      do i=1,itmax
       di=dfloat(i)
       an=-di*(di-dble(a))
       b=b+2.d0
       d=an*d+b
       if(dabs(d)<fpmin) d=fpmin
       c=b+an/c
       if(dabs(c)<fpmin) c=fpmin
       d=1.d0/d
       del=d*c
       h=h*del
       if(dabs(del-1.d0)<eps) then
        gammcf=dexp(-dx+dble(a)*dlog(dx)-gln)*h
        return
       endif
      enddo
      write(io,*) 'a = ',a,' too large, ITMAX too small in gcf'
      return
      end
c
c=========================================
c
      subroutine gser(gamser,a,x,gln,io)
c*************************************
c  Uses gammln
c=====================================
      parameter(itmax=1000,eps=3.d-7)
      real a,x
      real*8 gamser,gln,ap,dx,del,sum,gammln
      intent(in) a,x,io
      intent(out) gamser,gln
c=====================================
      if(x<=0.) then
       if(x<0.) then
        write(io,*) 'x < 0 in gser'
        return
       endif
       gamser=0.d0
       return
      endif
c-------
      ap=dble(a)
      gln=gammln(ap)
      dx=dble(x)
      sum=1.d0/ap
      del=sum
      do n=1,itmax
       ap=ap+1.d0
       del=del*dx/ap
       sum=sum+del
       if(dabs(del)<dabs(sum)*eps) then
        gamser=sum*dexp(-dx+dble(a)*dlog(dx)-gln)
        return
       endif
      enddo
      write(io,*) 'a = ',a,' too large, ITMAX too small in gser'
      return
      end
c
c=========================================
c
      function gammln(x)
c*************************************
c  log of gamma function
c=====================================
      real*8 ser,stp,tmp,x,y,cof(6),gammln
      save cof,stp
      data cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
      intent(in) x
c=====================================
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*dlog(tmp)-tmp
      ser=1.000000000190015d0
      do j=1,6
       y=y+1.d0
       ser=ser+cof(j)/y
      enddo
      gammln=tmp+dlog(stp*ser/x)
      return
      end
