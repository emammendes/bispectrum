      subroutine port(x,n,e,c,io)
c********************************************
c  c - protmentau test statistic p-value
c  x - data array, n - sample size, nlg=n**e, sum2 - sum of squared cors
c============================================================
      real x(n),c,e
      real*8 cv,sum2,rn
      integer t
      intent(in) x,n,e,io
      intent(out) c
c************************
      rn=dfloat(n-1)
      nlg=nint(float(n)**e)
      rng=float(nlg)
      c=1.
      sum2=0.d0
c  Start loop on k
      do k=1,nlg
       rn=dfloat(n-k)
c  Sum x(t)*x(t+k)
        cv=0.d0
        do t=1,n-k
         cv=cv+dble(x(t))*dble(x(t+k))
        enddo
       sum2=sum2+cv/rn*cv
      enddo
c  Chi**2 for squared cors
c  Correlation statistic
      call cdchi(sngl(sum2),rng,c,io)
      c=1.-c
c--------
      return
      end
