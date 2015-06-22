      subroutine far(y,c,s,r,in,r2,sig,n,p,po,pv,ier,io)
c********************************************
c  Multi level iterative AR fit which loops back to discard lags
c   whose coefficients have prob value of abs(t) statistics < pv
c  Input: y - n observations, p - order in, pv - two sided p-value
c  Output: c - po AR coefficients, s - standard errors of c coeff's
c    r(1),...,r(n) - residuals
c    r2 - R square,  sig - standard deviation of residuals
c    in - array of lag values for significant lags, po - order out
c  Version 3-12-2005
c   ier = 1: A is not positive definite, ier = 2: p < 1, ier = 3: p > n/2
c============================================
      allocatable::a,b,d,w
      integer p,pi,po
      integer in(p)
      real y(n),r(n),s(p),pv
      real*8 c(p),a(:),b(:),w(:),d(:),sy,ssy
      intent(in) y,n,p,pv
      intent(out) c,s,r,in,po
c****************************
c  Trap p
      if(p<1) then
       write(io,*) 'p in AR(p) ',p,' < 1'
       ier=2
       return
        elseif(p>n/2) then
         write(io,*) 'p in AR(p) ',p,' > n/2 ',n/2,' n = ',n
         ier=3
         return
      endif
c-------
      allocate(a(0:p*p-1),b(0:p),d(0:p),w(0:p),stat=ibad)
      if(ibad/=0) then
       write(io,*) 'Unable to allocate work space for arrays in far'
       return
      endif
c  Covariances in a
      call cr(y,a,b,w,sy,ssy,n,p,io)
      do k=1,p
       c(k)=0.d0
       in(k)=k
      enddo
      pi=p
      po=p-1
      dowhile(po>0)
c  Iteration on ols
       call ar(y,c,s,r,a,b,d,w,n,p,sy,ssy,r2,sig,pv,ier,in,pi,po,io)
       if(po<pi) then
        pi=po
         else
          exit
       endif
      enddo
c-------
      deallocate(a,b,d,w)
      return
      end
c
c============================================
c
      subroutine ar(y,c,s,r,a,b,d,w,n,p,sy,ssy,r2,sig,pv,ier,in,pi,po,io
     *)
c********************************************
c  Computes estimates of parameters of an AR(p) model and the
c   residuals of the OLS fit.
c  Input: y - data array of n observations, order p
c  Output: r - array of e(k)=y(k)-c1*y(k-1)-c2*y(k-1) ...
c   c - p AR coefficients, s - standard errors of coefficients
c   sig- mean and standard deviation of residuals, r2 - R squared
c   po - Number of p values of t statistics < pv
c============================================
      integer p,pi,po,t,n
      integer in(p)
      real y(n),r(n),s(p),sig,pv
      real*8 a(p*p),b(p),d(p),w(p),c(p),sy,ssy,sse,sm,hd,dy,dnp
      intent(in) y,n,p,pi,pv,sy,ssy
      intent(out) c,s,r,sig,r2,po
      intent(in out) in
c********************************************
c  Cholesky decomposition of a
      call dcholdc(a,pi,d,ier,io)
      if(ier/=0) then
       write(io,*) 'DCHOLDC failed, A is not positive definite'
       write(io,*) 'pi = ',pi
       return
      endif
c-------
      call dcholsl(a,pi,d,b,c)
c  Compute residuals, mean, and standard deviation
      sse=0.d0
      hd=0.
      do t=in(pi)+1,n
       sm=0.d0
       do k=1,pi
        sm=sm+c(k)*(dble(y(t-in(k)))-sy)
       enddo
       dy=dble(y(t))-sy-sm
       r(t)=sngl(dy)
       hd=hd+dy
       sse=sse+dy*dy
      enddo
c  Zero out first pi residuals
      do t=1,in(pi)
       r(t)=0.
      enddo
      dnp=dfloat(n-pi)
      hd=hd/dnp
      sse=sse-dnp*hd*hd
c  R squared
      r2=1.-sngl(sse/ssy)
      r2=r2-float((pi-1)/(n-pi))*(1-r2)
      sig=sngl(dsqrt(sse/dfloat(n-pi-1)))
c-------
c  Inverse of A
      call dinverse(a,pi,d)
c  Standard errors of c's
      prb=1.-pv
      po=0
      do k=1,pi
       s(k)=sig*sngl(dsqrt(a((k-1)*pi+k)))
       ch=sngl(c(k))/s(k)
       call cdchi(ch*ch,1.,pc,io)
       if(pc>prb) then
        po=po+1
        in(po)=in(k)
       endif
      enddo
c-------
      if(po<pi .and. po>0) then
c  Reduce down the covariance matrix
       call reduce(a,b,w,ssy,p,po,in,io)
      endif
c-------
      return
      end
c
c===========================================
c
      subroutine cr(y,a,b,w,sy,ssy,n,p,io)
c********************************************
c  Covariances of y in a & variances in b,w
c  sy - Mean of y, ssy - Sum of squares of y
c============================================
      integer p,t
      real y(n)
      real*8 a(p*p),b(p),w(p),sy,ssy,ac
      intent(in) y,n,p
      intent(out) a,b,w,sy,ssy
c********************************************
c  Compute mean
      sy=0.d0
      do t=1,n
       sy=sy+dble(y(t))
      enddo
      sy=sy/dfloat(n)
c-------
c  Compute sums
      ssy=(dble(y(n))-sy)*(dble(y(n))-sy)
      do k=1,p
       ac=0.d0
c  Compute covariances
       do t=1,n-k
        ac=ac+(dble(y(t))-sy)*(dble(y(t+k))-sy)
        if(k==1) then
         ssy=ssy+(dble(y(t))-sy)*(dble(y(t))-sy)
        endif
       enddo
       b(k)=ac
       w(k)=ac
      enddo
c-------
c  Put in upper triangle of matrix a
      a(p*p)=ssy
      do j=1,p-1
       do k=j+1,p
        a((k-1)*p+j)=b(k-j)
       enddo
       a((j-1)*p+j)=ssy
      enddo
c-------
      return
      end
c
c===========================================
c
      subroutine reduce(a,b,w,ssy,p,po,in,io)
c********************************************
c  Rearrange a & b for reduced least squares fit
c============================================
      integer p,po
      integer in(p)
      real*8 a(p*p),b(p),w(p),ssy
      intent(in) w,ssy,in,p,po
      intent(out) a,b
c********************************************
      do k1=1,po
       b(k1)=w(in(k1))
       a((k1-1)*po+k1)=ssy
       if(po>0) then
        do k2=k1+1,po
         a((k2-1)*po+k1)=w(in(k2)-in(k1))
        enddo
       endif
      enddo
c-------
      return
      end
