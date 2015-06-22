      subroutine statd(n,x,mean,sd,sk,c4,c6,max,min,io)
c**************************************************
c     Version : 9-14-2005
c     Precision : single precision out/double arithmetic
c     Function : Calculates mean, standard deviation, skewness - sk,
c      kurtosis - c4, 6-th order cumulant - c6, maximum - max, and
c      minimum - min of the sample.
c=================================================
      real*8 s1,s2,s3,s4,s6,dx,dx3,dmn,dn,ds,xmin,xmax
      real x(n),mean,max,min
      intent(in) n,x,io
      intent(out) mean,sd,sk,c4,c6,max,min
c**************************************************
c  Initialize accumulators
      xmax=x(1)
      xmin=x(1)
      s1=0.d0
      s2=0.d0
      s3=0.d0
      s4=0.d0
      s6=0.d0
      dn=dfloat(n)
      do k=1,n
       dx=dble(x(k))
       xmax=dmax1(dx,xmax)
       xmin=dmin1(dx,xmin)
c  Sum x
       s1=s1+dx
c  Sum x*x
       s2=s2+dx*dx
      enddo
      max=xmax
      min=xmin
c  Mean
      dmn=s1/dn
      mean=sngl(dmn)
c  Sum(x*x)-Sum(x)*Sum(x)/n
      s2=s2-dmn*s1
c  Check if variance=0
      if(s2<=0.d0) then
       write(io,*) 'Variance ',sngl(s2),' in STAT = 0 & is set = -1'
       sd=-1.0
       return
      endif
c  Check if variance<1.d-5
      if(s2<=1.d-5) then
       s2=0.d0
       do k=1,n
        dx=dble(x(k))-dmn
        s2=s2+dx*dx
       enddo
      endif
c  Compute standard deviation
      ds=dsqrt(s2/dfloat(n-1))
      sd=sngl(ds)
c  Compute other stats
      do k=1,n
       dx=dble(x(k))-dmn
       dx3=dx*dx*dx
       s3=s3+dx3
       s4=s4+dx3*dx
       s6=s6+dx3*dx3
      enddo
      dx3=ds*ds*ds
      s3=s3/(dn*dx3)
      sk=sngl(s3)
      s4=s4/(dn*dx3*ds)-3.d0
      c4=sngl(s4)
      s6=s6/(dx3*dx3)
      c6=sngl(s6/dn-15.d0*s4-10.d0*s3*s3-15.d0)
c-------
      return
      end
