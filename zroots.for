      subroutine zroots(a,m,roots,plsh,ier)
      parameter(eps=1.e-6,maxm=201)
c********************************************
c  Complex roots of polynomial Sum(i=0,m) a(i+1)x**i
c============================================
      complex a(m+1),roots(m),ad(maxm),x,b,c
      logical*1 plsh
      intent(in) a,m
      intent(out) roots,ier
c****************************
c Trap m
      if(m>=maxm) then
       write(6,*) 'm+1 ',m+1,' > maxm ',maxm,' in ZROOTS'
       return
      endif
      do j=1,m+1
       ad(j)=a(j)
      enddo
      do j=m,1,-1
       x=cmplx(0.,0.)
       call laguer(ad,j,x,its,ier)
       if(abs(aimag(x))<=2.*eps**2*abs(real(x))) x=cmplx(real(x),0.)
       roots(j)=x
       b=ad(j+1)
       do jj=j,1,-1
        c=ad(jj)
        ad(jj)=b
        b=x*b+c
       enddo
      enddo
      if(plsh) then
       do j=1,m
        call laguer(a,m,roots(j),its,ier)
       enddo
      endif
      do j=2,m
       x=roots(j)
       do i=j-1,1,-1
        if(real(roots(i))<=real(x)) goto 1
        roots(i+1)=roots(i)
       enddo
       i=0
1      roots(i+1)=x
      enddo
c-------
      return
      end
c
c============================================
c
      subroutine laguer(a,m,x,its,ier)
      parameter(epss=2.e-7,mr=8,mt=100,maxit=mt*mr)
c***********************************
c  Called by zroots
c===================================
      real frac(mr)
      complex a(m+1),x,dx,x1,b,d,f,g,h,sq,gp,gm,g2
      save frac
      data frac /.5,.25,.75,.13,.38,.62,.88,1./
      intent(in) a,m
      intent(out) x,its,ier
c****************************
      do iter=1,maxit
       its=iter
       b=a(m+1)
       err=abs(b)
       d=cmplx(0.,0.)
       f=cmplx(0.,0.)
       abx=abs(x)
       do j=m,1,-1
        if(abs(f)>1.e35.or.abs(b)>1.35.or.abs(d)>1.35) then
         exit
        endif
        f=x*f+d
        d=x*d+b
        b=x*b+a(j)
        err=abs(b)+abx*err
       enddo
       err=epss*err
       if(abs(b)<=err) then
        return
         else
          g=d/b
          g2=g*g
          h=g2-2.*f/b
          sq=sqrt((m-1)*(m*h-g2))
          gp=g+sq
          gm=g-sq
          abp=abs(gp)
          abm=abs(gm)
          if(abp<abm) gp=gm
          if(max(abp,abm)>0.) then
           dx=m/gp
            else
             dx=exp(cmplx(log(1.+abx),real(iter)))
          endif
        endif
        x1=x-dx
        if(abs(x-x1)<1.e-9) return
        if(mod(iter,mt)/=0) then
         x=x1
          else
           x=x-dx*frac(iter/mt)
       endif
      enddo
c-------
      ier=1
      return
      end
