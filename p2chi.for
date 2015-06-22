      function p2chi(x,fl,io)
c*****************************************************************
c  M.J. Hinich   Version: 2-12-95  Single Precision Function
c  Calculates the noncentral chi square cdf with 2 degrees-of-freedom
c   and noncentrality parameter Lamda(fl) at x.
c  io - output file number
c=====================================================
      real x,fl,p2chi
      real*8 c,y,fl2,dn,ly,tgam,ll,term,ey,el,pl,pw,chi,ac,at
      parameter(ntries=10000,ac=1.d-10,at=1.d-20)
      intent(in) x,fl,io
c*****************************************************
      if(x<=ac) then
       p2chi=0.
       return
        elseif(x<0.) then
         write(io,*) 'Argument is negative',x
         p2chi=-1.
         return
      endif
c-------
      if(fl<0.) then
       write(io,*) 'Noncentrality Parameter Lamda < 0. ',fl
       return
      endif
      if(fl<1.e-7) then
       y=dble(x/2.)
       if(y<=14.d0) then
        p2chi=real(1.d0-dexp(-y))
         else
          p2chi=1.
       endif
       return
      endif
c-------
      y=dble(x/2.)
      if(y<=14.d0) then
       ey=dexp(-y)
        else
        ey=0.d0
      endif
      fl2=dble(fl/2.)
      if(fl2<=14.d0) then
       el=dexp(-fl2)
        else
         el=0.d0
      endif
c-------
      ll=dlog(fl2)
      ly=dlog(y)
      c=0.d0
      tgam=ey
      chi=(1.d0-ey)*el
c   Compute weighted sum of GAMMA(y,1+t)
      do n=1,ntries
       dn=dfloat(n)
c   Compute log of (Lamda/2)**N/N!exp(-Lamda/2) Poisson probability
c   Log of N! accumulated in c
       c=c+dlog(dn)
       pl=dn*ll-c-fl2
c   Compute log of terms in gamma sum
       ey=dn*ly-c-y
       if(ey>=-14.d0) then
        tgam=tgam+dexp(ey)
       endif
c   Compute Poisson probability
       if(pl>=-14.d0) then
        pw=dexp(pl)
         else
          pw=0.d0
       endif
c--------
c   Compute Nth term in expansion of noncentral chi square
       term=pw*(1.d0-tgam)
c   Check for sum termination
       if(chi>0.99999d0) then
        p2chi=1.
        return
       endif
       if(term<ac*chi.or.(term<at.and.chi>1.d-6.or.dabs(tgam-
     *1.d0)<ac)) then
        p2chi=real(chi)
        return
       endif
       chi=chi+term
      enddo
      write(io,*) 'Sum did not converge in 10000 terms.',chi
      p2chi=-1.
c--------
      return
      end
