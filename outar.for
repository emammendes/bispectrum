      subroutine outar(c,s,a,rt,in,r2,se,su,sr,pv,p,po,iunit,io)
c*******************************************
c  Displays AR coefficients and their t stats.
c  Input: c - AR coefficients, s - std errors, se - sig of AR fit
c   sr - sampling rate, p - in no. of lags, po - out no. of lags
c  Output: rt - roots of the characteristic polynomial for AR model
c===========================================
      integer p,po,iunit,in(p),ep
      real*8 c(po)
      complex a(p+1),rt(p)
      real s(po),amp,per,pi,sr
      character*10 su
      logical*1 plsh
      intent(in) c,s,r2,se,su,sr,pv,p,po,iunit,io
      intent(out) rt
c********************
c  Trap zero s's
      ier=0
      do k=1,po
       if(s(k)<=0.) then
        write(io,*) 'Standard error s(',k,') = ',s(k),' < 0 in outar'
        ier=ier+1
       endif
      enddo
      if(ier>0) then
       return
      endif
      ep=in(po)
      do k=1,ep
       a(k)=cmplx(0.,0.)
      enddo
c-------
      pi=acos(-1.)
      plsh=.true.
c  Put coefficients of AR into a
      do k=1,po
       a(ep-in(k)+1)=cmplx(-1.*sngl(c(k)),0.)
      enddo
      a(ep+1)=cmplx(1.,0.)
      call zroots(a,ep,rt,plsh,ier)
c-------
      do k=1,ep
       amp=cabs(rt(k))
       if(abs(aimag(rt(k)))<1.e-5) then
        per=0.
         else
          if(iunit==1) then
           per=1.e3*sr*abs(atan2(aimag(rt(k)),real(rt(k))))/(2.*pi)
            elseif(iunit==2) then
             per=sr*abs(atan2(aimag(rt(k)),real(rt(k))))/(2.*pi)
              else
               per=2.*pi/(sr*abs(atan2(aimag(rt(k)),real(rt(k)))))
               if(per>1.e+4) then
                per=1.e4
               endif
          endif
       endif
       rt(k)=cmplx(amp,per)
      enddo
c-------
      write(io,'(/17x,''AR('',i3,'') parameters / t values'')') in(po)
      write(io,'(/6x,''Adjusted R Square = '',f5.3,2x,''Std Error of AR
     *Fit = '',g10.3)') r2,se
      write(io,'(7x,''p Value Threshold for the Iterative AR Prewhitenin
     *g Method = '',f5.3)') pv
      write(io,'(6x,47(''='')/)')
      write(io,'(70(:,i3,'' - '',f8.2,2x))') (in(k),c(k),k=1,po)
      write(io,'(70(:,6x,f8.2,2x))') (sngl(c(k))/s(k),k=1,po)
      write(io,*)
c-------
      if(iunit==2) then
       write(io,'(1x,''Amplitudes & Frequencies of the Roots of the AR P
     *olynomial in units of Hz'')')
       elseif(iunit==1) then
        write(io,'(1x,''Amplitudes & Frequencies of the Roots of the AR
     *polynomial in units of kHz'')')
         elseif(iunit==0) then
          write(io,'(1x,''Amplitudes & Periods of the Roots of the AR Po
     *lynomial in units of '',a5)') su
      endif
      write(io,'(11x,''A real root is given a period = 0.'')')
      write(io,'(6x,47(''='')/)')
      write(io,'(70(:,i3,'' - '',f8.2,2x))') (k,real(rt(k)),k=1,ep)
      write(io,'(70(:,6x,f8.2,2x))') (aimag(rt(k)),k=1,ep)
      write(io,*)
c-------
      return
      end
