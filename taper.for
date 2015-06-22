      subroutine taper(w,lb,p,sw,nt,io)
c***************************************
c  M.J. Hinich   Version: 8-22-94
c  Compute trancated cosine taper array
c=======================================
c   Input:
c    lb    Number of samples in frame
c    p     % of data series to be tapered (0.< p < 50.)
c    io    File number for error message
c   Output:
c    w     Cosine taper weights as a symmetric array
c    sw    mean sum of squares of the taper including the middle 1's
c    nt    Number of observations tapered on each end of window
c=======================================
      real w(lb)
      intent(in) lb,p,io
      intent(out) w,sw,nt
c***************************************
      pi=acos(-1.)
c  Return if p=0
      if(p<=0.) then
       return
      endif
      if(p<0..or.p>50.) then
       write(io,*) '* ERROR *  Improper % for tapering: ',p
       return
      endif
c-------
c  Compute number of values to be tapered on each end
      nt=nint(p*float(lb)/200.)
      if(nt>lb/4) then
       write(io,*) 'Taper size ',nt,' > lb/4. Increase data frame ',lb
       return
      endif
c  Taper weights and sum of squares sw
      sw=0.
      do k=1,nt
       w(k)=sin(pi*k/(2.*float(nt+1)))
       w(lb+1-k)=w(k)
       sw=sw+w(k)*w(k)
      enddo
      sw=(2.*sw+float(lb-2*nt))/float(lb)
c  Fill out w array
      do k=nt+1,lb-nt
       w(k)=1.
      enddo
c-------
      return
      end
