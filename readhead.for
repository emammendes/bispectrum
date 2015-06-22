      subroutine readhead(id,nfl,nc,sr,fmt,iunit,ii,io)
c********************************************
c  Read rows, columns, sampling rate, format, & check unit for data file ii
c============================================
      character*120 buf
      character*80 id
      character*50 charead,fmt
      character*20 par
      character*2 delim
      intent(in) iunit,ii,io
      intent(out) id,nfl,nc,sr,fmt
c****************************
      rewind(ii)
c  Read file header
      read(ii,'(a80)')id
      read(ii,'(a120)')buf
      ia=index(buf,'=kHz')
      ib=index(buf,'=khz')
      ic=index(buf,'=Khz')
      ij=index(buf,'=KHz')
      ig=index(buf,'=Hz')
      ih=index(buf,'=hz')
      if(ia==0.and.ib==0.and.ic==0.and.ij==0.and.ig==0.and.ih==0.and.iun
     *it>0) then
       write(io,'(/2x,''ERROR - unit=! is not typed after the format on
     *the data file used'')')
       write(io,*)buf
       write(6,'(/2x,''ERROR - unit=! is not typed after the format on t
     *he data file used'')')
       write(6,*)buf
       write(9,'(/2x,''ERROR - unit=! is not typed after the format on t
     *he data file used'')')
       write(9,*)buf
       return
      endif
c  Sample size (rows)
      delim(1:2)='= '
      par='ows'
      nfl=numread(par,buf,delim,ia,ier,io)
      if(ier>0) then
       write(io,*)'Error in reading the no. of rows of the data file'
       write(io,*)'Error number for rnumread = ',ier
       write(6,*)'Error in reading the no. of rows of the data file'
       write(6,*)'Error number for rnumread = ',ier
       write(9,*)'Error in reading the no. of rows of the data file'
       write(9,*)'Error number for rnumread = ',ier
       stop
      endif
c-------
c  Columns
      par='olumns'
      nc=numread(par,buf,delim,ia,ier,io)
      if(ier>0) then
       write(io,*)'Error in reading the no. of column of the data file'
       write(io,*)'Error number for rnumread = ',ier
       write(6,*)'Error in reading the no. of column of the data file'
       write(6,*)'Error number for rnumread = ',ier
       write(9,*)'Error in reading the no. of rows of the data file'
       write(9,*)'Error number for rnumread = ',ier
       return
      endif
      if(iunit>0) then
c  Sampling rate
       par='rate'
       sr=rnumread(par,buf,delim,ia,ier,io)
       if(ier>0) then
        par='Rate'
        sr=rnumread(par,buf,delim,ia,ier,io)
       endif
      endif
      if(ier>0) then
       write(io,*)'Error in reading the sampling rate of the data'
       write(io,*)'Error number for rnumread = ',ier
       write(6,*)'Error in reading the sampling rate of the data'
       write(6,*)'Error number for rnumread = ',ier
       write(9,*)'Error in reading the sampling rate of the data'
       write(9,*)'Error number for rnumread = ',ier
       stop
      endif
      if(iunit==0) then
c  Sampling interval
       par='nterval'
       sr=rnumread(par,buf,delim,ia,ier,io)
      endif
      if(ier>0) then
       write(io,*)'Error in reading the sampling interval of the data'
       write(io,*)'Error number for rnumread = ',ier
       write(6,*)'Error in reading the sampling interval of the data'
       write(6,*)'Error number for rnumread = ',ier
       write(9,*)'Error in reading the sampling interval of the data'
       write(9,*)'Error number for rnumread = ',ier
       stop
      endif
c-------
c  Format
      par='rmat'
      fmt=charead(par,buf,delim,ia,ier,3)
      if(ier>0) then
       write(io,*)'Error in reading the format from the data file'
       write(io,*)'Error number for rnumread = ',ier
       write(6,*)'Error in reading the format from the data file'
       write(6,*)'Error number for rnumread = ',ier
       write(9,*)'Error in reading the format from the data file'
       write(9,*)'Error number for rnumread = ',ier
       stop
      endif
c  Trap kHz or Hz in second line
      if(((ia==0.and.ib==0.and.ic==0.and.ij==0).and.(ig>0.or.ih>0)).and.
     *iunit==1) then
       write(io,'(/2x,''ERROR - the control file sets the units in kHz b
     *ut Hz is indicated on line 2 of the data file'')')
       write(io,*)buf
       write(6,'(/2x,''ERROR - the control file sets the units in kHz bu
     *t Hz is indicated on line 2 of the data file'')')
       write(6,*)buf
       write(9,'(/2x,''ERROR - the control file sets the units in kHz bu
     *t Hz is indicated on line 2 of the data file'')')
       write(9,*)buf
       return
      endif
      if((ia>0.or.ib>0.or.ic>0.or.ij>0).and.iunit==2) then
       write(io,'(/2x,''ERROR - the control file sets the units in Hz bu
     *t kHz is indicated on line 2 of the data file'')')
       write(io,*)buf
       write(6,'(/2x,''ERROR - the control file sets the units in Hz but
     * kHz is indicated on line 2 of the data file'')')
       write(6,*)buf
       write(9,'(/2x,''ERROR - the control file sets the units in Hz but
     * kHz is indicated on line 2 of the data file'')')
       write(9,*)buf
       return
      endif
c-------
      return
      end
