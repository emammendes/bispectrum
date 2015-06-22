      program bispec
c*********************************************************************
c  Version 1-3-2006   M. J. Hinich
c  File 3 (fname.out) is always opened where 'fname' is the name of the data
c  file used to the left of the .extention. The output in 3 are statistics for
c  the bispectrum for the data used along with summary stats for this data.
c
c  IMPORTANT - Error messages are written to bispec-error.out.
c
c  File 2 - data file
c  The bispectrum file name is #.bispectrum where # = data file name.
c======================================================================
c  Parameters in control file Runbispec.cnl. Use spaces between entries!
c  A delimiter symbol * must be placed on each line.
c  Place comments on the lines of runbispec.cnl file after the symbol *.
c  Runbispect.cnl allows runs a number of control files with different file
c  names containing different data files and parameters.
c  The parameters for each run are echoed in filename.out where filename
c  is the name of the data file used.
c===================================================================

c  Lines for the control file Runbispec.cnl
c  The parameters & commands are found using key words whose first letter
c  may be either upper or lower case. The rest of the key word must be in
c  lower case. The symbol ! is a wildcard for the value to be set.
c  Examples: !nrun below is the wildcard for the number of runs of the program
c  using different cnl files called by runbispect.cnl. The integer to the right
c  of the = sign in runbispect.cnl is read by the program and sets the number
c  of runs.
c  In any *.cnl called by runbispect.cnl, !r is the wildcard for the resolution
c  bandwidth. The real number to the right of !r= is the resolution bandwidth
c  that is used.
c
c  ALWAYS use 0.* rather than .* since the program searches for the number
c   to the left of the period in the parameter.
c
c  A delimiter symbol * must be placed on each line.
c  Use the keywords followed by an = sign and the setting. Some settings are
c   character strings while others are real numbers.
c  Comments on the lines of the cnl file are placed to the right of the
c   symbol *. These are not read by the program.
c
c===================================================================
c
c  Number of runs=!nrun   (!nrun - integer wildcard)
c
c  There has to be !nrun cnl files in runbispec.cnl
c
c============================
c
c  Then the list of !nrun control files is read
c
c  The path & names of the cnl files that pass parameters to the program
c  For example if !nrun=2 & the control files are in the directory cnl:
c  \cnl\bispec-fil1.cnl             * cnl file
c  \cnl\bispec-fil2.cnl             * cnl file
c
c  These entries are followed by the line
c  # */ End of Runbispec.cnl file names reads
c
c============================
c
c  The loop on runs k = 1,...,!nrun begins for !nrun control files.
c  The data file names for each control file & run parameters are now read.
c
c  The parameter lines are followed by the line
c  # */ End of control file parameter reads
c
c  The program looks for the delimiter symbol # to end the reading of the
c  parameters for each control file set in Runbispec.cnl
c
c=========================
c
c  Data file name=data file name with path
c
c=========================
c
c  Use column number=!ncol
c  !ncol is the column number of the nc columns in the data file
c  Set !ncol=1 if there is only one column in the data (flat file)
c
c=========================
c
c  Resolution bandwidth=!rb  ( !r - real wildcard)
c   This bandwidth is in Hz or the unit used below if frequency is not typed
c  Note: rb determines the frame length lb used to compute the spectrum
c
c
c=========================
c
c  Sampling unit=!unit
c  freqency+k or frequency+h to interpret the sampling rate as a multiple
c   of kHz or Hz. You can also use the upper case letters K or H.
c  If k is used the spectrum output unit is kHz, or if h is used the
c   spectrum output unit is Hz  OR
c    a 10 character unit in lower case to interpret the sampling rate
c    as a multipled of this unit. The output will be in periods using that
c    unit as for example  unit='sec'
c
c=======================================================================
c
c  NOTE: sr, the 2nd number on line 2 of the data file is the sampling
c   rate in kHz if frequency+K above, or Hz if frequency+H above, or sr is
c   a sampling interval with the unit determined by the unit determined by
c   the characters placed after unit=
c   If there is an 'a' in the format, then a time mark file is read
c   An example string is: (f7.2,2x,a7)
c   The time mark must be less than 21 characters long
c
c=======================================================================
c
c  Lower frequency=!rl  upper frequency=!ru or
c  Longest period=!rl   shortest period=!ru
c   The passband is (!rl,!ru)
c   !rl - lower frequency, !ru - upper frequency of band in units of kHz
c   if frequency+kHz  is typed or if OR Hz if frequency+Hz is typed on a line
c   !rl - longest period - the longest period
c   !ru - shortest period - the shortest period if a unit is used
c
c   If !ru= 0. then the normalized upper frequencey is 0.5. or the
c    normalized shortest period is lb/2
c
c=========================
c
c  File read start=!ns  file read end=!ne
c   !ns - index of datum first read, !ne - index of datum last read
c    If !ne = 0, all the data is read
c
c=========================
c
c  % taper=!rpt
c   !rpt = % taper of frame for sidelobe reduction (0. < rpt < 25.)
c   If !rpt = 0, frames are NOT tapered
c
c=========================
c
c  Number of resamples=!n, quantile=!q
c   Use a positive integer !n > 10 for the number of resamples if you want
c   to bootstrap the nonlinearity test statistic for the !q'th
c   quantile of the normalized bispectrum.
c   The bootstrap used is a uniform shuffle of the residuals of the
c   optimizing AR(p) fit used to whiten the data. The starting value of
c   p is set in %.cnl.
c   Type 0 if you do not want to bootstrap the test statistics
c   Note - bootstrapped p - value fractiles are only computed when the data
c   is whitened with an AR fit.
c
c=========================
c  NOTE:
c  The modified Hinich (1982) nonlinearity test uses the quantile qv = 0.8
c  Now I prefer qv = 0.99
c
c=========================
c
c  If you want to prewhiten with an AR(np) fit type the character string
c   prewhiten with AR(!np) fit
c   !np - starting number of parameters of a recursive least squares AR
c    fit if np > 0. If np = 0 then this step is bypassed.
c  No AR fit is made if this character string is absent in the lines above #.
c   The residuals of the AR(np) are white if np is sufficiently large.
c   Using an AR(np) fit to generate white residuals is a linear operation
c    on the raw data.
c   The AR model is estimated in the subroutine far that outputs the
c    po < np used as the end the iterations.
c
c=========================
c
c  Prob threshold=!rpv (for AR t values if AR fit is used)
c   !rpv - probability level threshold for the t statistics for the AR
c    coefficients of each AR fit. If a coefficient has a t value whose
c    absolute value has a probability Pr(abs(t)) < 1 - rpv, it is deleted in
c    the next AR fit. The covariance matrix is adjusted for the subset of
c    lagged values used.
c
c   The AR model is estimated in the subroutine far which outputs the
c    po < np used as the end of at most three iterations
c
c    If rpv = 1 then the full AR(np) model is estimated
c
c======================================================================
c
c  Data file format for the first two lines (records)
c
c  Line 1: A character string that identifies the data in thie file .
c  Line 2: If the unit is in frequency then line 2 using an EXAMPLE format is
c  rows=!nfl, columns=!nc, sampling rate=!sr, format=(f10.5,2x,a12) unit=kHz
c  Line 2: If the unit is a time unit then line 2 is
c  rows=!nfl, columns=!nc, sampling interval=!sr, format=(f10.5,2x,a12) unit=day
c
c  The data file contains nc time series in column form where the data
c  format is set in the second line of the data file.
c  Up to 50 columns can be read.
c
c   nfl - no. of observations, sr - sampling rate in kHz if freq. is
c   set, or in multiples of the time unit set in trispec-fn.cnl
c
c   Data format is a character*50 string including ( )
c   unit=kHz or khz if that is the sr unit
c   unit=Hz or hz if that is the sr unit  OR unit=unit in time for sr
c   This character string is used to check the read from %.cnl
c
c*****************************************************
      integer run,nr
      real gs,rt
      character*700 name
      character*80 buf,cnl
      character*50 charead
      character*20 par
      character*10 su
      character*2 delim
      logical exs
c**********************
c  Start time
      call cpu_time(gs)
      open(9,file='Bispec-error.out')
c  Inquire if file exists
      inquire(file='Runbispec.cnl',exist=exs)
      if(.not.exs) then
       write(6,'(/)')
       write(6,*)'Runbispec.cnl file does not exist'
       write(9,*)'Runsbispec.cnl file does not exist'
       stop
      endif
c  Open summary output file
      open(3,file='Bispec.out',err=2,iostat=io2)
c  Write name of test
      write(3,'(11x,'' Bispectrum Statistics by M. J. Hinich'')')
c  Open control file
      open(1,file='Runbispec.cnl',err=1,iostat=io1,status='old')
      read(1,'(a80)',end=2,err=3,iostat=io2) buf
      ig=index(buf,'*')
      if(ig==0) then
       write(6,*)'Place comment marker * after each line in Runbispect.c
     *nl'
       write(9,*)'Place comment marker * after each line in Runbispect.c
     *nl'
       write(6,'(a70)')buf
       write(9,'(a70)')buf
       stop
      endif
c  No. of runs
      par='uns'
      delim(1:2)='= '
      run=numread(par,buf,delim,ia,ier,io)
c  Multiple runs
      do nr=1,run
c  Read control file
       read(1,'(a80)',end=4,err=5,iostat=io2) cnl
       ig=index(cnl,'*')
       cnl=cnl(:ig-1)
       inquire(file=cnl,exist=exs)
       if(.not.exs) then
        write(6,'(/)')
        write(6,*) 'cnl file does not exist ',cnl
        write(9,'(/)')
        write(9,*) 'cnl file does not exist ',cnl
        stop
       endif
c  Open control file
       open(11,file=cnl,err=6,iostat=io3,status='old')
       call top(gs,run,nr,3)
      enddo
c  End of loop on runs
      call flush(3)
      stop
  1   write(6,*) '**** Error on opening Runspect.cnl ',io1
      stop
  2   write(6,*) 'End of file on runbispec.cnl reads'
      write(9,*) 'End of file on runbispec.cnl reads'
  3   write(6,*) 'Place the symbol # after the two top reads ',io1
      write(9,*) 'Place the symbol # after the two top reads ',io1
      stop
  4   write(6,*) 'End of file on runbispec.cnl reads'
      write(9,*) 'End of file on runbispec.cnl reads'
      stop
  5   write(6,*) '**** Error on reading name of cnl file ',nr,io2
      write(6,*) cnl
      write(9,*) '**** Error on reading name of cnl file ',nr,io2
      write(9,*) cnl
      stop
  6   write(6,*) '**** Error on opening cnl file no. ',nr,io3
      write(6,*) cnl
      write(9,*) '**** Error on opening cnl file no. ',nr,io3
      write(9,*) cnl
      stop
c-------
      stop
      end
c
c===========================================
c
      subroutine top(gs,run,nr,io)
c********************************************************
c  Open data file and read data
c========================================================
      parameter(mt=1,ndy=3,ny=2006)
      real*8 rlb
      integer il,iu,ot,p,res,run
      character*700 name
      character*80 buf,id
      character*50 charead,fmt,fname
      character*24 dt
      character*20 par
      character*15 su
      character*4 str
      character*2 delim
      character tro
      logical exs
      intent(in) gs,run,nr,io
c********************************************
      ip=0
      do k=1,17
c  Clear buf
       do n=1,80
        buf(n:n)=' '
       enddo
c  Read line
       read(11,'(a80)') buf
       ia=index(buf,'#')
       if(ia>0) then
        exit
       endif
       ig=index(buf,'*')
       if(ig==0) then
        write(io,*) 'Place comment marker * after parameters on map.cnl'
        write(io,'(a70)') buf
        stop
       endif
       ig=ig-1
       name(ip+1:ip+ig)=buf(:ig)
       ip=ip+ig
      enddo
c  Data file name from control file
      par='ile name'
      delim(1:2)='= '
      fmt=charead(par,name,delim,ia,ier,3)
      if(ier>0) then
       write(io,*)'Error in reading the data file name from %.cnl'
       write(6,*) 'Error in reading the data file name from %.cnl'
       write(9,*) 'Error in reading the data file name from %.cnl'
       write(io,*) 'Error number for charead = ',ier
       write(6,*) 'Error number for charead = ',ier
       write(9,*) 'Error number for charead = ',ier
       stop
      endif
c  Data file
      ia=index(fmt,'.')
      if(ia==0) then
       write(io,*) 'Data file name does not have an extension'
       write(io,*) fmt
       write(6,*) 'Data file name does not have an extension'
       write(6,*) fmt
       stop
      endif
c-------
      close(2)
c  Inquire if file exists
      inquire(file=fmt,exist=exs)
      if(.not.exs) then
       write(io,*)'Input file ',fmt,' does not exist'
       write(6,*)'Input file ',fmt,' does not exist'
       write(9,*)'Input file ',fmt,' does not exist'
       stop
      endif
      open(2,file=fmt,err=1,iostat=io1,status='old')
c  Parse for up to 5 path switches
      ib=index(fmt,'\')
      if(ib>0) then
       ic=index(fmt(ib:),'\')
       ib=ib+ic
       if(ic>0) then
        ic=index(fmt(ib:),'\')
        ib=ib+ic
        if(ic>0) then
         ic=index(fmt(ib:),'\')
         ib=ib+ic
        endif
        if(ic>0) then
         ic=index(fmt(ib:),'\')
         ib=ib+ic
        endif
       endif
        else
         ib=1
      endif
      fname=fmt(ib:ia)
      nfn=ia-ib+2
      fname(nfn:nfn+10)='bispectrum'
c  Open bispectrum output file for graphing
      close(7)
      open(7,file=fname,err=2,iostat=io2)
      call fdate(dt)
      write(7,'('' Version '',i2,''-'',i2,''-'',i4,7x,''Run '',a24)') mt
     *,ndy,ny,dt
c-------
c  Spectrum file name
      fname(nfn:nfn+10)='spectrum '
      open(4,file=fname,err=3,iostat=io3)
c  Column number
       kcol=0
       par='umber'
       kcol=numread(par,name,delim,ia,ier,io)
       if(ier>0.or.kcol<=0) then
        write(io,'(7x,''ERROR reading use column number '',i3)')kcol
        write(6,'(7x,''ERROR reading use column number '',i3)')kcol
        write(9,'(7x,''ERROR reading use column number '',i3)')kcol
        stop
       endif
c-------
c  Sampling unit
      par='unit'
      fmt=charead(par,name,delim,ia,ier,3)
      if(ier==1) then
       write(io,'(/7x,''Sampling unit in the cnl file is incorrect'')')
       stop
      endif
      su=fmt(:15)
      ift=index(fmt,'req')
      if(ift>0) then
       ik=index(fmt,'+K')
       ih=index(fmt,'+H')
       ikl=index(fmt,'+k')
       ihl=index(fmt,'+h')
      endif
      if(ift>0 .and. (ik>0 .or. ikl>0)) then
c  kHz
       iunit=1
        elseif(ift>0 .and. (ih>0 .or. ihl>0)) then
c  Hz
         iunit=2
          elseif(ift==0) then
c  Unit set in %.cnl
           iunit=0
      endif
      if(ift>0.and.((ik==0.and.ih==0).and.(ikl==0.and.ihl==0))) then
       write(io,'(2x,''Unit control parameter '',a15,'' is not followed
     *by +K or +H'')') su
       write(io,'(7x,''or +k or +h'')')
       write(6,'(2x,''Unit control parameter '',a15,'' is not followed
     *by +K or +H'')') su
       write(6,'(7x,''or +k or +h'')')
       stop
      endif
c-------
      if(iunit>0) then
c  Lower frequency
       par='wer freq'
       fl=rnumread(par,name,delim,ia,ier,io)
       if(ier==1) then
        write(io,'(/'' Type lower frequency rather than longest period f
     *or the passband in %.cnl'')')
        write(6,'(/'' Type lower frequency rather than longest period fo
     *r the passband in %.cnl'')')
        stop
       endif
c-------
c  Upper frequency
       par='per freq'
       fu=rnumread(par,name,delim,ia,ier,io)
       if(ier==1) then
        write(io,'(/'' Type upper frequency rather than shortest period
     *for the passband in %.cnl'')')
        write(6,'(/'' Type upper frequency rather than shortest period f
     *or the passband in %.cnl'')')
        stop
       endif
        else
c  Longest period
         par='gest period'
         fl=rnumread(par,name,delim,ia,ier,io)
         if(ier==1) then
          write(io,'(/'' Type longest period rather than lower frequency
     * for the passband in %.cnl'')')
          write(6,'(/'' Type longest period rather than lower frequency
     *for the passband in %.cnl'')')
          stop
         endif
c  Shortest period
         par='test period'
         fu=rnumread(par,name,delim,ia,ier,io)
         if(ier==1) then
          write(io,'(/'' Type shortest period rather than upper frequenc
     *y for the passband in %.cnl'')')
          write(6,'(/'' Type shortest period rather than upper frequency
     * for the passband in %.cnl'')')
          stop
         endif
      endif
c  Resolution bandwidth
      par='idth'
      rb=rnumread(par,name,delim,ia,ier,io)
      if(ier==4) then
       write(io,*) 'Place a period in the resolution bandwidth value'
       ia=index(name,'idth')
       write(io,*) name(ia+5:ia+15)
       stop
      endif
      if(rb<=0.) then
       write(io,*) 'Resolution bandwidth < 0'
       stop
      endif
      p=0
c  Prewhiten by an AR(p) fit
      ia=index(name,'rewhite')
      if(ia>0) then
       ib=index(name(ia:ia+20),'AR')
       ic=index(name(ia:ia+20),'ar')
       if(ib>0) then
        par='R'
        delim(1:2)='()'
        p=numread(par,name,delim,ia,ier,io)
         elseif(ic>0) then
          par='r'
          delim(1:2)='()'
          p=numread(par,name,delim,ia,ier,io)
       endif
      endif
      delim(1:2)='= '
      if(p>0) then
c  Prob threshold for AR t values
       par='hreshold'
       pv=rnumread(par,name,delim,ia,ier,io)
c  Trap pv
       if(pv<=0.) then
        write(io,*) 'Prob level pv ',pv,' < 0'
        write(io,*) 'Set pv = 0 if you do not want to prewhiten'
        stop
       endif
       if(pv>1) then
        write(io,*) 'Prob level pv ',pv,' > 1'
        stop
       endif
      endif
c  % taper
      par='taper'
      pt=rnumread(par,name,delim,ia,ier,io)
      if(ier>0) then
       write(6,'(/2x,''Error in reading the taper %''/)')
       write(9,'(/2x,''Error in reading the taper %''/)')
       stop
      endif
c-------
      if(iunit==0) then
c  Trap fl=0 for low band period
       if(fl<=0.) then
        fl=1.e6
       endif
c  Invert period to frequency
       fl=1./fl
       if(fu>0.) then
        fu=1./fu
         else
          fu=0.5
       endif
      endif
c-------
      if(fu>0. .and. fl>fu) then
       write(io,*)'Upper frequency of band ',fu,' < lower frequency ',fl
       if(iunit==1) then
        write(io,*) 'Check to see the (fl,fu) units are kHz'
         elseif(iunit==2) then
          write(io,*) 'Check to see the (fl,fu) units are Hz'
           else
            write(io,*) 'Check to see the (fl,fu) units are ',su
       endif
       stop
      endif
c-------
c  Trap taper %
      if(pt<0.or.pt>25.) then
       write(io,*) 'Taper % not in range (0,25) ',pt
       stop
      endif
c  File read start
      par='start'
      is=numread(par,name,delim,ia,ier,io)
      if(ier>0) then
       write(io,*) 'Error ',ier,' in read of start datum'
       write(io,*) par
       stop
      endif
c  File read end
      par='end'
      ie=numread(par,name,delim,ia,ier,io)
      if(ier>0) then
       write(io,*) 'Error ',ier,' in read of end datum'
       write(io,*) par
       stop
      endif
      if(is<1) then
       write(io,*) 'IS in CNL file is < 1 ',is
       stop
        elseif(ie/=0.and.ie<is) then
         write(io,*) 'IE < IS in CNL file ',is,ie
          stop
      endif
c-------
c  Resamples
      res=0
      par='sample'
      res=numread(par,name,delim,ia,ier,io)
      if(ier>0) then
       write(io,*) 'Error ',ier,' in read of resamples'
       write(io,*) par
       write(6,*) 'Error ',ier,' in read of resamples'
       write(6,*) par
       write(9,*) 'Error ',ier,' in read of resamples'
       write(9,*) par
       stop
      endif
c  Trap res
      if(res>0.and.res<11) then
       write(io,'(/'' The bootrsap is turned off if the number of resamp
     *les is less than 11''/,'' Resamples = '',i2)') res
       write(6,'(/'' The bootrsap is turned off if the number of resampl
     *es is less than 11''/,'' Resamples = '',i2)') res
       write(9,'(/'' The bootrsap is turned off if the number of resampl
     *es is less than 11''/,'' Resamples = '',i2)') res
       res=0
      endif
c  Quantile
      qv=.9
      par='ntile'
      qv=rnumread(par,name,delim,ia,ier,io)
      if(ier>0) then
       write(io,*) 'Error ',ier,' in read of quantile'
       stop
      endif
c-------
c  Read data file header
      call readhead(id,nfl,nc,sr,fmt,iunit,2,io)
      write(io,'(/7x,''Column Number Used = '',i3,2x,''No. of Columns =
     *'',i3)')kcol,nc
c--------
c  Trap top band
      if(fu>sr/2.) then
       write(io,*) 'Upper frequency = ',fu,' > Nyquist freq. ',sr/2.
       write(6,*) 'Upper frequency = ',fu,' > Nyquist freq. ',sr/2.
       write(9,*) 'Upper frequency = ',fu,' > Nyquist freq. ',sr/2.
       if(iunit==1) then
        write(io,*) 'Check to see the (fl,fu) units are kHz'
        write(6,*) 'Check to see the (fl,fu) units are kHz'
        write(9,*) 'Check to see the (fl,fu) units are kHz'
         elseif(iunit==2) then
          write(io,*) 'Check to see the (fl,fu) units are Hz'
          write(6,*) 'Check to see the (fl,fu) units are Hz'
          write(9,*) 'Check to see the (fl,fu) units are Hz'
           else
            write(io,*) 'Check to see the (fl,fu) units are ',su
            write(6,*) 'Check to see the (fl,fu) units are ',su
            write(9,*) 'Check to see the (fl,fu) units are ',su
       endif
       write(io,*) ' Sampling unit ',su,' Sampling rate/interval ',sr
       write(io,*) 'iunit = ',iunit
       write(6,*) ' Sampling unit ',su,' Sampling rate/interval ',sr
       write(6,*) 'iunit = ',iunit
       write(9,*) ' Sampling unit ',su,' Sampling rate/interval ',sr
       write(9,*) 'iunit = ',iunit
       stop
      endif
c  Convert to normalized values (0,.5)
      if(iunit>0) then
       gl=fl/sr
       if(fu>0.) then
        gu=fu/sr
         else
          gu=0.5
       endif
        else
         gl=fl*sr
         gu=fu*sr
      endif
c-------
c  Parse format to see if there is a 'a' for a time mark character string
      ia=index(fmt,'a')
      if(ia>0) then
c  Change a## to ##x                                   ,
       ia1=ia+1
       icom=index(fmt(ia1:),',')
       ipar=index(fmt(ia1:),')')
       if(icom>0) then
        iac=ia+icom-1
        str(1:icom)=fmt(ia1:ia+icom)
        fmt(ia:iac)=str(1:icom)
        fmt(iac:iac)='x'
         elseif(ipar>0) then
          iac=ia+ipar-1
          str(1:ipar)=fmt(ia1:ia+ipar)
          fmt(ia:iac)=str(1:ipar)
          fmt(iac:iac)='x'
       endif
      endif
c-------
c  Trap file length
      if(ie>nfl) then
       write(io,*) 'IE ',ie, '> N in data file',nfl
       stop
        elseif(ie==0) then
         ie=nfl
      endif
      n=ie-is+1
c  Find frame length lb
      if(iunit==1) then
c  kHz
       lb=nint(sr*1.e3/rb)
        elseif(iunit==2) then
c  Hz
         lb=nint(sr/rb)
          else
           lb=nint(rb/sr)
      endif
c-------
      if(lb<=9) then
       write(io,*) 'Frame length = ',lb,' < 10'
       write(io,*) 'Decrease resolution bandwidth = ',rb
       stop
c  Trap n<lb
        elseif(n/lb<4) then
         write(io,*) 'Frame length > n/4'
         write(io,*) 'Increase resolution bandwidth = ',rb
         stop
      endif
c-------
c  Number of values tapered from each end
      nt=nint(pt*float(lb)/200.)
c  If no. of tapered values at ends < 2 set pt=0
      if(nt<2) then
       pt=0.
      endif
      rlb=dfloat(lb)
      n2=lb/2
c  Set il - lower band integer limit
      il=nint(sngl(dble(gl)*rlb))
      il=max(il,1)
c  Set iu - upper band integer limit
      if(fu>0.) then
       iu=nint(sngl(dble(gu)*rlb))
       iu=min(iu,n2)
        else
         iu=n2
      endif
      if(il==iu) then
       write(io,*) 'fu-fl is too small. Increase fu-fl'
       write(9,*) 'fu-fl is too small. Increase fu-fl'
       stop
        elseif(il==iu .and. iu==n2) then
         write(io,*)'fl ',fl,'=',fu,' fu: ERROR in band settings in cnl'
         write(9,*)'fl ',fl,'=',fu,' fu: ERROR in band settings in cnl'
        stop
      endif
c-------
c  No. of frequencies used
      jf=iu-il+1
      e=alog(float(lb))/alog(float(n))
      if(e<=0.01.or.e>=.99) then
       write(io,*) 'e = ',e,' is out of bound'
       stop
        elseif(e>=0.50) then
         write(io,'(/2x,''Condition for Consistency of Estimates is Viol
     *ated - e ='',f7.4/)') e
         write(io,'(7x,''Resolution Bandwidth ='',f14.5)') rb
      endif
      call array(is,ie,lb,res,sr,pt,il,jf,pv,qv,e,su,id,fmt,p,iunit,rb,k
     *col,nc,io)
c  Start time
c+++++++++++++++++++++++++++++++++++++++++++
c  Time and date of run & run time
      call head(mt,ndy,ny,gs,rt,3)
c+++++++++++++++++++++++++++++++++++++++++++
      return
c---------
  1   write(io,*) '**** Error on opening data file ',io1
      write(io,*) fmt
      return
  2   write(io,*) '**** Error on opening ',fname,' ',io2
      return
  3   write(io,*) '**** Error on opening ',fname,' ',io2
      return
      end
c
c===========================================
c
      subroutine array(is,ie,lb,res,sr,pt,il,jf,pv,qv,e,su,id,fmt,p,iuni
     *t,rb,kcol,nc,io)
c**********************************************************************
c  Sets arrays
c======================================================================
      allocatable::a,ao,c,r,rt,f,v
      real a(:)
      real*8 c(:),v(:)
      complex ao(:),rt(:)
      complex*16 f(:)
      integer r(:),ot,p,res
      character*80 id
      character*50 fmt
      character*10 su
      intent(in) is,ie,lb,res,sr,pt,il,jf,pv,qv,e,su,id,fmt,p,iunit,rb,k
     *col,nc,io
c****************************
      n=ie-is+1
      iu=il+jf-1
c  Get array lengths for bispec
      call size(il,iu,lb,nb,ot)
c  Trap number of bifrequncies in PD
      if(nb<=5) then
       write(io,*) 'Number of bifrequencies to be computed ',nb,' < 5'
       write(io,*) 'Increase the passband'
       stop
      endif
c  Pointers for x,r,sp,br,bi,w,u
      i2=n*nc
      i3=i2+n
      i4=i3+jf
      i5=i4+nb
      i6=i5+nb
      i7=i6+lb+lb
      i8=i7+iu*(iu/2)
      ia=i8+p
c  Pointers for b1,b2,ik
      j2=nb
      j3=j2+nb
      ja=j3+max(nb,p)
c  Allocate space
      allocate(a(0:ia),ao(0:p),c(0:p-1),r(0:ja),rt(0:p-1),v(0:4*lb+14),f
     *(0:lb-1),stat=ibad)
      if(ibad/=0) then
       write(io,*) 'Unable to allocate work space for arrays'
       stop
      endif
c-------
      call run(a(0),a(i2),a(i3),a(i4),a(i5),a(i6),a(i7),a(i8),r(0),r(j2)
     *,r(j3),ao,c,rt,f,v,is,ie,n,lb,nb,ot,il,iu,sr,pt,e,res,pv,qv,su,id,
     *fmt,p,iunit,rb,kcol,nc,io)
      deallocate(a,ao,c,r,rt,f,v)
      return
      end
c
c===========================================
c
      subroutine run(x,r,sp,br,bi,w,u,s,b1,b2,ik,ao,c,rt,f,v,is,ie,n,lb,
     *nb,ot,il,iu,sr,pt,e,res,pv,qv,su,id,fmt,p,iunit,rb,kcol,nc,io)
c**********************************************************************
c  Input: x - data array, su - sampling unit, sr - sampling rate
c   fl - bin no. of lower freq, fu - bin no. of upper freq of passband
c   lb - bandwidth is 1/lb, s & b - spec output switches, res - resamples
c   pt - % taper, nb - no. of bispec values, pv - p value threshold
c======================================================================
      integer p
      real x(n,nc),r(n),sp(iu-il+1),br(nb),bi(nb),w(2*lb),u(iu,iu/2),s(p
     *),ft(1),f0(1),frv(1),fal(1)
      real*8 c(p),v(4*lb+15)
      complex ao(p+1),rt(p)
      complex*16 f(0:lb-1)
      integer b1(nb),b2(nb),ik(max(nb,p)),ot,po,res,t
      character*80 id
      character*50 fmt
      character*10 su
      intent(in)ie,is,n,lb,nb,ot,il,iu,sr,pt,e,res,pv,qv,su,id,fmt,p,iun
     *it,rb,kcol,nc,io
c****************************
c  Initialize v for lb
      call cffti(lb,v)
      jf=iu-il+1
c  Write header for summary stats in file 3
      write(io,'(67(''*''))')
      call wr2(id,su,sr,e,n,lb,pt,qv,is,ie,il,iu,res,rb,iunit,io)
      write(io,'(17x,''Format: '',a50)') fmt
c-------
      call dat(x,fmt,is,ie,n,nc,2,io)
c  Compute statistics for kcol data
      call statd(n,x(1,kcol),ax,sg,c3,c4,c6,smax,smin,io)
      write(io,'(60(''=''))')
      write(io,'(10x,''Descriptive Statistics of Data'')')
      call wrstat(io,ax,sg,c3,c4,c6,smax,smin)
      po=0
      if(p>0) then
c  Least squares AR(p) - residuals in array r, coefficients in c
       call far(x(1,kcol),c,s,r,ik,r2,sig,n,p,po,pv,ier,io)
       if(po>0) then
c  Compute statistics of n-po residuals
        ip=po+1
        call statd(n-po,r(ip),am,sd,c3,c4,c6,smax,smin,io)
c  Standardize residuals from far
        do t=ip,n
         r(t)=(r(t)-am)/sd
        enddo
c  White test
        nlg=nint(float(n)**e)
        if(nlg>(n/10)) then
         exp=0.25
          else
           exp=0.4
        endif
        call port(r,n,exp,ct,io)
        write(io,'(60(''=''))')
        write(io,'(10x,''Descriptive Statistics of Residuals'')')
        call wrstat(io,am,sd,c3,c4,c6,smax,smin)
        write(io,'(/7x,''Pure Noise Test Statistic p-value = '',f7.3/)')c
     *t
        write(io,'(60(''*''))')
        call outar(c,s,ao,rt,ik,r2,sig,su,sr,pv,p,po,iunit,io)
c  Compute spectrum and bispectrum of r
        call b23(r,br,bi,b1,b2,ik,w,sp,f,v,n,lb,il,jf,nb,ot,pt,c3,bt,b0,
     *brv,bal,po,qv,alm,ier,io)
        if(ier>0) then
         write(6,'(/7x,''Error in call to the b23 in run''/)')
         write(9,'(/7x,''Error in call to the b23 in run''/)')
         return
        endif
        if(res>0) then
c  Resample r where v initialized for fft with lb
         call resamp(r,br,bi,b1,b2,ik,w,sp,f,v,n,lb,il,jf,nb,ot,p,res,pt
     *,c3,qv,alm,bt,b0,brv,bal,ft,f0,frv,fal,io)
        endif
       endif
       if(po==0) then
        write(io,'(/7x,''The AR fit has insignificant coefficents for th
     *e test threshold '',f7.3/)')pv
        write(6,'(/7x,''The AR fit has insignificant coefficents for the
     * test threshold '',f7.3/)')pv
        write(9,'(/7x,''The AR fit has insignificant coefficents for the
     * test threshold '',f7.3/)')pv
        return
       endif
      endif
c  End of branch on po in AR fit
      if(p==0) then
c  Normalize x(1,kcol) =>r
       do t=1,n
        r(t)=(x(t,kcol)-ax)/sg
       enddo
c  Compute spectrum and bispectrum of r
       call b23(r,br,bi,b1,b2,ik,w,sp,f,v,n,lb,il,jf,nb,ot,pt,c3,bt,b0,b
     *rv,bal,p,qv,alm,ier,io)
       if(ier>0) then
        write(6,'(/7x,''Error in call to the b23 in run''/)')
        write(9,'(/7x,''Error in call to the b23 in run''/)')
        return
       endif
       if(res>0) then
        call resamp(r,br,bi,b1,b2,ik,w,sp,f,v,n,lb,il,jf,nb,ot,p,res,pt,
     *c3,qv,alm,bt,b0,brv,bal,ft,f0,frv,fal,io)
       endif
      endif
      write(io,'(67(''#'')/)')
      it=nb-ot
      call outbisn(b1,b2,br,bi,ik,it,ot,su,rb,alm,bt,b0,brv,bal,res,ft,f
     *0,frv,fal,iunit,io)
      call wrtbisn(b1,b2,br,u,iu,it,rb,iunit,7)
      call flush(io)
      call flush(7)
c-------
c  Output spectrum if s & b are set
      call wrtspec(sp,il,jf,su,rb,iunit,4)
c  Write large sample se
      cb=10./(alog(10.)*sqrt(float(n/lb)))
      write(4,'(/'' Large Sample Standard Error ='',g9.4)') cb
c-------
      return
      end
c
c===========================================
c
      subroutine resamp(x,br,bi,b1,b2,ik,w,sp,f,v,n,lb,il,jf,nb,ot,p,res
     *,pt,c3,qv,alm,bt,b0,brv,bal,ft,f0,frv,fal,io)
c********************************************
c  Compute statistics for shuffled data for resamples=res
c  v - fft initialization for lb
c  ft - nonlinearity test, f0 - zero bispectrum, frv - time reversibility
c  fal - aliasing thresholds
c  bt- CDF value for the qv nonlinearity test, b0 - CDF value for the zero
c  bispectrum test, brv - CDF value for the time reversibility test
c  bal - aliasing test CDf
c============================================
      allocatable::at,a0,ar,aa,h,y
      real x(n),br(nb),bi(nb),w(2*lb),sp(jf),at(:),a0(:),ar(:),aa(:),y(:
     *),fi(1),ft(1),f0(1),frv(1),fal(1)
      integer h(:),b1(nb),b2(nb),ik(nb),ot,p,res
      real*8 v(4*lb+15)
      complex*16 f(0:lb-1)
      intent(in) x,v,n,lb,il,jf,nb,ot,p,res,pt,c3,qv,bt,b0,brv,bal,io
      intent(out) alm,ft,f0,frv,fal
c********************************************
c  Allocate space
      ns=res-1
      nh=n-1
      allocate(h(0:nh),y(0:nh),at(0:ns),a0(0:ns),ar(0:ns),aa(0:ns),stat=
     *ibad)
      if(ibad/=0) then
       write(io,*) 'Unable to allocate work space for arrays in resamp'
       stop
      endif
c-------
      do kr=1,res
       k=kr-1
       call shuf(x,y,h,n,io)
c  Compute spectrum and bispectrum of y
       call b23(y,br,bi,b1,b2,ik,w,sp,f,v,n,lb,il,jf,nb,ot,pt,c3,rt,r0,r
     *rv,ral,p,qv,alm,ier,io)
       if(ier>0) then
        return
       endif
c  Sort br
       it=nb-ot
c  Nonlinear test quantile qv p-value (1-prob)
       at(k)=1.-rt
c  Gaussianity test p-value
       a0(k)=1.-r0
c  Time reversibility p-value
       ar(k)=1.-rrv
c  Aliasing test p-value
       aa(k)=1.-ral
c  End of resample loop
      enddo
c  Threshold for qv quantile linearity test p-values
      fi(1)=1.-bt
      call sort(h,at,res,1,fi,ft,'B',io)
c  Threshold for gaussian test p-values
      fi(1)=1.-b0
      call sort(h,a0,res,1,fi,f0,'B',io)
c  Threshold for time reversibility test p-values
      fi(1)=1.-brv
      call sort(h,ar,res,1,fi,frv,'B',io)
c  Threshold for aliasing test p-values
      fi(1)=1.-bal
      call sort(h,aa,res,1,fi,fal,'B',io)
c-------
      deallocate(at,a0,ar,aa,h,y)
      return
      end
c
c===========================================
c
      subroutine shuf(y,w,hold,n,io)
c********************************************
c  Shuffled y signal in w using the uniform generator
c============================================
      real y(n),w(n)
      integer hold(n),t
      intent(in) y,n
      intent(out) w
c********************************************
c  Uniforms
      call random_number(w)
      do t=1,n
       hold(t)=t
      enddo
      do t=1,n
       do k=t+1,n
        if(w(t)>w(k)) then
         store=w(t)
         w(t)=w(k)
         w(k)=store
         ih=hold(t)
         hold(t)=hold(k)
         hold(k)=ih
        endif
       enddo
       w(t)=y(hold(t))
      enddo
c-------
      return
      end
c
c===========================================
c
      subroutine b23(x,br,bi,b1,b2,ik,w,sp,f,v,n,lb,il,jf,nb,ot,pt,c3,bt
     *,b0,brv,bal,p,qv,alm,ier,io)
c********************************************
c  M. J. Hinich 12-30-2004
c  Input: x - data window, n - sample size, lb - block size
c  pt - taper, il - low freq bin of passband, iu - high freq bin of passband
c  v - fft initialization
c  Output: Phases and amplitudes of bispectrum (br,bi)
c  Counters b1,b2 for bispectrum, alm - average noncentrality lamda
c  bt- CDF value for the qv nonlinearity test, b0 - CDF value for the zero
c  bispectrum test, brv - CDF value for the time reversibility test
c  bal - aliasing test CDf
c============================================================
      real x(n),sp(jf),br(nb),bi(nb),w(2*lb),fi(1),fo(1)
      real*8 v(4*lb+15)
      complex*16 f(0:lb-1)
      integer b1(nb),b2(nb),ik(nb),bn,ot,p
      intent(in) x,v,n,lb,il,jf,nb,ot,pt,c3,p,qv,io
      intent(out) alm,br,bi,b1,b2,ier,sp,bt,b0,brv,bal
c********************************************
c  Initialize taper
      nt=0
      if(pt>0.) then
       call taper(w,lb,pt,sw,nt,io)
c  If no. of tapered values at ends < 3 set sw=1
       if(nt<3) then
        sw=1.
       endif
        else
         sw=1.
      endif
c-------
c  Initialize arrays
      do k=1,jf
       sp(k)=0.
      enddo
      do k=1,nb
       br(k)=0.
       bi(k)=0.
      enddo
      bn=n/lb
c  Start loop over frames
      do kb=1,bn
       kp=(kb-1)*lb+1
       if(nt>=3.) then
c  Taper frame kb
        do k=1,lb
         ka=kp-1+k
         w(lb+k)=w(k)*x(ka)
        enddo
         else
c  Shift to w
          do k=1,lb
           ka=kp-1+k
           w(lb+k)=x(ka)
          enddo
       endif
       ip=lb+1
       call p23(w(ip),br,bi,b1,b2,sp,f,v,bn,lb,nb,ot,il,jf,kb,nt,sw,io)
      enddo
c  End of loop on frames
      cm=2.*float(n)/float(lb*lb)*c3*c3
      call sc(br,bi,b1,b2,sp,bn,lb,il,jf,nb,ot,b0,brv,bal,cm,p,alm,ier,i
     *o)
      it=nb-ot
      fi(1)=qv
      call sort(ik,br,it,1,fi,fo,'B',io)
c  Z value for qv statistic
      zv=(fo(1)-qv)/sqrt(qv*(1.-qv)/float(it))
c  CDF value of nonlinearity test statistic
      bt=cdg(zv)
c-------
      return
      end
c
c===========================================
c
      subroutine p23(x,br,bi,b1,b2,sp,f,v,bn,lb,ib,ot,il,jf,kb,nt,sw,io)
c********************************************************
c  Computes products for periodogram & bispectrum
c   b1, b2 - IT freq values, ib - actual size of br & bi arrays for IT & OT
c   bn - no. of frames, kb - block number, it - size of IT array
c========================================================
      real x(lb),sp(jf),br(ib),bi(ib)
      real*8 v(4*lb+15),dlb
      complex*16 f(0:lb-1),cs2,cs3
      integer b1(ib),b2(ib),bn,ot
      intent(in) x,v,bn,lb,ib,ot,il,jf,kb,nt,sw,io
      intent(out) b1,b2
      intent(in out) br,bi,sp
c********************************************
      iu=il+jf-1
      sq2=sqrt(2.)
      bk=float(bn)
c  Scale factor
      dlb=dfloat(lb)
      cs2=dcmplx(dlb)
      cs3=dcmplx(dsqrt(dlb)*dlb)
c-------
c  OT settings
      n2=lb/2
      n4=n2/2
      if(4*n4<lb) then
       n4=n4+1
      endif
      if(n4<il.or.n4>iu) then
c  n4 is not in the band
       n4=lb
      endif
      ks=il-1
c-------
c  Move x block to complex f
      do j=1,lb
       f(j-1)=dcmplx(dble(x(j)))
      enddo
c  FFT of block
      call cfftf(lb,f,v)
c  Initialize bispec counters
      nb=0
      not=0
c  Periodogram fl<k1=k2<=fu
      do k=il,iu
       jk=k-ks
       sp(jk)=sp(jk)+dreal(f(k)/cs2*f(lb-k))
       if(kb==bn) then
        sp(jk)=sp(jk)/bk
        if(nt>3) then
         sp(jk)=sp(jk)/sw
        endif
        if(sp(jk)<1.e-6) then
         sp(jk)=1.e-6
        endif
       endif
      enddo
c-------
c  Biperiodogram for k1=k2=il
      if(2*il<=iu) then
       nb=nb+1
       br(nb)=br(nb)+dreal(f(il)/cs3*f(il)*f(lb-2*il))
       bi(nb)=bi(nb)+dimag(f(il)/cs3*f(il)*f(lb-2*il))
c  Set indices
       if(kb==bn) then
        b1(nb)=il
        b2(nb)=il
        br(nb)=br(nb)/(sq2*bk)
        bi(nb)=bi(nb)/(sq2*bk)
       endif
      endif
c--------
      do k1=il+1,iu
       do k2=il,k1-1
c  Biperiodogram for IT
        ks=k1+k2
        if(ks<=iu) then
         nb=nb+1
         br(nb)=br(nb)+dreal(f(k1)/cs3*f(k2)*f(lb-ks))
         bi(nb)=bi(nb)+dimag(f(k1)/cs3*f(k2)*f(lb-ks))
c  Set indices
         if(kb==bn) then
          b1(nb)=k1
          b2(nb)=k2
          br(nb)=br(nb)/bk
          bi(nb)=bi(nb)/bk
         endif
        endif
c  Biperiodogram for OT
        if(k1>n4.and.k1<(lb-ks).and.(lb-ks)<=iu) then
         not=not+1
         kt=ib-ot+not
         br(kt)=br(kt)+dreal(f(k1)/cs3*f(k2)*f(lb-ks))
         bi(kt)=bi(kt)+dimag(f(k1)/cs3*f(k2)*f(lb-ks))
c  Set indices
         if(kb==bn) then
          b1(kt)=k1
          b2(kt)=k2
          br(kt)=br(kt)/bk
          bi(kt)=bi(kt)/bk
         endif
        endif
       enddo
c  Biperiodogram for k1=k2 < iu
       if(2*k1<=iu) then
        nb=nb+1
        br(nb)=br(nb)+dreal(f(k1)/cs3*f(k1)*f(lb-2*k1))
        bi(nb)=bi(nb)+dimag(f(k1)/cs3*f(k1)*f(lb-2*k1))
c  Set indices
        if(kb==bn) then
         b1(nb)=k1
         b2(nb)=k1
         br(nb)=br(nb)/(sq2*bk)
         bi(nb)=bi(nb)/(sq2*bk)
        endif
       endif
      enddo
c-------
      return
      end
c
c=======================
c
      subroutine sc(br,bi,b1,b2,sp,bn,lb,il,jf,nb,ot,b0,brv,bal,cm,p,alm
     *,ier,io)
c********************************************************
c  Computes bispectrum. 8-4-95
c   b0 - CDF value for the zero bispectrum null
c   brv - CDF value for the zero imaginary part, bal - CDF aliasing test
c   alm - average noncentrality lamda for bispectrum
c========================================================
      real sp(jf),br(nb),bi(nb)
      integer b1(nb),b2(nb),bn,it,ot,p
      intent(in) b1,b2,sp,bn,lb,il,jf,nb,ot,cm,p,io
      intent(out) alm,ier,b0,brv,bal
      intent(in out) br,bi
c********************************************
      bk2=float(bn)*2.
      ks=il-1
c  Initialize count for spec=0 values
      nb0=0
      not0=0
      a1=0.
      a2=0.
      aot=0.
      rlb=float(lb)
      it=nb-ot
      do k=1,it
       h=sp(b1(k)-ks)*sp(b2(k)-ks)*sp(b1(k)+b2(k)-ks)
       h=sqrt(h/bk2)
       if(h>1.e-25) then
        br(k)=br(k)/h
        bi(k)=bi(k)/h
c  Chi*2 & phase
        ar=br(k)*br(k)
        ai=bi(k)*bi(k)
        a1=a1+ar
        a2=a2+ai
        bi(k)=atan2(bi(k),br(k))*57.2958
        br(k)=ar+ai
         else
          nb0=nb0+1
       endif
      enddo
c  Test for aliasing
      do k=it+1,nb
       h=sp(b1(k)-ks)*sp(b2(k)-ks)*sp(lb-b1(k)-b2(k)-ks)
       h=sqrt(h/bk2)
       if(h>1.e-25) then
        br(k)=br(k)/h
        bi(k)=bi(k)/h
        aot=aot+br(k)*br(k)+bi(k)*bi(k)
         else
          not0=not0+1
       endif
      enddo
c-------
c  No. of terms with non-zero spectral products
      ndb=it-nb0
      rdb=float(ndb)
      ier=0
      if(ndb<12) then
       write(io,'(60(''*'')/)')
       write(io,'('' Valid bispec values = '',i1,'' < 12'')') ndb
       write(7,'(60(''*'')/)')
       write(7,'('' Valid bispec values = '',i1,'' < 12'')') ndb
       ier=1
       return
      endif
c  Noncentrality lamda
      if(p>0) then
       alm=cm
        elseif(p==0) then
         alm=max((a1+a2)/rdb-2.,0.)
          else
           write(io,*) 'Value of p',p,' is incorrect'
           stop
      endif
c  Uniform variates under linear null hypothesis
      do k=1,it
       br(k)=p2chi(br(k),alm,io)
      enddo
c  CDF statistic U(0,1) of normalized a1+a2 - gaussianity test
      call cdchi(a1+a2,2.*rdb,b0,io)
c  CDF statistic U(0,1) of normalized a2 - time reversibility test
      call cdchi(a2,rdb,brv,io)
c  CDF statistic U(0,1) of normalized OT bispecs - aliasing test
      if((ot-not0)>24) then
       call cdchi(aot,2.*float(ot-not0),bal,io)
        else
         write(io,'(60(''*'')/)')
         write(io,'('' Valid OT bispec values = '',i2,'' < 25''/)') ot-n
     *ot0
         bal=0.
      endif
c-------
      return
      end
c
c===========================================
c
      subroutine dat(x,fmt,is,ie,n,nc,ii,io)
c********************************************************
c  Reads n data values from nc columns in x
c========================================================
      real x(n,nc)
      character*50 fmt
      intent(in) fmt,is,ie,n,nc,ii,io
      intent(out) x
c********************************************
      rewind(ii)
c  Skip header and parameter records
      read(ii,*)
      read(ii,*)
      if(is==1) then
       do i=1,ie
        read(ii,fmt,end=1,err=2,iostat=ko)(x(i,k),k=1,nc)
       enddo
      endif
      if(is>1) then
       do i=1,is-1
        read(ii,fmt,end=1,err=2,iostat=ko)
       enddo
       do i=is,ie
        read(ii,fmt,end=1,err=2,iostat=ko)(x(i-is+1,k),k=1,nc)
       enddo
      endif
      return
c--------
  1   write(io,*) '**** eof in read from data file '
      write(io,*) 'i = ',i
      return
      write(6,*) '**** eof in read from data file '
      write(6,*) 'i = ',i
      write(9,*) '**** eof in read from data file '
      write(9,*) 'i = ',i
c  Error in data read
  2   write(io,*) '** Error in read of data file ',ko
      write(io,*) 'Check format',fmt
      write(6,*) '** Error in read of data file ',ko
      write(6,*) 'Check format',fmt
      write(9,*) '** Error in read of data file ',ko
      write(9,*) 'Check format',fmt
      stop
      end
c
c===========================================
c
      subroutine wr2(id,su,sr,e,n,lb,pt,qv,is,ie,il,iu,res,rb,iunit,io)
c********************************************************
c  Writes run information for main output in output files
c========================================================
      real pt,qv
      integer res
      character*79 id
      character*10 su
      intent(in) id,su,sr,e,n,lb,pt,qv,is,ie,il,iu,res,rb,iunit,io
c*****************************************
      jf=iu-il+1
      rl=float(lb)
      write(io,'(1x,a79)') id
      write(io,'(/7x,''First datum read'',i7,2x,''Last datum read'',i8)'
     *) is,ie
      write(io,'(/'' Sample size = '',i8,2x,''No. of frequencies in band
     * '',i6,2x,''No. frames '',i4/)') n,jf,n/lb
c-------
      if(iunit==1) then
c  Lower and upper frequencies for band
       a=float(il)*rb*1.e-3
       b=float(iu)*rb*1.e-3
       write(io,'('' Sampling rate ='',f12.4,'' kHz '',4x,''Frame size '
     *',i5/)') sr,lb
       write(io,'('' Resolution Bandwidth :'',g14.4,'' Hz'',g14.4,'' mse
     *c''/)') rb,1./rb
       write(io,'('' Passband ('',f9.4,2x,f10.4,'' ) kHz''/)') a,b
      endif
      if(iunit==2) then
c  Lower and upper frequencies for band
       a=float(il)*rb
       b=float(iu)*rb
       write(io,'('' Sampling rate ='',f12.4,'' Hz '',4x,''Frame size ''
     *,i5/)') sr,lb
       write(io,'('' Resolution Bandwidth :'',g14.4,'' Hz'',g14.4,'' mse
     *c''/)') rb,1./rb
       write(io,'('' Passband ('',f9.4,2x,f10.4,'' ) Hz''/)') a,b
      endif
      if(iunit==0) then
c  Time unit
         a=rb/float(il)
         b=rb/float(iu)
         write(io,'('' Sampling interval = '',f12.4,1x,a10,2x,''Frame si
     *ze '',i5/)')sr,su,lb
         write(io,'('' Resolution Bandwidth :'',g14.4,1x,a10/)') rb,su
         write(io,'('' Passband ('',f10.2,2x,f7.4,'' )'',1x,a10/)') a,b,
     *su
      endif
c-------
c  Number of values tapered from each end
      nt=nint(pt*rl/200.)
c  If no. of tapered values at ends < 2 set nt=0
      if(nt<2) then
       nt=0
      endif
      if(pt>0. .and. nt>0) then
       write(io,'('' % taper = '',f5.2,4x,''No. tapered at each end = ''
     *,i5/)') pt,nt
      endif
      if(nt==0) then
       write(io,'('' % Taper yields no. of tapered values < 2''/)')
      endif
      if(res>0) then
       write(io,'(4x,''Residuals are Bootstrapped using '',i5,'' resampl
     *es''/)') res
       write(io,'(7x,''Nonlinearity Test Computed for the '',f5.2,'' Qua
     *ntile''/)') qv
      endif
      write(io,'(7x,''Bandwidth Frame Exponent = '',f5.2/)') e
c-------
      return
      end
c
c===================================================
c
      subroutine wrstat(i,am,sig,sk,c4,c6,smax,smin)
c****************************************************
c  Data statistics
c============================================
      write(i,'(60(''='')/)')
      write(i,'('' Mean ='',g12.3,7x,''Std Dev ='',g12.3/)') am,sig
      write(i,'('' Skew ='',g10.3,4x,''Kurtosis ='',g10.3,4x,''C(6) ='',
     *f10.3/)') sk,c4,c6
      write(i,'('' Max value ='',g12.3,7x,''Min value ='',g12.3)') smax,
     *smin
      return
      end
c
c=============================================
c
      subroutine wrtspec(sp,il,jf,su,rb,iunit,io)
c*******************************************
c  Output spectrum 7-22-2001
c===========================================
      real sp(jf)
      character*10 su
      intent(in) sp,il,jf,su,rb,iunit,io
c********************************************
      ks=il-1
      if(iunit==1) then
       write(io,'(2x,''Frequency (kHz)'',2x,''Log Spectrum (dB)''/)')
       do k=1,jf
        tk=float(ks+k)*rb*1.e-3
        if(tk>1.e-2) then
         write(io,'(f14.3,4x,g10.4)') tk,10.*alog10(sp(k))
          else
           write(io,'(g14.3,4x,g10.4)')
     *tk,10.*alog10(sp(k))
        endif
       enddo
      endif
      if(iunit==2) then
       write(io,'(2x,''Frequency (Hz)'',2x,''Log Spectrum (dB)''/)')
       do k=1,jf
        tk=float(ks+k)*rb
        if(tk>1.e-2) then
         write(io,'(f14.3,4x,g10.4)') tk,10.*alog10(sp(k))
          else
           write(io,'(g14.3,4x,g10.4)')
     *tk,10.*alog10(sp(k))
        endif
       enddo
      endif
      if(iunit==0) then
c  Time unit
         write(io,'(7x,''Period'',9x,''Log Spectrum (dB)''/)')
         do k=1,jf
          tk=rb/float(ks+k)
          write(io,'(g14.4,1x,a10,2x,g10.4)') tk,su,10.*alog10(sp(k))
          enddo
      endif
      return
      end
c
c===========================================
c
      subroutine wrtbisn(b1,b2,br,u,iu,it,rb,iunit,io)
c*******************************************
c  Output bispectrum for plotting 11-4-99
c===========================================
      real br(it),u(iu,iu/2)
      integer b1(it),b2(it)
      intent(in) b1,b2,br,iu,it,rb,iunit,io
c********************************************
      do k1=1,iu
       do k2=1,iu/2
        u(k1,k2)=0.
       enddo
      enddo
      do k=1,it
       u(b1(k),b2(k))=br(k)
      enddo
      do k2=1,iu/2
       if(iunit>0) then
        write(io,'(:,10000(f9.4))') (u(k1,k2),k1=1,iu)
         else
          write(io,'(:,10000(f14.3))') (u(k1,k2),k1=1,iu)
       endif
      enddo
      if(iunit==1) then
       write(io,'(:,10000(f9.3))') (float(k)*rb*1.e-3,k=1,iu)
        elseif(iunit==2) then
         write(io,'(:,10000(f9.3))') (float(k)*rb,k=1,iu)
          else
           write(io,'(:,10000(f14.3))') (rb/float(k),k=1,iu)
      endif
c-------
      return
      end
c
c===========================================
c
      subroutine outbisn(b1,b2,br,bi,ik,it,ot,su,rb,alm,bt,b0,brv,bal,re
     *s,ft,f0,frv,fal,iunit,io)
c*******************************************
c  Output large bispectral values
c===========================================
      allocatable:: i1
      real br(it),bi(it),fi(11),fo(11),ft(1),f0(1),frv(1),fal(1)
      integer b1(it),b2(it),ik(it),i1(:),it,ot,res
      character*10 su
      data fi/.001,.01,.05,.1,.25,.5,.75,.9,.95,.99,.999/
      intent(in) b1,b2,br,bi,it,ot,su,rb,alm,bt,b0,brv,bal,res,ft,f0,frv
     *,fal,iunit,io
c********************************************
      allocate(i1(0:it-1),stat=ibad)
      if(ibad/=0) then
       write(io,*) 'Unable to allocate work space in outbis'
       stop
      endif
      write(io,'(11x,''No. of bispectral values in IT = '',i6/)') it
      write(io,'(11x,''No. of bispectral values in OT = '',i6)') ot
c  Write p-values for tests
      write(io,'(/2x,''p-value = '',f7.4,'' for the quantile bispectrum
     *based nonlinearity test'')') 1.-bt
      write(io,'(/2x,''p-value = '',f7.4,'' for the time reversibility t
     *est'')') 1.-brv
      write(io,'(/2x,''p-value = '',f7.4,'' for the gaussianity test'')'
     *) 1.-b0
      write(io,'(/2x,''p-value = '',f7.4,'' for the aliasing test''/)')
     *1.-bal
c  Write lamda for chi2 of sum of squares
      write(io,'(11x,''Mean noncentrality parameter = '',f10.2/)') alm
      write(io,'(2x,70(''='')/)')
      call sort(ik,br,it,11,fi,fo,'B',io)
      if(res>0) then
c  Write shuffled p-values for tests
       write(io,'(2x,''Shuffled p-value = '',f7.4,'' for the quantile bi
     *spectrum based nonlinearity test'')') ft(1)
       write(io,'(/2x,''Shuffled p-value = '',f7.4,'' for the time rever
     *sibility test'')') frv(1)
       write(io,'(/2x,''Shuffled p-value = '',f7.4,'' for the zero bispe
     *ctrum test'')') f0(1)
       write(io,'(/2x,''Shuffled p-value = '',f7.4,'' for the aliasing t
     *est''/)') fal(1)
       write(io,'(2x,70(''*'')/)')
      endif
c-------
c  Write fractiles
      write(io,'(11x,''Fractiles of Normalized Bispectral Probabilities'
     *'/)')
      write(io,'(4x,''0.001'',4x,''0.01'',5x,''0.05'',5x,''0.10'',5x,''0
     *.25'',5x,''0.50'',5x,''0.75'',5x,''0.90'',5x,''0.95'',5x,''0.99'',
     *5x,''0.999''/)')
      write(io,'(2x,11(f7.3,2x)/)') (fo(k),k=1,11)
      do k=1,11
       ab=abs((fo(k)-fi(k))/sqrt(fi(k)*(1.-fi(k))/float(it)))
       fo(k)=1.-cdg(ab)
      enddo
      write(io,'(2x,11(f7.3,2x),'' p-values'')') (fo(k),k=1,11)
      write(io,'(2x,70(''='')/)')
c-------
c  Output top bispectral values
      nout=0
      pt=0.005
      do j=1,it
       ji=ik(j)
       if(br(ji)>1.-pt) then
        nout=nout+1
        i1(nout-1)=ji
       endif
      enddo
      if(nout>0) then
       write(io,'(7x,''% of Bispectral Probabilites < '',f6.3,'' = '',f6
     *.3,4x,''Standard Deviation = '',f6.3/)') pt*100.0,(float(nout)/flo
     *at(it))*100.0,sqrt(pt*(1.-pt)/float(it))*100.0
       else
        return
      endif
c-------
      write(io,'(1x,60(''='')/)')
      if(iunit==1) then
       write(io,'(12x,'' Frequencies in kHz''/)')
       write(io,'(5x,''f(1)'',7x,''f(2)'',11x,''1-Pr'',8x,''Phase''/)')
        elseif(iunit==2) then
         write(io,'(12x,'' Frequencies in Hz''/)')
        write(io,'(5x,''f(1)'',7x,''f(2)'',11x,''1-Pr'',8x,''Phase''/)')
      endif
      if(iunit==1) then
       do j=1,nout
        ji=i1(j-1)
        t1=float(b1(ji))*rb*1.e-3
        t2=float(b2(ji))*rb*1.e-3
        pr=1.-br(ji)
        if(t1>1.e-2.or.t2>1.e-2) then
         write(io,'(1x,f9.3,2x,f9.3,7x,f8.5,5x,f6.1/)') t1,t2,pr,bi(ji)
          else
          write(io,'(1x,g9.3,2x,g9.3,7x,f8.5,5x,f6.1/)') t1,t2,pr,bi(ji)
        endif
       enddo
      endif
      if(iunit==2) then
       do j=1,nout
        ji=i1(j-1)
        t1=float(b1(k))*rb
        t2=float(b2(ji))*rb
        pr=1.-br(ji)
        if(t1>1.e-2.or.t2>1.e-2) then
         write(io,'(1x,f9.3,2x,f9.3,7x,f8.5,5x,f6.1/)') t1,t2,pr,bi(ji)
          else
          write(io,'(1x,g9.3,2x,g9.3,7x,f8.5,5x,f6.1/)') t1,t2,pr,bi(ji)
        endif
       enddo
      endif
      if(iunit==0) then
c  Time unit
       write(io,'(17x,''Periods in '',a10/)') su
       write(io,'(9x,''Period 1'',8x,''Period 2'',8x,''1-Pr'',8x,''Phase
     *''/)')
       do j=1,nout
        ji=i1(j-1)
        t1=rb/float(b1(k))
        t2=rb/float(b2(ji))
        pr=1.-br(ji)
        if(t1>1.e-2.or.t2>1.e-2) then
        write(io,'(2x,f14.3,2x,f14.3,7x,f8.5,5x,f6.1/)') t1,t2,pr,bi(ji)
         else
        write(io,'(7x,g11.3,5x,g11.3,5x,f8.5,5x,f6.1/)') t1,t2,pr,bi(ji)
        endif
       enddo
      endif
c--------
      deallocate(i1)
      return
      end
c
c===========================================
c
      subroutine size(fl,fu,lb,nb,ot)
c*******************************************
c  Computes bispectrum loops and gets array size
c   it - size of IT, ot - size of OT
c   nb - actual size of br & bi arrays
c========================================================
      integer fl,fu,nb,it,ot
      intent(in) fl,fu,lb
      intent(out) nb,ot
c********************************************
c  Initialize bispec counters
      it=0
      ot=0
      n2=lb/2
      n4=n2/2
      if(4*n4<lb) then
       n4=n4+1
      endif
      if(n4<fl.or.n4>fu) then
c  n4 is not in the band
       n4=lb
      endif
c  Biperiodogram for k1=k2=fl
      if(2*fl<=fu) then
       it=it+1
      endif
      do k1=fl+1,fu
       do k2=fl,k1-1
c  Biperiodogram for IT
        ks=k1+k2
        if(ks<=fu) then
         it=it+1
        endif
c  Biperiodogram for OT
        if(k1>n4.and.k1<(lb-ks).and.(lb-ks)<=fu) then
          ot=ot+1
        endif
       enddo
c  Biperiodogram for k1=k2 < fu
       if(2*k1<=fu) then
        it=it+1
       endif
      enddo
      nb=it+ot
      return
      end
