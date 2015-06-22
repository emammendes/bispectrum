      subroutine head(mth,ndy,nyr,gs,rt,io)
c********************************************
c  Write headers and sets date, time, version. Version 9-27-99
c  Input: mth, ndy, ny - date from call, gs - start time from cpu_time
c  Output: rt - run time in seconds
c============================================
      integer mth,ndy,nyr,io
      real gs,ge,rt,aux
      character*24 dat
      intent(in) mth,ndy,nyr,gs,io
      intent(out) rt
c********************************************
      write(io,'('' Version '',i2,''-'',i2,''-'',i4)') mth,ndy,nyr
c  Output time and date
      call fdate(dat)
      write(io,'('' Date & Time '',a24)') dat
c  End time
      call cpu_time(ge)
      rt=ge-gs
c  Correct for time fold
      if(rt<0.) then
       rt=rt+86400.
      endif
c-------
      if(rt<60.) then
       write(io,'('' Elapsed time = '',f9.2,''  secs'')') rt
       write(6,'('' Elapsed time = '',f9.2,'' secs'')') rt
       return
        elseif(rt<3600.) then
         rm=rt/60.
         write(io,'('' Elapsed time = '',f9.2,'' minutes'')') rm
         write(6,'(/'' Elapsed time = '',f9.2,'' minutes'')') rm
         return
          else
           rh=rt/3600.
           write(io,'('' Elapsed time = '',f9.2,'' hours'')') rh
           write(6,'(/'' Elapsed time = '',f9.2,'' hours'')') rh
      endif
c----------
      return
      end
