      function numread(par,string,delim,ia,ier,io)
c*******************************************
c  Find a parameter name in the character*700 string. Then find the
c  integer numread in string enclosed by the symbols in delim.
c  There may be blanks before delim(1:1) and up to two blanks after it.
c  ier=1 - parameter not found in string, ier=2 - no left delimiter,
c  ier=3 - no right delimiter, ier=4 - number > 9999999
c===========================================
      integer numread
      character*700 string
      character*20 par
      character*2 delim
      intent(in) par,string,delim,io
      intent(out) ia,ier
c*******************************************
      ier=0
c  Find par
      nc=len_trim(par)
      ib=index(string,par(1:nc))
      if(ib==0) then
       ier=1
       return
      endif
      ia=index(string(ib:),delim(1:1))
      if(ia==0) then
       ier=2
       return
      endif
c  Position of first character in par
      ia=ia+ib
c  Trap blanks after delim(1:1)
      ic=index(string(ia:ia),' ')
      if(ic>0) then
       ia=ia+1
      endif
      ic=index(string(ia:ia),' ')
      if(ic>0) then
       ia=ia+1
      endif
c  Check for negative
      if(string(ia:ia)=='-') then
       neg=-1
       ia=ia+1
        else
         neg=1
      endif
c  Size of number
      ic=index(string(ia:),delim(2:2))-1
      if(ic<=0) then
       ier=3
       return
      endif
      if(ic<8) then
       call number(string(ia:),ic,numread,io)
        numread=numread*neg
        else
         write(io,*) 'The number ',string(ia:),' > 999999'
         ier=4
         return
      endif
c----------
      return
      end
