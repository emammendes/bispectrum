      function charead(par,string,delim,ia,ier,io)
c*******************************************
c  Find a parameter name in the character*700 string. Then find the
c  character*50 string charead in string enclosed by the symbols in delim.
c  There may be blanks before delim(1:1) and up to two blanks after it.
c  ier=1 - parameter not found in string, ier=2 - no left delimiter,
c  ier=3 - no right delimiter
c===========================================
      character*50 charead
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
      charead='          '
      ia=index(string(ib:),delim(1:1))
      if(ia==0) then
       ier=2
       return
      endif
c  Position of first character in par
      ia=ia+ib
      ic=index(string(ia:),delim(2:2))-1
      if(ic<=0) then
       ier=3
       return
      endif
      charead=string(ia:ia+ic)
c----------
      return
      end
