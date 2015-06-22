      function rnumread(par,string,delim,ia,ier,io)
c*******************************************
c  Find a parameter name in the character*700 string. Then find the real
c  number in the string enclosed by the symbols in delim.
c  There may be blanks before delim(1:1) and up to two blanks after it.
c  ier=1 - parameter not found in string, ier=2 - no left delimiter,
c  ier=3 - no right delimiter, ier=4 - no period, ier=5 - number > 999,999
c===========================================
      real rnumread
      real*8 mag,frac
      character*700 string
      character*20 par
      character*10 ac,buf
      character*2 delim
      intent(in) par,delim,io
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
       rneg=-1.
       ia=ia+1
        else
         rneg=1.
      endif
      it=index(string(ia:),'.')
      if(it==0) then
       ier=4
       return
      endif
c  Size of number
      ic=it-1
      if(ic<8) then
       call number(string(ia:),ic,numag,io)
        else
         write(io,*) 'The number ',string(ia:),' > 9,999,999'
         ier=5
         return
      endif
      ib=ia+it
      buf=string(ib-1:)
c  Check on blank after period
      if(buf(2:2)==' ') then
       buf(2:2)='0'
      endif
      ic=index(buf(2:),delim(2:2))-1
      if(ic<=0) then
       ier=3
       return
      endif
      if(ic<7) then
       ac=buf(2:)
       call number(ac,ic,numfrac,io)
        else
         write(io,*) 'The number ',string(ib:),' > 9,999,999'
         ier=5
         return
      endif
      mag=dfloat(numag)
      frac=dfloat(numfrac)/dfloat(10**ic)
      rnumread=sngl(mag+frac)*rneg
c----------
      return
      end
