      subroutine sort(indx,x,n,nfrac,frac,vfrac,isrt,lun)
c*********************************************************
c    D.M. Patterson, and M.J. Hinich
c    Update: 5-22-95
c    Single Precision   Error output in unit=lun, ftile='error.sort'
c
c    Sorts the pointer array, indx, into ascending order based on reals
c    in array x. Routine uses Heapsort method.
c    Any number of fractiles can be computed from the order statistics
c     using a linear interpolation.
c-----------------------------
c  Definintions of variables
c-----------------------------
c  indx  : integer array that points to locations in array x
c          It is initialized if x is sorted. On output indx(k) is the
c          index of the x(k) sorted in ascending order.
c  x     : array to be sorted by reordering pointers in indx
c  n     : number of elements in x and indx
c  nfrac : number of quantiles to be found and the number of
c           quantiles in the vfrac and frac arrays
c  frac  : input quantiles set in call to sort
c  vfrac : ouput quantiles computed from x
c  isrt  : job control character
c          1) isrt = S : Sort the values in the array x. No fractiles!
c          2) isrt = F : Compute the fractiles. The array x is assumed
c                        to be already sorted!
c          3) isrt = B : Sort the array and compute the fractiles.
c  lun   : unit number for file for error messages
c==================================================
      real x(n),frac(nfrac),vfrac(nfrac)
      integer indx(n)
      character isrt,op
      intent(in) x,frac,n,nfrac
      intent(out) vfrac
      intent(in out) indx
c**************************************************
      op='n'
      if(isrt/='S'.and.isrt/='B'.and.isrt/='F') then
       call fwrite(op,lun)
       write(6,*) 'Isrt control character in SORT is wrong  ',isrt
       write(lun,*) 'Isrt control character in SORT is wrong  ',isrt
      endif
      if(nfrac<=0. and. isrt/='S') then
       call fwrite(op,lun)
       write(6,*) 'Number of fractiles in SORT <= 0: ',nfrac
       write(lun,*) 'Number of fractiles in SORT <= 0: ',nfrac
       return
      endif
c  Trap n < 1
      if(n<1) then
       call fwrite(op,lun)
       write(6,*) 'n in x(n) < 1',n
       write(lun,*) 'n in x(n) < 1',n
       return
      endif
c  If the array has one element, set vfrac=x
      if(n==1. and. isrt/='S') then
       call fwrite(op,lun)
       write(6,*) 'fractiles set to x since n=1',n
       write(lun,*) 'fractiles set to x since n=1',n
       do jl=1,nfrac
        vfrac(jl)=x(n)
       enddo
       return
      endif
      if(isrt=='S'.or.isrt=='B') then
c  Call indexed Heapsort from "Numerical Recipes"
       call indexx(n,x,indx)
      endif
c-------
c  Find the nfrac p% quantiles
      if(isrt=='F'.or.isrt=='B') then
c  If frac(j) > 1 then the fractiles are set to the max
c  If frac(j) < 0 then the fractiles are set to the min
c-------
       do jl=1,nfrac
        if(frac(jl)>=1.) then
         vfrac(jl)=x(indx(n))
          elseif(frac(jl)<=0.) then
           vfrac(jl)=x(indx(1))
            else
             pn=frac(jl)*float(n+1)
             ji=int(pn)
             g=pn-float(ji)
             ji1=ji+1
             if(ji1>n) ji1=n
             if(ji<=0) ji=1
             if(ji>n) ji=n
             vfrac(jl)=(1-g)*x(indx(ji))+g*x(indx(ji1))
        endif
       enddo
      endif
c-------
      return
      end
c
c=================================
c
      subroutine fwrite(op,lun)
c*********************************
c  Delete old file='error.sort' and open new one on lun
c=================================
      character op
      logical exs
c*********************************
      if(op=='y') then
       return
      endif
      inquire(file='error.sort',err=1,iostat=i1,exist=exs)
      if(exs) then
       open(lun,file='error.sort',status='old')
       close(lun,status='delete')
       open(lun,file='error.sort',err=2,iostat=i2)
       op='y'
        return
         else
          open(lun,file='error.sort',err=2,iostat=i2)
          return
       endif
c-------
 1    write(6,*) 'Error in the inquire call in SORT - sub FWRITE',i1
 2    write(6,*) 'Error in opening file error sort in sub FWRITE',i2
      end
c
c========================================
c
      subroutine indexx(n,arrin,indx)
c*************************************
c  Sorts real array arrin in ascending order using an indexed Heapsort
c  Input: arrin  Version: 5-22-95
c============================================
      real arrin(n)
      integer indx(n)
c*************************************
      if(n==1) return
      do j=1,n
       indx(j)=j
      enddo
      l=n/2+1
      ir=n
  2   continue
        if(l>1)then
          l=l-1
          indxt=indx(l)
          q=arrin(indxt)
        else
          indxt=indx(ir)
          q=arrin(indxt)
          indx(ir)=indx(1)
          ir=ir-1
          if(ir==1)then
            indx(1)=indxt
            return
          endif
        endif
        i=l
        j=l+l
  3     if(j<=ir)then
          if(j<ir)then
           if(arrin(indx(j))<arrin(indx(j+1)))j=j+1
          endif
          if(q<arrin(indx(j)))then
            indx(i)=indx(j)
            i=j
            j=j+j
          else
            j=ir+1
          endif
        goto 3
        endif
        indx(i)=indxt
      goto 2
c-------
      end
