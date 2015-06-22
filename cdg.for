      function cdg(y)
c*******************************************
c  Normal N(0,1) distribution function at y
c===========================================
      real y
c*******************************************
      cdg=y*(.70710678)
      if(cdg>=0) then
       if(cdg<=4.3) then
       cdg=.5+erf(cdg)/2.
        else
         cdg=1.
       endif
        elseif(cdg<0.) then
         cdg=-cdg
         if(cdg<=4.3) then
          cdg=.5-erf(cdg)/2.
           else
            cdg=0.
         endif
      endif
      return
      end
