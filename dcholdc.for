      subroutine dcholdc(a,n,d,ier,io)
c********************************************
c  Cholesky decomposition A=LL'
c  Iput: A - positive definite matrix
c  Output: L - lower diagonal of A, diagonal in d
c============================================
      integer n,i,j,k
      real*8 a(n,n),d(n),sum
      intent(in) n
      intent(out) d,ier
      intent(in out) a
c********************************************
c  Set error flag to off
      ier=0
      do i=1,n
       do j=i,n
        sum=a(i,j)
        do k=i-1,1,-1
         sum=sum-a(i,k)*a(j,k)
        enddo
c------
        if(i==j) then
         if(sum<=0.d0) then
          ier=1
c  write(io,*) 'DCHOLDC failed, A is not positive definite'
          return
         endif
          d(i)=dsqrt(sum)
           else
            a(j,i)=sum/d(i)
        endif
       enddo
      enddo
c-------
      return
      end
