      subroutine dinverse(a,n,d)
c********************************************
c  Finds inverse of A from cholesky decomposition A=LL'
c  Input: A & d from DCHOLSL
c  Output: A - inverse of covariance matrix
c============================================
      integer n,i,j
      real*8 a(n,n),d(n),sum
      intent(in) d,n
c********************************************
c  Lower diagonal inverse of L in a
      do i=1,n
       a(i,i)=1.d0/d(i)
       do j=i+1,n
        sum=0.d0
        do k=i,j-1
         sum=sum-a(j,k)*a(k,i)
        enddo
        a(j,i)=sum/d(j)
       enddo
      enddo
c------
c  Inverse of LL'
      do i=1,n
       do j=i+1,n
        sum=0.d0
        do k=j,n
         sum=sum+a(k,i)*a(k,j)
        enddo
        a(i,j)=sum
       enddo
c------
c  Diagonal
       sum=0.d0
       do k=i,n
        sum=sum+a(k,i)*a(k,i)
       enddo
        a(i,i)=sum
      enddo
c------
      do i=1,n
       do j=i+1,n
        a(j,i)=a(i,j)
       enddo
      enddo
c----------
      return
      end
