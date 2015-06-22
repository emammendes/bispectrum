      subroutine dcholsl(a,n,d,b,x)
c********************************************
c  Solves the n linear equations Ax=b
c  Input: A & d from DCHOLDC. Only lower triangle of A is used
c============================================
      integer n,i,k
      real*8 a(n,n),b(n),d(n),x(n),sum
      intent(in) a,b,d
      intent(out) x
c********************************************
      do i=1,n
       sum=b(i)
       do k=i-1,1,-1
        sum=sum-a(i,k)*x(k)
       enddo
       x(i)=sum/d(i)
      enddo
c------
      do i=n,1,-1
       sum=x(i)
       do k=i+1,n
        sum=sum-a(k,i)*x(k)
       enddo
       x(i)=sum/d(i)
      enddo
c------
      return
      end
