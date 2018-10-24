      subroutine my_inv(n,a,lda,ipvt,info)
c     n = dimension of matrix
c     a = A matrix dimensioned lda,N
c     lda = leading dimension of A
c     ipvt = pivot matrix ipvt(n) - size = n
c     info = integer 0 for successful return
c
      implicit none
      integer lda,n,info
      integer ipvt(n)
      real*8 a(lda,n)
c
      integer i,j,mi,k
      real*8 piv,term,eps,fac,row(n),col(n)

      eps = 1.0d-08
c  gauss-jordan-transformation begins here
          do i=1,n
            ipvt(i)=i
          enddo
          do   i=1,n
            piv=abs(a(i,i))
            mi=i
c***    search for a pivot
            do j=i+1,n
              if ( abs(a(i,j)) .gt. piv ) then
                piv=abs(a(i,j))
                mi=j
              endif
            enddo
            if ( mi .ne. i ) then
              j=ipvt(mi)
              ipvt(mi)=ipvt(i)
              ipvt(i)=j
              do j=1,n
                term=a(j,i)
                a(j,i)=a(j,mi)
                a(j,mi)=term
              enddo
            if ( abs(a(i,i)) .le. eps ) then
c******* this is a bit poor ... eps should be about n*machine_precision*
c        maximum |a(i,j)| (initial matrix)
              stop 'matrix singulaer'
            endif
            piv=1.d0/a(i,i)
            do  j=1,n
              col(j)=a(j,i)
              row(j)=a(i,j)
            enddo
            do  j=1,n
              fac=row(j)*piv
              do  k=1,n
                a(k,j)=a(k,j)-col(k)*fac
              enddo
            enddo
            do  k=1,n
              a(i,k)=row(k)*piv
            enddo
            do  k=1,n
              a(k,i)=-col(k)*piv

            enddo
            a(i,i)=piv
          enddo
c  backchange
          do i=1,n
            k=ipvt(i)
            do while ( k .ne. i )
              do j=1,n
                term=a(i,j)
                a(i,j)=a(k,j)
                a(k,j)=term
              enddo
              j=ipvt(i)
              ipvt(i)=ipvt(k)
              ipvt(k)=j
              k=ipvt(i)
            enddo
          enddo
        enddo
c**** end of gauss-jordan
        return
        end
