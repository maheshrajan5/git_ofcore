C
      subroutine datagen(n,nrhs,ndp)
C
C----------------------------------------------------------------
C     PURPOSE:
C        USER-SUPPLIED MAIN APPLICATION CODE TO GENERATE THE
C        COEFFICIENT & R.H.S. MATRIX A & B ( AX=B )
C
C     BUT HERE A DIAGONALLY DOMINANT DOUBLE COMPLEX A IS
C     GENERATED. THE R.H.S is so
C     CONSTRUCTED THAT SOLUTION VECTOR WILL BE (1.0,1.0)
C----------------------------------------------------------------
C
C
c
      integer n,nrhs,ndp,nhalf,nhalf2
      integer, allocatable, dimension(:,:)::indexi,indexj
      double precision, allocatable, dimension(:,:)::zar,zai
      double precision, allocatable, dimension(:,:)::zrhsr
      double precision, allocatable, dimension(:,:)::zrhsi
      double precision, allocatable, dimension(:,:)::z1r
      double precision, allocatable, dimension(:,:)::z1i
      double precision, allocatable, dimension(:,:)::z2r
      double precision, allocatable, dimension(:,:)::z2i
      common /io/ireal1a,iimag1a,ireal1b,iimag1b,ireal2,iimag2
c
      nhalf = ndp/2
      nhalf2 = ndp - nhalf
      nrecl1 = 8*n*n
      nrecl2 = 8*n*nrhs

c     allocate arrays
      allocate(indexi(n,n))
      allocate(indexj(n,n))
      allocate(zar(n,n))
      allocate(zai(n,n))
      allocate(z1r(n,n))
      allocate(z1i(n,n))
      allocate(z2r(n,n))
      allocate(z2i(n,n))
      allocate(zrhsr(n,nrhs))
      allocate(zrhsi(n,nrhs))

c
      z1r = 1.0d00
      z1i = 1.5d00
c
      do 10 i=1,n
       indexi(i,1:n)=i
 10   continue
c     
      do 50 ip=1,ndp
c
        zrhsr = 0.0d00
        zrhsi = 0.0d00

        do 20  j=1,n
          indexj(1:n,j)=j
 20     continue

        do 49 jp=1,ndp
C
          where(indexi == indexj)
             zar = 10.0d00
          elsewhere
             zar = (1.0d00/float(indexi*indexj))
          endwhere
         
          zai =2.0d00* zar
c
          call Y_MATMUL(zar,zai,z1r,z1i,z2r,z2i,n,n,nrhs)

          zrhsr = zrhsr + z2r
          zrhsi = zrhsi + z2i
C

            if( ip .le. nhalf)then
              ioff = (jp-1)*nhalf*nrecl1 + (ip-1)*nrecl1
              call mpl_write(zar,ireal1a,ioff,nrecl1)
              call mpl_write(zai,iimag1a,ioff,nrecl1)          
            else
              ioff = (jp-1)*nhalf2*nrecl1 + (ip - 1 - nhalf)*nrecl1
              call mpl_write(zar,ireal1b,ioff,nrecl1)
              call mpl_write(zai,iimag1b,ioff,nrecl1) 
            endif         

          indexj = indexj + jp*n
 49     continue

        ioff = (ip-1)*nrecl2
        call mpl_write(zrhsr,ireal2,ioff,nrecl2)
        call mpl_write(zrhsi,iimag2,ioff,nrecl2)
        indexi = indexi + ip*n
 50   continue
C
c     deallocate arrays
      deallocate(indexi)
      deallocate(indexj)
      deallocate(zar)
      deallocate(zai)
      deallocate(z1r)
      deallocate(z1i)
      deallocate(z2r)
      deallocate(z2i)
      deallocate(zrhsr)
      deallocate(zrhsi)
      return
      end
C







