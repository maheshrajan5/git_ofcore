C
      program main
C
cmpf  mpl mpl_open
C	ncol:	no. of columns in the coefficient matrix
C       nrhs:   no. of Right Hand Side
C
C
      common /io/ireal1a,iimag1a,ireal1b,iimag1b,ireal2,iimag2

      open(23,file='INPUT')
      read (23,*)ncol,nrhs,isize,ib !Get the sizes of the problem
      ndp = (ncol-1)/isize + 1
      ndpco = (isize-1)/ib + 1
      call mpl_open(ireal1a,iimag1a,ireal1b,iimag1b,ireal2,iimag2)

      call datagen(isize,nrhs,ndp) !Make test data
      call mypapi_init()	
      call ofsolve(ncol,nrhs,isize,ib,ndp,ndpco) !Solve equations
      call mypapi_finalize()
      stop
      end
