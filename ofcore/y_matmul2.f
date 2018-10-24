      subroutine y_matmul2(ar,ai,br,bi,cr,ci,l,m,n)
      double precision, dimension(l,m):: ar,ai
      double precision, dimension(m,n):: br,bi
      double precision, dimension(l,n):: cr,ci
c
cmr      cr = cr -  matmul(ar,br) + matmul(ai,bi)
cmr      ci = ci -  matmul(ar,bi) - matmul(ai,br) 
      real*8 alpha, beta
	real*8 istart, iend, seconds, elapsed_time
c
      alpha = -1.0d00
      beta = 1.0d00
      istart = seconds(0.)
      call dgemm('n','n',l,n,m,alpha,ar,l,br,m,beta,cr,l)	
      iend =  seconds(0.)
      elapsed_time =  (iend - istart)
cmr      print *, 'dgemm(secs) for matrix size:n= ',n,'  ', elapsed_time
      alpha = 1.0d00
      beta = 1.0d00
      call dgemm('n','n',l,n,m,alpha,ai,l,bi,m,beta,cr,l)	
      alpha = -1.0d00
      beta = 1.0d00
      call dgemm('n','n',l,n,m,alpha,ar,l,bi,m,beta,ci,l)	
      alpha = -1.0d00
      beta = 1.0d00
      call dgemm('n','n',l,n,m,alpha,ai,l,br,m,beta,ci,l)	
c
      return
      end  
     
