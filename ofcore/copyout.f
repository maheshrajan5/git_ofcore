       subroutine copyout(z,zco,isize,ib,irow,jcol)
       double precision, dimension(isize,isize):: z
       double precision, dimension(ib,ib):: zco
c
CMPF  ONDPU z,zco
c
      istart = (irow-1)*ib + 1
      jstart = (jcol-1)*ib + 1
c 
      iend = istart + ib - 1 
      jend = jstart + ib - 1
c 
      z(istart:iend,jstart:jend) = zco(1:ib,1:ib)
c
      return
      end
