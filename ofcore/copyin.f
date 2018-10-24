       subroutine copyin(z,zco,isize,ib,irow,jcol)
       double precision, dimension(isize,isize):: z
       double precision, dimension(ib,ib):: zco
c
CMPF  ONDPU z,zco
c
      istart = (irow-1)*ib+1
      jstart = (jcol-1)*ib+1
      iend = istart + ib - 1
      jend = jstart + ib - 1
c 
          zco(1:ib,1:ib) = z(istart:iend,jstart:jend)
c
      return
      end
