       subroutine copy2out(z,zco,isize,ib,jcol)
       double precision, dimension(isize,isize):: z
       double precision, dimension(isize,ib):: zco
c
CMPF  ONDPU z,zco
c
      jstart = (jcol-1)*ib + 1
      jend = jstart + ib - 1
c 
      z(:,jstart:jend) = zco(:,1:ib)
c
      return
      end
