       subroutine getrhs(z,zco,isize,nrhs,ib,irow)
c
      double precision, dimension(isize,nrhs):: z
      double precision, dimension(ib,nrhs):: zco

      istart = (irow-1)*ib+1
      iend = istart + ib - 1
c 
          zco( 1:ib , :) = z( istart:iend , :)
c
      return
      end
