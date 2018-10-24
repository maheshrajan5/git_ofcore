       subroutine putrhs(z,zco,isize,nrhs,ib,irow)
c
      double precision, dimension(isize,nrhs):: z
      double precision, dimension(ib,nrhs):: zco

      istart = (irow-1)*ib+1
      iend = istart + ib - 1
c 
           z( istart:iend , :) = zco( 1:ib , :)
c
      return
      end
