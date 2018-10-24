C
      subroutine output(zr,zi,zerr,zeri,isize,nrhs,ndp)
      implicit double precision (z)
      common /io/ir1,ii1,irb,iib,ir2,ii2
C
      dimension zr(1:isize,1:nrhs),zerr(1:isize,1:nrhs)
      dimension zi(1:isize,1:nrhs),zeri(1:isize,1:nrhs)
      double precision,  allocatable, dimension(:):: temp
      double precision tot_error
c
c
c     interface blocks
      interface
         subroutine getblk(z,iflag,ip,jp,ndp,nrecl)
         integer iflag,ip,jp,ndp,nrecl
         double precision z(:,:)
         end subroutine getblk
         subroutine putblk(z,iflag,ip,jp,ndp,nrecl)
         integer iflag,ip,jp,ndp,nrecl
         double precision z(:,:)
         end subroutine putblk
      end interface
c
c     allocate arrays
      allocate(temp(isize))
c
      nrecl2 = 8*isize*nrhs

      do 50 ip=1,ndp
        ioff = (ip-1)*nrecl2
cm        call mpl_read(zr,ir2,ioff,nrecl2)
cm        call mpl_read(zi,ii2,ioff,nrecl2)

        call getblk(zr,3,ip,1,ndp,nrecl2)
        call getblk(zi,4,ip,1,ndp,nrecl2)

        zerr = abs(1.0d00 - zr)
        temp = sum(zerr,dim=2)

        tot_error = sum(temp,dim=1)

        zeri = abs(1.5d00 - zi)
        temp = sum(zeri,dim=2)
        tot_error = tot_error + sum(temp,dim=1)

        avg_error = tot_error/(isize*nrhs)
cmr        write(6,*)'SOLUTION FROM OUTPUT: real part',zr
cmr        write(6,*)'SOLUTION FROM OUTPUT: imag. part',zi
        write(6,*)'AVERAGE ABSOLUTE ERROR',avg_error,'  FOR BLOCK',ip
 50   continue
C
c     deallocate arrays
      deallocate(temp)

      return
      end
C





