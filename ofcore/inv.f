      subroutine inv(ib,zdrco,zdico,zirco,ziico)
      double precision, dimension(ib,ib):: zdrco,zirco
      double precision, dimension(ib,ib):: zdico,ziico
      complex*16 det(2)
      integer ib, ier, lwork
      complex*16, allocatable, dimension(:):: work
      complex*16, allocatable, dimension(:,:)::zdco
      integer, allocatable, dimension(:):: ipvt
c
c     allocate arrays
      allocate(work(ib*ib))
      allocate(ipvt(ib))
      allocate(zdco(ib,ib))
c

      lwork = ib*ib
      zdco(1:ib,1:ib) = cmplx(zdrco(1:ib,1:ib),zdico(1:ib,1:ib))
      call zgetrf(ib, ib, zdco, ib, ipvt, ier)
      if(ier .eq. 0) then
         call zgetri(ib,zdco,ib,ipvt,work,lwork,ier)
      else
         write(6,*)"inverse matrix is singular in inv"
         stop
      endif
c

      zirco(1:ib,1:ib) = real(zdco(1:ib,1:ib))
      ziico(1:ib,1:ib) = aimag(zdco(1:ib,1:ib))
c     deallocate arrays
      deallocate(work)
      deallocate(ipvt)
      deallocate(zdco)

      return
      end
