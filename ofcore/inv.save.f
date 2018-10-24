      subroutine inv(ib,zdrco,zdico,zirco,ziico)
      double precision, dimension(ib,ib):: zdrco,zirco
      double precision, dimension(ib,ib):: zdico,ziico
      real*8 rcond,det(2)
      integer ib, job
      double precision, allocatable, dimension(:):: work
      integer, allocatable, dimension(:):: ipvt
c
c     allocate arrays
      allocate(work(ib))
      allocate(ipvt(ib))

      job = 1
      zirco(1:ib,1:ib) = zdrco(1:ib,1:ib)
      call dgeco(zirco, ib,ib,ipvt,rcond,work)
      if(1.0d00 + rcond .ne. 1.0d00) then
         call cgedi(zirco,ib,ib,ipvt,det,work,job)
      else
         write(6,*)"inverse matrix is singular in inv"
         stop
      endif
c
      job = 1
      ziico(1:ib,1:ib) = zdico(1:ib,1:ib)
      if(1.0d00 + rcond .ne. 1.0d00) then
         call dgeco(ziico, ib,ib,ipvt,rcond,work)
      else
         write(6,*)"inverse matrix is singular in inv"
         stop
      endif
      call cgedi(ziico,ib,ib,ipvt,det,work,job)
c
c     deallocate arrays
      deallocate(work)
      deallocate(ipvt)

      return
      end
