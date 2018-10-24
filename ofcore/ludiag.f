      subroutine ludiag(zdr,zdi,isize,ib,ndpco)
C
c
      double precision, dimension(isize,isize):: zdr,zdi
      real ops_mul,matmul_time,inv_time
      common /stat/ops_mul, matmul_time, inv_time
      real*8 istart, iend, seconds
c
      double precision, allocatable, dimension(:,:):: zbrco
      double precision, allocatable, dimension(:,:):: zbico
      double precision, allocatable, dimension(:,:):: zdrco
      double precision, allocatable, dimension(:,:):: zdico
      double precision, allocatable, dimension(:,:):: zirco
      double precision, allocatable, dimension(:,:):: ziico
c
c     allocate arrays
      allocate(zdrco(ib,ib))
      allocate(zdico(ib,ib))
      allocate(zbrco(ib,ib))
      allocate(zbico(ib,ib))
      allocate(zirco(ib,ib))
      allocate(ziico(ib,ib))

c
c     zd contains the diagonal i/o block
c
c
      do 900 kp=1,ndpco
       kp1 = kp + 1
       call copyin(zdr,zdrco,isize,ib,kp,kp)
       call copyin(zdi,zdico,isize,ib,kp,kp)

       istart = seconds(0.)
       call inv(ib,zdrco,zdico,zirco,ziico)
       iend =  seconds(0.)
       inv_time = inv_time + iend - istart

       call copyout(zdr,zirco,isize,ib,kp,kp)
       call copyout(zdi,ziico,isize,ib,kp,kp)

c
       do 200 ip=kp+1,ndpco
         call copyin(zdr,zbrco,isize,ib,ip,kp)
         call copyin(zdi,zbico,isize,ib,ip,kp)

         istart = seconds(0.)
         call Y_MATMUL(zbrco,zbico,zirco,ziico,zdrco,zdico,ib,ib,ib)
         iend =  seconds(0.)
         ops_mul = ops_mul + 1
         matmul_time = matmul_time + iend - istart

         call copyout(zdr,zdrco,isize,ib,ip,kp)
         call copyout(zdi,zdico,isize,ib,ip,kp)
200    continue
c
         do 230 jp=kp+1,ndpco
         call copyin(zdr,zirco,isize,ib,kp,jp)
         call copyin(zdi,ziico,isize,ib,kp,jp)

         do 220 ip=kp+1,ndpco
           call copyin(zdr,zdrco,isize,ib,ip,jp)
           call copyin(zdi,zdico,isize,ib,ip,jp)

           call copyin(zdr,zbrco,isize,ib,ip,kp)
           call copyin(zdi,zbico,isize,ib,ip,kp)

           istart = seconds(0.)
           call Y_MATMUL2(zbrco,zbico,zirco,ziico,zdrco,zdico,ib,ib,ib)
           iend =  seconds(0.)
           ops_mul = ops_mul + 1
           matmul_time = matmul_time + iend - istart

           call copyout(zdr,zdrco,isize,ib,ip,jp)
           call copyout(zdi,zdico,isize,ib,ip,jp)

220      continue
230    continue
c
900   continue
c
c     deallocate arrays
      deallocate(zdrco)
      deallocate(zdico)
      deallocate(zbrco)
      deallocate(zbico)
      deallocate(zirco)
      deallocate(ziico)

      return
      end      



