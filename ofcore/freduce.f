      subroutine freduce(zdr,zdi,zrhsr,zrhsi,isize,nrhs,ib,ndpco)
C
c
      integer isize,nrhs,ib,ndpco
      double precision, dimension(isize,isize):: zdr,zdi
      double precision, dimension(isize,nrhs):: zrhsr,zrhsi
      double precision, allocatable, dimension(:,:):: zdrco
      double precision, allocatable, dimension(:,:):: zdico
      double precision, allocatable, dimension(:,:):: zsrco
      double precision, allocatable, dimension(:,:):: zsico
      double precision, allocatable, dimension(:,:):: z2srco
      double precision, allocatable, dimension(:,:):: z2sico
c
      interface
         subroutine getrhs(z,zco,isize,nrhs,ib,irow)
         integer isize,nrhs,ib,irow
         double precision, dimension(isize,nrhs):: z
         double precision, dimension(ib,nrhs):: zco
         integer iflag,ip,jp,ndp,nrecl
         end subroutine getrhs
         subroutine putrhs(z,zco,isize,nrhs,ib,irow)
         integer isize,nrhs,ib,irow
         double precision, dimension(isize,nrhs):: z
         double precision, dimension(ib,nrhs):: zco
         end subroutine putrhs
      end interface
c

c     allocate arrays
      allocate(zdrco(ib,ib))
      allocate(zdico(ib,ib))
      allocate(zsrco(ib,nrhs))
      allocate(zsico(ib,nrhs))
      allocate(z2srco(ib,nrhs))
      allocate(z2sico(ib,nrhs))
c
c     zd contains the diagonal i/o block

c     forward reduce

       do 250 kp = 1,ndpco-1

         call getrhs(zrhsr,zsrco,isize,nrhs,ib,kp)
         call getrhs(zrhsi,zsico,isize,nrhs,ib,kp)

         do 240 ip = kp + 1, ndpco

         call getrhs(zrhsr,z2srco,isize,nrhs,ib,ip)
         call getrhs(zrhsi,z2sico,isize,nrhs,ib,ip)

         call copyin(zdr,zdrco,isize,ib,ip,kp)
         call copyin(zdi,zdico,isize,ib,ip,kp)       

          call Y_MATMUL2(zdrco,zdico,zsrco,zsico,z2srco,z2sico,
     &                 ib,ib,nrhs)


          call putrhs(zrhsr,z2srco,isize,nrhs,ib,ip)
          call putrhs(zrhsi,z2sico,isize,nrhs,ib,ip)

240    continue
250    continue

c     back solve

       do 270 kp = ndpco,1,-1

         call getrhs(zrhsr,zsrco,isize,nrhs,ib,kp)
         call getrhs(zrhsi,zsico,isize,nrhs,ib,kp)

         call copyin(zdr,zdrco,isize,ib,kp,kp)
         call copyin(zdi,zdico,isize,ib,kp,kp)       

         call Y_MATMUL(zdrco,zdico,zsrco,zsico,z2srco,z2sico,
     &                 ib,ib,nrhs)


         call putrhs(zrhsr,z2srco,isize,nrhs,ib,kp)
         call putrhs(zrhsi,z2sico,isize,nrhs,ib,kp)

         do 260 ip=kp-1,1,-1

          call copyin(zdr,zdrco,isize,ib,ip,kp)
          call copyin(zdi,zdico,isize,ib,ip,kp)       

          call getrhs(zrhsr,zsrco,isize,nrhs,ib,ip)
          call getrhs(zrhsi,zsico,isize,nrhs,ib,ip)

          call Y_MATMUL2(zdrco,zdico,z2srco,z2sico,zsrco,zsico,
     &                 ib,ib,nrhs)


          call putrhs(zrhsr,zsrco,isize,nrhs,ib,ip)
          call putrhs(zrhsi,zsico,isize,nrhs,ib,ip)

260    continue
270    continue
c
c     deallocate arrays
      deallocate(zdrco)
      deallocate(zdico)
      deallocate(zsrco)
      deallocate(zsico)
      deallocate(z2srco)
      deallocate(z2sico)

       return
       end




