      subroutine ofsolve(ncol,nrhs,isize,ib,ndp,ndpco)
      integer ncol,nrhs,isize,ndp
      real io_time,matmul_time,inv_time,factor_time,total_time
      real bsfr_time,ops_io,ops_mul,lud_time,tsol_time
      common /io/ir1,ii1,irb,iib,ir2,ii2
      common /stat/ops_mul, matmul_time, inv_time
      double precision, allocatable, dimension(:,:):: zbr,zdr,zir
      double precision, allocatable, dimension(:,:):: zbi,zdi,zii
      double precision, allocatable, dimension(:,:):: zrhsr,zrhs2r
      double precision, allocatable, dimension(:,:):: zrhsi,zrhs2i
      real*8 istart, iend, istatus, seconds, ibegin, fact_begin,fac_lop
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
         subroutine freduce(zdr,zdi,zrhsr,zrhsi,isize,nrhs,ib,ndpco)
         integer isize,nrhs,ib,ndpco
         double precision, dimension(isize,isize):: zdr,zdi
         double precision, dimension(isize,nrhs):: zrhsr,zrhsi
         end subroutine freduce
      end interface
c
c     allocate arrays
      allocate(zbr(isize,isize))
      allocate(zdr(isize,isize))
      allocate(zir(isize,isize))
      allocate(zbi(isize,isize))
      allocate(zdi(isize,isize))
      allocate(zii(isize,isize))
      allocate(zrhsr(isize,nrhs))
      allocate(zrhsi(isize,nrhs))
      allocate(zrhs2r(isize,nrhs))
      allocate(zrhs2i(isize,nrhs))
c
      ntp = ndp*ndp
      nrecl1 = 8 * isize * isize
      nrecl2 = 8 * isize * nrhs
c
      io_time=0
      matmul_time=0
      inv_time=0
      factor_time=0
      total_time=0
      bsfr_time=0
      lud_time=0
      tsol_time=0
c
      ibegin = seconds(0.)
      fact_begin =  seconds(0.)
      ops_io = 0
      ops_mul = 0
c
c
      do 900 kp=1,ndp
       kp1 = kp + 1
       fac_lop = seconds(0.)
       istart = seconds(0.)
       call getblk(zdr,1,kp,kp,ndp,nrecl1)
       call getblk(zdi,2,kp,kp,ndp,nrecl1)
       iend =  seconds(0.)
       ops_io = ops_io + 1
       io_time = io_time + iend - istart

       istart = seconds(0.)
cm       call inv(isize,zdr,zdi,zir,zii)
cmr       write(6,*)'zdr-before ludiag',zdr
       call ludiag(zdr,zdi,isize,ib,ndpco)
cmr       write(6,*)'zdi-after ludiag',zdr
       iend =  seconds(0.)
       lud_time = lud_time + iend - istart
c
       istart = seconds(0.)
       call putblk(zdr,1,kp,kp,ndp,nrecl1)
       call putblk(zdi,2,kp,kp,ndp,nrecl1)
       iend =  seconds(0.)
       ops_io = ops_io + 1
       io_time = io_time + iend - istart
c
c
       do 200 ip=kp+1,ndp

         istart = seconds(0.)
         call getblk(zbr,1,ip,kp,ndp,nrecl1)
         call getblk(zbi,2,ip,kp,ndp,nrecl1)
         iend =  seconds(0.)
         ops_io = ops_io + 1
         io_time = io_time + iend - istart
c
         istart = seconds(0.)
         call tsolver(zbr,zbi,zdr,zdi,isize,ib,ndpco)
         iend =  seconds(0.)
         tsol_time = tsol_time + iend - istart

          istart = seconds(0.)
          call putblk(zbr,1,ip,kp,ndp,nrecl1)
          call putblk(zbi,2,ip,kp,ndp,nrecl1)
          iend =  seconds(0.)
          ops_io = ops_io + 1
          io_time = io_time + iend - istart

200    continue
c
         do 230 jp=kp+1,ndp

          istart = seconds(0.)
          call getblk(zir,1,kp,jp,ndp,nrecl1)
          call getblk(zii,2,kp,jp,ndp,nrecl1)
          iend =  seconds(0.)
          ops_io = ops_io + 1
          io_time = io_time + iend - istart

          do 220 ip=kp+1,ndp

            istart = seconds(0.)
            call getblk(zdr,1,ip,jp,ndp,nrecl1)
            call getblk(zdi,2,ip,jp,ndp,nrecl1)
            iend =  seconds(0.)
            ops_io = ops_io + 1
            io_time = io_time + iend - istart

            istart = seconds(0.)
            call getblk(zbr,1,ip,kp,ndp,nrecl1)
            call getblk(zbi,2,ip,kp,ndp,nrecl1)
            iend =  seconds(0.)
            ops_io = ops_io + 1
            io_time = io_time + iend - istart

            istart = seconds(0.)
            call Y_MATMUL2(zbr,zbi,zir,zii,zdr,zdi,isize,isize,isize)
            iend =  seconds(0.)
            ops_mul = ops_mul + 1
            matmul_time = matmul_time + iend - istart

            istart = seconds(0.)
            call putblk(zdr,1,ip,jp,ndp,nrecl1)
            call putblk(zdi,2,ip,jp,ndp,nrecl1)
            iend =  seconds(0.)
            ops_io = ops_io + 1
            io_time = io_time + iend - istart

220      continue
230    continue
c
       istatus =  seconds(0.)
       write(6,*)' STATUS REPORT: 1'
       write(6,*)'  COMPLETED FACTORIZATION STEP',KP, 
     &           ' IN TIME', istatus - fac_lop,'   SECONDS'
900   continue
c
       istatus =  seconds(0.)
      factor_time = istatus - fact_begin
c
      if( ndp .ne. 1) then
c     forward reduce
      istart = seconds(0.)

       do 250 kp = 1,ndp-1

          call getblk(zrhsr,3,kp,1,ndp,nrecl2)
          call getblk(zrhsi,4,kp,1,ndp,nrecl2)

         do 240 ip = kp + 1, ndp

          call getblk(zrhs2r,3,ip,1,ndp,nrecl2)
          call getblk(zrhs2i,4,ip,1,ndp,nrecl2)
          call getblk(zdr,1,ip,kp,ndp,nrecl1)
          call getblk(zdi,2,ip,kp,ndp,nrecl1)

          call Y_MATMUL2(zdr,zdi,zrhsr,zrhsi,zrhs2r,zrhs2i,
     &                 isize,isize,nrhs)


          call putblk(zrhs2r,3,ip,1,ndp,nrecl2)
          call putblk(zrhs2i,4,ip,1,ndp,nrecl2)
240    continue
250    continue
       istatus =  seconds(0.)
       write(6,*)' STATUS REPORT: 2'
       write(6,*)' PROGRAM COMPLETED FORWARD REDUCTION',
     &           ' AT TIME', istatus -istart,'   SECONDS'
c
c     back solve
      istart = seconds(0.)
       do 270 kp = ndp,1,-1
         call getblk(zrhsr,3,kp,1,ndp,nrecl2)
         call getblk(zrhsi,4,kp,1,ndp,nrecl2)
         call getblk(zir,1,kp,kp,ndp,nrecl1)
         call getblk(zii,2,kp,kp,ndp,nrecl1)
cmr         call Y_MATMUL(zir,zii,zrhsr,zrhsi,zrhs2r,zrhs2i,
cmr     &                 isize,isize,nrhs)

cdeb         write(6,*)'calling freduce; kp = ',kp
         call freduce(zir,zii,zrhsr,zrhsi,isize,nrhs,ib,ndpco)

         call putblk(zrhsr,3,kp,1,ndp,nrecl2)
         call putblk(zrhsi,4,kp,1,ndp,nrecl2)

         do 260 ip=kp-1,1,-1
          call getblk(zdr,1,ip,kp,ndp,nrecl1)
          call getblk(zdi,2,ip,kp,ndp,nrecl1)
          call getblk(zrhs2r,3,ip,1,ndp,nrecl2)
          call getblk(zrhs2i,4,ip,1,ndp,nrecl2)
cdeb           write(6,*)'calling matmul; kp = ',kp,' ip=',ip
          call Y_MATMUL2(zdr,zdi,zrhsr,zrhsi,zrhs2r,zrhs2i,
     &                 isize,isize,nrhs)

          call putblk(zrhs2r,3,ip,1,ndp,nrecl2)
          call putblk(zrhs2i,4,ip,1,ndp,nrecl2)
260    continue
270    continue
c
       iend =  seconds(0.)
       bsfr_time = bsfr_time + iend - istart
       istatus =  seconds(0.)
       write(6,*)' STATUS REPORT: 2'
       write(6,*)' PROGRAM COMPLETED BACK SOLLVE',
     &           ' IN TIME', istatus -istart,'   SECONDS'
       endif

c       
       if( ndp .eq. 1)then
        call getblk(zir,1,1,1,ndp,nrecl1)
        call getblk(zii,2,1,1,ndp,nrecl1)
        call getblk(zrhsr,3,1,1,ndp,nrecl2)
        call getblk(zrhsi,4,1,1,ndp,nrecl2)
        call Y_MATMUL(zir,zii,zrhsr,zrhsi,zrhs2r,zrhs2i,
     &               isize,isize,nrhs)
        call putblk(zrhs2r,3,1,1,ndp,nrecl2)
        call putblk(zrhs2i,4,1,1,ndp,nrecl2)
       endif
c
      iend = seconds(0.)
      total_time =  iend - ibegin

c
      call output(zrhsr,zrhsi,zrhs2r,zrhs2i,isize,nrhs,ndp)
c
      write(6,*)'**************** PROBLEM PARAMETERS *****************'
      write(6,*)'PROBLEM SIZE = ',ncol,'   BLOCK SIZE',isize
      write(6,*)'NUMBER OF RIGHT HAND SIDES',nrhs
      write(6,*)'SUB-BLOCK SIZE',ib
      write(6,*)'---------------------------------------------------'  
c
      write(6,*)'*****************  PERFORMANCE TIME IN SECONDS *' 
      write(6,*)'total time',total_time,'  seconds'
      write(6,*)'factor time',factor_time,'  seconds'
      write(6,*)'backsolve/FR time',bsfr_time,'  seconds'
      write(6,*)'matmul time',matmul_time,'  seconds'
      write(6,*)'io time',io_time,'  seconds'
      write(6,*)'inverse time',inv_time,'  seconds'
      write(6,*)'---------------------------------------------------'  
      write(6,*)'lu diag. blk. time',lud_time,'  seconds'
      write(6,*)'triangular solver time',tsol_time,'  seconds'
      write(6,*)'---------------------------------------------------'  
c
      t1 = io_time
      t2 = matmul_time
      t3 = inv_time
      t4 = factor_time
      t5 = total_time
      t6 = bsfr_time

      ops_mullu=(ndpco*(ndpco+1)*(2*ndpco+1)/6 - ndpco*(ndpco+1)/2)*ndp
      ops_mulh = ndp*(ndp+1)*(2*ndp+1)/6 - ndp*(ndp+1) + ndp
      ops_mulo = ops_mul - ops_mulh - ops_mullu
cm      ops_io = 2*ndp*(ndp+1)*(2*ndp+1) - 3*ndp*(ndp+1) + 6*ndp
cm      ops_io = ops_io/6
      ops_inv = ( 8.0*(float(ib))**3) * ndp * ndpco
      float_ops = ( 8.0*(float(ncol))**3 )/3
      float_opst = float_ops + ( 8.0*(float(ncol))**2 )
c
      write(6,*)'*************  PERFORMANCE FIGURES FOR SOLVER *******'


      write(6,*)'NUMBER OF MATRIX MULTIPLICATIONS',ops_mul
      write(6,*)'NUMBER OF DISK READ/WRITES',ops_io
      write(6,*)'NUMBER OF FLOATING POINT OPERATIONS= ',float_opst
      ops_mu = ops_mulh *8.*(float(isize))**3
      ops_mu = ops_mu + ops_mullu *8.*(float(ib))**3
      ops_mu = ops_mu + (ops_mulo*ndpco) *8.*(float(ib))**3
      ops_i = 2.0*ops_io * nrecl1
      write(6,*)'---------------------------------------------------'  
      write(6,*)'MFLOPS FOR FACTORING',float_ops/(t4*1.0e6) 
      write(6,*)'OVERALL MFLOPS ',float_opst/(t5*1.0e6)
      if(ops_mul .ne. 0)write(6,*)'MATRIX MUlTIPLY MFLOPS ',
     &                  ops_mu/(t2*1.0e6)
      write(6,*)'INVERSION MFLOPS ',ops_inv/(t3*1.0e6)
      write(6,*)'DISK TRANSFER RATE',ops_i/(t1*1.0e6),' MBYTES/SEC'      
      write(6,*)'---------------------------------------------------'         
      write(6,*)'TOTAL EXECUTION TIME',t5,'  seconds'
      write(6,*)'TOTAL MATMUL TIME',t2,'  seconds'
      write(6,*)'TOTAL I/O TIME',t1,  '  seconds'
      write(6,*)'TOTAL INVERSION TIME',t3,'  seconds'
      write(6,*)'TOTAL FACTOR TIME',t4,  '  seconds'
      write(6,*)'TOTAL BACK SOLV / FORWARD REDU. TIME',t6, '  seconds'
c
c     deallocate arrays
      deallocate(zbr)
      deallocate(zdr)
      deallocate(zir)
      deallocate(zbi)
      deallocate(zdi)
      deallocate(zii)
      deallocate(zrhsr)
      deallocate(zrhsi)
      deallocate(zrhs2r)
      deallocate(zrhs2i)

      return
      end





