      subroutine tsolver(zbr,zbi,zdr,zdi,isize,ib,ndpco)
C
c
      double precision, dimension(isize,isize):: zdr,zdi,zbr,zbi
      double precision, allocatable, dimension(:,:):: zbrco
      double precision, allocatable, dimension(:,:):: zbico
      double precision, allocatable, dimension(:,:):: zdrco
      double precision, allocatable, dimension(:,:):: zdico
      double precision, allocatable, dimension(:,:):: zirco
      double precision, allocatable, dimension(:,:):: ziico
      real ops_mul,matmul_time,inv_time
      common /stat/ops_mul, matmul_time, inv_time
      real*8 istart,iend,seconds
c
c     allocate arrays
      allocate(zdrco(ib,ib))
      allocate(zdico(ib,ib))
      allocate(zbrco(isize,ib))
      allocate(zbico(isize,ib))
      allocate(zirco(isize,ib))
      allocate(ziico(isize,ib))

CMPF  ONDPU zdr,zdi,zbr,zbi
CMPF  ONDPU zbrco,zdrco,zirco
CMPF  ONDPU zbico,zdico,ziico
cmpf  mpl Y_Matmul, Y_Matmul2
c
c     zd contains the diagonal i/o block

c     solve x*upper = a(ip,kp) for x;ip,kp from ofsolve
c
       do 250 kp = 1,ndpco
        
         call copyin(zdr,zdrco,isize,ib,kp,kp)  ! get diag inverse
         call copyin(zdi,zdico,isize,ib,kp,kp)       

         call copy2in(zbr,zbrco,isize,ib,kp)
         call copy2in(zbi,zbico,isize,ib,kp)       

         istart = seconds(0.)
         call Y_MATMUL(zbrco,zbico,zdrco,zdico,zirco,ziico,isize,ib,ib)
         iend =  seconds(0.)
         ops_mul = ops_mul + 1
         matmul_time = matmul_time + iend - istart

         call copy2out(zbr,zirco,isize,ib,kp)
         call copy2out(zbi,ziico,isize,ib,kp)       

c        save zirco,ziico for use in 240 loop       
 
         do 240 jp = kp+1 , ndpco

            call copyin(zdr,zdrco,isize,ib,kp,jp)  ! u12 ,u13 etc
            call copyin(zdi,zdico,isize,ib,kp,jp)       

            call copy2in(zbr,zbrco,isize,ib,jp)
            call copy2in(zbi,zbico,isize,ib,jp)       

            istart = seconds(0.)
         call Y_MATMUL2(zirco,ziico,zdrco,zdico,zbrco,zbico,isize,ib,ib)
            iend =  seconds(0.)
            ops_mul = ops_mul + 1
            matmul_time = matmul_time + iend - istart
            call copy2out(zbr,zbrco,isize,ib,jp)

            call copy2out(zbi,zbico,isize,ib,jp)       

240      continue
250    continue

c
c     solve y*lower = x for a(ip,kp) for y;ip,kp from ofsolve
c
       do 350 kp = ndpco,1,-1

         do 340 jp =  kp-1, 1, -1

            call copyin(zdr,zdrco,isize,ib,kp,jp)  ! L32,L31etc
            call copyin(zdi,zdico,isize,ib,kp,jp)       

            call copy2in(zbr,zbrco,isize,ib,jp)   ! to update
            call copy2in(zbi,zbico,isize,ib,jp)   !x12,x22,x32 etc    

            call copy2in(zbr,zirco,isize,ib,kp)   !y13,y23,y33 etc
            call copy2in(zbi,ziico,isize,ib,kp)   ! to right    

           istart = seconds(0.)
        call Y_MATMUL2(zirco,ziico,zdrco,zdico,zbrco,zbico,isize,ib,ib)
            iend =  seconds(0.)
            ops_mul = ops_mul + 1
            matmul_time = matmul_time + iend - istart

            call copy2out(zbr,zbrco,isize,ib,jp)
            call copy2out(zbi,zbico,isize,ib,jp)   !y12,y22,y32 etc    

340      continue
350    continue

c
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


