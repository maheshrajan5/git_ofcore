      subroutine getblk(z,iflag,ip,jp,ndp,nrecl)
      integer iunit,ip,jp,ndp,nrecl,nhalf,nhalf2
      double precision z(:,:)
c      double precision, dimension(2,2):: z(:,:)
      common /io/ireal1a,iimag1a,ireal1b,iimag1b,ireal2,iimag2
CMPF  ONDPU z
cmpf  mpl mpl_read
      
      nhalf = ndp/2
      nhalf2 = ndp - nhalf
       if( iflag .eq. 1)then
         if( ip .le. nhalf)then
           ioff = (jp-1)*nhalf*nrecl + (ip-1)*nrecl
           call mpl_read(z,ireal1a,ioff,nrecl)
          else
           ioff = (jp-1)*nhalf2*nrecl + (ip - 1 - nhalf)*nrecl
           call mpl_read(z,ireal1b,ioff,nrecl)
         endif
       endif

       if( iflag .eq. 2)then
         if( ip .le. nhalf)then
           ioff = (jp-1)*nhalf*nrecl + (ip-1)*nrecl
           call mpl_read(z,iimag1a,ioff,nrecl)
          else
           ioff = (jp-1)*nhalf2*nrecl + (ip - 1 - nhalf)*nrecl
           call mpl_read(z,iimag1b,ioff,nrecl)
         endif
       endif

       if( iflag .eq. 3)then
           ioff =  (ip-1)*nrecl
           call mpl_read(z,ireal2,ioff,nrecl)
       endif

       if( iflag .eq. 4)then
           ioff = (ip-1)*nrecl
           call mpl_read(z,iimag2,ioff,nrecl)
       endif

       return
       end

      subroutine putblk(z,iflag,ip,jp,ndp,nrecl)
      integer iunit,ip,jp,ndp,nrecl,nhalf
      double precision z(:,:)
c      double precision, dimension(2,2):: z(:,:)
      common /io/ireal1a,iimag1a,ireal1b,iimag1b,ireal2,iimag2

CMPF  ONDPU z
cmpf  mpl mpl_write
c#ifdef DEBUG
cmr      if(iflag .eq.3. or. iflag.eq.4)then
cmr      write(6,*)'DEBUG from putblk: iflag=', iflag,' ip= ',ip,' jp= '
cmr     &          ,jp
cmr      write(6,*)'z= ', z
cmr      endif
c#endif

      nhalf = ndp/2
      nhalf2 = ndp - nhalf
      
       if( iflag .eq. 1)then
         if( ip .le. nhalf)then
           ioff = (jp-1)*nhalf*nrecl + (ip-1)*nrecl
           call mpl_write(z,ireal1a,ioff,nrecl)
          else
           ioff = (jp-1)*nhalf2*nrecl + (ip - 1 - nhalf)*nrecl
           call mpl_write(z,ireal1b,ioff,nrecl)
         endif
       endif

       if( iflag .eq. 2)then
         if( ip .le. nhalf)then
           ioff = (jp-1)*nhalf*nrecl + (ip-1)*nrecl
           call mpl_write(z,iimag1a,ioff,nrecl)
          else
           ioff = (jp-1)*nhalf2*nrecl + (ip - 1 - nhalf)*nrecl
           call mpl_write(z,iimag1b,ioff,nrecl)
         endif
       endif

       if( iflag .eq. 3)then
           ioff =  (ip-1)*nrecl
           call mpl_write(z,ireal2,ioff,nrecl)
       endif

       if( iflag .eq. 4)then
           ioff = (ip-1)*nrecl
           call mpl_write(z,iimag2,ioff,nrecl)
       endif

       return
       end

