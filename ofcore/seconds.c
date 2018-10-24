#include <sys/time.h>
/* Transportable timeing function */

double seconds_ (delta)
double *delta;
{
     struct timeval tp;
     struct timezone tzp;
     int time_chk; 
     double secs;
 
/* ---------------------------------------------- */     
 
     time_chk = gettimeofday (&tp, &tzp);
 
     if (time_chk == 0)
        secs = (double) tp.tv_sec + (double) tp.tv_usec * (double) 1.e-6; 
     else
        secs = 0.0;
     if (*delta != 0) secs = secs - *delta;
 
     return (secs);
}
