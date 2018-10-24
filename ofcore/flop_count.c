#include <stdio.h>
#include <papi.h>

int mypapi_init_ (int *, char ***);
int mypapi_finalize_ ();


long_long flops_start, flops_end;
long_long start_usec, end_usec;
int EventSet;

int mypapi_init_ (int *argc, char ***argv) {

  int iret;

  if (PAPI_library_init (PAPI_VER_CURRENT) != PAPI_VER_CURRENT) {
    fprintf (stderr, "PAPI library init error!\n");
  }
  EventSet = PAPI_NULL;
  if (PAPI_create_eventset (&EventSet) != PAPI_OK) {
    fprintf (stderr, "PAPI_create_eventset failed\n");
  }
  if (PAPI_add_event (EventSet, PAPI_FP_INS) != PAPI_OK) {
    fprintf (stderr, "PAPI_add_event failed\n");
  }
  if (PAPI_start (EventSet) != PAPI_OK) {
    fprintf (stderr, "PAPI_start failed\n");
  }
  start_usec = PAPI_get_real_usec();
  PAPI_read (EventSet, &flops_start);

  return iret;

}

int mypapi_finalize_ () {

  int iret;
  long_long flops_me, flops, elapsed_usec, max_elapsed_usec;
  double dble_flops,dble_elapsed_usec,total_mflops,dble_elapsed_sec;
  int me;

  PAPI_read (EventSet, &flops_end);
  end_usec = PAPI_get_real_usec();
  flops_me = flops_end - flops_start;
  elapsed_usec = end_usec - start_usec;
  flops = flops_me;
  max_elapsed_usec = elapsed_usec;
  dble_flops = (double)flops;
  dble_elapsed_usec = (double)max_elapsed_usec;
  dble_elapsed_sec = dble_elapsed_usec/1.0e06;
  printf ("*********** TOTAL PROGRAM PAPI INFORMATION  START **********\n");
  printf ("TOTAL FLOPS = %e\n", dble_flops);
  printf ("ELAPSED WALL TIME SECS = %e\n", dble_elapsed_sec);
  total_mflops = (dble_flops/dble_elapsed_usec);
  printf ("TOTAL MFLOPS = %e\n", total_mflops);
  printf ("*********** TOTAL PROGRAM PAPI INFORMATION  END ************\n");

  return iret;

}

