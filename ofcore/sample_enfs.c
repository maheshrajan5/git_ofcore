/* 
	Sample C code to open a file in the current working directory
	and write to it. If the cwd is on the /enfs file system, the
	"enfs:" prefix is added to the path.
*/

#include <mpi.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <math.h>
#ifdef __linux__
#include <linux/limits.h>
#endif

int 
main (int argc, char** argv)
{
  char filename[FILENAME_MAX], 
       path[PATH_MAX],
       enfs_prefix[] = "enfs:", 
       no_prefix[]   = "",
       *prefix;
  double array[2] = {M_E, M_PI};
  int rank, n, array_size = sizeof(array)/sizeof(double);
  FILE *file;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  /* 
   * Find the current working directory 
   */
  if (getcwd(path, PATH_MAX) == NULL) {
	printf ("Failed to get cwd in process %d: %s\n",
	        rank, strerror(errno));
	MPI_Abort(MPI_COMM_WORLD, errno);
  }

  /* 
   * Select the prefix to use based on cwd, either "enfs:" or "" 
   */
  prefix = strstr(path, "enfs") ? enfs_prefix : no_prefix;

  /*
   * Build the full path to the file, rank is appended to
   * create a unique filename for each process
   */
  if (argc > 1)
    sprintf ( filename, "%s%s/%s.%d", prefix, path, argv[1], rank );
  else
    sprintf ( filename, "%s%s/%s.%d", prefix, path, "outfile", rank );

  /* 
   * Open the file 
   */
  if ((file = fopen(filename, "w")) == NULL) {
	printf ("Failed to open %s in process %d: %s\n", 
	        filename, rank, strerror(errno));
	MPI_Abort(MPI_COMM_WORLD, errno);
  }

  /* 
   * Write to the file 
   */
  if ((n = fwrite(array, sizeof(double), array_size, file)) != array_size) { 
	printf("Incorrect number of elements written: %d, array size: %d in process %d\n", 
	       n, array_size, rank);
	MPI_Abort(MPI_COMM_WORLD, errno);
  }

  fclose(file);  
  MPI_Finalize();
}
