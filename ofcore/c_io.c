#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>

#define PMODE 0644 /* RW for owner, R for group, others */
#define SIZ  65536   /* optimal bytes per read */

mpl_write_ (float* mat_adr, int* fd_ptr, int* offset_ptr, int* length_ptr)
{
        int fd;
        int offset;
        int length;

	fd = *fd_ptr;
	offset = *offset_ptr;
	length = *length_ptr;

	lseek (fd, offset, SEEK_SET);
          if((write (fd, mat_adr, length)) < 0) {
                printf("error in write\n");
                exit(1);}
}

mpl_open_ (fd1_ptr, fd2_ptr, fd3_ptr, fd4_ptr, fd5_ptr, fd6_ptr)

        int *fd1_ptr;
        int *fd2_ptr;
        int *fd3_ptr;
        int *fd4_ptr;
        int *fd5_ptr;
        int *fd6_ptr;

{	

        int  fd1;
        int  fd2;
        int  fd3;
        int  fd4;
        int  fd5;
        int  fd6;

	fd1 = *fd1_ptr;
	fd2 = *fd2_ptr;
	fd3 = *fd3_ptr;
	fd4 = *fd4_ptr;
	fd5 = *fd5_ptr;
	fd6 = *fd6_ptr;



	if((fd1 = open ("ara.1", O_RDWR | O_CREAT, PMODE))==-1){
          printf("error opening file ara.1\n");
          exit(1);}
	if((fd2 = open ("aia.1", O_RDWR | O_CREAT, PMODE))==-1){
          printf("error opening file aia.1\n");
          exit(1);}
	if((fd3 = open ("arb.1", O_RDWR | O_CREAT, PMODE))==-1){
          printf("error opening file arb.1\n");
          exit(1);}
	if((fd4 = open ("aib.1", O_RDWR | O_CREAT, PMODE))==-1){
          printf("error opening file aib.1\n");
          exit(1);}
	if((fd5 = open ("rhsr.1", O_RDWR | O_CREAT, PMODE))==-1){
          printf("error opening file rhsr.1\n");
          exit(1);}
	if((fd6 = open ("rhsi.1", O_RDWR | O_CREAT, PMODE))==-1){
          printf("error opening file rhsi.1\n");
          exit(1);}

        *fd1_ptr = fd1;
	*fd2_ptr = fd2;
	*fd3_ptr = fd3;
	*fd4_ptr = fd4;
	*fd5_ptr = fd5;
	*fd6_ptr = fd6;

}



mpl_read_ (float* mat_adr, int* fd_ptr, int* offset_ptr, int* length_ptr)
{
        int fd;
        int offset;
        int length;

	fd = *fd_ptr;
	offset = *offset_ptr;
	length = *length_ptr;

	lseek (fd, offset, SEEK_SET);
          if((read (fd, mat_adr, length)) < 0) {
                printf("error in read\n");
                exit(1);}
}







