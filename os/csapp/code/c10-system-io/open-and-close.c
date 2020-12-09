#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int open(char *filename, int flags, mode_t mode);
/* return new file descriptor if ok, -1 on error */

/* 
flags:
O_RDONLY: read only
O_WRONLY: write only
O_RDWR: read and write

The flags argument can also be or-ed with one or more bit masks
that provide additional instructions for writing:
O_CREATE: if the file doesn't exist, then create a truncated (emtpy version of it).
O_TRUNC: if the file already exist, then truncate it.
O_APPEND: before each writting operation, set the file position to the end of the file.
 */

int main(void)
{
  int fd = open("foo.txt", O_RDONLY, 0);
  int fd2 = open("foo2.txt", O_WRONLY|O_APPEND, 0);

  
}

#define DEF_MODE S_IRUSER|S_IWUSER|S_IRGPP|S_IWGPP|S_IROTH|S_IWOTH
#define DEF_UMASK S_IWGPP|S_IWOTH

umask(DEF_UMASK);
fd = open("foo.txt", O_CREATE|O_TRUNC|O_WRONLY, DEF_MODE);




#include <unistd.h>
int close(int fd);
/* return 0 if ok, -1 on error */
