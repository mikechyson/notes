#include "csapp.h"

/* uses mmap to copy file fd to stdout */
void mmapcopy(int fd, int size)
{
  char *bufp;			/* ptr to memory-mapped VM area */
  /* (start, length, prot, flags, fd, offset) */
  bufp = Mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0); /* sinal checked version mmap */
  Write(1, bufp, size);					  /* 1: stdout */
  return;
}

/* mmapcopy driver */
int main(int argc, char **argv)
{
  struct stat stat;
  int fd;

  /* check for required command-line argument */
  if (argc != 2) {
    printf("usage: %s <filename>\n", argv[0]);
    exit(0);
  }

  /* copy the input argument to stdout */
  fd = Open(argv[1], O_RDONLY, 0);
  fstat(fd, &stat);		/* file state, to get the size of the file */
  mmapcopy(fd, stat.st_size);
  exit(0);
    
}
