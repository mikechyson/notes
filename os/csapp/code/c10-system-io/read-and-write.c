#include <unistd.h>

ssize_t read(int fd, void *buf, size_t n);
/* return numbers of bytes read if ok, 0 on EOF, -1 on error */

ssize_t write(int fd, void *buf, size_t n);
/* return numbers of bytes write if ok, -1 on error */


