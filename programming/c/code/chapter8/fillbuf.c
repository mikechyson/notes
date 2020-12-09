#include "syscalls.h"


/* _fillbuf: allocate and fill input buffer */
int _fillbuf(FILE *fp)
{
  int bufsize;

  if ((fp->flag & (_READ | _EOF_ERR)) != _READ) /* end of file */
    return EOF;

  bufsize = (fp->flag & _UNBUF) ? 1 : BUFSIZ; /* buffered or not */

  fp->ptr = fp->base;		/* point to the location of buffer */
  /* read bufsize bytes and update the count */
  fp->cnt = read(fp->fd, fp->ptr, bufsize);

  if (--fp->cnt < 0) {		/* if there are no characters left to read */
    if (fp->cnt == -1)		/* end of file */
      fp->flag |= _EOF;
    else			/* error */
      fp->flag |= _ERR;
    fp->cnt = 0;		/* no more character to read */
    return EOF;
  }
  return (unsigned char) *fp->ptr++; /* return the next character */
}
    

FILE _iob[OPEN_MAX] = {
		       /* count, ptr, base, flag, fd */
		       { 0, (char *) 0, (char *) 0, _READ, 0 },
		       { 0, (char *) 0, (char *) 0, _WRITE, 1 },
		       { 0, (char *) 0, (char *) 0, _WRITE | _UNBUF, 2 }
};
