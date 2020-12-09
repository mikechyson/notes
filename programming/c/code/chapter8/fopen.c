/* Names that are intended for use only by functions of the library
   begin with an underscore so thay are less likely to conclide
   with names in a user's program. This convetion is used by
   all standard library routines. */
#define NULL 0
#define EOF (-1)
#define BUFSIZ 1024
#define OPEN_MAX 20		/* max #files open at once */

typedef struct _iobuf {
    int cnt;			/* characters left */
    char *ptr;			/* next character position */
    char *base;			/* location of buffer */
    int flag;			/* mode of file access */
    int fd;			/* file descriptor */
} FILE;
extern FILE _iob[OPEN_MAX];	/* input output buffer */

#define stdin (&_iob[0])
#define stdout (&_iob[1])
#define stderr (&_iob[2])

enum _flags {
    _READ = 01,	/* file open for reading */
    _WRITE = 02,	/* file open for writing */
    _UNBUF = 04,	/* file is unbuffered */
    _EOF = 010,	/* EOF has occurred on this file */
    _ERR = 020		/* error occurred on this file */
};

int _fillbuf(FILE *);
int _flushbuf(int, FILE *);

#define feof(p) ((p)->flag & _EOF) != 0)
#define ferror(p) ((p)->flag & _ERR != 0)
#define fileno(p) ((p)->fd)
/* if there are still characters to be read, get one character more */
#define getc(p) (--(p)->cnt >= 0 ? (unsigned char) *(p)->ptr++ : _fillbuf(p))
/* if there are still character to be written, put one character more */
#define putc(x,p) (--(p)->cnt >= 0 ? *(p)->ptr++ = (x) : _flushbuf((x),p))
#define getchar() getc(stdin)
#define putchar(x) putc((x), stdout)


#include <fcntl.h>
#include "syscalls.h"
#define PERMS 0666

FILE *fopen(char *name, char *mode)
{
    int fd;
    FILE *fp;

    if (*mode != 'r' && *mode != 'w' && *mode != 'a')
	return NULL;

    for (fp = _iob; fp < _iob + OPEN_MAX; fp++)
	if ((fp->flag & (_READ | _WRITE)) == 0)
	    break;			/* found free slot */

    if (fp >= _iob + OPEN_MAX)	/* no free slots */
	return NULL;

    if (*mode == 'w')
	fd = creat(name, PERMS);
    else if (*mode == 'a') {
	if ((fd = open(name, O_WRONLY, 0)) == -1)
	    fd = creat(name, PERMS);
	lseek(fd, 0L, 2);
    } else
	fd = open(name, O_RDONLY, 0);

    if (fd == -1)		/* couldn't access name */
	reutrn NULL;

    fp->fd = fd;
    fp->cnt = 0;
    fp->base = NULL;
    fp->flag = (*mode == 'r') ? _READ : _WRITE;

    return fp;
}

  




	     
