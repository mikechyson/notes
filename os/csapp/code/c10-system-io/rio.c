#include "csapp.h"

ssize_t rio_readn(int fd, void *usrbuf, size_t n);
/* return number of bytes transferred if ok, 0 on EOF, -1 on error */

ssize_t rio_writen(int fd, void *usrbuf, size_t n);
/* return number of bytes transferred if ok, -1 on error */




ssize_t rio_readn(int fd, void *usrbuf, size_t n)
{
    size_t nleft = n;
    ssize_t nread;
    char *bufp = usrbuf;

    while (nleft > 0) {
	if ((nread = read(fd, bufp, nleft)) < 0) {
	    if (errno == EINTR)	/* interrupted by sig handler return */
		nread = 0;		/* and call read() again */
	    else
		return -1;		/* errno set by read */
	}
	else if (nread == 0)
	    break;		/* EOF */

	nleft -= nread;
	bufp += nread;
    }

    return (n - nleft);		/* return >= 0 */
}



      
ssize_t rio_writen(int fd, void *usrbuf, size_t n)
{
    size_t nleft = n;
    ssize_t nwritten;
    char *buf = usrbuf;

    while (nleft > 0) {
	if ((nwritten = write(fd, bufp, nleft)) < 0) {
	    if (errno == EINTR)
		nwritten = 0;
	    else
		return -1;
	}
	nleft -= nwritten;
	bufp += nwritten;
    }

    return n;
}


/* The rio_readinitb function is called once per open descriptor. 
It associates the descriptor fd with a read buffer of type rio_t at address rp. */
void rio_readinitb(rio_b *rp, int fd); /* rp: read pointer; fd: file descriptor */

/* read the next line from the rp,  (including the terminating newline character), 
copies it to memory location usrbuf, and 
terminates the text line with the NULL (zero) character. */
/* reads at most maxlen-1 bytes, leaving room for the terminating NULL character. */
ssize_t rio_readlineb(rio_t *rp, void *usrbuf, size_t maxlen);

ssize_t rio_readnb(rio_t *rp, void usrbuf, size_t n);


#define RIO_BUFSIZE 8192
typedef struct {
    int rio_fd;			/* descriptor for this internal buffer */
    int rio_cnt;		/* unread bytes in internal buffer */
    char *rio_bufptr;		/* next unread tyte in internal buffer */
    char rio_buf[RIO_BUFSIZE];	/* internal buffer */
} rio_t;


void rio_readinitb(rio_t *rp, int fd)
{
    rp->rio_fd = fd;
    rp->rio_cnt = 0;
    rp->rio_bufptr = rp->rio_buf;
}

static ssize_t rio_read(rio_t *rp, char *usrbuf, size_t n)
{
    int cnt;

    while (rp->cnt <= 0) {	/* refill if buf is empty */
	rp->cnt = read(rp->rio_fd, rp->rio_buf, sizeof(rp->rio_buf));

	if (rp->cnt < 0) {
	    if (errno != EINTR) /* interrupted by sig handler return */
		return -1;
	}
	else if (rp->rio_cnt == 0) /* EOF */
	    return 0;
	else
	    rp->rio_bufptr = rp->rio_buf; /* reset buffer ptr */
    }

    /* copy min(n, np->rio_cnt) bytes from internal buf to user buf */
    cnt = n;
    if (rp->rio_cnt < n)
	cnt = rp->rio_cnt;

    memcpy(usrbuf, rp->rio_bufptr, cnt);
    rp->rio_bufptr += cnt;
    rp->rio_cnt -= cnt;
    return cnt;
}



	
ssize_t rio_readlineb(rio_t *rp, void *usrbuf, size_t maxlen)
{
    int n, rc;
    char c, *bufp = usrbuf;

    for (n = 1; n < maxlen; n++) {
	if ((rc = rio_read(rp, &c, 1)) == 1) {
	    *bufp++ = c;
	    if (c == '\n') {
		n++;
		break;
	    }
	} else if (rc == 0) {
	    if (n == 1)		/* EOF, no data read */
		return 0;
	    else
		break;		/* EOF, some data was read */
	} else
	    return -1;		/* error */
    }

    *bufp = 0;
    return n-1;
}

ssize_t rio_readnb(rio_t *rp, void usrbuf, size_t n)
{
    size_t nleft = n;
    ssize_t nread;
    char *bufp = usrbuf;

    while (nleft > 0) {
	if ((nread = rio_read(rp, bufp, nleft)) < 0)
	    return -1;		/* errno set by read() */
	else if (nread == 0)
	    break;		/* EOF */

	nleft -= nread;
	bufp += nread;
    }

    return (n - nleft);		/* return >= 0 */
}
