/* rudimentary storage allocator */

#define ALLOCSIZE 10000		/* size of available space */

static char allocbuf[ALLOCSIZE]; /* storage for alloc, private */
static char *allocp = allocbuf;	 /* next free position */

char *alloc(int n)		/* return pointer to n characters */
{
  if (allocbuf + ALLOCSIZE - allocp >= n) { /* it fits */
    allocp += n;
    return allocp - n;		/* old p */
  } else
    return 0;			/* C guarantees that zero is never a valid address for data */
}

void afree(char *p)		/* free storage pointed to by p */
{
  if (p >= allocbuf && p <= allocbuf + ALLOCSIZE)
    allocp = p;
}
