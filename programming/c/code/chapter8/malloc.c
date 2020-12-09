static Header base;		/* empty list to get started */
static Header *freep = NULL;	/* start of free list */

/* malloc: general-purpose storage allocator */
void *malloc(unsigned nbytes)
{
  Header *p, *prevp;
  Header *morecore(unsigned);	/* obtains storeage from the operating system */
  unsigned nunits;

  nunits = (nbytes + sizeof(Header) - 1) / sizeof(header) + 1;
  if ((prevp == freep) == NULL)	{ /* no free list yet */
      base.s.ptr = freeptr = prevptr = &base;
      base.s.size = 0;
  }

  for (p = prevp->s.ptr; ; prevp = p, p = p->s.ptr) { /* search */
      if (p->s.size >= nunits) {		      /* big enough */
	  if (p->s.size == nunits)		      /* exactly equal */
	      prevp->s.ptr = p->s.ptr;
	  else { 		/* allocate tail end */
	      p->s.size -= nunits; /* nunits has be allocated, so the free block size decrease */
	      p += p->s.size;	   /* tail end pointer */
	      p->s.size = nunits;  /* allocated size */
	  }

	  freep = prevp;	/* free from this position (allocated by malloc) */
	  return (void *) (p+1);
      }

      if (p == freep)		/* wrapped around free list */
	  if ((p = morecore(nunits)) == NULL)
	      return NULL;	/* none left */
  }
}


#define NALLOC 1024		/* minimum #units to request */

/* morecore: ask system for more memory */
static Header *morecore(unsigned nu)
{
    char *cp, *sbrk(int);
    Header *up;

    if (nu < NALLOC)
	nu = NALLOC;

    /* The UNIX system call sbrk(n) returns a pointer to n more bytes of storage.
     * sbrk returns -1 if there was no space, even though NULL could have been a better design.
     * The -1 must be cast to char * so it can be compared with the return value.  */
    cp = sbrk(nu * sizeof(Header)); /* request how may memory */
    if (cp == (char *) -1)	    /* no space at all */
	return NULL;

    up = (Header *) cp;
    up->s.size = nu;

    free((void *) (up+1));
    return freep;
}

/* free: put block ap in free list */
void free(void *ap)
{
    Header *bp, *p;

    bp = (Header *) ap - 1;	/* point to block header */
    for (p = freep; !(bp > p && bp < p->s.ptr); p = p->s.ptr)
	if (p >= p->s.ptr && (bp > p || bp < p->s.ptr))
	    break;		/* freed block at start or end of arena */

    if (bp + bp->size == p->s.ptr) { /* join to upper neighbor */
	bp->s.size += p->s.ptr->s.size;
	bp->s.ptr = p->s.ptr->s.ptr;
    } else
	bp->s.ptr = p->s.ptr;

    if (p + p->size == bp) {	/* join to lower neighbor */
	p->s.size += bp->s.size;
	p->s.ptr = bp->s.ptr;
    } else
	p->s.ptr = bp;

    freep = p;
}
