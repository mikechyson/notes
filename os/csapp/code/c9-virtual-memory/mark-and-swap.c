typedef void *ptr


/* If p points to some word in an allocated block, it returns a */
/* pointer b to the beginning of that block. Returns NULL otherwise. */
ptr isPtr(ptr p);

/* Returns true if block b is already marked. */
int blockMarked(ptr b);

/* Returns true if block b is allocated. */
int blockAllocated(ptr b);

/* Marks block b. */
void markBlock(ptr b);

/* Returns the length in words (excluding the header) of block b. */
int length(ptr b);

/* Changes the status of block b from marked to un- marked. */
void unmarkBlock(ptr b);

/* Returns the successor of block b in the heap. */
ptr nextBlock(ptr b);


void mark(ptr p)
{
    if ((b = isPtr(p)) == NULL)
	return;			/* garbage */

    if (blockMarked(b))
	return;			/* already marked */

    markBlock(b);		/* mark if not marked */
    len = length(b);
    for (int i = 0; i < len; i++)
	mark(b[i]);
    return;
}

void sweep(ptr b, ptr end)
{
    while (b < end) {
	if (blockMarked(b))
	    unmarkBlock(b);
	else if (blockAllocated(b))
	    free(b);

	b = nextBlock(b);
    }
    return;
}
