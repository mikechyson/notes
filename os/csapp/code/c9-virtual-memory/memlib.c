#include "csapp.h"
#include <stdio.h>


/* private global variables */
static char *mem_heap;		/* pointer to first byte of heap */
static char *mem_brk;		/* pointer to last byte of heap plus 1 */
static char *mem_max_addr;	/* max legal heap addr plus 1 */

/* 
 * mem_init: initialize the memory system model
 */
void mem_init(void)
{
    mem_heap = (char *) Malloc(MAX_HEAP);
    mem_brk = (char *) mem_heap;
    mem_max_addr = (char *) (mem_heap + MAX_HEAP);
}

/* 
 * mem_sbrk: simple model of the sbrk function. extends the heap by
 * incr bytes and returns the start address of the new data. In this
 * model the heap can not be shrink.
 */
void *mem_sbrk(int incr)
{
    char *old_brk = mem_brk;

    if ((incr < 0) || ((mem_brk + incr) > mem_max_addr)) {
	errno = ENOMEM;
	fprintf(stderr, "ERROR: mem_brk failed. Ran out of memory...\n");
	return (void *)-1;
    }
    mem_brk += incr;
    return (void *)old_brk;
}
