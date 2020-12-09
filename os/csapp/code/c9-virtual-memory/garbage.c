#include "csapp.h"

void garbage()
{
  int *p = (int *)Malloc(15213);

  return;			/* Array p is garbage at this point */
}

