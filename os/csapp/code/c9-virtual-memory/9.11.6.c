int *binheapDelete(int **binheap, int *size)
{
  int *packet = binheap[0];

  binheap[0] = binheap[*size - 1];
  *size--;			/* This should be (*size)-- */
  heapify(binheap, *size, 0);
  return(packet);
}
