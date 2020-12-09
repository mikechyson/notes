/* The standard library includes a version of qsort */
/* qsort: sort v[left]...v[right] into increasing order */
void qsort(int v[], int left, int right)
{
  int i, last;
  void swap(int v[], int i, int j);

  if (left >= right)		/* do nothing if array contains fewer than two elements */
    return;
  
  swap(v, left, (left + right)/2); /* move partition element */
  last = left;

  for (i = left + 1; i <= right; i++) /* partition */
    if (v[i] < v[left])
      swap(v, ++last, i);
  swap(v, left, last-1);	/* restore partition element */
  qsort(v, left, last-1);
  qsort(v, last+1, right);
}

/* swap: interchage v[i] and v[j] */
void swap(int v[], int i, int j)
{
  int temp;

  temp = v[i];
  v[i] = v[j];
  v[j] = temp;
}

