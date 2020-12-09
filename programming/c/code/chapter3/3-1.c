#include <stdio.h>

#define SIZE 4

/* binsearch: find x in v[0] <= v[1] <= ... <= v[n-1] */
/* int binsearch(int x, int v[], int n) */
/* { */
/*   int low, high, mid; */

/*   low = 0; */
/*   high = n -1; */

/*   while(low < high) */
/*     { */
/*       mid = (low + high) / 2; */
/*       if (x < v[mid]) */
/* 	high = mid - 1; */
/*       else  */
/* 	low = mid; */
/*     } */
/*   return (x == v[low]) ? low : -1; */
/* } */

/* there is a bug in the above version, but i don't know where it is */
/* when the input is 2, size is 4, the output can not be computed */
/* but the following version is ok */

int binsearch(int x, int v[], int n)
{
  int low, high, mid;

  low = 0;
  high = n -1;

  while(low < high)
    {
      mid = (low + high) / 2;
      if (x > v[mid])
	low = mid + 1;
      else 
	high = mid;
    }
  return (x == v[low]) ? low : -1;
}


int main()
{
  int i, n, result;
  int values[SIZE];
  n = SIZE;

  for (i = 0; i < n; i++)
    values[i] = i;

  for (i = 0; i < n; i++)
    {
      result = binsearch(i, values, n);
      printf("%d\n", result);
    }

  return 0;
}
