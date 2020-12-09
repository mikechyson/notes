#include <stdio.h>

/* ascii table symbol range (inclusive) */
#define MIN 32
#define MAX 127

int main()
{
  int c = EOF;
  int i,j;
  int array[MAX - MIN + 1];

  /* initialization */
  for (i = MIN; i <= MAX; i++)
    array[i] = 0;


  /* statistics */
  while ((c =getchar()) != EOF)
    {
      if (c >= MIN && c <= MAX)
	++array[c];
    }

  /* paint */
  for (i = MIN; i <= MAX; i++)
    {
      printf("|%c|", i);
      for (j = 1; j <= array[i]; j++)
	putchar('*');
      putchar('\n');
    }

  return 0;

}
