#include <ctype.h>
#include <stdio.h>
#include "getch.h"

#define SIZE 10

int getint(int *);

int main(void)
{
  int n, array[SIZE], i;

  for (n = 0; n < SIZE && (i = getint(&array[n])) != EOF; n++) 
    if (i)
      printf("you input a number: %d\n", array[n]);

  return 0;
}


/* getint: get next integer from input into *pn */
int getint(int *pn)
{
  int c, sign, c2;

  while (isspace(c = getch()))	/* skip white space */
    ;

  if (!isdigit(c) && c != EOF && c != '+' && c != '-') {
    ungetch(c);			/* it is not a number */
    return 0;
  }

  sign = (c == '-') ? -1 : 1;
  if (c == '+' || c == '-') {
    if (!isdigit(c2 = getch())) { /* + or - not followed by a digit */
      ungetch(c2);
      ungetch(c);
      return 0;
    } else
      c = c2;
  }


  for (*pn = 0; isdigit(c); c = getch())
    *pn = 10 * *pn + (c - '0');

  *pn *= sign;

  if (c != EOF)
    ungetch(c);

  return c;
  
}
