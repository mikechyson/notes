#include <ctype.h>
#include <stdio.h>
#include "getch.h"

#define SIZE 10

int getfloat(double *);

int main(void)
{
  int n;
  double array[SIZE], i;

  for (n = 0; n < SIZE && (i = getfloat(&array[n])) != EOF; n++) 
    if (i)
      printf("you input a number: %f\n", array[n]);

  return 0;
}


/* getint: get next integer from input into *pn */
int getfloat(double *pn)
{
  int c, sign;
  double power;

  while (isspace(c = getch())) /* skip white space */
    ;

  if (!isdigit(c) && c != EOF && c != '+' && c != '-') {
    ungetch(c); /* it is not a number */
    return 0;
  }

  sign = (c == '-') ? -1 : 1;
  if (c == '+' || c == '-')
    c = getch();

  for (*pn = 0.0; isdigit(c) && c != '.'; c = getch())
    *pn = 10.0 * *pn + (c - '0');

  if (c == '.')
    c = getch();

  for (power = 1.0; isdigit(c); c = getch()) {
    *pn = 10.0 * *pn + (c - '0');
    power *= 10;
  }
    
  *pn = sign * *pn / power;
  
  if (c != EOF)
    ungetch(c);
  
  return c;
}
