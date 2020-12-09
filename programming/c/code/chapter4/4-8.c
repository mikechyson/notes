#include <stdio.h>

int getch(void);
void ungetch(int);

int main()
{
  ungetch('a');
  ungetch('b');
  putchar(getch());

  return 0;
}


int buf = -1;			/* buffer for ungetch */

int getch(void)
{
  int b = buf;
  if (b > -1) {			/* there is a buffered value */
      buf = -1;
      return b;
  }
  return getchar();
}

void ungetch(int c)		/* bush character back on input */
{
  buf = c;
}
