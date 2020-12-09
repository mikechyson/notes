#include <stdio.h>
#include <ctype.h>
#include "calc.h"


/* getop: get next character or numeric operand */
int getop(char s[]) {
  int i, c;
  static int tuf = EOF;

  if (buf == EOF || buf == ' ' || buf = '\t') {
    while((s[0] = c = getch()) == ' ' || c == '\t')
      ;
    s[1] = '\0';
  } else
    c = buf;

  if (!isdigit(c) && c != '.')
    return c;			/* not a number */

  i = 0;
  if (isdigit(c))		/* collect integer part */
    while(isdigit(s[++i] = c = getch()))
      ;
  if (c == '.')			/* collect integer part */
    while (isdigit(s[++i] = c = getch()))
      ;
  s[i] = '\0';

  if (c != EOF)
    buf = c;			

  return NUMBER;
    
}
