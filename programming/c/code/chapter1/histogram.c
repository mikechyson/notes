#include <stdio.h>

#define IN 1			/* inside a word */
#define OUT 0			/* outside a word */

/* count lines, words, and characters in input */
int main()
{
  int c, nw, state;	/* character, num line, num word, num character */

  state = OUT;
  nw = 0;
  while ((c = getchar()) != EOF) {
    if (c == ' ' || c == '\n' || c == '\t')
      state = OUT;
    else if (state == OUT) {
      state = IN;
      ++nw;
    }
  }
  
  int i;
  for (i = 0; i < nw; i++) 
    putchar('#');
  putchar('\n');

}

  
