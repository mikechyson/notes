#include <stdio.h>

#define IN 1			/* inside a word */
#define OUT 0			/* outside a word */

/* count lines, words, and characters in input */
int main()
{
  int c, state, pre_state;

  pre_state = state = OUT;

  while ((c = getchar()) != EOF) {
    if (c == ' ' || c == '\n' || c == '\t') {
      pre_state = state;
      state = OUT;
    }
    else if (state == OUT) {
      state = IN;
    }
    if (state == IN)
      putchar(c);
    else if (pre_state != OUT)
      putchar('\n');
  }
}

  
