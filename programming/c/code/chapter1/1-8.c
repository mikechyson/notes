#include <stdio.h>

/* count lines in input */
int main()
{
  int c, nl, nb, nt;

  nl = 0;
  nb = 0;
  nt = 0;
  while ((c = getchar()) != EOF) {
    if (c == '\n')
      ++nl;
    if (c == ' ')
      ++nb;
    if (c == '\t')
      ++nt;
  }
  printf("lines: %d\nblacks: %d\ntabs: %d\n", nl, nb, nt);
}

