#include <stdio.h>

/* copy input to output; 1st version */
int main()
{
  int c;
  int pre_c;

  c = getchar();
  pre_c = c;
  while (c!= EOF) {
    if (!(pre_c == ' ' && c == ' '))
      putchar(c);
    pre_c = c;
    c = getchar();
  }
}
