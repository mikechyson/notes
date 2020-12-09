#include <stdio.h>

/* copy input to output; 2nd version */
int main()
{
  int c;

  printf("The value of EOF: %d\n", EOF);
  while ((c = getchar()) != EOF) {
    putchar(c);
  }
}
