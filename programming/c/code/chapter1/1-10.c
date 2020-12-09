#include <stdio.h>

/* /\* copy input to output; 2nd version *\/ */
/* int main() */
/* { */
/*   int c; */

/*   while ((c = getchar()) != EOF) { */
/*     if (c == '\t') { */
/*       putchar('\\'); */
/*       putchar('t'); */
/*     } */
/*     if (c == '\b') { */
/*       putchar('\\'); */
/*       putchar('b'); */
/*     } */
/*     if (c == '\\') { */
/*       putchar('\\'); */
/*       putchar('\\'); */
/*     } */
/*     if (c != '\t' && c != '\b' && c != '\\') */
/*       putchar(c); */
/*   } */
/* } */


/* copy input to output; 2nd version */
int main()
{
  int c, d;

  while ((c = getchar()) != EOF) {
    d = 0;
    if (c == '\t') {
      putchar('\\');
      putchar('t');
      d = 1;
    }
    if (c == '\b') {
      putchar('\\');
      putchar('b');
      d = 1;
    }
    if (c == '\\') {
      putchar('\\');
      putchar('\\');
      d = 1;
    }
    if (d == 0)
      putchar(c);
  }
  return 0;
}
