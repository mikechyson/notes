#include <limits.h>
#include <stdio.h>

/* itoa: convert n to characters in s */
void itoa(int n, char s[]) {
  int i, sign;

  sign = n;
  i = 0;
  
  do {
    s[i++] = abs(n % 10) + '0';
  } while (n /= 10);	/* delete it */

  if (sign < 0)
    s[i++] = '-';

  s[i] = '\0';
  reverse(s);
}
