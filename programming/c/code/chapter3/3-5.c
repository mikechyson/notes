#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define SEP 10
#define MAXBASE 36
#define MAXLENGTH 1000

/* reverse: reverse string s in place */
void reverse(char s[])
{
  int c, i, j;

  for (i = 0, j = strlen(s)-1; i < j; i++, j--)
    {
      c = s[i];
      s[i] = s[j];
      s[j] = c;
    }
}


int itob(int n, char s[], int b) {
  char digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  int i, sign, remainder;

  sign = n;
  i = 0;

  if (b < 2 || b > 36) {
    printf("Cannot support base %d\n", b);
    exit(2);
  }

  do {
    s[i++] = digits[abs(n %b)];
  } while (n /= b);
  
  if (sign < 0)
    s[i++] = '-';

  s[i] = '\0';
  reverse(s);

  return 0;
}


int main(){
  char s[MAXLENGTH];
  int i;
  int n = 255;

  for (i = 2; i <= 36; i++) {
    itob(n, s, i);
    printf("Decimal %d in base %-2d : %s\n", n, i, s);
  }

  return 0;
}
