#include <stdio.h>
#include <string.h>
#include <stdlib.h>

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


/* itoa: convert n to characters in s */
void itoa(int n, char s[], int minlen) {
  int i, sign;

  sign = n;
  i = 0;
  
  do {
    s[i++] = abs(n % 10) + '0';
  } while (n /= 10);	/* delete it */

  
  if (sign < 0)
    s[i++] = '-';

  while(i < minlen)
    s[i++] = ' ';


  s[i] = '\0';
  reverse(s);
}

int main(){
  char s[MAXLENGTH];
  int minlen = 10;
  int n;

  for (n = -16; n <= 16; n++){
    itoa(n, s, minlen);
    printf("%s\n", s);
  }


  return 0;
}
