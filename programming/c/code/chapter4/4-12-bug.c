#include <stdio.h>

#define MAXLINE 1000

void itoa(int n, char s[], int i);

int main(void)
{
  char s[MAXLINE];
  int n = 1234;

  itoa(n, s, 0);

  printf("%s\n", s);

  return 0;
}


/* itoa: convert n to characters in s */
void itoa(int n, char s[], int i)
{

  
  if (n < 0)
    {
      s[i++] = '-';
      n = -n;
    }

  if (n / 10)
    itoa(n / 10, s, i);
  
  s[i++] = n % 10 + '0';
  s[i] = '\0';

}

/* there is bug in this solution. */
