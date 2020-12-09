#include <stdio.h>

/* A char is just a small integer, so chars may be freely used in arithmetic expressions.  */

/* atoi: convert s to integer */
int atoi(char s[])
{
  int i, n;
  n = 0;
  for (i = 0; s[i] >= '0' && s[i] <= '9'; ++i)
    n = 10 * n + (s[i] - '0');	/* decimal, the base is 10 */
  return n;
}

int main()
{
  int n;
  char s[2];
  
  s[0] = '1';
  s[1] = '2';

  n = atoi(s);

  printf("%d\n", n);
  
  return 0;  
    
}
