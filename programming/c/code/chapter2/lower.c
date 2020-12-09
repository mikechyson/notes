#include <stdio.h>

/* lower: convert c to lower case; ASCII only */

#define SIZE 3
int lower(int c)
{
  if (c >= 'A' && c <= "Z")
    return c + 'a' - 'A';
  else
    return c;
}

int main()
{
  char s[SIZE];

  s[0] = 'A';
  s[1] = 'B';
  s[2] = 'c';

  int i;
  for (i = 0; i < SIZE; ++i)
    {
      s[i] = lower(s[i]);
      printf("%c\n",s[i]);
    }

    
}


/* there is a bug  */
/* "ABc" -> "ab?" */
