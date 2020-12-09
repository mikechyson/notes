#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define MAXLINE 1000
void expand(char s1[], char s2[])
{
  int i, j, k;
  char c;

  for (i = 0, j = 0; s1[i] != '\0'; i++)
    {
      if (s1[i+1] == '-' && (c = s1[i+2]) != '\0' &&
	  (('a' <= s1[i] && s1[i] <= 'z' && s1[i] <= c && c <= 'z') ||
	   ('A' <= s1[i] && s1[i] <= 'Z' && s1[i] <= c && c <= 'Z') ||
	   ('0' <= s1[i] && s1[i] <= '9' && s1[i] <= c && c <= '9')))
	{
	  k = 0;
	  while (k <= c - s1[i])
	    s2[j++] = s1[i] + k++;
	  i += 2;
	}
      else
	s2[j++] = s1[i];
    }
  s2[j] = '\0';
}

int main()
{
  char s1[] = "-a-z 0-9 A-E c-g-";
  char s2[MAXLINE];
  
  expand(s1, s2);

  printf("%s\n", s2);

  return 0;
}
