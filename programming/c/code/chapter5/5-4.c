#include <stdio.h>
#include <string.h>

#define MAXLINE 1000

int strend(char *s, char *t);

int main(void)
{
  char s[] = "hello world";
  char t[] = "world!";

  printf("%d\n", strend(s, t));
}


/* strend: returns 1 if the string t occurs at the end of the string s
   and zero otherwise */
int strend(char *s, char *t)
{
  int slen = strlen(s);
  int tlen = strlen(t);

  
  if (slen >= tlen) {
    s = s + slen - tlen;
    while (*s++ == *t++)
      if (!*s)
	return 1;
  }

  return 0;
}
  
