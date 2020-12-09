#include <stdio.h>

/* squeeze: delete all c from s */
int any(char s1[], char s2[])
{
  int i, j, k;

  for (k = 0; s2[k] != '\0'; k++)
    for (i = j = 0; s1[i] != '\0'; i++)
      if (s1[i] == s2[k])
	return i;
  return -1;
}

int main()
{
  int i;
  char s1[] = "abcdefg";
  char s2[] = "xyz";
  i = any(s1,s2);
  printf("%d\n",i);
  return 0;
}
