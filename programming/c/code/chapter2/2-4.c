#include <stdio.h>

/* squeeze: delete all c from s */
void squeeze(char s1[], char s2[])
{
  int i, j, k;

  for (k = 0; s2[k] != '\0'; k++)
    {
      for (i = j = 0; s1[i] != '\0'; i++)
	if (s1[i] != s2[k])
	  s1[j++] = s1[i];
      s1[j] = '\0';
    }
}

int main()
{
  char s1[] = "abcdefg";
  char s2[] = "abc";
  squeeze(s1,s2);
  printf("%s\n",s1);
  return 0;
}
