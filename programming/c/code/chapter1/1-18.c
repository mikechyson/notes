#include <stdio.h>

#define MAXLINE 1000

int getline_mike(char str[], int lim);

int main()
{
  int len;
  char line[MAXLINE];

  while ((len = getline_mike(line, MAXLINE)) > 0)
    if (len > 1)
	printf("%s", line);
  return 0;
}


int getline_mike(char str[], int lim)
{
  int c, i, j;
  i = j = 0;
  while ((c = getchar()) != EOF && c != '\n') /* not end of file nor return */
    {
      if (j < lim - 1)
	str[j++] = c;
      if (c != ' ' && c != '\t')
	i = j;			/* the last index of character that is not blank nor tab */
    }

  if (c == '\n') {
    str[i] = c;
    ++i;
  }
  str[i] = '\0';
  return i;

}
