#include <stdio.h>

#define MAXLINE 1000
#define TABWIDTH 7


int getline_mike(char s[], int lim);
void detab(char to[], char from[]);


int main()
{
  int len;			/* current line length */
  char line[MAXLINE];		/* current intput line */
  char nline[MAXLINE]; 		/* detabed line saved */

  while ((len = getline_mike(line, MAXLINE)) > 0)
    {
      detab(nline, line);
      printf("%s", nline);
    }

  return 0;

}


int getline_mike(char s[], int lim)
{
  int c, i;

  for (i = 0; i < lim-1 && (c=getchar()) != EOF && c != '\n'; ++i)
    s[i] = c;
  if (c == '\n') {
    s[i] = c;
    ++i;
  }
  s[i] = '\0';
  return i;
}

void detab(char to[], char from[])
{
  int i, j, n;
  i = j = n = 0;

  while((to[j] = from[i]) != '\0')
    {
      if (to[j] == '\t')
	for (n = 0; n < TABWIDTH; ++n, ++j)
	  to[j] = ' ';
      else
	++j;

      ++i;
    }
}
