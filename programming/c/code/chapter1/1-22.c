#include <stdio.h>

#define MAXLINE 1000
#define FOLDLENGTH 10


int getline_mike(char s[], int lim)
{
  int c, i;

  for (i = 0; i < lim-1 && (c=getchar()) != EOF && c != '\n'; ++i)
    s[i] = c;

  if (c == '\n')
    {
      s[i] = c;
      ++i;
    }

  s[i] = '\0';
  return i;
      
}

/* there is a bug:
   the output will contain the previous characters
   longer than current line. */
void fold(char to[], char from[])
{

  int i, j, k;


  for (i = 0, j = 0, k = 0; from[i] != '\0'; i++, j++, k++)
    {
      if (j <= FOLDLENGTH)
	to[k] = from[i];
      else 
	{
	  if (from[i] == ' ')
	    to[k] = from[i];
	  else
	    {
	      to[k] = '\n';
	      k++;
	      to[k] = from[i];
	      j = 0;
	    }
	}
    }
  to[i] = '\0';			/* this line settles the bug */
}
  
int main()
{
  int len;
  char line[MAXLINE];
  char folded_line[MAXLINE];

  while ((len = getline_mike(line, MAXLINE)) > 0)
    {
      fold(folded_line, line);
      printf("%s\n", folded_line);
    }
}

