#include <stdio.h>

#define MAXLINE 1000
#define TAB2SPACE 4

int getline_mike(char s[], int lim);

int main()
{
  int len, i, t, space_count;
  char line[MAXLINE];
  

  while ((len = getline_mike(line, MAXLINE)) > 0)
    {
      space_count = 0;
      for (i = 0; i < len; i++)
	{
	  if(line[i] == ' ')
	    space_count++;
	  else
	    space_count = 0;

	  if (space_count == TAB2SPACE)
	    {
	      /* Because we are removing 4 spaces and
		 replacing them with 1 tab we move back
		 three chars and replae the ' ' with a \t
		 */
	      i -= 3;
	      len -= 3;
	      line[i] = '\t';

	      for (t = i + 1; t < len; t++)
		line[t] = line[t + 3];

	      space_count = 0;
	      line[len] = '\0';
	    }
	}
      printf("%s", line);
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
