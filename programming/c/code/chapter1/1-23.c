#include <stdio.h>

#define MAXLENGTH 1000

int getline_mike(char s[], int lim);

int main()
{
  int len, i;
  char s[MAXLENGTH];

  printf("Input code, then press Ctrl+D:\n");
  while((len = getline_mike(s, MAXLENGTH)) > 0)
    {
      printf("\nResult:\n==========================\n");
      i = 0;
      while (s[i] != '\0')
	{
	  if (s[i] == '/' && s[i+1] == '/')
	    {
	      i += 2;
	      while (s[i] != '\n' && s[i] != '\0') /* not end */
		++i;
	    }
	  else if (s[i] == '/' && s[i+1] == '*') 
	    {
	      i += 2;
	      while (s[i] != '\0' && s[i+1] != '\0' && (s[i] != '*' || s[i+1] != '/')) /* not end */
		++i;
	      if (s[i] != '\0' && s[i+1] == '\0') 
		++i;
	      if (s[i] == '*')	/* end */
		i += 2;
	    }
	  else if (s[i] == '\"')
	    {
	      putchar('\"');
	      ++i;
	      while (s[i] != '\0' && (s[i-1] == '\\' || s[i] != '\"'))
		putchar(s[i++]);
	    }
	  else
	    putchar(s[i++]);
	}
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

