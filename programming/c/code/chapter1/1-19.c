/* 
Write a function reverse(s) that reverses the character string s. 
Use it to write a program that reverses its input a line at a time.
*/

#include <stdio.h>

#define MAXLINE 1000

int getline_mike(char line[], int lim);
void reverse(char s[]);

int main()
{
  int len;
  char line[MAXLINE];

  while((len = getline_mike(line, MAXLINE)) > 0)
    {
      reverse(line);
      printf("%s\n", line);
    }
  return 0;
	
}

void reverse(char s[])
{
  char c;
  int i, j;

  for (j = 0; s[j] != '\0'; j++)
    ;
  --j;

  for (i = 0; i < j; i++)
    {
      /* inplace reverse */
      c = s[i];
      s[i] = s[j];
      s[j] = c;
      --j;
    }
  
}


/* get_line: read a line into s, return length */
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
  
