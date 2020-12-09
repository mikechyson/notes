#include <stdio.h>

#define SIZE 100

void escape(char s[], char t[])
{
  int i, j;

  for (i = 0, j = 0; t[i] != '\0'; i++)
    {
      switch (t[i])
	{
	case '\n':
	  s[j++] = '\\';
	  s[j++] = 'n';
	  break;
	case '\t':
	  s[j++] = '\\';
	  s[j++] = 't';
	  break;
	default:
	  s[j++] = t[i];
	  break;
	}
    }
  s[j] = '\0';
}

void convert(char s[], char t[])
{
  int i, j;

  for (i = 0, j = 0; t[i] != '\0'; i++)
    {
      switch (t[i])
	{
	case '\\':
	  switch (t[i+1])
	    {
	    case 'n':
	      s[j++] = '\n';
	      i++;
	      break;
	    case 't':
	      s[j++] = '\t';
	      i++;
	      break;
	    default:
	      s[j++] = '\\';
	      s[j++] = t[i+1];
	      i++;
	      break;
	    }
	  break;
	default:
	  s[j++] = t[i];
	  break;
	}
    }
  s[j] = '\0';

}

int main()
{
  char s[SIZE];
  char t[SIZE] = {'a', '\t', 'b', '\n', 'c', '\\', 'd', '\0'};
  char re[SIZE];

  escape(s, t);
  printf("%s\n", s);

  convert(re, s);
  printf("%s\n", re);
  
}
