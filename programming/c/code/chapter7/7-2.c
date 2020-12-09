#include <stdio.h>
#include <string.h>

#define MAXLEN 20
#define WIDTH 4


int find(int c);

int main(int argc, char *argv[])
{
  char *format;
  int c;
  int textlen = 0;

  /* parse command-line argument */
  while (--argc > 0 && (*++argv)[0] == '-')
    while ((c = *++argv[0]))
      switch (c) {
      case 'o':
      case 'O':
	format = "%3o ";
	break;
      case 'x':
      case 'X':
	format = "%2X ";
	break;
      default:
	printf("find: illegal option %c\n", c);
	argc = 0;
	break;
      }

  
  if (argc != 0)
    printf("Usage: %s -x/-X/-o/-O\n", argv[0]);
  else
    while ((c = getchar()) != EOF && find(c)) {

      if (textlen == MAXLEN) {
	putchar('\n');
	textlen = 0;
      }

      if (c == '\n') {
	textlen = 0;
	putchar('\n');
      }	else {
	printf(format, c);
	textlen++;
      }
	
    }

  return 0;
}

int find(int c)
{
  char *table = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890 !\"#%&'()*+,-./:;<=>?[\\]^_{|}~\t\f\v\r\n";

  char *s;

  for (s = table; *s; s++)
    if (*s == c)
      return 1;

  return 0;
}


