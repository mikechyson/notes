#include <stdlib.h>
#include <string.h>

/* copies the string into a safe place */
char *str_dup(char *s)		/* make a duplicated of s */
{
  char *p;

  p = (char *) malloc(strlen(s)+1); /* +1 for '\0' */
  if (p != NULL)
    strcpy(p, s);
  return p;
}
    
