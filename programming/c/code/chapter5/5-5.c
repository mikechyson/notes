#include <string.h>

/* strcpy: copy t to s; pointer version 3 */
void strcpy(char *s, char *t, int n)
{
  int tlen = strlen(t);

  if (n >= tlen)
    while (*s++ = *t++)
      ;
  else {
    int i;
    for (i = 0; i < n; i++)
      *s++ = *t++;
    *s = '\0';
  }
    
}


/* strcat: concatenate t to end of s; s must be big enough  */
void strcat(char *s, char *t, int n)
{
  while (*s++)		/* find end of s */
    ;

  int tlen = strlen(t);
  if (n >= tlen)
    while (*s++ = *t++) /* copy t */
      ;
  else {
    int i;
    for (i = 0; i < n; i++)
      *s++ = *t++;
    *s = '\0';
  }
}


/* strcmp: return <0 if s<t, 0 if s==t, >0 if s>t */
int strcmp(char *s, char *t, int n)
{
  int tlen = strlen(t);

  if (n >= tlen) {
    for (; *s++ == *t++; )
      if (*s == '\0')
	return 0;
  } else {
    int i;
    for (i = 0; i < n && *s++ == *t++ ; i++)
      if (*s == '\0')
	return 0;
  }
  return *s - *t;

}
