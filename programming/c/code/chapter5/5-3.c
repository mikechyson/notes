/* strcat: concatenate t to end of s; s must be big enough  */
void strcat(char *s, char *t)
{
  while (*s++)		/* find end of s */
    ;
  while (*s++ = *t++) /* copy t */
    ;
}
