/* 
while (there's another line)
     if (the line contains the pattern)
       print it 
*/

#include <stdio.h>
#define MAXLINE 1000		/* maximum input line length */

int getline2(char line[], int max);
int strindex(char source[], char searchfor[]);

char pattern[] = "ould";	/* pattern to search for */

/* find all lines matching pattern */
int main() {
  char line[MAXLINE];
  int found = 0;
  int index = -1;

  while (getline2(line, MAXLINE) > 0)
    if ((index = strindex(line, pattern)) >= 0) {
      printf("%s", line);
      found++;
      printf("%d\n", index);
    }
  return found;
}


/* getline: get line into s, return length */
int getline2(char s[], int lim) {
  int c, i;

  i = 0;
  while (--lim > 0 && (c = getchar()) != EOF && c != '\n')
    s[i++] = c;

  if (c == '\n')
    s[i++] = c;
  s[i] = '\0';

  return i;
}



/* strindex: return index of t in s, -1 if none */
int strindex(char s[], char t[]) {
  int i, j, k;
  int rightmost = -1;

  for (i = 0; s[i] != '\0'; i++) {
    for (j = i, k = 0; t[k] != '\0' && s[j] == t[k]; j++, k++)
      ;
    if (k > 0 && t[k] == '\0')	/* whole pattern matched */
      rightmost = i;
  }

  if (rightmost != -1)
    return rightmost;
  
  return -1;
}


  



