#include <stdio.h>
#include <string.h>


int htoi(char s[])
{
  int i, n, length;
  n = 0;
  length = strlen(s);
  
  for (i = 0; i < length; i++)
    {
      if (s[i] == '0' && (s[i+1] == 'x' || s[i+1] == 'X')) /* 0x or 0X prefix */
	  ;
      if (s[i] >= '0' && s[i] <= '9') /* 0-9 */
	n = 16 * n + (s[i] - '0');

      if (s[i] >= 'a' && s[i] <= 'f') /* a-f */
	n = 16 * n + (s[i] - 'a' + 10);

      if (s[i] >= 'A' && s[i] <= 'F') /* A-F */
	n = 16 * n + (s[i] - 'A' + 10);
    }
  
  return n;
}

int main()
{
  char s[20] = {'0', 'x', '0', 'F', 'F', '\0'};
  int n;
  n = htoi(s);
  printf("%d\n", n);
}
