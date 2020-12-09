#include <stdio.h>
#include <string.h>


void reverse(char s[], int i, int j);

int main(void)
{
  char s[] = "123456";
  int len = strlen(s);
  reverse(s, 0, len - 1);
  printf("%s\n", s);

  char s2[] = "12345";
  len = strlen(s2);
  reverse(s2, 0, len - 1);
  printf("%s\n", s2);

}
  


void reverse(char s[], int i, int j)
{
  int tmp;

  if (i < j) {
    tmp = s[i];
    s[i] = s[j];
    s[j] = tmp;

    reverse(s, ++i, --j);
  }
}


