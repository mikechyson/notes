#include <stdio.h>

#define swap(t, x, y) { t tmp; tmp = x; x = y; y = tmp;}

int main(void)
{
  int a = 1;
  int b = 2;
  swap(int, a, b);
  printf("a:%d, b:%d\n", a, b);
}
