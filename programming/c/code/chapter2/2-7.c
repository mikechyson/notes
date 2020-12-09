#include <stdio.h>

unsigned invert(unsigned x, int p, int n)
{
  int mask = (~(~0 << n)) << (p + 1 - n);
  return x ^ mask;
}




int main()
{
  unsigned int y;
  unsigned int x = 0xFFFF;

  y = invert(x, 7, 4);
  printf("%x\n", y);

  return 0;
  
}
