#include <stdio.h>

int main() {
  int x = 0x87654321;
  int a1, a2, a3;
  a1 = 0xFF & x;
  /* a2 = 0xFFFFFF00 ^ x; */
  a2 = ~0xFF ^ x; 		/* portable */
  a3 = 0xFF | x;
  printf("%x",x);
  printf("\n");
  printf("%x",a1);
  printf("\n");
  printf("%x",a2);
  printf("\n");
  printf("%x",a3);

}
