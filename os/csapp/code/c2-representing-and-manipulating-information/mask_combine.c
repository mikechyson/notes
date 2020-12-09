#include <stdio.h>
#include <assert.h>

/* int mask_combine(int x, int y) { */
/*   int mask = 0xFF; */

/*   int lest_significant = x & mask; */
/*   int most_significant = (y >> 8) << 8 ; */

/*   printf("%x, %x\n", lest_significant, most_significant); */
/*   int result = x | y; */
/*   printf("%x\n", result); */
/*   return result; */
/* } */


/* int main(){ */
/*   int x = 0x89ABCDEF; */
/*   int y = 0x76543210; */

/*   int combined = mask_combine(x,y); */
        
/* } */

/* ef, 76543200
ffffffff
I don't know why. */


int main() {
  size_t mask = 0xFF;
  size_t x = 0x89ABCDEF;
  size_t y = 0x76543210;

  size_t res = (x & mask) | (y & ~mask);
  assert(res == 0x765432EF);

  return 0;
}
