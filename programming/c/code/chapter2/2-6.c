/* getbits: get n bits from position p */
/* getbits(x,4,3) returns the three bits
   in positions 4, 3 and 2, right-adjusted. */
/* start from zero */
unsigned getbits(unsigned x, int p, int n)
{
  return (x >> (p+1-n)) & ~(~0 << n);
}


unsigned setbits(unsigned x, int p, int n, unsigned y)
{
  int mask = ~(~0 << n) << (p - n + 1);
  x &= ~mask;

  y = getbits(y, p, n);
  y <<= (p - n + 1);

  return x | y;
}

int main()
{
  unsigned int x = 0xFFFF;
  unsigned int y = 0x0000;


  printf("setbit result: %x\n", setbits(x, 9, 4, y));
  

  return 0;
}
