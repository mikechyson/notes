/* getbits: get n bits from position p */
/* getbits(x,4,3) returns the three bits
   in positions 4, 3 and 2, right-adjusted. */
unsigned getbits(unsigned x, int p, int n)
{
  return (x >> (p+1-n)) & ~(~0 << n);
}
