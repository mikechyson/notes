/* bitcount: count 1 bits in x */
int bitcount(unsigned x)	/* ensure logical right-shift */
{
  int b;

  for (b = 0; x != 0; x >>= 1)
    if (x & 01)
      b++;
  return b;
}
