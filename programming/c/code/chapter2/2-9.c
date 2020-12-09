/* x &= (x-1) delete the rightmost 1-bit in x */
int bitcount(int x)
{
  int b;
  for (b=0; x!=0; x &= (x-1))
    b++;
  return b;
}
