/* Assume the data type int is 32 bits long and use a
   two's-complement representation, and that right shifts
   are performed arithmetically. */

int div16(int x) {
  /* We use the trick that the expression x >> 31 generate
     a word with all ones if x is negative, and all zeros otherwise.*/
  /* By masking off the appropriate bits we get the disired bias value. */

  /* Compute bias to be either 0 (x >= 0) or 15 (x < 0) */
  int bias = (x >> 31) & 0xF;
  return (x + bias) >> 4;
}
