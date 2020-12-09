/* Declarations of functions implementing operations bis and bic */
int bis(int x, int m);
int bic(int x, int m);

/* bis: bit set */
/* bic: bit clear */
/* Both instructions take a data word x and a mask word m.
   They generate a result z consisting of the bits of x modified according to the bits of m.
   With bis, the modification involves setting z to 1 at each bit position where m is 1.
   With bic, the modification involves setting z to 0 at each bit position where m is 1. */


/* Compute x|y using only calls to functions bis and bic */
int bool_or(int x, int y) {
  int result = bis(x,y);
  return result;
}

/* Compute x^y using only calls to functions bis and bic */
int bool_xor(int x, int y) {
  int result = bis(bic(x,y),bic(y,x));
  return result;
}

/* bic(x,m) is equivalent to x & ~m */

    
