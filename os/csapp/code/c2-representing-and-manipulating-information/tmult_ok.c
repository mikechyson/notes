/* Determine whether arguments can be multiplied without overflow */
int tmult_ok(int x, int y) {
  int p = x*y;
  /* Either x is zero, or dividing p by x gives y */
  return !x || p/x == y;
}

/* 1. x \cdot y = p + t 2^2 */
/* 2. By definition of integer division, dividing p by nonzero x gives
   a quotient q and a remainder r such that p = x \cdot q + r, and |r| < |x| */
/* 3. suppose q=y, we have x \cdot y = x \cdot y + r + t 2^w.
   r + t 2^2 = 0, but |r| < |x| \le 2^w, and so this identity can hold
   only if t=0 in which case r=0.*/




/* For the case where data type int has 32 bits */
int tmult_ok(int x, int y) {
  int64_t x_64 = x;
  int64_t y_64 = y;
  /* Compute product without overflow */
  int64_t p = x_64 * y_64;
  /* See if casting to int preserves value */
  return p == (int) p;
}


