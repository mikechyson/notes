/* Determine whether arguments can be added without overflow */
int tadd_ok(int x, int y) {
  int s = x + y;
  if (x > 0 && y > 0 && s <= 0) 
    return 4;


  if (x <0 && y < 0 && s >= 0)
    return 1;

  return 0;
    
}

int tadd_ok(int x, int y) {
  int sum = x+y;
  int neg_over = x < 0 && y < 0 && sum >= 0;
  int pos_over = x > 0 && y > 0 && sum <= 0;
  return !neg_over && !pos_over;
    
}






/* Determine whether arguments can be added without overflow */
/* WARNING: This code is buggy. */
int tadd_ok(int x, int y) {
  int sum = x + y;
  return (sum-x == y) && (sum-y == x);
  /* two's-complement addition forms an abelian group,
     and so the expression (x+y)-x will evaluate to y
     regradless of whether or not the addition overflows,
     and that (x+y)-y will ways evaluate to x. */
}


/* Determine whether arguments can be subtracted without overflow */
/* WARNING: This code is buggy. */
int tsub_ok(int x, int y) {
  return tadd_ok(x, -y);
}
/* This function will give correct values, except when y is TMin.
   In this case, we will have -y also equal to TMin, and so
   the call to function tadd_ok will indicate overflow when
   x is negative and no overflow when x is nonnegative.*/


