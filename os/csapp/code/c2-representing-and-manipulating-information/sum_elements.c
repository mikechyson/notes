/* WARNING: This is a buggy code */
float sum_elements(float a[], unsigned length) {
  int i;
  float result = 0;

  for (i=0; i <= length-1; i++)
    result += a[i];
  return result;
}

/* Since parameter length is unsigned, the computation 0 - 1 is perform
   using unsigned arithmetic, which is equivalent to modular addition.
   The result is then UMax. The <= comparison is also performed using an
   unsigned comparison, and since any number is less than or equal to UMax,
   the comparison always hold!*/

/* solutions:
   1. declaring length to be an int
   2. change the test of the for loop to be i < length */
      
