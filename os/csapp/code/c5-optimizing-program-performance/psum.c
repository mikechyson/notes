void psum1(float a[], float p[], long n)
{
  long i;
  p[0] = a[0];
  for (i = 1; i < n; i++)
    p[i] = p[i-1] + a[i];
}
/* psum1 has a CPE of 9.0 */
/* There are:
   write/read dependency +
   floating-point addition accumulation */


void psum1a(float a[], float p[], long n)
{
  long i;
  /* last_val holds p[i-1]; val holds p[i] */
  float last_val, val;
  last_val = p[0] = a[0];

  for (i = 1; i < n; i++) {
    val = last_val + a[i];
    p[i] = val;
    last_val = val;
  }
}

/* This version eliminate the write/read dependency */
