double poly(double a[], double x, long degree)
{
  long i;
  double result = a[0];
  double xpwr = x; /* Equals x^i at start of loop */
  for (i = 1; i <= degree; i++) {
    result += a[i] * xpwr;
    xpwr = x * xpwr;
  }
  return result;
}
