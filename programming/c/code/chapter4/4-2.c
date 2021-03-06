/* stdlib.h includes an atof */
#include <ctype.h>
#include <math.h>
#include <stdio.h>

double atof(char s[]);

/* atof: convert string s to double */
double atof(char s[]) {
  double val, power, exp;
  int i, sign, expsign;

  for (i = 0; isspace(s[i]); i++) /* skip white space */
    ;

  sign = (s[i] == '-') ? -1 : 1;
  if (s[i] == '+' || s[i] == '-') /* skip sign */
    i++;

  for (val = 0.0; isdigit(s[i]); i++)
    val = 10.0 * val + (s[i] - '0');

  if (s[i] == '.')
    i++;

  for (power = 1.0; isdigit(s[i]); i++) {
    val = 10.0 * val + (s[i] - '0');
    power *= 10;
  }

  if (s[i] == 'e' || s[i] == 'E')
    i++;

  expsign = (s[i] == '-') ? -1 : 1;
  if (s[i] == '+' || s[i] == '-')
    i++;

  for (exp = 0.0; isdigit(s[i]); i++)
    exp = 10.0 * exp + (s[i] - '0');

  return sign * val / power * pow(10, expsign * exp);
}


int main(void) {
  printf("%f\n", atof("123.45e-6"));
}
