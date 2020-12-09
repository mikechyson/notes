/* Prototype for library function strlen */
size_t strlen(const char *s);

/* Determine whether string s is longer than string t */
/* WARNING: This function is buggy */
int strlonger(char *s, char *t) {
  return strlen(s) - strlen(t) > 0;
}

/* When compiled as a 32-bit program, data type size_t is
   define (via typedef) in header file stdio.h to be unsigned.*/

/* The function will incorrectly return 1 when s is shorter than t. */

/* Since strlen is defined to yield an unsigned result,
   the difference and the comparison are both computed using
   unsigned arithmetic. When s is shorter than t,
   the difference strlen(s) - strlen(t) should be negative,
   but instead becomes a large, unsigned number,
   which is greater than 0.*/

/* solution:
   return strlen(s) > strlen(t); */
