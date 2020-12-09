#include <stdio.h>
#include <limits.h>

int main()
{
  printf("Maximum value of type char: %d\n", CHAR_MAX);
  printf("Minimum value of type char: %d\n", CHAR_MIN);

  printf("Maximum value of type signed char: %d\n", SCHAR_MAX);
  printf("Minimum value of type signed char: %d\n", SCHAR_MIN);

  printf("Maximum value of type unsigned char: %u\n", UCHAR_MAX);

  printf("Maximum value of type signed short: %d\n", SHRT_MAX);
  printf("Minimum value of type signed short: %d\n", SHRT_MIN);

  printf("Maximum value of type unsigned short: %u\n", USHRT_MAX);

  printf("Maximum value of type signed int: %d\n", INT_MAX);
  printf("Minimum value of type signed int: %d\n", INT_MIN);

  printf("Maximum value of type unsigned int: %u\n", UINT_MAX);

  printf("Maximum value of type unsigned long: %ld\n", LONG_MAX);
  printf("Minimum value of type unsigned long: %ld\n", LONG_MIN);

  printf("Maximum value of type unsigned long: %lu\n", ULONG_MAX);
  
}
