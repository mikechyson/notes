#include <stdio.h>

int main()
{
  printf("hello, world\n\c");
}

/* 1-2.c:5:25: warning: unknown escape sequence '\c' [-Wunknown-escape-sequence] */
