#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

int main(int argc, char *argv[])
{
    printf("usage: %s l(ower)/u(pper)", argv[0]);

    int (*convert)(int c) = NULL;
    int c;

    if (argc != 2)
      printf("usage: %s l(ower)/u(pper)", argv[0]);

    if (strcmp(argv[1], "l") == 0 || strcmp(argv[1], "lower") == 0)
      convert = tolower;
    else if(strcmp(argv[1], "u") == 0 || strcmp(argv[1], "upper") == 0)
      convert = toupper;
    else
      printf("usage: %s l(ower)/u(pper)", argv[0]);

    while ((c = getchar()) != EOF)
      putchar((*convert)(c));

    return 0;
}
