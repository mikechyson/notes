#include "csapp.h"

int main(int argc, char *argv[], char *envp[])
{
  int i;

  printf("Command-line arguments:\n");

  for (i = 0; argv[i] != NULL; i++)
    printf("\targv[%2d]: %s\n", i, argv[i]);

  printf("\n");
  printf("Environment variable:\n");
  for (i = 0; envp[i] != NULL; i++)
    printf("\tenvp[%2d]: %s\n", i, envp[i]);

  exit(0);
}
