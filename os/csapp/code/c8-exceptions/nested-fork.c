#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>


pid_t Fork(void);

int main()
{
  Fork();
  Fork();
  printf("hello\n");
  exit(0);
}
