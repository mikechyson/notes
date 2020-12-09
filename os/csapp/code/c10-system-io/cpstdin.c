#include "csapp.h"
#include <unistd.h>
#include <stdio.h>

int main(void)
{
  char c;

  while(Read(STDIN_FILENO, &c, 1) != 0)
    Write(STDOUT_FILENO, &c, 1);
  exit(0);
}
    
