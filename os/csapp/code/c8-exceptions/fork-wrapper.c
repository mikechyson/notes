#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

void unix_error(char *msg);

pid_t Fork(void)
{
  pid_t pid;

  if ((pid = fork()) < 0)
    unix_error("Fork error");
  return pid;
}
