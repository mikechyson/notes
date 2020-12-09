void handler2(int sig)
{
  int olderrno = errno;
  pid_t pid;

  while ((pid = waitpid(-1, NULL, 0)) > 0) 
    printf("Handler reaped child %d\n", (int)pid);

  if (errno != ECHILD)
    printf("waitpid error");

  Sleep(1)
    errno = olderrno;

}


    
