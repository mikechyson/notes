#include "csapp.h"

/* WARNING: This code is buggy! */
void handler(int sig)
{
    int olderrno = errno;
    sigset_t mask_all, prev_all;
    pid_t pid;

    Sigfillset(&mask_all);	/* initialize the signal set pointed to by mask_all set */
    while ((pid = waitpid(-1, NULL, 0)) > 0) { /* reap a zombie child */
	Sigprocmask(SIG_BLOCK, &mask_all, &prev_all); /* save mask_all into prev_all */
	/* The set of blocked signals is the union of the current set and the set argument. */
	deletejob(pid);		/* delete the child from the job list */
	Sigprocmask(SIG_SETMASK, &prev_all, NULL);
    }

    if (errno != ECHILD)
	unix_error("waitpid error");
    errno = olderrno;
}

int main(int argc, char **argv)
{
    int pid;
    sigset_t mask_all, prev_all;

    Sigfillset(&mask_all);
    Signal(SIGCHLD, handler);
    initjobs();			/* initailize the job list */

    while (1) {
	if ((pid = Fork()) == 0) /* child process */
	    Execve("/bin/date", argv, NULL);

	Sigprocmask(SIG_BLOCK, &mask_all, &prev_all); /* parent process */
	addjob(pid);				      /* add the child to the job list */
	Sigprocmask(SIG_SETMASK, &prev_all, NULL);
    }

    exit(0);
}
    
