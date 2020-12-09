#include "csapp.h"

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
    sigset_t mask_all, mask_one, prev_one;

    Sigfillset(&mask_all);	/* init mask_set */
    Sigemptyset(&mask_one);
    Sigaddset(&mask_one, SIGCHLD);
    Signal(SIGCHLD, handler);
    initjobs();			/* initailize the job list */

    while (1) {
	Sigprocmask(SIG_BLOCK, &mask_one, &prev_one); /* block SIGCHLD */
	if ((pid = Fork()) == 0) /* child process */
	    Sigprocmask(SIG_SETMASK, &prev_one, NULL); /* unblock SIGCHILD */
	    Execve("/bin/date", argv, NULL);

	Sigprocmask(SIG_BLOCK, &mask_all, NULL); /* parent process */
	addjob(pid);				      /* add the child to the job list */
	Sigprocmask(SIG_SETMASK, &prev_one, NULL);    /* unblock SIGCHLD */
    }

    exit(0);
}
    
