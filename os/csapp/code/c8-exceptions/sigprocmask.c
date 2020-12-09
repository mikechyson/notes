#include "csapp.h"

volatile sig_atomic_t pid;

void sigchld_handler(int s)
{
    int olderrno = errno;
    pid = waitpid(-1, NULL, 0);	/* reap all */
    errno = olderrno;
}

void sigint_handler(int s)
{
}

int main(int argc, char **argv)
{
    sigset_t mask, prev;

    Signal(SIGCHLD, sigchld_handler); /* install handler */
    Signal(SIGINT, sigint_handler);   /* install handler */
    Sigemptyset(&mask);
    Sigaddset(&mask, SIGCHLD);

    while (1) {
	Sigprocmask(SIG_BLOCK, &mask, &prev); /* block SIGCHLD */
	if (Fork() == 0)		      /* child */
	    exit(0);

	/* parent */
	pid = 0;
	Sigprocmask(SIG_SETMASK, &prev, NULL); /* unblock SIGCHLD */

	/* wait for SIGCHLD to be received (wasteful) */
	while (!pid)
	    sigsuspend(&prev);

	/* optionally unblock SIGCHLD */
	sigprocmask(SIG_SETMASK, &prev, NULL);

	/* do some work after receiving SIGCHLD */
	printf(".");
    }
    exit(0);
}
