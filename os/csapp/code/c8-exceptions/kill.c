#include "csapp.h"

int main(void)
{
    pid_t pid;

    /* child sleeps until SIGKILL signal received, then dies */
    if ((pid = Fork()) == 0) {	/* child */
	Pause();		/* wait for a signal to arrive */
	printf("control should never reach hear!\n");
	exit(0);
    }

    /* parent sends a SIGKILL signal to a child */
    Kill(pid, SIGKILL);
    exit(0);
}
