#include "csapp.h"

void sigint_handler(int sig)	/* SIGINT handler */
{
    return;			
}


unsigned int snooze(unsigned int secs)
{
    unsigned int rc = sleep(secs);

    printf("Slept for %d of %d secs.\n", secs - rc, secs);
    return rc;
}


int main(int argc, char **argv)
{
    if (argc != 2) {
	fprintf(stderr, "usage: %s <secs>\n", argv[0]);
	exit(0);
    }
    
    /* install the sigint handler */
    if (signal(SIGINT, sigint_handler) == SIG_ERR)
	unix_error("signal error");

    (void)snooze(atoi(argv[1]));

    exit(0);
}
