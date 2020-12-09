#include "csapp.h"

sigjmp_buf buf;

void handler(int sig)
{
    siglongjmp(buf, 1);
}

int main(void)
{
    if (!sigsetjmp(buf, 1)) {
	Signal(SIGINT, handler); /* install handler */
	printf("starting\n");
    } else
	printf("restarting\n");

    while (1) {
	Sleep(1);
	printf("processing...\n");
    }

    exit(0);			/* control never reaches here */
}

    
	    
