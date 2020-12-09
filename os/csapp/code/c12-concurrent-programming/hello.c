#include "csapp.h"

void *thread(void *vargp);

int main()
{
    pthread_t tid;		/* variable used to store tid */
    Pthread_create(&tid, NULL, thread, NULL);
    Pthread_join(tid, NULL);	/* the main thread waits for the peer thread to terminate
				 *  with the call to pthread_join */
    exit(0);
}


void *thread(void vargp)	/* thread routine */
{
    printf("Hello, world!\n");
    return NULL;
}
