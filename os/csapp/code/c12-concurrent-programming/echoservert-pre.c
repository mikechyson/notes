#include "csapp.h"
#include "sbuf.h"
#define NTHREADS 4
#define SBUFSIZE 16

void echo_cnt(int connfd);
void *thread(void *vargp);

sbuf_t sbuf;			/* shared buffer of connected descriptors */

int main(int argc, char **argv)
{
    int i, listenfd, connfd;
    socklen_t clientlen;
    struct sockaddr_storage clientaddr;
    pthread_t tid;

    if (argc != 2) {
	fprintf(stderr, "usage: %s <port>\n", argv[0]);
	exit(0);
    }

    listenfd = Open_listenfd(argv[1]);

    sbuf_init(&sbuf, SBUFSIZE);
    for (i = 0; i < NTHREADS; i++) /* create worker threads */
	Pthread_create(&tid, NULL, thread, NULL);

    while (1) {
	clientlen = sizeof(struct sockaddr_storage);
	connfd = Accept(listenfd, (SA *) &clientaddr, &clientlen);
	sbuf_insert(&sbuf, connfd); /* insert connfd in buffer */
    }

}

void *thread(void *vargp)
{
    Pthread_detach(pthread_self()); /* avoid memory leak */
    while (1) {
	int connfd = sbuf_remove(&sbuf); /* remove connfd from buffer */
	echo_cnt(connfd);		 /* service client */
	Close(connfd);
    }
}
