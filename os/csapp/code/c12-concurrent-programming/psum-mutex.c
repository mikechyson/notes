#include "csapp.h"
#define MAXTHREADS 32

void *sum_mutex(void *vargp);	/* thread routine */

/* global shared variables */
long gsum = 0;			/* global sum */
long nelems_per_thread;		/* number of elements to sum */
sem_t mutex;			/* mutex to protect global sum */

int main(int argc, char **argv)
{
    long i;			/* index */
    long nelems;		/* number of elements */
    long log_nelems;
    long nthreads;		/* number of threads */
    long myid[MAXTHREADS];	/* array of long */
    pthread_t tid[MAXTHREADS];	/* array of type pthread_t */


    /* get input arguments */
    if (argc != 3) {
	printf("Usage: %s <nthreads> <log_nelems>\n", argv[0]);
	exit(0);
    }

    nthreads = atoi(argv[1]);	/* convert the first argument to long integer */
    log_nelems = atoi(argv[2]);	/* number of element to be summed up (2^n) */
    nelems = (1L << log_nelems);	   /* left shift to become 2^n */
    nelems_per_thread = nelems / nthreads; /* number of elements per thread */
    sem_init(&mutex, 0, 1);		   /* init mutex */

    /* create peer threads and wait for them to finish */
    for (i = 0; i < nthreads; i++) {
	/* The idea of passing a small unique thread ID to the peer threads
	 * is a general technique that is used in many parallel applications.*/
	myid[i] = i;
	Pthread_create(&tid[i], NULL, sum_mutex, &myid[i]);
    }
    for (i = 0; i < nthreads; i++)
	Pthread_join(tid[i], NULL);

    /* check final answer */
    /* this is just an example code for show parallelism */
    if (gsum != (nelems * (nelems-1))/2) /* the right answer using the sum function */
	printf("Error: result-%ld\n", gsum);

    exit(0);
}


/* thead routine for psum-mutex.c */
void *sum_mutex(void *vargp)
{
    long myid = *((long *)vargp); /* extract the thread ID */
    long start = myid * nelems_per_thread; /* start element index */
    long end = start + nelems_per_thread;  /* end element index */
    long i;

    for (i = start; i < end; i++) {
	printf("%d", (int)&mutex);
	P(&mutex);
	gsum += i;
	V(&mutex);
    }
    return NULL;
}

