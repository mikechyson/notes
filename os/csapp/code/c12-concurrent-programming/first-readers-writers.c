/* solution to first readers-writers problem */
/* may lead to writers infinitly waiting */


/* global variables */
int readcnt;			/* initially = 0 */
sem_t mutex, w;			/* both initially = 1 */

void reader(void)
{
    while (1) {
	P(&mutex);		
	readcnt++;		/* reader + 1 */
	if (readcnt == 1)	/* first in */
	    P(&w);		
	V(&mutex);		

	/* critical section */
	/* reading happens */

	P(&mutex);
	readcnt--;
	if (readcnt == 0)	/* last out */
	    V(&w);		
	V(&mutex);
    }
}

void write(void)
{
    while (1) {
	P(&w);

	/* critical section */
	/* writing happens */

	V(&w);
}
