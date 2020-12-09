#include "csapp.h"

int open_clientfd(char *hostname, char *port)
{
    int clientfd;
    struct addrinfo hints, *listp, *p;

    /* get a list of potential server addresses */
    memset(&hints, 0, sizeof(struct addinfo));
    hints.ai_socktype = SOCK_STREAM; /* open a connection */
    hints.ai_flags = AI_NUMERICSERV; /* using a numeric port arg */
    hints.ai_flags |= AI_ADDRCONFIG; /* recommended for connection */
    getaddrinfo(hostname, port, &hints, &listp); /* convert string to address */

    /* walk the list to the one that we successfully connected to */
    for (p = listp; p; p = p->ai_next) {
	/* create a socket descriptor */
	if ((clientfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol)) < 0)
	    continue;		/* socket failed, try the next */

	/* connect to the server */
	if (connect(clientfd, p->ai_addr, p->ai_addrlen) != -1)
	    break;		/* success */

	close(clientfd);	/* connect failed, try another */
    }

    /* clean up */
    freeaddrinfo(listp);
    if (!p) 			/* all connects failed */
	return -1;
    else
	return clientfd;	/* the last connect succeeded */
    
}



