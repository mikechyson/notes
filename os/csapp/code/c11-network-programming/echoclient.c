#include "csapp.h"

int main(int argc, char **argv)
{
    int clientfd;
    char *host, *port, buf[MAXLINE];
    rio_t rio;

    if (argc != 3) {
	fprintf(stderr, "usage: %s <host> <port>\n", argv[0]);
	exit(0);
    }

    host = argv[1];
    port = argv[2];

    clientfd = Open_clientfd(host, port); /* connect to server */
    Rio_readinitb(&rio, clientfd);	  /* init */

    while (Fgets(buf, MAXLINE, stdin) != NULL) { /* read line from stdin */
	Rio_writen(clientfd, buf, strlen(buf));	 /* send the text to server */
	Rio_readlineb(&rio, buf, MAXLINE);	 /* read the echo line from server */
	Fputs(buf, stdout);			 /* put the result to stdout */
    }

    Close(clientfd);
    exit(0);
}
