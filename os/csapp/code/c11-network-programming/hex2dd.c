#include "csapp.h"

int main(int argc, char **argv)
{
    struct in_addr inaddr;	/* address in network byte order  */
    uint32_t addr;		/* address in host byte order */
    char buf[MAXBUF];		/* buffer for dotted-decimal string */

    if (argc != 2) {
	fprintf(stderr, "usage: %s <hex number>\n", argv[0]);
	exit(0);
    }

    sscanf(argv[1], "%x", &addr); /* scan hex number into address */
    inaddr.s_addr = htonl(addr);  /* convert host to network order and store in address structure*/

    if (!inet_ntop(AF_INET, &inaddr, buf, MAXBUF)) /* error when convertting */
	unix_error("inet_ntop");

    printf("%s\n", buf);

    exit(0);
}
