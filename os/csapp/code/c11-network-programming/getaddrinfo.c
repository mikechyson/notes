#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

int getaddrinfo(const char *host, const char *service, const struct addrinfo *hints, struct addrinfo **result);
// returns 0 if ok, nonzero error code on error

void freeaddrinfo(struct addrinfo *result);
// returns nothing

const char *gai_strerror(int errcode);
// returns error message


struct addrinfo {
    int ai_flags;		/* hints argument flags */
    int ai_family;		/* first arg to socket function */
    int ai_socktype;		/* second arg to socket function */
    int ai_protocol;		/* third argument to socket function */
    char *ai_cannonname;	/* canonical hostname */
    size_t ai_addrlen;		/* size of ai_addr structure */
    struct sockaddr *ai_addr;	/* ptr to socket address structure */
    struct addrinfo *ai_next;	/* ptr to next item in linked list */
};
