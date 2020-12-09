/* ip socket address structure */
struct sockaddr_in {	       /* 16-byte */
    uint16_t sin_family;	/* protocol familty (always AF_INET */
    uint16_t sin_port;		/* port number in network byte order */
    struct in_addr;		/* ip address in network byte order */
    unsigned char sin_zero[8];	/* pad to sizeof(struct sockaddr) */
};

/* generic socket address structure (for connect, bind, and accept) */
struct sockaddr {
    uint16_t sa_family;		/* protocol family */
    char sa_data[14];		/* address data */
}

    
typedef struct sockaddr SA;
