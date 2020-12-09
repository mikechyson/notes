#include <arpa/inet.h>

int inet_pton(AF_INET, const char *src, void *dst); /* presentation to network */
/* return 1 if OK, 0 if src is invalid dotted decimal, -1 on error */

const char *inet_ntop(AF_INET, const void *src, char *dst, socklen_t size); /* network to presentation */
/* return pointer to a dotted-decimal string if OK, NULL on error */

/* AF_INET for 32-bit IPv4 addresses */
/* AF_INET6 for 64-bit IPv6 addresses */

