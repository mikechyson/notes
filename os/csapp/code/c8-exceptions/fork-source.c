#include <sys/types.h>
#include <unistd.h>

pid_t fork(void);
/* return:
   0 to child,
   pid of child to parend,
   -1 on error
*/
