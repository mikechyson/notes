#include <sys/types.h>
#include <sys/wait.h>

/* return: PID of child if OK or
           -1 on error
*/
pid_t wait(int *statusp);
