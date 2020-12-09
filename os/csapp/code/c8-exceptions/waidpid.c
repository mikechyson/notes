#include <sys/types.h>
#include <sys/wait.h>

/* return: PID of child if OK,
           0 (if WNOHANG), or
           -1 on error
*/
pid_d waitpid(pid_t pid, int *statusp, int option);
