#include "csapp.h"
#define MAXARGS 128

/* function prototypes */
void eval(char *cmdline);
int parseline(char *buf, char **argv);
int builtin_command(char **argv);

int main(void)
{
  char cmdline[MAXLINE]; /* command line, MAXLINE define in csapp.h  */

  while(1) {
    /* read */
    printf("input:\t");
    Fgets(cmdline, MAXLINE, stdin);
    if (feof(stdin))
      exit(0);

    /* evaluate */
    eval(cmdline);
  }
}


/* eval - Evaluate a command line */
void eval(char *cmdline)
{
  char *argv[MAXARGS];		/* argument list */
  char buf[MAXLINE];		/* holds modified command line */
  int bg;			/* should the job run in bg or fg? */
  pid_t pid;			/* process id */

  strcpy(buf, cmdline);
  bg = parseline(buf, argv);
  if (argv[0] == NULL)
    return;			/* ignore empty lines */

  if (!builtin_command(argv)) {	/* not built in command */
    if ((pid = Fork()) == 0) {	/* child runs user job */
      if (execve(argv[0], argv, environ) < 0) {
	printf("%s: Command not found.\n", argv[0]);
	exit(0);
      }
    }

    /* parent waits for foreground job to terminate */
    if (!bg) {			/* foreground */
      int status;
      if (waitpid(pid, &status, 0) < 0)
	unix_error("waitfg: waitpid error");
    } else
      printf("%d %s", pid, cmdline);
    
  }
  return;
}

/* if first arg is a builtin command, run it and return true */
int builtin_command(char **argv)
{
  if (!strcmp(argv[0], "quit"))	/* quit command */
    exit(0);

  if (!strcmp(argv[0], "&"))	/* ignore singleton */
    return 1;

  return 0;
}


/* parseline - Parse the command line and build the argv array */
int parseline(char *buf, char **argv)
{
  char *delim;			/* pointer to first space delimiter */
  int argc;
  int bg;

  buf[strlen(buf)-1] = ' ';	/* replace trailing '\n' with space */
  while (*buf && (*buf == ' '))	/* ignore leading spaces */
    buf++;

  /* build the argv list */
  argc = 0;
  while((delim = strchr(buf, ' '))) {
    argv[argc++] = buf;
    *delim = '\0';
    buf = delim + 1;
    while (*buf && (*buf == ' ')) /* ignore spaces */
      buf++;
  }
  argv[argc] = NULL;		/* NULL terminate */

  if (argc == 0)		/* ignore black line */
    return 1;

  /* should the job run in the background? */
  if ((bg = (*argv[argc-1] == '&')) != 0)
    argv[--argc] = NULL;

  return bg;
}
