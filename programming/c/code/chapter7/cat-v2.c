#include <stdio.h>

/* cat: concatenate files, version 2 */
int main(int argc, char *argv[])
{
  FILE *fp;			/* file pointer */
  void filecopy(FILE *, FILE *);
  char *prog = argv[0];		/* program name for errors */

  if (argc == 1)		/* no args */
    filecopy(stdin, stdout);
  else
    while (--argc > 0)
      if ((fp = fopen(*++argv, "r")) == NULL) {
	/* we include the program name in the message
	   so if this program is used with others,
	   the source of an error is identified. */
	fprintf(stderr, "%s: can't open %s\n", prog, *argv);
	/* The argument of exit is available to
	   whatever process called this one. */
	exit(1);
      } else {
	filecopy(fp, stdout);
	fclose(fp);
      }

  if (ferror(stdout)) {
    fprintf(stderr, "%s: error writing stdout\n", prog);
    exit(2);
  }
  
  exit(0);
}

void filecopy(FILE *ifp, FILE *ofp)
{
  int c;

  while ((c = getc(ifp)) != EOF)
    putc(c, ofp);
}
  
