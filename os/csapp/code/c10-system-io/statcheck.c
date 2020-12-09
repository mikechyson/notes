#include "csapp.h"

int main(int argc, char **argv)
{
    struct stat stat;
    char *type, *readok;

    Stat(argv[1], &stat);	/* read metadata into stat */
    if (S_ISREG(stat.st_mode))	/* determine file type */
	type = "regular";
    else if (S_ISDIR(stat.st_mode))
	type = "directory";
    else
	type = "other";

    if ((stat.st_mode & S_IRUSR)) /* check read access */
	readok = "yes";
    else
	readok = "no";

    printf("type: %s, read: %s\n", type, readok);
    exit(0);
}
