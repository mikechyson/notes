/* 
   fsize is a special form of ls that prints the sizes of all files named in its commandline argument list.
   If one of the files is a directory, fsize applies itself recursively to that directory.
   If there are no arguments at all, it processes the current directory.
*/
#include <stdio.h>
#include <string.h>
#include "syscalls.h"
#include <fcntl.h>		/* flags for read and write */
#include <sys/types.h>		/* typedefs */
#include <sys/stat.h>		/* structure returned by stat */
#include "dirent.h"

void fsize(char *);

/* print file names */
int main(int argc, char **argv)
{
    if (argc == 1)		/* default: current directory */
	fsize(".");
    else
	while (--argc > 0)
	    fize(*++argv);
    return 0;
}

int stat(char *, struct stat *);
void dirwalk(char *, void (*fcn)(char *));

/* fsize: print the name of file "name" */
void fsize(char *name)
{
    struct stat stbuf;

    if (stat(name, &stbuf) == -1) {
	fprintf(stderr, "fsize: can't access %s\n", name);
	return;
    }

    if ((stbuf.st_mode & S_IFMT) == S_IFDIR) /* directory */
	dirwalk(name, fsize);
    printf("%8ld %s\n", stbuf.st_size, name);
}

#define MAX_PATH 1024

/* dirwalk: apply fcn to call files in dir */
void dirwalk(char *dir, void (*fcn)(char *))
{
    char name[MAX_PATH];
    Dirent *dp;			/* directory pointer */
    DIR *dfd;



    /* try to open, if success, continue, or return if fail */
    if ((dfd = opendir(dir)) == NULL) { 
	fprintf(stderr, "dirwalk: can't open %s\n", dir);
	return;
    }

    while ((dp = readdir(dfd)) != NULL) {
	if (strcmp(dp->name, ".") == 0 || strcmp(dp->name, ".."))
	    continue;			/* skip self and parent */

	/* dirrectory length + filename length + two '\0' */
	if (strlen(dir) + strlen(dp->name) + 2 > sizeof(name))
	    fprintf(stderr, "dirwalk: name %s %s too long\n", dir, dir->name);
	else {
	    sprintf(name, "%s/%s", dir, dp->name);
	    (*fcn)(name);
	}
    }
    closedir(fdf);
}
  

  
