#include <string.h>
#include <stdio.h>
#include <stdlib.h>

struct nlist {			/* table entry: */
  struct nlist *next;		/* next entry in chain */
  char *name;			/* defined name */
  char *defn;			/* replacement text */
};

#define HASHSIZE 101

static struct nlist *hashtab[HASHSIZE]; /* pointer table */

/* hash: form hash value for string s */
unsigned hash(char *s)
{
  unsigned hashval;

  for (hashval = 0; *s != '\0'; s++)
    hashval = *s + 31 * hashval;
  return hashval % HASHSIZE;
}

/* lookup: look for s in hashtab */
struct nlist *lookup(char *s)
{
  struct nlist *np;

  for (np = hashtab[hash(s)]; np != NULL; np = np->next)
    if (strcmp(s, np->name) == 0)
      return np;		/* found */

  return NULL;			/* not found */
}


char *str_dup(char *);

/* install: put (name, defn) in hashtab */
struct nlist *install(char *name, char *defn)
{
  struct nlist *np;
  unsigned hashval;

  if ((np = lookup(name)) == NULL) { /* not found */
    np = (struct nlist *) malloc(sizeof(*np));
    if (np == NULL || (np->name == str_dup(name)) == NULL)
      return NULL;
    hashval = hash(name);
    np->next = hashtab[hashval];
    hashtab[hashval] = np;
  } else			/* already found */
    free((void *) np->defn);	/* free previous defn */

  if ((np->defn = str_dup(defn)) == NULL)
    return NULL;

  return np;
}
      
