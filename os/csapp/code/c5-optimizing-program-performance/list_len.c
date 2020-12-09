#include <stdlib.h>
#include <stdio.h>

/* Compute the length of a linked list. */
typedef struct ELE {
  long data;
  struct ELE *next;
} list_ele, *list_ptr;

long list_len(list_ptr ls) {
  long len = 0;
  while (ls) {
    len++;
    ls = ls->next;
  }
  return len;
}


list_ptr create(int len)
{
  int i;
  list_ptr p1, p2;
  p1 = NULL;
  for (i = 0; i < len; i++) {
    p2 = (list_ptr)malloc(sizeof(list_ele));
    p2->data = i;
    p2->next = p1;
    p1 = p2;
  }
  return p2;
}


  
int main()
{
  list_ptr p = create(10);
  long len = list_len(p);
  printf("%ld\n", len);
}

