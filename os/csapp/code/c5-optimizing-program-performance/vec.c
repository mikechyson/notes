#include <stdlib.h>
#include "vec.h"
#define IDENT 1
#define OP *



/* Create vector of specified length */
vec_ptr new_vec(long len)
{
  /* Allocate header structure */
  vec_ptr result = (vec_ptr) malloc(sizeof(vec_rec));
  data_t *data = NULL;
  if (!result)
    return NULL;		/* Couldn't allocate storage */
  result->len = len;

  /* Allocate array */
  if (len > 0) {
    /* void* calloc (size_t num, size_t size);
       Allocates a block of memory for an array of num elements, each of them size bytes long,
       and initializes all its bits to zero. */
    data = (data_t *)calloc(len, sizeof(data_t));
    if (!data) {
      free((void *) result);
      return NULL;		/* Couldn't allocate storage */
    }
  }

  /* Data will either be NULL or allocated array */
  result->data = data;
  return result;
}


/* Retrieve vector element and store at dest.
   Return 0 (out of bounds) or 1 (successful) */
int get_vec_element(vec_ptr v, long index, data_t *dest)
{
  if (index <0 || index >= v->len)
    return 0;
  *dest = v->data[index];
  return 1;
}

/* Return length of vector */
long vec_length(vec_ptr v)
{
  return v->len;
}

/* Get the first element of the vector */
data_t *get_vec_start(vec_ptr v)
{
  return v->data;
}




/* Implementation with maximum use of data abstraction */
void combine1(vec_ptr v, data_t *dest)
{
  long i;

  *dest = IDENT;
  for (i = 0; i < vec_length(v); i++) {
    data_t val;
    get_vec_element(v, i, &val);
    *dest = *dest OP val;
  }
}



/* Move call to vec_length out of loop */
void combine2(vec_ptr v, data_t *dest)
{
  long i;
  long length = vec_length(v);

  *dest = IDENT;
  for (i = 0; i < length; i++) {
    data_t val;
    get_vec_element(v, i, &val);
    *dest = *dest OP val;
  }
}


/* Direct access to vector data */
void combine3(vec_ptr v, data_t *dest)
{
  long i;
  long length = vec_length(v);
  data_t *data = get_vec_start(v);

  *dest = IDENT;
  for (i = 0; i < length; i++) {
    *dest = *dest OP data[i];
  }
}


/* Accumulate result in local variable */
void combine4(vec_ptr v, data_t *dest)
{
  long i;
  long length = vec_length(v);
  data_t *data = get_vec_start(v);
  data_t acc = IDENT;

  for (i = 0; i < length; i++) {
    acc = acc OP data[i];
  }
  *dest = acc;
}


/* 2 x 1 loop unrolling */
void combine5(vec_ptr v, data_t *dest)
{
  long i;
  long length = vec_length(v);
  long limit = length - 1;
  data_t *data = get_vec_start(v);
  data_t acc = IDENT;

  /* Combine 2 elements at a time */
  for (i = 0; i < limit; i+=2) {
    acc = (acc OP data[i]) OP data[i+1];
  }

  /* Finish any remaining elements */
  for (; i < length; i++) {
    acc = acc OP data[i];
  }
  *dest = acc;
}

/* 5 x 1 loop unrolling */
void combine5(vec_ptr v, data_t *dest)
{
  long i;
  long length = vec_length(v);
  long limit = length - 4;
  data_t *data = get_vec_start(v);
  data_t acc = IDENT;

  /* Combine 2 elements at a time */
  for (i = 0; i < limit; i+=5) {
    acc = acc OP data[i];
    acc = acc OP data[i+1];
    acc = acc OP data[i+2];
    acc = acc OP data[i+3];
    acc = acc OP data[i+4];
  }

  /* Finish any remaining elements */
  for (; i < length; i++) {
    acc = acc OP data[i];
  }
  *dest = acc;
}

/* 2 x 2 loop unrolling */
void combine6(vec_ptr v, data_t *dest)
{
  long i;
  long length = vec_length(v);
  long limit = length - 1;
  data_t *data = get_vec_start(v);
  data_t acc0 = IDENT;
  data_t acc1 = IDENT;

  /* Combine 2 elements at a time */
  for (i = 0; i < limit; i+=2) {
    acc0 = acc0 OP data[i];
    acc1 = acc1 OP data[i+1];
  }

  /* Finish any remaining elements */
  for (; i < length; i++) {
    acc0 = acc0 OP data[i];
  }

  *dest = acc0 OP acc1;
}


void combine7(vec_ptr v, data_t *dest)
{
  long i;
  long length = vec_length(v);
  long limit = length - 1;
  data_t *data = get_vec_start(v);
  data_t acc = IDENT;

  /* Combine 2 elements at a time */
  for (i = 0; i < limit; i+=2) {
    acc = acc OP (data[i] OP data[i+1]); /* Here's change */
  }

  /* Finish any remaining elements */
  for (; i < length; i++) {
    acc = acc OP data[i];
  }
  *dest = acc;
}
