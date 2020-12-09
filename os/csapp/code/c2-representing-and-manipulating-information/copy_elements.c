/* Illustration of code vulnerability similar to that found in Sun's XDR library. */
void* copy_elements(void *ele_src[], int ele_cnt, size_t ele_size) {
  /* Allocate buffer for ele_cnt objects, each of ele_size bytes
     and copy from locations designated by ele_src */
  void *result =malloc(ele_cnt * ele_size);
  if (result == NULL)
    /* malloc failed */
    return NULL;
  void *next = result;
  int i;
  for (i = 0; i < ele_cnt; i++) {
    /* Copy object i to destination */
    memcpy(next, ele_src[i], ele_size);
    /* Move pointer to next memory region */
    next += ele_size;
  }
  return result;
}


/* CHANGE 1: */
/* uint64_t asize = ele_cnt * (uint64_t) ele_size; */
/* void *result = malloc(asize); */
/* This change doest not help at all. Even though the computation of asize will
   be accurate, the call to malloc will cause this value to be converted to a
   32-bit unsigned number, and so the same overflow conditions will occur. */


/* With malloc having a 32-bit unsigned number as it argument, it cannot possibly
   allocate a block of more than 2^32 bytes, and so there is no point attempting to
   allocate or copy this much memory. Instead the function should abort and
   return NULL. */
void* copy_elements(void *ele_src[], int ele_cnt, size_t ele_size) {
  /* Allocate buffer for ele_cnt objects, each of ele_size bytes
     and copy from locations designated by ele_src */
  uint64_t required_size = ele_cnt * (uint64_t) ele_size;
  size_t request_size = (size_t) required_size;
  if (required_size != request_size)
    /* Overflow must have occurred. Abort operation */
    return NULL;
  
  void *next = result;
  int i;
  for (i = 0; i < ele_cnt; i++) {
    /* Copy object i to destination */
    memcpy(next, ele_src[i], ele_size);
    /* Move pointer to next memory region */
    next += ele_size;
  }
  return result;
}
