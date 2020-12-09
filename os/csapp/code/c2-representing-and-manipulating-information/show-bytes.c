# include <stdio.h>

typedef unsigned char *byte_pointer;

/* size_t, preferred deta type for expressing the sizes of data structures */
void show_bytes(byte_pointer start, size_t len) { 
  int i;
  for (i = 0; i < len; i++)
    printf(" %.2x", start[i]); 	/* an integer should be printed in hexadecimal with at least 2 digits */
  printf("\n");
}

void show_int(int x) {
  /* This cast indicates to the compiler that the program should consider
     the pointer to be a sequence of bytes rather than to an object
     of the original data type.*/
  /* Using sizeof rather than a fixed value is one step
     toward writing code that is portable across different machine types. */
  show_bytes((byte_pointer) &x, sizeof(int));
}


void show_float(float x) {
  show_bytes((byte_pointer) &x, sizeof(float));
}

void show_pointer(void *x) {
  show_bytes((byte_pointer) &x, sizeof(void *));
}

void show_short(short x) {
  show_bytes((byte_pointer) &x, sizeof(short));
}

void show_long(long x) {
  show_bytes((byte_pointer) &x, sizeof(long));
}

void show_double(double x) {
  show_bytes((byte_pointer) &x, sizeof(double));
}


void test_show_bytes(int val) {
  float fval = (float) val;
  int *pval = &val;
  short sval = (short) val;
  long lval = (long) val;
  double dval = (double) val;
  
  show_int(val);
  show_float(fval);
  show_pointer(pval);
  show_short(sval);
  show_long(lval);
  show_double(dval);
}



int main() {
  test_show_bytes(12345);
  /* printf("---"); */
  /* int a = 0x12345678; */
  
    
}

