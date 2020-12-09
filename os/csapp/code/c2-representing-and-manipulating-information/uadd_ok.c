/* Determine whether arguments can be added without overflow */
int uadd_ok(unsigned x, unsigned y) {
  if (x + y < x) {
    return 1;
  } else {
    return 0;
  }
}


int uadd_ok(unsigned x, unsigned y) {
  unsigned sum = x + y;
  return sum >= x;
}

