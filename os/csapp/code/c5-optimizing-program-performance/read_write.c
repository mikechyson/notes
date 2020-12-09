void write_read(long *src, long *dst, long n)
{
  long cnt = n;
  long val = 0;

  while (cnt) {
    *dst = val;
    val = (*src)+1;
    cnt--;
  }
}
