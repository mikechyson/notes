# 1 "main.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 363 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "main.c" 2
int sum(int *a, int n);

int array[2] = { 1, 2 };

int main(void)
{
  int val = sum(array, 2);
  return val;
}
