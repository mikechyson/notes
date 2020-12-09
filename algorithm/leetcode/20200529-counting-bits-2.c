int* countBits(int num, int* returnSize){
  int i;
  int curr;
  int ones;
  int size = num + 1;
  int* arr = (int*)malloc(sizeof(int) * size);
  
  arr[0] = 0;
  *returnSize = size;
  for(i = 1; i <= num; i++){
    curr = i;
    ones = 0;
    while(curr){
      if(curr > 0){
        ones++;
      }
      curr &= curr - 1;
    }
    arr[i] = ones;
  }
  return arr;
}
