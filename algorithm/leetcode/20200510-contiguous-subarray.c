/* 
   Given a binary array, find the maximum length of a contiguous subarray with equal number of 0 and 1.

   Example 1:

   Input: [0,1]
   Output: 2
   Explanation: [0, 1] is the longest contiguous subarray with equal number of 0 and 1.
   Example 2:

   Input: [0,1,0]
   Output: 2
   Explanation: [0, 1] (or [1, 0]) is a longest contiguous subarray with equal number of 0 and 1.

   Note: The length of the given binary array will not exceed 50,000.
*/
#include <stdlib.h>
#include <stdio.h>

int max(int a, int b)
{
    return a > b ? a : b;
}


int findMaxLength(int *nums, int numsSize)
{
    int maxsize = -1;		/* store result */
    int startindex;
    int sumleft[numsSize];	/* sum of array elements from nums[0] to nums[i] */
    int min, max;
    int i;

    sumleft[0] = ((nums[0] == 0) ? -1 : 1);
    min = sumleft[0];
    max = sumleft[0];
    for (i = 1; i < numsSize; i++) {
	sumleft[i] = sumleft[i-1] + ((nums[i] == 0) ? -1 : 1);
	if (sumleft[i] < min)
	    min = sumleft[i];
	if (sumleft[i] > max)
	    max = sumleft[i];
    }

    /* max value of j -i such that sumleft[i] = sumleft[j] */
    int hashlen = max - min + 1;
    int hash[hashlen];	/* optimum size of hash */
    for (i = 0; i < hashlen; i++)
	hash[i] = -1;  /* the array index of nums is greater than 0 */

    int offset;
    for (i = 0; i < numsSize; i++) {
	/* case 1: when the subarray starts from index 0 */
	if (sumleft[i] == 0) {
	    maxsize = i + 1;
	    startindex = 0;
	}

	/* case 2: not start from index 0 */
	offset = sumleft[i] - min;
	printf("offset: %d; sumleft[i]: %d; min: %d\n", offset, sumleft[i], min);
	if (hash[offset] == -1)	/* not seen */
	    hash[offset] = i;
	else {			/* seen */
	    if (i - hash[offset] > maxsize) {
		maxsize = i - hash[offset];
		startindex = hash[offset] + 1;
	    }
	}
    }
    return maxsize == -1 ? 0 : maxsize;
    
}

int main()
{
    int arr[] = { 0, 1 }; 
    int size = sizeof(arr) / sizeof(arr[0]); 
  
    int len = findMaxLength(arr, size);
    printf("%d\n", len);
    return 0; 
}
