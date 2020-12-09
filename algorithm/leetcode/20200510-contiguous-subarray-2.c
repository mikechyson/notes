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


int findMaxLength(int* nums, int numsSize){

    if (numsSize < 1)
	return 0;
    
    int size = numsSize * 2 + 1;
    int* arr;
    arr = malloc(sizeof(int) * size);
    for (int i = 0; i < size; i++) arr[i] = -1;
    arr[numsSize] = 0;

    int maxlength = 0;
    int level = 0;

    for (int i = 0; i < numsSize; i++) {
        level += (nums[i] == 0 ? -1 : 1);

        if (arr[level + numsSize] == -1) {
            arr[level + numsSize] = i + 1;
        } else {
            int tmp = i + 1 - arr[level + numsSize];
            maxlength = (maxlength > tmp ? maxlength : tmp);
        }
        
    }
    return maxlength;
}
