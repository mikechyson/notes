/* 
   Given an array nums of n integers where n > 1,  return an array output such that output[i] is equal to the product of all the elements of nums except nums[i].

   Example:

   Input:  [1,2,3,4]
   Output: [24,12,8,6]
   Constraint: It's guaranteed that the product of the elements of any prefix or suffix of the array (including the whole array) fits in a 32 bit integer.

   Note: Please solve it without division and in O(n).

   Follow up:
   Could you solve it with constant space complexity? (The output array does not count as extra space for the purpose of space complexity analysis.)
*/

/**
 * Note: The returned array must be malloced, assume caller calls free().
 */

#include <stdlib.h>
#include <stdio.h>

int* productExceptSelf(int* nums, int numsSize, int* returnSize){
    *returnSize = numsSize;


    int i;
    int tmp = 1;
    int *prod = (int *) malloc(sizeof(int) * numsSize);

    /* initialization */
    for (i = 0; i < numsSize; i++)
	prod[i] = 1;

    /* product of left i-1 elements */
    for (i = 0; i < numsSize; i++) {
	prod[i] = tmp;
	tmp *= nums[i];
    }

    tmp = 1;
    /* product of right i+1 elements */
    for (i = numsSize-1; i >= 0; i--) {
	prod[i] *= tmp;
	tmp *= nums[i];
    }

    for (i = 0; i < numsSize; i++)
	printf("%d\n", prod[i]);
    
    return prod;
}

int main(void)
{
    int nums[] = {1, 2, 3, 4};
    int *returnSize;
    int size = sizeof(nums) / sizeof(int);
    productExceptSelf(nums, size, returnSize);


    return 0;
}
