/* 
   Given an array of integers and an integer k, you need to find the total number of continuous subarrays whose sum equals to k.

   Example 1:

   Input:nums = [1,1,1], k = 2
   Output: 2
 

   Constraints:

   The length of the array is in range [1, 20,000].
   The range of numbers in the array is [-1000, 1000] and the range of the integer k is [-1e7, 1e7].
*/

#include <stdio.h>

int subarraySum(int *nums, int numsSize, int k)
{

    int sums[numsSize+1];
    int count = 0;


    sums[0] = 0;
    for (int i = 1; i <= numsSize; i++) {
	sums[i] = sums[i-1] + nums[i-1];
    }

    for (int start = 0; start < numsSize; start++)
	for (int end = start + 1; end <= numsSize; end++) {
	    if (sums[end] - sums[start] == k)
		count++;
	}
	
    return count;
		
}

int main(void)
{
    int nums[] = { 1, 1, 1 };
    int numsSize = sizeof(nums) / sizeof(int);
    int k = 2;
    int count = subarraySum(nums, numsSize, k);
    printf("%d\n", count);
}
