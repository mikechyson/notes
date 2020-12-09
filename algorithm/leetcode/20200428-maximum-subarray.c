/* 
   Given an integer array nums, find the contiguous subarray (containing at least one number) which has the largest sum and return its sum.

   Example:

   Input: [-2,1,-3,4,-1,2,1,-5,4],
   Output: 6
   Explanation: [4,-1,2,1] has the largest sum = 6.
   Follow up:

   If you have figured out the O(n) solution, try coding another solution using the divide and conquer approach, which is more subtle.
*/

#include <limits.h>

int maximum(int, int, int);
int maxCrossingSubarray(int*, int, int, int);
int maxSumSubarray(int*, int, int);
    

int maxSubArray(int* nums, int numsSize)
{
    return maxSumSubarray(nums, 0, numsSize-1);
}


/* maximum: find the largest value of three numbers */
int maximum(int a, int b, int c)
{
    if (a >=b && a >= c)
	return a;
    else if (b >=c)
    	return b;
    else
	return c;
}

/* maxCrossingSubarray: find the maximum sum of subarray crossing the middle element */
int maxCrossingSubarray(int *nums, int low, int mid, int high)
{
    int lsum = INT_MIN;
    int rsum = INT_MIN;
    int sum = 0;
    int i;

    for (i = mid; i >= low; i--) {
    	sum += nums[i];
	if (sum > lsum)
	    lsum = sum;
    }

    sum = 0;
    for (i = mid+1; i <= high; i++) {
    	sum += nums[i];
	if (sum > rsum)
	    rsum = sum;
    }

    return lsum + rsum;
}


int maxSumSubarray(int *nums, int low, int high)
{
    if (low == high)
	return nums[high];

    int mid = (low + high) / 2;

    int lsum = maxSumSubarray(nums, low, mid);
    int rsum = maxSumSubarray(nums, mid+1, high);
    int msum = maxCrossingSubarray(nums, low, mid, high);

    return maximum(lsum, rsum, msum);
}
