/* 
   Suppose an array sorted in ascending order is rotated at some pivot unknown to you beforehand.

   (i.e., [0,1,2,4,5,6,7] might become [4,5,6,7,0,1,2]).

   You are given a target value to search. If found in the array return its index, otherwise return -1.

   You may assume no duplicate exists in the array.

   Your algorithm's runtime complexity must be in the order of O(log n).

   Example 1:

   Input: nums = [4,5,6,7,0,1,2], target = 0
   Output: 4
   Example 2:

   Input: nums = [4,5,6,7,0,1,2], target = 3
   Output: -1
*/

/* 
   attribute: mid as the delimiter, one of the left or right must be sorted.
*/

#include <stdio.h>

int search(int *nums, int numsSize, int target)
{
    int left = 0;
    int right = numsSize-1;
    int mid;

    while (left <= right) {
	mid = left + (right - left) / 2;
	printf("mid: %d\n", mid);
	if (nums[mid] == target)
	    return mid;

	if (nums[mid] < nums[right]) {
	    if (nums[mid] < target && target <= nums[right])
		left = mid + 1;
	    else
		right = mid -1;
	} else {
	    if (nums[left] <= target && target < nums[mid])
		right = mid - 1;
	    else
		left = mid + 1;
	}
    }
    return -1;
}

int main()
{
    int nums[] = { 4, 5, 6, 7, 0, 1, 2 };
    int target = 4;
    int numsSize = sizeof(nums) / sizeof(int);

    printf("result: %d\n", search(nums, numsSize, target));
}
