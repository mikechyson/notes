/* 
   Given an array of non-negative integers, you are initially positioned at the first index of the array.

   Each element in the array represents your maximum jump length at that position.

   Determine if you are able to reach the last index.

 

   Example 1:

   Input: nums = [2,3,1,1,4]
   Output: true
   Explanation: Jump 1 step from index 0 to 1, then 3 steps to the last index.
   Example 2:

   Input: nums = [3,2,1,0,4]
   Output: false
   Explanation: You will always arrive at index 3 no matter what. Its maximum jump length is 0, which makes it impossible to reach the last index.
 

   Constraints:

   1 <= nums.length <= 3 * 10^4
   0 <= nums[i][j] <= 10^5
*/


#include <stdio.h>
#include <stdbool.h>


bool canJump(int* nums, int numsSize)
{
    int last = numsSize - 1;
    for (int i = last; i >= 0; i--) {
        if (i + nums[i] >= last)
            last = i;
    }
    
    return last == 0;
}

int main(void)
{
    int nums[] = { 2, 3, 1, 1, 4, 0 };
    int numsSize = sizeof(nums) / sizeof(int);
    bool can = canJump(nums, numsSize);
    printf("%d\n", (int)can);
}

/* 
From a given position, when we try to see if we can jump to a GOOD position, 
we only ever use one - the first one. In other words, the left-most one. 
If we keep track of this left-most GOOD position as a separate variable, 
we can avoid searching for it in the array. Not only that, but we can stop 
using the array altogether.
 */
