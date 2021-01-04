# Given an array nums, write a function to move all 0's to the end of it
# while maintaining the relative order of the non-zero elements.
#
# Example:
#
# Input: [0,1,0,3,12]
# Output: [1,3,12,0,0]
# Note:
#
# You must do this in-place without making a copy of the array.
# Minimize the total number of operations.

from typing import List


class Solution:
    def moveZeroes(self, nums: List[int]) -> None:
        """
        Do not return anything, modify nums in-place instead.
        """
        i = 0
        for num in nums:
            if num != 0:
                nums[i] = num
                i += 1

        for z in range(i, len(nums)):
            nums[z] = 0


if __name__ == '__main__':
    nums = [0, 1, 0, 3, 12]
    sol = Solution()
    sol.moveZeroes(nums)
    print(nums)
