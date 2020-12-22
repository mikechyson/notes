# Given a sorted array nums,
# remove the duplicates in-place such that
# each element appears only once and returns the new length.
#
# Do not allocate extra space for another array,
# you must do this by modifying the input array in-place with O(1) extra memory.
#
#
# Example 1:
#
# Input: nums = [1,1,2]
# Output: 2, nums = [1,2]
# Explanation: Your function should return length = 2,
# with the first two elements of nums being 1 and 2 respectively.
# It doesn't matter what you leave beyond the returned length.
# Example 2:
#
# Input: nums = [0,0,1,1,1,2,2,3,3,4]
# Output: 5, nums = [0,1,2,3,4]
# Explanation: Your function should return length = 5,
# with the first five elements of nums being modified to 0, 1, 2, 3, and 4 respectively.
# It doesn't matter what values are set beyond the returned length.
#
#
# Constraints:
#
# 0 <= nums.length <= 3 * 104
# -104 <= nums[i] <= 104
# nums is sorted in ascending order.

from typing import List


class Solution:
    def removeDuplicates(self, nums: List[int]) -> int:
        if nums is None or len(nums) == 0:
            return nums

        length = len(nums)
        current_index = 0
        check_index = 0
        be_checked_index = 1
        while be_checked_index < len(nums):
            if nums[check_index] == nums[be_checked_index]:
                length -= 1
            else:
                current_index += 1
                check_index = be_checked_index
                nums[current_index] = nums[be_checked_index]
            be_checked_index += 1
        return nums[:length]

# A better solution

# class Solution:
#     def removeDuplicates(self, nums: List[int]) -> int:
#         length = len(nums)
#         if length <= 1:
#             return length
#         i = 1
#         for num in nums:
#             if nums[i - 1] != num:
#                 nums[i] = num
#                 i += 1
#         return i
