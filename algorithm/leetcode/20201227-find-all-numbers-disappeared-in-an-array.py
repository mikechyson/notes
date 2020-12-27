# Given an array of integers where 1 ≤ a[i] ≤ n (n = size of array),
# some elements appear twice and others appear once.
#
# Find all the elements of [1, n] inclusive that do not appear in this array.
#
# Could you do it without extra space and in O(n) runtime?
# You may assume the returned list does not count as extra space.
#
# Example:
#
# Input:
# [4,3,2,7,8,2,3,1]
#
# Output:
# [5,6]

from typing import List


class Solution:
    def findDisappearedNumbers(self, nums: List[int]) -> List[int]:
        disappeared = []
        i = 0
        length = len(nums)
        while i < length:
            while nums[i] != nums[nums[i] - 1]:
                tmp = nums[nums[i] - 1]
                nums[nums[i] - 1] = nums[i]
                nums[i] = tmp
            i += 1
        for i in range(length):
            if nums[i] != i + 1:
                disappeared.append(i + 1)
        return disappeared


if __name__ == '__main__':
    nums = [4, 3, 2, 7, 8, 2, 3, 1]
    sol = Solution()
    result = sol.findDisappearedNumbers(nums)
    print(result)
