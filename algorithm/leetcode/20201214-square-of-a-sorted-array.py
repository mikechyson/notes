# Given an integer array nums sorted in non-decreasing order, return an array of the squares of each number sorted in non-decreasing order.


# Example 1:

# Input: nums = [-4,-1,0,3,10]
# Output: [0,1,9,16,100]
# Explanation: After squaring, the array becomes [16,1,0,9,100].
# After sorting, it becomes [0,1,9,16,100].
# Example 2:

# Input: nums = [-7,-3,2,3,11]
# Output: [4,9,9,49,121]


# Constraints:

# 1 <= nums.length <= 104
# -104 <= nums[i] <= 104
# nums is sorted in non-decreasing order.

from typing import List


def findIndex(nums: List[int], start, end) -> int:
    i = (start + end) // 2
    if nums[i] <= 0 and nums[i + 1] >= 0:
        return i
    elif nums[i] >= 0:
        return findIndex(nums, start, i)
    else:
        return findIndex(nums, i, end)


class Solution:
    def sortedSquares(self, nums: List[int]) -> List[int]:
        # because the array is non-decreasing, use binary search
        # to split the array into left and right subarray

        if nums[-1] <= 0:
            return list(reversed([_ * _ for _ in nums]))

        if nums[0] >= 0:
            return [_ * _ for _ in nums]

        split = findIndex(nums, 0, len(nums) - 1)
        left = list(reversed([_ * _ for _ in nums[0:split + 1]]))
        right = [_ * _ for _ in nums[split + 1:len(nums)]]

        combine = []
        r_start = 0
        l_end = 0
        for l in range(len(left)):
            for r in range(r_start, len(right)):
                if left[l] <= right[r]:
                    combine.append(left[l])
                    l_end += 1
                    break
                else:
                    combine.append(right[r])
                    r_start += 1
                    continue
        combine += left[l_end:len(left)]
        combine += right[r_start:len(right)]
        return combine


if __name__ == '__main__':
    nums = [-4, -1, 0, 3, 10]
    # nums = [-7, -3, 2, 3, 11]
    # nums = [-3, 0, 2]
    # nums = [-9, -7, -5, -3, -1, 2, 4, 4, 7, 10]
    # nums = [-3, -3, -2, 1]
    solution = Solution()
    result = solution.sortedSquares(nums)
    print(result)
