from typing import List


class Solution:
    def twoSum(self, nums: List[int], target: int) -> List[int]:
        for i in range(len(nums) - 1):
            if target - nums[i] in nums[i + 1:]:
                return [i, i + 1 + nums[i + 1:].index(target - nums[i])]


if __name__ == '__main__':
    nums = [2, 7, 11, 15]
    target = 9
    nums = [3, 2, 4]
    target = 6
    nums = [3, 3]
    target = 6
    sol = Solution()
    result = sol.twoSum(nums, target)
    print(result)
