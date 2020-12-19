# Given an array arr of integers,
# check if there exists two integers N and M
# such that N is the double of M ( i.e. N = 2 * M).
#
# More formally check if there exists two indices i and j such that :
#
# i != j
# 0 <= i, j < arr.length
# arr[i] == 2 * arr[j]
#
#
# Example 1:
#
# Input: arr = [10,2,5,3]
# Output: true
# Explanation: N = 10 is the double of M = 5,that is, 10 = 2 * 5.
# Example 2:
#
# Input: arr = [7,1,14,11]
# Output: true
# Explanation: N = 14 is the double of M = 7,that is, 14 = 2 * 7.
# Example 3:
#
# Input: arr = [3,1,7,11]
# Output: false
# Explanation: In this case does not exist N and M, such that N = 2 * M.
#
#
# Constraints:
#
# 2 <= arr.length <= 500
# -10^3 <= arr[i] <= 10^3

from typing import List


def findIndex(nums: List[int], start, end) -> int:
    """
    Find the first element that is greater than zero
    :param nums:
    :param start:
    :param end:
    :return:
    """
    i = (start + end) // 2
    if nums[i] < 0 and nums[i + 1] >= 0:
        return i + 1
    elif nums[i] >= 0:
        return findIndex(nums, start, i)
    else:
        return findIndex(nums, i, end)


class Solution:
    def checkIfExist(self, arr: List[int]) -> bool:
        if arr is None or len(arr) <= 1:
            return False

        arr = sorted(arr)
        length = len(arr)
        if arr[length - 1] < 0:
            sep_i = length - 1
        elif arr[0] >= 0:
            sep_i = 0
        else:
            sep_i = findIndex(arr, 0, length - 1)
        i = 0
        j = 1
        while i < sep_i and j < sep_i:
            if arr[i] == 2 * arr[j]:
                return True
            elif arr[i] > 2 * arr[j]:
                j += 1
            else:
                i += 1
                j -= 1

        i = sep_i
        j = sep_i + 1
        while i < length and j < length:
            if 2 * arr[i] == arr[j]:
                return True
            elif 2 * arr[i] > arr[j]:
                j += 1
            else:
                i += 1
                j -= 1

        return False


if __name__ == '__main__':
    arr = [10, 2, 5, 3]
    # arr = [7, 1, 14, 11]
    # arr = [3, 1, 7, 11]
    # arr = [-16, -13, 8]
    arr = [0, 0]
    arr = [10, 5, -9, 15, 3, 8, 12, -10]
    arr = [-766, 259, 203, 601, 896, -226, -844, 168, 126, -542, 159, -833, 950, -454, -253, 824, -395, 155, 94, 894,
           -766, -63, 836, -433, -780, 611, -907, 695, -395, -975, 256, 373, -971, -813, -154, -765, 691, 812, 617,
           -919, -616, -510, 608, 201, -138, -669, -764, -77, -658, 394, -506, -675, 523]
    arr = [-2, 0, 10, -19, 4, 6, -8]
    s = Solution()
    result = s.checkIfExist(arr)
    print(result)
