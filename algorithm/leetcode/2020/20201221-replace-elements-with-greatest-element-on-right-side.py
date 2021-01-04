# Given an array arr, replace every element in that array
# with the greatest element among the elements to its right,
# and replace the last element with -1.
#
# After doing so, return the array.
#
#
#
# Example 1:
#
# Input: arr = [17,18,5,4,6,1]
# Output: [18,6,6,6,1,-1]
#
#
# Constraints:
#
# 1 <= arr.length <= 10^4
# 1 <= arr[i] <= 10^5

from typing import List


class Solution:
    def replaceElements(self, arr: List[int]) -> List[int]:
        i = 0
        length = len(arr)
        while i < length - 1:
            max_index = arr[i + 1:].index(max(arr[i + 1:])) + i + 1
            for i in range(i, max_index):
                arr[i] = arr[max_index]
            i = max_index

        arr[length - 1] = -1
        return arr


if __name__ == '__main__':
    arr = [17, 18, 5, 4, 6, 1]
    sol = Solution()
    result = sol.replaceElements(arr)
    print(arr)
