# Given two sorted integer arrays nums1 and nums2, merge nums2 into nums1 as one sorted array.
#
# Note:
#
# The number of elements initialized in nums1 and nums2 are m and n respectively.
# You may assume that nums1 has enough space (size that is equal to m + n) to hold additional elements from nums2.
# Example:
#
# Input:
# nums1 = [1,2,3,0,0,0], m = 3
# nums2 = [2,5,6],       n = 3
#
# Output: [1,2,2,3,5,6]
#
#
# Constraints:
#
# -10^9 <= nums1[i], nums2[i] <= 10^9
# nums1.length == m + n
# nums2.length == n

from typing import List


class Solution:
    def merge(self, nums1: List[int], m: int, nums2: List[int], n: int) -> None:
        """
        Do not return anything, modify nums1 in-place instead.
        """
        if n == 0:
            return None

        i1 = 0  # index nums1
        i2 = 0  # index nums2
        i = 0  # index index in nums1
        index = 0  # total index in nums1
        # Store the index of nums2 should be inserted
        # into nums1
        while i1 < m and i2 < n:
            if nums1[i1] <= nums2[i2]:
                i1 += 1
            else:
                nums1[m + i] = index
                i2 += 1
                i += 1
            index += 1

        if n == 1 and nums1[0] > nums2[0]:
            pass
        else:
            for i in range(m + n - 1, m - 1, -1):
                if nums1[i] == 0:
                    nums1[i] = i
                else:
                    break
        print(nums1)

        # Put elements in nums2 into nums1 according to
        # the index stored in nums1
        # i2 = 0  # used to put elements stored in nums2 back
        # i = m  # start from m to iterate index stored in nums1
        # index = nums1[i]
        # while index < m + n:
        #     if index == nums1[i]:
        #         tmp = nums1[index]
        #         nums1[index] = nums2[i - m]
        #         nums2[i - m] = tmp
        #         if i < m + n - 1:
        #             i += 1
        #     else:
        #         nums1[index] = nums2[i2]
        #         i2 += 1
        #     index += 1
        for i in range(m, m + n):
            tmp = nums1[i]
            for j in range(i, nums1[i], -1):
                nums1[j] = nums1[j - 1]
            nums1[tmp] = nums2[i - m]


if __name__ == '__main__':
    nums1 = [1, 2, 3, 0, 0, 0]
    # nums1 = [1, 7, 10, 0, 0, 0]
    m = 3
    nums2 = [2, 5, 6]
    n = 3

    nums1 = [2, 0]
    m = 1
    nums2 = [1]
    n = 1
    # #
    # nums1 = [1, 0]
    # m = 1
    # nums2 = [2]
    # n = 1
    #
    # nums1 = [1, 2, 4, 5, 6, 0, 0]
    # m = 5
    # nums2 = [3, 7]
    # n = 2
    s = Solution()
    s.merge(nums1, m, nums2, n)
    print(nums1)
