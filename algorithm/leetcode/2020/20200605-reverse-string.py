from typing import List


class Solution:
    def reverseString(self, s: List[str]) -> None:
        """
        Do not return anything, modify s in-place instead.
        """
        length = len(s)
        for i in range((length // 2)):
            print(i, length - i - 1)
            tmp = s[length - i - 1]
            s[length - i - 1] = s[i]
            s[i] = tmp
        # print(s)


if __name__ == '__main__':
    s = list('abcef')
    so = Solution()
    so.reverseString(s)
