# Given two words word1 and word2, find the minimum number of operations required to convert word1 to word2.

# You have the following 3 operations permitted on a word:

# Insert a character
# Delete a character
# Replace a character
# Example 1:

# Input: word1 = "horse", word2 = "ros"
# Output: 3
# Explanation: 
# horse -> rorse (replace 'h' with 'r')
# rorse -> rose (remove 'r')
# rose -> ros (remove 'e')
# Example 2:

# Input: word1 = "intention", word2 = "execution"
# Output: 5
# Explanation: 
# intention -> inention (remove 't')
# inention -> enention (replace 'i' with 'e')
# enention -> exention (replace 'n' with 'x')
# exention -> exection (replace 'n' with 'c')
# exection -> execution (insert 'u')




class Solution:
    def minDistance(self, word1: str, word2: str) -> int:
        len1 = len(word1)
        len2 = len(word2)

        dp = [[0 for x in range(len2+1)] for x in range(len1+1)] # dynamic programming

        for i in range(len1 + 1):
            for j in range(len2 + 1):
                # if first string is empty, only option is
                # to insert all characters of second string
                if i == 0:
                    dp[i][j] = j
                elif j == 0:
                    dp[i][j] = i
                elif word1[i-1] == word2[j-1]:
                    # If last characters are same, ignore last char 
                    # and recur for remaining string 
                    dp[i][j] = dp[i-1][j-1]
                else:
                    dp[i][j] = 1 + min(dp[i][j-1], # insert
                                       dp[i-1][j], # remove
                                       dp[i-1][j-1]) # replace
        return dp[len1][len2]


if __name__ == '__main__':
    d = Solution()
    m = d.minDistance('abcdef','abcfed')
    print(m)
