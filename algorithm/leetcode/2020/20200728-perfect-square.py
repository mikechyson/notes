# Given a positive integer n, find the least number of perfect square numbers (for example, 1, 4, 9, 16, ...) which sum to n.

# Example 1:

# Input: n = 12
# Output: 3 
# Explanation: 12 = 4 + 4 + 4.
# Example 2:

# Input: n = 13
# Output: 2
# Explanation: 13 = 4 + 9.



# dynamic programming


class Solution:
    def numSquares(self, n: int) -> int:
        dp = [0, 1, 2, 3]       # cache to save computation
        for i in range(4, n + 1):
            dp.append(i)        # cache
            # go through all smaller numbers to recursive find the minimum
            for x in range(1, int(ceil(sqrt(i))) + 1):
                tmp = x * x
                if tmp > i:
                    break
                else:
                    dp[i] = min(dp[i], 1 + dp[i-tmp])

        return dp[n]
            
        
        
