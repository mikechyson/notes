# There are 2N people a company is planning to interview. The cost of flying the i-th person to city A is costs[i][0], and the cost of flying the i-th person to city B is costs[i][1].

# Return the minimum cost to fly every person to a city such that exactly N people arrive in each city.


# Example 1:

# Input: [[10,20],[30,200],[400,50],[30,20]]
# Output: 110
# Explanation: 
# The first person goes to city A for a cost of 10.
# The second person goes to city A for a cost of 30.
# The third person goes to city B for a cost of 50.
# The fourth person goes to city B for a cost of 20.

# The total minimum cost is 10 + 30 + 50 + 20 = 110 to have half the people interviewing in each city.


# Note:

# 1 <= costs.length <= 100
# It is guaranteed that costs.length is even.
# 1 <= costs[i][0], costs[i][1] <= 1000


from typing import List


class Solution:
    def twoCitySchedCost(self, costs: List[List[int]]) -> int:
        nums = len(costs)
        costs = sorted(costs, key=lambda x: abs(x[0] - x[1]), reverse=True)
        list_a = list()
        list_b = list()
        for i in range(nums):
            if costs[i][0] <= costs[i][1]:
                if len(list_a) <= nums // 2:
                    list_a.append(costs[i][0])
                else:
                    list_b.append(costs[i][1])
            else:
                if len(list_b) <= nums // 2:
                    list_b.append(costs[i][1])
                else:
                    list_a.append(costs[i][0])

        return sum(list_a) + sum(list_b)


if __name__ == '__main__':
    costs = [[10, 20], [30, 200], [400, 50], [30, 20]]

    solution = Solution()
    cost = solution.twoCitySchedCost(costs)
    print(cost)
