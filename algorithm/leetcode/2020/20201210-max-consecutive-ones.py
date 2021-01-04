# Input: [1,1,0,1,1,1]
# Output: 3
# Explanation: The first two digits or the last three digits are consecutive 1s.
#     The maximum number of consecutive 1s is 3.

class Solution:
    def findMaxConsecutiveOnes(self, nums: List[int]) -> int:
        current = 0
        maximum = 0
        accumulate = False
        
        for i in range(len(nums)):
            if accumulate:
                if nums[i] == 1:
                    current += 1
                else:
                    accumulate = False
                
                if current > maximum:
                    maximum = current
                    
            else:
                current = 0
                if nums[i] == 1:
                    accumulate = True
                    current += 1
                
                if current > maximum:
                    maximum = current
         
        return maximum


# Better solution:
class Solution:
    def findMaxConsecutiveOnes(self, nums):
        local_max = 0
        abs_max = 0
        for i in nums:
            if i ==1:
                local_max += 1
            else:
                if local_max > abs_max:
                    abs_max = local_max
                local_max = 0
        return max(abs_max,local_max)
