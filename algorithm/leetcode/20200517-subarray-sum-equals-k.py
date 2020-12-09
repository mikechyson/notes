class Solution:
    def subarraySum(self, nums: List[int], k: int) -> int:
        sums = {0:1}
        count = 0;
        current_sum = 0;
        nums_len = len(nums)

        for i in range(nums_len):
            current_sum += nums[i]

            if current_sum - k in sums:
                count += sums[current_sum - k]
                
            sums[current_sum] = sums.get(current_sum, 0) + 1

        return count;    
