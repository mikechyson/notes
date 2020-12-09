/* 
   Given a range [m, n] where 0 <= m <= n <= 2147483647, return the bitwise AND of all numbers in this range, inclusive.

   Example 1:

   Input: [5,7]
   Output: 4
   Example 2:

   Input: [0,1]
   Output: 0
*/

#include <stdio.h>

int rangeBitwiseAnd(int m, int n)
{
    int result = m & n;
    int gap = n - m;
    int i;
    
    for (i = 0; gap > 0; i++) 
	gap /= 2;

    printf("result: %d, i: %d\n", result, i);
    return result >> i << i;
	
}

int main(void)
{
    
    printf("result: %d\n", rangeBitwiseAnd(5, 7));
    printf("result: %d\n", rangeBitwiseAnd(0, 1));
    printf("result: %d\n", rangeBitwiseAnd(3, 6));
    printf("result: %d\n", rangeBitwiseAnd(1, 1));

    
}
