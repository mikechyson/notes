/** 
    Given a non negative integer number num. For every numbers i in the range 0 ≤ i ≤ num calculate the number of 1's in their binary representation and return them as an array.

    Example 1:

    Input: 2
    Output: [0,1,1]
    Example 2:

    Input: 5
    Output: [0,1,1,2,1,2]
    Follow up:

    It is very easy to come up with a solution with run time O(n*sizeof(integer)). But can you do it in linear time O(n) /possibly in a single pass?
    Space complexity should be O(n).
    Can you do it like a boss? Do it without using any builtin function like __builtin_popcount in c++ or in any other language.
*/


/**
 * Note: The returned array must be malloced, assume caller calls free().
 */

/* discipline:
   [0, 1]
   [0, 1, 1, 2]
   [0, 1, 1, 2, 1, 2, 2, 3]
   [0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4]
 */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

int* countBits(int num, int* returnSize){
    int *array = (int *) malloc((num+1) * sizeof(int));
    *returnSize = num + 1;
    
    // init the first two element the output array
    array[0] = 0;
    if (num == 0) {
        return array;
    } else {
	array[1] = 1;
    }



    double idx = 0.0;
    while (pow(2, idx) <= (double)num) {
	idx = idx + 1;
    }
    int prod = (int) pow(2, idx-1) - 1;
    printf("prod: %d, num: %d\n", prod, num);


    
    for (int i = 2; i < prod ; i *= 2) {
	for (int j = 0; j < i; j++) {
	    array[i + j] = array[j] + 1;
	    printf("array[%d + %d] = array[%d] + 1; \n", i, j, j);
	    printf("array[%d] = %d\n", i+j, array[i+j]);
	}
    }

    int i = 1;
    for (; i + prod <= num; i++) {
	printf("array[%d + %d] = array[%d - 1] + 1; \n", i, prod, i);
	array[i + prod] = array[i-1] + 1;
    }


    return array;

}

int main(void)
{
    int num = 2;
    int returnSize;
    int *array;

    for (int num = 0; num < 5; num++) {
	printf("\n\n\n\n");
	array = countBits(num, &returnSize);
	for (int i = 0; i < returnSize; i ++) 
	    printf("%d ", array[i]);
    }

}
