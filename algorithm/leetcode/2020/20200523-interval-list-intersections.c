/* 
   Given two lists of closed intervals, each list of intervals is pairwise disjoint and in sorted order.

   Return the intersection of these two interval lists.

   (Formally, a closed interval [a, b] (with a <= b) denotes the set of real numbers x with a <= x <= b.  The intersection of two closed intervals is a set of real numbers that is either empty, or can be represented as a closed interval.  For example, the intersection of [1, 3] and [2, 4] is [2, 3].)

 

   Example 1:

   Input: A = [[0,2],[5,10],[13,23],[24,25]], B = [[1,5],[8,12],[15,24],[25,26]]
   Output: [[1,2],[5,5],[8,10],[15,23],[24,24],[25,25]]
   Reminder: The inputs and the desired output are lists of Interval objects, and not arrays or lists.
 

   Note:

   0 <= A.length < 1000
   0 <= B.length < 1000
   0 <= A[i].start, A[i].end, B[i].start, B[i].end < 10^9
   NOTE: input types have been changed on April 15, 2019. Please reset to default code definition to get new method signature.
*/


/**
 * Return an array of arrays of size *returnSize.
 * The sizes of the arrays are returned as *returnColumnSizes array.
 * Note: Both returned array and *columnSizes array must be malloced, assume caller calls free().
 */
int** intervalIntersection(int** A, int ASize, int* AColSize, int** B, int BSize, int* BColSize, int* returnSize, int** returnColumnSizes)
{
    *returnSize = 0;
    int **C;
    

    for (int i = 0; i < ASize; i++)
	for (int j = 0; j < BSize; j++) {
	    if (B[j][1] < A[i][0]) /* B is before A, continue in B */
		continue;

	    if (B[j][1] >= A[i][0] && B[j][1] <= A[i][1]) { /* intersection between A and B */
		**C = B[j][1];
		*returnSize += 1;
		
	    }
	}
}
