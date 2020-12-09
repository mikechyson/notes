/* 
   Given a m x n grid filled with non-negative numbers, find a path from top left to bottom right which minimizes the sum of all numbers along its path.

   Note: You can only move either down or right at any point in time.

   Example:

   Input:
   [
   [1,3,1],
   [1,5,1],
   [4,2,1]
   ]
   Output: 7
   Explanation: Because the path 1→3→1→1→1 minimizes the sum.
*/

#include <limits.h>

int min(int a, int b)
{
    return a < b ? a : b;
}

int minPathSum(int** grid, int gridSize, int* gridColSize){
    int i,j;
    
    const int row = gridSize;
    if (row == 0)    
        return 0;
    
    const int col = gridColSize[0];
    if (col == 0)
        return 0;
    
    int **paths = (int **) malloc(row * sizeof(int *));
    for (i = 0; i < row; i++)
        *(paths+i) = (int *) malloc(col * sizeof(int));
    
    for (i = 0; i < row; i++)
        for (j = 0; j < col; j++)
            *(*(paths+i)+j) = 0;
    
    for (i = 0; i < row; i++)
        for (j = 0; j < col; j++) {
            if(i-1 >= 0 && j-1 >= 0)
                *(*(grid+i)+j) += min(*(*(grid+i-1)+j),*(*(grid+i)+j-1));
            else {
                if (i-1 >= 0)
                    *(*(grid+i)+j) += *(*(grid+i-1)+j);
                if (j-1 >= 0)
                    *(*(grid+i)+j) += *(*(grid+i)+j-1);
            }
            
        }
    return *(*(grid+row-1)+col-1);
    
}
