/* 
   Given a 2D binary matrix filled with 0's and 1's, find the largest square containing only 1's and return its area.

   Example:

   Input: 

   1 0 1 0 0
   1 0 1 1 1
   1 1 1 1 1
   1 0 0 1 0

   Output: 4
*/

/* 
   f(x,y) = min( f(x-1,y), f(x,y-1), f(x-1,y-1)) + 1.
*/


#include <stdio.h>

int min(int a, int b, int c)
{
    if (a <= b && a <= c)
	return a;

    if (b <= c && b <= a)
	return b;

    return c;
}

int max(int a, int b)
{
    return a > b ? a : b;
}


int maximalSquare(char matrix[6][6], int matrixSize, int* matrixColSize)
{
    /* base case */
    if (matrix == NULL)
	return 0;

    int ROW = matrixSize;
    if (ROW == 0)
	return 0;
    
    int COL = matrixColSize[0];
    if (COL == 0)
	return 0;

    int tmp;
    int c[ROW][COL];
    int m = 0;

    for (int i = 0; i < ROW; i++) {
	c[i][0] = matrix[i][0] == '1' ? 1 : 0;
	m = max(m, c[i][0]);
    }


    for (int j = 0; j < COL; j++) {
	c[0][j] = matrix[0][j] == '1' ? 1 : 0;
	m = max(m, c[0][j]);
    }

    for (int i = 1; i < ROW; i++)
	for (int j = 1; j < COL; j++) {
	    if (matrix[i][j] == '0')
		c[i][j] = 0;
	    else {
		c[i][j] = min(c[i-1][j-1], c[i-1][j], c[i][j-1]) + 1;
		m = max(m, c[i][j]);
	    }
	}

    for (int i = 1; i < ROW; i++) {
	for (int j = 1; j < COL; j++) 
	    printf("%d ", c[i][j]);
	printf("\n");
    }


    
    return m * m;
}

int main(void)
{
    char matrix[6][6] = {
	{'1','0','1','1','0','1'},
	{'1','1','1','1','1','1'},
	{'0','1','1','0','1','1'},
	{'1','1','1','0','1','0'},
	{'0','1','1','1','1','1'},
	{'1','1','0','1','1','1'}
    };

    int matrixSize = 6;
    int matrixColSize[] = { 6, 6, 6, 6, 6, 6 };

    int m = maximalSquare(matrix, matrixSize, matrixColSize);
    printf("max: %d\n", m);
}
