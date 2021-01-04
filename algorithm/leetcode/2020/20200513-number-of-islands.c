/* 
   Given a 2d grid map of '1's (land) and '0's (water), count the number of islands. An island is surrounded by water and is formed by connecting adjacent lands horizontally or vertically. You may assume all four edges of the grid are all surrounded by water.

   Example 1:

   Input:
   11110
   11010
   11000
   00000

   Output: 1
   Example 2:

   Input:
   11000
   11000
   00100
   00011

   Output: 3
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* 4 neighbours of a given cell */
int row_nei[] = { -1, 0, 0, 1};
int col_nei[] = { 0, -1, 1, 0};


int isSafe(char **grid, int row, int col,  int **visited, int maxrow, int maxcol)
{
    return row >= 0 &&
	row < maxrow &&
	col >= 0 &&
	col <= maxcol &&
	(*(*(grid+row)+col) == '1' && !*(*(visited+row)+col));
}

void dfs(char **grid, int row, int col,  int **visited, int maxrow, int maxcol)
{
    /* printf("visited: %d\n", visited); */
    int i;
    *(*(visited+row)+col) = 1;


    for (i = 0; i < 4; i++) {
    	if (isSafe(grid, row + row_nei[i], col + col_nei[i], visited, maxrow, maxcol))
    	    dfs(grid, row + row_nei[i], col + col_nei[i], visited, maxrow, maxcol);
    }
}
 

int numIslands(char** grid, int gridSize, int* gridColSize){

    int maxrow = gridSize;
    int maxcol = gridColSize[0];
    int i, j;
    int count = 0;

    int **visited = (int **) malloc(maxrow * sizeof(int *));
    for (i = 0; i < maxrow; i++)
	visited[i] = (int *) malloc(maxcol * sizeof(int));

    for (i = 0; i < maxrow; i++)
	for (j = 0; j < maxcol; j++)
	    *(*(visited+i)+j) = 0;
    

    for (i = 0; i < maxrow; i++) {
	for (j = 0; j < maxcol; j++) {
	    if (*(*(grid+i)+j) == '1' && !*(*(visited+i)+j)) { /* a cell with 1 and not visited, new island found */
	    	dfs(grid, i, j, visited, maxrow, maxcol);
	    	count++;
	    }
	}
    }
    return count;
}

   


int main(void)
{
    char *grid[] = {
	"11000",
	"11000",
	"00100",
	"00011"
    };
    int maxrow = 4;
    int maxcol[] = { 5, 5, 5, 5 };
    printf("%d\n", numIslands(grid, maxrow, maxcol));

    return 0;
	   
}
