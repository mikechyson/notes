
class Solution:
    def numIslands(self, grid: List[List[str]]) -> int:
        if len(grid) == 0:
            return 0

        self.grid = grid 
        self.row = len(grid)
        self.col = len(grid[0])

        visited = [[False for j in range(self.col)] for i in range(self.row)]
        
        count = 0
        for i in range(self.row):
            for j in range(self.col):
                if visited[i][j] == False and self.grid[i][j] == '1':
                    self.dfs(i, j, visited)
                    count += 1
        return count
    
        
    def isSafe(self, i, j, visited):
        return (i >= 0 and i <self.row and
                j >= 0 and j < self.col and
                self.grid[i][j] and not visited[i][j])
    
    def dfs(self, i, j, visited):
        visited[i][j] = True
        row_nei = [ -1, 0, 0, 1]
        col_nei = [ 0, -1, 1, 0]
        for k in range(4):
            if self.isSafe(i + row_nei[k], j + col_nei[k], visited):
                self.dfs(i + row_nei[k], j + col_nei[k], visited)
        
      
