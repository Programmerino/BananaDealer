using System;
using System.Linq;
using System.Collections.Generic;

namespace Connected
{
    public class Kosaraju
    {
        // Helper function to perform a depth-first search
        private void DFS(int[,] matrix, int i, int j, bool[,] visited)
        {
            visited[i, j] = true;

            // Check left
            if (j > 0 && matrix[i, j - 1] == 1 && !visited[i, j - 1])
            {
                DFS(matrix, i, j - 1, visited);
            }

            // Check right
            if (j < matrix.GetLength(1) - 1 && matrix[i, j + 1] == 1 && !visited[i, j + 1])
            {
                DFS(matrix, i, j + 1, visited);
            }

            // Check up
            if (i > 0 && matrix[i - 1, j] == 1 && !visited[i - 1, j])
            {
                DFS(matrix, i - 1, j, visited);
            }

            // Check down
            if (i < matrix.GetLength(0) - 1 && matrix[i + 1, j] == 1 && !visited[i + 1, j])
            {
                DFS(matrix, i + 1, j, visited);
            }
        }

        public bool IsConnected(int[,] matrix)
        {
            // Initialize visited array
            bool[,] visited = new bool[matrix.GetLength(0), matrix.GetLength(1)];

            // Perform initial depth-first search starting from the first 1 in the matrix
            for (int i = 0; i < matrix.GetLength(0); i++)
            {
                for (int j = 0; j < matrix.GetLength(1); j++)
                {
                    if (matrix[i, j] == 1)
                    {
                        DFS(matrix, i, j, visited);
                        break;
                    }
                }
            }

            // Check if all 1's have been visited
            for (int i = 0; i < matrix.GetLength(0); i++)
            {
                for (int j = 0; j < matrix.GetLength(1); j++)
                {
                    if (matrix[i, j] == 1 && !visited[i, j])
                    {
                        return false;
                    }
                }
            }

            return true;
        }
    }

}