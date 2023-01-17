using System;
using System.Collections.Generic;

public class ConnectedNodes
{
    static int[,] matrix;
    static int rows;
    static int cols;
    static bool[] visited;

    static void DFS(int currentNode)
    {
        visited[currentNode] = true;
        int row = currentNode / cols;
        int col = currentNode % cols;
        if (row > 0 && matrix[row - 1, col] == 1 && !visited[(row - 1) * cols + col])
            DFS((row - 1) * cols + col);
        if (row < rows - 1 && matrix[row + 1, col] == 1 && !visited[(row + 1) * cols + col])
            DFS((row + 1) * cols + col);
        if (col > 0 && matrix[row, col - 1] == 1 && !visited[row * cols + col - 1])
            DFS(row * cols + col - 1);
        if (col < cols - 1 && matrix[row, col + 1] == 1 && !visited[row * cols + col + 1])
            DFS(row * cols + col + 1);
    }

    static bool IsConnected()
    {
        int startNode = -1;
        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                if (matrix[i, j] == 1)
                {
                    startNode = i * cols + j;
                    break;
                }
            }
            if (startNode != -1)
                break;
        }
        if (startNode == -1)
            return true;
        DFS(startNode);
        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                if (matrix[i, j] == 1 && !visited[i * cols + j])
                    return false;
            }
        }
        return true;
    }

    public static void Main()
    {
        matrix = new int[,] { { 0, 0 }, { 1, 0 } };
        rows = matrix.GetLength(0);
        cols = matrix.GetLength(1);
        visited = new bool[rows * cols];
        Console.WriteLine(IsConnected());
    }
}
