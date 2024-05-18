unit GameGrid
{
    uses "/Source/System/Screen"

    const byte Width = 10;
    const byte Height = 20;
    bool[Width * Height] grid;

    Initialize()
    {
        Clear();
    }

    Clear()
    {
        for (byte row = 0; row < Height; row++)
        {
            for (byte col = 0; col < Width; col++)
            {
                grid[col + row * Width] = false;
            }
        }
    }

    SetCell(byte x, byte y, bool filled)
    {
        grid[x + y * Width] = filled;
        Screen.DrawChar(x, y, ' ', Colour.White, filled ? Colour.White : Colour.Black);
    }

    bool GetCell(byte x, byte y)
    {
        return grid[x + y * Width];
    }

    Render()
    {
        for (byte row = 0; row < Height; row++)
        {
            for (byte col = 0; col < Width; col++)
            {
                Screen.DrawChar(col, row, ' ', Colour.White, grid[col + row * Width] ? Colour.White : Colour.Black);
            }
        }
    }

    bool IsRowFull(byte row)
    {
        for (byte col = 0; col < Width; col++)
        {
            if (!grid[col + row * Width])
            {
                return false;
            }
        }
        return true;
    }

    ClearRow(byte row)
    {
        for (byte y = row; y > 0; y--)
        {
            for (byte col = 0; col < Width; col++)
            {
                grid[col + y * Width] = grid[col + (y - 1) * Width];
            }
        }
        for (byte col = 0; col < Width; col++)
        {
            grid[col] = false;
        }
    }
}

