unit GameGrid
{
    uses "DisplayHelper"
    
#ifdef MCU
    const byte Width = 10;
    const byte Height = 18;
#else    
    const byte Width = 10;
    const byte Height = 20;
#endif
    
    uint[Width * Height] colors;

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
                colors[col + row * Width] = Colour.Black;
            }
        }
    }

    SetCell(byte x, byte y, uint color)
    {
        uint index = x + y * Width;
        colors[index] = color;
    }

    bool GetCell(byte x, byte y)
    {
        return colors[x + y * Width] != Colour.Black;
    }

    uint GetColor(byte x, byte y)
    {
        return colors[x + y * Width];
    }

    Render()
    {
        for (byte row = 0; row < Height; row++)
        {
            for (byte col = 0; col < Width; col++)
            {
                uint index = col + row * Width;
                DrawCell(col, row);
            }
        }
    }

    bool IsRowFull(byte row)
    {
        for (byte col = 0; col < Width; col++)
        {
            if (!GetCell(col, row))
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
                colors[col + y * Width] = colors[col + (y - 1) * Width];
            }
        }
        for (byte col = 0; col < Width; col++)
        {
            colors[col] = Colour.Black;
        }
    }

    DrawCell(byte x, byte y)
    {
        DisplayHelper.DrawCell(x, y, colors[x + y * Width]);
    }
}

