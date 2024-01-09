unit Screen
{
    // MCU version is in Hopper (no system calls)
    uses "/Source/System/Color"
    uses "/Source/Library/Display"
    
    const uint cellWidth = 6;
    const uint cellHeight = 10;
    
    byte cursorX;
    byte cursorY;
    string fontData;
    uint[cellWidth*cellHeight] cellBuffer;
    
    byte CursorX { get { cursorX; }}
    byte CursorY { get { cursorY; }}
    byte Columns { get { return byte(Display.PixelWidth / cellWidth); }}
    byte Rows    { get { return byte(Display.PixelHeight / cellHeight); }}
    
    string FontData { get { return fontData; } set { fontData = value; } }
    
    render6x8MonoCharacter(char chr, uint foreColour, uint backColour)
    {
        uint pixelb = ((backColour == 0x0000) || (backColour == 0xF000)) ? 0x0000 : 0x0FFF;
        uint pixelf = ((foreColour == 0x0000) || (foreColour == 0xF000)) ? 0x0000 : 0x0FFF;
        if ((chr <= char(32)) || (chr > char(127)))
        {
            // ' '
            for (uint i = 0; i < cellWidth*cellHeight; i++)
            {
                cellBuffer[i] = pixelb;
            }
            return;
        }
        uint bi = 0;
        // top row
        cellBuffer[bi] = pixelb; bi++;
        cellBuffer[bi] = pixelb; bi++;
        cellBuffer[bi] = pixelb; bi++;
        cellBuffer[bi] = pixelb; bi++;
        cellBuffer[bi] = pixelb; bi++;
        cellBuffer[bi] = pixelb; bi++;
        
        uint addr = 0;
        addr = addr + (5 * (byte(chr)-32));
        byte[5] colColours;
        for (byte x = 0; x < 5; x++)
        {
            colColours[x] = byte(fontData[addr]); addr++;
        }

        for (byte y = 0; y < 8; y++)     // 0..7
        {
            for (byte x = 0; x < 5; x++) // 0..4
            {
                cellBuffer[bi] = (((colColours[x] & (1 << y)) != 0) ? pixelf : pixelb); bi++;
            }
            // space on right
            cellBuffer[bi] = pixelb; bi++;
        }
    
        // bottom row
        cellBuffer[bi] = pixelb; bi++;
        cellBuffer[bi] = pixelb; bi++;
        cellBuffer[bi] = pixelb; bi++;
        cellBuffer[bi] = pixelb; bi++;
        cellBuffer[bi] = pixelb; bi++;
        cellBuffer[bi] = pixelb; bi++;
    }
    
    scrollOneLine()
    {
        Display.Suspend();
        Display.ScrollUp(cellHeight);
        cursorY--;
        Display.Resume();
    }
    
    Clear() { Display.Clear(Color.Black); }
    
    SetCursor(uint col, uint row)
    {
        cursorX = col;
        cursorY = row;
    }

    DrawChar(uint col, uint row, char c, uint foreColour, uint backColour)
    {
        int x0 = int(col * cellWidth);
        int y0 = int(row * cellHeight);
        if (fontData.Length > 0)
        {
            Display.Suspend();
            render6x8MonoCharacter(c, foreColour, backColour);
            for (int y=0; y < cellHeight; y++)
            {
                for (int x=0; x < cellWidth; x++)
                {
                    uint colour = cellBuffer[x + y * cellWidth];
                    SetPixel(x+x0, y+y0, colour);
                }
            }
            Display.Resume();
        }
    }
    Print(char c,     uint foreColour, uint backColour)
    {
        DrawChar(cursorX, cursorY, c, foreColour, backColour);
        cursorX++;
        if (cursorX >= Columns)
        {
            cursorX = 0;
            cursorY++;
            if (cursorY == Rows)
            {
                scrollOneLine();
            }
        }
        
    }
    Print(string s,   uint foreColour, uint backColour)
    {
        Display.Suspend();
        foreach (var c in s)
        {
            Print(c, foreColour, backColour)
        }
        Display.Resume();
    }
    PrintLn()
    {
        cursorX = 0;
        cursorY++;
        if (cursorY == Rows)
        {
            scrollOneLine();
        }
    }

    PrintLn(char c,   uint foreColour, uint backColour)
    {
        Display.Suspend();
        Print(c, foreColour, backColour);
        PrintLn();
        Display.Resume();
    }
    PrintLn(string s, uint foreColour, uint backColour)
    {
        Display.Suspend();
        Print(s, foreColour, backColour);
        PrintLn();
        Display.Resume();
    }
    
    Print(char c)
    {
        Print(c, Color.MatrixGreen, Color.Black);
    }
    Print(string s)
    {
        Print(s, Color.MatrixGreen, Color.Black);
    }
    PrintLn(char c)
    {  
        PrintLn(c, Color.MatrixGreen, Color.Black);
    }
    PrintLn(string s)
    {  
        PrintLn(s, Color.MatrixGreen, Color.Black);
    }
}
