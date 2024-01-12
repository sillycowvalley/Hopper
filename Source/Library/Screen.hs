unit Screen
{
    // MCU version is in Hopper (no system calls)
    uses "/Source/System/Color"
#ifdef DISPLAY_DRIVER    
    uses "/Source/Library/Display"
#endif

#ifdef FONT_EXISTS
    const uint cellWidth     = CellWidth +1;
    const uint cellHeight    = CellHeight+1;
    uint[cellWidth*cellHeight] cellBuffer;
#endif

    byte cursorX;
    byte cursorY;
    string fontData;
    
    
    byte CursorX { get { cursorX; }}
    byte CursorY { get { cursorY; }}
#if defined(DISPLAY_DRIVER) && defined(FONT_EXISTS)
    byte Columns { get { return byte(Display.PixelWidth / cellWidth); }}
    byte Rows    { get { return byte(Display.PixelHeight / cellHeight); }}
#else
    byte Columns { get { return 0; }}
    byte Rows    { get { return 0; }}
#endif

#if defined(DISPLAY_DRIVER) && defined(FONT_EXISTS)
    string FontData { get { return fontData; } set { fontData = value; } }
    renderMonoCharacter(char chr, uint foreColour, uint backColour)
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
        
        uint addr = 0;
        addr = addr + (Font.CellWidth * (byte(chr)-32));
        byte[Font.CellWidth] colColours;
        for (byte x = 0; x < Font.CellWidth; x++)
        {
            colColours[x] = byte(fontData[addr]); addr++;
        }
        
        // This presumes fonts <= 8 pixels high (bytes per character == CellWidth)
        uint bi = 0;
        for (byte y = 0; y < Font.CellHeight; y++)     // 0..6
        {
            for (byte x = 0; x < Font.CellWidth; x++) // 0..4
            {
                cellBuffer[bi] = (((colColours[x] & (1 << y)) != 0) ? pixelf : pixelb); bi++;
            }
            // space on right
            cellBuffer[bi] = pixelb; bi++;
        }
    
        // bottom row
        for (byte i = 0; i < cellWidth; i++)
        {
            cellBuffer[bi] = pixelb; bi++;
        }
    }
#endif

    scrollOneLine()
    {
        
#ifdef FONT_EXISTS
        byte scrollLines = cellHeight;
#else
        byte scrollLines = 8;
#endif

#ifdef DISPLAY_DRIVER
        Display.Suspend();
        Display.ScrollUp(scrollLines);
#endif
        cursorY--;
#ifdef DISPLAY_DRIVER
        Display.Resume();
#endif
    }
    
    Clear() 
    {
#ifdef DISPLAY_DRIVER
        Display.Clear(Color.Black); 
#endif
        cursorX = 0;
        cursorY = 0;
    }
    
    SetCursor(uint col, uint row)
    {
        cursorX = col;
        cursorY = row;
    }

    DrawChar(uint col, uint row, char c, uint foreColour, uint backColour)
    {
#if defined(DISPLAY_DRIVER) && defined(FONT_EXISTS)
        int x0 = int(col * cellWidth);
        int y0 = int(row * cellHeight);
        if (fontData.Length > 0)
        {
            Display.Suspend();
            renderMonoCharacter(c, foreColour, backColour);
            for (int y=0; y < cellHeight; y++)
            {
                for (int x=0; x < cellWidth; x++)
                {
                    uint colour = cellBuffer[x + y * cellWidth];
                    DisplayDriver.SetPixel(x+x0, y+y0, colour);
                }
            }
            Display.Resume();
        }
#endif
    }
    Print(char c,     uint foreColour, uint backColour)
    {
#ifdef DISPLAY_DRIVER
        DrawChar(cursorX, cursorY, c, foreColour, backColour);
#endif
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
#ifdef DISPLAY_DRIVER
        Display.Suspend();
#endif
        foreach (var c in s)
        {
            Print(c, foreColour, backColour);
        }
#ifdef DISPLAY_DRIVER
        Display.Resume();
#endif
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
#ifdef DISPLAY_DRIVER
        Display.Suspend();
#endif
        Print(c, foreColour, backColour);
        PrintLn();
#ifdef DISPLAY_DRIVER
        Display.Resume();
#endif
    }
    PrintLn(string s, uint foreColour, uint backColour)
    {
#ifdef DISPLAY_DRIVER
        Display.Suspend();
#endif
        Print(s, foreColour, backColour);
        PrintLn();
#ifdef DISPLAY_DRIVER
        Display.Resume();
#endif
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
