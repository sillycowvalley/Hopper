unit Screen
{
    // MCU version is in Hopper (no system calls)
    uses "/Source/System/Colour"
    
#ifdef DISPLAY_DRIVER    
    uses "/Source/Library/Display"
#endif

    friend DisplayDriver;

#ifdef FONT_EXISTS
    const uint cellWidth     = CellWidth  + 1;
    const uint cellHeight    = CellHeight + 1;
    uint[cellWidth*cellHeight] cellBuffer;
#endif
    
#if defined(DISPLAY_DRIVER) && defined(FONT_EXISTS)
    byte cursorX;
    byte cursorY;
    byte textScale = 1;
    uint defaultForeColour = Colour.MatrixGreen;
    uint defaultBackColour = Colour.Black;
    uint ForeColour { get { return defaultForeColour; } set { defaultForeColour = value; }}
    uint BackColour { get { return defaultBackColour; } set { defaultBackColour = value; }}
    byte TextScale  { get { return textScale; } set { textScale = value; }}
    byte CursorX { get { return cursorX; } set { cursorX = value; } }
    byte CursorY { get { return cursorY; } set { cursorY = value; } }
    byte Columns { get { return byte(Display.PixelWidth / cellWidth); }}
    byte Rows    { get { return byte(Display.PixelHeight / cellHeight); }}
    
#else
    // stubs so code compiles even if it can't draw text (with no font)
    byte Columns { get { return 0; }}
    byte Rows    { get { return 0; }}
    byte CursorX { get { return 0; } set { } }
    byte CursorY { get { return 0; } set { } }
    uint ForeColour { get { return Colour.MatrixGreen; } set { } }
    uint BackColour { get { return Colour.Black; } set { } }
#endif

#if defined(DISPLAY_DRIVER) && defined(FONT_EXISTS)
    byte[] fontData;
    byte[] FontData    { get { return fontData; } set { fontData = value; } }
    bool   FontDataSet { get { return fontData.Count != 0; } }
    
    renderMonoCharacter(char chr, uint foreColour, uint backColour)
    {
#ifdef DISPLAY_IS_RGB565
    #ifdef HAS_DISPLAY_READ
        uint pixelb = (backColour == Colour.Invert) ? backColour : DisplayDriver.convertToRGB565(backColour);
        uint pixelf = (foreColour == Colour.Invert) ? foreColour : DisplayDriver.convertToRGB565(foreColour);
    #else
        uint pixelb = DisplayDriver.convertToRGB565(backColour);
        uint pixelf = DisplayDriver.convertToRGB565(foreColour);
    #endif
#else
        uint pixelb = backColour;
        uint pixelf = foreColour;
#endif        
        
        if ((chr <= char(32)) || (chr > char(127)))
        {
            // ' '
            uint cellSize = cellWidth*cellHeight;
            for (uint i = 0; i < cellSize; i++)
            {
                cellBuffer[i] = pixelb;
            }
            return;
        }
        uint charStart = (Font.CellWidth * (byte(chr)-32));
        
        // This presumes fonts <= 8 pixels high (bytes per character == CellWidth)
        uint bi;
        byte yMask;
        uint addr;
        for (byte y = 0; y < Font.CellHeight; y++)
        {
            yMask = (1 << y);
            addr = charStart;
            for (byte x = 0; x < Font.CellWidth; x++)
            {
                cellBuffer[bi] = (((fontData[addr] & yMask) != 0) ? pixelf : pixelb); bi++;
                addr++;
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
        CursorY--;
#ifdef DISPLAY_DRIVER
        Display.Resume();
#endif
    }
    
    Clear() 
    {
#ifdef DISPLAY_DRIVER
        Display.Clear(BackColour); 
#endif
        CursorX = 0;
        CursorY = 0;
    }
    
    SetCursor(byte col, byte row)
    {
        CursorX = col;
        CursorY = row;
    }
    
#ifdef BUFFER_TEXT
    drawBufferChar(byte col, byte row, char c, uint foreColour, uint backColour)
    {
#if defined(DISPLAY_DRIVER) && defined(FONT_EXISTS)
        int x0 = int(col * cellWidth);
        int y0 = int(row * cellHeight);
        renderMonoCharacter(c, foreColour, backColour);
        
        int index = 0;
        int y1 = y0;
        int x1;
        for (int y=0; y < cellHeight; y++)
        {
            x1 = x0;
            for (int x=0; x < cellWidth; x++)
            {
                uint pixelColour = cellBuffer[index];
                if (pixelColour != backColour)
                {
#ifdef DISPLAY_IS_RGB565
                    DisplayDriver.setTextPixel(x+x0, y1, pixelColour);
#else
                    Display.SetPixel(x+x0, y1, pixelColour);
#endif
                }
                index++;
                x1++;
            }
            y1++;
        }
#endif
    }
#endif    

    DrawChar(byte col, byte row, char c, uint foreColour, uint backColour)
    {
#if defined(DISPLAY_DRIVER) && defined(FONT_EXISTS)
        int x0 = int(col * cellWidth);
        int y0 = int(row * cellHeight);
        if (fontData.Count != 0)
        {
            Display.Suspend();
#ifdef HAS_FAST_FILLEDRECTANGLE
            if ((x0 >= 0) && (y0 >= 0) && (x0 + cellWidth <= Display.PixelWidth) && (y0 + cellHeight <= Display.PixelHeight))
            {
                if ((c <= char(32)) || (c > char(127)))
                {
                    DisplayDriver.filledRectangle(x0, y0, cellWidth, cellHeight, backColour);
                }
                else
                {
                    renderMonoCharacter(c, foreColour, backColour);
                    DisplayDriver.filledRectangle(x0, y0, cellWidth, cellHeight, cellBuffer);
                }
                Display.Resume();
                return;
            }          
#endif      
            
            if ((c <= char(32)) || (c > char(127)))
            {
                Display.FilledRectangle(x0, y0, cellWidth, cellHeight, backColour);
            }
            else
            {
                renderMonoCharacter(c, foreColour, backColour);
                int index = 0;
                int y1 = y0;
                int x1;
                
                if ((x0 >= 0) && (y0 >= 0) && (x0 + cellWidth <= Display.PixelWidth) && (y0 + cellHeight <= Display.PixelHeight))
                {
                    for (int y=0; y < cellHeight; y++)
                    {
                        x1 = x0;
                        for (int x=0; x < cellWidth; x++)
                        {
#if defined(DISPLAY_IS_RGB565) || defined(DISPLAY_IS_MONO)
                            DisplayDriver.setClippedTextPixel(x1, y1, cellBuffer[index]);
#endif
#if !defined(DISPLAY_IS_RGB565) && !defined(DISPLAY_IS_MONO)
                            DisplayDriver.setPixel(x1, y1, cellBuffer[index]);
#endif    
                            index++;
                            x1++;
                        }
                        y1++;
                    }
                }
                else
                {
                    for (int y=0; y < cellHeight; y++)
                    {
                        x1 = x0;
                        for (int x=0; x < cellWidth; x++)
                        {
#if defined(DISPLAY_IS_RGB565)
                            DisplayDriver.setTextPixel(x1, y1, cellBuffer[index]);
#endif
#if !defined(DISPLAY_IS_RGB565)
                            Display.SetPixel(x1, y1, cellBuffer[index]);
#endif    
                            index++;
                            x1++;
                        }
                        y1++;
                    }
                }
            }
            Display.Resume();
        }
#endif
#ifdef BUFFER_TEXT
        DisplayDriver.bufferText(col, row, c, foreColour, backColour);
#endif
    }
    DrawChar(byte col, byte row, char c, uint foreColour, uint backColour, byte scale, int dx, int dy)
    {
#if defined(DISPLAY_DRIVER) && defined(FONT_EXISTS)
        int x0 = int(col * cellWidth * scale) + dx;
        int y0 = int(row * cellHeight) + dy;
        if (fontData.Count != 0)
        {
            Display.Suspend();
#ifdef HAS_FAST_FILLEDRECTANGLE                
            if ((scale >= 2) && (x0 >= 0) && (y0 >= 0) && (x0 + cellWidth*scale < Display.PixelWidth) && (y0 + cellHeight*scale < Display.PixelHeight))
            {
                if ((c <= char(32)) || (c > char(127)))
                {
                    DisplayDriver.filledRectangle(x0, y0, cellWidth*scale, cellHeight*scale, backColour);
                }
                else
                {
                    renderMonoCharacter(c, foreColour, backColour);
                    int x1;
                    int y1 = y0;
                    int index;
                    uint colour;
                    for (int y=0; y < cellHeight; y++)
                    {
                        x1 = x0;
                        for (int x=0; x < cellWidth; x++)
                        {                     
                            DisplayDriver.filledRectangle(x1, y1, scale, scale, cellBuffer[index]);
                            x1 += scale;
                            index++;
                        }
                        y1 += scale;
                    }
                }
                Display.Resume();
                return;
            }
#endif
            if ((c <= char(32)) || (c > char(127)))
            {
                Display.FilledRectangle(x0, y0, cellWidth*scale, cellHeight*scale, backColour);
            }
            else
            {
                renderMonoCharacter(c, foreColour, backColour);
                int x1;
                int y1 = y0;
                int index;
                uint colour;
                for (int y=0; y < cellHeight; y++)
                {
                    x1 = x0;
                    for (int x=0; x < cellWidth; x++)
                    {                     
    #ifdef DISPLAY_IS_RGB565
                        Display.FilledRectangle(x1, y1, scale, scale, DisplayDriver.convertFromRGB565(cellBuffer[index]));
    #else
                        Display.FilledRectangle(x1, y1, scale, scale, cellBuffer[index]);
    #endif
                        x1 += scale;
                        index++;
                    }
                    y1 += scale;
                }
            }
            Display.Resume();
        }
#endif
    }
    
    
    Print(char c,     uint foreColour, uint backColour)
    {
#if defined(DISPLAY_DRIVER) && defined(FONT_EXISTS)
        if (1 == textScale)
        {
            DrawChar(cursorX, cursorY, c, foreColour, backColour);
        }
        else
        {
            DrawChar(cursorX, cursorY, c, foreColour, backColour, textScale, 0, 0);
        }
#endif
        CursorX++;
        if (CursorX >= Columns)
        {
            CursorX = 0;
            CursorY += textScale;
            while (CursorY >= Rows)
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
        CursorX = 0;
        CursorY += textScale;
        while (CursorY >= Rows)
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
        Print(c, ForeColour, BackColour);
    }
    Print(string s)
    {
        Print(s, ForeColour, BackColour);
    }
    PrintLn(char c)
    {  
        PrintLn(c, ForeColour, BackColour);
    }
    PrintLn(string s)
    {  
        PrintLn(s, ForeColour, BackColour);
    }
}
