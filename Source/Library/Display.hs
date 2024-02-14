unit Display
{
#if !defined(DISPLAY_DRIVER)
    #error "To use 'Display' unit, use a display driver from '/Source/Library/Displays'"
#endif
    uses "/Source/Library/Screen"
        
    int pixelWidth; 
    int pixelHeight;

    int  PixelWidth   { get { return pixelWidth;  } set { pixelWidth  = value;  }}
    int  PixelHeight  { get { return pixelHeight; } set { pixelHeight = value;  }}
    
    uint ForeColour { get { return Screen.ForeColour; } set { Screen.ForeColour = value; }}
    uint BackColour { get { return Screen.BackColour; } set { Screen.BackColour = value; }}
    
    
    bool Begin()
    {
#ifdef FONT_EXISTS
        Screen.FontData = Font.FontData;
#endif        
        if (DisplayDriver.Begin())
        {
            DisplayDriver.Visible = true;
            return true;
        }
        return false;
    }
#ifdef NO_SUSPEND_RESUME
    Reset()   {}
    Resume()  {}
    Suspend() {}
#else    
    int suspended;
    Reset()
    {
        suspended = 0;
    }
    Resume()
    {
        if (suspended > 0)
        {
            suspended--;
        }
        if (suspended == 0)
        {
            DisplayDriver.UpdateDisplay(); // defined in current display driver
        }
    }
    Suspend()
    {
        suspended++;
    }
#endif
    Clear()
    {
        Clear(Screen.BackColour);
    }
    Clear(uint colour)
    {
#ifdef DISPLAY_DIAGNOSTICS
        IO.Write("<Display.Clear");
#endif        
        Suspend();
        DisplayDriver.ClearDisplay(colour);
        Resume();
#ifdef DISPLAY_DIAGNOSTICS
        IO.WriteLn(">");
#endif                        
    }
    bool Visible { set { DisplayDriver.Visible = value; } }
    
    SetPixel(int x, int y, uint colour)
    {
        if ((x < 0) || (y < 0) || (x >= Display.PixelWidth) || (y >= Display.PixelHeight)) { return; }
        Suspend();
        DisplayDriver.RawSetPixel(x, y, colour);
        Resume();
    }
    HorizontalLine(int x1, int y, int x2, uint colour)
    {
        if (x1 > x2)
        {
            int t = x1;
            x1 = x2;
            x2 = t;
        }
        // clip here so we can use RawSetPixel
        if (x2 < 0) { return; }
        if (y < 0) { return; }
        int ymax = Display.PixelHeight-1;
        if (y > ymax) { return; }
        
        int xmax = Display.PixelWidth-1;
        if (x1 > xmax) { return; }
        
        if (x1 < 0) { x1 = 0; }
        if (x2 >= xmax) { x2 = xmax; }
        Suspend();
        DisplayDriver.RawHorizontalLine(x1, y, x2, colour);
        Resume();
    }
    VerticalLine(int x, int y1, int y2, uint colour)
    {
        if (y1 > y2)
        {
            int t = y1;
            y1 = y2;
            y2 = t;
        }
        // clip here so we can use RawSetPixel
        if (y2 < 0) { return; }
        if (x < 0) { return; }
        int ymax = Display.PixelHeight-1;
        if (y1 > ymax) { return; }
        
        int xmax = Display.PixelWidth-1;
        if (x > xmax) { return; }
        
        if (y1 < 0) { y1 = 0; }
        if (y2 >= ymax) { y2 = ymax; }
        Suspend();
        DisplayDriver.RawVerticalLine(x, y1, y2, colour);
        Resume();
    }
    
    Rectangle(int x, int y, int w, int h, uint colour)
    {
        int iw = int(w);
        int ih = int(h);
        Suspend();
        HorizontalLine(x,y,x+iw-1, colour);
        HorizontalLine(x,y+ih-1,x+iw-1, colour);
        VerticalLine(x,y,y+ih-1,colour);
        VerticalLine(x+iw-1,y,y+ih-1,colour);
        Resume();
    }
    FilledRectangle(int x, int y, int w, int h, uint colour)
    {
#ifdef DISPLAY_DIAGNOSTICS
        IO.Write("<Display.FilledRectangle");
#endif          
        int x2 = x+int(w)-1;
        int ih = int(h);
        Suspend();
        for (int i=y; i < y+ih; i++)
        {
            HorizontalLine(x, i, x2, colour);
        }
        Resume();
#ifdef DISPLAY_DIAGNOSTICS
        IO.WriteLn(">");
#endif                        
    }
    
    lineLow(int x0, int y0, int x1, int y1, uint colour)
    {
        int dx = x1 - x0;
        int dy = y1 - y0;
        int yi = 1;
        if (dy < 0)
        {
            yi = -1;
            dy = -dy;
        }
        int dydx2 = (2 * (dy - dx));
        int dy2 = 2*dy;
        int d = (2 * dy) - dx;
        int y = y0;
        for (int x = x0; x <= x1; x++)
        {    
            SetPixel(x, y, colour);
            if (d > 0)
            {
                y = y + yi;
                d = d + dydx2;
            }
            else
            {
                d = d + dy2;
            }
        }
    }

    lineHigh(int x0, int y0, int x1, int y1, uint colour)
    {
        int dx = x1 - x0;
        int dy = y1 - y0;
        int xi = 1;
        if (dx < 0)
        {
            xi = -1;
            dx = -dx;
        }
        int dxdy2 = (2 * (dx - dy));
        int dx2 = 2*dx;
        int d = (2 * dx) - dy;
        int x = x0;
        for (int y = y0; y <= y1; y++)
        {
            SetPixel(x, y, colour);
            if (d > 0)
            {
                x = x + xi;
                d = d + dxdy2;
            }
            else
            {
                d = d + dx2;
            }
        }
    }

    // alternate Bresenham : https://www.instructables.com/Getting-Started-With-OLED-Displays/
    Line(int x0, int y0, int x1, int y1, uint colour)
    {
        Suspend();
        if (x0 == x1)      { VerticalLine(x0, y0, y1, colour);   }
        else if (y0 == y1) { HorizontalLine(x0, y0, x1, colour); }
        
        if (Int.Abs(y1-y0) < Int.Abs(x1-x0))
        //if (((y1-y0 < 0) ? -(y1-y0) : y1-y0) < ((x1-x0 < 0) ? -(x1-x0) : x1-x0))
        {
            if (x0 > x1)
            {
                lineLow(x1, y1, x0, y0, colour);
            }
            else
            {
                lineLow(x0, y0, x1, y1, colour);
            }
        }
        else
        {
            if (y0 > y1)
            {
                lineHigh(x1, y1, x0, y0, colour);
            }
            else
            {
                lineHigh(x0, y0, x1, y1, colour);
            }
        }
        Resume();
    }
    
    
    ScrollUp(uint lines)
    {
        Suspend();
        DisplayDriver.ScrollUpDisplay(lines);
        Resume();
    }
    
}
