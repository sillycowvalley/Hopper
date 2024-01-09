unit Display
{
    uses "/Source/Library/Math.hs"
    
    byte i2cAddress;
    byte i2cController; // conveniently defaults to zero
    byte sdaPin;        // zero implies use the default depending on controller
    byte sclPin;        // zero implies use the default depending on controller
    byte I2CController { get { return i2cController; } set { i2cController = value; } }
    byte I2CAddress    { get { return i2cAddress; }    set { i2cAddress = value; } }
    byte I2CSDAPin     { get { return sdaPin; }        set { sdaPin = value; } }
    byte I2CSCLPin     { get { return sclPin; }        set { sclPin = value; } }
         
    bool Begin()
    {
        return DisplayDriver.Begin();
    }
    
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
            Update(); // defined in current display driver
        }
    }
    Suspend()
    {
        suspended++;
    }
    Clear()
    {
        Clear(Color.Black);
    }
    Clear(uint colour)
    {
#ifdef DISPLAYDIAGNOSTICS
        IO.Write("<Display.Clear");
#endif        
        Suspend();
        ClearBuffer(colour);
        Resume();
#ifdef DISPLAYDIAGNOSTICS
        IO.WriteLn(">");
#endif                        
    }
    
    Rectangle(int x, int y, int w, int h, uint colour)
    {
        Suspend();
        HorizontalLine(x,y,x+w-1,y, colour);
        HorizontalLine(x,y+h-1,x+w-1,y+h-1, colour);
        VerticalLine(x,y,x,y+h-1,colour);
        VerticalLine(x+w-1,y,x+w-1,y+h-1,colour);
        Resume();
    }
    FilledRectangle(int x, int y, int w, int h, uint colour)
    {
#ifdef DISPLAYDIAGNOSTICS
        IO.Write("<Display.FilledRectangle");
#endif                
        Suspend();
        for (int i=y; i < y+h; i++)
        {
            HorizontalLine(x, i, x+w-1, i, colour);
        }
        Resume();
#ifdef DISPLAYDIAGNOSTICS
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
        int d = (2 * dy) - dx;
        int y = y0;
        for (int x = x0; x <= x1; x++)
        {    
            SetPixel(x, y, colour);
            if (d > 0)
            {
                y = y + yi;
                d = d + (2 * (dy - dx));
            }
            else
            {
                d = d + 2*dy;
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
        int d = (2 * dx) - dy;
        int x = x0;
        for (int y = y0; y <= y1; y++)
        {
            SetPixel(x, y, colour);
            if (d > 0)
            {
                x = x + xi;
                d = d + (2 * (dx - dy));
            }
            else
            {
                d = d + 2*dx;
            }
        }
    }

    // alternate Bresenham : https://www.instructables.com/Getting-Started-With-OLED-Displays/
    Line(int x0, int y0, int x1, int y1, uint colour)
    {
        Suspend();
        if (x0 == x1)      { VerticalLine(x0, y0, x1, y1, colour);   }
        else if (y0 == y1) { HorizontalLine(x0, y0, x1, y1, colour); }
        else if (Math.Abs(y1-y0) < Math.Abs(x1-x0))
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
    
}
