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
        if (DisplayDriver.begin())
        {
            DisplayDriver.visible = true;
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
            DisplayDriver.update(); // defined in current display driver
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
        Suspend();
        DisplayDriver.clear(colour);
        Resume();
    }
    bool Visible { set { DisplayDriver.visible = value; } }
    
    SetPixel(int x, int y, uint colour)
    {
        if ((x < 0) || (y < 0) || (x >= Display.PixelWidth) || (y >= Display.PixelHeight)) { return; }
        Suspend();
        DisplayDriver.setPixel(x, y, colour);
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
        int x2 = x+int(w)-1;
        int ih = int(h);
        if ((w == 1) && (h == 1))
        {
            SetPixel(x, y, colour);
        }
        else
        {
            Suspend();
            for (int i=y; i < y+ih; i++)
            {
                HorizontalLine(x, i, x2, colour);
            }
            Resume();
        }
    }
    Triangle(int x0, int y0, int x1, int y1, int x2, int y2, uint colour)
    {
        Suspend();
        Line(x0,y0,x1,y1, colour);
        Line(x1,y1,x2,y2, colour);
        Line(x2,y2,x0,y0, colour);
        Resume();
    }
    
    FilledTriangle(int x0, int y0, int x1, int y1, int x2, int y2, uint colour)
    {
        int a;
        int b;
        int y;
        int last;
        
        // y2 >= y1 >= y0:
        if (y0 > y1) 
        {
            Int.Swap(ref y0, ref y1);
            Int.Swap(ref x0, ref x1);
        }
        if (y1 > y2) 
        {
            Int.Swap(ref y2, ref y1);
            Int.Swap(ref x2, ref x1);
        }
        if (y0 > y1) 
        {
            Int.Swap(ref y0, ref y1);
            Int.Swap(ref x0, ref x1);
        }
        
        if (y0 == y2) 
        {
            a = x0;b = x0;
            if (x1 < a)
            {
                a = x1;
            }
            else if (x1 > b)
            {
                b = x1;
            }
            if (x2 < a)
            {
                a = x2;
            }
            else if (x2 > b)
            {
                b = x2;
            }
            HorizontalLine(a, y0, b, colour);
            return;
        }
        
        int dx01 = x1 - x0;
        int dy01 = y1 - y0;
        int dx02 = x2 - x0;
        int dy02 = y2 - y0;
        int dx12 = x2 - x1;
        int dy12 = y2 - y1;
        long sa = 0;
        long sb = 0;
        
        if (y1 == y2)
        {
            last = y1;
        }
        else
        {
            last = y1 - 1;
        }
        Suspend();
        for (y = y0; y <= last; y++) 
        {
            a = x0 + int(sa / dy01);
            b = x0 + int(sb / dy02);
            sa += dx01;
            sb += dx02;
            HorizontalLine(a, y, b, colour);
        }
        sa = long(dx12) * (y - y1);
        sb = long(dx02) * (y - y0);
        for (; y <= y2; y++)
        {
            a = x1 + int(sa / dy12);
            b = x0 + int(sb / dy02);
            sa += dx12;
            sb += dx02;
            HorizontalLine(a, y, b, colour);
        }
        Resume();
    }
    
    filledCircleHelper(int x0, int y0, int r, byte corners, int delta, uint colour)
    {
        int f = 1 - r;
        int ddFx = 1;
        int ddFy = -2 * r;
        int x = 0;
        int y = r;
        int px = x;
        int py = y;
        int ys;
        
        while (x < y) 
        {
            if (f >= 0) 
            {
                y--;
                ddFy += 2;
                f += ddFy;
            }
            x++;
            ddFx += 2;
            f += ddFx;
            if (x < (y + 1))
            {
                ys = y0-y;
                if (corners & 0b01 != 0)
                {
                    VerticalLine(x0 + x, ys, ys + 2 * y + delta, colour);
                }
                if (corners & 0b10 != 0)
                {
                    VerticalLine(x0 - x, ys, ys + 2 * y + delta, colour);
                }
            }
            if (y != py) 
            {
                ys = y0-px;
                if (corners & 0b01 != 0)
                {
                    VerticalLine(x0 + py, ys, ys + 2 * px + delta, colour);
                }
                if (corners & 0b10 != 0)
                {
                    VerticalLine(x0 - py, ys, ys + 2 * px + delta, colour);
                }
                py = y;
            }
            px = x;
        }
    }  
    
    circleHelper(int x0, int y0, int r, byte cornername, uint colour)
    {
        int f = 1 - r;
        int ddFx = 1;
        int ddFy = -2 * r;
        int x = 0;
        int y = r;

        while (x < y)
        {
            if (f >= 0)
            {
                y--;
                ddFy += 2;
                f += ddFy;
            }
            x++;
            ddFx += 2;
            f += ddFx;
            if (cornername & 0b0100 != 0)
            {
                SetPixel(x0 + x, y0 + y, colour);
                SetPixel(x0 + y, y0 + x, colour);
            }
            if (cornername & 0b0010 != 0)
            {
                SetPixel(x0 + x, y0 - y, colour);
                SetPixel(x0 + y, y0 - x, colour);
            }
            if (cornername & 0b1000 != 0)
            {
                SetPixel(x0 - y, y0 + x, colour);
                SetPixel(x0 - x, y0 + y, colour);
            }
            if (cornername & 0b0001 != 0) 
            {
                SetPixel(x0 - y, y0 - x, colour);
                SetPixel(x0 - x, y0 - y, colour);
            }
        }
    }
    
    FilledCircle(int x0, int y0, int r, uint colour)
    {
        Suspend();
        VerticalLine(x0, y0 - r, y0 + r, colour);
        filledCircleHelper(x0, y0, r, 0b11, 0, colour);
        Resume();
    }
    
    Circle(int x0, int y0, int r, uint colour)
    {
        int f = 1 - r;
        int ddFx = 1;
        int ddFy = -2 * r;
        int x = 0;
        int y = r;     
        
        Suspend();
        if ((x0-r >= 0) && (y0-r >= 0) && (x0+r < PixelWidth) && (y0+r < PixelHeight))
        {
            colour = DisplayDriver.convertToRGB565(colour);
            DisplayDriver.setClippedTextPixel(x0, y0 + r, colour);
            DisplayDriver.setClippedTextPixel(x0, y0 - r, colour);
            DisplayDriver.setClippedTextPixel(x0 + r, y0, colour);
            DisplayDriver.setClippedTextPixel(x0 - r, y0, colour);
            while (x < y) 
            {
                if (f >= 0) 
                {
                    y--;
                    ddFy += 2;
                    f += ddFy;
                }
                x++;
                ddFx += 2;
                f += ddFx;
                
                DisplayDriver.setClippedTextPixel(x0 + x, y0 + y, colour);
                DisplayDriver.setClippedTextPixel(x0 - x, y0 + y, colour);
                DisplayDriver.setClippedTextPixel(x0 + x, y0 - y, colour);
                DisplayDriver.setClippedTextPixel(x0 - x, y0 - y, colour);
                DisplayDriver.setClippedTextPixel(x0 + y, y0 + x, colour);
                DisplayDriver.setClippedTextPixel(x0 - y, y0 + x, colour);
                DisplayDriver.setClippedTextPixel(x0 + y, y0 - x, colour);
                DisplayDriver.setClippedTextPixel(x0 - y, y0 - x, colour);
            }
        }
        else
        {
            SetPixel(x0, y0 + r, colour);
            SetPixel(x0, y0 - r, colour);
            SetPixel(x0 + r, y0, colour);
            SetPixel(x0 - r, y0, colour);
            while (x < y) 
            {
                if (f >= 0) 
                {
                    y--;
                    ddFy += 2;
                    f += ddFy;
                }
                x++;
                ddFx += 2;
                f += ddFx;
                
                SetPixel(x0 + x, y0 + y, colour);
                SetPixel(x0 - x, y0 + y, colour);
                SetPixel(x0 + x, y0 - y, colour);
                SetPixel(x0 - x, y0 - y, colour);
                SetPixel(x0 + y, y0 + x, colour);
                SetPixel(x0 - y, y0 + x, colour);
                SetPixel(x0 + y, y0 - x, colour);
                SetPixel(x0 - y, y0 - x, colour);
            }
        }
        Resume();
    }
    
    RoundedRectangle(int x, int y, int w, int h, int r, uint colour)
    {
        int maxRadius = ((w < h) ? w : h) / 2;
        if (r > maxRadius)
        {
            r = maxRadius;
        }
        
        Suspend();
        HorizontalLine(x + r, y, x + r + w - 2 * r - 1, colour);
        HorizontalLine(x + r, y + h - 1, x + r + w - 2 * r - 1, colour);
        VerticalLine(x, y + r, y + r + h - 2 * r - 1, colour);
        VerticalLine(x + w - 1, y + r, y + r + h - 2 * r - 1, colour);
        
        circleHelper(x + r, y + r, r, 0b0001, colour);
        circleHelper(x + w - r - 1, y + r, r, 0b0010, colour);
        circleHelper(x + w - r - 1, y + h - r - 1, r, 0b0100, colour);
        circleHelper(x + r, y + h - r - 1, r, 0b1000, colour);
        Resume();
    }
    FilledRoundedRectangle(int x, int y, int w, int h, int r, uint colour)
    {
        int maxRadius = ((w < h) ? w : h) / 2;
        if (r > maxRadius)
        {
            r = maxRadius;
        }
        Suspend();
        FilledRectangle(x + r, y, w - 2 * r, h, colour);
        // draw four corners
        filledCircleHelper(x + w - r - 1, y + r, r, 0b01, h - 2 * r - 1, colour);
        filledCircleHelper(x + r, y + r, r, 0b10, h - 2 * r - 1, colour);
        Resume();
    }
    
    HorizontalLine(int x1, int y, int x2, uint colour)
    {
        if (x1 > x2)
        {
            Int.Swap(ref x1, ref x2);
        }
        // clip here so we can use DisplayDriver.horizontalLine
        if (x2 < 0) { return; }
        if (y < 0) { return; }
        int ymax = Display.PixelHeight-1;
        if (y > ymax) { return; }
        
        int xmax = Display.PixelWidth-1;
        if (x1 > xmax) { return; }
        
        if (x1 < 0)     { x1 = 0; }
        if (x2 >= xmax) { x2 = xmax; }
        Suspend();
        DisplayDriver.horizontalLine(x1, y, x2, colour);
        Resume();
    }
    VerticalLine(int x, int y1, int y2, uint colour)
    {
        if (y1 > y2)
        {
            Int.Swap(ref y1, ref y2);
        }
        // clip here so we can use DisplayDriver.verticalLine
        if (y2 < 0) { return; }
        if (x < 0) { return; }
        int ymax = Display.PixelHeight-1;
        if (y1 > ymax) { return; }
        
        int xmax = Display.PixelWidth-1;
        if (x > xmax) { return; }
        
        if (y1 < 0)     { y1 = 0; }
        if (y2 >= ymax) { y2 = ymax; }
        Suspend();
        DisplayDriver.verticalLine(x, y1, y2, colour);
        Resume();
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
        
        // since x0 <= x2:
        if ((x0 >= 0) && (y0 >= 0) &&                      (y0 < PixelHeight) &&
                         (y1 >= 0) && (x1 < PixelWidth) && (y1 < PixelHeight))
        {
            for (int x = x0; x <= x1; x++)
            {    
                DisplayDriver.setPixel(x, y, colour);
                if (d > 0)
                {
                    y += yi;
                    d += dydx2;
                }
                else
                {
                    d += dy2;
                }
            }
        }
        else
        {
            for (int x = x0; x <= x1; x++)
            {    
                SetPixel(x, y, colour);
                if (d > 0)
                {
                    y += yi;
                    d += dydx2;
                }
                else
                {
                    d += dy2;
                }
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
        
        // since y0 <= y1:
        if ((x0 >= 0) && (y0 >= 0) && (x0 < PixelWidth) &&
            (x1 >= 0) &&              (x1 < PixelWidth) && (y1 < PixelHeight))
        {
            for (int y = y0; y <= y1; y++)
            {
                DisplayDriver.setPixel(x, y, colour);
                if (d > 0)
                {
                    x += xi;
                    d += dxdy2;
                }
                else
                {
                    d += dx2;
                }
            }
        }
        else
        {
            for (int y = y0; y <= y1; y++)
            {
                SetPixel(x, y, colour);
                if (d > 0)
                {
                    x += xi;
                    d += dxdy2;
                }
                else
                {
                    d += dx2;
                }
            }
        }
    }

    // alternate Bresenham : https://www.instructables.com/Getting-Started-With-OLED-Displays/
    Line(int x0, int y0, int x1, int y1, uint colour)
    {
        // 17021ms
        if      (x0 == x1) { VerticalLine(x0, y0, y1, colour);   }
        else if (y0 == y1) { HorizontalLine(x0, y0, x1, colour); }
        else
        {
            Suspend();
            if (Int.Abs(y1-y0) < Int.Abs(x1-x0))
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
    
    adafruitLine(int x0, int y0, int x1, int y1, uint colour)
    {     
        // 19266ms   
        if      (x0 == x1) { VerticalLine(x0, y0, y1, colour);   }
        else if (y0 == y1) { HorizontalLine(x0, y0, x1, colour); }
        else
        {
            Suspend();
            bool steep = Int.Abs(y1 - y0) > Int.Abs(x1 - x0);
            if (steep) 
            {
                Int.Swap(ref x0, ref y0);
                Int.Swap(ref x1, ref y1);
            }
            if (x0 > x1) 
            {
                Int.Swap(ref x0, ref x1);
                Int.Swap(ref y0, ref y1);
            }
            
            int dx = x1 - x0;
            int dy = Int.Abs(y1 - y0);
            
            int err = dx / 2;
            int ystep;
            
            if (y0 < y1) 
            {
                ystep = 1;
            } 
            else
            {
                ystep = -1;
            }
        
            // since x0 <= x1
            if ((x0 >= 0) && (y0 >= 0) &&                      (y0 < PixelHeight)
                          && (y1 >= 0) && (x1 < PixelWidth) && (y1 < PixelHeight))
            {
                if (steep)
                {
                    for (; x0 <= x1; x0++) 
                    {
                        DisplayDriver.setPixel(y0, x0, colour);
                        err -= dy;
                        if (err < 0) 
                        {
                            y0 += ystep;
                            err += dx;
                        }
                    }
                }
                else
                {
                    for (; x0 <= x1; x0++) 
                    {
                        DisplayDriver.setPixel(x0, y0, colour);
                        err -= dy;
                        if (err < 0) 
                        {
                            y0 += ystep;
                            err += dx;
                        }
                    }
                }
            }
            else
            {
                for (; x0 <= x1; x0++) 
                {
                    if (steep) 
                    {
                        SetPixel(y0, x0, colour);
                    }
                    else
                    {
                        SetPixel(x0, y0, colour);
                    }
                    err -= dy;
                    if (err < 0) 
                    {
                        y0 += ystep;
                        err += dx;
                    }
                }
            }
            Resume();
        }
    }
    
    
    ScrollUp(uint lines)
    {
        Suspend();
        DisplayDriver.scrollUp(lines);
        Resume();
    }
    
}
