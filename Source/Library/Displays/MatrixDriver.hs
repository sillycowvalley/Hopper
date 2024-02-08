unit DisplayDriver
{
    #define DISPLAY_DRIVER
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Display"
    
    const byte pixelWidth  = DeviceDriver.PW;
    const byte pixelHeight = DeviceDriver.PH;
    
    const uint bufferSize = pixelWidth*pixelHeight/8;
    byte[bufferSize] pixelBuffer;
    
    bool Begin()
    {
        bool success;
        loop
        {
            Display.PixelWidth  = pixelWidth;
            Display.PixelHeight = pixelHeight;
            
            success = true;
            break;
        }
        return success;
    }
    bool Visible
    {
        set
        {
            // TODO
        }
    }
    UpdateDisplay()
    {
        DeviceDriver.UpdateDisplay();
    }
    ClearDisplay(uint colour)
    {
        for (byte row = 0; row < pixelHeight; row++)
        {
            for (byte column = 0; column < pixelWidth; column++)
            {
                DeviceDriver.SetPixel(column, row, colour);
            }
        }
    }
    
    RawSetPixel(int column, int row, uint colour)
    {
        DeviceDriver.SetPixel(column, row, colour);
    }
    ScrollUpDisplay(uint lines)
    {
        // TODO
    }
    RawHorizontalLine(int x1, int y, int x2, uint colour)
    {
        for (int x = x1; x <= x2; x++)
        {
            RawSetPixel(x,y, colour);
        }
    }
    RawVerticalLine(int x, int y1, int y2, uint colour)
    {
        for (int y = y1; y <= y2; y++)
        {
            RawSetPixel(x,y, colour);
        }
    }
}
