unit DisplayDriver
{
    #define DISPLAY_DRIVER
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Display"  
    
    friend Display, Screen;
      
    const byte pixelWidth  = DeviceDriver.pw;
    const byte pixelHeight = DeviceDriver.ph;
    
    const uint bufferSize = pixelWidth*pixelHeight/8;
    byte[bufferSize] pixelBuffer;
    
    bool begin()
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
    bool visible
    {
        set
        {
            // TODO
        }
    }
    update()
    {
        DeviceDriver.update();
    }
    clear(uint colour)
    {
        for (byte row = 0; row < pixelHeight; row++)
        {
            for (byte column = 0; column < pixelWidth; column++)
            {
                DeviceDriver.setPixel(column, row, colour);
            }
        }
    }
    
    setPixel(int column, int row, uint colour)
    {
        DeviceDriver.setPixel(column, row, colour);
    }
    scrollUp(uint lines)
    {
        // TODO
    }
    horizontalLine(int x1, int y, int x2, uint colour)
    {
        for (int x = x1; x <= x2; x++)
        {
            setPixel(x,y, colour);
        }
    }
    verticalLine(int x, int y1, int y2, uint colour)
    {
        for (int y = y1; y <= y2; y++)
        {
            setPixel(x,y, colour);
        }
    }
}
