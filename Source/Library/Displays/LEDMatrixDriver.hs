unit DisplayDriver
{
    #define DISPLAY_DRIVER
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Display"
    
    // hardcoded till I get a bigger matrix ..
    const byte pixelWidth  = 8;
    const byte pixelHeight = 8;
    
    const byte DECODE_MODE       = 0x09;
    const byte BRIGHTNESS        = 0x0A;
    const byte SCAN_LIMIT        = 0x0B;
    const byte SHUT_DOWN         = 0x0C;
    const byte DISPLAY_TEST      = 0x0F;
    
    byte pinsSet;
    byte csPin;
    byte dataPin;
    byte clockPin;
    byte DataPin  { get { return dataPin; }   set { dataPin = value; pinsSet = pinsSet | 1; } }
    byte CSPin    { get { return csPin; }     set { csPin = value; pinsSet = pinsSet | 2;} }
    byte ClockPin { get { return clockPin; }  set { clockPin = value; pinsSet = pinsSet | 4;} }
    
    const uint bufferSize = pixelWidth*pixelHeight/8;
    byte[bufferSize] pixelBuffer;
    
    write(byte value)
    {
        for (byte mask = 0x80; mask > 0; mask = (mask >> 1))
        {
            DigitalWrite(ClockPin, false);
            DigitalWrite(DataPin, ((value & mask) != 0) ? true : false);
            DigitalWrite(ClockPin, true);
        }
    }
    
    bool Begin()
    {
        bool success;
        loop
        {
            if (pinsSet != 7)
            {
                break;
            }
            Display.PixelWidth  = pixelWidth;
            Display.PixelHeight = pixelHeight;
            
            // one time setup
            PinMode(DataPin,  PinModeOption.Output);
            PinMode(CSPin,    PinModeOption.Output);
            PinMode(ClockPin, PinModeOption.Output);
            DigitalWrite(DataPin,  true);
            DigitalWrite(CSPin,    true);
            DigitalWrite(ClockPin, true);
            
            // begin
            DigitalWrite(CSPin, false);
            write(SCAN_LIMIT);
            write(7); //  bits 0..7
            DigitalWrite(CSPin, true);
            
            DigitalWrite(CSPin, false);
            write(DECODE_MODE);
            write(0); //  No decode for digits 7..0
            DigitalWrite(CSPin, true);
            
            Visible = true;
            
            test = true; // 0
            
            Brightness = 3;
            success = true;
            break;
        }
        return success;
    }
    
    bool Visible 
    {
        set
        {
            DigitalWrite(CSPin, false);
            write(SHUT_DOWN);
            write(value ? 1 : 0); //  display on// off
            DigitalWrite(CSPin, true);
        }
    }
    bool test 
    {
        set
        {
            DigitalWrite(CSPin, false);
            write(DISPLAY_TEST);
            write(value ? 0 : 1); // 0 = normal mode
            DigitalWrite(CSPin, true);
        }
    }
    byte Brightness
    {
        set
        {
            if (value > 15) { value = 15; }
            DigitalWrite(CSPin, false);
            write(BRIGHTNESS);
            write(value);
            DigitalWrite(CSPin, true);
        }
    }
    UpdateDisplay()
    {
        for (byte row=0; row < 8; row++)
        {
            DigitalWrite(CSPin, false);
            write(row+1);
            write(pixelBuffer[row]);
            DigitalWrite(CSPin, true); 
        }
    }
    ClearDisplay(uint colour)
    {
        for (byte row = 0; row < 8; row++)
        {
            for (byte column = 0; column < 8; column++)
            {
                SetPixel(column, row, colour);
            }
        }
    }
    RawSetPixel(int column, int row, uint colour)
    {
        if (colour == 0xF000) // Colour.Invert
        {
            pixelBuffer[row] = pixelBuffer[row] ^ (0x80 >> column);
        }
        else if (colour == 0x0000) // Colour.Black
        {
            pixelBuffer[row] = pixelBuffer[row] & ~(0x80 >> column);
        }
        else
        {
            pixelBuffer[row] = pixelBuffer[row] | (0x80 >> column);
        }
    }
    ScrollUpDisplay(uint lines)
    {
        uint drow;
        for (uint row = lines; row < pixelHeight; row++)
        {
            pixelBuffer[drow] = pixelBuffer[row];
            drow++;    
        }
        loop
        {
            if (drow >= pixelHeight) { break; }
            pixelBuffer[drow] = 0;
            drow++;   
        }
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
