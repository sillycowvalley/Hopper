unit DisplayDriver
{
    #define DISPLAY_DRIVER
    #define SSD1306_OLED_128x64
    #define DISPLAY_IS_MONO
    
#ifdef MINIMAL_RUNTIME
    uses "/Source/Minimal/MCU"
#else    
    uses "/Source/Library/MCU"
#endif    
    uses "/Source/Library/Display"
    
    friend Display, Screen;
    
    //   https://github.com/adafruit/Adafruit_SSD1306/blob/master/Adafruit_SSD1306.cpp
    
    const byte SSD1306_MEMORYMODE       = 0x20;
    const byte SSD1306_COLUMNADDR       = 0x21;
    const byte SSD1306_PAGEADDR         = 0x22;
    const byte SSD1306_DEACTIVATESCROLL = 0x2E;
    
    const byte SSD1306_SETSTARTLINE    = 0x40;
    
    const byte SSD1306_SETCONTRAST     = 0x81;
    const byte SSD1306_CHARGEPUMP      = 0x8D;

    const byte SSD1306_SEGREMAP        = 0xA0;
    const byte SSD1306_SETSEGMENTREMAP = 0xA1;
    const byte SSD1306_DISPLAYALLONRESUME = 0xA4;
    const byte SSD1306_DISPLAYALLON    = 0xA5;
    const byte SSD1306_NORMALDISPLAY   = 0xA6;
    const byte SSD1306_INVERTDISPLAY   = 0xA7;

    const byte SSD1306_SETMULTIPLEX    = 0xA8;
    const byte SSD1306_DISPLAYOFF      = 0xAE;
    const byte SSD1306_DISPLAYON       = 0xAF;
    
    const byte SSD1306_SETPAGEADDR     = 0xB0;
    
    const byte SSD1306_COMSCANINC      = 0xC0;
    const byte SSD1306_COMSCANDEC      = 0xC8;
        
    const byte SSD1306_SETDISPLAYOFFSET   = 0xD3;
    const byte SSD1306_SETDISPLAYCLOCKDIV = 0xD5;
    
    const byte SSD1306_SETPRECHARGE       = 0xD9;
    const byte SSD1306_SETCOMPINS         = 0xDA;
    const byte SSD1306_SETVCOMDETECT      = 0xDB;
    
    byte i2cAddress = 0x3C; // typically a good default
    bool i2cConfigured;
    
    const int pw  = 128;
    const int ph  = 64;
    
    byte i2cController = Wire.DefaultI2CController;
    byte sdaPin        = Wire.DefaultI2CSDAPin;
    byte sclPin        = Wire.DefaultI2CSCLPin;
    byte I2CController { get { return i2cController; } set { i2cController = value; } }
    byte I2CAddress    { get { return i2cAddress; }    set { i2cAddress = value; } }
    byte I2CSDAPin     { get { return sdaPin; }        set { sdaPin = value; } }
    byte I2CSCLPin     { get { return sclPin; }        set { sclPin = value; } }
    
    bool flipX;
    bool flipY;
    bool isPortrait;
    bool FlipX { get { return flipX; } set { flipX = value; }}
    bool FlipY { get { return flipY; } set { flipY = value; }}
    bool IsPortrait { get { return isPortrait; } set { isPortrait = value; } }
    
    uint pw8;
    int  w1;
    int  h1;
    
    byte[DisplayDriver.pw * DisplayDriver.ph / 8] monoFrameBuffer; // 1 bit per pixel for monochrome (1024 = 128 * 64 / 8)
    
    scrollUp(uint lines)
    {
        uint dy;
        for (uint uy = lines; uy < uint(DisplayDriver.ph); uy++)
        {
            dy = uy - lines;
            for (uint ux = 0; ux < uint(DisplayDriver.pw); ux++)
            {
                uint soffset = ((uy & 0xFFF8) * pw8) + ux; 
                uint doffset = ((dy & 0xFFF8) * pw8) + ux; 
                if ((monoFrameBuffer[soffset] & (1 << (uy & 0x07))) == 0)
                {
                    monoFrameBuffer[doffset] = monoFrameBuffer[doffset] & ~(1 << (dy & 0x07));
                }
                else
                {
                    monoFrameBuffer[doffset] = monoFrameBuffer[doffset] | (1 << (dy & 0x07));
                }
            } 
        }
        loop
        {
            dy++;
            if (dy >= uint(DisplayDriver.ph)) { break; }
            for (uint ux = 0; ux < uint(DisplayDriver.pw); ux++)
            {
                uint doffset = ((dy & 0xFFF8) * pw8) + ux; 
                monoFrameBuffer[doffset] = monoFrameBuffer[doffset] & ~(1 << (dy & 0x07));
            }
        }
    }
    
    bool visible
    {
        set
        {
            if (i2cConfigured)
            {
                Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
                Wire.Write(DisplayDriver.I2CController, 0);
                Wire.Write(DisplayDriver.I2CController, value ? SSD1306_DISPLAYON : SSD1306_DISPLAYOFF);
                byte result = Wire.EndTx(DisplayDriver.I2CController);
                if (result != 0)
                {
                    IO.WriteLn("Visible failed: " + result.ToString());
                }     
            }
        }
    }
    bool begin()
    {
        i2cConfigured = false;
        loop
        {
            Display.Reset();
            
            if (DisplayDriver.IsPortrait)
            {
                Display.PixelWidth  = Int.Min(DisplayDriver.pw, DisplayDriver.ph);
                Display.PixelHeight = Int.Max(DisplayDriver.pw, DisplayDriver.ph);
            }
            else
            {
                Display.PixelWidth  = Int.Max(DisplayDriver.pw, DisplayDriver.ph);
                Display.PixelHeight = Int.Min(DisplayDriver.pw, DisplayDriver.ph);
            }
            
            pw8 = uint(DisplayDriver.pw/8);
            w1  = (Display.PixelWidth -1);
            h1  = (Display.PixelHeight-1);
            
            // overclock I2C to 1MHz! (default is 400kHz but 1MHz seens to work)
            if (!Wire.Initialize(DisplayDriver.I2CController, DisplayDriver.I2CSDAPin, DisplayDriver.I2CSCLPin, 1000))
            {
                break;
            }
            
            Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
            
            Wire.Write(DisplayDriver.I2CController, 0x00);
            Wire.Write(DisplayDriver.I2CController, SSD1306_DISPLAYOFF);
            Wire.Write(DisplayDriver.I2CController, SSD1306_SETDISPLAYCLOCKDIV);
            Wire.Write(DisplayDriver.I2CController, 0x80);
            Wire.Write(DisplayDriver.I2CController, SSD1306_SETMULTIPLEX);
            Wire.Write(DisplayDriver.I2CController, byte(Display.PixelHeight-1));
            byte result = Wire.EndTx(DisplayDriver.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init1 failed: " + result.ToString());
                break;
            }
            
            Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
            Wire.Write(DisplayDriver.I2CController, 0x00);
            Wire.Write(DisplayDriver.I2CController, SSD1306_SETDISPLAYOFFSET);
            Wire.Write(DisplayDriver.I2CController, 0x00);
            Wire.Write(DisplayDriver.I2CController, SSD1306_SETSTARTLINE);
            Wire.Write(DisplayDriver.I2CController, SSD1306_CHARGEPUMP); // Enable charge pump regulator (RESET = )
            Wire.Write(DisplayDriver.I2CController, 0x14);              // Generate the high voltage from the 3.3v line internally
            result = Wire.EndTx(DisplayDriver.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init2 failed: " + result.ToString());
                break;
            }
            
            Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
            Wire.Write(DisplayDriver.I2CController, 0x00);
            Wire.Write(DisplayDriver.I2CController, SSD1306_MEMORYMODE);
            Wire.Write(DisplayDriver.I2CController, 0x00);
            Wire.Write(DisplayDriver.I2CController, SSD1306_SETSEGMENTREMAP);
            Wire.Write(DisplayDriver.I2CController, SSD1306_COMSCANDEC);
             result = Wire.EndTx(DisplayDriver.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init3 failed: " + result.ToString());
                break;
            }
            
            Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
            Wire.Write(0x00);
            Wire.Write(DisplayDriver.I2CController, SSD1306_SETCOMPINS);
            Wire.Write(DisplayDriver.I2CController, (Display.PixelWidth > 2 * Display.PixelHeight) ? 0x02 : 0x12);
            Wire.Write(DisplayDriver.I2CController, SSD1306_SETCONTRAST);
            Wire.Write(DisplayDriver.I2CController, 0xCF);
            Wire.Write(DisplayDriver.I2CController, SSD1306_SETPRECHARGE);
            Wire.Write(DisplayDriver.I2CController, 0xF1);
            result = Wire.EndTx(DisplayDriver.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init4 failed: " + result.ToString());
                break;
            }
            
            Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
            Wire.Write(DisplayDriver.I2CController, 0x00);
            Wire.Write(DisplayDriver.I2CController, SSD1306_SETVCOMDETECT);
            Wire.Write(DisplayDriver.I2CController, 0x40);
            Wire.Write(DisplayDriver.I2CController, SSD1306_DISPLAYALLONRESUME);
            Wire.Write(DisplayDriver.I2CController, SSD1306_NORMALDISPLAY);
            Wire.Write(DisplayDriver.I2CController, SSD1306_DEACTIVATESCROLL);
            Wire.Write(DisplayDriver.I2CController, SSD1306_DISPLAYON);
            //Wire.Write(DisplayDriver.I2CController, SSD1306_DISPLAYALLON);
            result = Wire.EndTx(DisplayDriver.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init5 failed: " + result.ToString());
                break;
            }
            
            i2cConfigured = true;
            break;
        }
        return i2cConfigured;
    }
    
    sendCommandI2C(byte command)
    {
        Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
        Wire.Write(DisplayDriver.I2CController, 0x00);
        Wire.Write(DisplayDriver.I2CController, command);
        byte result = Wire.EndTx(DisplayDriver.I2CController);
        if (result != 0)
        {
            IO.WriteLn("sendCommandI2C failed: " + result.ToString());
        }
    }
   
    update()
    {
        byte page;
        uint address;
        if (i2cConfigured)
        {
            for (int y = 0; y < DisplayDriver.ph; y++) 
            {
                if (y % 8 == 0)
                {
                    Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);    
                    Wire.Write(DisplayDriver.I2CController, 0x00);
                    Wire.Write(DisplayDriver.I2CController, SSD1306_SETPAGEADDR + page);
                    Wire.Write(DisplayDriver.I2CController, 0x10);
                    Wire.Write(DisplayDriver.I2CController, 0x00);
                    _ = Wire.EndTx(DisplayDriver.I2CController);
                    page++;
                }
                
                Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
                Wire.Write(DisplayDriver.I2CController, 0x40);
                Wire.Write(DisplayDriver.I2CController, monoFrameBuffer, address, pw8);
                address += pw8;
                _ = Wire.EndTx(DisplayDriver.I2CController);
            }
        }
    }
    
    clear(uint colour)
    {
        int size = DisplayDriver.pw * DisplayDriver.ph / 8;
        if (colour == Colour.Black)
        {
            for (int i = 0; i < size; i++)
            {
                monoFrameBuffer[i] = 0;
            }
        }
        else if (colour == Colour.Invert)
        {
            for (int i = 0; i < size; i++)
            {
                monoFrameBuffer[i] = ~monoFrameBuffer[i];
            }
        }
        else // everything else is 'white'
        {
            for (int i = 0; i < size; i++)
            {
                monoFrameBuffer[i] = 0xFF;
            }
        }
    }
    
    setPixel(int vx, int vy, uint colour)
    {
        if (FlipX)
        {
            vx = w1 - vx;
        }
        if (FlipY)
        {
            vy = h1 - vy;
        }
        if (IsPortrait)
        {
            Int.Swap(ref vx, ref vy);
        }
        
        uint ux = uint(vx);
        uint uy = uint(vy);
        
        uint offset = ((uy & 0xFFF8) * uint(DisplayDriver.pw/8)) + ux;
        if (offset < DisplayDriver.pw * DisplayDriver.ph / 8) // Clipping
        {
            if (colour == 0xF000) // Colour.Invert
            {
                monoFrameBuffer[offset] = monoFrameBuffer[offset] ^ (1 << (uy & 0x07));
            }
            else if (colour == 0x0000) // Colour.Black
            {
                monoFrameBuffer[offset] = monoFrameBuffer[offset] & ~(1 << (uy & 0x07));
            }
            else
            {
                monoFrameBuffer[offset] = monoFrameBuffer[offset] | (1 << (uy & 0x07));
            }
        }
    }
    setClippedTextPixel(int vx, int vy, uint colour)
    {
        if (FlipX)
        {
            vx = w1 - vx;
        }
        if (FlipY)
        {
            vy = h1 - vy;
        }
        if (IsPortrait)
        {
            Int.Swap(ref vx, ref vy);
        }
        
        uint ux = uint(vx);
        uint uy = uint(vy);
        uint offset = ((uy & 0xFFF8) * uint(DisplayDriver.pw/8)) + ux;
        if (colour == 0xF000) // Colour.Invert
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] ^ (1 << (uy & 0x07));
        }
        else if (colour == 0x0000) // Colour.Black
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] & ~(1 << (uy & 0x07));
        }
        else
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] | (1 << (uy & 0x07));
        }
    }
    
    horizontalLine(int vx1, int vy, int vx2, uint colour)
    {
        if (FlipX)
        {
            vx1 = w1 - vx1;
            vx2 = w1 - vx2;
            if (vx2 < vx1)
            {
                Int.Swap(ref vx1, ref vx2);
            }
        }
        if (FlipY)
        {
            vy = h1 - vy;
        }
        if (IsPortrait)
        {
            metaVerticalLine(vy, vx1, vx2, colour);
        }
        else
        {
            metaHorizontalLine(vx1, vy, vx2, colour);
        }
    }   
    
    verticalLine(int vx, int vy1, int vy2, uint colour)
    {
        if (FlipX)
        {
            vx = w1 - vx;
        }
        if (FlipY)
        {
            vy1 = h1 - vy1;
            vy2 = h1 - vy2;
            if (vy2 < vy1)
            {
                Int.Swap(ref vy1, ref vy2);
            }
        }
        if (IsPortrait)
        {
            metaHorizontalLine(vy1, vx, vy2, colour); 
        }
        else
        {
            metaVerticalLine(vx, vy1, vy2, colour);
        }
    }
    
    metaHorizontalLine(int vx1, int vy, int vx2, uint colour)
    {        
        uint uy = uint(vy);
        uint ux1 = uint(vx1);
        uint ux2 = uint(vx2);
        
        uint uyandpw8 = (uy & 0xFFF8) * pw8;
        byte uybit = byte(1 << (uy & 0x07));
        if (colour == 0xF000) // Colour.Invert
        {
            for (uint ux=ux1; ux <= ux2; ux++)
            {
                uint offset = uyandpw8 + ux;
                monoFrameBuffer[offset] = monoFrameBuffer[offset] ^ uybit;
            }
        }
        else if (colour == 0x0000) // Colour.Black
        {
            uybit = ~uybit;
            for (uint ux=ux1; ux <= ux2; ux++)
            {
                uint offset = uyandpw8 + ux;
                monoFrameBuffer[offset] = monoFrameBuffer[offset] & uybit;
            }
        }
        else
        {
            for (uint ux=ux1; ux <= ux2; ux++)
            {
                uint offset = uyandpw8 + ux;
                monoFrameBuffer[offset] = monoFrameBuffer[offset] | uybit;
            }
        }
    }
    
    metaVerticalLine(int vx, int vy1, int vy2, uint colour)
    {
        uint ux  = uint(vx);
        uint uy1 = uint(vy1);
        uint uy2 = uint(vy2);
        if (colour == 0xF000) // Colour.Invert
        {
            for (uint uy=uy1; uy <= uy2; uy++)
            {
                uint offset = ((uy & 0xFFF8) * pw8) + ux;
                monoFrameBuffer[offset] = monoFrameBuffer[offset] ^ (1 << (uy & 0x07));
            }
        }
        else if (colour == 0x0000) // Colour.Black
        {
            for (uint uy=uy1; uy <= uy2; uy++)
            {
                uint offset = ((uy & 0xFFF8) * pw8) + ux;
                monoFrameBuffer[offset] = monoFrameBuffer[offset] & ~(1 << (uy & 0x07));
            }
        }
        else
        {
            for (uint uy=uy1; uy <= uy2; uy++)
            {
                uint offset = ((uy & 0xFFF8) * pw8) + ux;
                monoFrameBuffer[offset] = monoFrameBuffer[offset] | (1 << (uy & 0x07));
            }
        }
    }
}
