unit DisplayDriver
{
    #define DISPLAY_DRIVER
    #define DISPLAY_IS_MONO
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Display"
    
    friend Display, Screen;
    
    //   https://github.com/adafruit/Adafruit_SH110x/blob/master/Adafruit_SH110X.cpp
    
    const byte SH110X_MEMORYMODE       = 0x20; // 0x20 or 0x21
    const byte SH110X_COLUMNADDR       = 0x21;
    const byte SH110X_PAGEADDR         = 0x22;
    
    const byte SH110X_SETCONTRAST     = 0x81;
    const byte SH110X_CHARGEPUMP      = 0x8D;
    const byte SH110X_SEGREMAP        = 0xA0; // 0xA0 or 0xA1
    const byte SH110X_SETSEGMENTREMAP = 0xA1; 
    
    const byte SH110X_DISPLAYALLON_RESUME = 0xA4;
    const byte SH110X_DISPLAYALLON    = 0xA5;
    const byte SH110X_NORMALDISPLAY   = 0xA6;
    const byte SH110X_INVERTDISPLAY   = 0xA7;
    const byte SH110X_SETMULTIPLEX    = 0xA8;
    
    const byte SH110X_DCDC            = 0xAD;
    const byte SH110X_DISPLAYOFF      = 0xAE;
    const byte SH110X_DISPLAYON       = 0xAF;
    
    const byte SH110X_SETPAGEADDR     = 0xB0;
    
    const byte SH110X_COMSCANINC      = 0xC0; // Scan direction?
    const byte SH110X_COMSCANDEC      = 0xC8;
            
    const byte SH110X_SETDISPLAYOFFSET   = 0xD3;
    const byte SH110X_SETDISPLAYCLOCKDIV = 0xD5;
    
    const byte SH110X_SETPRECHARGE       = 0xD9;
    const byte SH110X_SETCOMPINS         = 0xDA;
    const byte SH110X_SETVCOMDETECT      = 0xDB;
    const byte SH110X_SETDISPSTARTLINE   = 0xDC;
    
    const byte SH110X_SETLOWCOLUMN      = 0x00;
    const byte SH110X_SETHIGHCOLUMN     = 0x10;
    const byte SH110X_SETSTARTLINE      = 0x40; // Scrolling?
    
    byte i2cAddress = 0x3C; // typically a good default
    bool i2cConfigured;
    
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
    
    byte[DeviceDriver.pw * DeviceDriver.ph / 8] monoFrameBuffer; // 1 bit per pixel for monochrome (1024 = 128 * 64 / 8)
    
    
    scrollUp(uint lines)
    {
        uint dy;
        for (uint uy = lines; uy < uint(DeviceDriver.ph); uy++)
        {
            dy = uy - lines;
            for (uint ux = 0; ux < uint(DeviceDriver.pw); ux++)
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
            if (dy >= uint(DeviceDriver.ph)) { break; }
            for (uint ux = 0; ux < uint(DeviceDriver.pw); ux++)
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
                Wire.Write(DisplayDriver.I2CController, value ? SH110X_DISPLAYON : SH110X_DISPLAYOFF);
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
                Display.PixelWidth  = Int.Min(DeviceDriver.pw, DeviceDriver.ph);
                Display.PixelHeight = Int.Max(DeviceDriver.pw, DeviceDriver.ph);
            }
            else
            {
                Display.PixelWidth  = Int.Max(DeviceDriver.pw, DeviceDriver.ph);
                Display.PixelHeight = Int.Min(DeviceDriver.pw, DeviceDriver.ph);
            }
            
            pw8 = uint(DeviceDriver.pw/8);
            w1  = (Display.PixelWidth -1);
            h1  = (Display.PixelHeight-1);
    
            // overclock I2C to 1MHz! (default is 400kHz but 1MHz seens to work)
            if (!Wire.Initialize(DisplayDriver.I2CController, DisplayDriver.I2CSDAPin, DisplayDriver.I2CSCLPin, 400)) 
            {
                break;
            }
            
            Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
                 
            Wire.Write(DisplayDriver.I2CController, 0);
            Wire.Write(DisplayDriver.I2CController, SH110X_DISPLAYOFF);
            Wire.Write(DisplayDriver.I2CController, SH110X_SETDISPLAYCLOCKDIV);
            Wire.Write(DisplayDriver.I2CController, 0x51);
            Wire.Write(DisplayDriver.I2CController, SH110X_MEMORYMODE);     // SH110X_COLUMNADDR or SH110X_MEMORYMODE (did nothing)
            Wire.Write(DisplayDriver.I2CController, SH110X_SETCONTRAST);
            Wire.Write(DisplayDriver.I2CController, 0x47);
            Wire.Write(DisplayDriver.I2CController, SH110X_DCDC);
            Wire.Write(DisplayDriver.I2CController, 0x8A);
            Wire.Write(DisplayDriver.I2CController, SH110X_SEGREMAP); // SH110X_SEGREMAP or SH110X_SETSEGMENTREMAP
            Wire.Write(DisplayDriver.I2CController, SH110X_COMSCANINC);      // SH110X_COMSCANINC or SH110X_COMSCANDEC (blank?)
            Wire.Write(DisplayDriver.I2CController, SH110X_SETDISPSTARTLINE);
            Wire.Write(DisplayDriver.I2CController, 0);
            Wire.Write(DisplayDriver.I2CController, SH110X_SETDISPLAYOFFSET);
            Wire.Write(DisplayDriver.I2CController, 0x60);
            Wire.Write(DisplayDriver.I2CController, SH110X_SETPRECHARGE);
            Wire.Write(DisplayDriver.I2CController, 0x22);
            Wire.Write(DisplayDriver.I2CController, SH110X_SETVCOMDETECT);
            Wire.Write(DisplayDriver.I2CController, 0x35);
            Wire.Write(DisplayDriver.I2CController, SH110X_SETMULTIPLEX);
            Wire.Write(DisplayDriver.I2CController, 0x3F);
            Wire.Write(DisplayDriver.I2CController, SH110X_DISPLAYALLON_RESUME);
            Wire.Write(DisplayDriver.I2CController, SH110X_NORMALDISPLAY);
                       
            byte result = Wire.EndTx(DisplayDriver.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init1 failed: " + result.ToString());
                break;
            }
            
            Time.Delay(100);
            sendCommandI2C(SH110X_DISPLAYON);
            
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
        if (i2cConfigured)
        {
            byte page = 0;
            uint address = 0;
            
            for (int y = 0; y < DeviceDriver.ph; y++) 
            {
                if (y % 8 == 0)
                {
                    Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);    
                    Wire.Write(DisplayDriver.I2CController, 0x00);
                    Wire.Write(DisplayDriver.I2CController, SH110X_SETPAGEADDR + page);
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
        int size = DeviceDriver.pw * DeviceDriver.ph / 8;
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
        if (!FlipX)
        {
            vx = w1 - vx;
        }
        if (!FlipY)
        {
            vy = h1 - vy;
        }
        if (!IsPortrait)
        {
            Int.Swap(ref vx, ref vy);
        }
        
        uint ux = uint(vx);
        uint uy = uint(vy);
        uint offset = ((uy & 0xFFF8) * uint(DeviceDriver.pw/8)) + ux;
        if (offset < DeviceDriver.pw * DeviceDriver.ph / 8) // Clipping ..
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
        if (!FlipX)
        {
            vx = w1 - vx;
        }
        if (!FlipY)
        {
            vy = h1 - vy;
        }
        if (!IsPortrait)
        {
            Int.Swap(ref vx, ref vy);
        }
        
        uint ux = uint(vx);
        uint uy = uint(vy);
        uint offset = ((uy & 0xFFF8) * uint(DeviceDriver.pw/8)) + ux;
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
        if (!FlipX)
        {
            vx1 = w1 - vx1;
            vx2 = w1 - vx2;
            if (vx2 < vx1)
            {
                Int.Swap(ref vx1, ref vx2);
            }
        }
        if (!FlipY)
        {
            vy = h1 - vy;
        }
        if (IsPortrait)
        {
            metaHorizontalLine(vx1, vy, vx2, colour);
        }
        else
        {
            metaVerticalLine(vy, vx1, vx2, colour);
        }
    }   
    
    verticalLine(int vx, int vy1, int vy2, uint colour)
    {
        if (!FlipX)
        {
            vx = w1 - vx;
        }
        if (!FlipY)
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
            metaVerticalLine(vx, vy1, vy2, colour);
        }
        else
        {
            metaHorizontalLine(vy1, vx, vy2, colour);
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

