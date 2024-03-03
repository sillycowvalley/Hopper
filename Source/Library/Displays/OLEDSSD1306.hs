unit DisplayDriver
{
    #define DISPLAY_DRIVER
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Display"
    
    friend Display, Screen;
    
    // Eventually got it working reliably with a variation of the AdaFruit driver:
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
    const byte SSD1306_COMSCANINC      = 0xC0;
    const byte SSD1306_COMSCANDEC      = 0xC8;
        
    const byte SSD1306_SETDISPLAYOFFSET   = 0xD3;
    const byte SSD1306_SETDISPLAYCLOCKDIV = 0xD5;
    
    const byte SSD1306_SETPRECHARGE       = 0xD9;
    const byte SSD1306_SETCOMPINS         = 0xDA;
    const byte SSD1306_SETVCOMDETECT      = 0xDB;
    
    byte i2cAddress = 0x3C; // typically a good default
    bool i2cConfigured;
    
    const int pixelWidth  = 128;
    const int pixelHeight = 64;
    
    byte i2cController = Wire.DefaultI2CController;
    byte sdaPin        = Wire.DefaultI2CSDAPin;
    byte sclPin        = Wire.DefaultI2CSCLPin;
    byte I2CController { get { return i2cController; } set { i2cController = value; } }
    byte I2CAddress    { get { return i2cAddress; }    set { i2cAddress = value; } }
    byte I2CSDAPin     { get { return sdaPin; }        set { sdaPin = value; } }
    byte I2CSCLPin     { get { return sclPin; }        set { sclPin = value; } }
    
    byte[pixelWidth * pixelHeight / 8] monoFrameBuffer; // 1 bit per pixel for monochrome (1024 = 128 * 64 / 8)
    
    scrollUp(uint lines)
    {
        uint pw8 = uint(pixelWidth/8);
        uint dy;
        for (uint uy = lines; uy < uint(Display.PixelHeight); uy++)
        {
            dy = uy - lines;
            for (uint ux = 0; ux < uint(Display.PixelWidth); ux++)
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
            if (dy >= uint(Display.PixelHeight)) { break; }
            for (uint ux = 0; ux < uint(Display.PixelWidth); ux++)
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
#ifdef DISPLAY_DIAGNOSTICS
        IO.Write("<OLEDSSD1306.Begin");
#endif
        loop
        {
            Display.Reset();
            
            
            Display.PixelWidth  = pixelWidth;
            Display.PixelHeight = pixelHeight;
            
            if (!Wire.Initialize(DisplayDriver.I2CController, DisplayDriver.I2CSDAPin, DisplayDriver.I2CSCLPin))
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
            result = Wire.EndTx(DisplayDriver.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init5 failed: " + result.ToString());
                break;
            }
            
            i2cConfigured = true;
            break;
        }
#ifdef DISPLAY_DIAGNOSTICS
        IO.WriteLn(">");
#endif        
        return i2cConfigured;
    }
    
    
    // https://github.com/ThingPulse/esp8266-oled-ssd1306/blob/master/src/OLEDDisplay.cpp
    Invert(bool invertColours)
    {
        if (i2cConfigured)
        {
            sendCommandI2C(invertColours ? SSD1306INVERTDISPLAY : SSD1306NORMALDISPLAY);
        }
    }
    Flip(bool flipVertical)
    {
        if (i2cConfigured)
        {
            sendCommandI2C(flipVertical ? SSD1306COMSCANDEC      : SSD1306COMSCANINC);
            sendCommandI2C(flipVertical ? SSD1306SETSEGMENTREMAP : SSD1306SEGREMAP);
        }
    }
    Show(bool on)
    {
        if (i2cConfigured)
        {
            sendCommandI2C(on ? SSD1306DISPLAYON : SSD1306DISPLAYOFF);
        }
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
#ifdef DISPLAY_DIAGNOSTICS
        IO.Write("<OLEDSSD1306.UpdateDisplay");
#endif
        if (i2cConfigured)
        {
            // This re-initialization seems to keep the screen position in sync:
            Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
            Wire.Write(DisplayDriver.I2CController, 0x00);
            Wire.Write(DisplayDriver.I2CController, SSD1306_PAGEADDR);   // Reset Page Address (for horizontal addressing)
            Wire.Write(DisplayDriver.I2CController, 0x00);
            Wire.Write(DisplayDriver.I2CController, 0xFF);             //Wire.Write(byte((Display.PixelHeight/8)-1));
            Wire.Write(DisplayDriver.I2CController, SSD1306_COLUMNADDR); // Reset Column Address (for horizontal addressing)
            if (Display.PixelWidth == 64)
            {
                Wire.Write(DisplayDriver.I2CController, 32);
                Wire.Write(DisplayDriver.I2CController, byte(32+Display.PixelWidth-1));
            }
            else
            {
                Wire.Write(DisplayDriver.I2CController, 0x00);
                Wire.Write(DisplayDriver.I2CController, byte(Display.PixelWidth-1));
            }
            byte result = Wire.EndTx(DisplayDriver.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Update init failed: " + result.ToString());
            }
            
            // update screen from buffer
            uint address = 0;
            byte written;
                
            uint pw8 = uint(Display.PixelWidth >> 3); // 128/8 = 16 bytes per transaction seems small enough
            for (int y = 0; y < pixelHeight; y++) 
            {
                Wire.BeginTx(DisplayDriver.I2CController, i2cAddress);
                Wire.Write(DisplayDriver.I2CController, 0x40);
                //for (uint i=0; i < pw8; i++) { Wire.Write(DisplayDriver.I2CController, monoFrameBuffer[address+i]); }
                Wire.Write(DisplayDriver.I2CController, monoFrameBuffer, address, pw8);
                address += pw8;
                result = Wire.EndTx(DisplayDriver.I2CController);
                if (result != 0)
                {
                    IO.WriteLn("Update failed: " + result.ToString());
                    break;
                }
            }
        }
#ifdef DISPLAY_DIAGNOSTICS
        IO.WriteLn(">");
#endif        
    }
    clear(uint colour)
    {
        int size = pixelWidth * pixelHeight / 8;
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
    setPixel(int x, int y, uint colour)
    {
        uint ux = uint(x);
        uint uy = uint(y);
        
        uint offset = ((uy & 0xFFF8) * uint(Display.PixelWidth/8)) + ux;
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
    
    horizontalLine(int x1, int y, int x2, uint colour)
    {
        uint pw8 = uint(Display.PixelWidth/8);
        uint uy = uint(y);
        uint uyandpw8 = (uy & 0xFFF8) * pw8;
        uint ux1 = uint(x1);
        uint ux2 = uint(x2);
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
    verticalLine(int x, int y1, int y2, uint colour)
    {
        uint ux  = uint(x);
        uint uy1 = uint(y1);
        uint uy2 = uint(y2);
        uint pw8 = uint(Display.PixelWidth/8);
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
