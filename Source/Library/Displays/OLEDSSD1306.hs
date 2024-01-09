unit DisplayDriver
{
    uses "/Source/Library/MCU"
    uses "/Source/Library/Display"
    
    // Eventually got it working reliablty with a variation of the AdaFruit driver:
    //   https://github.com/adafruit/Adafruit_SSD1306/blob/master/Adafruit_SSD1306.cpp
    
    const byte SSD1306MEMORYMODE      = 0x20;
    const byte SSD1306COLUMNADDR      = 0x21;
    const byte SSD1306PAGEADDR        = 0x22;
    const byte SD1306DEACTIVATESCROLL = 0x2E;
    
    const byte SSD1306SETSTARTLINE    = 0x40;
    
    const byte SSD1306SETCONTRAST     = 0x81;
    const byte SSD1306CHARGEPUMP      = 0x8D;

    const byte SSD1306SEGREMAP        = 0xA0;
    const byte SSD1306SETSEGMENTREMAP = 0xA1;
    const byte SSD1306DISPLAYALLONRESUME = 0xA4;
    const byte SSD1306DISPLAYALLON    = 0xA5;
    const byte SSD1306NORMALDISPLAY   = 0xA6;
    const byte SSD1306INVERTDISPLAY   = 0xA7;

    const byte SSD1306SETMULTIPLEX    = 0xA8;
    const byte SSD1306DISPLAYOFF      = 0xAE;
    const byte SSD1306DISPLAYON       = 0xAF;
    const byte SSD1306COMSCANINC      = 0xC0;
    const byte SSD1306COMSCANDEC      = 0xC8;
        
    const byte SSD1306SETDISPLAYOFFSET   = 0xD3;
    const byte SSD1306SETDISPLAYCLOCKDIV = 0xD5;
    
    const byte SSD1306SETPRECHARGE       = 0xD9;
    const byte SSD1306SETCOMPINS         = 0xDA;
    const byte SSD1306SETVCOMDETECT      = 0xDB;
    
    byte i2cAddress;
    bool resolutionSet;
    bool i2cConfigured;
    int pixelWidth; 
    int pixelHeight;
    
    byte[1024] monoFrameBuffer; // 1 bit per pixel for monochrome (1024 = 128 * 64 / 8)
    
    int PixelWidth  { get { return pixelWidth;  } }
    int PixelHeight { get { return pixelHeight; } }
    SetResolution(int width, int height)
    {
        pixelWidth = width;
        pixelHeight = height;
        resolutionSet = true;
    }
    bool Begin()
    {
        i2cConfigured = false;
#ifdef DISPLAYDIAGNOSTICS
        IO.Write("<OLEDSSD1306.Begin");
#endif
        loop
        {
            Display.Reset();
            i2cAddress = Display.I2CAddress;
            
            if (!resolutionSet)
            {
                pixelWidth = 128;
                pixelHeight = 64;
                resolutionSet = true;
            }
            if (!Wire.Initialize(Display.I2CController, Display.I2CSDAPin, Display.I2CSCLPin))
            {
                break;
            }
            
            Wire.BeginTx(Display.I2CController, i2cAddress);
            
            Wire.Write(Display.I2CController, 0x00);
            Wire.Write(Display.I2CController, SSD1306DISPLAYOFF);
            Wire.Write(Display.I2CController, SSD1306SETDISPLAYCLOCKDIV);
            Wire.Write(Display.I2CController, 0x80);
            Wire.Write(Display.I2CController, SSD1306SETMULTIPLEX);
            Wire.Write(Display.I2CController, byte(pixelHeight-1));
            byte result = Wire.EndTx(Display.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init1 failed: " + result.ToString());
                break;
            }
            
            Wire.BeginTx(Display.I2CController, i2cAddress);
            Wire.Write(Display.I2CController, 0x00);
            Wire.Write(Display.I2CController, SSD1306SETDISPLAYOFFSET);
            Wire.Write(Display.I2CController, 0x00);
            Wire.Write(Display.I2CController, SSD1306SETSTARTLINE);
            Wire.Write(Display.I2CController, SSD1306CHARGEPUMP); // Enable charge pump regulator (RESET = )
            Wire.Write(Display.I2CController, 0x14);              // Generate the high voltage from the 3.3v line internally
            result = Wire.EndTx(Display.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init2 failed: " + result.ToString());
                break;
            }
            
            Wire.BeginTx(Display.I2CController, i2cAddress);
            Wire.Write(Display.I2CController, 0x00);
            Wire.Write(Display.I2CController, SSD1306MEMORYMODE);
            Wire.Write(Display.I2CController, 0x00);
            Wire.Write(Display.I2CController, SSD1306SETSEGMENTREMAP);
            Wire.Write(Display.I2CController, SSD1306COMSCANDEC);
             result = Wire.EndTx(Display.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init3 failed: " + result.ToString());
                break;
            }
            
            Wire.BeginTx(Display.I2CController, i2cAddress);
            Wire.Write(0x00);
            Wire.Write(Display.I2CController, SSD1306SETCOMPINS);
            Wire.Write(Display.I2CController, (pixelWidth > 2 * pixelHeight) ? 0x02 : 0x12);
            Wire.Write(Display.I2CController, SSD1306SETCONTRAST);
            Wire.Write(Display.I2CController, 0xCF);
            Wire.Write(Display.I2CController, SSD1306SETPRECHARGE);
            Wire.Write(Display.I2CController, 0xF1);
            result = Wire.EndTx(Display.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init4 failed: " + result.ToString());
                break;
            }
            
            Wire.BeginTx(Display.I2CController, i2cAddress);
            Wire.Write(Display.I2CController, 0x00);
            Wire.Write(Display.I2CController, SSD1306SETVCOMDETECT);
            Wire.Write(Display.I2CController, 0x40);
            Wire.Write(Display.I2CController, SSD1306DISPLAYALLONRESUME);
            Wire.Write(Display.I2CController, SSD1306NORMALDISPLAY);
            Wire.Write(Display.I2CController, SD1306DEACTIVATESCROLL);
            Wire.Write(Display.I2CController, SSD1306DISPLAYON);
            result = Wire.EndTx(Display.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Init5 failed: " + result.ToString());
                break;
            }
            
            i2cConfigured = true;
            break;
        }
#ifdef DISPLAYDIAGNOSTICS
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
        Wire.BeginTx(Display.I2CController, i2cAddress);
        Wire.Write(Display.I2CController, 0x00);
        Wire.Write(Display.I2CController, command);
        byte result = Wire.EndTx(Display.I2CController);
        if (result != 0)
        {
            IO.WriteLn("sendCommandI2C failed: " + result.ToString());
        }
    }
    
    Update()
    {
#ifdef DISPLAYDIAGNOSTICS
        IO.Write("<OLEDSSD1306.Update");
#endif
        if (i2cConfigured)
        {
            // This re-initialization seems to keep the screen position in sync:
            Wire.BeginTx(Display.I2CController, i2cAddress);
            Wire.Write(Display.I2CController, 0x00);
            Wire.Write(Display.I2CController, SSD1306PAGEADDR);   // Reset Page Address (for horizontal addressing)
            Wire.Write(Display.I2CController, 0x00);
            Wire.Write(Display.I2CController, 0xFF);             //Wire.Write(byte((pixelHeight/8)-1));
            Wire.Write(Display.I2CController, SSD1306COLUMNADDR); // Reset Column Address (for horizontal addressing)
            if (pixelWidth == 64)
            {
                Wire.Write(Display.I2CController, 32);
                Wire.Write(Display.I2CController, byte(32+pixelWidth-1));
            }
            else
            {
                Wire.Write(Display.I2CController, 0x00);
                Wire.Write(Display.I2CController, byte(pixelWidth-1));
            }
            byte result = Wire.EndTx(Display.I2CController);
            if (result != 0)
            {
                IO.WriteLn("Update init failed: " + result.ToString());
            }
            
            // update screen from buffer
            uint address = 0;
            byte written;
                
            for (int y = 0; y < pixelHeight; y++) 
            {
                // 128/8 = 16 bytes per transaction seems small enough
                Wire.BeginTx(Display.I2CController, i2cAddress);
                Wire.Write(Display.I2CController, 0x40);
                for(int i = 0; i < pixelWidth/8; i++) 
                {
                    Wire.Write(Display.I2CController, monoFrameBuffer[address]); 
                    address++;
                }
                result = Wire.EndTx(Display.I2CController);
                if (result != 0)
                {
                    IO.WriteLn("Update failed: " + result.ToString());
                }
            }
        }
#ifdef DISPLAYDIAGNOSTICS
        IO.WriteLn(">");
#endif        
    }
    ClearBuffer(uint colour)
    {
        int size = pixelWidth * pixelHeight / 8;
        if (colour == Color.Black)
        {
            for (int i = 0; i < size; i++)
            {
                monoFrameBuffer[i] = 0;
            }
        }
        else if (colour == Color.Invert)
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
    RawSetPixel(int x, int y, uint colour)
    {
        uint ux = uint(x);
        uint uy = uint(y);
        
        uint offset = ((uy & 0xFFF8) * (uint(pixelWidth)/8)) + ux;
        if (colour == 0xF000) // Color.Invert
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] ^ (1 << (uy & 0x07));
        }
        else if (colour == 0x0000) // Color.Black
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] & ~(1 << (uy & 0x07));
        }
        else
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] | (1 << (uy & 0x07));
        }
    }
    SetPixel(int x, int y, uint colour)
    {
        if ((x < 0) || (y < 0) || (x >= pixelWidth) || (y >= pixelHeight)) { return; }
        
        uint ux = uint(x);
        uint uy = uint(y);
        uint offset = ((uy & 0xFFF8) * (uint(pixelWidth)/8)) + ux;
        if (colour == 0xF000) // Color.Invert
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] ^ (1 << (uy & 0x07));
        }
        else if (colour == 0x0000) // Color.Black
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] & ~(1 << (uy & 0x07));
        }
        else
        {
            monoFrameBuffer[offset] = monoFrameBuffer[offset] | (1 << (uy & 0x07));
        }
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
        int ymax = pixelHeight-1;
        if (y >= ymax) { return; }
        
        int xmax = pixelWidth-1;
        if (x1 >= xmax) { return; }
        
        if (x1 < 0) { x1 = 0; }
        if (x2 >= xmax) { x2 = xmax; }
        
        Suspend();
        for (int x=x1; x <= x2; x++)
        {
            RawSetPixel(x, y, colour);
        }
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
        int ymax = pixelHeight-1;
        if (y1 >= ymax) { return; }
        
        int xmax = pixelWidth-1;
        if (x >= xmax) { return; }
        
        if (y1 < 0) { y1 = 0; }
        if (y2 >= ymax) { y2 = ymax; }
        
        Suspend();
        for (int y=y1; y <= y2; y++)
        {
            RawSetPixel(x, y, colour);
        }
        Resume();
    }
}
