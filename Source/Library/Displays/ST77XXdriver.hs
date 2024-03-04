unit DisplayDriver
{
    #define DISPLAY_DRIVER
    #define NO_SUSPEND_RESUME
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Display"
    
    friend Display, Screen;
    
    const byte TFT_SLPOUT     = 0x11; //  Sleep Out
    
    const byte TFT_CASET      = 0x2A;
    const byte TFT_PASET      = 0x2B;
    const byte TFT_RAMWR      = 0x2C;
    const byte TFT_RAMRD      = 0x2E;
    
    const byte TFT_MADCTL     = 0x36;
    
    const byte MADCTL_MY     = 0x80; // Bottom to top
    const byte MADCTL_MX     = 0x40; // Right to left
    const byte MADCTL_MV     = 0x20; // Reverse Mode
    const byte MADCTL_ML     = 0x10;
    const byte MADCTL_BGR    = 0x08; // Blue-Green-Red pixel order
    const byte MADCTL_RGB    = 0x00;
    
    const byte ST77XX_SWRESET = 0x01;
    const byte ST77XX_NORON   = 0x13;
    
    const byte ST77XX_INVOFF  = 0x20;
    const byte ST77XX_INVON   = 0x21;
    const byte ST77XX_DISPON  = 0x29;
    
    const byte ST77XX_COLMOD  = 0x3A;
    
    const byte ST77XX_FRMCTR1 = 0xB1;
    const byte ST77XX_FRMCTR2 = 0xB2;
    const byte ST77XX_FRMCTR3 = 0xB3;
    const byte ST77XX_INVCTR  = 0xB4;
   
    const byte ST77XX_PWCTR1  = 0xC0;
    const byte ST77XX_PWCTR2  = 0xC1;
    const byte ST77XX_PWCTR3  = 0xC2;
    const byte ST77XX_PWCTR4  = 0xC3;
    const byte ST77XX_PWCTR5  = 0xC4;
    const byte ST77XX_VMCTR1  = 0xC5;
    
    const byte ST77XX_GMCTRP1 = 0xE0;
    const byte ST77XX_GMCTRN1 = 0xE1;
    
#ifdef ADAFRUIT_TFT_096    
    const byte INVERT = ST77XX_INVON;
    const byte MADCTL = (  MADCTL_MY | MADCTL_MV | MADCTL_BGR);
#endif
#ifdef ADAFRUIT_TFT_114    
    const byte INVERT = ST77XX_INVON;
    const byte MADCTL = ( MADCTL_MV | MADCTL_MX | MADCTL_RGB );
#endif
#ifdef WAVESHARE_PICO_LCD_144 
    // Pico-LCD-1.44 - Don't invert display
    const byte INVERT = ST77XX_INVOFF;
    const byte MADCTL = MADCTL_BGR;
#endif        
#ifdef WAVESHARE_PICO_LCD_096
    // Pico-LCD-0.96 - Invert display colours
    const byte INVERT = ST77XX_INVON;
    const byte MADCTL = (  MADCTL_MY | MADCTL_MV | MADCTL_BGR);
#endif
#ifdef WAVESHARE_PICO_LCD_114
    // Pico-LCD-1.14 - Invert display colours
    const byte INVERT = ST77XX_INVON;
    const byte MADCTL = ( MADCTL_MX | MADCTL_MV | MADCTL_RGB);
#endif
#ifdef WAVESHARE_RP2040_LCD_096
    // RP2040-LCD-0.96 - Invert display colours
    const byte INVERT = ST77XX_INVON;
    const byte MADCTL = MADCTL_BGR;
#endif
    const byte[] initCmdConst =
    {
      //  (COMMAND_BYTE), n, data_bytes....
      TFT_SLPOUT    , 0x80,     // Sleep exit
      ST77XX_FRMCTR1, 3      ,  //  3: Frame rate ctrl - normal mode, 3 args:
        0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
      ST77XX_FRMCTR2, 3      ,  //  4: Frame rate control - idle mode, 3 args:
        0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
      ST77XX_FRMCTR3, 6      ,  //  5: Frame rate ctrl - partial mode, 6 args:
        0x01, 0x2C, 0x2D,       //     Dot inversion mode
        0x01, 0x2C, 0x2D,       //     Line inversion mode
      ST77XX_INVCTR , 1      ,  //  6: Display inversion ctrl, 1 arg, no delay:
        0x07,                   //     No inversion
      ST77XX_PWCTR2 , 1      ,  //  8: Power control, 1 arg, no delay:
        0xC5,                   //     VGH25 = 2.4C VGSEL = -10 VGH = 3 * AVDD
      ST77XX_PWCTR3 , 2      ,  //  9: Power control, 2 args, no delay:
        0x0A,                   //     Opamp current small
        0x00,                   //     Boost frequency
      ST77XX_VMCTR1 , 1      ,  // 12: Power control, 1 arg, no delay:
        0x0E,

// ---- Device specific options:
      INVERT , 0      ,         // 13: 
      TFT_MADCTL      , 1,      MADCTL,              // Memory Access Control
// ---- 
      
      ST77XX_COLMOD , 1      ,  // 15: set colour mode, 1 arg, no delay:
        0x05,
      ST77XX_GMCTRP1, 16      , 0x02, 0x1c, 0x07, 0x12, 0x37, 0x32, 0x29, 0x2d, 0x29, 0x25, 0x2B, 0x39, 0x00, 0x01, 0x03, 0x10, // Set Gamma
      ST77XX_GMCTRN1, 16      , 0x03, 0x1d, 0x07, 0x06, 0x2E, 0x2C, 0x29, 0x2D, 0x2E, 0x2E, 0x37, 0x3F, 0x00, 0x00, 0x02, 0x10, // Set Gamma
      ST77XX_NORON  ,    0x80, //  3: Normal display on, no args, w/delay
      ST77XX_DISPON ,    0x80, //  4: Main screen turn on, no args w/delay
       0x00                        // End of list
    };
    
    bool flipX;
    bool flipY;
    bool isPortrait;
    bool FlipX { get { return flipX; } set { flipX = value; }}
    bool FlipY { get { return flipY; } set { flipY = value; }}
    bool IsPortrait { get { return isPortrait; } set { isPortrait = value; } }
    
    int  w1;
    uint w2;
    int  h1;
    uint pw2;
    int  pwm1;
    
    const uint bufferSize = DeviceDriver.pw*4;
    byte[bufferSize] frameBuffer; // 4 bytes per pixel (for reading)
        
#ifdef BUFFER_TEXT
    record CharCell
    {
        char c;
        uint foreColour;
        uint backColour;
    }
    <CharCell> textBuffer;
    uint textBufferSize;
    
    bufferText(byte col, byte row, char c, uint foreColour, uint backColour)
    {
        uint index = row * Screen.Columns + col;
        CharCell cell;
        cell.c = c;
        cell.foreColour = foreColour;
        cell.backColour = backColour;
        textBuffer[index] = cell;
    }
#endif

    uint last444 = 0;
    uint last565 = 0;
    uint convertToRGB565(uint rgb444)
    {
        if (last444 != rgb444)
        {
            byte rColor = byte(rgb444 >> 8);
            byte gColor = byte((rgb444 >> 4) & (0x0F));
            byte bColor = byte(rgb444 & 0x0F);
            uint c565 = uint((rColor << 12) + (gColor << 7) + (bColor << 1));
            if (rColor > 3)
            {
                c565 |= 0x0800;
            }
            if (gColor > 3)
            {
                c565 |= 0x0060;
            }
            if (bColor > 3)
            {
                c565 |= 0x0001;
            }
            last565 = c565;
            last444 = rgb444;
        }
        return last565;
    }
    uint convertFromRGB565(uint rgb565)
    {
        if (last565 != rgb565)
        {
            byte rColor = byte(rgb565 >> 12);
            byte gColor = byte((rgb565 >> 7) & 0x0F);
            byte bColor = byte((rgb565 >> 1) & 0x0F);
            last444     = uint((rColor << 8) + (gColor << 4) + bColor);
            last565 = rgb565;
        }
        return last444;
    }
    uint readColorSPI()
    {
        byte r;
        byte g;
        byte b;
        
        r = SPI.ReadByte(DeviceDriver.spiController);
        r = SPI.ReadByte(DeviceDriver.spiController);
        g = SPI.ReadByte(DeviceDriver.spiController);
        b = SPI.ReadByte(DeviceDriver.spiController);
    
        return ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
    }
    setAddrWindowSPI(int x1, int y1, int x2, int y2, bool write)
    {
        x1 += DeviceDriver.xFudge;
        x2 += DeviceDriver.xFudge;
        y1 += DeviceDriver.yFudge;
        y2 += DeviceDriver.yFudge;
        
        // Column address set
        MCU.DigitalWrite(DeviceDriver.dcPin, false);
        SPI.WriteByte(DeviceDriver.spiController, TFT_CASET);
        MCU.DigitalWrite(DeviceDriver.dcPin, true);
        SPI.WriteWord(DeviceDriver.spiController, uint(x1));
        SPI.WriteWord(DeviceDriver.spiController, uint(x2));
        
        // Row address set
        MCU.DigitalWrite(DeviceDriver.dcPin, false);
        SPI.WriteByte(DeviceDriver.spiController, TFT_PASET);
        MCU.DigitalWrite(DeviceDriver.dcPin, true);
        SPI.WriteWord(DeviceDriver.spiController, uint(y1));
        SPI.WriteWord(DeviceDriver.spiController, uint(y2));
        
        // Write or Read RAM
        MCU.DigitalWrite(DeviceDriver.dcPin, false);
        byte rw = write ? TFT_RAMWR : TFT_RAMRD;
        SPI.WriteByte(DeviceDriver.spiController, rw);
        MCU.DigitalWrite(DeviceDriver.dcPin, true);
    }
    
    bool visible
    {
        set
        {
            MCU.DigitalWrite(DeviceDriver.blPin, value); // backlight on|off ?
        }
    }
    clear(uint colour)
    {
        if (colour == Colour.Invert)
        {
            for (int y = 0; y < DeviceDriver.ph; y++)
            {
                horizontalLine(0, y, pwm1, colour);
            }
            return;
        }

        uint rgb565 = convertToRGB565(colour);
        byte lsb = byte(rgb565 & 0xFF);
        byte msb = byte(rgb565 >> 8);
        for (uint i=0; i < pw2; i += 2)
        {
            frameBuffer[i+1] = lsb;
            frameBuffer[i+0] = msb;
        }
        for (int y = 0; y < DeviceDriver.ph; y++)
        {
            SPI.BeginTransaction(DeviceDriver.spiController);
            MCU.DigitalWrite(DeviceDriver.csPin, false);
            setAddrWindowSPI(0, y, pwm1, y, true);
            SPI.WriteBuffer(DeviceDriver.spiController, frameBuffer, 0, pw2);
            MCU.DigitalWrite(DeviceDriver.csPin, true);
            SPI.EndTransaction(DeviceDriver.spiController);
        }
#ifdef BUFFER_TEXT
        for (uint index = 0; index < textBufferSize; index++)
        {
            CharCell cell = textBuffer[index];
            cell.c = ' ';
            cell.foreColour = colour;
            cell.backColour = colour;
            textBuffer[index] = cell;
        }
#endif
    }
    
#if !defined(HAS_DISPLAY_READ)
    bool warnOnce;
#endif
    scrollUp(uint lines)
    {
#if !defined(HAS_DISPLAY_READ)

#ifdef BUFFER_TEXT
        // We have a write-only display. In the absense of proper scrolling,
        // let's at least scroll the regular (unscaled) text.
        uint id = 0;
        uint is = Screen.Columns;
        while (is < textBufferSize)
        {
            textBuffer[id] = textBuffer[is];
            is++;
            id++;
        }
        CharCell cell;
        cell.c = ' ';
        cell.foreColour = Display.BackColour;
        cell.backColour = Display.BackColour;
        while (id < textBufferSize)
        {
            textBuffer[id] = cell;   
            id++;
        }
        for (byte row = 0; row < Screen.Rows; row++)
        {
            for (byte col = 0; col < Screen.Columns; col++)
            {
                uint index = row * Screen.Columns + col;
                cell = textBuffer[index];       
                Screen.DrawChar(col, row, cell.c, cell.foreColour, cell.backColour);
            }
        }
#else
        if (!warnOnce)
        {
            WriteLn("#### This display is write-only ####");
            warnOnce = true;
        }
#endif
#else
        int drow;
        byte r; byte g; byte b; uint ri; uint wi;
        for (int row = int(lines); row < DeviceDriver.ph; row++)
        {
            SPI.BeginTransaction(DeviceDriver.spiController);
            MCU.DigitalWrite(DeviceDriver.csPin, false);
            setAddrWindowSPI(0, row, w1, row, false);
            SPI.ReadBuffer(DeviceDriver.spiController, frameBuffer, 0, w2);
            MCU.DigitalWrite(DeviceDriver.csPin, true);
            SPI.EndTransaction(DeviceDriver.spiController);
            
            ri = 0;
            wi = 0;
            for (int x = 0; x < DeviceDriver.pw; x++)
            {
                r = frameBuffer[ri+1];
                g = frameBuffer[ri+2];
                b = frameBuffer[ri+3];
                ri += 4;
                
                uint rgb565 = ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
                frameBuffer[wi+0] = byte(rgb565 >> 8);
                frameBuffer[wi+1] = byte(rgb565 &  0xFF);
                wi += 2;
            }
            SPI.BeginTransaction(DeviceDriver.spiController);
            MCU.DigitalWrite(DeviceDriver.csPin, false);
            setAddrWindowSPI(0, drow, w1, drow, true);
            SPI.WriteBuffer(DeviceDriver.spiController, frameBuffer, 0, uint(DeviceDriver.pw));
            MCU.DigitalWrite(DeviceDriver.csPin, true);
            SPI.EndTransaction(DeviceDriver.spiController);
            drow++;    
        }
        loop
        {
            if (drow >= DeviceDriver.ph) { break; }
            horizontalLine(0, drow, w1, Display.BackColour);
            drow++;   
        }
#endif
    }
    
    sendCommandSPI(byte commandByte, byte[] dataBytes, uint startIndex, byte numDataBytes)
    {
        SPI.BeginTransaction(DeviceDriver.spiController);
        MCU.DigitalWrite(DeviceDriver.csPin, false);
    
        MCU.DigitalWrite(DeviceDriver.dcPin, false); // Command mode
        SPI.WriteByte(DeviceDriver.spiController, commandByte);     // Send the command byte
    
        MCU.DigitalWrite(DeviceDriver.dcPin, true);
        for (uint i = 0; i < numDataBytes; i++)
        {
            SPI.WriteByte(DeviceDriver.spiController, byte(dataBytes[startIndex+i])); // Send the data bytes
        }
    
        MCU.DigitalWrite(DeviceDriver.csPin, true);
        SPI.EndTransaction(DeviceDriver.spiController);
    }
    
    bool begin()
    {
        bool success = false;
        loop
        {
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
            
            w1 = (Display.PixelWidth -1);
            h1 = (Display.PixelHeight-1);
            w2 = uint(Display.PixelWidth) * 2;
            
            pw2  = uint(DeviceDriver.pw*2);
            pwm1 = DeviceDriver.pw-1;
            
#ifdef BUFFER_TEXT            
            textBufferSize = Screen.Rows * Screen.Columns;
            CharCell empty;
            empty.c = ' ';
            empty.foreColour = Display.BackColour;
            empty.backColour = Display.BackColour;
            for (uint index = 0; index < textBufferSize; index++)
            {
                textBuffer.Append(empty);
            }
#endif
            
            Display.Reset();
            for (uint i = 0; i < bufferSize; i++)
            {
                frameBuffer[i] = 0;
            }
            SPI.SetCSPin(DeviceDriver.spiController,  DeviceDriver.csPin);
            SPI.SetTxPin(DeviceDriver.spiController,  DeviceDriver.txPin);
#ifdef HAS_DISPLAY_READ
            SPI.SetRxPin(DeviceDriver.spiController,  DeviceDriver.rxPin);
#endif
            SPI.SetClkPin(DeviceDriver.spiController, DeviceDriver.clkPin);
            
            MCU.PinMode(DeviceDriver.blPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.blPin, true);
            
            MCU.PinMode(DeviceDriver.csPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.csPin, true); // Deselect
            MCU.PinMode(DeviceDriver.dcPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.dcPin, true); // Data mode
            
            //SPI.Settings(DeviceDriver.spiController, 4000000, DataOrder.MSBFirst, DataMode.Mode0);// these are the defaults
            
            if (!SPI.Begin(DeviceDriver.spiController))
            {
                IO.WriteLn("DeviceDriver.Begin failed in SPI.Begin");
                break;
            }

#ifdef HAS_RESET_PIN            
            // hardware reset
            MCU.PinMode(DeviceDriver.rstPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.rstPin, true);
            Time.Delay(100);
            MCU.DigitalWrite(DeviceDriver.rstPin, false);
            Time.Delay(100);
            MCU.DigitalWrite(DeviceDriver.rstPin, true);
            Time.Delay(200);
#endif
            
            byte[] initCmd = initCmdConst;
            uint i = 0;
            loop
            {
                byte cmd = byte(initCmd[i]);
                if (cmd == 0) { break; } 
                i++;
                byte x = byte(initCmd[i]);
                i++;
                byte numArgs = x & 0x7F;
                sendCommandSPI(cmd, initCmd, i, numArgs);
                i += numArgs;
                if (x & 0x80 != 0)
                {
                    Time.Delay(150);
                }
            }
            
            success = true;
            break;
        }
        return success;
    }
    uint getPixel(int x, int y)
    {
#if !defined(HAS_DISPLAY_READ)
        WriteLn("#### This display is write-only ####");
        Die(0x0A);
        return 0;
#else
        SPI.BeginTransaction(DeviceDriver.spiController);
        MCU.DigitalWrite(DeviceDriver.csPin, false);
        setAddrWindowSPI(x, y, x, y, false);
        uint rgb565 = readColorSPI();
        MCU.DigitalWrite(DeviceDriver.csPin, true);
        SPI.EndTransaction(DeviceDriver.spiController);
        uint colour = convertFromRGB565(rgb565);
        return colour;
#endif
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
        
        int x = vx;
        int y = vy;
        
        uint rgb565;
        if (colour == Colour.Invert) // Invert
        {
#if !defined(HAS_DISPLAY_READ)
            WriteLn("#### This display is write-only ####");
            Die(0x0A);
#else            
            SPI.BeginTransaction(DeviceDriver.spiController);
            MCU.DigitalWrite(DeviceDriver.csPin, false);
            setAddrWindowSPI(x, y, x, y, false);
            rgb565 = readColorSPI();
            MCU.DigitalWrite(DeviceDriver.csPin, true);
            SPI.EndTransaction(DeviceDriver.spiController);
            rgb565 = ~rgb565;
#endif
        }
        else
        {
            rgb565 = convertToRGB565(colour);
        }
        SPI.BeginTransaction(DeviceDriver.spiController);
        MCU.DigitalWrite(DeviceDriver.csPin, false);
        setAddrWindowSPI(x, y, x, y, true);
        SPI.WriteWord(DeviceDriver.spiController, rgb565);
        MCU.DigitalWrite(DeviceDriver.csPin, true);
        SPI.EndTransaction(DeviceDriver.spiController);
        
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
    
    metaHorizontalLine(int x1, int y, int x2, uint colour)
    {
        int w2 = (x2-x1+1) * 2;
        if (colour == Colour.Invert)
        {
#if !defined(HAS_DISPLAY_READ)
            WriteLn("#### This display is write-only ####");
            Die(0x0A);
#else
            SPI.BeginTransaction(DeviceDriver.spiController);
            MCU.DigitalWrite(DeviceDriver.csPin, false);
            setAddrWindowSPI(x1, y, x2, y, false);
            SPI.ReadBuffer(DeviceDriver.spiController, frameBuffer, 0, uint(w2*2));
            MCU.DigitalWrite(DeviceDriver.csPin, true);
            SPI.EndTransaction(DeviceDriver.spiController); 
            int s = 0;
            for (int d=0; d < w2; d += 2)
            {
                byte r = frameBuffer[s+1];
                byte g = frameBuffer[s+2];
                byte b = frameBuffer[s+3];
                uint rgb565 = ~(((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3));
                byte lsb = byte(rgb565 & 0xFF);
                byte msb = byte(rgb565 >> 8);
                frameBuffer[d+1] = lsb;
                frameBuffer[d+0] = msb;
                s += 4;
            }
            SPI.BeginTransaction(DeviceDriver.spiController);
            MCU.DigitalWrite(DeviceDriver.csPin, false);
            setAddrWindowSPI(x1, y, x2, y, true);
            SPI.WriteBuffer(DeviceDriver.spiController, frameBuffer, 0, uint(w2));
            MCU.DigitalWrite(DeviceDriver.csPin, true);
            SPI.EndTransaction(DeviceDriver.spiController);
#endif
        }
        else
        {
            uint rgb565 = convertToRGB565(colour);
            
            SPI.BeginTransaction(DeviceDriver.spiController);
            MCU.DigitalWrite(DeviceDriver.csPin, false);
            setAddrWindowSPI(x1, y, x2, y, true);
            for (int i=0; i < w2; i += 2)
            {
                SPI.WriteWord(DeviceDriver.spiController, rgb565);
            }
            MCU.DigitalWrite(DeviceDriver.csPin, true);
            SPI.EndTransaction(DeviceDriver.spiController);
        }
    }
    
    metaVerticalLine(int x, int y1, int y2, uint colour)
    {
        if (colour == Colour.Invert)
        {
            for (int y = y1; y <= y2; y++)
            {
                setPixel(x, y, colour);
            }
        }
        else
        {
            uint rgb565 = convertToRGB565(colour);
            int h2 = (y2-y1+1) * 2;
            
            SPI.BeginTransaction(DeviceDriver.spiController);
            MCU.DigitalWrite(DeviceDriver.csPin, false);
            setAddrWindowSPI(x, y1, x, y2, true);
            for (int i=0; i < h2; i += 2)
            {
                SPI.WriteWord(DeviceDriver.spiController, rgb565);
            }
            MCU.DigitalWrite(DeviceDriver.csPin, true);
            SPI.EndTransaction(DeviceDriver.spiController);
        }
    }
}
