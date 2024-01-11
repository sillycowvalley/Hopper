unit DisplayDriver
{
    #define DISPLAYDRIVER
    #define SPISCREENDRIVER
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Display"
    
    const int pixelWidth  = 128;
    const int pixelHeight = 128;
    
    // https://www.waveshare.com/wiki/Pico-LCD-1.44
    const byte dcPin   = 8;
    const byte csPin   = 9;
    const byte clkPin  = 10;
    const byte txPin   = 11;
    const byte rstPin  = 12;
    const byte blPin   = 13;
    
    const byte key0Pin = 15;
    const byte key1Pin = 17;
    const byte key2Pin = 2;
    const byte key3Pin = 3;
    
    const byte spiController = 1;
    
    const int xFudge = 2;
    const int yFudge = 1;
    
    const byte TFTSLPOUT     = 0x11; //  Sleep Out
    
    const byte TFTCASET      = 0x2A;
    const byte TFTPASET      = 0x2B;
    const byte TFTRAMWR      = 0x2C;
    const byte TFTRAMRD      = 0x2E;
    
    const byte TFTMADCTL     = 0x36;
    
    const byte MADCTLMY      = 0x80; // Bottom to top
    const byte MADCTLMX      = 0x40; // Right to left
    const byte MADCTLMV      = 0x20; // Reverse Mode
    const byte MADCTLBGR     = 0x08; // Blue-Green-Red pixel order
    const byte MADCTLRGB     = 0x00;
    
    const byte ST7735SWRESET = 0x01;
    const byte ST7735NORON   = 0x13;
    
    const byte ST7735INVOFF  = 0x20;
    const byte ST7735INVON   = 0x21;
    const byte ST7735DISPON  = 0x29;
    
    const byte ST7735COLMOD  = 0x3A;
    
    const byte ST7735FRMCTR1 = 0xB1;
    const byte ST7735FRMCTR2 = 0xB2;
    const byte ST7735FRMCTR3 = 0xB3;
    const byte ST7735INVCTR  = 0xB4;
    
    const byte ST7735PWCTR1  = 0xC0;
    const byte ST7735PWCTR2  = 0xC1;
    const byte ST7735PWCTR3  = 0xC2;
    const byte ST7735PWCTR4  = 0xC3;
    const byte ST7735PWCTR5  = 0xC4;
    const byte ST7735VMCTR1  = 0xC5;
    
    const byte ST7735GMCTRP1 = 0xE0;
    const byte ST7735GMCTRN1 = 0xE1;
    
    const string initCmdConst =
    {
      //  (COMMAND_BYTE), n, data_bytes....
      TFTSLPOUT    , 0x80,     // Sleep exit
      ST7735FRMCTR1, 3      ,  //  3: Frame rate ctrl - normal mode, 3 args:
        0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
      ST7735FRMCTR2, 3      ,  //  4: Frame rate control - idle mode, 3 args:
        0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
      ST7735FRMCTR3, 6      ,  //  5: Frame rate ctrl - partial mode, 6 args:
        0x01, 0x2C, 0x2D,       //     Dot inversion mode
        0x01, 0x2C, 0x2D,       //     Line inversion mode
      ST7735INVCTR , 1      ,  //  6: Display inversion ctrl, 1 arg, no delay:
        0x07,                   //     No inversion
      ST7735PWCTR2 , 1      ,  //  8: Power control, 1 arg, no delay:
        0xC5,                   //     VGH25 = 2.4C VGSEL = -10 VGH = 3 * AVDD
      ST7735PWCTR3 , 2      ,  //  9: Power control, 2 args, no delay:
        0x0A,                   //     Opamp current small
        0x00,                   //     Boost frequency
      ST7735VMCTR1 , 1      ,  // 12: Power control, 1 arg, no delay:
        0x0E,
      
      // Pico-LCD-1.44:
      ST7735INVOFF , 0      ,  // 13: Don't invert display, no args, no delay
      TFTMADCTL      , 1, (   MADCTLBGR),              // Memory Access Control
    
      ST7735COLMOD , 1      ,  // 15: set color mode, 1 arg, no delay:
        0x05,
      ST7735GMCTRP1, 16      , 0x02, 0x1c, 0x07, 0x12, 0x37, 0x32, 0x29, 0x2d, 0x29, 0x25, 0x2B, 0x39, 0x00, 0x01, 0x03, 0x10, // Set Gamma
      ST7735GMCTRN1, 16      , 0x03, 0x1d, 0x07, 0x06, 0x2E, 0x2C, 0x29, 0x2D, 0x2E, 0x2E, 0x37, 0x3F, 0x00, 0x00, 0x02, 0x10, // Set Gamma
      ST7735NORON  ,    0x80, //  3: Normal display on, no args, w/delay
      ST7735DISPON ,    0x80, //  4: Main screen turn on, no args w/delay
      0x00                        // End of list
    };
    
    byte[pixelWidth*4] frameBuffer; // 4 bytes per pixel (for reading)
    
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
            if ((rColor & 0x01) != 0)
            {
                c565 |= 0x0800;
            }
            if ((gColor & 0x01) != 0)
            {
                c565 |= 0x0060;
            }
            if ((bColor & 0x01) != 0)
            {
                c565 |= 0x0001;
            }
            last565 = c565;
            last444 = rgb444;
        }
        return last565;
    }
    uint readColorSPI()
    {
        byte r;
        byte g;
        byte b;
        
        r = SPI.ReadByte(spiController);
        r = SPI.ReadByte(spiController);
        g = SPI.ReadByte(spiController);
        b = SPI.ReadByte(spiController);
    
        return ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
    }
    setAddrWindowSPI(int x1, int y1, int x2, int y2, bool write)
    {
        x1 += xFudge;
        x2 += xFudge;
        y1 += yFudge;
        y2 += yFudge;
        
        // Column address set
        MCU.DigitalWrite(dcPin, false);
        SPI.WriteByte(spiController, TFTCASET);
        MCU.DigitalWrite(dcPin, true);
        SPI.WriteWord(spiController, uint(x1));
        SPI.WriteWord(spiController, uint(x2));
        
        // Row address set
        MCU.DigitalWrite(dcPin, false);
        SPI.WriteByte(spiController, TFTPASET);
        MCU.DigitalWrite(dcPin, true);
        SPI.WriteWord(spiController, uint(y1));
        SPI.WriteWord(spiController, uint(y2));
        
        // Write or Read RAM
        MCU.DigitalWrite(dcPin, false);
        SPI.WriteByte(spiController, write ? TFTRAMWR : TFTRAMRD);
        MCU.DigitalWrite(dcPin, true);
    }
    
    bool Visible
    {
        set
        {
            MCU.DigitalWrite(blPin, value); // backlight on|off ?
        }
    }
    ClearDisplay(uint colour)
    {
        int  pw1 = pixelWidth-1;
        uint pw2 = pixelWidth * 2;
        
        uint rgb565 = convertToRGB565(colour);
        byte lsb = byte(rgb565 & 0xFF);
        byte msb = byte(rgb565 >> 8);
        for (uint i=0; i < pw2; i += 2)
        {
            frameBuffer[i]   = lsb;
            frameBuffer[i+1] = msb;
        }
        for (int y = 0; y < pixelHeight; y++)
        {
            SPI.BeginTransaction(spiController);
            MCU.DigitalWrite(csPin, false);
            setAddrWindowSPI(0, y, pw1, y, true);
            SPI.WriteBuffer(spiController, frameBuffer, 0, pw2);
            MCU.DigitalWrite(csPin, true);
            SPI.EndTransaction(spiController);
        }
    }
    UpdateDisplay()
    {
        // NOP for TFT
    }
    ScrollUpDisplay(uint lines)
    {
        // TODO
    }
    
    sendCommandSPI(byte commandByte, string dataBytes, uint startIndex, byte numDataBytes)
    {
        SPI.BeginTransaction(spiController);
        MCU.DigitalWrite(csPin, false);
    
        MCU.DigitalWrite(dcPin, false); // Command mode
        SPI.WriteByte(spiController, commandByte);     // Send the command byte
    
        MCU.DigitalWrite(dcPin, true);
        for (uint i = 0; i < numDataBytes; i++)
        {
            SPI.WriteByte(spiController, byte(dataBytes[startIndex+i])); // Send the data bytes
        }
    
        MCU.DigitalWrite(csPin, true);
        SPI.EndTransaction(spiController);
    }
    
    bool Begin()
    {
        bool success = false;
        loop
        {
            Display.Reset();
            uint pw4 = pixelWidth*4;
            for (uint i = 0; i < pw4; i++)
            {
                frameBuffer[i] = 0;
            }
            
            SPI.SetCSPin(spiController,  csPin);
            SPI.SetTxPin(spiController,  txPin);
            SPI.SetClkPin(spiController, clkPin);
            
            MCU.PinMode(blPin, PinModeOption.Output);
            MCU.DigitalWrite(blPin, true);
            
            MCU.PinMode(csPin, PinModeOption.Output);
            MCU.DigitalWrite(csPin, true); // Deselect
            MCU.PinMode(dcPin, PinModeOption.Output);
            MCU.DigitalWrite(dcPin, true); // Data mode
            
            //SPI.Settings(spiController, 4000000, DataOrder.MSBFirst, DataMode.Mode0);// these are the defaults
            
            if (!SPI.Begin(spiController))
            {
                IO.WriteLn("DisplayDriver.Begin failed");
                break;
            }
            
            // hardware reset
            MCU.PinMode(rstPin, PinModeOption.Output);
            MCU.DigitalWrite(rstPin, true);
            Time.Delay(100);
            MCU.DigitalWrite(rstPin, false);
            Time.Delay(100);
            MCU.DigitalWrite(rstPin, true);
            Time.Delay(200);
            
            string initCmd = initCmdConst;
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
    RawPixel(int x, int y, uint colour)
    {
        uint rgb565;
        if (colour == 0xF000) // Invert
        {
            SPI.BeginTransaction(spiController);
            MCU.DigitalWrite(csPin, false);
            setAddrWindowSPI(x, y, x, y, false);
            rgb565 = readColorSPI();
            MCU.DigitalWrite(csPin, true);
            SPI.EndTransaction(spiController);
        
            rgb565 = ~rgb565;
        }
        else
        {
            rgb565 = convertToRGB565(colour);
        }
        SPI.BeginTransaction(spiController);
        MCU.DigitalWrite(csPin, false);
        setAddrWindowSPI(x, y, x, y, true);
        SPI.WriteWord(spiController, rgb565);
        MCU.DigitalWrite(csPin, true);
        SPI.EndTransaction(spiController);
        
    }
    SetPixel(int x, int y, uint colour)
    {
        if ((x < 0) || (y < 0) || (x >= pixelWidth) || (y >= pixelHeight)) { return; }
        RawPixel(x, y, colour);
    }
    
    rawHorizontalLine(int x1, int y, int x2, uint colour)
    {
        if (colour == 0xF000)
        {
            for (int x = x1; x <= x2; x++)
            {
                RawPixel(x, y, colour);
            }
        }
        else
        {
            uint rgb565 = convertToRGB565(colour);
            byte lsb = byte(rgb565 & 0xFF);
            byte msb = byte(rgb565 >> 8);
            int w2 = (x2-x1+1) * 2;
            for (int i=0; i < w2; i += 2)
            {
                frameBuffer[i]   = lsb;
                frameBuffer[i+1] = msb;
            }
            SPI.BeginTransaction(spiController);
            MCU.DigitalWrite(csPin, false);
            setAddrWindowSPI(x1, y, x2, y, true);
            SPI.WriteBuffer(spiController, frameBuffer, 0, uint(w2));
            MCU.DigitalWrite(csPin, true);
            SPI.EndTransaction(spiController);
        }
    }
    rawVerticalLine(int x, int y1, int y2, uint colour)
    {
        if (colour == 0xF000)
        {
            for (int y = y1; y <= y2; y++)
            {
                RawPixel(x, y, colour);
            }
        }
        else
        {
            uint rgb565 = convertToRGB565(colour);
            byte lsb = byte(rgb565 & 0xFF);
            byte msb = byte(rgb565 >> 8);
            int h2 = (y2-y1+1) * 2;
            for (uint i=0; i < h2; i += 2)
            {
                frameBuffer[i]   = lsb;
                frameBuffer[i+1] = msb;
            }
            SPI.BeginTransaction(spiController);
            MCU.DigitalWrite(csPin, false);
            setAddrWindowSPI(x, y1, x, y2, true);
            SPI.WriteBuffer(spiController, frameBuffer, 0, uint(h2));
            MCU.DigitalWrite(csPin, true);
            SPI.EndTransaction(spiController);
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
        if (y > ymax) { return; }
        
        int xmax = pixelWidth-1;
        if (x1 > xmax) { return; }
        
        if (x1 < 0) { x1 = 0; }
        if (x2 >= xmax) { x2 = xmax; }
        rawHorizontalLine(x1, y, x1, colour);
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
        if (y1 > ymax) { return; }
        
        int xmax = pixelWidth-1;
        if (x > xmax) { return; }
        
        if (y1 < 0) { y1 = 0; }
        if (y2 >= ymax) { y2 = ymax; }
        rawVerticalLine(x, y1, y2, colour);
    }
    
}
