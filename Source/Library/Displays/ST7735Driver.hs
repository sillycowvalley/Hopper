unit DisplayDriver
{
    #define NO_SUSPEND_RESUME
    #define DISPLAY_DRIVER
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Display"
    
    const byte TFT_SLPOUT     = 0x11; //  Sleep Out
    
    const byte TFT_CASET      = 0x2A;
    const byte TFT_PASET      = 0x2B;
    const byte TFT_RAMWR      = 0x2C;
    const byte TFT_RAMRD      = 0x2E;
    
    const byte TFT_MADCTL     = 0x36;
    
    const byte MADCTL_MY     = 0x80; // Bottom to top
    const byte MADCTL_MX     = 0x40; // Right to left
    const byte MADCTL_MV     = 0x20; // Reverse Mode
    const byte MADCTL_BGR    = 0x08; // Blue-Green-Red pixel order
    const byte MADCTL_RGB    = 0x00;
    
    const byte ST7735_SWRESET = 0x01;
    const byte ST7735_NORON   = 0x13;
    
    const byte ST7735_INVOFF  = 0x20;
    const byte ST7735_INVON   = 0x21;
    const byte ST7735_DISPON  = 0x29;
    
    const byte ST7735_COLMOD  = 0x3A;
    
    const byte ST7735_FRMCTR1 = 0xB1;
    const byte ST7735_FRMCTR2 = 0xB2;
    const byte ST7735_FRMCTR3 = 0xB3;
    const byte ST7735_INVCTR  = 0xB4;
    
    const byte ST7735_PWCTR1  = 0xC0;
    const byte ST7735_PWCTR2  = 0xC1;
    const byte ST7735_PWCTR3  = 0xC2;
    const byte ST7735_PWCTR4  = 0xC3;
    const byte ST7735_PWCTR5  = 0xC4;
    const byte ST7735_VMCTR1  = 0xC5;
    
    const byte ST7735_GMCTRP1 = 0xE0;
    const byte ST7735_GMCTRN1 = 0xE1;

#ifdef WAVESHARE_PICO_LCD_144 
    // Pico-LCD-1.44 - Don't invert display
    const byte INVERT = ST7735_INVOFF;
    const byte MADCTL = MADCTL_BGR;
#endif        
#ifdef WAVESHARE_PICO_LCD_096
    // Pico-LCD-0.96 - Invert display colours
    const byte INVERT = ST7735_INVON;
    const byte MADCTL = (  MADCTL_MY | MADCTL_MV | MADCTL_BGR);
#endif
#ifdef WAVESHARE_PICO_LCD_114
    // Pico-LCD-1.14 - Invert display colours
    const byte INVERT = ST7735_INVON;
    const byte MADCTL = ( MADCTL_MX | MADCTL_MV | MADCTL_RGB);
#endif
#ifdef WAVESHARE_RP2040_LCD_096
    // RP2040-LCD-0.96 - Invert display colours
    const byte INVERT = ST7735_INVON;
    const byte MADCTL = MADCTL_BGR;
#endif
    const byte[] initCmdConst =
    {
      //  (COMMAND_BYTE), n, data_bytes....
      TFT_SLPOUT    , 0x80,     // Sleep exit
      ST7735_FRMCTR1, 3      ,  //  3: Frame rate ctrl - normal mode, 3 args:
        0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
      ST7735_FRMCTR2, 3      ,  //  4: Frame rate control - idle mode, 3 args:
        0x01, 0x2C, 0x2D,       //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
      ST7735_FRMCTR3, 6      ,  //  5: Frame rate ctrl - partial mode, 6 args:
        0x01, 0x2C, 0x2D,       //     Dot inversion mode
        0x01, 0x2C, 0x2D,       //     Line inversion mode
      ST7735_INVCTR , 1      ,  //  6: Display inversion ctrl, 1 arg, no delay:
        0x07,                   //     No inversion
      ST7735_PWCTR2 , 1      ,  //  8: Power control, 1 arg, no delay:
        0xC5,                   //     VGH25 = 2.4C VGSEL = -10 VGH = 3 * AVDD
      ST7735_PWCTR3 , 2      ,  //  9: Power control, 2 args, no delay:
        0x0A,                   //     Opamp current small
        0x00,                   //     Boost frequency
      ST7735_VMCTR1 , 1      ,  // 12: Power control, 1 arg, no delay:
        0x0E,

// ---- Device specific options:
      INVERT , 0      ,         // 13: 
      TFT_MADCTL      , 1,      MADCTL,              // Memory Access Control
// ---- 
      
      ST7735_COLMOD , 1      ,  // 15: set color mode, 1 arg, no delay:
        0x05,
      ST7735_GMCTRP1, 16      , 0x02, 0x1c, 0x07, 0x12, 0x37, 0x32, 0x29, 0x2d, 0x29, 0x25, 0x2B, 0x39, 0x00, 0x01, 0x03, 0x10, // Set Gamma
      ST7735_GMCTRN1, 16      , 0x03, 0x1d, 0x07, 0x06, 0x2E, 0x2C, 0x29, 0x2D, 0x2E, 0x2E, 0x37, 0x3F, 0x00, 0x00, 0x02, 0x10, // Set Gamma
      ST7735_NORON  ,    0x80, //  3: Normal display on, no args, w/delay
      ST7735_DISPON ,    0x80, //  4: Main screen turn on, no args w/delay
       0x00                        // End of list
    };
    
    const uint bufferSize = DeviceDriver.PW*4;
    byte[bufferSize] frameBuffer; // 4 bytes per pixel (for reading)
    
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
        
        r = SPI.ReadByte(DeviceDriver.SPIController);
        r = SPI.ReadByte(DeviceDriver.SPIController);
        g = SPI.ReadByte(DeviceDriver.SPIController);
        b = SPI.ReadByte(DeviceDriver.SPIController);
    
        return ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
    }
    setAddrWindowSPI(int x1, int y1, int x2, int y2, bool write)
    {
        x1 += DeviceDriver.XFudge;
        x2 += DeviceDriver.XFudge;
        y1 += DeviceDriver.YFudge;
        y2 += DeviceDriver.YFudge;
        
        // Column address set
        MCU.DigitalWrite(DeviceDriver.DCPin, false);
        SPI.WriteByte(DeviceDriver.SPIController, TFT_CASET);
        MCU.DigitalWrite(DeviceDriver.DCPin, true);
        SPI.WriteWord(DeviceDriver.SPIController, uint(x1));
        SPI.WriteWord(DeviceDriver.SPIController, uint(x2));
        
        // Row address set
        MCU.DigitalWrite(DeviceDriver.DCPin, false);
        SPI.WriteByte(DeviceDriver.SPIController, TFT_PASET);
        MCU.DigitalWrite(DeviceDriver.DCPin, true);
        SPI.WriteWord(DeviceDriver.SPIController, uint(y1));
        SPI.WriteWord(DeviceDriver.SPIController, uint(y2));
        
        // Write or Read RAM
        MCU.DigitalWrite(DeviceDriver.DCPin, false);
        byte rw = write ? TFT_RAMWR : TFT_RAMRD;
        SPI.WriteByte(DeviceDriver.SPIController, rw);
        MCU.DigitalWrite(DeviceDriver.DCPin, true);
    }
    
    bool Visible
    {
        set
        {
            MCU.DigitalWrite(DeviceDriver.BlPin, value); // backlight on|off ?
        }
    }
    ClearDisplay(uint colour)
    {
        int  pw1 = Display.PixelWidth-1;
        if (colour == Color.Invert)
        {
            for (int y = 0; y < Display.PixelHeight; y++)
            {
                RawHorizontalLine(0, y, pw1, colour);
            }
            return;
        }

        uint pw2 = uint(Display.PixelWidth) * 2;        
        uint rgb565 = convertToRGB565(colour);
        byte lsb = byte(rgb565 & 0xFF);
        byte msb = byte(rgb565 >> 8);
        for (uint i=0; i < pw2; i += 2)
        {
            frameBuffer[i+1] = lsb;
            frameBuffer[i+0] = msb;
        }
        for (int y = 0; y < Display.PixelHeight; y++)
        {
            SPI.BeginTransaction(DeviceDriver.SPIController);
            MCU.DigitalWrite(DeviceDriver.CSPin, false);
            setAddrWindowSPI(0, y, pw1, y, true);
            SPI.WriteBuffer(DeviceDriver.SPIController, frameBuffer, 0, pw2);
            MCU.DigitalWrite(DeviceDriver.CSPin, true);
            SPI.EndTransaction(DeviceDriver.SPIController);
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
    
    sendCommandSPI(byte commandByte, byte[] dataBytes, uint startIndex, byte numDataBytes)
    {
        SPI.BeginTransaction(DeviceDriver.SPIController);
        MCU.DigitalWrite(DeviceDriver.CSPin, false);
    
        MCU.DigitalWrite(DeviceDriver.DCPin, false); // Command mode
        SPI.WriteByte(DeviceDriver.SPIController, commandByte);     // Send the command byte
    
        MCU.DigitalWrite(DeviceDriver.DCPin, true);
        for (uint i = 0; i < numDataBytes; i++)
        {
            SPI.WriteByte(DeviceDriver.SPIController, byte(dataBytes[startIndex+i])); // Send the data bytes
        }
    
        MCU.DigitalWrite(DeviceDriver.CSPin, true);
        SPI.EndTransaction(DeviceDriver.SPIController);
    }
    
    bool Begin()
    {
        bool success = false;
        loop
        {
            Display.PixelWidth  = DeviceDriver.PW;
            Display.PixelHeight = DeviceDriver.PH;
            
            Display.Reset();
            for (uint i = 0; i < bufferSize; i++)
            {
                frameBuffer[i] = 0;
            }
            
            SPI.SetCSPin(DeviceDriver.SPIController,  DeviceDriver.CSPin);
            SPI.SetTxPin(DeviceDriver.SPIController,  DeviceDriver.TxPin);
            //SPI.SetRxPin(DeviceDriver.SPIController,  DeviceDriver.DCPin);
            SPI.SetClkPin(DeviceDriver.SPIController, DeviceDriver.ClkPin);
            
            MCU.PinMode(DeviceDriver.BlPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.BlPin, true);
            
            MCU.PinMode(DeviceDriver.CSPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.CSPin, true); // Deselect
            MCU.PinMode(DeviceDriver.DCPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.DCPin, true); // Data mode
            
            //SPI.Settings(DeviceDriver.SPIController, 4000000, DataOrder.MSBFirst, DataMode.Mode0);// these are the defaults
            
            if (!SPI.Begin(DeviceDriver.SPIController))
            {
                IO.WriteLn("DeviceDriver.Begin failed in SPI.Begin");
                break;
            }
            
            // hardware reset
            MCU.PinMode(DeviceDriver.RstPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.RstPin, true);
            Time.Delay(100);
            MCU.DigitalWrite(DeviceDriver.RstPin, false);
            Time.Delay(100);
            MCU.DigitalWrite(DeviceDriver.RstPin, true);
            Time.Delay(200);
            
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
    RawSetPixel(int x, int y, uint colour)
    {
        uint rgb565;
        if (colour == Color.Invert) // Invert
        {
            SPI.BeginTransaction(DeviceDriver.SPIController);
            MCU.DigitalWrite(DeviceDriver.CSPin, false);
            setAddrWindowSPI(x, y, x, y, false);
            rgb565 = readColorSPI();
            MCU.DigitalWrite(DeviceDriver.CSPin, true);
            SPI.EndTransaction(DeviceDriver.SPIController);
        
            rgb565 = ~rgb565;
        }
        else
        {
            rgb565 = convertToRGB565(colour);
        }
        SPI.BeginTransaction(DeviceDriver.SPIController);
        MCU.DigitalWrite(DeviceDriver.CSPin, false);
        setAddrWindowSPI(x, y, x, y, true);
        SPI.WriteWord(DeviceDriver.SPIController, rgb565);
        MCU.DigitalWrite(DeviceDriver.CSPin, true);
        SPI.EndTransaction(DeviceDriver.SPIController);
        
    }
    
    RawHorizontalLine(int x1, int y, int x2, uint colour)
    {
        int w2 = (x2-x1+1) * 2;
        if (colour == Color.Invert)
        {
            SPI.BeginTransaction(DeviceDriver.SPIController);
            MCU.DigitalWrite(DeviceDriver.CSPin, false);
            setAddrWindowSPI(x1, y, x2, y, false);
            SPI.ReadBuffer(DeviceDriver.SPIController, frameBuffer, 0, uint(w2*2));
            MCU.DigitalWrite(DeviceDriver.CSPin, true);
            SPI.EndTransaction(DeviceDriver.SPIController); 
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
        }
        else
        {
            uint rgb565 = convertToRGB565(colour);
            byte lsb = byte(rgb565 & 0xFF);
            byte msb = byte(rgb565 >> 8);
            for (int i=0; i < w2; i += 2)
            {
                frameBuffer[i+1] = lsb;
                frameBuffer[i+0] = msb;
            }
        }
        SPI.BeginTransaction(DeviceDriver.SPIController);
        MCU.DigitalWrite(DeviceDriver.CSPin, false);
        setAddrWindowSPI(x1, y, x2, y, true);
        SPI.WriteBuffer(DeviceDriver.SPIController, frameBuffer, 0, uint(w2));
        MCU.DigitalWrite(DeviceDriver.CSPin, true);
        SPI.EndTransaction(DeviceDriver.SPIController);
    }
    RawVerticalLine(int x, int y1, int y2, uint colour)
    {
        if (colour == Color.Invert)
        {
            for (int y = y1; y <= y2; y++)
            {
                RawSetPixel(x, y, colour);
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
                frameBuffer[i+1] = lsb;
                frameBuffer[i+0] = msb;
            }
            SPI.BeginTransaction(DeviceDriver.SPIController);
            MCU.DigitalWrite(DeviceDriver.CSPin, false);
            setAddrWindowSPI(x, y1, x, y2, true);
            SPI.WriteBuffer(DeviceDriver.SPIController, frameBuffer, 0, uint(h2));
            MCU.DigitalWrite(DeviceDriver.CSPin, true);
            SPI.EndTransaction(DeviceDriver.SPIController);
        }
    }
}
