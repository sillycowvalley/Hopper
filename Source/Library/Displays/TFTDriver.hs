unit DisplayDriver
{
    #define DISPLAY_DRIVER
    #define NO_SUSPEND_RESUME
    #define HAS_FAST_FILLEDRECTANGLE
    #define DISPLAY_IS_RGB565
    
#ifdef MINIMAL_RUNTIME
    uses "/Source/Minimal/MCU"
#else    
  #ifdef SBC_BOARD_DEFINED
    uses "/Source/Library/SBC"
  #else
    uses "/Source/Library/MCU"
  #endif
#endif
    
    uses "/Source/Library/Display"
    
    friend Display;
    
    const byte TFT_SWRESET    = 0x01;
    
    const byte TFT_SLPOUT     = 0x11; //  Sleep Out
    const byte TFT_NORON      = 0x13;
    
    const byte TFT_INVOFF     = 0x20;
    const byte TFT_INVON      = 0x21;
    const byte TFT_GAMMASET   = 0x26;
    const byte TFT_DISPON     = 0x29;
    
    const byte TFT_CASET      = 0x2A;
    const byte TFT_PASET      = 0x2B;
    const byte TFT_RAMWR      = 0x2C;
    const byte TFT_RAMRD      = 0x2E;
    
    const byte TFT_VSCRSADD   = 0x37; // Vertical Scrolling Start Address
    
    const byte TFT_MADCTL     = 0x36;
    const byte TFT_PIXFMT     = 0x3A; // also known as COLMOD
    const byte TFT_RDPIXFMT   = 0x0C;
    
    const byte TFT_FRMCTR1 = 0xB1;
    const byte TFT_FRMCTR2 = 0xB2;
    const byte TFT_FRMCTR3 = 0xB3;
    const byte TFT_INVCTR  = 0xB4;
    
    const byte TFT_DFUNCTR  = 0xB6 ;
    
    const byte TFT_PWCTR1   = 0xC0;
    const byte TFT_PWCTR2   = 0xC1;
    const byte TFT_PWCTR3   = 0xC2;
    const byte TFT_PWCTR4   = 0xC3;
    const byte TFT_PWCTR5   = 0xC4;
    
    const byte TFT_VMCTR1   = 0xC5;
    const byte TFT_VMCTR2   = 0xC7;
    
    const byte TFT_GMCTRP1  = 0xE0;
    const byte TFT_GMCTRN1  = 0xE1;
    
    const byte MADCTL_MY     = 0x80; // Bottom to top
    const byte MADCTL_MX     = 0x40; // Right to left
    const byte MADCTL_MV     = 0x20; // Reverse Mode
    const byte MADCTL_ML     = 0x10;
    const byte MADCTL_BGR    = 0x08; // Blue-Green-Red pixel order
    const byte MADCTL_RGB    = 0x00;
    
#if defined(ILI9341_CONTROLLER)
    
    const byte[] initCommand =
    {
        TFT_SLPOUT    , 0x80,  // Sleep exit
        /*
        0xEF, 3, 0x03, 0x80, 0x02,
        0xCF, 3, 0x00, 0xC1, 0x30,
        0xED, 4, 0x64, 0x03, 0x12, 0x81,
        0xE8, 3, 0x85, 0x00, 0x78,
        0xCB, 5, 0x39, 0x2C, 0x00, 0x34, 0x02,
        0xF7, 1, 0x20,
        0xEA, 2, 0x00, 0x00,
        */
        TFT_PWCTR1  , 1, 0x23,             // Power control VRH[5:0]
        TFT_PWCTR2  , 1, 0x10,             // Power control SAP[2:0];BT[3:0]
        TFT_VMCTR1  , 2, 0x3e, 0x28,       // VCM control
        TFT_VMCTR2  , 1, 0x86,             // VCM control2
        TFT_MADCTL      , 1, (MADCTL_MY | MADCTL_BGR),       // Memory Access Control
        TFT_VSCRSADD, 1, 0x00,             // Vertical scroll zero
        TFT_PIXFMT  , 1, 0x55,
        TFT_RDPIXFMT, 1, 0x55,
        TFT_FRMCTR1 , 2,                   // Frame rate ctrl:
            0x00,                          
            0x18,                          //     0x18 79Hz, 0x1B default 70Hz, 0x13 100Hz
        TFT_DFUNCTR , 3, 0x08, 0x82, 0x27, // Display Function Control
        
        //0xF2, 1, 0x00,                         // 3Gamma Function Disable
        
        TFT_GAMMASET , 1, 0x01,             // Gamma curve selected
        TFT_GMCTRP1 , 15, 0x0F, 0x31, 0x2B, 0x0C, 0x0E, 0x08, 0x4E, 0xF1, 0x37, 0x07, 0x10, 0x03, 0x0E, 0x09, 0x00, // Set Gamma
        TFT_GMCTRN1 , 15, 0x00, 0x0E, 0x14, 0x03, 0x11, 0x07, 0x31, 0xC1, 0x48, 0x08, 0x0F, 0x0C, 0x31, 0x36, 0x0F, // Set Gamma
        //TFT_SLPOUT  , 0x80,                // Exit Sleep
        TFT_DISPON  , 0x80,                // Display on
        0x00                                   // End of list
    };
#endif    

#if defined(ST77XX_CONTROLLER)

    // ST7735 and ST7789 support rgb444?
    
    const byte[] initCommand =
    {
      TFT_SLPOUT    , 0x80,  // Sleep exit
      TFT_FRMCTR1, 3      ,  // Frame rate ctrl - normal mode, 3 args:
        0x01, 0x2C, 0x2D,    //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
      TFT_FRMCTR2, 3      ,  //  4: Frame rate control - idle mode, 3 args:
        0x01, 0x2C, 0x2D,    //     Rate = fosc/(1x2+40) * (LINE+2C+2D)
      TFT_FRMCTR3, 6      ,  //  5: Frame rate ctrl - partial mode, 6 args:
        0x01, 0x2C, 0x2D,    //     Dot inversion mode
        0x01, 0x2C, 0x2D,    //     Line inversion mode
      TFT_INVCTR , 1      ,  //  6: Display inversion ctrl, 1 arg, no delay:
        0x07,                //     No inversion
      TFT_PWCTR2 , 1      ,  //  8: Power control, 1 arg, no delay:
        0xC5,                //     VGH25 = 2.4C VGSEL = -10 VGH = 3 * AVDD
      TFT_PWCTR3 , 2      ,  //  9: Power control, 2 args, no delay:
        0x0A,                //     Opamp current small
        0x00,                //     Boost frequency
      TFT_VMCTR1 , 1      ,  // 12: Power control, 1 arg, no delay:
        0x0E,

      TFT_INVON , 0      ,   // 13: 
      TFT_MADCTL      , 1,      MADCTL_BGR,              // Memory Access Control
      
      TFT_PIXFMT , 1      ,  // 15: set colour mode, 1 arg, no delay:
        0x05,                // 0x03 : 12 bit 444, 0x05 : 16 bit 565, 0x06 : 18 bit 666
      TFT_GMCTRP1, 16      , 0x02, 0x1c, 0x07, 0x12, 0x37, 0x32, 0x29, 0x2d, 0x29, 0x25, 0x2B, 0x39, 0x00, 0x01, 0x03, 0x10, // Set Gamma
      TFT_GMCTRN1, 16      , 0x03, 0x1d, 0x07, 0x06, 0x2E, 0x2C, 0x29, 0x2D, 0x2E, 0x2E, 0x37, 0x3F, 0x00, 0x00, 0x02, 0x10, // Set Gamma
      TFT_NORON  ,    0x80, //  3: Normal display on, no args, w/delay
      TFT_DISPON ,    0x80, //  4: Main screen turn on, no args w/delay
       0x00                        // End of list
    };
#endif

#if defined(ST7796_CONTROLLER)

    // https://github.com/Bodmer/TFT_eSPI/blob/master/TFT_Drivers/ST7796_Init.h    
    const byte[] initCommand =
    {
        TFT_SWRESET   , 0x80,       // Software Reset
        TFT_SLPOUT    , 0x80,       // Sleep exit
        TFT_MADCTL , 1, MADCTL_RGB, // Memory Access Control: doesn't matter, substituted later
        TFT_PIXFMT , 1, 0x55,       // Control interface color format set to 16
        TFT_INVCTR , 1, 1,          // Column inversion : 1-dot inversion
        /*
        TFT_DFUNCTR , 3,            // Display Function Control:
            0x80,                   //     Bypass
            0x02,                   //     Source Output Scan from S1 to S960, Gate Output scan from G1 to G480, scan cycle=2 
            0x3B,                   //     LCD Drive Line=8*(59+1)
        */
        /*
        0xE8, 8,                    // Display Output Ctrl Adjust:
            0x40, 
            0x8A, 
            0x00, 
            0x00, 
            0x29,                   //     Source equalizing period time= 22.5 us
            0x19,                   //     Timing for "Gate start"=25 (Tclk)
            0xA5,                   //     Timing for "Gate End"=37 (Tclk), Gate driver EQ function ON
            0x33, 
        */
        TFT_PWCTR2, 1, 0x06,        // VAP(GVDD)=3.85+( vcom+vcom offset), VAN(GVCL)=-3.85+( vcom+vcom offset)
        TFT_PWCTR3, 1, 0xA7,        // Source driving current level=low, Gamma driving current level=High
        TFT_VMCTR1, (0x80|1), 0x18, // VCOM=0.9
         
        TFT_GMCTRP1, 14, 0xF0, 0x09, 0x0B, 0x06, 0x04, 0x15, 0x2F, 0x54, 0x42, 0x3C, 0x17, 0x14, 0x18, 0x1B,          // Gamma "+"
        TFT_GMCTRN1, (0x80 | 14), 0xE0, 0x09, 0x0B, 0x06, 0x04, 0x03, 0x2B, 0x43, 0x42, 0x3B, 0x16, 0x14, 0x17, 0x1B, // Gamma "-"
        
        TFT_DISPON ,    0x80,       // Display On
        0x00                        // End of list
    };
#endif

    
    bool flipX;
    bool flipY;
    bool isPortrait = false;
    bool FlipX { get { return flipX; } set { flipX = value; }}
    bool FlipY { get { return flipY; } set { flipY = value; }}
    bool IsPortrait { get { return isPortrait; } set { isPortrait = value; } }
    
#ifdef HAS_DISPLAY_READ
    byte[DeviceDriver.pw*4] frameBuffer; // 4 bytes per pixel (for reading)
#endif
    
#ifdef BUFFER_TEXT
    // based on our largest supported resolution / our smallest supported font: 
    //              480x320                      / (5+1)x(7+1)
    //              153600                       / 48
    //              3200 cells
    //              x3 uints per cell = 9600 = 19200 bytes!
    const uint textBufferSize = 3200*3; 
    uint[textBufferSize] textBuffer;
    
    bufferText(byte col, byte row, char c, uint foreColour, uint backColour)
    {
        uint index = (row * Display.Columns + col) * 3;
        textBuffer[index]   = uint(c);
        textBuffer[index+1] = foreColour;
        textBuffer[index+2] = backColour;
    }
#endif

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
            byte madArgument = DeviceDriver.getMAD();
            
#ifdef BUFFER_TEXT            
            for (uint index = 0; index < textBufferSize; index += 3)
            {
                textBuffer[index] = uint(' ');
                textBuffer[index+1] = Display.BackColour;
                textBuffer[index+2] = Display.BackColour;
            }
#endif
            
            Display.Reset();
            //for (uint i = 0; i < bufferSize; i++)
            //{
            //    frameBuffer[i] = 0;
            //}
            
            SPI.SetCSPin(DeviceDriver.spiController,  DeviceDriver.csPin);
            SPI.SetTxPin(DeviceDriver.spiController,  DeviceDriver.txPin);
#ifdef HAS_DISPLAY_READ
            SPI.SetRxPin(DeviceDriver.spiController,  DeviceDriver.rxPin);
#endif
            SPI.SetClkPin(DeviceDriver.spiController, DeviceDriver.clkPin);
            
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.PinMode(DeviceDriver.csPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.csPin, true); // Deselect
#endif
            MCU.PinMode(DeviceDriver.dcPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.dcPin, true); // Data mode
            
            if (!SPI.Begin(DeviceDriver.spiController))
            {
                
                IO.WriteLn("DeviceDriver.Begin failed in SPI.Begin");
                break;
            }
            byte[] commandLocal = initCommand;

            if (DeviceDriver.rstPin != -1)
            {
                // hardware reset
                MCU.PinMode(byte(DeviceDriver.rstPin), PinModeOption.Output);
                MCU.DigitalWrite(byte(DeviceDriver.rstPin), true);
                Time.Delay(100);
                MCU.DigitalWrite(byte(DeviceDriver.rstPin), false);
                Time.Delay(100);
                MCU.DigitalWrite(byte(DeviceDriver.rstPin), true);
                Time.Delay(200);
            }
            else
            {
                sendCommandSPI(TFT_SWRESET, commandLocal, 0, 0); // dummy argument (no arguments for TFT_SWRESET)
                Time.Delay(150);
            }

            uint i = 0;
            loop
            {
                byte cmd = byte(commandLocal[i]);
                if (cmd == 0) { break; } 
                i++;
                byte x = byte(commandLocal[i]);
                i++;
                byte numArgs = x & 0x7F;
                if (cmd == TFT_MADCTL)
                {
                    commandLocal[i] =  madArgument;
                }
#ifdef SET_TFT_INVOFF                
                if (cmd == TFT_INVON)
                {
                    cmd =  TFT_INVOFF;
                }
#endif
                sendCommandSPI(cmd, commandLocal, i, numArgs);
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

    uint last444 = 0;
    uint last565 = 0;
    uint convertToRGB565(uint rgb444)
    {
        if (rgb444 == 0)
        {
            return 0;
        }
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
        if (rgb565 == 0)
        {
            return 0;
        }
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
    uint readColour565()
    {
        byte a;
        byte r;
        byte g;
        byte b;
     
        a = SPI.ReadByte(DeviceDriver.spiController); // dummy read
        r = SPI.ReadByte(DeviceDriver.spiController); // 0..0xFC
        g = SPI.ReadByte(DeviceDriver.spiController); // 0..0xFC
        b = SPI.ReadByte(DeviceDriver.spiController); // 0..0xFC
        
        return ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
    }
    uint readColour444()
    {
        byte a;
        byte r;
        byte g;
        byte b;
     
        a = SPI.ReadByte(DeviceDriver.spiController); // dummy read
        r = SPI.ReadByte(DeviceDriver.spiController); // 0..0xFC
        g = SPI.ReadByte(DeviceDriver.spiController); // 0..0xFC
        b = SPI.ReadByte(DeviceDriver.spiController); // 0..0xFC
        
        return ((r & 0xF0) << 4) | (g & 0xF0) | (b  >> 4);    
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
    setPixelAddrWindowSPI(int x, int y)
    {
        x += DeviceDriver.xFudge;
        y += DeviceDriver.yFudge;
        
        // Column address set
        MCU.DigitalWrite(DeviceDriver.dcPin, false);
        SPI.WriteByte(DeviceDriver.spiController, TFT_CASET);
        MCU.DigitalWrite(DeviceDriver.dcPin, true);
        SPI.WriteWords(DeviceDriver.spiController, uint(x), 2);
        
        // Row address set
        MCU.DigitalWrite(DeviceDriver.dcPin, false);
        SPI.WriteByte(DeviceDriver.spiController, TFT_PASET);
        MCU.DigitalWrite(DeviceDriver.dcPin, true);
        SPI.WriteWords(DeviceDriver.spiController, uint(y), 2);
        
        // Write RAM
        MCU.DigitalWrite(DeviceDriver.dcPin, false);
        SPI.WriteByte(DeviceDriver.spiController, TFT_RAMWR);
        MCU.DigitalWrite(DeviceDriver.dcPin, true);
    }
    getPixelAddrWindowSPI(int x, int y)
    {
        x += DeviceDriver.xFudge;
        y += DeviceDriver.yFudge;
        
        // Column address set
        MCU.DigitalWrite(DeviceDriver.dcPin, false);
        SPI.WriteByte(DeviceDriver.spiController, TFT_CASET);
        MCU.DigitalWrite(DeviceDriver.dcPin, true);
        SPI.WriteWords(DeviceDriver.spiController, uint(x), 2);
        
        // Row address set
        MCU.DigitalWrite(DeviceDriver.dcPin, false);
        SPI.WriteByte(DeviceDriver.spiController, TFT_PASET);
        MCU.DigitalWrite(DeviceDriver.dcPin, true);
        SPI.WriteWords(DeviceDriver.spiController, uint(y), 2);
        
        // Write RAM
        MCU.DigitalWrite(DeviceDriver.dcPin, false);
        SPI.WriteByte(DeviceDriver.spiController, TFT_RAMRD);
        MCU.DigitalWrite(DeviceDriver.dcPin, true);
    }
    
    bool visible
    {
        set
        {
            // backlight on|off ?
        }
    }
    clear(uint colour)
    {
        if (colour == Colour.Invert)
        {
            for (int y = 0; y < Display.PixelHeight; y++)
            {
                horizontalLine(0, y, Display.PixelWidth-1, colour);
            }
            return;
        }
        uint rgb565 = convertToRGB565(colour);
        for (int y = 0; y < Display.PixelHeight; y++)
        {
            SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
            setAddrWindowSPI(0, y, Display.PixelWidth-1, y, true);
            SPI.WriteWords(DeviceDriver.spiController, rgb565, uint(Display.PixelWidth));
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
            SPI.EndTransaction(DeviceDriver.spiController);
        }
#ifdef BUFFER_TEXT
        for (uint index = 0; index < textBufferSize; index += 3)
        {
            textBuffer[index] = uint(' ');
            textBuffer[index+1] = colour;
            textBuffer[index+2] = colour;
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
        uint is = Display.Columns*3;
        while (is < textBufferSize)
        {
            textBuffer[id]   = textBuffer[is];
            textBuffer[id+1] = textBuffer[is+1];
            textBuffer[id+2] = textBuffer[is+2];
            is += 3;
            id += 3;
        }
        while (id < textBufferSize)
        {
            textBuffer[id] = uint(' ');
            textBuffer[id+1] = Display.BackColour;
            textBuffer[id+1] = Display.BackColour;
            id += 3;
        }
        
        // Display.Clear(Display.BackColour):
        uint rgb565 = convertToRGB565(Display.BackColour);
        for (int y = 0; y < Display.PixelHeight; y++)
        {
            SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
            setAddrWindowSPI(0, y, Display.PixelWidth-1, y, true);
            SPI.WriteWords(DeviceDriver.spiController, rgb565, uint(Display.PixelWidth));
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
            SPI.EndTransaction(DeviceDriver.spiController);
        }
        if (Display.FontDataSet)
        {
            Display.Suspend();
            byte rows = Display.Rows;
            byte cols = Display.Columns;
            uint index;
            char c;
            byte row;
            byte col;
            uint backColour;
            uint foreColour;
            for (row = 0; row < rows; row++)
            {
                for (col = 0; col < cols; col++)
                {
                    c = char(textBuffer[index]);
                    foreColour = textBuffer[index+1];
                    backColour = textBuffer[index+2];
                    index += 3;
                    
                    if ((c == ' ') && (backColour == Display.BackColour)) { continue; }
                    
                    Display.drawBufferChar(col, row, c, foreColour, backColour);
                }
            }
            Display.Resume();
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
        for (int row = int(lines); row < Display.PixelHeight; row++)
        {
            for (int x = 0; x < Display.PixelWidth; x++)
            {
                setPixel(x, drow, getPixel(x, row));
            }
            drow++;    
        }
        loop
        {
            if (drow >= Display.PixelHeight) { break; }
            horizontalLine(0, drow, Display.PixelWidth-1, Display.BackColour);
            drow++;   
        }
#endif
    }
    
    sendCommandSPI(byte commandByte, byte[] dataBytes, uint startIndex, byte numDataBytes)
    {
        SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
        MCU.DigitalWrite(DeviceDriver.dcPin, false); // Command mode
        SPI.WriteByte(DeviceDriver.spiController, commandByte);     // Send the command byte
    
        MCU.DigitalWrite(DeviceDriver.dcPin, true);
        for (uint i = 0; i < numDataBytes; i++)
        {
            SPI.WriteByte(DeviceDriver.spiController, byte(dataBytes[startIndex+i])); // Send the data bytes
        }
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
        SPI.EndTransaction(DeviceDriver.spiController);
    }
    
    
    uint getPixel(int vx, int vy)
    {
#if !defined(HAS_DISPLAY_READ)
        WriteLn("#### This display is write-only ####");
        Die(0x0A);
        return 0;
#else
        // https://forum.arduino.cc/t/ili9340-ili9340-how-to-read-pixel-color/293744/2
        
        SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
        getPixelAddrWindowSPI(vx, vy);
        uint rgb444 = readColour444();
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
        SPI.EndTransaction(DeviceDriver.spiController);
        return rgb444;
#endif
    }
    setPixel(int vx, int vy, uint colour)
    {
        if (colour == Colour.Invert) // Invert
        {
#if !defined(HAS_DISPLAY_READ)
            WriteLn("#### This display is write-only ####");
            Die(0x0A);
#else            
            SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
            getPixelAddrWindowSPI(vx, vy);
            colour = readColour565();
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
            SPI.EndTransaction(DeviceDriver.spiController);
            colour = ~colour;
#endif
        }
        else
        {
            colour = convertToRGB565(colour);
        }
        SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
        setPixelAddrWindowSPI(vx, vy);
        SPI.WriteWord(DeviceDriver.spiController, colour);
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
        SPI.EndTransaction(DeviceDriver.spiController);
    }
    
    setTextPixel(int x, int y, uint colour)
    {
        if ((x < 0) || (y < 0) || (x >= Display.PixelWidth) || (y >= Display.PixelHeight)) { return; }
        if (colour == Colour.Invert) // Invert
        {
#if !defined(HAS_DISPLAY_READ)
            WriteLn("#### This display is write-only ####");
            Die(0x0A);
#else            
            SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
            getPixelAddrWindowSPI(x, y);
            colour = readColour565();
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
            SPI.EndTransaction(DeviceDriver.spiController);
            colour = ~colour;
#endif
        }
        SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
        setPixelAddrWindowSPI(x, y);
        SPI.WriteWord(DeviceDriver.spiController, colour);
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
        SPI.EndTransaction(DeviceDriver.spiController);
    }
    setClippedTextPixel(int x, int y, uint colour)
    {
        if (colour == Colour.Invert) // Invert
        {
#if !defined(HAS_DISPLAY_READ)
            WriteLn("#### This display is write-only ####");
            Die(0x0A);
#else            
            SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
            getPixelAddrWindowSPI(x, y);
            colour = readColour565();
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
            SPI.EndTransaction(DeviceDriver.spiController);
            colour = ~colour;
#endif
        }
        SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
        setPixelAddrWindowSPI(x, y);
        SPI.WriteWord(DeviceDriver.spiController, colour);
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
        SPI.EndTransaction(DeviceDriver.spiController);
    }

#ifdef FONT_EXISTS    
    filledRectangle(int x0, int y0, int w, int h, uint colour)
    {
        SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
        setAddrWindowSPI(x0, y0, x0+w-1, y0+h-1, true);
        SPI.WriteWords(DeviceDriver.spiController, colour, uint(w*h)); 
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
        SPI.EndTransaction(DeviceDriver.spiController); 
    }
    filledRectangle(int x0, int y0, int w, int h, uint[] cellBuffer)
    {
        SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
        setAddrWindowSPI(x0, y0, x0+w-1, y0+h-1, true);
        SPI.WriteBuffer(DeviceDriver.spiController, cellBuffer, 0, uint(w*h)); 
#ifndef BOARD_HAS_HARDWARE_SPI
        MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
        SPI.EndTransaction(DeviceDriver.spiController); 
    }
#endif
       
    horizontalLine(int x1, int y, int x2, uint colour)
    {     
        if (colour == Colour.Invert)
        {
            for (int x = x1; x <= x2; x++)
            {
                setPixel(x, y, colour);
            }
            /*
            int w2 = (x2-x1+1) * 2;
            SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
            setAddrWindowSPI(x1, y, x2, y, false);
            SPI.ReadBuffer(DeviceDriver.spiController, frameBuffer, 0, uint(w2*2));
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
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
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
            setAddrWindowSPI(x1, y, x2, y, true);
            SPI.WriteBuffer(DeviceDriver.spiController, frameBuffer, 0, uint(w2));
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
            SPI.EndTransaction(DeviceDriver.spiController);
            */
        }
        else
        {
            SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
            setAddrWindowSPI(x1, y, x2, y, true);
            SPI.WriteWords(DeviceDriver.spiController, convertToRGB565(colour), uint(x2-x1+1));
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
            SPI.EndTransaction(DeviceDriver.spiController);
        }
    }
    
    verticalLine(int x, int y1, int y2, uint colour)
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
            SPI.BeginTransaction(DeviceDriver.spiController);
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, false);
#endif
            setAddrWindowSPI(x, y1, x, y2, true);
            SPI.WriteWords(DeviceDriver.spiController, convertToRGB565(colour), uint(y2-y1+1));
#ifndef BOARD_HAS_HARDWARE_SPI
            MCU.DigitalWrite(DeviceDriver.csPin, true);
#endif
            SPI.EndTransaction(DeviceDriver.spiController);
        }
    }
}

