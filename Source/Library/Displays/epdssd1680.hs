unit DisplayDriver
{
    // The Hopper version of this EPD driver is based on work from Adafruit.
    // Buy their devices!
    
    /*
     *
     * This is a library for our EPD displays based on EPD drivers.
     * Designed specifically to work with Adafruit EPD displays.
     *
     * These displays use SPI to communicate, 4 or 5 pins are required to
     * interface
     *
     * Adafruit invests time and resources providing this open source code,
     * please support Adafruit and open-source hardware by purchasing
     * products from Adafruit!
     *
     * Written by Dean Miller for Adafruit Industries.
     *
     * BSD license, all text here must be included in any redistribution.
     *
     */

    #define DISPLAY_DRIVER
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Display"
    
    const byte SSD1680_DRIVER_CONTROL = 0x01;
    const byte SSD1680_GATE_VOLTAGE = 0x03;
    const byte SSD1680_SOURCE_VOLTAGE = 0x04;
    const byte SSD1680_PROGOTP_INITIAL = 0x08;
    const byte SSD1680_PROGREG_INITIAL = 0x09;
    const byte SSD1680_READREG_INITIAL = 0x0A;
    const byte SSD1680_BOOST_SOFTSTART = 0x0C;
    const byte SSD1680_DEEP_SLEEP = 0x10;
    const byte SSD1680_DATA_MODE = 0x11;
    const byte SSD1680_SW_RESET = 0x12;
    const byte SSD1680_TEMP_CONTROL = 0x18;
    const byte SSD1680_TEMP_WRITE = 0x1A;
    const byte SSD1680_MASTER_ACTIVATE = 0x20;
    const byte SSD1680_DISP_CTRL1 = 0x21;
    const byte SSD1680_DISP_CTRL2 = 0x22;
    const byte SSD1680_WRITE_RAM1 = 0x24;
    const byte SSD1680_WRITE_RAM2 = 0x26;
    const byte SSD1680_WRITE_VCOM = 0x2C;
    const byte SSD1680_READ_OTP = 0x2D;
    const byte SSD1680_READ_STATUS = 0x2F;
    const byte SSD1680_WRITE_LUT = 0x32;
    const byte SSD1680_WRITE_BORDER = 0x3C;
    const byte SSD1680_SET_RAMXPOS = 0x44;
    const byte SSD1680_SET_RAMYPOS = 0x45;
    const byte SSD1680_SET_RAMXCOUNT = 0x4E;
    const byte SSD1680_SET_RAMYCOUNT = 0x4F;
    
    bool flipX;
    bool flipY;
    bool isPortrait;
    bool FlipX { get { return flipX; } set { flipX = value; }}
    bool FlipY { get { return flipY; } set { flipY = value; }}
    bool IsPortrait { get { return isPortrait; } set { isPortrait = value; } }
    
    byte [DeviceDriver.PW*(DeviceDriver.PH+8)/8] blackBuffer;
#ifdef EPD_TWO_BUFFERS
    byte [DeviceDriver.PW*(DeviceDriver.PH+8)/8] colourBuffer;
#endif

    uint roundedPH;         // PH rounded up to the nearest multiple of 8
    uint roundedBufferSize; // roundedPH * PW / 8
    
    const byte[] ssd1680DefaultInitCode =
    {
        SSD1680_SW_RESET, 0, // soft reset
        0xFF, 20,          // busy wait
        SSD1680_DATA_MODE, 1, 0x03, // Ram data entry mode
        SSD1680_WRITE_BORDER, 1, 0x05, // border color
    
        SSD1680_WRITE_VCOM, 1, 0x36,   // Vcom Voltage
        SSD1680_GATE_VOLTAGE, 1, 0x17, // Set gate voltage 
        SSD1680_SOURCE_VOLTAGE, 3, 0x41, 0x00, 0x32,   // Set source voltage
    
        SSD1680_SET_RAMXCOUNT, 1, 1,
        SSD1680_SET_RAMYCOUNT, 2, 0, 0,
        0xFE
    };
    
    bool Visible
    {
        set
        {
            // TODO
        }
    }
    bool inTransaction;
    
    csHigh()
    {
        MCU.DigitalWrite(DeviceDriver.CSPin, true);
        if (inTransaction) 
        {
            SPI.EndTransaction(DeviceDriver.SPIController);
            inTransaction = false;
        }
    }
    csLow()
    {
        if (!inTransaction)
        {
            SPI.BeginTransaction(DeviceDriver.SPIController);
            inTransaction = true;
        }
        MCU.DigitalWrite(DeviceDriver.CSPin, false);
    }
    dcHigh()
    {
        MCU.DigitalWrite(DeviceDriver.DCPin, true);
    }
    dcLow()
    {
        MCU.DigitalWrite(DeviceDriver.DCPin, false);
    }
    
    epdCommandList(byte [] initCode) 
    {
        byte[64] buffer;
       uint index = 0;
        while (initCode[index] != 0xFE) 
        {
            byte cmd = initCode[index];
            index++;
            byte numArgs = initCode[index];
            index++;
            if (cmd == 0xFF) 
            {
                Delay(500);
                Delay(numArgs);
                continue;
            }
           for (byte i = 0; i < numArgs; i++) 
            {
                buffer[i] = initCode[index];
                index++;
            }
            epdCommand(cmd, buffer, numArgs);
        }
    }
    
    spiTransfer(byte tx)
    {
        csLow();   
        SPI.WriteByte(DeviceDriver.SPIController, tx);
        csHigh();
    }
    epdCommand(byte cmd, byte[] buffer, uint length)
    {
        epdCommand(cmd, false);
        epdData(buffer, length);
    }
    epdCommand(byte cmd, bool end)
    {
        csHigh();
        dcLow();
        csLow();
        spiTransfer(cmd);
        if (end) 
        {
            csHigh();
        }
    }
    epdCommand(byte cmd)
    {
        epdCommand(cmd, true);
    }    
    epdData(byte[] buffer, uint length)
    {
        dcHigh();
        for (uint i=0; i < length; i++)
        {
            spiTransfer(buffer[i]);
        }
        csHigh();
    }
    epdData(byte data) 
    {
        csHigh();
        dcHigh();
        csLow();
        spiTransfer(data);
        csHigh();
    }
    
    update() 
    {
        byte[1] buf;
        // display update sequence
        buf[0] = 0xF4;
        epdCommand(SSD1680_DISP_CTRL2, buf, 1);
        epdCommand(SSD1680_MASTER_ACTIVATE);
        Delay(1500);
    }
    
    setRAMAddress() 
    {
      byte[2] buf;
    
      // set RAM x address count
      buf[0] = 1; // _xram_offset;
      epdCommand(SSD1680_SET_RAMXCOUNT, buf, 1);
    
      // set RAM y address count
      buf[0] = 0;
      buf[1] = 0;
      epdCommand(SSD1680_SET_RAMYCOUNT, buf, 2);
    }
    writeRAMCommand(byte index) 
    {
        if (index == 0) 
        {
            epdCommand(SSD1680_WRITE_RAM1, false);
        }
        if (index == 1) 
        {
            epdCommand(SSD1680_WRITE_RAM2, false);
        }
    }
    
    writeRAMFramebufferToEPD(ref byte[] framebuffer, uint framebufferSize, byte epdLocation) 
    {
        writeRAMCommand(epdLocation);
        dcHigh();
        for (uint i = 0; i < framebufferSize; i++) 
        {
            byte d = framebuffer[i];
            spiTransfer(d);
        }
        csHigh();
        return;
    }
    
    UpdateDisplay()
    {
        powerUp();
        setRAMAddress();
        
        // writeRAMFramebufferToEPD(blackBuffer, roundedBufferSize, 0);
        writeRAMCommand(0);
        dcHigh();
        for (uint i = 0; i < roundedBufferSize; i++) 
        {
            byte d = blackBuffer[i];
            spiTransfer(d);
        }
        csHigh();
        
        Delay(2);
        setRAMAddress();
#ifdef EPD_TWO_BUFFERS
        writeRAMFramebufferToEPD(colourBuffer, roundedBufferSize, 1);
#else
        //writeRAMFramebufferToEPD(blackBuffer, roundedBufferSize, 1);
        writeRAMCommand(1);
        dcHigh();
        for (uint i = 0; i < roundedBufferSize; i++) 
        {
            byte d = blackBuffer[i];
            spiTransfer(d);
        }
        csHigh();
#endif
        
        update();
        //if (sleep) 
        //{
        //    powerDown();
        //}
        Delay(DeviceDriver.DefaultRefreshDelay);
    }
    
    powerUp() 
    {
        byte[5] buf;
        Delay(600);
        byte[] initCode = ssd1680DefaultInitCode;
        epdCommandList(initCode);
        
        // Set ram X start/end postion
        buf[0] = 1; // _xram_offset;
        buf[1] = byte(roundedPH / 8 - 1 + 1); // _xram_offset;
        epdCommand(SSD1680_SET_RAMXPOS, buf, 2);
        
        // Set ram Y start/end postion
        buf[0] = 0x00;
        buf[1] = 0x00;
        buf[2] = byte(DeviceDriver.PW - 1);
        buf[3] = byte((DeviceDriver.PW - 1) >> 8);
        epdCommand(SSD1680_SET_RAMYPOS, buf, 4);
        
        // Set display size and driver output control
        buf[0] = byte(DeviceDriver.PW - 1);
        buf[1] = byte((DeviceDriver.PW - 1) >> 8);
        buf[2] = 0x00;
        epdCommand(SSD1680_DRIVER_CONTROL, buf, 3);
    }
    
    powerDown() 
    {
        epdCommand(SSD1680_SW_RESET);
        Delay(500);
    }
    
    bool Begin()
    {
        bool success = false;
        loop
        {
            if (DisplayDriver.IsPortrait)
            {
                Display.PixelWidth  = Int.Min(DeviceDriver.PW, DeviceDriver.PH);
                Display.PixelHeight = Int.Max(DeviceDriver.PW, DeviceDriver.PH);
            }
            else
            {
                Display.PixelWidth  = Int.Max(DeviceDriver.PW, DeviceDriver.PH);
                Display.PixelHeight = Int.Min(DeviceDriver.PW, DeviceDriver.PH);
            }
            roundedPH = uint(DeviceDriver.PH);
            if ((roundedPH % 8) != 0) { roundedPH = roundedPH + 8 - (roundedPH % 8); }
            roundedBufferSize = roundedPH * PW / 8;
            
            Screen.ForeColour = Colour.Black;
            Screen.BackColour = Colour.White;
            
            MCU.PinMode(DeviceDriver.CSPin, PinModeOption.Output);
            MCU.PinMode(DeviceDriver.DCPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.CSPin, true);
            
            SPI.SetClkPin(DeviceDriver.SPIController, DeviceDriver.ClkPin);
            SPI.SetTxPin (DeviceDriver.SPIController, DeviceDriver.TxPin);
            SPI.SetRxPin (DeviceDriver.SPIController, DeviceDriver.RxPin);
            //SPI.Settings(DeviceDriver.SPIController, 4000000, DataOrder.MSBFirst, DataMode.Mode0);// these are the defaults
            if (!SPI.Begin(DeviceDriver.SPIController))
            {
                IO.WriteLn("DisplayDriver.Begin failed in SPI.Begin");
                return false;
            }
            
            
            powerDown();
            
            success = true;
            break;
        }
        return success;
    }
    ScrollUpDisplay(uint lines)
    {
        /*
        uint drow;
        for (uint row = lines; row < int(Display.PixelHeight); row++)
        {
            for (int x = 0; x < int(Display.PixelWidth-1); x++)
            {
                RawSetPixel(x, int(drow), RawGetPixel(x, int(row)));
            }
            drow++;    
        }
        loop
        {
            if (drow >= int(Display.PixelHeight)) { break; }
            for (int x = 0; x < int(Display.PixelWidth-1); x++)
            {
                RawSetPixel(x, int(drow), Colour.White);
            }
            drow++;   
        }
        */    }
    bool ColourToBit0(uint colour)
    {
        bool bit0;
        switch (colour)
        {
            case 0x0000: 
            { 
                bit0 = EPDBlack0; 
            }
            case 0x0F00: 
            {
                bit0 = EPDRed0;   
            }
            default:     { bit0 = EPDWhite0; }
        }
        return bit0;
    }
    bool ColourToBit1(uint colour)
    {
        bool bit1;
        switch (colour)
        {
            case 0x0000: { bit1 = EPDBlack1; }
            case 0x0F00: { bit1 = EPDRed1;   }
            default:     { bit1 = EPDWhite1; }
        }
        return bit1;
    }
    ClearDisplay(uint colour)
    {
        bool bit0 = ColourToBit0(colour);
        for (uint i = 0; i < roundedBufferSize; i++)
        {
            blackBuffer[i] = bit0 ? 0x00 : 0xFF;
        }
#ifdef EPD_TWO_BUFFERS
        bool bit1 = ColourToBit1(colour);
        for (uint i = 0; i < roundedBufferSize; i++)
        {
            colourBuffer[i] = bit1 ? 0x00 : 0xFF;
        }
#endif        
    } 
    RawSetPixel(int vx, int vy, uint colour)
    {
        if (FlipX)
        {
            vx = (Display.PixelWidth-1) - vx;
        }
        if (FlipY)
        {
            vy = (Display.PixelHeight-1) - vy;
        }
        int x = vx;
        int y = vy;
        if (IsPortrait)
        {
            x = vy;
            y = vx;
        }
        // deal with non-8-bit heights
        uint address = ((uint(DeviceDriver.PW) - 1 - uint(x)) * roundedPH + uint(y)) / 8;
        
        bool bit0 = ColourToBit0(colour);
        
#ifdef EPD_TWO_BUFFERS
        bool bit1 = ColourToBit1(colour);
        if (bit1) 
        {
            colourBuffer[address] = colourBuffer[address] & ~(1 << (7 - y % 8));
        }
        else 
        {
            colourBuffer[address] = colourBuffer[address] | (1 << (7 - y % 8));
        }
#endif
        if (bit0) 
        {
            blackBuffer[address] = blackBuffer[address] & ~(1 << (7 - y % 8));
        } 
        else 
        {
            blackBuffer[address] = blackBuffer[address] | (1 << (7 - y % 8));
        }
    }  
    RawHorizontalLine(int x1, int y, int x2, uint colour)
    {
        for (int x = x1; x <= x2; x++)
        {
            RawSetPixel(x, y, colour);
        }
    }
    RawVerticalLine(int x, int y1, int y2, uint colour)
    {
        for (int y = y1; y <= y2; y++)
        {
            RawSetPixel(x, y, colour);
        }
    }
}
