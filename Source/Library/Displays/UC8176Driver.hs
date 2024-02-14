unit DisplayDriver
{
    #define DISPLAY_DRIVER
    
    uses "/Source/Library/MCU"
    uses "/Source/Library/Display"
    
    // https://www.waveshare.com/wiki/Pico-ePaper-4.2
    
    flags pswFlags
    {
        RES_400x300  = 0b00000000,
        RES_320x300  = 0b01000000,
        RES_320x240  = 0b10000000,
        RES_200x300  = 0b11000000,
        
        LUT_OTP      = 0b00000000,
        LUT_REG      = 0b00100000,
        
        FORMAT_BWR   = 0b00000000,
        FORMAT_BW    = 0b00010000,
        
        SCAN_DOWN    = 0b00000000,
        SCAN_UP      = 0b00001000,
        
        SHIFT_LEFT   = 0b00000000,
        SHIFT_RIGHT  = 0b00000100,
        
        BOOSTER_OFF  = 0b00000000,
        BOOSTER_ON   = 0b00000010,
        
        RESET_SOFT   = 0b00000000,
        RESET_NONE   = 0b00000001
    }
    
    flags pwrFlags1
    {
        VDS_EXTERNAL = 0b00000000,
        VDS_INTERNAL = 0b00000010,
        
        VDG_EXTERNAL = 0b00000000,
        VDG_INTERNAL = 0b00000001
    }

    flags pwrFlags2
    {
        VCOM_VD      = 0b00000000,
        VCOM_VG      = 0b00000100,
        
        VGHL_16V     = 0b00000000,
        VGHL_15V     = 0b00000001,
        VGHL_14V     = 0b00000010,
        VGHL_13V     = 0b00000011
    }
    flags boosterFlags
    {
        START_10MS = 0b00000000,
        START_20MS = 0b01000000,
        START_30MS = 0b10000000,
        START_40MS = 0b11000000,
        
        STRENGTH_1 = 0b00000000,
        STRENGTH_2 = 0b00001000,
        STRENGTH_3 = 0b00010000,
        STRENGTH_4 = 0b00011000,
        STRENGTH_5 = 0b00100000,
        STRENGTH_6 = 0b00101000,
        STRENGTH_7 = 0b00110000,
        STRENGTH_8 = 0b00111000,
        
        OFF_0_27US = 0b00000000,
        OFF_0_34US = 0b00000001,
        OFF_0_40US = 0b00000010,
        OFF_0_54US = 0b00000011,
        OFF_0_80US = 0b00000100,
        OFF_1_54US = 0b00000101,
        OFF_3_34US = 0b00000110,
        OFF_6_58US = 0b00000111
    }

    flags pfsFlags
    {
        FRAMES_1   = 0b00000000,
        FRAMES_2   = 0b00010000,
        FRAMES_3   = 0b00100000,
        FRAMES_4   = 0b00110000
    }

    flags tseFlags
    {
        TEMP_INTERNAL = 0b00000000,
        TEMP_EXTERNAL = 0b10000000,
        
        OFFSET_0      = 0b00000000,
        OFFSET_1      = 0b00000001,
        OFFSET_2      = 0b00000010,
        OFFSET_3      = 0b00000011,
        OFFSET_4      = 0b00000100,
        OFFSET_5      = 0b00000101,
        OFFSET_6      = 0b00000110,
        OFFSET_7      = 0b00000111,
        
        OFFSET_MIN_8  = 0b00001000,
        OFFSET_MIN_7  = 0b00001001,
        OFFSET_MIN_6  = 0b00001010,
        OFFSET_MIN_5  = 0b00001011,
        OFFSET_MIN_4  = 0b00001100,
        OFFSET_MIN_3  = 0b00001101,
        OFFSET_MIN_2  = 0b00001110,
        OFFSET_MIN_1  = 0b00001111
    }

    flags pplFlags
    {
        // other frequency options exist but there doesn't seem to be much
        // point in including them - this is a fair range of options...
        HZ_29      = 0b00111111,
        HZ_33      = 0b00111110,
        HZ_40      = 0b00111101,
        HZ_50      = 0b00111100,
        HZ_67      = 0b00111011,
        HZ_100     = 0b00111010,
        HZ_200     = 0b00111001
    }
    
    enum register 
    {
        PSR      = 0x00,
        PWR      = 0x01,
        POF      = 0x02,
        PFS      = 0x03,
        PON      = 0x04,
        PMES     = 0x05,
        BTST     = 0x06,
        DSLP     = 0x07,
        DTM1     = 0x10,
        DSP      = 0x11,
        DRF      = 0x12,
        DTM2     = 0x13,
        LUT_VCOM = 0x20,
        LUT_WW   = 0x21,
        LUT_BW   = 0x22,
        LUT_WB   = 0x23,
        LUT_BB   = 0x24,
        PLL      = 0x30,
        TSC      = 0x40,
        TSE      = 0x41,
        TSR      = 0x43,
        TSW      = 0x42,
        CDI      = 0x50,
        LPD      = 0x51,
        TCON     = 0x60,
        TRES     = 0x61,
        REV      = 0x70,
        FLG      = 0x71,
        AMV      = 0x80,
        VV       = 0x81,
        VDCS     = 0x82,
        PTL      = 0x90,
        PTIN     = 0x91,
        PTOU     = 0x92,
        PGM      = 0xa0,
        APG      = 0xa1,
        ROTP     = 0xa2,
        CCSET    = 0xe0,
        PWS      = 0xe3,
        TSSET    = 0xe5,
    }

    bool flipX;
    bool flipY;
    bool isPortrait;
    bool FlipX { get { return flipX; } set { flipX = value; }}
    bool FlipY { get { return flipY; } set { flipY = value; }}
    bool IsPortrait { get { return isPortrait; } set { isPortrait = value; } }
    
    bool alternateAspect;
    const uint bufferSize = DeviceDriver.PW / 8 * DeviceDriver.PH;
    byte [bufferSize] frameBuffer; // 296*128/8 = 4736 bytes
    
    bool Visible
    {
        set
        {
            // TODO
        }
    }
    
    bool isBusy {get { return !DigitalRead(DeviceDriver.BusyPin); } }

    busyWait()
    {
        while(isBusy) 
        {
           // tight_loop_contents();
        }
    }
    
    sendCommand(register reg)
    {
        SPI.BeginTransaction(DeviceDriver.SPIController);
        MCU.DigitalWrite(DeviceDriver.CSPin, false);
        MCU.DigitalWrite(DeviceDriver.DCPin, false); // command mode
        SPI.WriteByte(DeviceDriver.SPIController,  byte(reg));
        MCU.DigitalWrite(DeviceDriver.CSPin, true);
        SPI.EndTransaction(DeviceDriver.SPIController);
    }
    
    sendCommand(register reg, byte[] data)
    {
        uint length = data.Count;
        SPI.BeginTransaction(DeviceDriver.SPIController);
        MCU.DigitalWrite(DeviceDriver.CSPin, false);
        MCU.DigitalWrite(DeviceDriver.DCPin, false); // command mode
        SPI.WriteByte(DeviceDriver.SPIController,  byte(reg));
        if (length > 0)
        {
            MCU.DigitalWrite(DeviceDriver.DCPin, true); // data mode
            SPI.WriteBuffer(DeviceDriver.SPIController,  data, 0, length);
        }
        MCU.DigitalWrite(DeviceDriver.CSPin, true);
        SPI.EndTransaction(DeviceDriver.SPIController);
    }
    sendData(byte[] data)
    {
        uint length = data.Count;
        if (length > 0)
        {
            SPI.BeginTransaction(DeviceDriver.SPIController);
            MCU.DigitalWrite(DeviceDriver.CSPin, false);
            MCU.DigitalWrite(DeviceDriver.DCPin, true); // data mode
            SPI.WriteBuffer(DeviceDriver.SPIController,  data, 0, length);
            MCU.DigitalWrite(DeviceDriver.CSPin, true);
            SPI.EndTransaction(DeviceDriver.SPIController);
        }
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
            
            Display.Reset();
            for (uint i = 0; i < bufferSize; i++)
            {
                frameBuffer[i] = 0;
            }
            
            SPI.SetCSPin(DeviceDriver.SPIController,  DeviceDriver.CSPin);
            SPI.SetTxPin(DeviceDriver.SPIController,  DeviceDriver.TxPin);
            SPI.SetClkPin(DeviceDriver.SPIController, DeviceDriver.ClkPin);
            
            MCU.PinMode(DeviceDriver.CSPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.CSPin, true); // Deselect
            MCU.PinMode(DeviceDriver.DCPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.DCPin, true); // Data mode
            MCU.PinMode(DeviceDriver.RstPin, PinModeOption.Output);
            MCU.DigitalWrite(DeviceDriver.RstPin, true);
            MCU.PinMode(DeviceDriver.BusyPin, PinModeOption.InputPullup);
            
            SPI.Settings(DeviceDriver.SPIController, 12000000, DataOrder.MSBFirst, DataMode.Mode0);
            
            if (!SPI.Initialize(DeviceDriver.SPIController))
            {
                IO.WriteLn("DeviceDriver.Begin failed in SPI.Begin");
                break;
            }
            
            // setup(0);
            // https://github.com/pimoroni/pimoroni-pico/blob/911cbb710ebb725d83065a21438c35783ef8b19c/drivers/uc8151_legacy/uc8151_legacy.cpp#L323
            byte[] pswData = { pswFlags.LUT_OTP | pswFlags.FORMAT_BW | pswFlags.SHIFT_RIGHT | pswFlags.BOOSTER_ON | pswFlags.RESET_NONE };
            if (    ((Display.PixelWidth == 400) && (Display.PixelHeight == 300))
                 || ((Display.PixelWidth == 300) && (Display.PixelHeight == 400))
               )
            {
                alternateAspect = ((Display.PixelWidth == 300) && (Display.PixelHeight == 400));
                pswData[0] = pswData[0] | byte(pswFlags.RES_400x300);
            }
            else if (   ((Display.PixelWidth == 320) && (Display.PixelHeight == 300))
                     || ((Display.PixelWidth == 300) && (Display.PixelHeight == 320))
               )
            {
                alternateAspect = ((Display.PixelWidth == 300) && (Display.PixelHeight == 320));
                pswData[0] = pswData[0] | byte(pswFlags.RES_320x300);
            }
            else if (   ((Display.PixelWidth == 320) && (Display.PixelHeight == 240))
                     || ((Display.PixelWidth == 240) && (Display.PixelHeight == 320))
               )
            {
                alternateAspect = ((Display.PixelWidth == 240) && (Display.PixelHeight == 320));
                pswData[0] = pswData[0] | byte(pswFlags.RES_320x240);
            }
            else if (   ((Display.PixelWidth == 200)  && (Display.PixelHeight == 300))
                     || ((Display.PixelWidth == 300) && (Display.PixelHeight == 200))
               )
            {
                alternateAspect = ((Display.PixelWidth == 300) && (Display.PixelHeight == 200));
                pswData[0] = pswData[0] | byte(pswFlags.RES_200x300);
            }
            else
            {
                IO.WriteLn("DeviceDriver.Begin failed : unsupported resolution");
                break;
            }
            MCU.InterruptsEnabled = false;
            busyWait();
            sendCommand(register.PSR, pswData);
            
            byte[]
            data = {
                        pwrFlags1.VDS_INTERNAL | pwrFlags1.VDG_INTERNAL,
                        pwrFlags2.VCOM_VD      | pwrFlags2.VGHL_16V,
                        0b101011,
                        0b101011,
                        0b101011
                   };
            sendCommand(register.PWR, data);
    
            sendCommand(register.PON); // power on
            busyWait();
    
            // booster soft start configuration
            data = {
                      boosterFlags.START_10MS | boosterFlags.STRENGTH_3 | boosterFlags.OFF_6_58US,
                      boosterFlags.START_10MS | boosterFlags.STRENGTH_3 | boosterFlags.OFF_6_58US,
                      boosterFlags.START_10MS | boosterFlags.STRENGTH_3 | boosterFlags.OFF_6_58US
                   };
            sendCommand(register.BTST, data);
        
            data = {pfsFlags.FRAMES_1};
            sendCommand(register.PFS, data);
        
            data = { tseFlags.TEMP_INTERNAL | tseFlags.OFFSET_0};
            sendCommand(register.TSE, data);
        
            data = {0x22};
            sendCommand(register.TCON, data); // tcon setting
            data = { 0b01001100 }; // {(uint8_t)(inverted ? 0b01011100 : 0b01001100)}; // vcom and data interval
            sendCommand(register.CDI, data );
           
            data = { pplFlags.HZ_100};
            sendCommand(register.PLL, data);
            
            // send it again..
            busyWait();
            sendCommand(register.PSR, pswData);
        
            sendCommand(register.POF); // power off
            busyWait();
            MCU.InterruptsEnabled = true;
            
            ph1 = uint(Display.PixelHeight - 1);
            pw1 = uint(Display.PixelWidth  - 1);
            ph8 = uint(Display.PixelHeight / 8);
            pw8 = uint(Display.PixelWidth / 8);
            
            success = true;
            break;
        }
        return success;
    }
    
    uint ph1;
    uint pw1;
    uint ph8;
    uint pw8;
    
    RawSetPixel(int x, int y, uint colour)
    {
        uint offset;
        byte mask;
        uint ux = uint(x);
        uint uy = uint(y);
        if (flipY)
        {
            uy = ph1 - uy;    
        }
        if (flipX)
        {
            ux = pw1 - ux;    
        }
        if (alternateAspect)
        {
            offset = (uy / 8) + (ux * ph8);
            mask   = (1 << (0x07 - (uy & 0x07)));
        }
        else
        {
            offset = (ux / 8) + (uy * pw8);
            mask   = (1 << (0x07 - (ux & 0x07)));
        }
        if (colour == 0x0000) // Colour.Black
        {
            frameBuffer[offset] = frameBuffer[offset] | mask; 
        }
        else if (colour == 0xF000) // Colour.Invert
        {
            frameBuffer[offset] = frameBuffer[offset] ^ mask;
        }
        else
        {
            frameBuffer[offset] = frameBuffer[offset] & ~mask;
        }
    }
    
    ClearDisplay(uint colour)
    {
        if (colour == 0xF000) // Colour.Invert
        {
            for (uint i = 0; i < bufferSize; i++)
            {
                frameBuffer[i] = ~frameBuffer[i];
            }
        }
        else if (colour == 0x0000) // Colour.Black
        {
            for (uint i = 0; i < bufferSize; i++)
            {
                frameBuffer[i] = 0xFF;
            }
        }
        else
        {
            for (uint i = 0; i < bufferSize; i++)
            {
                frameBuffer[i] = 0x00;
            }
        }
    }
    UpdateDisplay()
    {
#ifdef DISPLAY_DIAGNOSTICS
        IO.Write("<DisplayDriver.UpdateDisplay");
#endif        
        MCU.InterruptsEnabled = false;
        busyWait();
        sendCommand(register.PON); // turn on
        sendCommand(register.PTOU); // disable partial mode
        sendCommand(register.DTM2, frameBuffer); // transmit framebuffer
        sendCommand(register.DSP); // data stop
        sendCommand(register.DRF); // start display refresh
        busyWait();
        sendCommand(register.POF); // turn off
        MCU.InterruptsEnabled = true;
#ifdef DISPLAY_DIAGNOSTICS
        IO.WriteLn("DisplayDriver.UpdateDisplay>");
#endif        
    }
    uint RawGetPixel(int x, int y)
    {
        uint ux = uint(x);
        uint uy = uint(y);
        
        uint offset = (ux / 8) + (uy * (Display.PixelWidth / 8));
        byte mask   = (1 << (0x07 - (ux & 0x07)));
        if (0 == (frameBuffer[offset] & mask))
        {
            return Colour.White;
        }
        return Colour.Black;
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
        */
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
