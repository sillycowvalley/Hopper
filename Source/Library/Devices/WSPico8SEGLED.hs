unit Pico8SEGLED
{
    // https://www.waveshare.com/wiki/Pico-8SEG-LED
    
#if !defined(MCU_BOARD_DEFINED)
    // plugs directly into the Pi Pico so no board defined, assume generic Pi Pico
    uses "/Source/Library/Boards/PiPico"
#endif
    
    byte RClkPin       { get { return Board.D9; } }
    byte ClkPin        { get { return Board.D10; } }
    byte TxPin         { get { return Board.D11; } }
    const byte SPIController = 1; // this device uses SPI1 on Raspberry Pi Pico
    
    const byte THOUSANDS = 0xFE;
    const byte HUNDREDS  = 0xFD;
    const byte TENS      = 0xFB;
    const byte UNITS     = 0xF7;
    const byte DECIMAL   = 0x80;
    
    const byte[] seg8Code =
    {
        0x3F, // 0
        0x06, // 1
        0x5B, // 2
        0x4F, // 3
        0x66, // 4
        0x6D, // 5
        0x7D, // 6
        0x07, // 7
        0x7F, // 8
        0x6F, // 9
        0x77, // A
        0x7C, // b
        0x39, // C
        0x5E, // d
        0x79, // E
        0x71, // F
    };
    byte decimalMask1;
    byte decimalMask2;
    byte decimalMask3;
    byte decimalMask4;
    byte decimalPosition;
    
    bool hexDigits;
    bool leadingZeroes;
    bool Hex             { set { hexDigits = value; } get { return hexDigits; } }
    bool LeadingZeroes   { set { leadingZeroes = value; } get { return leadingZeroes; } }
    byte DecimalPosition 
    { 
        get { return decimalPosition; }
        set 
        { 
            decimalMask1 = 0;
            decimalMask2 = 0;
            decimalMask3 = 0;
            decimalMask4 = 0;
            decimalPosition = value;
            switch (value)
            {
                case 1: { decimalMask1 = DECIMAL; } 
                case 2: { decimalMask2 = DECIMAL; } 
                case 3: { decimalMask3 = DECIMAL; } 
                case 4: { decimalMask4 = DECIMAL; } 
            }
        } 
    }
    
    bool Begin()
    {
        SPI.SetClkPin(SPIController, ClkPin);
        SPI.SetTxPin (SPIController, TxPin);
        PinMode(RClkPin, PinModeOption.Output);
        if (!SPI.Begin(SPIController))
        {
            IO.WriteLn("DeviceDriver.Begin failed in SPI.Begin");
            return false;
        }
        return true;
    }
    
    sendCommand(byte digit, byte segments) 
    {
        DigitalWrite(RClkPin, true);
        SPI.BeginTransaction(SPIController);
        SPI.WriteByte(SPIController, digit);
        SPI.WriteByte(SPIController, segments);
        SPI.EndTransaction(SPIController);
        DigitalWrite(RClkPin, false);
        Delay(1);
        DigitalWrite(RClkPin, true);
    }
    
    Show(long value)
    {
        if (Hex)
        {
            if ((value >= 0x0000) && (value <= 0xFFFF))
            {
                showHex(uint(value));
            }
            else
            {
                //showHex("0x0ERR");
            }
        }
        else 
        {
            showDecimal(value);
        }
    }
    showDecimal(long value)
    {
        // 1ms (should be 0.5ms)
        Delay(1); 
        sendCommand(UNITS,    seg8Code[value % 10] | decimalMask1);
        Delay(1); 
        if (LeadingZeroes || (value > 9) || (DecimalPosition >= 2))
        {
            sendCommand(TENS,     seg8Code[(value % 100)/10] | decimalMask2);
        } else { Delay(1); }
        Delay(1); 
        if (LeadingZeroes || (value > 99) || (DecimalPosition >= 3))
        {
            sendCommand(HUNDREDS, seg8Code[(value % 1000)/100] | decimalMask3);
        } else { Delay(1); }
        Delay(1); 
        if (LeadingZeroes || (value > 999) || (DecimalPosition >= 4))
        {
            sendCommand(THOUSANDS, seg8Code[(value % 10000)/1000] | decimalMask4);
        } else { Delay(1); }
    }
    showHex(uint value)
    {
        // 1ms (should be 0.5ms)
        Delay(1); 
        sendCommand(UNITS,    seg8Code[value & 0x0F] | decimalMask1);
        Delay(1); 
        if (LeadingZeroes || (value > 0x000F) || (DecimalPosition >= 2))
        {
            sendCommand(TENS,     seg8Code[(value >> 4) & 0x0F] | decimalMask2);
        } else { Delay(1); }
        Delay(1); 
        if (LeadingZeroes || (value > 0x00FF) || (DecimalPosition >= 3))
        {
            sendCommand(HUNDREDS, seg8Code[(value >> 8) & 0x0F] | decimalMask3);
        } else { Delay(1); }
        Delay(1); 
        if (LeadingZeroes || (value > 0x0FFF) || (DecimalPosition >= 4))
        {
            sendCommand(THOUSANDS, seg8Code[(value >> 12) & 0x0F] | decimalMask4);
        } else { Delay(1); }
    }
}
