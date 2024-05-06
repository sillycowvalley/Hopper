unit MCU
{
    // 8 bit CPU version of MCU
    // assuming the W65C22 VIA for now:
    const byte PORTB                = 0xF0;
    const byte PORTA                = 0xF1;
    const byte DDRB                 = 0xF2;
    const byte DDRA                 = 0xF3;
    
    uses "/Source/Minimal/Memory"
    uses "/Source/Minimal/Wire"
    uses "/Source/Minimal/IO"
    
    const byte GP0 = 0;
    const byte GP1 = 1;
    const byte GP2 = 2;
    const byte GP3 = 3;
    const byte GP4 = 4;
    const byte GP5 = 5;
    const byte GP6 = 6;
    const byte GP7 = 7;
    
    const byte GP8 = 8;
    const byte GP9 = 9;
    const byte GP10 = 10;
    const byte GP11 = 11;
    const byte GP12 = 12;
    const byte GP13 = 13;
    const byte GP14 = 14;
    const byte GP15 = 15;
    
    flags PinModeOption
    {
        Input         = 0x00,
        Output        = 0x01,
        InputPullup   = 0x02,
        InputPulldown = 0x03,
    }
    
    enum PinStatus
    {
        Low = 0,
        High = 1,
    }
#ifdef MCU
    PinMode(byte pin, PinModeOption pinMode) library;
    bool DigitalRead(byte pin) library;
    DigitalWrite(byte pin, bool value) library;
#else    
    
    PinMode(byte pin, PinModeOption pinMode)
    {
        byte ddr = (pin <= 7) ? DDRA : DDRB;
        pin = pin & 0x07;
        pin = 1 << pin;
        byte currentValue = Memory.ReadByte(ddr);
        if (pinMode == PinModeOption.Input)
        {
            currentValue = currentValue & ~pin;
        }
        else
        {
            currentValue = currentValue | pin;
        }
        Memory.WriteByte(ddr, currentValue);
    }
    
    bool DigitalRead(byte pin)
    {
        byte port = (pin <= 7) ? PORTA : PORTB;
        pin = pin & 0x07;
        pin = 1 << pin;
        byte currentValue = Memory.ReadByte(port);
        return ((currentValue & pin) != 0);
    }
    DigitalWrite(byte pin, bool value)
    {
        byte port = (pin <= 7) ? PORTA : PORTB;
        pin = pin & 0x07;
        pin = 1 << pin;
        byte currentValue = Memory.ReadByte(port);
        if (value)
        {
            currentValue = currentValue | pin;
        }
        else
        {
            currentValue = currentValue & ~pin;
        }
        Memory.WriteByte(port, currentValue);
    }
#endif
}
