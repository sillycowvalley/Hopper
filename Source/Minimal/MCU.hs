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
        pin = 1 << (pin & 0x07);
        return ((Memory.ReadByte(port) & pin) != 0);
    }
    DigitalWrite(byte pin, bool value)
    {
        byte port = (pin <= 7) ? PORTA : PORTB;
        pin = 1 << (pin & 0x07);
        if (value)
        {
            Memory.WriteByte(port, Memory.ReadByte(port) | pin);
        }
        else
        {
            Memory.WriteByte(port, Memory.ReadByte(port) & ~pin);
        }
    }
    
    bool ledState;
    bool LED 
    { 
        set 
        { 
            ledState = value;
            PinMode(Board.BuiltInLED, MCU.PinModeOption.Output);
            DigitalWrite(Board.BuiltInLED, value); 
        } 
        get
        {
            return ledState;
        }
    }
#endif
}
