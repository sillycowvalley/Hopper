unit MCU
{
    // 8 bit CPU version of MCU
#if !defined(MCU_BOARD_DEFINED)
    #error "Find your board under /Source/Library/Boards, add 'uses' for it in your program"
#endif    

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
    
#ifdef M6821_PIA
    byte portBShadow;
#endif    
    
#ifdef MCU
    PinMode(byte pin, PinModeOption pinMode) library;
    bool DigitalRead(byte pin) library;
    DigitalWrite(byte pin, bool value) library;
#else    
    
    PinMode(byte pin, PinModeOption pinMode)
    {
        uint ddr = (pin <= 7) ? DDRA : DDRB;
#ifdef M6821_PIA
        uint cr  = (pin <= 7) ? CRA  : CRB;
        // Select the DDR
        //byte crValue = (pin <= 7) ? 0b00110000 : 0b00000000;
        //byte crValue = 0b00110000;
        byte crValue = 0b00000000;
        Memory.WriteByte(cr, crValue);
#endif        
        pin = pin & 0b00000111;
        pin = 1 << pin;
        if (pinMode == PinModeOption.Input)
        {
            Memory.WriteByte(ddr, Memory.ReadByte(ddr) & ~pin);
        }
        else
        {
            Memory.WriteByte(ddr, Memory.ReadByte(ddr) | pin);
        }
    }
    
    bool DigitalRead(byte pin)
    {
        uint port = (pin <= 7) ? PORTA : PORTB;
#ifdef M6821_PIA
        // Select the port register
        uint cr   = (pin <= 7) ? CRA   : CRB;
        //byte crValue = (pin <= 7) ? 0b00110100 : 0b00000100;
        //byte crValue = 0b00110100;
        byte crValue = 0b00000100;
        Memory.WriteByte(cr, crValue);
#endif   
        byte b = Memory.ReadByte(port);
        return ((Memory.ReadByte(port) & (1 << (pin & 0b00000111))) != 0);

    }
    DigitalWrite(byte pin, bool value)
    {
        uint port = (pin <= 7) ? PORTA : PORTB;
#ifdef M6821_PIA
        // Select the port register
        uint cr      = (pin <= 7) ? CRA   : CRB;
        byte crValue = (pin <= 7) ? 0b00110100 : 0b00000100;
        Memory.WriteByte(cr, crValue);
        
        pin = 1 << (pin & 0b00000111);
        if (port == PORTB)
        {
            if (value)
            {
                portBShadow = portBShadow | pin;
            }
            else
            {
                portBShadow = portBShadow & ~pin;
            }
            Memory.WriteByte(port, portBShadow);
        }
        else
        {
            if (value)
            {
                Memory.WriteByte(port, Memory.ReadByte(port) | pin);
            }
            else
            {
                Memory.WriteByte(port, Memory.ReadByte(port) & ~pin);
            }
        }
#else
        pin = 1 << (pin & 0b00000111);
        if (value)
        {
            Memory.WriteByte(port, Memory.ReadByte(port) | pin);
        }
        else
        {
            Memory.WriteByte(port, Memory.ReadByte(port) & ~pin);
        }
#endif
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
