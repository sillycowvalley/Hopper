unit GPIO
{
    const byte[] BitMasks = { 0b00000001, 0b00000010, 0b00000100, 0b00001000,
                              0b00010000, 0b00100000, 0b01000000, 0b10000000 };
     
    // Pin mode constants
    enum PINMODE
    {
        INPUT  = 0x00,
        OUTPUT = 0x01,
    }
    
#ifdef M6821_PIA    
    const byte PIA_DDR  = 0b00110000;    // CR for DDR access
    const byte PIA_PORT = 0b00110100;    // CR for PORT access
#endif    
    
    // Configure pin mode (INPUT or OUTPUT)
    // Input: A = pin number (0-15), Y = mode (PinMode.INPUT or PinMode.OUTPUT)
    // Output: None
    PinMode()
    {
        CMP #8
        if (C)
        {
            // Pin 8-15: Use Port B
            SEC
            SBC #8
            TAX
            LDA BitMasks, X
            
#ifdef M6821_PIA
            PHX
            LDX #PIA_DDR
            STX ZP.CRB
            PLX
#endif
            
            CPY #PINMODE.INPUT
            if (Z)
            {
                EOR #0xFF
                AND ZP.DDRB
            }
            else
            {
                ORA ZP.DDRB
            }
            STA ZP.DDRB

#ifdef M6821_PIA
            LDX #PIA_PORT
            STX ZP.CRB
#endif
        }
        else
        {
            // Pin 0-7: Use Port A
            TAX
            LDA BitMasks, X
            
#ifdef M6821_PIA
            PHX
            LDX #PIA_DDR
            STX ZP.CRA
            PLX
#endif
            
            CPY #PINMODE.INPUT
            if (Z)
            {
                EOR #0xFF
                AND ZP.DDRA
            }
            else
            {
                ORA ZP.DDRA
            }
            STA ZP.DDRA

#ifdef M6821_PIA
            LDX #PIA_PORT
            STX ZP.CRA
#endif
        }
    }
    
    // Write digital value to pin
    // Input: A = pin number (0-15), Y = value (0 or 1)
    PinWrite()
    {
        CMP #8
        if (C)
        {
            // Pin 8-15: Use Port B
            SEC
            SBC #8
            TAX
            LDA BitMasks, X
            
    #ifdef M6821_PIA
            PHX
            LDX #PIA_PORT
            STX ZP.CRB
            PLX
    #endif
            
            CPY #0
            if (Z)
            {
                EOR #0xFF
                AND ZP.PORTB
            }
            else
            {
                ORA ZP.PORTB
            }
            STA ZP.PORTB
        }
        else
        {
            // Pin 0-7: Use Port A
            TAX
            LDA BitMasks, X
            
    #ifdef M6821_PIA
            PHX
            LDX #PIA_PORT
            STX ZP.CRA
            PLX
    #endif
            
            CPY #0
            if (Z)
            {
                EOR #0xFF
                AND ZP.PORTA
            }
            else
            {
                ORA ZP.PORTA
            }
            STA ZP.PORTA
        }
    }
    
    // Read digital value from pin
    // Input:    A = pin number (0-15)
    // Output:   A = pin value (0 or 1), Z flag set if LOW
    PinRead()
    {
        CMP #8
        if (C)
        {
            // Pin 8-15: Use Port B
            SEC
            SBC #8
            TAX
            LDA BitMasks, X
            
    #ifdef M6821_PIA
            PHX
            LDX #PIA_PORT
            STX ZP.CRB
            PLX
    #endif
            
            AND ZP.PORTB
        }
        else
        {
            // Pin 0-7: Use Port A
            TAX
            LDA BitMasks, X
            
    #ifdef M6821_PIA
            PHX
            LDX #PIA_PORT
            STX ZP.CRA
            PLX
    #endif
            
            AND ZP.PORTA
        }
        
        if (Z)
        {
            LDA #0
        }
        else
        {
            LDA #1
        }
    }
}
