unit Serial
{
    // Zero Page locations used by Serial:
    const byte InWritePointer  = ZP.SerialInWritePointer;
    const byte InReadPointer   = ZP.SerialInReadPointer;
    const byte BreakFlag       = ZP.SerialBreakFlag;
    const byte WorkSpace       = ZP.ACCL;
    
    // Location of the Serial input buffer (256 bytes)
    const uint InBuffer        = Address.SerialInBuffer;
    
    
    //uses "Devices/ACIA6850"
    uses "Devices/PIA6821"
    
    // initialize or reset the serial ACIA firmware
    Initialize()
    {
        PHA
        
        // reset buffer so at least start and end are the same
        SEI                    // disable interrupts
        LDA #0
        STA Serial.InWritePointer
        STA Serial.InReadPointer
        STA Serial.BreakFlag
        CLI                    // enable interrupts
        
        SerialDevice.initialize(); // device-specific initialization
                        
        PLA
    }
    
    ISR()
    {
        SerialDevice.isr();
    }
    
    EmptyTheBuffer()
    {
        PHA
        loop
        {
            IsAvailable();
            if (Z) { break; }
            WaitForChar();
        }
        PLA
    }
    
    // returns Z flag clear if there is a character available in the buffer, Z set if not (disables and enables interrupts)
    IsAvailable()
    {
#ifndef HAS_SERIAL_ISR
        SerialDevice.pollRead();        
#endif        
        SEI
        LDA BreakFlag
        if (Z)
        {
            LDA InReadPointer
            CMP InWritePointer
        }
        CLI
    }
    
    // consumes the next character from the buffer and returns value in A
    //     munts X on CPU_6502
    WaitForChar()
    {
        loop
        {
            IsAvailable();
            if (NZ) { break; }
        }
#ifdef CPU_65C02        
        PHX
#endif        
        LDA ZP.SerialBreakFlag
        if (NZ)
        {
            SEI
            DEC Serial.BreakFlag
            CLI
            LDA #0x03
        }
        else
        {
            LDX Serial.InReadPointer
            LDA Serial.InBuffer, X
            INC Serial.InReadPointer
        }
#ifdef CPU_65C02        
        PLX
#endif
    }
    
       
    // transmits A
    WriteChar()
    {
        SerialDevice.writeChar();
    }
    // transmits A as two hex characters
    HexOut()
    {
        PHA
        PHA
        
        // most significant nibble
        LSR LSR LSR LSR
        CMP #0x0A
        if (C)
        {
            ADC #6 // add 7 (6+C=1)
        }
        ADC #'0'
        WriteChar();
        
        // least significant nibble
        PLA        
        AND #0x0F
        CMP #0x0A
        if (C)
        {
            ADC #6 // add 7 (6+C=1)
        }
        ADC #'0'
        WriteChar();
        
        PLA
    }
    
    // loads two hex characters from Serial to byte in A
    //    uses WorkSpace (ZP.ACCL  )
    HexIn()
    {
        Serial.WaitForChar();
        Utilities.MakeNibble();
        ASL ASL ASL ASL
        AND #0xF0
        STA WorkSpace
        Serial.WaitForChar();
        Utilities.MakeNibble();
        ORA WorkSpace
    }
}
