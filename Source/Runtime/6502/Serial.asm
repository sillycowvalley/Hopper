unit Serial
{
    uses "ZeroPage"
    uses "MemoryMap"
    
    // Zero Page locations used by Serial:
    const byte InWritePointer        = ZP.SerialInWritePointer;
    const byte InReadPointer         = ZP.SerialInReadPointer;
    const byte BreakFlag             = ZP.SerialBreakFlag;
    const byte WorkSpaceHexIn        = ZP.W0;
#ifndef CPU_65C02S    
    const byte WorkSpaceWaitForChar  = ZP.W1;
#endif
    
    // Location of the Serial input buffer (256 bytes)
    const uint InBuffer        = Address.SerialInBuffer;
#if defined(ACIA_6850) && !defined(APPLE_I)
    uses "Devices/ACIA6850"
#endif
#if defined(ACIA_6551) && !defined(APPLE_I)
    uses "Devices/ACIA6551"
#endif
#ifdef APPLE_I
    uses "Devices/AppleI"
#endif    
    // initialize or reset the serial ACIA firmware
    Initialize()
    {
        // reset buffer so at least start and end are the same
        SEI                    // disable interrupts
#ifdef CPU_65C02S
        STZ Serial.InWritePointer
        STZ Serial.InReadPointer
        STZ Serial.BreakFlag
#else
        LDA #0
        STA Serial.InWritePointer
        STA Serial.InReadPointer
        STA Serial.BreakFlag
#endif
        CLI                    // enable interrupts
        
        SerialDevice.initialize(); // device-specific initialization
    }
    
    ISR()
    {
#ifdef HAS_SERIAL_ISR        
        SerialDevice.isr();
#endif        
    }
    
    EmptyTheBuffer()
    {
        loop
        {
            IsAvailable();
            if (Z) { break; }
            WaitForChar();
        }
    }
    
    // returns Z flag clear if there is a character available in the buffer, Z set if not (disables and enables interrupts)
    IsAvailable()
    {
#ifndef HAS_SERIAL_ISR
        SerialDevice.pollRead(); // munts 
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
#ifdef CPU_65C02S
            PHX
#else      
            TXA PHA
#endif        
            LDX Serial.InReadPointer
            LDA Serial.InBuffer, X
            INC Serial.InReadPointer
#ifdef CPU_65C02S
            PLX       
#else
            STA WorkSpaceWaitForChar
            PLA TAX
            LDA WorkSpaceWaitForChar
#endif
        }
    }
       
    // transmits A
    WriteChar()
    {
#ifndef HAS_SERIAL_ISR
        SerialDevice.pollRead();  // munts 
#endif                
        SerialDevice.writeChar();
    }
    // transmits A as two hex characters
    HexOut()
    {
        // consider tight nibble swap in future:
        // http://6502.org/source/general/SWN.html in future
        
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
    //    uses ZP.U0
    HexIn()
    {
        Serial.WaitForChar();
        Utilities.MakeNibble();
        ASL ASL ASL ASL
        AND #0xF0
        STA WorkSpaceHexIn
        Serial.WaitForChar();
        Utilities.MakeNibble();
        ORA WorkSpaceHexIn
    }
}
