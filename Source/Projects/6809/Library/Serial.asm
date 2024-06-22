unit Serial
{
    uses "ZeroPage"
    uses "MemoryMap"
    
    // Zero Page locations used by Serial:
    const byte InWritePointer        = ZP.SerialInWritePointer;
    const byte InReadPointer         = ZP.SerialInReadPointer;
    const byte BreakFlag             = ZP.SerialBreakFlag;
    const byte WorkSpaceHexIn        = ZP.W0;
    
    // Location of the Serial input buffer (256 bytes)
    const uint InBuffer        = Address.SerialInBuffer;
#ifdef ACIA_6850    
    uses "Devices/ACIA6850"
#endif
#ifdef ACIA_6551
    uses "Devices/ACIA6551"
#endif
#ifdef APPLE_I
    uses "Devices/AppleI"
#endif    
    // initialize or reset the serial ACIA firmware
    Initialize()
    {
        // reset buffer so at least start and end are the same
        ORCC #0x50                // Disable interrupts (SEI)
        CLR Serial.InWritePointer
        CLR Serial.InReadPointer
        CLR Serial.BreakFlag
        ANDCC #0xAF                // Enable interrupts (CLI)
        
        JSR SerialDevice.initialize // device-specific initialization
    }
    
    ISR()
    {
#ifdef HAS_SERIAL_ISR        
        JSR SerialDevice.isr
#endif        
    }
    
    EmptyTheBuffer()
    {
        loop
        {
            JSR IsAvailable
            if (Z) { break; }
            JSR WaitForChar
        }
    }
    
    // returns Z flag clear if there is a character available in the buffer, Z set if not (disables and enables interrupts)
    IsAvailable()
    {
#ifndef HAS_SERIAL_ISR
        JSR SerialDevice.pollRead // munts 
#endif        
        ORCC #0x50                // Disable interrupts (SEI)
        LDA BreakFlag
        if (Z)
        {
            LDA InReadPointer
            CMPA InWritePointer
        }
        ANDCC #0xAF                // Enable interrupts (CLI)
    }
    
    // consumes the next character from the buffer and returns value in A
    WaitForChar()
    {
        loop
        {
            JSR IsAvailable
            if (NZ) { break; }
        }
        LDA ZP.SerialBreakFlag
        if (NZ)
        {
            ORCC #0x50                // Disable interrupts (SEI)
            DEC Serial.BreakFlag
            ANDCC #0xAF                // Enable interrupts (CLI)
            LDA #0x03
        }
        else
        {
            PSHS X
            LDX Serial.InReadPointer
            LDA Serial.InBuffer, X
            INC Serial.InReadPointer
            PULS X
        }
    }
       
    // transmits A
    WriteChar()
    {
#ifndef HAS_SERIAL_ISR
        JSR SerialDevice.pollRead  // munts 
#endif                
        JSR SerialDevice.writeChar
    }
    
    // transmits A as two hex characters
    HexOut()
    {
        PSHS A
        PSHS A
        
        // most significant nibble
        LSR A
        LSR A
        LSR A
        LSR A
        CMPA #0x0A
        if (C)
        {
            ADDA #0x06 // add 6 if carry set
        }
        ADDA #0x30
        JSR WriteChar
        
        // least significant nibble
        PULS A
        ANDA #0x0F
        CMPA #0x0A
        if (C)
        {
            ADDA #0x06 // add 6 if carry set
        }
        ADDA #0x30
        JSR WriteChar
        
        PULS A
    }
    
    // loads two hex characters from Serial to byte in A
    // uses ZP.U0
    HexIn()
    {
        JSR WaitForChar
        JSR Utilities.MakeNibble
        LSL A
        LSL A
        LSL A
        LSL A
        ANDA #0xF0
        STA WorkSpaceHexIn
        JSR WaitForChar
        JSR Utilities.MakeNibble
        ORA WorkSpaceHexIn
    }
}

