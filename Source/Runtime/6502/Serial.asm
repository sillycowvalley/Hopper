unit Serial // Serial.asm
{
    // Zero Page locations used by Serial:
    const byte InWritePointer        = ZP.SerialInWritePointer;
    const byte InReadPointer         = ZP.SerialInReadPointer;
    
#ifndef HOPPER_BASIC
    const byte BreakFlag             = ZP.SerialBreakFlag;
    const byte WorkSpaceHexIn        = ZP.W0;
  #ifndef CPU_65C02S    
    const byte WorkSpaceWaitForChar  = ZP.W1;
  #endif
#else
    const byte XOFF = 0x13;  // Ctrl-S
    const byte XON  = 0x11;  // Ctrl-Q
  
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
  #ifdef HOPPER_BASIC
        STZ ZP.SerialFlags
  #else        
        STZ Serial.BreakFlag
  #endif
#else
        LDA #0
        STA Serial.InWritePointer
        STA Serial.InReadPointer
        STA Serial.BreakFlag
#endif
        CLI                    // enable interrupts
        
        SerialDevice.initialize(); // device-specific initialization
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
    
        
    
#ifdef HOPPER_BASIC

    ISR()
    {
        SerialDevice.isr();
        if (C)                       // Byte was added to buffer?
        {
            // Check if we need to send XOFF
            PHA
            LDA InWritePointer
            SEC
            SBC InReadPointer
            CMP #240              // Nearly full? (240/256 bytes used)
            if (C)                // Carry set if >= 240
            {
                if (BBR1, SerialFlags)  // Bit 1 clear? (not stopped yet)
                {
                    PHX
                    LDA #0x13     // XOFF
                    SerialDevice.writeChar();  // Send XOFF
                    PLX
                    
                    SMB1 SerialFlags     // Set bit 1 (XOFF sent)
                }
            }
            PLA
        }
    }

    // returns Z flag clear if there is a character available in the buffer, Z set if not (disables and enables interrupts)
    IsAvailable()
    {
        SEI
        if (BBS0, ZP.SerialFlags)   // Bit 0 set? (break detected)
        {
            LDA #1 // <ctrl><C> is avaiable    
        }
        else
        {
            LDA InReadPointer
            CMP InWritePointer
            // NZ means characters available in buffer
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
        if (BBS0, ZP.SerialFlags) // break?
        {
            RMB0 ZP.SerialFlags
            LDA #0x03 // <ctrl><C>
        }
        else
        {
            PHX
            LDX Serial.InReadPointer
            LDA Serial.InBuffer, X
            INC Serial.InReadPointer
            PLX     
            
            // Check if we can send XON after consuming byte
            if (BBS1, ZP.SerialFlags)      // Bit 1 set? (currently stopped)
            {
                PHA
                LDA Serial.InWritePointer
                SEC
                SBC Serial.InReadPointer
                CMP #16                  // Mostly empty? (only 16/256 bytes used)
                if (NC)                  // Carry clear if < 16
                {
                    LDA #XON
                    SerialDevice.writeChar();  // Send XON
                    
                    RMB1 SerialFlags     // Clear bit 1 (resume flow)
                }
                PLA
            }
        }
    }
    
#else  

    ISR()
    {
#ifdef HAS_SERIAL_ISR        
        SerialDevice.isr();
#endif        
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
#endif
       
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
