unit Serial // Serial.asm
{
    
#if defined(ACIA_6850)
    uses "Devices/ACIA6850"
#endif

    // initialize or reset the serial ACIA firmware
    Initialize()
    {
        // reset buffer so at least start and end are the same
        SEI                    // disable interrupts
        
#ifdef UNIVERSAL        
        LDA #0
        STA ZP.SerialInWritePointer
        STA ZP.SerialInReadPointer
        LDA #0b11111010
        AND ZP.FLAGS
        STA ZP.FLAGS
#else
        STZ ZP.SerialInWritePointer
        STZ ZP.SerialInReadPointer
        RMB2 ZP.FLAGS // XON / XOFF
        RMB0 ZP.FLAGS // NMI break
#endif        
        CLI                    // enable interrupts
        
        SerialDevice.initialize(); // device-specific initialization
    }
    
    EmptyTheBuffer()
    {
        loop
        {
            IsAvailable();
            if (NC) { break; } // no more
            WaitForChar();
        }
    }
    
    ISR()
    {
        SerialDevice.isr();
        if (C)                       // Byte was added to buffer?
        {
            // Check if we need to send XOFF
            PHA
            LDA ZP.SerialInWritePointer
            SEC
            SBC ZP.SerialInReadPointer
            CMP #240              // Nearly full? (240/256 bytes used)
            if (C)                // Carry set if >= 240
            {
#ifdef UNIVERSAL                
                LDA ZP.FLAGS
                AND #0b00000100
                if (Z)  // Bit 2 clear? (not stopped yet)
                {
                    TXA PHA
                    LDA # Char.XOFF
                    SerialDevice.writeChar();  // Send XOFF
                    PLA TAX
                    
                    LDA #0b00000100
                    ORA ZP.FLAGS     // Set bit 2 (XOFF sent)
                    STA ZP.FLAGS
                }
#else
                if (BBR2, ZP.FLAGS)  // Bit 2 clear? (not stopped yet)
                {
                    PHX
                    LDA # Char.XOFF
                    SerialDevice.writeChar();  // Send XOFF
                    PLX
                    
                    SMB2 ZP.FLAGS     // Set bit 2 (XOFF sent)
                }
#endif                
            }
            PLA
        }
    }

    // returns C flag set if there is a character available in the buffer, NC if not (disables and enables interrupts)
    IsAvailable()
    {
        SEI
#ifdef UNIVERSAL        
        PHA
        LDA ZP.FLAGS
        AND #0b00000001
        if (NZ) // Bit 0 set? (break detected)
        {
            // <ctrl><C> is avaiable    
        }
        else
        {
            LDA ZP.SerialInReadPointer
            CMP ZP.SerialInWritePointer
            // Z means no characters available in buffer
            if (Z)
            {
                PLA
                CLC
                CLI
                return;
            }
        }
        PLA
#else
        if (BBS0, ZP.FLAGS)   // Bit 0 set? (break detected)
        {
            // <ctrl><C> is avaiable    
        }
        else
        {
            LDA ZP.SerialInReadPointer
            CMP ZP.SerialInWritePointer
            // Z means no characters available in buffer
            if (Z)
            {
                CLC
                CLI
                return;
            }
        }
#endif        
        SEC
        CLI
    }
    
    // consumes the next character from the buffer and returns value in A
    WaitForChar()
    {
        loop
        {
            IsAvailable();
            if (C) { break; }
        }
#ifdef UNIVERSAL
        LDA ZP.FLAGS
        AND #00000001
        if (NZ) // break?
        {
            LDA #0b11111110
            AND ZP.FLAGS
            STA ZP.FLAGS
            LDA # Char.CtrlC // <ctrl><C>
        }
        else
        {
            TXA PHA
            LDX ZP.SerialInReadPointer
            LDA Address.SerialInBuffer, X
            INC ZP.SerialInReadPointer
            STA ZP.TEMP
            
            // Check if we can send XON after consuming byte  
            LDA #0b00000100
            if (NZ)      // Bit 2 set? (currently stopped)
            {
                LDA ZP.SerialInWritePointer
                SEC
                SBC ZP.SerialInReadPointer
                CMP #16                  // Mostly empty? (only 16/256 bytes used)
                if (NC)                  // Carry clear if < 16
                {
                    LDA # Char.XON
                    SerialDevice.writeChar();  // Send XON
                    
                    LDA #0b11111011
                    AND ZP.FLAGS
                    STA ZP.FLAGS     // Clear bit 2 (resume flow)
                }
            }
            PLA TAX
            LDA ZP.TEMP
        }
#else
        if (BBS0, ZP.FLAGS) // break?
        {
            RMB0 ZP.FLAGS
            LDA # Char.CtrlC // <ctrl><C>
        }
        else
        {
            PHX
            LDX ZP.SerialInReadPointer
            LDA Address.SerialInBuffer, X
            INC ZP.SerialInReadPointer
            TAX
            
            // Check if we can send XON after consuming byte
            if (BBS2, ZP.FLAGS)      // Bit 2 set? (currently stopped)
            {
                LDA ZP.SerialInWritePointer
                SEC
                SBC ZP.SerialInReadPointer
                CMP #16                  // Mostly empty? (only 16/256 bytes used)
                if (NC)                  // Carry clear if < 16
                {
                    LDA # Char.XON
                    SerialDevice.writeChar();  // Send XON
                    
                    RMB2 ZP.FLAGS     // Clear bit 2 (resume flow)
                }
            }
            TXA
            PLX
        }        
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
        // consider tight nibble swap in future:
        // http://6502.org/source/general/SWN.html in future
        
        PHA
        PHA
        
        // most significant nibble
        LSR A LSR A LSR A LSR A
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
    
    // Converts '0'..'9' or 'A'..'F' to 4-bit value
    // only uses A
    makeNibble()
    {
        // only touches A
        CMP # ('9'+1)
        if (C)
        {
            SBC # (7+1)
        }
        SBC     # ('0'-1)
        AND     # 0x0F
    }
    
    // loads two hex characters from Serial to byte in A
    //    uses ZP.TEMP
    HexIn()
    {
        Serial.WaitForChar();
        makeNibble();
        ASL A ASL A ASL A ASL A
        AND #0xF0
        STA ZP.TEMP
        Serial.WaitForChar();
        makeNibble();
        ORA ZP.TEMP
    }
}
