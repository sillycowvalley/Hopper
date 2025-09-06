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
        STZ ZP.SerialInWritePointer
        STZ ZP.SerialInReadPointer
        RMB2 ZP.FLAGS // XON / XOFF
        RMB0 ZP.FLAGS // NMI break
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
                if (BBR2, ZP.FLAGS)  // Bit 2 clear? (not stopped yet)
                {
                    PHX
                    LDA # Char.XOFF
                    SerialDevice.writeChar();  // Send XOFF
                    PLX
                    
                    SMB2 ZP.FLAGS     // Set bit 2 (XOFF sent)
                }
            }
            PLA
        }
    }

    // returns C flag set if there is a character available in the buffer, NC if not (disables and enables interrupts)
    IsAvailable()
    {
        SEI
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
        SEC
        CLI
    }
    
    // consumes the next character from the buffer and returns value in A
    //     munts X on CPU_6502
    WaitForChar()
    {
        loop
        {
            IsAvailable();
            if (C) { break; }
        }
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
            PLX     
            
            // Check if we can send XON after consuming byte
            if (BBS2, ZP.FLAGS)      // Bit 2 set? (currently stopped)
            {
                PHA
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
                PLA
            }
        }
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
