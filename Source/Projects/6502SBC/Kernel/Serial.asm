unit Serial // Serial.asm
{
    const byte CTRLC = 0x03; // <ctrL><C>
    const byte XOFF  = 0x13; // <ctrl><S>
    const byte XON   = 0x11; // <ctrl><Q>
    
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
        RMB1 ZP.FLAGS // XON / XOFF
        RMB0 ZP.FLAGS // NMI break
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
                if (BBR1, ZP.FLAGS)  // Bit 1 clear? (not stopped yet)
                {
                    PHX
                    LDA # XOFF
                    SerialDevice.writeChar();  // Send XOFF
                    PLX
                    
                    SMB1 ZP.FLAGS     // Set bit 1 (XOFF sent)
                }
            }
            PLA
        }
    }

    // returns Z flag clear if there is a character available in the buffer, Z set if not (disables and enables interrupts)
    IsAvailable()
    {
        SEI
        if (BBS0, ZP.FLAGS)   // Bit 0 set? (break detected)
        {
            LDA #1 // <ctrl><C> is avaiable    
        }
        else
        {
            LDA ZP.SerialInReadPointer
            CMP ZP.SerialInWritePointer
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
        if (BBS0, ZP.FLAGS) // break?
        {
            RMB0 ZP.FLAGS
            LDA # CTRLC // <ctrl><C>
        }
        else
        {
            PHX
            LDX ZP.SerialInReadPointer
            LDA Address.SerialInBuffer, X
            INC ZP.SerialInReadPointer
            PLX     
            
            // Check if we can send XON after consuming byte
            if (BBS1, ZP.FLAGS)      // Bit 1 set? (currently stopped)
            {
                PHA
                LDA ZP.SerialInWritePointer
                SEC
                SBC ZP.SerialInReadPointer
                CMP #16                  // Mostly empty? (only 16/256 bytes used)
                if (NC)                  // Carry clear if < 16
                {
                    LDA #XON
                    SerialDevice.writeChar();  // Send XON
                    
                    RMB1 ZP.FLAGS     // Clear bit 1 (resume flow)
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
    
    // loads two hex characters from Serial to byte in A
    //    uses ZP.U0
    /*
    HexIn()
    {
        Serial.WaitForChar();
        Utilities.MakeNibble();
        ASL A ASL A ASL A ASL A
        AND #0xF0
        STA WorkSpaceHexIn
        Serial.WaitForChar();
        Utilities.MakeNibble();
        ORA WorkSpaceHexIn
    }
    */
}
