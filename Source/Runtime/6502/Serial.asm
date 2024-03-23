unit Serial
{
    
    // initialize or reset the serial ACIA firmware
    Initialize()
    {
        PHA
        
        // reset buffer so at least start and end are the same
        SEI                    // disable interrupts
        STZ ZP.SerialInWritePointer
        STZ ZP.SerialInReadPointer
        //STZ ZP.SerialBreakFlag
        CLI                    // enable interrupts
        
        LDA #0b00000011        // reset the 6850
        STA ZP.ACIACONTROL
        
        // LDA #0b00010101     // 8-N-1, 115200 baud (/16 for  1.8432 mHz), no rx interrupt
        // LDA #0b00010110     // 8-N-1,  28800 baud (/64 for  1.8432 mHz), no rx interrupt
        LDA #0b10010110        // 8-N-1,  28800 baud (/64 for  1.8432 mHz), rx interrupt
        STA ZP.ACIACONTROL
        
        PLA
    }
    
    ISR()
    {
        PHA
        LDA ZP.ACIADATA   // read serial byte
        CMP #0x03         // is it break? (<ctrl><C>)
        if (Z)
        {
            INC ZP.SerialBreakFlag
        }
        else
        {
            PHX
            LDX ZP.SerialInWritePointer    // push it into serial input buffer
            STA Address.SerialInBuffer, X
            INC ZP.SerialInWritePointer
            PLX
        }
        PLA
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
        SEI
        LDA ZP.SerialBreakFlag
        if (Z)
        {
            LDA ZP.SerialInReadPointer
            CMP ZP.SerialInWritePointer
        }
        CLI
    }
    
    // consumes the next character from the buffer and returns value in A
    WaitForChar()
    {
        loop
        {
            IsAvailable();
            if (NZ) { break; }
        }
        PHX
        LDA ZP.SerialBreakFlag
        if (NZ)
        {
            SEI
            DEC ZP.SerialBreakFlag
            CLI
            LDA #0x03
        }
        else
        {
            LDX ZP.SerialInReadPointer
            LDA Address.SerialInBuffer, X
            INC ZP.SerialInReadPointer
        }
        PLX
    }
    
       
    // transmits A
    WriteChar()
    {
        PHX
        PHA
        loop
        {
            LDA ZP.ACIASTATUS  
            AND #0b00000010   // Bit 1 - Transmit Data Register Empty (TDRE)
            if (NZ) { break; } // loop if not ready (bit set means TDRE is empty and ready)
        } // loop
        PLA
        STA ZP.ACIADATA           // output character to TDRE
        PLX
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
    //    uses ZP.ACCL  
    HexIn()
    {
        Serial.WaitForChar();
        Utilities.MakeNibble();
        ASL ASL ASL ASL
        AND #0xF0
        STA ZP.ACCL
        Serial.WaitForChar();
        Utilities.MakeNibble();
        ORA ZP.ACCL
    }
}
