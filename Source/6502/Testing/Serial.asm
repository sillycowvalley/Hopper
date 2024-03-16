unit Serial
{
    const byte ACIADATA    = 0xEC;
    const byte ACIASTATUS  = 0xED;
    const byte ACIACOMMAND = 0xEE;
    const byte ACIACONTROL = 0xEF;
    
    const byte SerialInWritePointer = 0xD3;
    const byte SerialInReadPointer  = 0xD4;
    const byte SerialBreakFlag      = 0xDE;
    
    const uint SerialInBuffer       = 0x0300;  // 256-byte buffer 0x0300-0x03FF
    
    // initialize or reset the serial ACIA firmware
    Initialize()
    {
        PHA
        
        // reset buffer so at least start and end are the same
        SEI                    // disable interrupts
        STZ SerialInWritePointer
        STZ SerialInReadPointer
        STZ SerialBreakFlag
        CLI                    // enable interrupts
        
        LDA #0b00000011        // reset the 6850
        STA ACIACONTROL
        
        // LDA #0b00010101     // 8-N-1, 115200 baud (/16 for  1.8432 mHz), no rx interrupt
        // LDA #0b00010110     // 8-N-1,  28800 baud (/64 for  1.8432 mHz), no rx interrupt
        LDA #0b10010110        // 8-N-1,  28800 baud (/64 for  1.8432 mHz), rx interrupt
        STA ACIACONTROL
        
        PLA
    }
    
    ISR()
    {
        PHA
        LDA ACIADATA   // read serial byte
        CMP #0x03      // is it break? (<ctrl><C>)
        if (Z)
        {
            INC SerialBreakFlag
        }
        else
        {
            PHX
            LDX SerialInWritePointer    // push it into serial input buffer
            STA SerialInBuffer, X
            INC SerialInWritePointer
            PLX
        }
        PLA
    }
    
    // returns Z flag clear if there is a character available in the buffer, Z set if not (disables and enables interrupts)
    IsAvailable()
    {
                                  //  6 - jsr
        SEI                       //  2 - disable interrupts
        LDA SerialInReadPointer   //  3
        CMP SerialInWritePointer  //  3
        CLI                       //  2 - enable interrupts
                                  //  6- rts
                                  // 22
    }
    
    // consumes the next character from the buffer and returns value in A
    ReadChar()
    {
        PHX
  
        LDX SerialInReadPointer
        LDA SerialInBuffer, X
        INC SerialInReadPointer
        
        PLX
    }
    
    // transmits A
    WriteChar()
    {
        PHX
        PHA
        loop
        {
            LDA ACIASTATUS  
            
            AND #0b00000010    // Bit 1 - Transmit Data Register Empty (TDRE)
            if (NZ) { break; } // loop if not ready (bit set means TDRE is empty and ready)
        } // loop
        PLA
        STA ACIADATA           // output character to TDRE
        PLX
    }
}
