unit SerialDevice
{
    // Motorolla 6850
    #define HAS_SERIAL_ISR
    
    // On the 6850 the control and status registers are at the same
    // address and are selected based on R or W.
    // If these addresses are bytes, then they are on zero page, if uint regular memory space.
    //
    const byte ControlRegister    = ZP.ACIACONTROL;
    const byte DataRegister       = ZP.ACIADATA;
    const byte StatusRegister     = ZP.ACIASTATUS;
    
    
    friend Serial;
     
    initialize()
    {
        LDA #0b00000011        // reset the 6850
        STA ControlRegister
        
        // LDA #0b00010101     // 8-N-1, 115200 baud (/16 for  1.8432 mHz), no rx interrupt
        // LDA #0b00010110     // 8-N-1,  28800 baud (/64 for  1.8432 mHz), no rx interrupt
        LDA #0b10010110        // 8-N-1,  28800 baud (/64 for  1.8432 mHz), rx interrupt
        STA ControlRegister
    }
    
    writeChar()
    {
        PHA
        loop
        {
            LDA StatusRegister  
            AND #0b00000010   // Bit 1 - Transmit Data Register Empty (TDRE)
            if (NZ) { break; } // loop if not ready (bit set means TDRE is empty and ready)
        } // loop
        PLA
        STA DataRegister           // output character to TDRE
    }
    
    isr()
    {
        if (BBS7, StatusRegister) // interrupt request by 6850
        {
            if (BBS0, StatusRegister) // RDRF : receive data register full
            {
                PHA
                PHA
                LDA DataRegister // read serial byte
                CMP #0x03               // is it break? (<ctrl><C>)
                if (Z)
                {
                    INC Serial.BreakFlag
                }
                else
                {
                    PHX
                    LDX Serial.InWritePointer    // push it into serial input buffer
                    STA Serial.InBuffer, X
                    INC Serial.InWritePointer
                    PLX
                }
                PLA
            }
        }
    }
}
