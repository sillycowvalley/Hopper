unit SerialDevice
{
    // Motorola 6850
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
#ifdef HAS_SERIAL_ISR        
        LDA #0b10010110        // 8-N-1,  28800 baud (/64 for  1.8432 mHz), rx interrupt
#else        
        LDA #0b00010110        // 8-N-1,  28800 baud (/64 for  1.8432 mHz), no rx interrupt
#endif
        STA ControlRegister
    }
    
    writeChar()
    {
        loop
        {
            LDA StatusRegister  
            BITA #0b00000010    // Bit 1 - Transmit Data Register Empty (TDRE)
            if (NZ) { break; } // loop if not ready (bit set means TDRE is empty and ready)
        } // loop
        STA DataRegister           // output character to TDRE
    }
    
    isr()
    {
        PSHS A, X, Y, U
        
        LDA StatusRegister 
        BITA #0b10000000
        if (NZ) // interrupt request by 6850
        {
            LDA StatusRegister 
            BITA #0b00000001
            if (NZ) // RDRF : receive data register full
            {
                LDA DataRegister // read serial byte
                CMPA #0x03               // is it break? (<ctrl><C>)
                if (Z)
                {
                    INC Serial.BreakFlag
                }
                else
                {
                    LDX Serial.InWritePointer    // push it into serial input buffer
                    STA Serial.InBuffer, X
                    INC Serial.InWritePointer
                }
            }
        }
        PULS A, X, Y, U
    }
    
#ifndef HAS_SERIAL_ISR    
    // munts X
    pollRead()
    {
        PSHS A, X, Y, U
        LDA StatusRegister 
        BITA #0b00000001
        if (NZ) // RDRF : receive data register full
        {
            LDA DataRegister // read serial byte
            CMPA #0x03               // is it break? (<ctrl><C>)
            if (Z)
            {
                INC Serial.BreakFlag
            }
            else
            {
                LDX Serial.InWritePointer    // push it into serial input buffer
                STA Serial.InBuffer, X
                INC Serial.InWritePointer
            }
        }
        PULS A, X, Y, U
    }
#endif    
}

