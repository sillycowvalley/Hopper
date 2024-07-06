unit SerialDevice
{
    // Motorola 6850
    #define HAS_SERIAL_ISR
    
    // On the 6850 the control and status registers are at the same
    // address and are selected based on R or W.
    // If these addresses are bytes, then they are on zero page, if uint regular memory space.
    //
#ifdef ZEROPAGE_IO    
    const byte ControlRegister    = ZP.ACIACONTROL;
    const byte DataRegister       = ZP.ACIADATA;
    const byte StatusRegister     = ZP.ACIASTATUS;
#else
    const uint ControlRegister    = ZP.ACIACONTROL;
    const uint DataRegister       = ZP.ACIADATA;
    const uint StatusRegister     = ZP.ACIASTATUS;
#endif
    
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
#if defined(CPU_65C02S) && defined(ZEROPAGE_IO)
        loop
        {
            if (BBS1, StatusRegister) { break; } // loop if not ready (bit set means TDRE is empty and ready)
        } // loop
        STA DataRegister           // output character to TDRE
#else
        PHA
        loop
        {
            LDA StatusRegister  
            AND #0b00000010    // Bit 1 - Transmit Data Register Empty (TDRE)
            if (NZ) { break; } // loop if not ready (bit set means TDRE is empty and ready)
        } // loop
        PLA
        STA DataRegister           // output character to TDRE
#endif
    }
    
    isr()
    {
#if defined(CPU_65C02S) && defined(ZEROPAGE_IO)
        if (BBS7, StatusRegister) // interrupt request by 6850
        {
            if (BBS0, StatusRegister) // RDRF : receive data register full
            {
                PHA
                LDA DataRegister        // read serial byte
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
#else
        PHA
        LDA StatusRegister 
        AND # 0b10000000
        if (NZ) // interrupt request by 6850
        {
            LDA StatusRegister 
            AND # 0b00000001
            if (NZ) // RDRF : receive data register full
            {
                TXA PHA // can't use XREG in ISR
                LDA DataRegister // read serial byte
                CMP #0x03               // is it break? (<ctrl><C>)
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
                PLA TAX // can't use XREG in ISR
            }
        }
        PLA
#endif        
    }
    
#ifndef HAS_SERIAL_ISR    
    // munts X
    pollRead()
    {
        PHA
        //LDA StatusRegister 
        //AND # 0b10000000
        //if (NZ) // interrupt request by 6850
        //{
            LDA StatusRegister 
            AND # 0b00000001
            if (NZ) // RDRF : receive data register full
            {
                TXA PHA // can't use XREG in ISR
                LDA DataRegister // read serial byte
                CMP #0x03               // is it break? (<ctrl><C>)
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
                PLA TAX // can't use XREG in ISR
            }
        //}
        PLA        
    }
#endif    
}
