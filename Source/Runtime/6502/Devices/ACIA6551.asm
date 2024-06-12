unit SerialDevice
{
    // Rockwell 6551
    //#define HAS_SERIAL_ISR
    
    const uint ControlRegister    = ZP.ACIACONTROL;
    const uint CommandRegister    = ZP.ACIACOMMAND;
    const uint DataRegister       = ZP.ACIADATA;
    const uint StatusRegister     = ZP.ACIASTATUS;
    
    
    const byte ACIA_PARITY_DISABLE          = 0b00000000;
    const byte ACIA_ECHO_DISABLE            = 0b00000000;
    const byte ACIA_TX_INT_DISABLE_RTS_LOW  = 0b00001000;
    const byte ACIA_RX_INT_ENABLE           = 0b00000000;
    const byte ACIA_RX_INT_DISABLE          = 0b00000010;
    const byte ACIA_DTR_LOW                 = 0b00000001;
    
    friend Serial;
     
    initialize()
    {
        LDA #0x00
        STA StatusRegister
        LDA # (ACIA_PARITY_DISABLE | ACIA_ECHO_DISABLE | ACIA_TX_INT_DISABLE_RTS_LOW | ACIA_RX_INT_DISABLE | ACIA_DTR_LOW)
        STA CommandRegister
     // LDA #0x10                 // 16 x external clock which if running a 1.8432 MZ can, you get 115200 baud.
        LDA #0x1F                 // This gives 19200 baud rate generator.
        STA ControlRegister
    }
    
    writeChar()
    {
        PHA
        loop
        {
            LDA StatusRegister  
            AND #0x10    // Bit 1 - Transmit Data Register Empty (TDRE)
            if (NZ) { break; } // loop if not ready (bit set means TDRE is empty and ready)
        } // loop
        PLA
        STA DataRegister           // output character to TDRE
    }
    
    isr()
    {       
    }
    
    // munts X
    pollRead()
    {
        PHA
        {
            LDA StatusRegister 
            AND # 0x08
            if (NZ) // RDRF : receive data register full
            {
                TXA PHA
                LDA DataRegister // read serial byte
                CMP #0x03        // is it break? (<ctrl><C>)
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
    }
}
