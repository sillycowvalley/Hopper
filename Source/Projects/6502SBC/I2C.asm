unit I2C
{
    #define I2C
    
    uses "/Source/Runtime/6502/ZeroPage"
    
    const byte SCL     = 0b00000001; // DRB0 bitmask
    const byte SCL_INV = 0b11111110; //   inverted for easy clear bit
    const byte SDA     = 0b00000010; // DRB1 bitmask
    const byte SDA_INV = 0b11111101; //   inverted for easy clear bit
    
    delay()
    {
        PHX
        LDX # 0x01
        loop
        {
            DEX
            if (Z) { break; }
        }
        PLX
    }
    sclLow()
    {
        RMB0 PORTB
        SMB0 DDRB    
    }
    sclHigh()
    {
        RMB0 DDRB    
    }
    sdaLow()
    {
        RMB1 PORTB
        SMB1 DDRB    
    }
    sdaHigh()
    {
        RMB1 DDRB    
    }
    sdaRead()
    {
        RMB1 DDRB
        LDA  PORTB
        AND  SDA // Z set or clear depending on bit
    }
    checkAck()
    {
        sclHigh();
        delay();
        sdaRead();
        sclLow();
    }
    
    Initialize()
    {
        sdaHigh();
        sclHigh();
    }
    Start()
    {
        
        LDA I2CADDR
        ROL            // shift in carry: set means read, clear means write
        STA ZP.OUTB    // Save addr + r/w bit
        
        sdaHigh();
        delay();
        sclHigh();
        delay();
        sdaLow();
        delay();
        sclLow();
        delay();
        
        LDA # '['
        Serial.WriteChar();
        
        // send address + RW bit
        ByteOut();
    }
    
    ByteOut() // clears outb
    {
        LDA ZP.OUTB
        PHA
        
        LDA # ' '
        Serial.WriteChar();
        PLA
        PHA
        Serial.HexOut();
        
        loop
        {
            ASL ZP.OUTB // MSB to carry
            if (C)
            {
                sdaLow();
            }
            else
            {
                sdaHigh();
            }

            delay();
            sclHigh();
            delay();
            sclLow();
            delay();
            
            DEX
            if (Z) { break; }
        }
        
        checkAck();
        
        PLA
        STA ZP.OUTB
    }
    Stop()
    {
        sdaLow();
        delay();
        sclHigh();
        delay();
        sdaHigh();
        delay();
        
        LDA # ']'
        Serial.WriteChar();
        LDA # 0x0A
        Serial.WriteChar();
        
    }
}
