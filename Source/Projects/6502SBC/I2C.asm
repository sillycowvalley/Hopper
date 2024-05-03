unit I2C
{
    #define I2C
    
    uses "/Source/Runtime/6502/ZeroPage"
    
    const byte SCL     = 0b00000001; // DRB0 bitmask
    const byte SCL_INV = 0b11111110; //   inverted for easy clear bit
    const byte SDA     = 0b00000010; // DRB1 bitmask
    const byte SDA_INV = 0b11111101; //   inverted for easy clear bit
    
    Start()
    {
        LDA I2CADDR
        ROL            // Shift in carry
        STA ZP.OUTB    // Save addr + r/w bit
        
        LDA #SCL_INV
        AND ZP.DDRB
        STA ZP.DDRB    // Start with SCL as input HIGH - that way we can inc/dec from here
        
        LDA #SDA       // Ensure SDA is output low before SCL is LOW
        ORA ZP.DDRB
        STA ZP.DDRB
        LDA #SDA_INV
        AND ZP.PORTB
        STA ZP.PORTB
        
        LDA #SCL_INV   // Ensure SCL is low when it turns to output
        AND ZP.PORTB
        STA ZP.PORTB
        INC ZP.DDRB    // Set to output by incrementing the direction register == OUT, LOW
        // send address + RW bit
        ByteOut();
    }
    
    ByteOut() // clears outb
    {
        LDA #SDA_INV // In case this is a data byte we set SDA LOW
        AND ZP.PORTB
        STA ZP.PORTB
        LDX #8
        JMP FIRST  // BRA - skip INC since first time already out, low
        loop
        {
            INC DDRB // SCL out, low
FIRST:
            ASL ZP.OUTB // MSB to carry
            if (C)
            {
                LDA DDRB
                AND #SDA_INV
                STA DDRB
            }
            else
            {
                LDA DDRB
                ORA #SDA
                STA DDRB
            }

            DEC DDRB
            DEX
            if (Z) { break; }
        }
        
        INC DDRB

        LDA DDRB // Set SDA to INPUT (HIGH)
        AND #SDA_INV
        STA DDRB
        
        DEC DDRB   // Clock high
        LDA PORTB  // Check ACK bit
        SEC
        AND #SDA
        if (Z)
        {
            CLC // Clear carry on ACK
        }
        INC DDRB // SCL low
    }
    ByteIn()
    {
        // Assume SCL is low from address byte
        LDA DDRB // SDA, input
        AND #SDA_INV
        STA DDRB
        LDA #0
        STA inb
        LDX # 8
        loop
        {
            CLC
            DEC DDRB // SCL HIGH
            LDA DRB  // Let's read after SCL goes high
            AND # SDA
            if (NZ)
            {
                SEC
            }
            ROL inb  // Shift bit into the input byte
            INC DDRB // SCL LOW
            DEX
            if (Z) { break; }
        }
        
        LDA DDRB // Send NACK == SDA high (only single bytes for now)
        AND # SDA_INV
        STA DDRB
        DEC DDRB // SCL HIGH
        INC DDRB // SCL LOW
    }
    Stop()
    {
        LDA ZP.DDRB  // SDA low
        ORA #SDA
        STA ZP.DDRB
        DEC ZP.DDRB  // SCL HIGH
        LDA ZP.DDRB  // Set SDA high after SCL == Stop condition
        AND #SDA_INV
        STA ZP.DDRB
    }
}
