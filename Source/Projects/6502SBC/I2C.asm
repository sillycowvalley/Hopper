unit I2C
{
    // Original written by Anders Nielsen, 2023-2024
    // License: https://creativecommons.org/licenses/by-nc/4.0/legalcode
    
    // https://github.com/AndersBNielsen/65uino/blob/main/i2c.s
    
    uses "/Source/Runtime/6502/ZeroPage"
    
    const byte SCL     = 0b00000001;    // DRB0 bitmask
    const byte SCL_INV = 0b11111110;    //   inverted for easy clear bit
    const byte SDA     = 0b00000010;    // DRB1 bitmask
    const byte SDA_INV = 0b11111101;    //   inverted for easy clear bit
      
    Start()
    {
        //LDA # 0x0A
        //Serial.WriteChar();
        //LDA # '['
        //Serial.WriteChar();
        
        LDA ZP.I2CADDR
        ROL                // Shift in carry
        STA ZP.OutB        // Save addr + r/w bit

        LDA # SCL_INV
        AND ZP.DDRB
        STA ZP.DDRB      // Start with SCL as input HIGH - that way we can inc/dec from here
        
        LDA # SDA          // Ensure SDA is output low before SCL is LOW
        ORA ZP.DDRB
        STA ZP.DDRB
        LDA # SDA_INV
        AND ZP.PORTB
        STA ZP.PORTB
        
        LDA # SCL_INV      // Ensure SCL is low when it turns to output
        AND ZP.PORTB
        STA ZP.PORTB
        INC ZP.DDRB      // Set to output by incrementing the direction register == OUT, LOW
        
        ByteOut();
    } 
    ByteOut() // clears ZP.OutB
    {
        //LDA # ' '
        //Serial.WriteChar();
        //LDA ZP.OutB
        //Serial.HexOut();
        
#ifdef CPU_65C02S
        PHX
#else
        TXA PHA
#endif
        
        LDA # SDA_INV // In case this is a data byte we set SDA LOW
        AND ZP.PORTB
        STA ZP.PORTB
        LDX # 8
        JMP first     // skip INC since first time already out, low
        loop
        {
            INC ZP.DDRB  // SCL out, low
first:
            ASL ZP.OutB    // MSB to carry
            if (C)
            {
                LDA ZP.DDRB
                AND # SDA_INV
                STA ZP.DDRB
            }
            else
            {
                LDA ZP.DDRB
                ORA # SDA
                STA ZP.DDRB
            }
            DEC ZP.DDRB
            DEX
            if (Z) { break; }
        }

        INC ZP.DDRB

        LDA ZP.DDRB // Set SDA to INPUT (HIGH)
        AND # SDA_INV
        STA ZP.DDRB

        DEC ZP.DDRB // Clock high
        LDA ZP.PORTB  // Check ACK bit
        SEC
        AND # SDA
        if (Z)
        {
            CLC       // Clear carry on ACK
        }
        INC ZP.DDRB // SCL low
        
#ifdef CPU_65C02S
        PLX
#else
        PLA TAX
#endif
    } 
    ByteIn()
    {
        // Assume SCL is low from address byte
        LDA ZP.DDRB     // SDA, input
        AND # SDA_INV
        STA ZP.DDRB
        LDA # 0
        STA ZP.InB
        LDX # 8
        loop
        {
            CLC
            DEC ZP.DDRB // SCL HIGH
            LDA ZP.PORTB  // Let's read after SCL goes high
            AND # SDA
            if (NZ)
            {
                SEC
            }
            ROL ZP.InB    // Shift bit into the input byte
            INC ZP.DDRB // SCL LOW
            DEX
            if (Z) { break; }
        }
        
        LDA ZP.DDRB     // Send NACK == SDA high (only single bytes for now)
        AND # SDA_INV
        STA ZP.DDRB
        DEC ZP.DDRB     // SCL HIGH
        INC ZP.DDRB     // SCL LOW
    }
    Stop()
    {
        LDA ZP.DDRB // SDA low
        ORA # SDA
        STA ZP.DDRB
        DEC ZP.DDRB // SCL HIGH
        LDA ZP.DDRB // Set SDA high after SCL == Stop condition
        AND # SDA_INV
        STA ZP.DDRB
        
        //LDA # ']'
        //Serial.WriteChar();
    }
}
