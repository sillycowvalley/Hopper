unit I2C
{
    // Original written by Anders Nielsen, 2023-2024
    // License: https://creativecommons.org/licenses/by-nc/4.0/legalcode
    
    // https://github.com/AndersBNielsen/65uino/blob/main/i2c.s
    
    uses "RIOT"
    
    const byte SCL     = 0b00000001;    // DRB0 bitmask
    const byte SCL_INV = 0b11111110;    //   inverted for easy clear bit
    const byte SDA     = 0b00000010;    // DRB1 bitmask
    const byte SDA_INV = 0b11111101;    //   inverted for easy clear bit
      
    Start()
    {
        LDA Sprites.I2CADDR
        ROL                // Shift in carry
        STA Sprites.OutB        // Save addr + r/w bit

        LDA # SCL_INV
        AND RIOT.DDRB
        STA RIOT.DDRB      // Start with SCL as input HIGH - that way we can inc/dec from here
        
        LDA # SDA          // Ensure SDA is output low before SCL is LOW
        ORA RIOT.DDRB
        STA RIOT.DDRB
        LDA # SDA_INV
        AND RIOT.DRB
        STA RIOT.DRB
        
        LDA # SCL_INV      // Ensure SCL is low when it turns to output
        AND RIOT.DRB
        STA RIOT.DRB
        INC RIOT.DDRB      // Set to output by incrementing the direction register == OUT, LOW
        
        ByteOut();
    } 
    ByteOut() // clears ZP.OutB
    {
        TXA PHA
        
        LDA # SDA_INV // In case this is a data byte we set SDA LOW
        AND DRB
        STA DRB
        LDX # 8
        JMP first     // skip INC since first time already out, low
        loop
        {
            INC RIOT.DDRB  // SCL out, low
first:
            ASL Sprites.OutB    // MSB to carry
            if (C)
            {
                LDA RIOT.DDRB
                AND # SDA_INV
                STA RIOT.DDRB
            }
            else
            {
                LDA RIOT.DDRB
                ORA # SDA
                STA RIOT.DDRB
            }
            DEC RIOT.DDRB
            DEX
            if (Z) { break; }
        }

        INC RIOT.DDRB

        LDA RIOT.DDRB // Set SDA to INPUT (HIGH)
        AND # SDA_INV
        STA RIOT.DDRB

        DEC RIOT.DDRB // Clock high
        LDA RIOT.DRB  // Check ACK bit
        SEC
        AND # SDA
        if (Z)
        {
            CLC       // Clear carry on ACK
        }
        INC RIOT.DDRB // SCL low
        
        PLA TAX
    } 
    ByteIn()
    {
        // Assume SCL is low from address byte
        LDA RIOT.DDRB     // SDA, input
        AND # SDA_INV
        STA RIOT.DDRB
        LDA # 0
        STA ZP.InB
        LDX # 8
        loop
        {
            CLC
            DEC RIOT.DDRB // SCL HIGH
            LDA RIOT.DRB  // Let's read after SCL goes high
            AND # SDA
            if (NZ)
            {
                SEC
            }
            ROL ZP.InB    // Shift bit into the input byte
            INC RIOT.DDRB // SCL LOW
            DEX
            if (Z) { break; }
        }
        
        LDA RIOT.DDRB     // Send NACK == SDA high (only single bytes for now)
        AND # SDA_INV
        STA RIOT.DDRB
        DEC RIOT.DDRB     // SCL HIGH
        INC RIOT.DDRB     // SCL LOW
    }
    Stop()
    {
        LDA RIOT.DDRB // SDA low
        ORA # SDA
        STA RIOT.DDRB
        DEC RIOT.DDRB // SCL HIGH
        LDA RIOT.DDRB // Set SDA high after SCL == Stop condition
        AND # SDA_INV
        STA RIOT.DDRB
    }
}
