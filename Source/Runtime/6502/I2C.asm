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
      
    BeginTx()
    {
        //SEI
        
        //LDA # 0x0A
        //Serial.WriteChar();
        //LDA # '['
        //Serial.WriteChar();
        
        PopTop();          // I2C address
        LDA ZP.TOPL
        
        //ROL              // Shift in carry : carry set means 'read', carry clear means 'write'
        ASL                // assume always 'write' for now (API needs another argument or method)
        
        STA ZP.OutB        // Save addr + r/w bit

#ifdef CPU_65C02S
        RMB0 ZP.DDRB     // Start with SCL as input HIGH - that way we can inc/dec from here
        SMB1 ZP.DDRB     // Ensure SDA is output low before SCL is LOW
        RMB1 ZP.PORTB
        RMB0 ZP.PORTB    // Ensure SCL is low when it turns to output
        SMB0 ZP.DDRB     // Set to output by incrementing the direction register == OUT, LOW
#else        
        LDA # SCL_INV
        AND ZP.DDRB
        STA ZP.DDRB      // Start with SCL as input HIGH - that way we can inc/dec from here
        
        LDA # SDA        // Ensure SDA is output low before SCL is LOW
        ORA ZP.DDRB
        STA ZP.DDRB
        LDA # SDA_INV
        AND ZP.PORTB
        STA ZP.PORTB
        
        LDA # SCL_INV    // Ensure SCL is low when it turns to output
        AND ZP.PORTB
        STA ZP.PORTB
        INC ZP.DDRB      // Set to output by incrementing the direction register == OUT, LOW
#endif
        
        byteOut();
    } 
    
    EndTx()
    {
#ifdef CPU_65C02S        
        SMB1 ZP.DDRB // SDA low
        RMB0 ZP.DDRB // SCL high
        RMB1 ZP.DDRB // SDA high after SCL == Stop condition
#else
        LDA ZP.DDRB // SDA low
        ORA # SDA
        STA ZP.DDRB
        DEC ZP.DDRB // SCL high
        LDA ZP.DDRB // SDA high after SCL == Stop condition
        AND # SDA_INV
        STA ZP.DDRB
#endif
        
        //LDA # ']'
        //Serial.WriteChar();
        
        //CLI
        
        /*
        0: success
        1: busy timeout upon entering endTransmission()
        2: START bit generation timeout
        3: end of address transmission timeout
        4: data byte transfer timeout
        5: data byte transfer succeeded, busy timeout immediately after
        6: timeout waiting for peripheral to clear stop bit
        */
        LDA # 0
        STA ZP.TOPL
        STA ZP.TOPH
        LDA # Types.Bool
        PushTop();
    }
    
    Write() 
    {
        PopTop();          // byte to send
        LDA ZP.TOPL
        STA ZP.OutB
        byteOut();
    }
     
    byteOut() // clears ZP.OutB   
    {
        //LDA # ' '
        //Serial.WriteChar();
        //LDA ZP.OutB
        //Serial.HexOut();
        
#ifdef CPU_65C02S

        RMB1 ZP.PORTB // in case this is a data byte we set SDA low
        LDX # 8
        loop
        {
            SMB0 ZP.DDRB // SCL out, clock low
            ASL ZP.OutB    // MSB to carry
            if (C)
            {
                RMB1 ZP.DDRB  // set SDA low
            }
            else
            {
                SMB1 ZP.DDRB  // set SDA high
            }
            RMB0 ZP.DDRB // Clock high
            DEX
            if (Z) { break; }
        }
        
        SMB0 ZP.DDRB  // Clock low
        RMB1 ZP.DDRB  // Set SDA to INPUT (HIGH)
        RMB0 ZP.DDRB  // Clock high
        
        SEC
        if (BBR1, ZP.PORTB)
        {
            CLC       // clear carry on ACK
        }
        SMB0 ZP.DDRB    // clock low
        
#else             
                                
        LDA # SDA_INV // in case this is a data byte we set SDA low
        AND ZP.PORTB
        STA ZP.PORTB
        LDX # 8
        JMP first     // skip INC since first time already out, low
        loop
        {
            INC ZP.DDRB  // SCL out, clock low
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
            DEC ZP.DDRB  // Clock high
            DEX
            if (Z) { break; }
        }
        INC ZP.DDRB  // Clock low
        LDA ZP.DDRB  // Set SDA to INPUT (HIGH)
        AND # SDA_INV
        STA ZP.DDRB
        DEC ZP.DDRB   // Clock high
        
        LDA ZP.PORTB  // Check ACK bit
        SEC
        AND # SDA
        if (Z)
        {
            CLC       // Clear carry on ACK
        }
        INC ZP.DDRB   // Clock low
        
#endif        
        
    } 
    /*
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
    */
    
}
