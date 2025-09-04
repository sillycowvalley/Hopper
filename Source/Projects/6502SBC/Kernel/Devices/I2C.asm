unit I2C
{
    // Based on work by Anders Nielsen, 2023-2024
    // License: https://creativecommons.org/licenses/by-nc/4.0/legalcode
    
    // https://github.com/AndersBNielsen/65uino/blob/main/i2c.s
    
    const byte SSD1306Address      = 0x3C;
    const byte SerialEEPROMAddress = 0x50;
    
    const byte SCL     = 0b00000001;    // DRB0 bitmask
    const byte SCL_INV = 0b11111110;    //   inverted for easy clear bit
    const byte SDA     = 0b00000010;    // DRB1 bitmask
    const byte SDA_INV = 0b11111101;    //   inverted for easy clear bit
      
    Scan()
    {
        // I2C address in A
        ASL                // always 'write'
        STA ZP.OutB        // Save addr + r/w bit
        Start();
        Stop();
        LDA ZP.LastAck // LastAck in A, Z set if found
    }
    BeginTx()
    {
        PopA();          // I2C address -> A
        ASL                // always 'write'
        STA ZP.OutB        // Save addr + r/w bit
        Start();
    }
    BeginRx()
    {
        PopA();          // I2C address -> A
        ASL                
        ORA # 0b00000001   // always 'read'
        STA ZP.OutB        // Save addr + r/w bit
        Start();
    }
    EndTx()
    {
        Stop();
        LDA ZP.LastAck
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        LDA # Types.Bool
        PushTop();
    }
    Start()
    {
#if defined(ZEROPAGE_IO)
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
        ByteOut();
    } 
    
    Stop()
    {
#if defined(ZEROPAGE_IO)
        SMB1 ZP.DDRB // SDA low
        RMB0 ZP.DDRB // SCL high
        RMB1 ZP.DDRB // SDA high after SCL == Stop condition
#else
        PHA // preserve A (called from loadCommand)
        LDA ZP.DDRB // SDA low
        ORA # SDA
        STA ZP.DDRB
        DEC ZP.DDRB // SCL high
        LDA ZP.DDRB // SDA high after SCL == Stop condition
        AND # SDA_INV
        STA ZP.DDRB
        PLA
#endif
    }
        
    Write() 
    {
        PopA();          // byte to send
        STA ZP.OutB
        ByteOut();
    }
    
    RequestFrom()
    {
        PopTop();           // bytes to read (0..255) -> TOPL
        PopA();            // I2C address -> A
        RequestFromTOPA();
        // bytes read in TOPL
        LDA # 0
        STA ZP.TOPH
        LDA # Types.Byte
        PushTop();
    }
    // A has I2C adddress, TOPL has number of bytes to return, TOPL returns number of bytes read
    //    munts A, X
    RequestFromTOPA() 
    {
        ASL                // always 'read'
        ORA # 0b00000001
        STA ZP.OutB        // Save addr + r/w bit
        Start();
        // after start() SCL is low, DDRB is all output
        // (SDA is bit 1, SCL is bit 0)
         
        // initialize the I2C buffer
#if defined(ZEROPAGE_IO)
        STZ ZP.I2CInWritePtr
        STZ ZP.I2CInReadPtr
        LDX ZP.TOPL
        if (NZ) // bytes to read != 0?
        {
            STZ ZP.TOPL
            loop
            {
                PHX
                
                RMB1 ZP.DDRB   // SDA input
                
                CLC
                STZ ZP.InB
                LDX # 8
                loop
                {
                    RMB0 ZP.DDRB   // SCL high
                    
                    // Let's read after SCL goes high
                    if (BBS1, ZP.PORTB)
                    {
                        SEC       // 1 -> C
                    }
                    ROL  ZP.InB   // Shift bit into the input byte
                    SMB0 ZP.DDRB  // SCL low again for the next bit
                    DEX
                    if (Z) { break; }
                }
                
                LDA ZP.InB
                
                LDX ZP.I2CInWritePtr    // push it into serial input buffer
                STA Address.I2CInBuffer, X
                INC ZP.I2CInWritePtr
                
                INC ZP.TOPL // count bytes actually read
                
                PLX
                DEX
                
                RMB1 ZP.PORTB // make sure SDA is indeed low
                if (Z)
                {
                    // last byte: code to send a NACK (SDA high, SCL high then low)
                    RMB1 ZP.DDRB // SDA high
                    RMB0 ZP.DDRB // SCL high
                    SMB0 ZP.DDRB // SCL low
                    break;
                }
                // code to send an ACK (SDA low, SCL high then low):
                SMB1 ZP.DDRB // SDA low
                RMB0 ZP.DDRB // SCL high
                SMB0 ZP.DDRB // SCL low
            } // loop  
        }      
          
        Stop();
#else
        LDA # 0
        STA ZP.I2CInWritePtr
        STA ZP.I2CInReadPtr
        LDX ZP.TOPL
        if (NZ) // bytes to read != 0?
        {
            LDA # 0
            STA ZP.TOPL
            loop
            {
                TXA PHA
                
                LDA ZP.DDRB // SDA input
                AND # SDA_INV
                STA ZP.DDRB
                
                CLC
                LDA # 0
                STA ZP.InB
                LDX # 8
                loop
                {
                    
                    LDA ZP.DDRB // SCL high
                    AND # SCL_INV
                    STA ZP.DDRB
                    
                    LDA  ZP.PORTB  // Let's read after SCL goes high
                    AND # SDA
                    if (NZ)
                    {
                        SEC       // 1 -> C
                    }
                    ROL  ZP.InB   // Shift bit into the input byte
                    
                    LDA ZP.DDRB // SCL low again for the next bit
                    ORA # SCL
                    STA ZP.DDRB
                    
                    DEX
                    if (Z) { break; }
                }
                
                LDA ZP.InB
                
                LDX ZP.I2CInWritePtr    // push it into serial input buffer
                STA Address.I2CInBuffer, X
                INC ZP.I2CInWritePtr
                
                INC ZP.TOPL // count bytes actually read
                
                PLA TAX
                
                
                LDA ZP.PORTB   // make sure SDA is indeed low
                AND # SDA_INV
                STA ZP.PORTB
                
                DEX
                if (Z)
                {
                    // last byte: code to send a NACK (SDA high, SCL high then low)
                    LDA ZP.DDRB   // SDA high
                    AND # SDA_INV
                    STA ZP.DDRB
                    AND # SCL_INV // SCL high
                    STA ZP.DDRB
                    ORA # SCL     // SCL low
                    STA ZP.DDRB
                    break;
                }
                // code to send an ACK (SDA low, SCL high then low):
                LDA ZP.DDRB   // SDA low
                ORA # SDA
                STA ZP.DDRB
                AND # SCL_INV // SCL high
                STA ZP.DDRB
                ORA # SCL     // SCL low
                STA ZP.DDRB
            } // loop  
        }      
          
        Stop();
        
        // bytes read in TOPL
#endif
    }
    Read()
    {
        // return zero if we read past end of buffer
        STZ ZP.TOPL
        STZ ZP.TOPH
        
        LDA ZP.I2CInReadPtr
        CMP ZP.I2CInWritePtr
        if (NZ) // ReadPtr != WritePtr means we have more data available in the I2CInBuffer
        {
            LDX ZP.I2CInReadPtr
            LDA Address.I2CInBuffer, X
            STA ZP.TOPL
            INC ZP.I2CInReadPtr
        }
        LDA # Types.Byte
        PushTop();
    }
     
    ByteOut() // clears ZP.OutB 
    {
        PHA        
#if defined(ZEROPAGE_IO)
        PHX
        
        RMB1 ZP.PORTB // in case this is a data byte we set SDA low
        LDX # 8
        loop
        {
            SMB0 ZP.DDRB   // SCL out, clock low
            ASL  ZP.OutB   // MSB to carry
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
        
        if (BBR1, ZP.PORTB)
        {
            STZ ZP.LastAck // ACK
        }
        else
        {
            SMB0 ZP.LastAck // NACK
        }
        SMB0 ZP.DDRB    // clock low
        
        PLX
#else             
        TXA PHA                     
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
        AND # SDA
        if (Z)
        {
            LDA # 0 // ACK
        }
        else
        {
            LDA # 1 // NACK
        }
        STA ZP.LastAck 
        INC ZP.DDRB   // Clock low
        PLA TAX
#endif        
        PLA
    } 
}
