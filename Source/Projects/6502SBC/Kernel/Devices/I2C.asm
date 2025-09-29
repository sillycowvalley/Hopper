unit I2C
{
    // Based on work by Anders Nielsen, 2023-2024
    // License: https://creativecommons.org/licenses/by-nc/4.0/legalcode
    
    // https://github.com/AndersBNielsen/65uino/blob/main/i2c.s
    
    const byte SSD1306Address       = 0x3C;
    const byte SerialEEPROMAddress  = 0x50;
    const byte SerialEEPROMAddress2 = 0x51;
    
#ifdef M6821_PIA    
    const uint I2C_DDR  = ZP.DDRA;
    const uint I2C_PORT = ZP.PORTA;
    const uint I2C_CR   = ZP.CRA;
    const byte PIA_DDR  = 0b00110000;    // CR for DDR
    const byte PIA_PORT = 0b00110100;    // CR for PORT
    const byte SCL      = 0b01000000;    // DRA6 bitmask
    const byte SCL_INV  = 0b10111111;    //   inverted for easy clear bit
    const byte SDA      = 0b10000000;    // DRA7 bitmask
    const byte SDA_INV  = 0b01111111;    //   inverted for easy clear bit
#else
    const byte I2C_DDR  = ZP.DDRB;
    const byte I2C_PORT = ZP.PORTB;
    const byte SCL      = 0b00000001;    // DRB0 bitmask
    const byte SCL_INV  = 0b11111110;    //   inverted for easy clear bit
    const byte SDA      = 0b00000010;    // DRB1 bitmask
    const byte SDA_INV  = 0b11111101;    //   inverted for easy clear bit
#endif

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
        STA ZP.TOP0
        LDA # 0
        STA ZP.TOP1
        LDA # Types.Bool
        PushTop();
    }
    Start()
    {
#if defined(CPU_65C02S) && defined(ZEROPAGE_IO)
        RMB0 I2C_DDR     // Start with SCL as input HIGH
        SMB1 I2C_DDR     // Ensure SDA is output low before SCL is LOW
        RMB1 I2C_PORT
        RMB0 I2C_PORT    // Ensure SCL is low when it turns to output
        SMB0 I2C_DDR     // Set to output by incrementing the direction register == OUT, LOW
#else   

  #ifdef M6821_PIA
        LDA # PIA_DDR
        STA I2C_CR
  #endif
        LDA # SCL_INV
        AND I2C_DDR
        STA I2C_DDR      // Start with SCL as input HIGH
        
        LDA # SDA        // Ensure SDA is output low before SCL is LOW
        ORA I2C_DDR
        STA I2C_DDR

  #ifdef M6821_PIA
        LDA # PIA_PORT
        STA I2C_CR
  #endif
        LDA # SDA_INV
        AND I2C_PORT
        STA I2C_PORT
        
        LDA # SCL_INV    // Ensure SCL is low when it turns to output
        AND I2C_PORT
        STA I2C_PORT
        
  #ifdef M6821_PIA
        LDA # PIA_DDR
        STA I2C_CR
  #endif        
        LDA I2C_DDR
        ORA # SCL        // Set to output == OUT, LOW
        STA I2C_DDR   
#endif
        ByteOut();
    } 
    
    Stop()
    {
#if defined(CPU_65C02S) && defined(ZEROPAGE_IO)
        SMB1 I2C_DDR // SDA low
        RMB0 I2C_DDR // SCL high
        RMB1 I2C_DDR // SDA high after SCL == Stop condition
#else
        PHA          // preserve A (called from loadCommand)
    
  #ifdef M6821_PIA
        LDA # PIA_DDR
        STA I2C_CR
  #endif    
        LDA I2C_DDR  // SDA low (output mode)
        ORA # SDA
        STA I2C_DDR
        
        LDA I2C_DDR  // SCL high (input mode)
        AND # SCL_INV
        STA I2C_DDR
        
        LDA I2C_DDR  // SDA high after SCL == Stop condition
        AND # SDA_INV
        STA I2C_DDR
        
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
        PopTop();          // bytes to read (0..255) -> TOP0
        PopA();            // I2C address -> A
        RequestFromTOPA();
        // bytes read in TOP0
        LDA # 0
        STA ZP.TOP1
        LDA # Types.Byte
        PushTop();
    }
    // A has I2C adddress, TOP0 has number of bytes to return, TOP0 returns number of bytes read
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
#if defined(CPU_65C02S) && defined(ZEROPAGE_IO)
        STZ ZP.I2CInWritePtr
        STZ ZP.I2CInReadPtr
        LDX ZP.TOP0
        if (NZ) // bytes to read != 0?
        {
            STZ ZP.TOP0
            loop
            {
                PHX
                
                RMB1 I2C_DDR   // SDA input
                
                CLC
                STZ ZP.InB
                LDX # 8
                loop
                {
                    RMB0 I2C_DDR   // SCL high
                    
                    // Let's read after SCL goes high
                    if (BBS1, I2C_PORT)
                    {
                        SEC       // 1 -> C
                    }
                    ROL  ZP.InB   // Shift bit into the input byte
                    SMB0 I2C_DDR  // SCL low again for the next bit
                    DEX
                    if (Z) { break; }
                }
                
                LDA ZP.InB
                
                LDX ZP.I2CInWritePtr    // push it into serial input buffer
                STA Address.I2CInBuffer, X
                INC ZP.I2CInWritePtr
                
                INC ZP.TOP0 // count bytes actually read
                
                PLX
                DEX
                
                RMB1 I2C_PORT // make sure SDA is indeed low
                if (Z)
                {
                    // last byte: code to send a NACK (SDA high, SCL high then low)
                    RMB1 I2C_DDR // SDA high
                    RMB0 I2C_DDR // SCL high
                    SMB0 I2C_DDR // SCL low
                    break;
                }
                // code to send an ACK (SDA low, SCL high then low):
                SMB1 I2C_DDR // SDA low
                RMB0 I2C_DDR // SCL high
                SMB0 I2C_DDR // SCL low
            } // loop  
        }      
          
        Stop();
#else
        LDA # 0
        STA ZP.I2CInWritePtr
        STA ZP.I2CInReadPtr
        LDX ZP.TOP0
        if (NZ) // bytes to read != 0?
        {
            LDA # 0
            STA ZP.TOP0
            loop
            {
                TXA PHA
                
                LDA I2C_DDR // SDA input
                AND # SDA_INV
                STA I2C_DDR
                
                CLC
                LDA # 0
                STA ZP.InB
                LDX # 8
                loop
                {
                    
                    LDA I2C_DDR // SCL high
                    AND # SCL_INV
                    STA I2C_DDR
                    
                    LDA  I2C_PORT  // Let's read after SCL goes high
                    AND # SDA
                    if (NZ)
                    {
                        SEC       // 1 -> C
                    }
                    ROL  ZP.InB   // Shift bit into the input byte
                    
                    LDA I2C_DDR // SCL low again for the next bit
                    ORA # SCL
                    STA I2C_DDR
                    
                    DEX
                    if (Z) { break; }
                }
                
                LDA ZP.InB
                
                LDX ZP.I2CInWritePtr    // push it into serial input buffer
                STA Address.I2CInBuffer, X
                INC ZP.I2CInWritePtr
                
                INC ZP.TOP0 // count bytes actually read
                
                PLA TAX
                
                
                LDA I2C_PORT   // make sure SDA is indeed low
                AND # SDA_INV
                STA I2C_PORT
                
                DEX
                if (Z)
                {
                    // last byte: code to send a NACK (SDA high, SCL high then low)
                    LDA I2C_DDR   // SDA high
                    AND # SDA_INV
                    STA I2C_DDR
                    AND # SCL_INV // SCL high
                    STA I2C_DDR
                    ORA # SCL     // SCL low
                    STA I2C_DDR
                    break;
                }
                // code to send an ACK (SDA low, SCL high then low):
                LDA I2C_DDR   // SDA low
                ORA # SDA
                STA I2C_DDR
                AND # SCL_INV // SCL high
                STA I2C_DDR
                ORA # SCL     // SCL low
                STA I2C_DDR
            } // loop  
        }      
          
        Stop();
        
        // bytes read in TOP0
#endif
    }
    Read()
    {
        // return zero if we read past end of buffer
#if defined(CPU_65C02S)
        STZ ZP.TOP0
        STZ ZP.TOP1
#else
        LDA # 0
        STA ZP.TOP0
        STA ZP.TOP1
#endif
        
        LDA ZP.I2CInReadPtr
        CMP ZP.I2CInWritePtr
        if (NZ) // ReadPtr != WritePtr means we have more data available in the I2CInBuffer
        {
            LDX ZP.I2CInReadPtr
            LDA Address.I2CInBuffer, X
            STA ZP.TOP0
            INC ZP.I2CInReadPtr
        }
        LDA # Types.Byte
        PushTop();
    }
     
    ByteOut() // clears ZP.OutB 
    {
        PHA        
#if defined(CPU_65C02S) && defined(ZEROPAGE_IO)
        PHX
        
        RMB1 I2C_PORT // in case this is a data byte we set SDA low
        LDX # 8
        loop
        {
            SMB0 I2C_DDR   // SCL out, clock low
            ASL  ZP.OutB   // MSB to carry
            if (C)
            {
                RMB1 I2C_DDR  // set SDA low
            }
            else
            {
                SMB1 I2C_DDR  // set SDA high
            }
            RMB0 I2C_DDR // Clock high
            DEX
            if (Z) { break; }
        }
        
        SMB0 I2C_DDR  // Clock low
        RMB1 I2C_DDR  // Set SDA to INPUT (HIGH)
        RMB0 I2C_DDR  // Clock high
        
        if (BBR1, I2C_PORT)
        {
            STZ ZP.LastAck // ACK
        }
        else
        {
            SMB0 ZP.LastAck // NACK
        }
        SMB0 I2C_DDR    // clock low
        
        PLX
#else             
        TXA PHA      
        
  #ifdef M6821_PIA
        LDA # PIA_PORT
        STA I2C_CR
  #endif                                      
        LDA # SDA_INV   // Prepare SDA low for data transmission
        AND I2C_PORT
        STA I2C_PORT
        LDX # 8
  #ifdef M6821_PIA
        LDA # PIA_DDR
        STA I2C_CR
  #endif        
        JMP first       // Clock already low from Start()
        loop
        {
            LDA I2C_DDR
            ORA # SCL   // SCL output, clock low
            STA I2C_DDR
first:
            ASL ZP.OutB // MSB to carry for transmission
            if (C)
            {
                LDA I2C_DDR
                AND # SDA_INV  // SDA high (input mode)
                STA I2C_DDR
            }
            else
            {
                LDA I2C_DDR
                ORA # SDA      // SDA low (output mode)
                STA I2C_DDR
            }
            LDA I2C_DDR
            AND # SCL_INV      // Clock high for data valid
            STA I2C_DDR
            DEX
            if (Z) { break; }
        }
        
        LDA I2C_DDR
        ORA # SCL          // Clock low after last bit
        STA I2C_DDR
        
        LDA I2C_DDR        // Release SDA for ACK bit
        AND # SDA_INV
        STA I2C_DDR
        
        LDA I2C_DDR
        AND # SCL_INV      // Clock high to read ACK
        STA I2C_DDR
        
  #ifdef M6821_PIA
        LDA # PIA_PORT
        STA I2C_CR
  #endif        
        LDA I2C_PORT       // Sample ACK bit
        AND # SDA
        if (Z)
        {
            LDA # 0        // ACK received (SDA pulled low)
        }
        else
        {
            LDA # 1        // NACK received (SDA high)
        }
        STA ZP.LastAck 
        
  #ifdef M6821_PIA
        LDA # PIA_DDR
        STA I2C_CR
  #endif        
        LDA I2C_DDR
        ORA # SCL          // Clock low to complete ACK
        STA I2C_DDR
        
        PLA TAX
#endif        
        PLA
    } 
}

