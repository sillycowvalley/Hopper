unit Float
{
    
    friend Long;
    
    const byte TOPEXP0   = M0;
    const byte TOPEXP1   = M1;
    const byte NEXTEXP0  = M2;
    const byte NEXTEXP1  = M3;
    const byte TOPSIGN   = M4;
    const byte NEXTSIGN  = M5;
    
    expGT()
    {
        LDX #0 // NEXT <= TOP
        LDA NEXTEXP1
        CMP TOPEXP1
        if (Z)
        {
            LDA NEXTEXP0
            CMP TOPEXP0
        }
        if (NZ) // NEXT == TOP (not >) ?
        {
            if (C) // NEXT <  TOP (not >)?
            {
                LDX #1   // NEXT > TOP
            }
        }
    }
    
    expLT()
    {
        LDX #1 // NEXT < TOP
        LDA NEXTEXP1
        CMP TOPEXP1
        if (Z)
        {
            LDA NEXTEXP0
            CMP TOPEXP0
        }
        if (C) // NEXT < TOP?
        {
            LDX #0 // NEXT >= TOP
        }
        
    }
    
    New()
    {
        // IEEE +0.0
        STZ ZP.NEXT0
        STZ ZP.NEXT1
        STZ ZP.NEXT2
        STZ ZP.NEXT3
    }
    
    IsZeroNext()
    {
        LDX #0
        LDA ZP.NEXT0
        if (Z)
        {
            LDA ZP.NEXT1
            if (Z)
            {
                LDA ZP.NEXT2
                if (Z)
                {
                    LDA ZP.NEXT3
                    AND # 0x7F    // ignore the sign for -0
                    if (Z)
                    {
                        INX
                    }
                }
            }
        }
    }
    IsZeroTop()
    {
        LDX #0
        LDA ZP.TOP0
        if (Z)
        {
            LDA ZP.TOP1
            if (Z)
            {
                LDA ZP.TOP2
                if (Z)
                {
                    LDA ZP.TOP3
                    AND # 0x7F    // ignore the sign for -0
                    if (Z)
                    {
                        INX
                    }
                }
            }
        }
    }
    
    getSignNEXT()
    {
        STZ NEXTSIGN
        if (BBS7, ZP.NEXT3)
        {
            INC NEXTSIGN
        }
    }
    getSignTOP()
    {
        STZ TOPSIGN
        if (BBS7, ZP.TOP3)
        {
            INC TOPSIGN
        }
    }
    getExponentNEXT()
    {
        LDA ZP.NEXT3
        ASL A
        if (BBS7, ZP.NEXT2)
        {
            ORA # 0b00000001
        }        
        STA NEXTEXP0
        STZ NEXTEXP1
    }
    getExponentTOP()
    {
        LDA ZP.TOP3
        ASL A
        if (BBS7, ZP.TOP2)
        {
            ORA # 0b00000001
        }        
        STA TOPEXP0
        STZ TOPEXP1
    }
    getMantissaNEXT()
    {
        LDA ZP.NEXT2
        AND # 0x7F
        STA ZP.NEXT2
        STZ ZP.NEXT3
    }
    getMantissaTOP()
    {
        LDA ZP.TOP2
        AND # 0x7F
        STA ZP.TOP2
        STZ ZP.TOP3
    }
    countLeadingZeros()
    {
        LDX #0
        LDA ZP.RESULT3
        if (Z)
        {
            LDX # 8
            
            LDA ZP.RESULT2
            if (Z)
            {
                LDX # 16
                
                LDA ZP.RESULT1
                if (Z)
                {
                    LDX # 24
                    
                    LDA ZP.RESULT0
                    if (Z)
                    {
                        LDX # 32
                        return;
                    }
                }
            }
        }
        // this loop exits because A != 0
        BRA countEntry
        loop
        {        
            INX
countEntry:        
            ASL
            if (NC) { continue; }
            break;
        }
    }
    Add()
    {
        commonAdd();
    }
    Sub()
    {
        // Flip the sign of TOP
        if (BBS7, ZP.TOP3)
        {
            RMB7 ZP.TOP3
        }
        else
        {
            SMB7 ZP.TOP3
        }
        commonAdd();
    }
    
    handleExponentOverflow()
    {
        // Handle exponent overflow/underflow
        LDA NEXTEXP1
        if (MI) 
        {
            // exponentA < 0
            STZ NEXTEXP1
            STZ NEXTEXP0
            STZ ZP.RESULT0
            STZ ZP.RESULT1
            STZ ZP.RESULT2
            STZ ZP.RESULT3
            return;
        }
        if (Z)
        {
            LDA NEXTEXP0
            if (Z)
            {
                // exponentA == 0
                STZ NEXTEXP1
                STZ NEXTEXP0
                STZ ZP.RESULT0
                STZ ZP.RESULT1
                STZ ZP.RESULT2
                STZ ZP.RESULT3
                return;
            }
            CMP # 0xFF
            if (Z)
            {
                // exponentA == 255
                STZ ZP.RESULT0
                STZ ZP.RESULT1
                STZ ZP.RESULT2
                STZ ZP.RESULT3
                return;
            }
        }
        else
        {
            // exponentA > 255
            LDA # 0xFF
            STA NEXTEXP0
            STZ NEXTEXP1
            STZ ZP.RESULT0
            STZ ZP.RESULT1
            STZ ZP.RESULT2
            STZ ZP.RESULT3
        }
    }
    commonAdd()
    {
        loop
        {
            Float.IsZeroTop();
            CPX # 1
            if (Z)
            {
                break;
            }
            Float.IsZeroNext();
            CPX # 1
            if (Z)
            {
                Shared.SwapNextTop();
                break;
            }
            
            getSignNEXT();
            getExponentNEXT();
            getMantissaNEXT();
            
            getSignTOP();  
            getExponentTOP();
            getMantissaTOP();
            
            // Add the implicit leading '1' bit
            LDA ZP.NEXT2
            ORA # 0b10000000
            STA ZP.NEXT2
            LDA ZP.TOP2
            ORA # 0b10000000
            STA ZP.TOP2
            
            // Align exponents
            expGT();
            CPX # 1
            if (Z)
            {
                // int shift = exponentA - exponentB;
                SEC
                LDA NEXTEXP0
                SBC TOPEXP0
                TAX
                
                // mantissaB = Long.shiftRight(mantissaB, shift);
                loop
                {
                    LSR ZP.TOP3
                    ROR ZP.TOP2
                    ROR ZP.TOP1
                    ROR ZP.TOP0
                    DEX
                    if (Z) { break; }
                }
                
                // exponentB = exponentA;
                LDA NEXTEXP0
                STA TOPEXP0
            }
            else
            {
                expLT();
                CPX # 1
                if (Z)
                {
                    // int shift = exponentB - exponentA;
                    SEC
                    LDA TOPEXP0
                    SBC NEXTEXP0
                    TAX
                    
                    // mantissaA = Long.shiftRight(mantissaA, shift);
                    loop
                    {
                        LSR ZP.NEXT3
                        ROR ZP.NEXT2
                        ROR ZP.NEXT1
                        ROR ZP.NEXT0
                        DEX
                        if (Z) { break; }
                    }
                    // exponentA = exponentB;
                    LDA TOPEXP0
                    STA NEXTEXP0
                }
            }
            LDA NEXTSIGN
            CMP TOPSIGN
            if (Z)
            {
                CLC
                LDA ZP.NEXT0
                ADC ZP.TOP0
                STA ZP.RESULT0
                LDA ZP.NEXT1
                ADC ZP.TOP1
                STA ZP.RESULT1
                LDA ZP.NEXT2
                ADC ZP.TOP2
                STA ZP.RESULT2
                LDA ZP.NEXT3
                ADC ZP.TOP3
                STA ZP.RESULT3
            }
            else
            {
                Long.commonLT();
                CPX # 0 // not <, so >=
                if (Z)
                {
                    // mantissaA >= mantissaB
                    SEC
                    LDA ZP.NEXT0
                    SBC ZP.TOP0
                    STA ZP.RESULT0
                    LDA ZP.NEXT1
                    SBC ZP.TOP1
                    STA ZP.RESULT1
                    LDA ZP.NEXT2
                    SBC ZP.TOP2
                    STA ZP.RESULT2
                    // always zeroes?
                    LDA ZP.NEXT3
                    SBC ZP.TOP3
                    STA ZP.RESULT3
                }
                else
                {
                    // mantissaA < mantissaB
                    SEC
                    LDA ZP.TOP0
                    SBC ZP.NEXT0
                    STA ZP.RESULT0
                    LDA ZP.TOP1
                    SBC ZP.NEXT1
                    STA ZP.RESULT1
                    LDA ZP.TOP2
                    SBC ZP.NEXT2
                    STA ZP.RESULT2
                    // always zeroes?
                    LDA ZP.TOP3
                    SBC ZP.NEXT3
                    STA ZP.RESULT3
                    
                    LDA TOPSIGN
                    STA NEXTSIGN
                }
                // Check for zero mantissa (cancellation)
                LDA ZP.RESULT0
                if (Z)
                {
                    LDA ZP.RESULT1
                    if (Z)
                    {
                        LDA ZP.RESULT2
                        if (Z)
                        {
                            LDA ZP.RESULT3
                            if (Z)
                            {
                                // Return +0.0
                                STZ ZP.NEXT0
                                STZ ZP.NEXT1
                                STZ ZP.NEXT2
                                STZ ZP.NEXT3
                                break;
                            }
                        }
                    }
                }
            }
            countLeadingZeros();
            STX ACCH
            CPX # 8
            if (NC)
            {
                // leadingZeros < 8
                STX ACCL
                SEC
                LDA # 8
                SBC ACCL
                TAX
                loop
                {
                    if (Z) { break; }
                    LSR ZP.RESULT3
                    ROR ZP.RESULT2
                    ROR ZP.RESULT1
                    ROR ZP.RESULT0
                    DEX
                }
            }
            else
            {
                // leadingZeros >= 8
                TXA
                SEC
                SBC # 8
                TAX
                loop
                {
                    if (Z) { break; }
                    ASL ZP.RESULT0
                    ROL ZP.RESULT1
                    ROL ZP.RESULT2
                    ROL ZP.RESULT3
                    DEX
                }
            }
            CLC
            LDA NEXTEXP0
            ADC # 8
            STA NEXTEXP0
            LDA ZP.TEMP
            ADC # 0
            STA ZP.TEMP
            SEC
            LDA NEXTEXP0
            SBC ACCH
            STA NEXTEXP0
            LDA NEXTEXP1
            SBC # 0
            STA NEXTEXP1
            handleExponentOverflow();
                       
            // Remove the implicit leading bit
            LDA ZP.RESULT2
            AND # 0x7F
            STA ZP.RESULT2
            STZ ZP.RESULT3
            
            // Set the least significant bit of the exponent
            LDA NEXTEXP0
            AND # 0b00000001
            if (NZ)
            {
                LDA ZP.RESULT2
                ORA # 0b10000000
                STA ZP.RESULT2
            }
            
            // Take the next 7 bits of the exponent
            LDA NEXTEXP0
            LSR
            STA ZP.RESULT3
            
            // Set the sign bit
            LDA NEXTSIGN
            if (NZ)
            {
                LDA # 0b10000000
                ORA ZP.RESULT3
                STA ZP.RESULT3
            }
            
            LDA ZP.RESULT0
            STA ZP.NEXT0
            LDA ZP.RESULT1
            STA ZP.NEXT1
            LDA ZP.RESULT2
            STA ZP.NEXT2
            LDA ZP.RESULT3
            STA ZP.NEXT3
            
            break;   
        }
        // result in NEXT
    }
    countLeadingZeros48()
    {
        LDX #0
        LDA ZP.RESULT5
        if (Z)
        {
            LDX # 8
            
            LDA ZP.RESULT4
            if (Z)
            {
                LDX # 16
                
                LDA ZP.RESULT3
                if (Z)
                {
                    LDX # 24
                    
                    LDA ZP.RESULT2
                    if (Z)
                    {
                        LDX # 32
                        LDA ZP.RESULT1
                        if (Z)
                        {
                            LDX # 40
                            LDA ZP.RESULT0
                            if (Z)
                            {
                                LDX # 48
                                return;
                            }
                        }
                    }
                }
            }
        }
        // this loop exits because A != 0
        BRA countEntry
        loop
        {        
            INX
countEntry:        
            ASL
            if (NC) { continue; }
            break;
        }
    }
    Mul()
    {
        loop
        {
            getSignNEXT();
            getSignTOP();  
            LDA NEXTSIGN
            EOR TOPSIGN
            STA NEXTSIGN
            
            Float.IsZeroNext();
            CPX # 1
            if (Z)
            {
                break;
            }
            Float.IsZeroTop();
            CPX # 1
            if (Z)
            {
                Shared.SwapNextTop();
                break;
            }
            
            getExponentNEXT();
            getMantissaNEXT();
            
            getExponentTOP();
            getMantissaTOP();
            
            // Add the implicit leading '1' bit
            LDA ZP.NEXT2
            ORA # 0b10000000
            STA ZP.NEXT2
            LDA ZP.TOP2
            ORA # 0b10000000
            STA ZP.TOP2
            
            Long.utilityLongMUL();
                       
            countLeadingZeros48();
            STX ACCH
            CPX # 24
            if (NC)
            {
                // leadingZeros < 24
                STX ACCL
                SEC
                LDA # 24
                SBC ACCL
                TAX
                loop
                {
                    if (Z) { break; }
                    LSR LRESULT5
                    ROR LRESULT4
                    ROR LRESULT3
                    ROR LRESULT2
                    ROR LRESULT1
                    ROR LRESULT0
                    DEX
                }
            }
            else
            {
                // leadingZeros >= 24
                TXA
                SEC
                SBC # 24
                TAX
                loop
                {
                    if (Z) { break; }
                    ASL LRESULT0
                    ROL LRESULT1
                    ROL LRESULT2
                    ROL LRESULT3
                    DEX
                }
            }
            
            // exponent = exponentA + exponentB - 127
            CLC
            LDA NEXTEXP0
            ADC TOPEXP0
            STA NEXTEXP0
            LDA NEXTEXP1
            ADC TOPEXP1
            STA NEXTEXP1
            SEC
            LDA NEXTEXP0
            SBC # 127
            STA NEXTEXP0
            LDA NEXTEXP1
            SBC # 0
            STA NEXTEXP1
            
            // if leadingZeros == 0, exponent++
            LDA ACCL
            if (Z)
            {
                INC NEXTEXP0
                if (Z)
                {
                    INC NEXTEXP1
                }
            }
            
            handleExponentOverflow();
            
            // Remove the implicit leading bit
            LDA LRESULT2
            AND # 0x7F
            STA ZP.NEXT2
            
            // Set the least significant bit of the exponent
            LDA NEXTEXP0
            AND # 0b00000001
            if (NZ)
            {
                LDA ZP.NEXT2
                ORA # 0b10000000
                STA ZP.NEXT2
            }
            
            // Take the next 7 bits of the exponent
            LDA NEXTEXP0
            LSR
            STA ZP.NEXT3
            
            LDA LRESULT0
            STA ZP.NEXT0
            LDA LRESULT1
            STA ZP.NEXT1
            break;
        }
        // apply the sign
        LDA NEXTSIGN
        if (NZ)
        {
            LDA ZP.NEXT3
            ORA # 0b10000000
            STA ZP.NEXT3
        }
        
        // result in NEXT
    }
    
    checkDividendBit()
    {
        // check if bit X is set in dividend
        // Calculate the byte index and bit mask
        TXA
        AND # 0x07
        TAY   // bit index
        TXA
        LSR A
        LSR A
        LSR A
        TAX   // byte index
        
        // load bit mask
        LDA Array.bitMasks, Y
        AND ZP.NEXT0, X
    }
    orQuotientBit()
    {
        // set bit X in the quotient
        // Calculate the byte index and bit mask
        TXA
        AND # 0x07
        TAY   // bit index
        TXA
        LSR A
        LSR A
        LSR A
        TAX   // byte index
        
        // load bit mask
        LDA Array.bitMasks, Y
        ORA LRESULT0, X
        STA LRESULT0, X
    }
    
    mantissaDivide()
    {
        // NEXT is dividend
        // TOP  is divisor
        
        // quotient
        STZ LRESULT0
        STZ LRESULT1
        STZ LRESULT2
        STZ LRESULT3
        
        // remainder:
        STZ LRESULT4
        STZ LRESULT5
        STZ LRESULT6
        STZ LRESULT7
        
        LDX # 47
        loop
        {
            // X : 47 .. 0
            
            // remainder = remainder << 1
            ASL LRESULT4
            ROL LRESULT5
            ROL LRESULT6
            ROL LRESULT7

            CPX # 24
            if (C)
            {
                // X >= 24
                TXA
                SEC
                SBC # 16
            }
            else
            {
                // X < 24
                STX ACCL
                SEC
                LDA # 24
                SBC ACCL
            }
            
            PHX
            TAX
            checkDividendBit();
            
            if (NZ)
            {
                
                LDA LRESULT4
                ORA # 1
                STA LRESULT4
            }
            
            // If the remainder is greater than or equal to the divisor, subtract the divisor from the remainder
            
            // remainder - divisor >= 0?
            SEC
            LDA LRESULT4
            SBC ZP.TOP0
            LDA LRESULT5
            SBC ZP.TOP1
            LDA LRESULT6
            SBC ZP.TOP2
            LDA LRESULT7
            SBC ZP.TOP3
            if (PL)
            {
                // remainder = remainder - divisor;
                SEC
                LDA LRESULT4
                SBC ZP.TOP0
                STA LRESULT4
                LDA LRESULT5
                SBC ZP.TOP1
                STA LRESULT5
                LDA LRESULT6
                SBC ZP.TOP2
                STA LRESULT6
                LDA LRESULT7
                SBC ZP.TOP3
                STA LRESULT7
                PLX
                PHX
                orQuotientBit();
            }          
            
            PLX
            if (Z) { break; }
            DEX
        }
    }
    Div()
    {
        loop
        {
            getSignNEXT();
            getSignTOP();  
            LDA NEXTSIGN
            EOR TOPSIGN
            STA NEXTSIGN
            
            Float.isZeroNEXT();
            CPX # 1
            if (Z)
            {
                break; // return NEXT
            }
                        
            // TOP == 1 or TOP == -1 ? (mantissa = 0, exponent = 127
            LDA ZP.TOP0
            if (Z)
            {
                LDA ZP.TOP1
                if (Z)
                {
                    LDA ZP.TOP2
                    CMP # 0x80
                    if (Z)
                    {
                        LDA ZP.TOP3
                        AND # 0x7F
                        CMP # 0x3F
                        if (Z)
                        {
                            break; // return NEXT
                        }
                    }
                }
            }
            
            getExponentNEXT();
            getMantissaNEXT();
            
            getExponentTOP();
            getMantissaTOP();
            
            
            // Add the implicit leading '1' bit
            LDA ZP.NEXT2
            ORA # 0b10000000
            STA ZP.NEXT2
            LDA ZP.TOP2
            ORA # 0b10000000
            STA ZP.TOP2
            
            mantissaDivide();
            
            countLeadingZeros();
            STX ACCL
            CPX # 8
            if (NC)
            {
                // leadingZeros < 8
                SEC
                LDA # 8
                SBC ACCL
                TAX
                loop
                {
                    if (Z) { break; }
                    LSR LRESULT3
                    ROR LRESULT2
                    ROR LRESULT1
                    ROR LRESULT0
                    DEX
                }
            }
            else
            {
                // leadingZeros >= 8
                TXA
                SEC
                SBC # 8
                TAX
                loop
                {
                    if (Z) { break; }
                    ASL LRESULT0
                    ROL LRESULT1
                    ROL LRESULT2
                    ROL LRESULT3
                    DEX
                }
            }
            
            // exponent = exponentA - exponentB + 127
            CLC
            LDA NEXTEXP0
            ADC # 127
            STA NEXTEXP0
            LDA NEXTEXP1
            ADC # 0
            STA NEXTEXP1
            SEC
            LDA NEXTEXP0
            SBC TOPEXP0
            STA NEXTEXP0
            LDA NEXTEXP1
            SBC TOPEXP1
            STA NEXTEXP1
            
            // leadingZeros == 16?  exponent--
            LDA ACCL
            CMP # 16
            if (Z)
            {
                LDA NEXTEXP0
                if (Z)
                {
                    DEC NEXTEXP1
                }
                DEC NEXTEXP0
            }
            
            handleExponentOverflow();
            
            // Remove the implicit leading bit
            LDA LRESULT2
            AND # 0x7F
            STA ZP.NEXT2
            
            // Set the least significant bit of the exponent
            LDA NEXTEXP0
            AND # 0b00000001
            if (NZ)
            {
                LDA ZP.NEXT2
                ORA # 0b10000000
                STA ZP.NEXT2
            }
            
            // Take the next 7 bits of the exponent
            LDA NEXTEXP0
            LSR
            STA ZP.NEXT3
            
            LDA LRESULT0
            STA ZP.NEXT0
            LDA LRESULT1
            STA ZP.NEXT1
            
            break;
        } // loop
        
        // apply the sign
        LDA NEXTSIGN
        if (NZ)
        {
            LDA ZP.NEXT3
            ORA # 0b10000000
            STA ZP.NEXT3
        }
        else
        {
            LDA ZP.NEXT3
            AND # 0b01111111
            STA ZP.NEXT3
        }
        
        // result in NEXT
    }
    ToLong()
    {
        // source in NEXT
        
        getSignNEXT();  
        getExponentNEXT();
        getMantissaNEXT();
        
        // exponent = exponent - 127; // Bias adjustment
        SEC
        LDA NEXTEXP0
        SBC # 127
        STA NEXTEXP0
        LDA NEXTEXP1
        SBC # 0
        STA NEXTEXP1
        loop
        {
            if (NZ)
            {
                // exponent is -ve so fraction only
                STZ ZP.NEXT0
                STZ ZP.NEXT1
                STZ ZP.NEXT2
                STZ ZP.NEXT3
                break;
            }
        
            // Add the implicit leading '1' bit
            LDA ZP.NEXT2
            ORA # 0b10000000
            STA ZP.NEXT2
            
            LDA NEXTEXP0
            CMP # 23
            if (Z)
            {
                // exponent == 23
            }
            else
            {
                if (C)
                {
                    // exponent > 23
                    SEC
                    SBC # 23
                    TAX
                    loop
                    {
                        if (Z) { break; }
                        ASL ZP.NEXT0
                        ROL ZP.NEXT1
                        ROL ZP.NEXT2
                        ROL ZP.NEXT3
                        DEX
                    }
                }
                else
                {
                    // exponent < 23
                    SEC
                    LDA # 23
                    SBC NEXTEXP0
                    TAX
                    loop
                    {
                        if (Z) { break; }
                        LSR ZP.NEXT3
                        ROR ZP.NEXT2
                        ROR ZP.NEXT1
                        ROR ZP.NEXT0
                        DEX
                    }
                }
            }
        
            // apply the sign
            LDA NEXTSIGN
            if (NZ)
            {
                // result = -result
                SEC
                LDA # 0
                SBC ZP.NEXT0
                STA ZP.NEXT0
                LDA # 0
                SBC ZP.NEXT1
                STA ZP.NEXT1
                LDA # 0
                SBC ZP.NEXT2
                STA ZP.NEXT2
                LDA # 0
                SBC ZP.NEXT3
                STA ZP.NEXT3
            }
            break;
        }
        
        // result in NEXT
    }
    EQ()
    {
        IsZeroTop();
        TXA
        if (NZ)
        {
            IsZeroNext();
            TXA
            if (NZ)
            {
                // TOP == 0 && NEXT == 0
                // X is already 1
                return;
            }
        }
        // X is already 0
        LDA ZP.NEXT0
        CMP ZP.TOP0
        if (Z)
        {
            LDA ZP.NEXT1
            CMP ZP.TOP1
            if (Z)
            {
                LDA ZP.NEXT2
                CMP ZP.TOP2
                if (Z)
                {
                    LDA ZP.NEXT3
                    CMP ZP.TOP3
                    if (Z)
                    {
                        INX
                    }
                }
            }
        }
        // result in X
    }
    
    LT()
    {
        loop
        {
            getSignNEXT();
            getSignTOP();  
            LDX # 0
            LDA NEXTSIGN
            CMP TOPSIGN
            if (NZ)
            {    
                // signA != signB
                if (C)
                {
                    // signA > signB
                    INX
                }
                break;
            }
            getExponentNEXT();
            getExponentTOP();
            LDA NEXTEXP0
            CMP TOPEXP0
            if (NZ)
            {
                // exponentA != exponentB
                LDX # 0
                LDA NEXTSIGN
                if (Z)
                {
                    // signA == 0
                    LDA NEXTEXP0
                    CMP TOPEXP0
                    if (NC)
                    {
                        // exponentA < exponentB
                        INX
                    }
                }
                else
                {
                    LDA NEXTEXP0
                    CMP TOPEXP0
                    if (C)
                    {
                        // exponentA > exponentB
                        INX
                    }
                }
                break;
            }
            getMantissaNEXT();
            getMantissaTOP();
            
            LDA NEXTSIGN
            if (Z)
            {
                // signA == 0
                // mantissaA < mantissaB ?
                Long.commonLT(); 
            }
            else
            {
                Long.commonEQ();
                CPX # 1
                if (Z)
                {
                    DEX
                }
                else
                {
                    // mantissaA > mantissaB ?
                    Long.commonLT();
                    TXA
                    EOR # 0b00000001
                    TAX
                }
            }
            break;
        }
        // result in X
    }
    
}
