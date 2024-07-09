unit Float
{
    uses "/Source/Runtime/6502/ZeroPage"
    
    New()
    {
        // IEEE +0.0
        STZ ZP.LNEXT0
        STZ ZP.LNEXT1
        STZ ZP.LNEXT2
        STZ ZP.LNEXT3
        LDA # Types.Float
        Long.pushNewFromL();
    }
    
    NewFromConstant()
    {
        LDA # Types.Float
        Long.newFromConstant();
    }
    FromBytes()
    {
        LDA # Types.Float
        Long.fromBytes();
    }
    GetByte()
    {
        Long.GetByte();
    }
    
    isZeroNEXT()
    {
        LDX #0
        LDA ZP.LNEXT0
        if (Z)
        {
            LDA ZP.LNEXT1
            if (Z)
            {
                LDA ZP.LNEXT2
                if (Z)
                {
                    LDA ZP.LNEXT3
                    AND # 0x7F    // ignore the sign for -0
                    if (Z)
                    {
                        INX
                    }
                }
            }
        }
    }
    isZeroTOP()
    {
        LDX #0
        LDA ZP.LTOP0
        if (Z)
        {
            LDA ZP.LTOP1
            if (Z)
            {
                LDA ZP.LTOP2
                if (Z)
                {
                    LDA ZP.LTOP3
                    AND # 0x7F    // ignore the sign for -0
                    if (Z)
                    {
                        INX
                    }
                }
            }
        }
    }
    EQ()
    {
        Long.commonLongNEXTTOP();
        isZeroTOP();
        TXA
        if (NZ)
        {
            isZeroNEXT();
            TXA
            if (NZ)
            {
                // TOP == 0 && NEXT == 0
                // X is already 1
                Stacks.PushX(); // as Type.Bool
                return;
            }
        }
        // X is already 0
        LDA LNEXT0
        CMP LTOP0
        if (Z)
        {
            LDA LNEXT1
            CMP LTOP1
            if (Z)
            {
                LDA LNEXT2
                CMP LTOP2
                if (Z)
                {
                    LDA LNEXT3
                    CMP LTOP3
                    if (Z)
                    {
                        INX
                    }
                }
            }
        }
        Stacks.PushX(); // as Type.Bool
    }

    getSignNEXT()
    {
        STZ LSIGNNEXT
        if (BBS7, LNEXT3)
        {
            INC LSIGNNEXT
        }
    }
    getSignTOP()
    {
        STZ LSIGNTOP
        if (BBS7, LTOP3)
        {
            INC LSIGNTOP
        }
    }
    getExponentNEXT()
    {
        LDA LNEXT3
        ASL A
        if (BBS7, LNEXT2)
        {
            ORA # 0b00000001
        }        
        STA NEXTL
        STZ NEXTH
    }
    getExponentTOP()
    {
        LDA LTOP3
        ASL A
        if (BBS7, LTOP2)
        {
            ORA # 0b00000001
        }        
        STA TOPL
        STZ TOPH
    }
    getMantissaNEXT()
    {
        LDA LNEXT2
        AND # 0x7F
        STA LNEXT2
        STZ LNEXT3
    }
    getMantissaTOP()
    {
        LDA LTOP2
        AND # 0x7F
        STA LTOP2
        STZ LTOP3
    }
    countLeadingZeros()
    {
        LDX #0
        LDA ZP.LRESULT3
        if (Z)
        {
            LDX # 8
            
            LDA ZP.LRESULT2
            if (Z)
            {
                LDX # 16
                
                LDA ZP.LRESULT1
                if (Z)
                {
                    LDX # 24
                    
                    LDA ZP.LRESULT0
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
        Long.commonLongNEXTTOP();
        commonAdd();
    }
    Sub()
    {
        Long.commonLongNEXTTOP();
        
        // Flip the sign of TOP
        if (BBS7, LTOP3)
        {
            RMB7 LTOP3
        }
        else
        {
            SMB7 LTOP3
        }
        commonAdd();
    }
    commonAdd()
    {
        loop
        {
            Float.isZeroTOP();
            CPX # 1
            if (Z)
            {
                break;
            }
            Float.isZeroNEXT();
            CPX # 1
            if (Z)
            {
                Long.commonSwapNEXTTOP();
                break;
            }
            
            getSignNEXT();
            getExponentNEXT();
            getMantissaNEXT();
            
            getSignTOP();  
            getExponentTOP();
            getMantissaTOP();
            
            // Add the implicit leading '1' bit
            LDA LNEXT2
            ORA # 0b10000000
            STA LNEXT2
            LDA LTOP2
            ORA # 0b10000000
            STA LTOP2
            
            // Align exponents
            Instruction.gtShared();
            CPX # 1
            if (Z)
            {
                // int shift = exponentA - exponentB;
                SEC
                LDA ZP.NEXTL
                SBC ZP.TOPL
                TAX
                
                // mantissaB = Long.shiftRight(mantissaB, shift);
                loop
                {
                    LSR LTOP3
                    ROR LTOP2
                    ROR LTOP1
                    ROR LTOP0
                    DEX
                    if (Z) { break; }
                }
                
                // exponentB = exponentA;
                LDA ZP.NEXTL
                STA ZP.TOPL
            }
            else
            {
                Instruction.ltShared();
                CPX # 1
                if (Z)
                {
                    // int shift = exponentB - exponentA;
                    SEC
                    LDA ZP.TOPL
                    SBC ZP.NEXTL
                    TAX
                    
                    // mantissaA = Long.shiftRight(mantissaA, shift);
                    loop
                    {
                        LSR LNEXT3
                        ROR LNEXT2
                        ROR LNEXT1
                        ROR LNEXT0
                        DEX
                        if (Z) { break; }
                    }
                    // exponentA = exponentB;
                    LDA ZP.TOPL
                    STA ZP.NEXTL
                }
            }
            LDA LSIGNNEXT
            CMP LSIGNTOP
            if (Z)
            {
                CLC
                LDA ZP.LNEXT0
                ADC ZP.LTOP0
                STA ZP.LRESULT0
                LDA ZP.LNEXT1
                ADC ZP.LTOP1
                STA ZP.LRESULT1
                LDA ZP.LNEXT2
                ADC ZP.LTOP2
                STA ZP.LRESULT2
                LDA ZP.LNEXT3
                ADC ZP.LTOP3
                STA ZP.LRESULT3
            }
            else
            {
                Long.commonLT();
                CPX # 0 // not <, so >=
                if (Z)
                {
                    // mantissaA >= mantissaB
                    SEC
                    LDA ZP.LNEXT0
                    SBC ZP.LTOP0
                    STA ZP.LRESULT0
                    LDA ZP.LNEXT1
                    SBC ZP.LTOP1
                    STA ZP.LRESULT1
                    LDA ZP.LNEXT2
                    SBC ZP.LTOP2
                    STA ZP.LRESULT2
                    // always zeroes?
                    LDA ZP.LNEXT3
                    SBC ZP.LTOP3
                    STA ZP.LRESULT3
                }
                else
                {
                    // mantissaA < mantissaB
                    SEC
                    LDA ZP.LTOP0
                    SBC ZP.LNEXT0
                    STA ZP.LRESULT0
                    LDA ZP.LTOP1
                    SBC ZP.LNEXT1
                    STA ZP.LRESULT1
                    LDA ZP.LTOP2
                    SBC ZP.LNEXT2
                    STA ZP.LRESULT2
                    // always zeroes?
                    LDA ZP.LTOP3
                    SBC ZP.LNEXT3
                    STA ZP.LRESULT3
                    
                    LDA LSIGNTOP
                    STA LSIGNNEXT
                }
                // Check for zero mantissa (cancellation)
                LDA LRESULT0
                if (Z)
                {
                    LDA LRESULT1
                    if (Z)
                    {
                        LDA LRESULT2
                        if (Z)
                        {
                            LDA LRESULT3
                            if (Z)
                            {
                                // Return +0.0
                                STZ LNEXT0
                                STZ LNEXT1
                                STZ LNEXT2
                                STZ LNEXT3
                                break;
                            }
                        }
                    }
                }
            }
            countLeadingZeros();
            STX ZP.ACCH
            CPX # 8
            if (NC)
            {
                // leadingZeros < 8
                STX ZP.ACCL
                SEC
                LDA # 8
                SBC ZP.ACCL
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
            CLC
            LDA NEXTL
            ADC # 8
            STA NEXTL
            LDA NEXTT
            ADC # 0
            STA NEXTT
            SEC
            LDA NEXTL
            SBC ZP.ACCH
            STA NEXTL
            LDA NEXTT
            SBC # 0
            STA NEXTT
            loop
            {
                // Handle exponent overflow/underflow
                LDA NEXTH
                if (MI) 
                {
                    // exponentA < 0
                    STZ NEXTH
                    STZ NEXTL
                    STZ LRESULT0
                    STZ LRESULT1
                    STZ LRESULT2
                    STZ LRESULT3
                    break;
                }
                if (Z)
                {
                    LDA NEXTL
                    if (Z)
                    {
                        // exponentA == 0
                        STZ NEXTH
                        STZ NEXTL
                        STZ LRESULT0
                        STZ LRESULT1
                        STZ LRESULT2
                        STZ LRESULT3
                        break;
                    }
                    CMP # 0xFF
                    if (Z)
                    {
                        // exponentA == 255
                        STZ LRESULT0
                        STZ LRESULT1
                        STZ LRESULT2
                        STZ LRESULT3
                        break;
                    }
                }
                else
                {
                    // exponentA > 255
                    LDA # 0xFF
                    STA NEXTL
                    STZ NEXTH
                    STZ LRESULT0
                    STZ LRESULT1
                    STZ LRESULT2
                    STZ LRESULT3
                    break;
                }
                break;
            }
            
            // Remove the implicit leading bit
            LDA LRESULT2
            AND # 0x7F
            STA LRESULT2
            STZ LRESULT3
            
            // Set the least significant bit of the exponent
            LDA NEXTL
            AND # 0b00000001
            if (NZ)
            {
                LDA LRESULT2
                ORA # 0b10000000
                STA LRESULT2
            }
            
            // Take the next 7 bits of the exponent
            LDA NEXTL
            LSR
            STA LRESULT3
            
            // Set the sign bit
            LDA LSIGNNEXT
            if (NZ)
            {
                LDA # 0b10000000
                ORA LRESULT3
                STA LRESULT3
            }
            
            LDA LRESULT0
            STA LNEXT0
            LDA LRESULT1
            STA LNEXT1
            LDA LRESULT2
            STA LNEXT2
            LDA LRESULT3
            STA LNEXT3
            
            break;   
        }
        LDA # Types.Long
        Long.pushNewFromL(); 
    }
}
