unit Long
{
    friend Time, IntMath;
    
    PushTop()    // Push 4-byte value from LTOP0-3 + BASICType.LONG
    {
        LDY ZP.SP                    // Current stack pointer
        LDA #BASICType.LONG
        STA TypeStackLSB,Y           // Store type
        LDA ZP.LTOP0
        STA ValueStackLSB,Y          // Store byte 0 (LSB)
        LDA ZP.LTOP1  
        STA ValueStackMSB,Y          // Store byte 1
        LDA ZP.LTOP2
        STA ValueStackMSB2,Y         // Store byte 2
        LDA ZP.LTOP3
        STA ValueStackMSB3,Y         // Store byte 3 (MSB)
        INC ZP.SP                    // Advance stack pointer
    }

    PopTop()     // Pop 4-byte value to LTOP0-3, verify LONG type
    {
        DEC ZP.SP                    // Move back to top item
        LDY ZP.SP
        LDA ValueStackLSB,Y          // Load byte 0 (LSB)
        STA ZP.LTOP0
        LDA ValueStackMSB,Y          // Load byte 1
        STA ZP.LTOP1  
        LDA ValueStackMSB2,Y         // Load byte 2
        STA ZP.LTOP2
        LDA ValueStackMSB3,Y         // Load byte 3 (MSB)
        STA ZP.LTOP3
        // Optional: verify type is BASICType.LONG
    }
    
    PopNext()     // Pop 4-byte value to LNEXT0-3, verify LONG type
    {
        DEC ZP.SP                    // Move back to top item
        LDY ZP.SP
        LDA ValueStackLSB,Y          // Load byte 0 (LSB)
        STA ZP.LNEXT0
        LDA ValueStackMSB,Y          // Load byte 1
        STA ZP.LNEXT1  
        LDA ValueStackMSB2,Y         // Load byte 2
        STA ZP.LNEXT2
        LDA ValueStackMSB3,Y         // Load byte 3 (MSB)
        STA ZP.LNEXT3
        // Optional: verify type is BASICType.LONG
    }
    
    IsLong() // checks A: C if LONG, NC if not
    {
        PHA
        AND # BASICType.TYPEMASK
        CMP # BASICType.LONG
        if (Z)
        {
            SEC
        }
        else
        {
            CLC
        }
        PLA
    }
    
    // Input: ZP.TOPL, ZP.TOPH, ZP.TOPT
    // Output: ZP.LTOP0-3, ZP.TOPT
    ToLong()
    {
        loop
        {
            // Check the current type and convert accordingly
            LDA ZP.TOPT
            AND # BASICType.TYPEMASK
            switch (A)
            {
                case BASICType.BYTE:
                {
                    // Zero-extend BYTE: 0x42 → 0x00000042
                    LDA ZP.TOPL
                    STA ZP.LTOP0
                    STZ ZP.LTOP1
                    STZ ZP.LTOP2
                    STZ ZP.LTOP3
                }
                case BASICType.WORD:
                {
                    // Zero-extend WORD: 0x1234 → 0x00001234
                    LDA ZP.TOPL
                    STA ZP.LTOP0
                    LDA ZP.TOPH
                    STA ZP.LTOP1
                    STZ ZP.LTOP2
                    STZ ZP.LTOP3
                }
                case BASICType.INT:
                {
                    // Sign-extend INT based on high bit
                    LDA ZP.TOPL
                    STA ZP.LTOP0
                    LDA ZP.TOPH
                    STA ZP.LTOP1
                    
                    // Check sign bit in TOPH and extend accordingly
                    if (MI) // Set MI (negative)
                    {
                        LDA #0xFF     // Negative: extend with 0xFF
                        STA ZP.LTOP2
                        STA ZP.LTOP3
                    }
                    else
                    {
                        STZ ZP.LTOP2  // Positive: extend with 0x00
                        STZ ZP.LTOP3
                    }
                }
                default:
                {
                    // Unsupported type for LONG conversion
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    CLC
                }
                
            }
            LDA #BASICType.LONG
            STA ZP.TOPT
            SEC
            break;
        } // single exit
    }
    
    // Powers of 10 table for 32-bit LONG values (little-endian format)
    // Each entry is 4 bytes: LSB, byte1, byte2, MSB
    const byte[] PrDec32Tens = { 
        0x01, 0x00, 0x00, 0x00,  // 1
        0x0A, 0x00, 0x00, 0x00,  // 10
        0x64, 0x00, 0x00, 0x00,  // 100  
        0xE8, 0x03, 0x00, 0x00,  // 1000
        0x10, 0x27, 0x00, 0x00,  // 10000
        0xA0, 0x86, 0x01, 0x00,  // 100000
        0x40, 0x42, 0x0F, 0x00,  // 1000000
        0x80, 0x96, 0x98, 0x00,  // 10000000
        0x00, 0xE1, 0xF5, 0x05,  // 100000000
        0x00, 0xCA, 0x9A, 0x3B   // 1000000000
    };

    // Print 32-bit LONG decimal number with no leading zeros
    // Input: ZP.LTOP0-3 = 32-bit number to print (0-4294967295)
    //        ZP.TOPT = type (for signed/unsigned determination) 
    // Output: Decimal number printed to serial
    // Preserves: Everything
    Print()
    {
        PHA
        PHX
        PHY
        
        // Save ZP.ACC since we'll use it as working space
        LDA ZP.ACCL
        PHA
        LDA ZP.ACCH
        PHA
        
        // Save ZP.LTOP since we'll modify it during conversion
        LDA ZP.LTOP0
        PHA
        LDA ZP.LTOP1
        PHA
        LDA ZP.LTOP2
        PHA
        LDA ZP.LTOP3
        PHA
        
        
        BIT ZP.LTOP3  // Test sign bit of MSB
        if (MI)       // Negative
        {
            // Print minus sign
            LDA #'-'
            Serial.WriteChar();
            
            // Negate the 32-bit value: LTOP = 0 - LTOP
            negateLongTOP();
        }
        
        STZ ZP.ACCL         // Initialize: no padding (suppress leading zeros)
        
        LDY #36             // Offset to powers of ten table (10 entries × 4 bytes = 40, start at end: 36)
        
        loop                // Outer loop for each digit
        {
            LDX #0xFF       // Start with digit = -1
            SEC             // Prepare for subtraction
            
            loop            // Inner loop - subtract current power of 10
            {
                // 32-bit subtraction: LTOP = LTOP - PrDec32Tens[Y]
                LDA ZP.LTOP0
                SBC PrDec32Tens, Y
                STA ZP.LTOP0
                LDA ZP.LTOP1
                SBC PrDec32Tens+1, Y
                STA ZP.LTOP1
                LDA ZP.LTOP2
                SBC PrDec32Tens+2, Y
                STA ZP.LTOP2
                LDA ZP.LTOP3
                SBC PrDec32Tens+3, Y
                STA ZP.LTOP3
                
                INX         // Count digits
                if (NC) { break; } // Loop until result < 0 (no carry)
            }
            
            // Add the power of 10 back (we subtracted one too many)
            LDA ZP.LTOP0
            ADC PrDec32Tens, Y
            STA ZP.LTOP0
            LDA ZP.LTOP1
            ADC PrDec32Tens+1, Y
            STA ZP.LTOP1
            LDA ZP.LTOP2
            ADC PrDec32Tens+2, Y
            STA ZP.LTOP2
            LDA ZP.LTOP3
            ADC PrDec32Tens+3, Y
            STA ZP.LTOP3
            
            TXA             // Get digit count
            if (NZ)         // Not zero, print it
            {
                LDX #'0'    // No more zero padding needed
                STX ZP.ACCL
                ORA #'0'    // Convert digit to ASCII
                Serial.WriteChar();
            }
            else
            {
                LDA ZP.ACCL // Check padding
                if (NZ)     // pad != 0, use it
                {
                    Serial.WriteChar();
                }
            }
            
            // Move to next power of 10 (table entries are 4 bytes each)
            DEY
            DEY
            DEY
            DEY
            if (MI) { break; } // Exit when Y goes negative
        }
        
        // If we never printed anything (ACCL is still 0), the number was 0
        LDA ZP.ACCL
        if (Z)  // Never set padding, so number was 0
        {
            LDA #'0'
            Serial.WriteChar();
        }
        
        // Restore ZP.LTOP
        PLA
        STA ZP.LTOP3
        PLA
        STA ZP.LTOP2
        PLA
        STA ZP.LTOP1
        PLA
        STA ZP.LTOP0
        
        // Restore ZP.ACC
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
    }

    compareEqual()
    {
        // compare two objects in IDX and IDY
        //    munts Y, sets X
        LDY # siData
        LDX # 4
        loop
        {
            LDA [IDX], Y
            CMP [IDY], Y
            if (NZ)
            {
                LDX # 0 // different -> not equal
                break;
            }
            INY
            DEX
            if (Z)
            {
                LDX # 1 // no differences -> equal
                break;
            }
        }
    }

    commonLongTOP()
    {
        Stacks.PopIDX();
        LDY # siData
        LDA [IDX], Y
        STA ZP.LTOP0
        INY
        LDA [IDX], Y
        STA ZP.LTOP1
        INY
        LDA [IDX], Y
        STA ZP.LTOP2
        INY
        LDA [IDX], Y
        STA ZP.LTOP3
        GC.Release();
    }
    commonLongNEXT()
    {
        Stacks.PopIDX();
        LDY # siData
        LDA [IDX], Y
        STA ZP.LNEXT0
        INY
        LDA [IDX], Y
        STA ZP.LNEXT1
        INY
        LDA [IDX], Y
        STA ZP.LNEXT2
        INY
        LDA [IDX], Y
        STA ZP.LNEXT3
        GC.Release();
    }
    commonLongNEXTTOP()
    {
        commonLongTOP();
        commonLongNEXT();
    }

    pushNewFromL()
    {
        STA FTYPE
        
        // type in A
        // size is in FSIZE
        // return address in IDX
        LDA #0
        STA ZP.FSIZEH
        LDA #4
        STA ZP.FSIZEL
        LDA FTYPE
        GC.Create();   
        
        LDY # siData
        LDA ZP.LNEXT0
        STA [IDX], Y
        INY 
        LDA ZP.LNEXT1
        STA [IDX], Y
        INY 
        LDA ZP.LNEXT2
        STA [IDX], Y
        INY 
        LDA ZP.LNEXT3
        STA [IDX], Y
        
        LDA IDXL
        STA TOPL
        LDA IDXH
        STA TOPH
        LDA FTYPE
        Stacks.PushTop();
    }
    
    fromBytes()
    {
        STA FTYPE
        PopTop(); // B3
        LDA ZP.TOPL
        STA ZP.LNEXT3
        PopTop(); // B2
        LDA ZP.TOPL
        STA ZP.LNEXT2
        PopTop(); // B1
        LDA ZP.TOPL
        STA ZP.LNEXT1
        PopTop(); // B0
        LDA ZP.TOPL
        STA ZP.LNEXT0
        LDA FTYPE
        pushNewFromL();
    }
    newFromConstant()
    {
        STA FTYPE
        PopNext(); // location
        
        // constant data address -> FSOURCEADDRESSL
        LDY # 2
        CLC
        LDA Address.HopperData, Y
        ADC # (Address.HopperData & 0xFF)
        STA FSOURCEADDRESSL
        INY
        LDA Address.HopperData, Y
        ADC # (Address.HopperData >> 8)
        STA FSOURCEADDRESSH
        
        // += location
        CLC
        LDA NEXTL
        ADC FSOURCEADDRESSL
        STA FSOURCEADDRESSL
        LDA NEXTH
        ADC FSOURCEADDRESSH
        STA FSOURCEADDRESSH
        
        LDY # 0
        LDA [FSOURCEADDRESS], Y
        STA ZP.LNEXT0
        INY
        LDA [FSOURCEADDRESS], Y
        STA ZP.LNEXT1
        INY
        LDA [FSOURCEADDRESS], Y
        STA ZP.LNEXT2
        INY
        LDA [FSOURCEADDRESS], Y
        STA ZP.LNEXT3
        
        LDA FTYPE
        pushNewFromL();
    }
    New()
    {
        STZ ZP.LNEXT0
        STZ ZP.LNEXT1
        STZ ZP.LNEXT2
        STZ ZP.LNEXT3
        LDA # Types.Long
        pushNewFromL();
    }
    NewFromConstant()
    {
        LDA # Types.Long
        Long.newFromConstant();
    }
    FromBytes()
    {
        LDA # Types.Long
        Long.fromBytes();
    }
    GetByte()
    {
        Stacks.PopIDY(); // index
        Stacks.PopIDX(); // this
        
        CLC
        LDA ZP.IDYL
        ADC # siData
        TAY 
        LDA [IDX], Y
        STA ZP.NEXTL
        LDA # 0
        STA ZP.NEXTH
         
        GC.Release();
        
        LDA # Types.Byte
        Stacks.PushNext();
    }
    
    negateLongTOP()
    {
        SEC
        LDA # 0
        SBC LTOP0
        STA LTOP0
        LDA # 0
        SBC LTOP1
        STA LTOP1
        LDA # 0
        SBC LTOP2
        STA LTOP2
        LDA # 0
        SBC LTOP3
        STA LTOP3
    }
    negateLongNEXT()
    {
        SEC
        LDA # 0
        SBC LNEXT0
        STA LNEXT0
        LDA # 0
        SBC LNEXT1
        STA LNEXT1
        LDA # 0
        SBC LNEXT2
        STA LNEXT2
        LDA # 0
        SBC LNEXT3
        STA LNEXT3
    }
    
    utilityDoLongSigns()
    {
        PHX
        LDX #0
        LDA ZP.LNEXT3
        ASL // sign bit into carry
        if (C)
        {
            INX // count the -ve
            negateLongNEXT(); // NEXT = -NEXT
        }
        LDA ZP.LTOP3
        ASL // sign bit into carry
        if (C)
        {
            INX // count the -ve
            negateLongTOP(); // TOP = -TOP
        }
        STX ZP.FSIGN // store the sign count
        
        PLX
    }
        
    DivMod()
    {
        // LNEXT = LNEXT / LTOP + LRESULT
        
        // #### https://llx.com/Neil/a2/mult.html ####
        // http://www.6502.org/source/integers/32muldiv.htm
        
        // Initialize remainder to 0
        STZ LRESULT0
        STZ LRESULT1
        STZ LRESULT2
        STZ LRESULT3
        LDX #32       // there are 16 bits in N

        loop
        {
            ASL LNEXT0    // shift hi bit of N into R
            ROL LNEXT1    // (vacating the lo bit, which will be used for the quotient)
            ROL LNEXT2
            ROL LNEXT3
            ROL LRESULT0
            ROL LRESULT1
            ROL LRESULT2
            ROL LRESULT3
            
            SEC           // trial subtraction
            LDA LRESULT0
            SBC LTOP0
            STA LRESULT4
            LDA LRESULT1
            SBC LTOP1
            STA LRESULT5
            LDA LRESULT2
            SBC LTOP2
            STA LRESULT6
            LDA LRESULT3
            SBC LTOP3
            //STA LRESULT7
            if (C)        // did subtraction succeed?
            {
                // LDA LRESULT7
                STA LRESULT3  // if yes, save it
                LDA LRESULT6
                STA LRESULT2
                LDA LRESULT5 
                STA LRESULT1
                LDA LRESULT4
                STA LRESULT0
                INC LNEXT0    // and record a 1 in the quotient
            }
            DEX
            if (Z) { break; }
        } // loop
        CLC
        LDA #2
        ADC IDXL
        STA IDYL
        LDA #0
        ADC IDXH
        STA IDYH
    }

    Add()
    {  
        commonLongTOP();
        Stacks.PopIDX();
        CLC
        LDY # siData
        LDA [IDX], Y
        ADC LTOP0
        STA LNEXT0
        INY
        LDA [IDX], Y
        ADC LTOP1
        STA LNEXT1
        INY
        LDA [IDX], Y
        ADC LTOP2
        STA LNEXT2
        INY
        LDA [IDX], Y
        ADC LTOP3
        STA LNEXT3
        GC.Release();
        
        LDA # Types.Long
        pushNewFromL(); 
    }
    AddB()
    {  
        Stacks.PopTop();
        Stacks.PopIDX();
        CLC
        LDY # siData
        LDA [IDX], Y
        ADC ZP.TOPL
        STA LNEXT0
        INY
        LDA [IDX], Y
        ADC # 0
        STA LNEXT1
        INY
        LDA [IDX], Y
        ADC # 0
        STA LNEXT2
        INY
        LDA [IDX], Y
        ADC # 0
        STA LNEXT3
        GC.Release();
        
        LDA # Types.Long
        pushNewFromL(); 
    }
    Sub()
    {  
        commonLongTOP();
        Stacks.PopIDX();
        SEC
        LDY # siData
        LDA [IDX], Y
        SBC LTOP0
        STA LNEXT0
        INY
        LDA [IDX], Y
        SBC LTOP1
        STA LNEXT1
        INY
        LDA [IDX], Y
        SBC LTOP2
        STA LNEXT2
        INY
        LDA [IDX], Y
        SBC LTOP3
        STA LNEXT3
        GC.Release();
        
        LDA # Types.Long
        pushNewFromL(); 
    }
    SubB()
    {  
        Stacks.PopTop();
        Stacks.PopIDX();
        SEC
        LDY # siData
        LDA [IDX], Y
        SBC ZP.TOPL
        STA LNEXT0
        INY
        LDA [IDX], Y
        SBC # 0
        STA LNEXT1
        INY
        LDA [IDX], Y
        SBC # 0
        STA LNEXT2
        INY
        LDA [IDX], Y
        SBC # 0
        STA LNEXT3
        GC.Release();
        
        LDA # Types.Long
        pushNewFromL(); 
    }
    Div()
    {
        commonLongNEXTTOP();
        utilityDoLongSigns();
        DivMod();
        LDA ZP.FSIGN // load the sign count
        CMP # 1
        if (Z)
        {
            negateLongNEXT(); // NEXT  = -NEXT 
        }
        LDA # Types.Long
        pushNewFromL(); 
    }
    Mod()
    {
        commonLongNEXTTOP();
        DivMod();
        LDA LRESULT0
        STA LNEXT0
        LDA LRESULT1
        STA LNEXT1
        LDA LRESULT2
        STA LNEXT2
        LDA LRESULT3
        STA LNEXT3
        LDA # Types.Long
        pushNewFromL(); 
    }
    
    utilityLongMUL()
    {
        // LNEXT = LNEXT0..LNEXT3 * LTOP0..LTOP3
        
        // https://llx.com/Neil/a2/mult.html
        // http://www.6502.org/source/integers/32muldiv.htm
        
        LDA # 0x00
        STA LRESULT4   // Clear upper half of
        STA LRESULT5   // product
        STA LRESULT6
        STA LRESULT7
        LDX # 32     // set binary count to 32
        loop
        {
            LSR LNEXT3   // shift multiplyer right
            ROR LNEXT2
            ROR LNEXT1
            ROR LNEXT0
            if (C) // Go rotate right if c = 0
            {
                LDA LRESULT4   // get upper half of product and add multiplicand to it
                CLC               
                ADC LTOP0
                STA LRESULT4
                LDA LRESULT5
                ADC LTOP1
                STA LRESULT5
                LDA LRESULT6
                ADC LTOP2
                STA LRESULT6
                LDA LRESULT7
                ADC LTOP3
            }
            ROR A    // rotate partial product
            STA LRESULT7   // right
            ROR LRESULT6
            ROR LRESULT5
            ROR LRESULT4
            ROR LRESULT3
            ROR LRESULT2
            ROR LRESULT1
            ROR LRESULT0
            DEX                // decrement bit count and
            if (Z) { break; }  // exit loop when 32 bits are done
        }
    }
    
        
    Mul()
    {
        commonLongNEXTTOP();
        utilityDoLongSigns();
    
        // (IDY) = (NEXT) * (TOP)
        
        // #### https://llx.com/Neil/a2/mult.html ####
        // http://www.6502.org/source/integers/32muldiv.htm
    
        utilityLongMUL();
        LDA LRESULT0
        STA LNEXT0
        LDA LRESULT1
        STA LNEXT1
        LDA LRESULT2
        STA LNEXT2
        LDA LRESULT3
        STA LNEXT3
    
        LDA ZP.FSIGN // load the sign count
        CMP # 1
        if (Z)
        {
            negateLongNEXT(); // NEXT  = -NEXT 
        }
        
        LDA # Types.Long
        pushNewFromL(); 
    }
    
        
    Negate()
    {  
        Stacks.PopIDX();
        SEC
        LDY # siData
        LDA # 0
        SBC [IDX], Y
        STA LNEXT0
        INY
        LDA # 0
        SBC [IDX], Y
        STA LNEXT1
        INY
        LDA # 0
        SBC [IDX], Y
        STA LNEXT2
        INY
        LDA # 0
        SBC [IDX], Y
        STA LNEXT3
        GC.Release();
        
        LDA # Types.Long
        pushNewFromL(); 
    }
    commonEQ()
    {
        // NEXT == TOP
        LDX # 0
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
    }
    commonLT()
    {
        LDX # 0
        
        // NEXT = NEXT - TOP
        SEC
        LDA LNEXT0
        SBC LTOP0
        LDA LNEXT1
        SBC LTOP1
        LDA LNEXT2
        SBC LTOP2
        LDA LNEXT3
        SBC LTOP3
        if (MI)
        {
            INX
        }
    }
    commonSwapNEXTTOP()
    {
        LDA ZP.LTOP0
        TAY
        LDA ZP.LNEXT0
        STA ZP.LTOP0
        STY ZP.LNEXT0
        LDA ZP.LTOP1
        TAY
        LDA ZP.LNEXT1
        STA ZP.LTOP1
        STY ZP.LNEXT1
        LDA ZP.LTOP2
        TAY
        LDA ZP.LNEXT2
        STA ZP.LTOP2
        STY ZP.LNEXT2
        LDA ZP.LTOP3
        TAY
        LDA ZP.LNEXT3
        STA ZP.LTOP3
        STY ZP.LNEXT3
    }
    LT()
    {
        commonLongNEXTTOP();
        commonLT();
        Stacks.PushX(); // as Type.Bool
    }
    GT()
    {
        commonLongNEXTTOP();
        commonSwapNEXTTOP();
        commonLT();
        Stacks.PushX(); // as Type.Bool
    }
    EQ()
    {  
        commonLongNEXTTOP();
        commonEQ();
        Stacks.PushX(); // as Type.Bool
    }
    LE()
    {
        commonLongNEXTTOP();
        commonEQ();
        CPX # 0
        if (Z)
        {
            commonLT();           
        }
        Stacks.PushX(); // as Type.Bool
    }
    GE()
    {
        commonLongNEXTTOP();
        commonEQ();
        CPX # 0
        if (Z)
        {
            commonSwapNEXTTOP();
            commonLT();           
        }
        Stacks.PushX(); // as Type.Bool
    }

}
