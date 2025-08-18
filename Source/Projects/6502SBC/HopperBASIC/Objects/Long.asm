unit Long
{
    friend Time, IntMath;
    
    // Input: ZP.TOPL, ZP.TOPH, ZP.TOPT
    // Output: ZP.TOP0-3, ZP.TOPT
    TopToLong()
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
                    // Zero-extend BYTE: 0x42 -> 0x00000042
                    STZ ZP.TOP1
                    STZ ZP.TOP2
                    STZ ZP.TOP3
                }
                case BASICType.WORD:
                {
                    // Zero-extend WORD: 0x1234 -> 0x00001234
                    STZ ZP.TOP2
                    STZ ZP.TOP3
                }
                case BASICType.INT:
                {
                    // Sign-extend INT based on high bit
                    // Check sign bit in TOPH and extend accordingly
                    if (BBS7, ZP.TOP1) // Set MI (negative)
                    {
                        LDA #0xFF     // Negative: extend with 0xFF
                        STA ZP.TOP2
                        STA ZP.TOP3
                    }
                    else
                    {
                        STZ ZP.TOP2  // Positive: extend with 0x00
                        STZ ZP.TOP3
                    }
                }
                default:
                {
#ifdef DEBUG                    
//Debug.NL(); TLOut();
#endif
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
#ifndef BASICLONG
    PopTopNext()
    {
        Stacks.PopTopNext(); // revert to 16 bit only
    }   
    PopTop()
    {
        Stacks.PopTop(); // revert to 16 bit only
    } 
    PushTop()    
    {
        LDA ZP.TOPT
        Stacks.PushTop(); // revert to 16 bit only
    }
    PushNext()    
    {
        LDA ZP.NEXTT
        Stacks.PushNext(); // revert to 16 bit only
    }
#endif
#ifdef BASICLONG    
    // type in ZP.TOPT
    PushTop()    // Push 4-byte value from TOP0-3 + BASICType.LONG
    {
        LDY ZP.SP                    // Current stack pointer
        LDA ZP.TOPT
        STA TypeStack, Y           // Store type
        LDA ZP.TOP0
        STA ValueStackB0,Y          // Store byte 0 (LSB)
        LDA ZP.TOP1  
        STA ValueStackB1,Y          // Store byte 1
        if (BBS3, ZP.TOPT)
        {
            LDA ZP.TOP2
            STA ValueStackB2,Y         // Store byte 2
            LDA ZP.TOP3
            STA ValueStackB3,Y         // Store byte 3 (MSB)
        }
        INC ZP.SP                    // Advance stack pointer
    }
    // type in ZP.NEXTT
    PushNext()    // Push 4-byte value from NEXT0-3 + BASICType.LONG
    {
        LDY ZP.SP                    // Current stack pointer
        LDA ZP.NEXTT
        STA TypeStack, Y           // Store type
        LDA ZP.NEXT0
        STA ValueStackB0,Y          // Store byte 0 (LSB)
        LDA ZP.NEXT1
        STA ValueStackB1,Y          // Store byte 1
        if (BBS3, ZP.NEXTT)
        {
            LDA ZP.NEXT2
            STA ValueStackB2,Y         // Store byte 2
            LDA ZP.NEXT3
            STA ValueStackB3,Y         // Store byte 3 (MSB)
        }
        INC ZP.SP                    // Advance stack pointer
    }
    
    // Adaptive : pops 16 bits if that is the type, LONG if not : promotes inferior type to LONG (if only one is LONG)
    PopTopNext()
    {
        DEC ZP.SP                    
        LDY ZP.SP                    // Y point to TOP
        DEC ZP.SP                    // X point to NEXT
        LDX ZP.SP
        
        LDA TypeStack, Y
        STA ZP.TOPT
        LDA TypeStack, X
        STA ZP.NEXTT
        
        LDA ValueStackB0,Y          // Load byte 0
        STA ZP.TOP0
        LDA ValueStackB1,Y          // Load byte 1
        STA ZP.TOP1  
        if (BBS3, ZP.TOPT)
        {
            LDA ValueStackB2,Y          // Load byte 2
            STA ZP.TOP2
            LDA ValueStackB3,Y          // Load byte 3
            STA ZP.TOP3
        }
        
        LDA ValueStackB0,X          // Load byte 0
        STA ZP.NEXT0
        LDA ValueStackB1,X          // Load byte 1
        STA ZP.NEXT1  
        if (BBS3, ZP.NEXTT)
        {
            LDA ValueStackB2,X          // Load byte 2
            STA ZP.NEXT2
            LDA ValueStackB3,X          // Load byte 3
            STA ZP.NEXT3
            if (BBR3, ZP.TOPT)
            {
                // NEXT is LONG, TOP is not
                Long.TopToLong(); 
            }
        }
        else
        {
            if (BBS3, ZP.TOPT)
            {
                // TOP is LONG, NEXT is not
                Long.NextToLong(); 
            }
        }
    }
    
    // Adaptive PopTop: pops 16 bits if that is the type, LONG if LONG type
    PopTop()
    {
        DEC ZP.SP                    
        LDX ZP.SP                    // Y points to TOP
        
        LDA TypeStack, X
        STA ZP.TOPT
        
        LDA ValueStackB0, X         // Load byte 0
        STA ZP.TOP0
        LDA ValueStackB1, X         // Load byte 1
        STA ZP.TOP1  
        if (BBS3, ZP.TOPT)          // If LONG type
        {
            LDA ValueStackB2, X     // Load byte 2
            STA ZP.TOP2
            LDA ValueStackB3, X     // Load byte 3
            STA ZP.TOP3
        }
    }
    
    Mod()
    {
        DivMod();
        LDA ZP.RESULT0
        STA ZP.NEXT0
        LDA ZP.RESULT1
        STA ZP.NEXT1
        LDA ZP.RESULT2
        STA ZP.NEXT2
        LDA ZP.RESULT3
        STA ZP.NEXT3
        Long.PushNext(); 
    }
    Div()
    {
        utilityDoLongSigns();
        DivMod();
        LDA ZP.FSIGN // load the sign count
        CMP # 1
        if (Z)
        {
            negateLongNEXT(); // NEXT  = -NEXT 
        }
        Long.PushNext(); 
    }
    Mul()
    {
        // (IDY) = (NEXT) * (TOP)
        
        // #### https://llx.com/Neil/a2/mult.html ####
        // http://www.6502.org/source/integers/32muldiv.htm
    
        utilityLongMUL();
        LDA ZP.RESULT0
        STA ZP.NEXT0
        LDA ZP.RESULT1
        STA ZP.NEXT1
        LDA ZP.RESULT2
        STA ZP.NEXT2
        LDA ZP.RESULT3
        STA ZP.NEXT3
    
        LDA ZP.FSIGN // load the sign count
        CMP # 1
        if (Z)
        {
            negateLongNEXT(); // NEXT  = -NEXT 
        }
        Long.PushNext();
    }
    
    negateLongNEXT()
    {
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
    
    utilityDoLongSigns()
    {
        PHX
        LDX #0
        LDA ZP.NEXT3
        ASL // sign bit into carry
        if (C)
        {
            INX // count the -ve
            negateLongNEXT(); // NEXT = -NEXT
        }
        LDA ZP.TOP3
        ASL // sign bit into carry
        if (C)
        {
            INX // count the -ve
            negateLongTOP(); // TOP = -TOP
        }
        STX ZP.FSIGN // store the sign count
        
        PLX
    }
    
    utilityLongMUL()
    {
        // ZP.NEXT = ZP.NEXT0..ZP.NEXT3 * ZP.TOP0..ZP.TOP3
        
        // https://llx.com/Neil/a2/mult.html
        // http://www.6502.org/source/integers/32muldiv.htm
        
        LDA # 0x00
        STA ZP.RESULT4   // Clear upper half of
        STA ZP.RESULT5   // product
        STA ZP.RESULT6
        STA ZP.RESULT7
        LDX # 32     // set binary count to 32
        loop
        {
            LSR ZP.NEXT3   // shift multiplyer right
            ROR ZP.NEXT2
            ROR ZP.NEXT1
            ROR ZP.NEXT0
            if (C) // Go rotate right if c = 0
            {
                LDA ZP.RESULT4   // get upper half of product and add multiplicand to it
                CLC               
                ADC ZP.TOP0
                STA ZP.RESULT4
                LDA ZP.RESULT5
                ADC ZP.TOP1
                STA ZP.RESULT5
                LDA ZP.RESULT6
                ADC ZP.TOP2
                STA ZP.RESULT6
                LDA ZP.RESULT7
                ADC ZP.TOP3
            }
            ROR A    // rotate partial product
            STA ZP.RESULT7   // right
            ROR ZP.RESULT6
            ROR ZP.RESULT5
            ROR ZP.RESULT4
            ROR ZP.RESULT3
            ROR ZP.RESULT2
            ROR ZP.RESULT1
            ROR ZP.RESULT0
            DEX                // decrement bit count and
            if (Z) { break; }  // exit loop when 32 bits are done
        }
    }
        
    NextToLong()
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
                    // Zero-extend BYTE: 0x42 -> 0x00000042
                    STZ ZP.TOP1
                    STZ ZP.TOP2
                    STZ ZP.TOP3
                }
                case BASICType.WORD:
                {
                    // Zero-extend WORD: 0x1234 -> 0x00001234
                    STZ ZP.TOP2
                    STZ ZP.TOP3
                }
                case BASICType.INT:
                {
                    // Sign-extend INT based on high bit
                    // Check sign bit in TOPH and extend accordingly
                    if (BBS7, ZP.TOP1) // Set MI (negative)
                    {
                        LDA #0xFF     // Negative: extend with 0xFF
                        STA ZP.TOP2
                        STA ZP.TOP3
                    }
                    else
                    {
                        STZ ZP.TOP2  // Positive: extend with 0x00
                        STZ ZP.TOP3
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
    
    commonEQ()
    {
        // NEXT == TOP
        LDX # 0
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
    }
    commonLT()
    {
        LDX # 0
        
        // NEXT = NEXT - TOP
        SEC
        LDA ZP.NEXT0
        SBC ZP.TOP0
        LDA ZP.NEXT1
        SBC ZP.TOP1
        LDA ZP.NEXT2
        SBC ZP.TOP2
        LDA ZP.NEXT3
        SBC ZP.TOP3
        if (MI)
        {
            INX
        }
    }
    commonSwapNEXTTOP()
    {
        LDA ZP.TOP0
        TAY
        LDA ZP.NEXT0
        STA ZP.TOP0
        STY ZP.NEXT0
        LDA ZP.TOP1
        TAY
        LDA ZP.NEXT1
        STA ZP.TOP1
        STY ZP.NEXT1
        LDA ZP.TOP2
        TAY
        LDA ZP.NEXT2
        STA ZP.TOP2
        STY ZP.NEXT2
        LDA ZP.TOP3
        TAY
        LDA ZP.NEXT3
        STA ZP.TOP3
        STY ZP.NEXT3
    }
    LT()
    {
        commonLT();
        Stacks.PushX(); // as Type.Bool
    }
    GT()
    {
        commonSwapNEXTTOP();
        commonLT();
        Stacks.PushX(); // as Type.Bool
    }
    EQ()
    {  
        commonEQ();
        Stacks.PushX(); // as Type.Bool
    }
    NE()
    {  
        commonEQ();
        CPX # 0
        if (Z)
        {
            LDX #1
        }
        else
        {
            LDX #0
        }
        Stacks.PushX(); // as Type.Bool
    }
    LE()
    {
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
        commonEQ();
        CPX # 0
        if (Z)
        {
            commonSwapNEXTTOP();
            commonLT();           
        }
        Stacks.PushX(); // as Type.Bool
    }

#endif

    DivMod()
    {
        // NEXT = NEXT / TOP + RESULT
        
        // #### https://llx.com/Neil/a2/mult.html ####
        // http://www.6502.org/source/integers/32muldiv.htm
        
        // Initialize remainder to 0
        STZ RESULT0
        STZ RESULT1
        STZ RESULT2
        STZ RESULT3
        LDX #32       // there are 16 bits in N
        loop
        {
            ASL NEXT0    // shift hi bit of N into R
            ROL NEXT1    // (vacating the lo bit, which will be used for the quotient)
            ROL NEXT2
            ROL NEXT3
            ROL RESULT0
            ROL RESULT1
            ROL RESULT2
            ROL RESULT3
            
            SEC           // trial subtraction
            LDA RESULT0
            SBC TOP0
            STA RESULT4
            LDA RESULT1
            SBC TOP1
            STA RESULT5
            LDA RESULT2
            SBC TOP2
            STA RESULT6
            LDA RESULT3
            SBC TOP3
            //STA RESULT7
            if (C)        // did subtraction succeed?
            {
                // LDA LRESULT7
                STA RESULT3  // if yes, save it
                LDA RESULT6
                STA RESULT2
                LDA RESULT5 
                STA RESULT1
                LDA RESULT4
                STA RESULT0
                INC NEXT0    // and record a 1 in the quotient
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
    negateLongTOP()
    {
        SEC
        LDA # 0
        SBC TOP0
        STA TOP0
        LDA # 0
        SBC TOP1
        STA TOP1
        LDA # 0
        SBC TOP2
        STA TOP2
        LDA # 0
        SBC TOP3
        STA TOP3
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
    // Input: ZP.TOP0-3 = 32-bit number to print (0-4294967295)
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
        
        // Save ZP.TOP since we'll modify it during conversion
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        LDA ZP.TOP2
        PHA
        LDA ZP.TOP3
        PHA
        
//Debug.NL(); TLOut();        
        
        if (BBS7, ZP.TOP3)       // Negative
        {
            // Print minus sign
            LDA #'-'
            Serial.WriteChar();
            
            // Negate the 32-bit value: ZP.TOP = 0 - ZP.TOP
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
                // 32-bit subtraction: ZP.TOP = ZP.TOP - PrDec32Tens[Y]
                LDA ZP.TOP0
                SBC PrDec32Tens, Y
                STA ZP.TOP0
                LDA ZP.TOP1
                SBC PrDec32Tens+1, Y
                STA ZP.TOP1
                LDA ZP.TOP2
                SBC PrDec32Tens+2, Y
                STA ZP.TOP2
                LDA ZP.TOP3
                SBC PrDec32Tens+3, Y
                STA ZP.TOP3
                
                INX         // Count digits
                if (NC) { break; } // Loop until result < 0 (no carry)
            }
            
            // Add the power of 10 back (we subtracted one too many)
            LDA ZP.TOP0
            ADC PrDec32Tens, Y
            STA ZP.TOP0
            LDA ZP.TOP1
            ADC PrDec32Tens+1, Y
            STA ZP.TOP1
            LDA ZP.TOP2
            ADC PrDec32Tens+2, Y
            STA ZP.TOP2
            LDA ZP.TOP3
            ADC PrDec32Tens+3, Y
            STA ZP.TOP3
            
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
        
        // Restore ZP.TOP
        PLA
        STA ZP.TOP3
        PLA
        STA ZP.TOP2
        PLA
        STA ZP.TOP1
        PLA
        STA ZP.TOP0
        
        // Restore ZP.ACC
        PLA
        STA ZP.ACCH
        PLA
        STA ZP.ACCL
        
        PLY
        PLX
        PLA
    }
}
