unit Long
{
    friend Time, IntMath;
    
    // type in ZP.TOPT
    PushTop()    // Push 4-byte value from TOP0-3 + BASICType.LONG
    {
        loop
        {
            LDY ZP.SP                    // Current stack pointer
            LDA ZP.TOPT
            
            STA TypeStack, Y            // Store type
            LDA ZP.TOP0
            STA ValueStackB0,Y          // Store byte 0 (LSB)
            LDA ZP.TOP1  
            STA ValueStackB1,Y          // Store byte 1
            
            LDA ZP.TOP2
            STA ValueStackB2,Y          // Store byte 2
            LDA ZP.TOP3
            STA ValueStackB3,Y          // Store byte 3 (MSB)
            
            INC ZP.SP                   // Advance stack pointer
            
            SEC
            break;
        }
    }
    
    // type in ZP.TOPT
    PushTopStrict()    // Push 4-byte value from TOP0-3 + BASICType.LONG
    {
        loop
        {
            LDY ZP.SP                    // Current stack pointer
            LDA ZP.TOPT
            
            if (BBR3, ZP.TOPT)
            {
                BASICTypes.Promote();
                if (NC)
                {
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL
                    break;
                }
            }
            
            STA TypeStack, Y           // Store type
            LDA ZP.TOP0
            STA ValueStackB0,Y          // Store byte 0 (LSB)
            LDA ZP.TOP1  
            STA ValueStackB1,Y          // Store byte 1
            
            LDA ZP.TOP2
            STA ValueStackB2,Y         // Store byte 2
            LDA ZP.TOP3
            STA ValueStackB3,Y         // Store byte 3 (MSB)
            
            INC ZP.SP                    // Advance stack pointer
            
            SEC
            break;
        }
    }
    // type in ZP.NEXTT
    PushNext()    // Push 4-byte value from NEXT0-3 + BASICType.LONG
    {
        loop
        {
            LDY ZP.SP                    // Current stack pointer
            LDA ZP.NEXTT
            
            if (BBR3, ZP.NEXTT)
            {
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
        
            STA TypeStack, Y           // Store type
            LDA ZP.NEXT0
            STA ValueStackB0,Y          // Store byte 0 (LSB)
            LDA ZP.NEXT1
            STA ValueStackB1,Y          // Store byte 1
            LDA ZP.NEXT2
            STA ValueStackB2,Y         // Store byte 2
            LDA ZP.NEXT3
            STA ValueStackB3,Y         // Store byte 3 (MSB)

            INC ZP.SP                    // Advance stack pointer
            
            SEC
            break;
        }
    }
    
    PopTopNextStrict()
    {
        loop
        {
            DEC ZP.SP                    
            LDY ZP.SP                    // Y point to TOP
            DEC ZP.SP                    // X point to NEXT
            LDX ZP.SP
            
            LDA TypeStack, Y
            STA ZP.TOPT
            
            if (BBR3, ZP.TOPT)
            {
#ifdef DEBUG
Debug.NL(); LDA ZP.TOPT HOut();
#endif                
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            
            LDA TypeStack, X
            STA ZP.NEXTT
            
            if (BBR3, ZP.NEXTT)
            {
#ifdef DEBUG
Debug.NL(); LDA ZP.NEXTT HOut();
#endif                
                Error.TypeMismatch(); BIT ZP.EmulatorPCL
                break;
            }
            
            LDA ValueStackB0,Y          // Load byte 0
            STA ZP.TOP0
            LDA ValueStackB1,Y          // Load byte 1
            STA ZP.TOP1  
            LDA ValueStackB2,Y          // Load byte 2
            STA ZP.TOP2
            LDA ValueStackB3,Y          // Load byte 3
            STA ZP.TOP3
            
            LDA ValueStackB0,X          // Load byte 0
            STA ZP.NEXT0
            LDA ValueStackB1,X          // Load byte 1
            STA ZP.NEXT1  
            LDA ValueStackB2,X          // Load byte 2
            STA ZP.NEXT2
            LDA ValueStackB3,X          // Load byte 3
            STA ZP.NEXT3
            
            SEC
            break;
        } // single exit
    }
    
    PopTopNext()
    {
        loop
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
                if (BBR3, ZP.NEXTT)
                {
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL // if one is LONG, both must be LONG
                    break;
                }
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
                if (BBR3, ZP.TOPT)
                {
                    Error.TypeMismatch(); BIT ZP.EmulatorPCL // if one is LONG, both must be LONG
                    break;
                }
                LDA ValueStackB2,X          // Load byte 2
                STA ZP.NEXT2
                LDA ValueStackB3,X          // Load byte 3
                STA ZP.NEXT3
            }
            SEC
            break;
        }
    }
    // Input: A = signed offset from SP
    // Output: ZP.TOP = value at SP+offset, ZP.TOPT = type
    // Modifies: A, Y
    GetStackTopSP()
    {
        CLC
        ADC ZP.SP
        TAY
        LDA Address.ValueStackB0, Y
        STA ZP.TOP0
        LDA Address.ValueStackB1, Y
        STA ZP.TOP1
        LDA Address.ValueStackB0, Y
        STA ZP.TOP2
        LDA Address.ValueStackB1, Y
        STA ZP.TOP3
        LDA Address.TypeStackLSB, Y
        AND #BASICType.TYPEMASK  // Strip VAR bit 
        STA ZP.TOPT
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
        if (BBS3, ZP.TOPT)
        {
            LDA ValueStackB2,X          // Load byte 2
            STA ZP.TOP2
            LDA ValueStackB3,X          // Load byte 3
            STA ZP.TOP3
        }
    }
    
    // Adaptive PopNext: pops 16 bits if that is the type, LONG if LONG type
    PopNext()
    {
        DEC ZP.SP                    
        LDX ZP.SP                    // Y points to TOP
        
        LDA TypeStack, X
        STA ZP.NEXTT
        
        LDA ValueStackB0, X         // Load byte 0
        STA ZP.NEXT0
        LDA ValueStackB1, X         // Load byte 1
        STA ZP.NEXT1  
        if (BBS3, ZP.TOPT)
        {
            LDA ValueStackB2,X          // Load byte 2
            STA ZP.NEXT2
            LDA ValueStackB3,X          // Load byte 3
            STA ZP.NEXT3
        }
    }
    
    Mod()
    {
        LDA ZP.NEXT3
        PHA
        
        utilityDoLongSigns();
        DivMod();
        LDA ZP.RESULT0
        STA ZP.NEXT0
        LDA ZP.RESULT1
        STA ZP.NEXT1
        LDA ZP.RESULT2
        STA ZP.NEXT2
        LDA ZP.RESULT3
        STA ZP.NEXT3
        
        PLA // take the sign from the divisor (NEXT)
        if (MI)
        {
            negateLongNEXT(); // NEXT  = -NEXT 
        }
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
            NegateLongTOP(); // TOP = -TOP
        }
        STX ZP.FSIGN // store the sign count
        
        PLX
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

    DivMod()
    {
        // NEXT = NEXT / TOP + RESULT
        
        // #### https://llx.com/Neil/a2/mult.html ####
        // http://www.6502.org/source/integers/32muldiv.htm
        
        // TODO : optimize for TOP = 1, 2, 4, 8, 16
        
        // Initialize remainder to 0
        STZ RESULT0
        STZ RESULT1
        STZ RESULT2
        STZ RESULT3

        loop
        {
            // Check if both operands fit in 16 bits
            LDA NEXT3
            ORA TOP3
            ORA NEXT2  
            ORA TOP2
            if (Z)  // All high bytes are zero - use 16-bit division
            {
                // Clear upper bytes and initialize remainder
                STZ NEXT2
                STZ NEXT3
                
                LDX #16       // 16 iterations instead of 32
                loop
                {
                    ASL NEXT0    // shift hi bit of dividend into remainder
                    ROL NEXT1    
                    ROL RESULT0
                    ROL RESULT1
                    
                    SEC           // trial subtraction
                    LDA RESULT0
                    SBC TOP0
                    TAY          // temp storage
                    LDA RESULT1
                    SBC TOP1
                    if (C)        // did subtraction succeed?
                    {
                        STA RESULT1  // if yes, save it
                        STY RESULT0
                        INC NEXT0    // and record a 1 in the quotient
                    }
                    DEX
                    if (Z) { break; }
                }
            
                break; // 16 bit exit
            }
        
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
            break; // 32 bit exit
        } // loop
    }
    NegateLongTOP()
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
        if (MI)       // Negative
        {
            // Print minus sign
            LDA #'-'
            Serial.WriteChar();
            
            // Negate the 32-bit value: ZP.TOP = 0 - ZP.TOP
            NegateLongTOP();
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
    
    // APIs for Tokenizer.GetTokenNumber() 
    utilityLongMUL()
    {
        // ZP.NEXT = ZP.NEXT0..ZP.NEXT3 * ZP.TOP0..ZP.TOP3
        
        // https://llx.com/Neil/a2/mult.html
        // http://www.6502.org/source/integers/32muldiv.htm
        
        // TODO : optimize for 0, 1, 2, 4, 8, 16
        STZ ZP.RESULT0
        STZ ZP.RESULT1
        STZ ZP.RESULT2
        STZ ZP.RESULT3
        
        STZ ZP.RESULT4   // Clear upper half of
        STZ ZP.RESULT5   // product
        STZ ZP.RESULT6
        STZ ZP.RESULT7
        loop
        {
            // Check if both operands fit in 16 bits
            LDA ZP.NEXT3
            ORA ZP.TOP3
            ORA ZP.NEXT2  
            ORA ZP.TOP2
            if (Z)  // All high bytes are zero - use 16-bit multiply
            {
                LDA ZP.NEXT1
                ORA ZP.TOP1
                if (Z)
                {
                    LDA #0
                    LDX #8        // Only 8 iterations!
                    loop
                    {
                        LSR ZP.NEXT0
                        if (C)
                        {
                            CLC
                            ADC ZP.TOP0  // Simple 8-bit add
                        }
                        ROR A            // Rotate into result
                        ROR ZP.RESULT0
                        DEX
                        if (Z) { break; }
                    }
                    STA ZP.RESULT1   // Upper byte of 16-bit result
                    break; // 8 bit exit
                }
                
                LDA #0
                LDX #16                // 16 bits instead of 32
                loop
                {
                    LSR ZP.NEXT1       // shift 16-bit multiplier right
                    ROR ZP.NEXT0
                    if (C)             // Go rotate right if c = 0
                    {
                        LDA ZP.RESULT2   // get upper half of product and add multiplicand to it
                        CLC               
                        ADC ZP.TOP0
                        STA ZP.RESULT2
                        LDA ZP.RESULT3
                        ADC ZP.TOP1
                    }
                    ROR A              // rotate partial product
                    STA ZP.RESULT3     // right
                    ROR ZP.RESULT2
                    ROR ZP.RESULT1
                    ROR ZP.RESULT0
                    DEX                // decrement bit count and
                    if (Z) { break; }  // exit loop when 16 bits are done
                }
                break; // 16 bit exit
            }
            
            LDA # 0
            LDX # 32
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
            break; // 32 bit exit
        }
    }
    
    // Multiply 32-bit value by 10 in place
    // Input: ZP.TOP0-3 = 32-bit value to multiply
    // Output: ZP.TOP0-3 = input * 10, C set on success, NC on 32-bit overflow
    // Error: Returns NC if result > 0xFFFFFFFF
    MultiplyBy10()
    {
        // ZP.RESULT = ZP.NEXT0..ZP.NEXT3 * ZP.TOP0..ZP.TOP3
        LDA #10
        STA ZP.NEXT0
        STZ ZP.NEXT1
        STZ ZP.NEXT2
        STZ ZP.NEXT3
        utilityLongMUL();
        LDA ZP.RESULT0
        STA ZP.TOP0
        LDA ZP.RESULT1
        STA ZP.TOP1
        LDA ZP.RESULT2
        STA ZP.TOP2
        LDA ZP.RESULT3
        STA ZP.TOP3
        
        LDA ZP.RESULT4
        ORA ZP.RESULT5  // Check if any high bytes 
        ORA ZP.RESULT6  // are non-zero
        ORA ZP.RESULT7
        if (NZ)
        {
            CLC
        }
        else
        {
            SEC
        }
    }
    
    // Add single digit (0-9) to 32-bit value
    // Input: ZP.TOP0-3 = 32-bit value, A = digit (0-9)
    // Output: ZP.TOP0-3 = input + digit, C set on success, NC on 32-bit overflow  
    // Munts: ZP.TOP0, ZP.TOP1, ZP.TOP2, ZP.TOP3
    // Error: Returns NC if result > 0xFFFFFFFF or A > 9
    AddDigit()
    {
        CMP #10
        if (C)  // A >= 10, invalid digit
        {
            CLC  // Return NC for error
            return;
        }
        CLC
        ADC ZP.TOP0
        STA ZP.TOP0
        LDA ZP.TOP1
        ADC #0
        STA ZP.TOP1
        LDA ZP.TOP2
        ADC #0
        STA ZP.TOP2
        LDA ZP.TOP3
        ADC #0
        STA ZP.TOP3
        if (C)
        {
            CLC // Return NC for overflow
        }
        else
        {
            SEC // Return C for success
        }
    }
}
