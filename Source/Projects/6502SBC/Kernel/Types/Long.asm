unit Long
{
    friend Time, Float;
        
    
    shiftNEXTleft()
    {
        ASL ZP.NEXT0
        ROL ZP.NEXT1
        ROL ZP.NEXT2
        ROL ZP.NEXT3
        ROL ZP.RESULT4
        ROL ZP.RESULT5
        ROL ZP.RESULT6
        ROL ZP.RESULT7
        
        if (C)
        {
            Error.NumericOverflow(); 
            BIT ZP.EmulatorPCL
        }
    }
    shiftNEXTright()  // 9 bytesincluding RTS
    {
        LSR ZP.NEXT3
        ROR ZP.NEXT2
        ROR ZP.NEXT1
        ROR ZP.NEXT0
    }
    
    Mod()
    {
        LDA ZP.NEXT3
        PHA
        
        utilityDoLongSigns();
        LDX #1 // Mod
        DivMod(); // RESULT = NEXT % TOP
        if (NC) { PLA return; }
        // RESULT0-3 -> NEXT0-3
        Shared.MoveResultToNext();
        
        PLA // take the sign from the divisor (NEXT)
        if (MI)
        {
            NegateNext(); // NEXT  = -NEXT 
        }
        SEC
        // result in NEXT
    }
    Div()
    {
        utilityDoLongSigns();
        LDX #0 // Div
        DivMod(); // NEXT = NEXT / TOP
        if (NC) { return; }
        LDA ZP.TEMP // load the sign count
        CMP # 1
        if (Z)
        {
            NegateNext(); // NEXT  = -NEXT 
        }
        SEC
        // result in NEXT
    }
    
    Add()
    {
        // NEXT = NEXT + TOP
        CLC
        LDA ZP.NEXT0
        ADC ZP.TOP0
        STA ZP.NEXT0
        LDA ZP.NEXT1
        ADC ZP.TOP1
        STA ZP.NEXT1
        LDA ZP.NEXT2
        ADC ZP.TOP2
        STA ZP.NEXT2
        LDA ZP.NEXT3
        ADC ZP.TOP3
        STA ZP.NEXT3
    }
    Sub()
    {
        // NEXT = NEXT - TOP
        SEC
        LDA ZP.NEXT0
        SBC ZP.TOP0
        STA ZP.NEXT0
        LDA ZP.NEXT1
        SBC ZP.TOP1
        STA ZP.NEXT1
        LDA ZP.NEXT2
        SBC ZP.TOP2
        STA ZP.NEXT2
        LDA ZP.NEXT3
        SBC ZP.TOP3
        STA ZP.NEXT3
    }
    Mul()
    {
        // (NEXT) = (NEXT) * (TOP)
        utilityDoLongSigns();
        // #### https://llx.com/Neil/a2/mult.html ####
        // http://www.6502.org/source/integers/32muldiv.htm
    
        utilityLongMUL();
        Shared.MoveResultToNext(); // RESULT0-3 -> NEXT0-3
        
        LDA ZP.TEMP // load the sign count
        CMP # 1
        if (Z)
        {
            NegateNext(); // NEXT  = -NEXT 
        }
        // result in NEXT
        
    }
    
    NegateNext()
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
            NegateNext(); // NEXT = -NEXT
        }
        LDA ZP.TOP3
        ASL // sign bit into carry
        if (C)
        {
            INX // count the -ve
            NegateTop(); // TOP = -TOP
        }
        STX ZP.TEMP // store the sign count
        
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
        CPX #1
        if (Z)
        {
            SEC
        }
        else
        {
            CLC
        }
    }
    commonLT()
    {
        // <result> = NEXT - TOP
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
            SEC
        }
        else
        {
            CLC
        }
    }
    LT()
    {
        commonLT();
    }
    GT()
    {
        Shared.SwapNextTop();
        commonLT();
    }
    EQ()
    {  
        commonEQ();
    }
    NE()
    {  
        commonEQ();
        if (C)
        {
            CLC
        }
        else
        {
            SEC
        }
    }
    LE()
    {
        commonEQ();
        if (NC)
        {
            commonLT();           
        }
    }
    
    GE()
    {
        commonEQ();
        if (NC)
        {
            Shared.SwapNextTop();
            commonLT();           
        }
    }
    
        
    const byte[] modRemaining  = { 0x00, 0x06, 0x02, 0x08, 0x04, 0x00, 0x06, 0x02, 0x08, 0x04 };
    const byte[] tensRemaining = { 0,      25,   51,   76,  102,  128,  153,  179,  204,  230 };
    
    // NEXT = NEXT / 10, remainder in RESULT
    utility16BitDiv10()
    {
        
        LDA ZP.NEXT0
        STA ZP.TOP0
        LDA ZP.NEXT1
        STA ZP.TOP1
        
        //    UNSIGNED DIVIDE BY 10 (16 BIT)
        //    111 cycles (max), 96 bytes
        //    https://forums.atariage.com/blogs/entry/11044-16-bit-division-fast-divide-by-10/
        loop
        {
            LDA    ZP.TOP1
            STA    ZP.ACCL
            LSR        
            ADC    #13 
            ADC    ZP.ACCL
            ROR        
            LSR        
            LSR        
            ADC    ZP.ACCL
            ROR        
            ADC    ZP.ACCL
            ROR        
            LSR        
            AND    # 0x7C                // AND'ing here...
            STA    ZP.ACCL                  // and saving result as highTen (times 4)
            LSR        
            LSR        
            STA    ZP.NEXT1
            ADC    ZP.ACCL                  // highTen (times 5)
            ASL                          // highTen (times 10)
            SBC    ZP.TOP1
            EOR    # 0xFF
            TAY                          // mod 10 result!
            
            LDA    tensRemaining, Y      // Fill the low byte with the tens it should
            STA    ZP.NEXT0                 // have at this point from the high byte divide.
            LDA    ZP.TOP0
            ADC    modRemaining, Y       // 4  @69
            
            if (NC) 
            { 
                STA    ZP.ACCL
                LSR        
                ADC    # 13 
                ADC    ZP.ACCL
                ROR        
                LSR        
                LSR        
                ADC    ZP.ACCL
                ROR        
                ADC    ZP.ACCL
                ROR        
                LSR        
                LSR        
                LSR        
                CLC  
                break; 
            }
            
            CMP    #4                    //  We have overflowed, but we can apply a shortcut.
            LDA    #25                   //  Divide by 10 will be at least 25, and the
            if (NZ)                      //  carry is set when higher for the next addition. 
            { 
                break; 
            }
        }
        ADC    ZP.NEXT0
        STA    ZP.NEXT0
    }

    // X = 0 means you only care about the / result in NEXT
    // X = 1 means you care about the % result in RESULT (and you'll get the / result in NEXT)
    DivMod()
    {
        // NEXT = NEXT / TOP + RESULT
        
        // #### https://llx.com/Neil/a2/mult.html ####
        // http://www.6502.org/source/integers/32muldiv.htm
        
        // Initialize remainder to 0
        Shared.ZeroResult();
        loop
        {
            // Check for division by zero
            Shared.ZeroCheckTop();
            if (Z)  // Divisor is zero
            {
                Error.DivisionByZero(); BIT ZP.EmulatorPCL
                CheckError();
                return;
            }
            
            // only do optimizations in Div (X = 0) mode, not Mod (X = 1) mode since we don't calculate remainder
            CPX #0
            if (Z)
            {
                LDA ZP.TOP2
                ORA ZP.TOP3
                if (Z) // 16 bit divisor
                {
                    LDA ZP.TOP1
                    if (Z)
                    {
                        // TOP is the divisor (0..255)
                        LDA ZP.TOP0
                        CMP #1
                        if (Z)
                        {
                            // / 1 -> NEXT is already the answer
                            break; // exit
                        }
                        CMP #16
                        if (Z)
                        {
                            shiftNEXTright(); // NEXT >> 1
                            LDA #8            // Set up for next CMP
                        }
                        CMP #8
                        if (Z)
                        {
                            shiftNEXTright(); // NEXT >> 1
                            LDA #4            // Set up for next CMP
                        }
                        CMP #4
                        if (Z)
                        {
                            shiftNEXTright(); // NEXT >> 1
                            LDA #2            // Set up for next CMP
                        }
                        CMP #2
                        if (Z)
                        {
                            shiftNEXTright(); // NEXT >> 1 (final shift)
                            // NEXT now has quotient, RESULT stays 0 for remainder
                            break; // ÷2, ÷4, ÷8, ÷16 all exit here
                        }                  
                    }
                    
                    // Check if both dividend and divisor fit in 16 bits
                    LDA ZP.NEXT2
                    ORA ZP.NEXT3  
                    if (Z)  // All high bytes are zero - use 16-bit division
                    {
                        // Clear upper bytes and initialize remainder
                        STZ ZP.NEXT2
                        STZ ZP.NEXT3
                        
                        LDA ZP.TOP1
                        if (Z)
                        {
                            LDA ZP.TOP0
                            CMP # 10
                            if (Z)
                            {
                                utility16BitDiv10(); //  / 10
                                break; // 16 bit exit
                            }
                            CMP # 50
                            if (Z)
                            {
                                utility16BitDiv10(); //  / 10
                                ASL ZP.NEXT0         //  * 2
                                ROL ZP.NEXT1
                                utility16BitDiv10(); //  / 10
                                break; // 16 bit exit
                            }
                            CMP # 100
                            if (Z)
                            {
                                utility16BitDiv10(); //  / 10
                                utility16BitDiv10(); //  / 10
                                break; // 16 bit exit
                            }
                        }
                        
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
                    } // 16 bit numerator and denominator
                } // 16 bit denominator
            } // optimizations (bypassed for Mod)
            
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
        SEC
    }
    NegateTop()
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
    
    // APIs for Tokenizer.GetTokenNumber() 
    utilityLongMUL()
    {
        // ZP.RESULT = ZP.NEXT0..ZP.NEXT3 * ZP.TOP0..ZP.TOP3
        
        // https://llx.com/Neil/a2/mult.html
        // http://www.6502.org/source/integers/32muldiv.htm
        
        Shared.ZeroResult8();
        
        loop
        {
            // Check if NEXT has a special multiplier - swap if so
            LDA ZP.NEXT1
            ORA ZP.NEXT2
            ORA ZP.NEXT3
            if (Z)  // NEXT is 8-bit
            {

                LDA ZP.NEXT0
                switch (A)
                {
                    case 0:
                    case 1:
                    case 2:
                    case 4:
                    case 8:
                    case 16:
                    { 
                        Shared.SwapNextTop(); 
                    }
                    case 10:
                    {
                        // Check if TOP has a 'better' special multiplier (0,1, and multiples of 2 are better than 10)
                        LDA ZP.TOP1
                        ORA ZP.TOP2
                        ORA ZP.TOP3
                        if (Z)  // TOP is 8-bit
                        {
                            LDA ZP.TOP0
                            switch (A)
                            {
                                case  0:
                                case  1:
                                case  2:
                                case  4:
                                case  8:
                                case 16:
                                { } // keep TOP
                                default:
                                {
                                    Shared.SwapNextTop();
                                }
                            }
                        }
                        else
                        {
                            Shared.SwapNextTop(); 
                        }
                    }
                }
            }

            // TOP is now the number to compare to..
            LDA ZP.TOP2
            ORA ZP.TOP3
            if (Z)
            {
                // TOP is 16 bit
                LDA ZP.TOP1
                if (Z)
                {
                    // TOP is 0..255
                    LDA ZP.TOP0
                    CMP #0
                    if (Z)
                    {
                        // x 0  (RESULT is already zero)
                        break; // exit
                    }
                    CMP #1
                    if (Z)
                    {
                        // x 1 -> NEXT -> RESULT
                        Shared.MoveNextToResult();
                        break; // exit
                    }
                    CMP #16 
                    if (Z)
                    {
                        shiftNEXTleft(); // NEXT << 1, can set overflow error
                        LDA #8
                    }
                    CMP #8
                    if (Z)
                    {
                        shiftNEXTleft(); // NEXT << 1, can set overflow error
                        LDA #4
                    }
                    CMP #4
                    if (Z)
                    {
                        shiftNEXTleft(); // NEXT << 1, can set overflow error
                        LDA #2
                    }
                    CMP #2
                    if (Z)
                    {
                        shiftNEXTleft(); // NEXT << 1, can set overflow error
                        CheckError();
                        Shared.MoveNextToResult();
                        break; // x2, x4, x8, x16 exit 
                    }
                    CMP #10
                    if (Z)
                    {
                        // TODO : (NEXT << 3) + (NEXT << 1)
                    }
                }
                
                // Check if both operands fit in 16 bits
                LDA ZP.NEXT2
                ORA ZP.NEXT3
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
                                LDA ZP.RESULT1   // get upper half of product
                                CLC               
                                ADC ZP.TOP0      // add multiplicand to it
                                STA ZP.RESULT1   // (carry may be set from this add)
                            }
                            ROR ZP.RESULT1       // rotate partial product right
                            ROR ZP.RESULT0       // through both bytes
                            DEX                  // decrement bit count
                            if (Z) { break; }    // exit loop when 8 bits are done
                        }
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
                } // both 16 bit
                
            } // TOP 16 bitLDA
            LDA # 0
            LDX # 32
            loop
            {
                // shift multiplyer right
                shiftNEXTright();
                //LSR ZP.NEXT3   
                //ROR ZP.NEXT2
                //ROR ZP.NEXT1
                //ROR ZP.NEXT0
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
        // A + TOP --> TOP
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
    
    // Parse decimal string to 32-bit value
    // Input: ZP.STR = pointer to decimal string, Y = starting offset
    // Output: ZP.TOP0-3 = parsed value, Y = position after last digit
    //         C set on success, NC on overflow
    // Munts: ZP.TOP0-3, ZP.NEXT0-3, ZP.RESULT0-7, A, Y
    FromDecimal()
    {
        Long.ZeroTop();
#ifdef DEBUG
        // just to make the debug output prettier:
        LDA #BASICType.LONG
        STA ZP.TOPT
        STA ZP.NEXTT
#endif
        loop
        {
            LDA [ZP.STR], Y
            
            if (Z) { break; }  // Null terminator
            
            Char.IsDigit();
            if (NC) { break; }  // Not a digit
            
            // Convert ASCII to digit
            LDA [ZP.STR], Y
            SEC
            SBC #'0'
            PHA  // Save digit
            
            // Multiply TOP by 10
            LDA #10
            STA ZP.NEXT0
            ZeroNext3();
            
            PHY
            utilityLongMUL();  // RESULT = NEXT * TOP, munts Y
            PLY
            
            // Check for overflow
            LDA ZP.RESULT4
            ORA ZP.RESULT5
            ORA ZP.RESULT6
            ORA ZP.RESULT7
            if (NZ)  // Overflow
            {
                PLA  // Clean up stack
                CLC
                return;
            }
            
            // Move result back to TOP
            moveResultToTop(); // preserves Y
            
            // Add the digit
            PLA
            Long.AddDigit(); // preserves Y
            if (NC)  // Overflow
            {
                CLC
                return;
            }
            INY
        } // loop
        SEC  // Success
    }

    // Parse hex string to 32-bit value  
    // Input: ZP.STR = pointer to hex string (after "0x"), Y = starting offset
    // Output: ZP.TOP0-3 = parsed value, Y = position after last hex digit
    //         C set on success, NC on error
    // Munts: ZP.TOP0-3, A, Y
    FromHex()
    {
        Long.ZeroTop();
        
        loop
        {
            LDA [ZP.STR], Y
            if (Z) { break; }  // Null terminator
            
            Char.IsHex();
            if (NC) { break; }  // Not hex digit
            
            // Shift TOP left 4 bits (multiply by 16)
            LDX #4
            loop
            {
                ASL ZP.TOP0
                ROL ZP.TOP1
                ROL ZP.TOP2
                ROL ZP.TOP3
                if (C)  // Overflow
                {
                    CLC
                    return;
                }
                DEX
                if (Z) { break; }
            }
            
            // Convert hex char to value
            LDA [ZP.STR], Y
            Char.IsDigit();
            if (C)
            {
                SEC
                SBC #'0'
            }
            else
            {
                Char.IsLower();
                if (C)
                {
                    SEC
                    SBC #('a' - 10)
                }
                else
                {
                    SEC
                    SBC #('A' - 10)
                }
            }
            
            // Add to TOP
            ORA ZP.TOP0
            STA ZP.TOP0
            
            INY
        }
        SEC  // Success
    }
    
    // Fast 8-bit divide by 10 with remainder
    // Input:  ZP.NEXT0 = 8-bit value (0-255)
    // Output: ZP.NEXT0 = quotient (0-25)
    //         ZP.RESULT0 = remainder (0-9)
    // Munts: A, Y
    utility8BitDivMod10()
    {
        LDA ZP.NEXT0
        LDY #0              // Initialize quotient
        
        // Handle >= 100
        CMP #100
        if (C)              // >= 100
        {
            SEC
            SBC #100
            LDY #10         // Add 10 to quotient
            
            CMP #100        // Check for >= 200
            if (C)          
            {
                SEC
                SBC #100
                LDY #20     // Set quotient to 20
            }
        }
        
        // Now handle remainder 0-99 with a simple loop
        loop
        {
            CMP #10
            if (NC) { break; }  // < 10, we're done
            
            SEC
            SBC #10
            INY
        }
        
        STA ZP.RESULT0      // Store remainder (0-9)
        STY ZP.NEXT0        // Store quotient
    }    
    
    // Print 32-bit LONG decimal number
    // Input: ZP.TOP0-3 = 32-bit number to print
    // Output: Decimal number printed to serial
    // Munts: ZP.NEXT, ZP.RESULT, ZP.ACC, A
    Print()
    {
        PHX
        PHY
        
        LDA ZP.NEXT3
        PHA
        LDA ZP.NEXT2
        PHA
        LDA ZP.NEXT1
        PHA
        LDA ZP.NEXT0
        PHA
        
        loop
        {
            Shared.MoveTopToNext();
#ifdef CPU_65C02S
            if (BBS7, ZP.NEXT3)
            {
                LDA #'-'
                Serial.WriteChar();
                NegateNext();
            }
#else
            LDA ZP.NEXT3
            if (MI)
            {
                LDA #'-'
                Serial.WriteChar();
                NegateNext();
            }
#endif            
            
            // Check for zero special case
            Shared.ZeroCheckNext();
            if (Z)
            {
                LDA #'0'
                Serial.WriteChar();
                break;
            }
            
            // Extract digits by repeated division by 10
            LDY #0  // Digit counter (also stack depth)
            loop
            {
                // Setup for DivMod: NEXT = value, TOP = 10
                PHY
                LDA ZP.NEXT1
                ORA ZP.NEXT2
                ORA ZP.NEXT3
                if (Z)
                {
                    // 8 bit
                    // ZP.NEXT0 = ZP.NEXT0 / 10, remainder in ZP.RESULT0
                    utility8BitDivMod10();
                }
                else
                {
                    // 32 bit
                    LDA #10
                    LoadTopByte();
                
                
                    // NEXT = NEXT / TOP, remainder in RESULT
                    LDX #1          // X=1 for mod (ensures we get remainder)
                    DivMod();
                }
                
                PLY
                
                // Push digit (remainder) onto stack
                LDA ZP.RESULT0  // Remainder is 0-9
                ORA #'0'        // Convert to ASCII
                PHA
                INY             // Count digits
                
                // Check if quotient is zero
                Shared.ZeroCheckNext();
                if (Z) { break; }  // Done extracting digits
            }
            
            // Pop and print digits (they're in reverse order on stack)
            loop
            {
                PLA
                Serial.WriteChar();
                DEY
                if (Z) { break; }
            }
            SEC
            break;
        } // single exit
        
        PLA
        STA ZP.NEXT0
        PLA
        STA ZP.NEXT1
        PLA
        STA ZP.NEXT2
        PLA
        STA ZP.NEXT3
        
        PLY
        PLX
    }
}
