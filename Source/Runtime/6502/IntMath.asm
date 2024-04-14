unit IntMath
{
    // FASTINTS
    
    swapNEXTandTOP()
    {
        LDA NEXTL
        PHA
        LDA NEXTH
        PHA
        LDA TOPL
        STA NEXTL
        LDA TOPH
        STA NEXTH
        PLA
        STA TOPH
        PLA
        STA TOPL 
    }
    mulShared()
    {
        // TOP = NEXT * TOP
        
#ifdef FASTINTS

        LDA TOPH
        if (Z)
        {
            loop
            {
                LDA NEXTL
                if (Z)
                {
                    LDA #0
                    STA TOPL
                    STA TOPH
                    return;
                }
                CMP #1
                if (Z)
                {
                    swapNEXTandTOP();
                    break;
                }
                CMP #2
                if (Z)
                {
                    swapNEXTandTOP();
                    break;
                }
                CMP #4
                if (Z)
                {
                    swapNEXTandTOP();
                    break;
                }
                CMP #8
                if (Z)
                {
                    swapNEXTandTOP();
                    break;
                }
                CMP #16
                if (Z)
                {
                    swapNEXTandTOP();
                    break;
                }
                break;
            }
        }
        LDA NEXTH
        if (Z)
        {
            LDA NEXTL
            if (Z)
            {
                LDA #0
                STA TOPL
                STA TOPH
                return;
            }
            CMP #1
            if (Z)
            {
                return; // just return TOP
            }
            CMP #2
            if (Z)
            {
                ASL TOPL
                ROL TOPH
                return;
            }
            CMP #4
            if (Z)
            {
                ASL TOPL
                ROL TOPH
                ASL TOPL
                ROL TOPH
                return;
            }
            CMP #8
            if (Z)
            {
                ASL TOPL
                ROL TOPH
                ASL TOPL
                ROL TOPH
                ASL TOPL
                ROL TOPH
                return;
            }
            CMP #16
            if (Z)
            {
                ASL TOPL
                ROL TOPH
                ASL TOPL
                ROL TOPH
                ASL TOPL
                ROL TOPH
                ASL TOPL
                ROL TOPH
                return;
            }
        }
               
#endif        
        // https://llx.com/Neil/a2/mult.html
        // Initialize RESULT to 0
        LDA # 0
        STA ZP.UWIDE2
        LDX #16      // there are 16 bits in TOP
        loop
        {
            LSR ZP.TOPH  // get low bit of TOP
            ROR ZP.TOPL
            if (C)       // 0 or 1?
            {
                TAY      // if 1, add NUM1 (hi byte of RESULT is in A)
                CLC
                LDA ZP.NEXTL
                ADC ZP.UWIDE2
                STA ZP.UWIDE2
                TYA
                ADC ZP.NEXTH
            }
            ROR A        // "Stairstep" shift
            ROR ZP.UWIDE2
            ROR ZP.UWIDE1
            ROR ZP.UWIDE0
            DEX
            if (Z) { break; }
        }
        STA ZP.UWIDE3
        
        LDA ZP.UWIDE0
        STA ZP.TOPL
        LDA ZP.UWIDE1
        STA ZP.TOPH
        LDA #Types.UInt
        STA ZP.TOPT
    }
    
    const byte[] modRemaining  = { 0x00, 0x06, 0x02, 0x08, 0x04, 0x00, 0x06, 0x02, 0x08, 0x04 };
    const byte[] tensRemaining = { 0,25,51,76,102,128,153,179,204,230 };
    
    
    utilityDiv10()
    {
        // NEXT = NEXT / 10
        LDA NEXTL
        STA TOPL
        LDA NEXTH
        STA TOPH
        
        //    UNSIGNED DIVIDE BY 10 (16 BIT)
        //    111 cycles (max), 96 bytes
        //    https://forums.atariage.com/blogs/entry/11044-16-bit-division-fast-divide-by-10/
        loop
        {
            LDA    TOPH
            STA    ACCL
            LSR        
            ADC    #13 
            ADC    ACCL
            ROR        
            LSR        
            LSR        
            ADC    ACCL
            ROR        
            ADC    ACCL
            ROR        
            LSR        
            AND    # 0x7C                // AND'ing here...
            STA    ACCL                  // and saving result as highTen (times 4)
            LSR        
            LSR        
            STA    NEXTH
            ADC    ACCL                  // highTen (times 5)
            ASL                          // highTen (times 10)
            SBC    TOPH
            EOR    # 0xFF
            TAY                          // mod 10 result!
            LDA    tensRemaining, Y      // Fill the low byte with the tens it should
            STA    NEXTL                 // have at this point from the high byte divide.
            LDA    TOPL
            ADC    modRemaining, Y       // 4  @69
            
            if (NC) 
            { 
                STA    ACCL
                LSR        
                ADC    # 13 
                ADC    ACCL
                ROR        
                LSR        
                LSR        
                ADC    ACCL
                ROR        
                ADC    ACCL
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
        ADC    NEXTL
        STA    NEXTL
    }
    
    utilityDiv()
    {
        // NEXT = NEXT (dividend=result) / TOP (divisor)
#ifdef FASTINTS        
        LDA TOPH
        if (Z) // MSB zero?
        {
            LDA TOPL
            CMP # 1
            if (Z)
            {
                return; //     / 1 -> answer is already in NEXT
            }
            CMP # 2
            if (Z)
            {
                LSR NEXTH //   / 2
                ROR NEXTL
                return;
            }
            CMP # 4
            if (Z)
            {
                LSR NEXTH //   / 2
                ROR NEXTL
                LSR NEXTH //   / 2
                ROR NEXTL
                return;
            }
            CMP # 10
            if (Z)
            {
                utilityDiv10(); //  / 10
                return;
            }
            CMP # 100
            if (Z)
            {
                utilityDiv10(); //  / 10
                utilityDiv10(); //  / 10
                return;
            }
            CMP # 50
            if (Z)
            {
                utilityDiv10(); //  / 10
                ASL NEXTL       //  * 2
                ROL NEXTH
                utilityDiv10(); //  / 10
                return;
            }
        }
#endif        
        divmod();
    }
    divmod()
    {
        // NEXT = NEXT (dividend=result) / TOP (divisor)
        // ACC (remainder)
               
        // https://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
        // https://llx.com/Neil/a2/mult.html
        LDA #0
        STA ZP.ACCL
        STA ZP.ACCH
        LDX #16
        
        loop
        {
            ASL ZP.NEXTL
            ROL ZP.NEXTH
            ROL ZP.ACCL
            ROL ZP.ACCH
            LDA ZP.ACCL
            SEC
            SBC ZP.TOPL
            TAY
            LDA ZP.ACCH
            SBC ZP.TOPH
            if (C)
            {
                STA ZP.ACCH
                STY ZP.ACCL
                INC ZP.NEXTL
            }
            DEX
            if (Z) { break; }
        }
    }
    negateTop()
    {
        // TOP = 0 - TOP
        SEC
        LDA #0
        SBC ZP.TOPL
        STA ZP.TOPL
        LDA #0
        SBC ZP.TOPH
        STA ZP.TOPH        
    }
    negateNext()
    {
        // NEXT = 0 - NEXT
        SEC
        LDA #0
        SBC ZP.NEXTL
        STA ZP.NEXTL
        LDA #0
        SBC ZP.NEXTH
        STA ZP.NEXTH
    }
    
    doSigns() // munts X
    {   
        LDX #0 
        LDA ZP.NEXTH
        ASL // sign bit into carry
        if (C)
        {
            INX // count the -ve
            negateNext(); // NEXT = -NEXT
        }
        LDA ZP.TOPH
        ASL // sign bit into carry
        if (C)
        {
            INX // count the -ve
            negateTop(); // TOP = -TOP
        }
        STX ZP.FSIGN // store the sign count
    }
    
    Mul()
    {
        // TODO : there is a lot of fast multiply code in v0 (incluing 8x8)
        Stacks.PopTop();
        Stacks.PopNext();
        mulShared();
        Stacks.PushTop();
    }
    MulI()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        doSigns(); // munts X
        mulShared();
        LDA ZP.FSIGN     // load the sign count
        CMP #1
        if (Z)           // 0 or 2negatives
        {
            negateTop(); // TOP = -TOP
        }
        LDA #Types.Int
        STA ZP.TOPT
        PushTop();
    }
    Div()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        // NEXT = NEXT / TOP
        utilityDiv();
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    Mod()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        // ACC = NEXT % TOP
        divmod();
        LDA #Types.UInt
        STA ZP.ACCT
        Stacks.PushACC();
    }
    DivI()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        doSigns(); // munts X
        utilityDiv();
        
        LDA ZP.FSIGN     // load the sign count
        CMP #1
        if (Z)            // 0 or 2negatives
        {
            negateNext(); // NEXT = -NEXT
        }
        LDA #Types.Int
        STA ZP.NEXTT
        PushNext();
    }
    ModI()
    {
        // supporting floored division remainder is always positive
        //
        //   dividend = divisor * quotient + remainder
        //    10 /  3 = q  3, r  1
        //   -10 / -3 = q  3, r -1
        //   -10 /  3 = q -3, r -1
        //    10 / -3 = q -3, r -11 ?!
        
        Stacks.PopTop();
        Stacks.PopNext();
        doSigns();
        divmod();
    
        // always leave remainder ACC as positive
        LDA #Types.Int
        STA ZP.ACCT
        PushACC();
    }
}
