unit IntMath
{
    // FASTINTS
    
    MulShared()
    {
        // TOP = NEXT * TOP
        
        LDA #Types.UInt
        STA ZP.TOPT
        
#ifdef FASTINTS
        LDA ZP.TOPH
        if (Z)
        {
            LDA ZP.TOPL
            if (Z)
            {
                // TOP is zero
                return;
            }
            CMP #8
            if (Z)
            {
                ASL ZP.NEXTL
                ROL ZP.NEXTH
                LSR
            }
            CMP #4
            if (Z)
            {
                ASL ZP.NEXTL
                ROL ZP.NEXTH
                LSR
            }
            CMP #2
            if (Z)
            {
                ASL ZP.NEXTL
                ROL ZP.NEXTH
                LSR
            }
            CMP #1
            if (Z)
            {
                LDA ZP.NEXTL
                STA ZP.TOPL
                LDA ZP.NEXTH
                STA ZP.TOPH
                return; // return TOP
            }
        }
        LDA ZP.NEXTH
        if (Z)
        {
            LDA ZP.NEXTL
            if (Z)
            {
                // NEXT is zero
#ifdef CPU_65C02S
                STZ ZP.TOPL
                STZ ZP.TOPH
#else                
                LDA #0
                STA ZP.TOPL
                STA ZP.TOPH
#endif
                return;
            }
            CMP #8
            if (Z)
            {
                ASL ZP.TOPL
                ROL ZP.TOPH
                LSR
            }
            CMP #4
            if (Z)
            {
                ASL ZP.TOPL
                ROL ZP.TOPH
                LSR
            }
            CMP #2
            if (Z)
            {
                ASL ZP.TOPL
                ROL ZP.TOPH
                LSR
            }
            CMP #1
            if (Z)
            {
                return; // just return TOP
            }
                    
            LDA ZP.TOPH // 8x8 since TOPH and NEXTH are zero
            if (Z)
            {
                // https://codebase64.org/doku.php?id=base:8bit_multiplication_16bit_product_fast_no_tables
                LDX ZP.TOPL
                DEX          // decrement TOPL because we will be adding with carry set for speed (an extra one)
                STX ZP.TOPL
                ROR ZP.NEXTL
                if (C)
                {
                    ADC ZP.TOPL
                }
                ROR
                ROR ZP.NEXTL
                if (C)
                {
                    ADC ZP.TOPL
                }
                ROR
                ROR ZP.NEXTL
                if (C)
                {
                    ADC ZP.TOPL
                }
                ROR
                ROR ZP.NEXTL
                if (C)
                {
                    ADC ZP.TOPL
                }
                ROR
                ROR ZP.NEXTL
                if (C)
                {
                    ADC ZP.TOPL
                }
                ROR
                ROR ZP.NEXTL
                if (C)
                {
                    ADC ZP.TOPL
                }
                ROR
                ROR ZP.NEXTL
                if (C)
                {
                    ADC ZP.TOPL
                }
                ROR
                ROR ZP.NEXTL
                if (C)
                {
                    ADC ZP.TOPL
                }
                ROR
                ROR ZP.NEXTL
                STA ZP.TOPH
                LDA ZP.NEXTL
                STA ZP.TOPL                
                return;
            }
        }
#endif        
        // https://llx.com/Neil/a2/mult.html
        // Initialize RESULT to 0
        LDA # 0
        STA ZP.UWIDE2
        LDX # 16      // there are 16 bits in TOP
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
    }
    
    const byte[] modRemaining  = { 0x00, 0x06, 0x02, 0x08, 0x04, 0x00, 0x06, 0x02, 0x08, 0x04 };
    const byte[] tensRemaining = { 0,25,51,76,102,128,153,179,204,230 };
    
    
    utilityDiv10()
    {
        // NEXT = NEXT / 10
        LDA ZP.NEXTL
        STA ZP.TOPL
        LDA ZP.NEXTH
        STA ZP.TOPH
        
        //    UNSIGNED DIVIDE BY 10 (16 BIT)
        //    111 cycles (max), 96 bytes
        //    https://forums.atariage.com/blogs/entry/11044-16-bit-division-fast-divide-by-10/
        loop
        {
            LDA    ZP.TOPH
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
            STA    ZP.NEXTH
            ADC    ZP.ACCL                  // highTen (times 5)
            ASL                          // highTen (times 10)
            SBC    ZP.TOPH
            EOR    # 0xFF
            TAY                          // mod 10 result!
            LDA    tensRemaining, Y      // Fill the low byte with the tens it should
            STA    ZP.NEXTL                 // have at this point from the high byte divide.
            LDA    ZP.TOPL
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
        ADC    ZP.NEXTL
        STA    ZP.NEXTL
    }
    
    UtilityDiv()
    {
        // NEXT = NEXT (dividend=result) / TOP (divisor)
#ifdef FASTINTS        
        LDA ZP.TOPH
        if (Z) // MSB zero?
        {
            LDA ZP.TOPL
            CMP # 1
            if (Z)
            {
                return; //     / 1 -> answer is already in NEXT
            }
            CMP # 2
            if (Z)
            {
                LSR ZP.NEXTH //   / 2
                ROR ZP.NEXTL
                return;
            }
            CMP # 4
            if (Z)
            {
                LSR ZP.NEXTH //   / 2
                ROR ZP.NEXTL
                LSR ZP.NEXTH //   / 2
                ROR ZP.NEXTL
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
                ASL ZP.NEXTL       //  * 2
                ROL ZP.NEXTH
                utilityDiv10(); //  / 10
                return;
            }
        }
#endif        
        DivMod();
    }
    DivMod()
    {
        // NEXT = NEXT (dividend=result) / TOP (divisor)
        // ACC (remainder)
               
        // https://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
        // https://llx.com/Neil/a2/mult.html
        LDA # 0
        STA ZP.ACCL
        STA ZP.ACCH
        LDX # 16
        
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
    NegateTop()
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
    NegateNext()
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
    
    popTopNextandDoSigns() // munts X
    {   
        Stacks.PopTopNext();
        
        LDX #0 
        LDA ZP.NEXTH
        ASL // sign bit into carry
        if (C)
        {
            INX // count the -ve
            NegateNext(); // NEXT = -NEXT
        }
        LDA ZP.TOPH
        ASL // sign bit into carry
        if (C)
        {
            INX // count the -ve
            NegateTop(); // TOP = -TOP
        }
        STX ZP.FSIGN // store the sign count
    }
    
    Mul()
    {
        Stacks.PopTopNext();
        MulShared();
        LDA ZP.TOPT
        Stacks.PushTop();
    }
    MulI()
    {
        popTopNextandDoSigns(); // munts X
        MulShared();
        LDA ZP.FSIGN     // load the sign count
        CMP #1
        if (Z)           // 1 negative (not 0 or 2)
        {
            NegateTop(); // TOP = -TOP
        }
        LDA #Types.Int
        Stacks.PushTop();
    }
    Div()
    {
        Stacks.PopTopNext();
        // NEXT = NEXT / TOP
        UtilityDiv();
        LDA #Types.UInt
        Stacks.PushNext();
    }
    Mod()
    {
        Stacks.PopTopNext();
        // ACC = NEXT % TOP
        DivMod();
        LDA #Types.UInt
        STA ZP.ACCT
        Stacks.PushACC(); // munts Y, A
    }
    DivI()
    {
        popTopNextandDoSigns(); // munts X
        UtilityDiv();
        
        LDA ZP.FSIGN     // load the sign count
        CMP #1
        if (Z)           // 1 negative (not 0 or 2)
        {
            NegateNext(); // NEXT = -NEXT
        }
        LDA #Types.Int
        Stacks.PushNext();
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
        
        popTopNextandDoSigns(); // munts X
        DivMod();
    
        // always leave remainder ACC as positive
        LDA #Types.Int
        STA ZP.ACCT
        Stacks.PushACC();  // munts Y, A
    }
    
    UIntToInt()
    {
        Stacks.PopTop();
#ifdef CHECKED
        BIT TOPH
        if (MI) // value > 32767
        {
            LDA # 0x0D // Numeric type out of range/overflow.
            Diagnostics.die();
        }
#endif   
        LDA # Types.Int     
        Stacks.PushTop();
    }
}
