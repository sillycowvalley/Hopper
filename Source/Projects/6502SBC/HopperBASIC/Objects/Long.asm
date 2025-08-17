unit Long
{
    friend Time, IntMath;
    
    const uint siData = 2;
    
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
