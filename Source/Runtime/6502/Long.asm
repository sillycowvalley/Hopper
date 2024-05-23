unit Long
{
    uses "/Source/Runtime/6502/ZeroPage"
    
    friend Time, Float;
    
    const uint siData = 2;
       
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
        
        LDY # siData
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
        LDA # 0
        STA ZP.LNEXT0
        STA ZP.LNEXT1
        STA ZP.LNEXT2
        STA ZP.LNEXT3
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
    
    DivMod()
    {
        // LNEXT = LNEXT / LTOP + LRESULT
        
        // #### https://llx.com/Neil/a2/mult.html ####
        // http://www.6502.org/source/integers/32muldiv.htm
        
        // Initialize remainder to 0
#ifdef CPU_65C02S
        STZ LRESULT0
        STZ LRESULT1
        STZ LRESULT2
        STZ LRESULT3
#else        
        LDA # 0
        STA LRESULT0
        STA LRESULT1
        STA LRESULT2
        STA LRESULT3
#endif
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
}
