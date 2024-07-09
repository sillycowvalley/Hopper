unit Float
{
    uses "/Source/Runtime/6502/ZeroPage"
    
    New()
    {
        // IEEE +0.0
        LDA # 0
        STA ZP.LNEXT0
        STA ZP.LNEXT1
        STA ZP.LNEXT2
        STA ZP.LNEXT3
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
#ifdef FAST_6502_RUNTIME
    Add()
    {
        loop
        {
            Long.commonLongNEXTTOP();
            Float.isZeroTOP();
            FSIGN
            CPX #1
            if (Z)
            {
                break;
            }
            Float.isZeroNEXT();
            CPX #1
            if (Z)
            {
                Long.commonSwapNEXTTOP();
                break;
            }
         
            break;   
        }
        LDA # Types.Long
        Long.pushNewFromL(); 
    }
#endif    
}
