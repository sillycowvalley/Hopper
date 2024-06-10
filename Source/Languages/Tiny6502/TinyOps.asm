unit TinyOps
{
    CompareLT()
    {
        // arguments in NEXT and TOP
        LDX # 1 // NEXT < TOP
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
        }
        if (C) // NEXT < TOP
        {
            LDX # 0 // NEXT >= TOP
        }
        // result in X
    }
    CompareLE()
    {
        // arguments in NEXT and TOP
        LDX # 1 // NEXT <= TOP
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
        }
        if (NC) // NEXT <= TOP
        {
            LDX # 0 // NEXT > TOP
        }
        // result in X
    }

    CompareGT()
    {
        // arguments in NEXT and TOP
        LDX # 0 // NEXT <= TOP
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
        }
        if (NC) // NEXT > TOP
        {
            LDX # 1 // NEXT > TOP
        }
        // result in X
    }

    CompareGE()
    {
        // arguments in NEXT and TOP
        LDX # 0 // NEXT < TOP
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
        }
        if (C) // NEXT >= TOP
        {
            LDX # 1 // NEXT >= TOP
        }
        // result in X
    }
    
    CompareEQ()
    {
        // arguments in NEXT and TOP
        LDX # 0 // NEXT != TOP
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
            if (Z)
            {
                LDX # 1 // NEXT == TOP
            }
        }
        // result in X
    }
    CompareNE()
    {
        // arguments in NEXT and TOP
        LDX # 1 // NEXT != TOP
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
            if (Z)
            {
                LDX # 0 // NEXT == TOP
            }
        }
        // result in X
    }
    
    Add16()
    {
        // + 16-bit

        CLC // Clear Carry Flag
        LDA ZP.NEXTL
        ADC ZP.TOPL
        STA ZP.TOPL

        LDA ZP.NEXTH
        ADC ZP.TOPH
        STA ZP.TOPH
    }
    
    Sub16()
    {
        // - 16-bit

        SEC // Set Carry Flag for Subtraction
        LDA ZP.NEXTL
        SBC ZP.TOPL
        STA ZP.TOPL

        LDA ZP.NEXTH
        SBC ZP.TOPH
        STA ZP.TOPH
    }
    
    Mul16()
    {
        MulShared();
    }
    
    Mul8()
    {
        STZ TOPH
        STZ TOPL
        MulShared();
    }
    
    Div16()
    {
        UtilityDiv();
        LDA NEXTL
        STA TOPL
        LDA NEXTH
        STA TOPH
    }
    
    Div8()
    {
        STZ TOPH
        STZ TOPL
        UtilityDiv();
        LDA NEXTL
        STA TOPL
    }
    
    Mod16()
    {
        UtilityDiv();
        LDA ACCL
        STA TOPL
        LDA ACCH
        STA TOPH
    }
    
    Mod8()
    {
        STZ TOPH
        STZ TOPL
        UtilityDiv();
        LDA ACCL
        STA TOPL
    }
}