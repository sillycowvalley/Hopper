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



}