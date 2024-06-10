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
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// + 16-bit", 0);

        PadOut("CLC // Clear Carry Flag", 0);
        PadOut("LDA ZP.NEXTL", 0);
        PadOut("ADC ZP.TOPL", 0);
        PadOut("STA ZP.TOPL", 0);

        PadOut("LDA ZP.NEXTH", 0);
        PadOut("ADC ZP.TOPH", 0);
        PadOut("STA ZP.TOPH", 0);
    }
    
    Sub16()
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// - 16-bit", 0);

        PadOut("SEC // Set Carry Flag for Subtraction", 0);
        PadOut("LDA ZP.NEXTL", 0);
        PadOut("SBC ZP.TOPL", 0);
        PadOut("STA ZP.TOPL", 0);

        PadOut("LDA ZP.NEXTH", 0);
        PadOut("SBC ZP.TOPH", 0);
        PadOut("STA ZP.TOPH", 0);
    }





}