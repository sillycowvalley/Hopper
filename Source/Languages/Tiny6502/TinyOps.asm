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
        if (C) // TOP <= NEXT ?
        {
            LDX # 0 // TOP <= NEXT
        }
        // result in X
    }

    CompareGT()
    {
        // arguments in NEXT and TOP
        LDX # 1 // NEXT > TOP
        LDA ZP.TOPH
        CMP ZP.NEXTH
        if (Z)
        {
            LDA ZP.TOPL
            CMP ZP.NEXTL
        }
        if (C) // TOP > NEXT ?
        {
            LDX # 0 // TOP > NEXT
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
        // arguments in NEXT and TOP
        
        // + 16-bit

        CLC // Clear Carry Flag
        LDA ZP.NEXTL
        ADC ZP.TOPL
        STA ZP.TOPL

        LDA ZP.NEXTH
        ADC ZP.TOPH
        STA ZP.TOPH
        
        // result in TOP
    }
    
    Sub16()
    {
        // arguments in NEXT and TOP
        
        // - 16-bit

        SEC // Set Carry Flag for Subtraction
        LDA ZP.NEXTL
        SBC ZP.TOPL
        STA ZP.TOPL

        LDA ZP.NEXTH
        SBC ZP.TOPH
        STA ZP.TOPH
        
        // result in TOP
    }
    
    Mul16()
    {
        // arguments in NEXT and TOP
        
        MulShared();
        
        // result in TOP
    }
    
    Mul8()
    {
        // arguments in NEXT and TOP
        
        STZ TOPH
        STZ NEXTH
        MulShared();
        
        // result in TOP
    }
    
    Div16()
    {
        // arguments in NEXT and TOP
        
        UtilityDiv();
        LDA NEXTL
        STA TOPL
        LDA NEXTH
        STA TOPH
        
        // result in TOP
    }
    
    Div8()
    {
        // arguments in NEXT and TOP
        
        STZ TOPH
        STZ NEXTH
        UtilityDiv();
        LDA NEXTL
        STA TOPL
        
        // result in TOP
    }
    
    Mod16()
    {
        // arguments in NEXT and TOP
        
        UtilityDiv();
        LDA ACCL
        STA TOPL
        LDA ACCH
        STA TOPH
        
        // result in TOP
    }
    
    Mod8()
    {
        // arguments in NEXT and TOP
        
        STZ TOPH
        STZ NEXTH
        UtilityDiv();
        LDA ACCL
        STA TOPL
        
        // result in TOP
    }
    
    CompareLTI()
    {
        // arguments in NEXT and TOP
        
        // NEXT < TOP?
        // TOP - NEXT > 0
        SEC
        LDA ZP.TOPL
        SBC ZP.NEXTL
        STA ZP.TOPL
        LDA ZP.TOPH
        SBC ZP.NEXTH
        STA ZP.TOPH
        
        ASL           // sign bit into carry
        
        LDX #0  // TOP <= 0
        loop
        {
            if (C) { break; }
            //  0 or positive
            LDA ZP.TOPL
            if (Z)
            {
                LDA ZP.TOPH
                if (Z) { break; }
            }
            LDX #1
            break;
        }
        
        // result in X
    }

    CompareLEI()
    {
        // arguments in NEXT and TOP
        
        // NEXT <= TOP?
        // TOP - NEXT >= 0
        
        SEC
        LDA ZP.TOPL
        SBC ZP.NEXTL
        STA ZP.TOPL
        LDA ZP.TOPH
        SBC ZP.NEXTH
        STA ZP.TOPH
        
        ASL           // sign bit into carry
        
        LDX #0
        if (NC)
        {
            // 0 or positive
            LDX #1
        }
        
        // result in X
    }
    
    CompareGTI()
    {
        // arguments in NEXT and TOP
        
        // NEXT > TOP?
        // NEXT - TOP > 0
        
        SEC
        LDA ZP.NEXTL
        SBC ZP.TOPL
        STA ZP.TOPL
        LDA ZP.NEXTH
        SBC ZP.TOPH
        STA ZP.TOPH
        
        ASL           // sign bit into carry
        
        LDX #0  // TOP <= 0
        loop
        {
            if (C) { break; }
            //  0 or positive
            LDA ZP.TOPL
            if (Z)
            {
                LDA ZP.TOPH
                if (Z) { break; }
            }
            LDX #1
            break;
        }
        
        // result in X
    }

    CompareGEI()
    {
        // arguments in NEXT and TOP
        
        // NEXT >= TOP?
        // NEXT - TOP >= 0
        
        SEC
        LDA ZP.NEXTL
        SBC ZP.TOPL
        STA ZP.TOPL
        LDA ZP.NEXTH
        SBC ZP.TOPH
        STA ZP.TOPH
        
        ASL           // sign bit into carry
        
        LDX #0
        if (NC)
        {
            // 0 or positive
            LDX #1
        }
        
        // result in X
    }
    
    CompareLE()
    {
        // arguments in NEXT and TOP
        LDX #1 // NEXT <= TOP
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
        }
        if (NZ) // NEXT == TOP (not >)?
        {
            if (C) // NEXT <  TOP (not >)?
            {
                LDX #0  // NEXT > TOP
            }
        }
        // result in X
    }

    CompareGE()
    {
        // arguments in NEXT and TOP
        LDX #0 // NEXT < TOP
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
        }
        if (C) // NEXT < TOP?
        {
            LDX #1   
        }
        // result in X
    }
}