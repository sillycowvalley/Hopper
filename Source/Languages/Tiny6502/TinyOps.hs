unit TinyOps
{
    uses "TinyCode"
    
    PopTopNext(bool isByte)
    {
        if (!isByte)
        {
            PadOut("PLA", 0);
            PadOut("STA ZP.TOPH", 0);
        }
        PadOut("PLA", 0);
        PadOut("STA ZP.TOPL", 0);
        if (!isByte)
        {
            PadOut("PLA", 0);
            PadOut("STA ZP.NEXTH", 0);
            
        }
        PadOut("PLA", 0);
        PadOut("STA ZP.NEXTL", 0);
    }
    PushTop(bool isByte)
    {
        PadOut("LDA ZP.TOPL", 0);
        PadOut("PHA", 0);
        if (!isByte)
        {
            PadOut("LDA ZP.TOPH", 0);
            PadOut("PHA", 0);
        }
    }
    CompareLT(bool isByte)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// <" + Bitness(isByte), 0); 
            
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TinyOps.CompareLT();", 0);
        }
        else
        {
            PadOut("LDX # 1 // NEXT < TOP", 0); 
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("CMP ZP.TOPL", 0);
            PadOut("if (C) // NEXT < TOP", 0);
            PadOut("{", 0);
            PadOut("LDX # 0 // NEXT >= TOP", 1); 
            PadOut("}", 0);
        }
        PadOut("PHX", 0);
    }
    CompareLE(bool isByte)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// <=" + Bitness(isByte), 0); 
        
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TinyOps.CompareLE();", 0);
        }
        else
        {
            PadOut("LDX # 1 // NEXT <= TOP", 0); 
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("CMP ZP.TOPL", 0);
            PadOut("if (NC) // NEXT <= TOP", 0);
            PadOut("{", 0);
            PadOut("LDX # 0 // NEXT > TOP", 1); 
            PadOut("}", 0);
        }
        PadOut("PHX", 0);
    }
    CompareGT(bool isByte)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// >" + Bitness(isByte), 0); 
        
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TinyOps.CompareGT();", 0);
        }
        else
        {
            PadOut("LDX # 0 // NEXT <= TOP", 0); 
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("CMP ZP.TOPL", 0);
            PadOut("if (NC) // NEXT > TOP", 0);
            PadOut("{", 0);
            PadOut("LDX # 1 // NEXT > TOP", 1); 
            PadOut("}", 0);
        }
        PadOut("PHX", 0);
    }
    
    CompareGE(bool isByte)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// >=" + Bitness(isByte), 0); 
        
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TinyOps.CompareGE();", 0);
        }
        else
        {
            PadOut("LDX # 0 // NEXT < TOP", 0); 
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("CMP ZP.TOPL", 0);
            PadOut("if (C) // NEXT >= TOP", 0);
            PadOut("{", 0);
            PadOut("LDX # 1 // NEXT >= TOP", 1); 
            PadOut("}", 0);
        }
        PadOut("PHX", 0);
    }
    
    CompareEQ(bool isByte)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// == " + Bitness(isByte), 0); 
        
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TinyOps.CompareEQ();", 0);
        }
        else
        {
            PadOut("LDX # 0 // NEXT != TOP", 0); 
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("CMP ZP.TOPL", 0);
            PadOut("if (Z) // NEXT == TOP", 0);
            PadOut("{", 0);
            PadOut("LDX # 1 // NEXT == TOP", 1); 
            PadOut("}", 0);
        }
        PadOut("PHX", 0);
    }
    CompareNE(bool isByte)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// !=" + Bitness(isByte), 0); 
        
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TinyOps.CompareNE();", 0);
        }
        else
        {
            PadOut("LDX # 1 // NEXT != TOP", 0); 
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("CMP ZP.TOPL", 0);
            PadOut("if (Z) // NEXT == TOP", 0);
            PadOut("{", 0);
            PadOut("LDX # 0 // NEXT == TOP", 1); 
            PadOut("}", 0);
        }
        PadOut("PHX", 0);
    }
    
    Add(bool isByte)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// + " + Bitness(isByte), 0); 
    
        // arguments
        PopTopNext(isByte);
    
        // operation
        if (!isByte)
        {
            PadOut("TinyOps.Add16();", 0);
        }
        else
        {
            PadOut("CLC // Clear Carry Flag", 0);
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("ADC ZP.TOPL // Add with Carry", 0);
            PadOut("STA ZP.TOPL", 0);
        }
        PushTop(isByte);
    }
    
    Sub(bool isByte)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// - " + Bitness(isByte), 0); 
    
        // arguments
        PopTopNext(isByte);
    
        // operation
        if (!isByte)
        {
            PadOut("TinyOps.Sub16();", 0);
        }
        else
        {
            PadOut("SEC // Set Carry Flag for Subtraction", 0);
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("SBC ZP.TOPL // Subtract with Carry", 0);
            PadOut("STA ZP.TOPL", 0);
        }
        PushTop(isByte);
    }
    
    Mul(bool isByte, bool isSigned)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// + " + Bitness(isByte), 0); 
    
        // arguments
        PopTopNext(isByte);
    
        // operation
        if (!isByte)
        {
            PadOut("TinyOps.Mul16();", 0);
        }
        else
        {
            PadOut("TinyOps.Mul8();", 0);
        }
        PushTop(isByte);
    }
    
    Div(bool isByte, bool isSigned)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// + " + Bitness(isByte), 0); 
    
        // arguments
        PopTopNext(isByte);
    
        // operation
        if (!isByte)
        {
            PadOut("TinyOps.Div16();", 0);
        }
        else
        {
            PadOut("TinyOps.Div8();", 0);
        }
        PushTop(isByte);
    }
    
    Mod(bool isByte, bool isSigned)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// + " + Bitness(isByte), 0); 
    
        // arguments
        PopTopNext(isByte);
    
        // operation
        if (!isByte)
        {
            PadOut("TinyOps.Mod16();", 0);
        }
        else
        {
            PadOut("TinyOps.Mod8();", 0);
        }
        PushTop(isByte);
    }
    
    
    
    
    
}
