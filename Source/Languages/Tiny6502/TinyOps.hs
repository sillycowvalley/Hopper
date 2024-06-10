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
            PadOut("PLA", 0);
            PadOut("STA ZP.NEXTL", 0);
        }
        else
        {
            PadOut("PLA", 0); // NEXTL
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
            PadOut("CMP ZP.TOPL", 0);
            PadOut("if (Z) // NEXT == TOP", 0);
            PadOut("{", 0);
            PadOut("LDX # 0 // NEXT == TOP", 1); 
            PadOut("}", 0);
        }
        PadOut("PHX", 0);
    }
    
    
    
    
    
}
