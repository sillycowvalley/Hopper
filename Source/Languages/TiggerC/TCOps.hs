unit TCOps
{
    uses "TCCode"
    uses "TCGen"
    
    PopTop(bool isByte)
    {
        if (!isByte)
        {
            PadOut("PLA", 0);
            PadOut("STA ZP.TOPH", 0);
        }
        PadOut("PLA", 0);
        PadOut("STA ZP.TOPL", 0);
    }
    PopNext(bool isByte)
    {
        if (!isByte)
        {
            PadOut("PLA", 0);
            PadOut("STA ZP.NEXTH", 0);
        }
        PadOut("PLA", 0);
        PadOut("STA ZP.NEXTL", 0);
    }
    PopTopNext(bool isByte)
    {
        PopTop(isByte);
        PopNext(isByte);
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
    PushNext(bool isByte)
    {
        PadOut("LDA ZP.NEXTL", 0);
        PadOut("PHA", 0);
        if (!isByte)
        {
            PadOut("LDA ZP.NEXTH", 0);
            PadOut("PHA", 0);
        }
    }
    CompareLT(bool isByte)
    {
        TCCode.PadOut("// <" + Bitness(isByte), 0); 
        
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TCOps.CompareLT();", 0);
        }
        else
        {
            PadOut("LDX # 1 // NEXT < TOP", 0);
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("CMP ZP.TOPL", 0);
            PadOut("if (C) // TOP <= NEXT ?", 0);
            PadOut("{", 0);
            PadOut("LDX # 0 // TOP <= NEXT", 1);
            PadOut("}", 0);
        }
        PadOut("PHX", 0);
    }
    CompareGT(bool isByte)
    {
         
        TCCode.PadOut("// >" + Bitness(isByte), 0); 
        
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TCOps.CompareGT();", 0);
        }
        else
        {
            PadOut("LDX # 1 // NEXT > TOP", 0);
            PadOut("LDA ZP.TOPL", 0);
            PadOut("CMP ZP.NEXTL", 0);
            PadOut("if (C) // TOP >= NEXT ?", 0);
            PadOut("{", 0);
            PadOut("LDX # 0 // TOP > NEXT", 1);
            PadOut("}", 0);
        }
        PadOut("PHX", 0);
    }
       
        
    CompareLTI()
    {
         
        TCCode.PadOut("// <" + Bitness(false), 0); 
        
        PopTopNext(false);
        PadOut("TCOps.CompareLTI();", 0);
        PadOut("PHX", 0);    
    }
    CompareLEI()
    {
         
        TCCode.PadOut("// <=" + Bitness(false), 0); 
        
        PopTopNext(false);
        PadOut("TCOps.CompareLEI();", 0);
        PadOut("PHX", 0);    
    }
    CompareGTI()
    {
         
        TCCode.PadOut("// >" + Bitness(false), 0); 
        
        PopTopNext(false);
        PadOut("TCOps.CompareGTI();", 0);
        PadOut("PHX", 0);    
    }
    CompareGEI()
    {
         
        TCCode.PadOut("// >=" + Bitness(false), 0); 
        
        PopTopNext(false);
        PadOut("TCOps.CompareGEI();", 0);
        PadOut("PHX", 0);    
    }
    
    Add(bool isByte)
    {
        // arguments
        PopTopNext(isByte);
    
        // operation
        if (!isByte)
        {
            PadOut("CLC // Clear Carry Flag", 0);
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("ADC ZP.TOPL", 0);
            PadOut("STA ZP.TOPL", 0);

            PadOut("LDA ZP.NEXTH", 0);
            PadOut("ADC ZP.TOPH", 0);
            PadOut("STA ZP.TOPH", 0);
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
         
        TCCode.PadOut("// - " + Bitness(isByte), 0); 
    
        // arguments
        PopTopNext(isByte);
    
        // operation
        if (!isByte)
        {
            PadOut("TCOps.Sub16();", 0);
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
    
    BitNot(bool isByte)
    {
         
        TCCode.PadOut("// ~ " + Bitness(isByte), 0); 
    
        // operation
        if (isByte)
        {
            PadOut("PLA", 0);
            PadOut("EOR # 0xFF", 0);
            PadOut("PHA", 0);
        }
        else
        {
            PadOut("PLX", 0);
            PadOut("PLA", 0);
            PadOut("EOR # 0xFF", 0);
            PadOut("PHA", 0);
            PadOut("TXA", 0);
            PadOut("EOR # 0xFF", 0);
            PadOut("PHA", 0);
        }
    }
    
    BoolNot()
    {
        TCCode.PadOut("// ! ", 0); 
        PadOut("PLA", 0);
        PadOut("if (Z)", 0);
        PadOut("{", 0);
        PadOut("LDA # 1", 1);
        PadOut("}", 0);
        PadOut("else", 0);
        PadOut("{", 0);
        PadOut("LDA # 0", 1);
        PadOut("}", 0);
        PadOut("PHA", 0);
    }
    
    Mul(bool isByte)
    {
         
        TCCode.PadOut("// * " + Bitness(isByte), 0); 
    
        // arguments
        PopTopNext(isByte);
    
        // operation
        if (!isByte)
        {
            PadOut("TCOps.Mul16();", 0);
        }
        else
        {
            PadOut("TCOps.Mul8();", 0);
        }
        PushTop(isByte);
    }
    
    Div(bool isByte)
    {
         
        TCCode.PadOut("// / " + Bitness(isByte), 0); 
    
        // arguments
        PopTopNext(isByte);
    
        // operation
        if (!isByte)
        {
            PadOut("TCOps.Div16();", 0);
        }
        else
        {
            PadOut("TCOps.Div8();", 0);
        }
        PushTop(isByte);
    }
    
    Mod(bool isByte)
    {
         
        TCCode.PadOut("// % " + Bitness(isByte), 0); 
    
        // arguments
        PopTopNext(isByte);
    
        // operation
        if (!isByte)
        {
            PadOut("TCOps.Mod16();", 0);
        }
        else
        {
            PadOut("TCOps.Mod8();", 0);
        }
        PushTop(isByte);
    }
    
    
    MulI()
    {
         
        TCCode.PadOut("// * " + Bitness(false), 0); 
    
        // arguments
        PopTopNext(false);
    
        // operation
        PadOut("TCOps.MulI();", 0);
        PushTop(false);
    }
    
    DivI()
    {
         
        TCCode.PadOut("// / " + Bitness(false), 0); 
    
        // arguments
        PopTopNext(false);
    
        // operation
        PadOut("TCOps.DivI();", 0);
        PushTop(false);
    }
    
    ModI()
    {
        TCCode.PadOut("// % " + Bitness(false), 0); 
    
        // arguments
        PopTopNext(false);
    
        // operation
        PadOut("TCOps.ModI();", 0);
        PushTop(false);
    }
    
    
    Or(bool isByte)
    {
        PopTopNext(isByte);
        TCCode.PadOut("LDA ZP.NEXTL", 0);
        TCCode.PadOut("ORA ZP.TOPL", 0);
        TCCode.PadOut("PHA", 0);
        if (!isByte)
        {
            TCCode.PadOut("LDA ZP.NEXTH", 0);
            TCCode.PadOut("ORA ZP.TOPH", 0);
            TCCode.PadOut("PHA", 0);
        }
    }        
    And(bool isByte)
    {
        PopTopNext(isByte);
        TCCode.PadOut("LDA ZP.NEXTL", 0);
        TCCode.PadOut("AND ZP.TOPL", 0);
        TCCode.PadOut("PHA", 0);
        if (!isByte)
        {
            TCCode.PadOut("LDA ZP.NEXTH", 0);
            TCCode.PadOut("AND ZP.TOPH", 0);
            TCCode.PadOut("PHA", 0);
        }
    }
    Xor(bool isByte)
    {
        PopTopNext(isByte);
        TCCode.PadOut("LDA ZP.NEXTL", 0);
        TCCode.PadOut("EOR ZP.TOPL", 0);
        TCCode.PadOut("PHA", 0);
        if (!isByte)
        {
            TCCode.PadOut("LDA ZP.NEXTH", 0);
            TCCode.PadOut("EOR ZP.TOPH", 0);
            TCCode.PadOut("PHA", 0);
        }
    }
    
    Shr(bool isByte)
    {
        TCCode.PadOut("// >>", 0);
        PopTopNext(isByte);
        if (isByte)
        {
            TCCode.PadOut("loop", 0);
            TCCode.PadOut("{", 0);
            TCCode.PadOut("LDA ZP.TOPL", 1);
            TCCode.PadOut("if (Z) { break; }", 1);
            TCCode.PadOut("LSR ZP.NEXTL", 1);
            TCCode.PadOut("DEC ZP.TOPL", 1);
            TCCode.PadOut("}", 0);
        }
        else
        {
            TCCode.PadOut("loop", 0);
            TCCode.PadOut("{", 0);
            TCCode.PadOut("LDA ZP.TOPL", 1);
            TCCode.PadOut("if (Z) { break; }", 1);
            TCCode.PadOut("LSR ZP.NEXTH", 1);
            TCCode.PadOut("ROR ZP.NEXTL", 1);
            TCCode.PadOut("DEC ZP.TOPL", 1);
            TCCode.PadOut("}", 0);
        }
        PushNext(isByte);
    }
    Shl(bool isByte)
    {
        TCCode.PadOut("// >>", 0);
        PopTopNext(isByte);
        if (isByte)
        {
            TCCode.PadOut("loop", 0);
            TCCode.PadOut("{", 0);
            TCCode.PadOut("LDA ZP.TOPL", 1);
            TCCode.PadOut("if (Z) { break; }", 1);
            TCCode.PadOut("ASL ZP.NEXTL", 1);
            TCCode.PadOut("DEC ZP.TOPL", 1);
            TCCode.PadOut("}", 0);
        }
        else
        {
            TCCode.PadOut("loop", 0);
            TCCode.PadOut("{", 0);
            TCCode.PadOut("LDA ZP.TOPL", 1);
            TCCode.PadOut("if (Z) { break; }", 1);
            TCCode.PadOut("ASL ZP.NEXTL", 1);
            TCCode.PadOut("ROL ZP.NEXTH", 1);
            TCCode.PadOut("DEC ZP.TOPL", 1);
            TCCode.PadOut("}", 0);
        }
        PushNext(isByte);   
    }
    CompareLE(bool isByte)
    {
        TCCode.PadOut("// <=" + Bitness(isByte), 0); 
        
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TCOps.CompareLE();", 0);
        }
        else
        {
            PadOut("LDX # 1 // NEXT <= TOP", 0);
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("CMP ZP.TOPL", 0);
            PadOut("if (NZ) // NEXT == TOP (not >)?", 0);
            PadOut("{", 0);
            PadOut("if (C) // NEXT <  TOP (not >)?", 1);
            PadOut("{", 1);
            PadOut("LDX # 0 // NEXT > TOP", 2);
            PadOut("}", 1);
            PadOut("}", 0);
        }
        PadOut("PHX", 0);
    }
    
    CompareGE(bool isByte)
    {
         
        TCCode.PadOut("// >=" + Bitness(isByte), 0); 
        
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TCOps.CompareGE();", 0);
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
         
        TCCode.PadOut("// == " + Bitness(isByte), 0); 
        
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TCOps.CompareEQ();", 0);
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
         
        TCCode.PadOut("// !=" + Bitness(isByte), 0); 
        
        // arguments
        PopTopNext(isByte);
        
        // operation
        if (!isByte)
        {
            PadOut("TCOps.CompareNE();", 0);
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
}
