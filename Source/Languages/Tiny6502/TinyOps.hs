unit TinyOps
{
    uses "TinyCode"
    
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
            PadOut("if (C) // TOP <= NEXT ?", 0);
            PadOut("{", 0);
            PadOut("LDX # 0 // TOP <= NEXT", 1); 
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
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// <" + Bitness(false), 0); 
        
        PopTopNext(false);
        PadOut("TinyOps.CompareLTI();", 0);    
        PadOut("PHX", 0);    
    }
    CompareLEI()
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// <=" + Bitness(false), 0); 
        
        PopTopNext(false);
        PadOut("TinyOps.CompareLEI();", 0);    
        PadOut("PHX", 0);    
    }
    CompareGTI()
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// >" + Bitness(false), 0); 
        
        PopTopNext(false);
        PadOut("TinyOps.CompareGTI();", 0);    
        PadOut("PHX", 0);    
    }
    CompareGEI()
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// >=" + Bitness(false), 0); 
        
        PopTopNext(false);
        PadOut("TinyOps.CompareGEI();", 0);    
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
    
    AddLiteral(bool isByte, uint literal)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// + " + literal.ToString() + Bitness(isByte), 0); 
    
        PopTop(isByte);
    
        // operation
        PadOut("CLC // Clear Carry Flag", 0);
        PadOut("LDA ZP.TOPL", 0);
        PadOut("ADC # 0x" + (literal.GetByte(0)).ToHexString(2), 0);
        PadOut("STA ZP.TOPL", 0);
        if (!isByte)
        {
            PadOut("LDA ZP.TOPH", 0);
            PadOut("ADC # 0x" + (literal.GetByte(1)).ToHexString(2), 0);
            PadOut("STA ZP.TOPH", 0);
        }
        PushTop(isByte);
    }
    
    SubLiteral(bool isByte, uint literal)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// - " + literal.ToString() + Bitness(isByte), 0); 
    
        PopTop(isByte);
    
        // operation
        PadOut("SEC // Set Carry Flag for Subtraction", 0);
        PadOut("LDA ZP.TOPL", 0);
        PadOut("SBC # 0x" + (literal.GetByte(0)).ToHexString(2), 0);
        PadOut("STA ZP.TOPL", 0);
        
        if (!isByte)
        {
            PadOut("LDA ZP.TOPH", 0);
            PadOut("SBC # 0x" + (literal.GetByte(1)).ToHexString(2), 0);
            PadOut("STA ZP.TOPH", 0);
        }
        PushTop(isByte);
    }
    
    BitNot(bool isByte)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// ~ " + Bitness(isByte), 0); 
    
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
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// ! ", 0); 
    
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
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// * " + Bitness(isByte), 0); 
    
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
    
    Div(bool isByte)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// / " + Bitness(isByte), 0); 
    
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
    
    Mod(bool isByte)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// % " + Bitness(isByte), 0); 
    
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
    
    
    MulI()
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// * " + Bitness(false), 0); 
    
        // arguments
        PopTopNext(false);
    
        // operation
        PadOut("TinyOps.MulI();", 0);
        PushTop(false);
    }
    
    DivI()
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// / " + Bitness(false), 0); 
    
        // arguments
        PopTopNext(false);
    
        // operation
        PadOut("TinyOps.DivI();", 0);
        PushTop(false);
    }
    
    ModI()
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// % " + Bitness(false), 0); 
    
        // arguments
        PopTopNext(false);
    
        // operation
        PadOut("TinyOps.ModI();", 0);
        PushTop(false);
    }
    
    
    Or(string comment, bool isByte)
    {
        TinyCode.PadOut("", 0);
        TinyCode.PadOut("// " + comment, 0);
        PopTopNext(isByte);
        TinyCode.PadOut("LDA ZP.NEXTL", 0);
        TinyCode.PadOut("ORA ZP.TOPL", 0);
        TinyCode.PadOut("PHA", 0);
        if (!isByte)
        {
            TinyCode.PadOut("LDA ZP.NEXTH", 0);
            TinyCode.PadOut("ORA ZP.TOPH", 0);
            TinyCode.PadOut("PHA", 0);
        }
    }        
    And(string comment, bool isByte)
    {
        TinyCode.PadOut("", 0);
        TinyCode.PadOut("// " + comment, 0);
        PopTopNext(isByte);
        TinyCode.PadOut("LDA ZP.NEXTL", 0);
        TinyCode.PadOut("AND ZP.TOPL", 0);
        TinyCode.PadOut("PHA", 0);
        if (!isByte)
        {
            TinyCode.PadOut("LDA ZP.NEXTH", 0);
            TinyCode.PadOut("AND ZP.TOPH", 0);
            TinyCode.PadOut("PHA", 0);
        }
    }
    Xor(string comment, bool isByte)
    {
        TinyCode.PadOut("", 0);
        TinyCode.PadOut("// " + comment, 0);
        PopTopNext(isByte);
        TinyCode.PadOut("LDA ZP.NEXTL", 0);
        TinyCode.PadOut("EOR ZP.TOPL", 0);
        TinyCode.PadOut("PHA", 0);
        if (!isByte)
        {
            TinyCode.PadOut("LDA ZP.NEXTH", 0);
            TinyCode.PadOut("EOR ZP.TOPH", 0);
            TinyCode.PadOut("PHA", 0);
        }
    }
    Shr(bool isByte)
    {
        TinyCode.PadOut("", 0);
        TinyCode.PadOut("// >>", 0);
        PopTopNext(isByte);
        if (isByte)
        {
            TinyCode.PadOut("loop", 0);
            TinyCode.PadOut("{", 0);
            TinyCode.PadOut("LDA ZP.TOPL", 1);
            TinyCode.PadOut("if (Z) { break; }", 1);
            TinyCode.PadOut("LSR ZP.NEXTL", 1);
            TinyCode.PadOut("DEC ZP.TOPL", 1);
            TinyCode.PadOut("}", 0);
        }
        else
        {
            TinyCode.PadOut("loop", 0);
            TinyCode.PadOut("{", 0);
            TinyCode.PadOut("LDA ZP.TOPL", 1);
            TinyCode.PadOut("if (Z) { break; }", 1);
            TinyCode.PadOut("LSR ZP.NEXTH", 1);
            TinyCode.PadOut("ROR ZP.NEXTL", 1);
            TinyCode.PadOut("DEC ZP.TOPL", 1);
            TinyCode.PadOut("}", 0);
        }
        PushNext(isByte);
    }
    Shl(bool isByte)
    {
        TinyCode.PadOut("", 0);
        TinyCode.PadOut("// >>", 0);
        PopTopNext(isByte);
        if (isByte)
        {
            TinyCode.PadOut("loop", 0);
            TinyCode.PadOut("{", 0);
            TinyCode.PadOut("LDA ZP.TOPL", 1);
            TinyCode.PadOut("if (Z) { break; }", 1);
            TinyCode.PadOut("ASL ZP.NEXTL", 1);
            TinyCode.PadOut("DEC ZP.TOPL", 1);
            TinyCode.PadOut("}", 0);
        }
        else
        {
            TinyCode.PadOut("loop", 0);
            TinyCode.PadOut("{", 0);
            TinyCode.PadOut("LDA ZP.TOPL", 1);
            TinyCode.PadOut("if (Z) { break; }", 1);
            TinyCode.PadOut("ASL ZP.NEXTL", 1);
            TinyCode.PadOut("ROL ZP.NEXTH", 1);
            TinyCode.PadOut("DEC ZP.TOPL", 1);
            TinyCode.PadOut("}", 0);
        }
        PushNext(isByte);   
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
            PadOut("if (NZ) // NEXT == TOP (not >)?", 0);
            PadOut("{", 0);
            PadOut("if (C) // NEXT <  TOP (not >)?", 1);
            PadOut("{", 1);
            PadOut("LDX # 0 // NEXT > TOP", 1); 
            PadOut("}", 1);
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
    
    CompareLTLiteral(bool isByte, uint literalValue)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// < " + literalValue.ToString() + Bitness(isByte), 0); 
        
        PopNext(isByte);
        byte topl = literalValue.GetByte(0);
        byte toph = literalValue.GetByte(1);
        
        PadOut("LDX # 1 // NEXT < TOP", 0); 
        if (isByte)
        {
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("CMP # 0x" + topl.ToHexString(2), 0);
        }
        else
        {
            PadOut("LDA ZP.NEXTH", 0);
            PadOut("CMP # 0x" + toph.ToHexString(2), 0);
            PadOut("if (Z)", 0);
            PadOut("{", 0);
            PadOut("LDA ZP.NEXTL", 1);
            PadOut("CMP # 0x" + topl.ToHexString(2), 1);
            PadOut("}", 0);
        }
        PadOut("if (C) // TOP <= NEXT ?", 0);
        PadOut("{", 0);
        PadOut("LDX # 0 // TOP <= NEXT", 1); 
        PadOut("}", 0);
    
        PadOut("PHX", 0);
    }
    CompareGTLiteral(bool isByte, uint literalValue)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// > " + literalValue.ToString() + Bitness(isByte), 0); 
        
        PopNext(isByte);
        byte topl = literalValue.GetByte(0);
        byte toph = literalValue.GetByte(1);
        
        PadOut("LDX # 1 // NEXT > TOP", 0); 
        
        if (isByte)
        {
            PadOut("LDA # 0x" + topl.ToHexString(2), 0);
            PadOut("CMP ZP.NEXTL", 0);
        }
        else
        {
            PadOut("LDA # 0x" + toph.ToHexString(2), 0);
            PadOut("CMP ZP.NEXTH", 0);
            PadOut("if (Z)", 0);
            PadOut("{", 0);
            PadOut("LDA # 0x" + topl.ToHexString(2), 1);
            PadOut("CMP ZP.NEXTL", 1);
            PadOut("}", 0);
        }
        PadOut("if (C) // TOP >= NEXT ?", 0);
        PadOut("{", 0);
        PadOut("LDX # 0 // TOP > NEXT", 1); 
        PadOut("}", 0);
        PadOut("PHX", 0);
    }
    
    CompareLELiteral(bool isByte, uint literalValue)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// <= " + literalValue.ToString() + Bitness(isByte), 0); 
        
        PopNext(isByte);
        byte topl = literalValue.GetByte(0);
        byte toph = literalValue.GetByte(1);
        
        
        PadOut("LDX # 1 // NEXT <= TOP", 0); 
        if (isByte)
        {
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("CMP # 0x" + topl.ToHexString(2), 0);
        }
        else
        {
            PadOut("LDA ZP.NEXTH", 0);
            PadOut("CMP # 0x" + toph.ToHexString(2), 0);
            PadOut("if (Z)", 0);
            PadOut("{", 0);
            PadOut("LDA ZP.NEXTL", 1);
            PadOut("CMP # 0x" + topl.ToHexString(2), 1);
            PadOut("}", 0);
        }
        
        PadOut("if (NZ) // NEXT == TOP (not >)?", 0);
        PadOut("{", 0);
        PadOut("if (C) // NEXT <  TOP (not >)?", 1);
        PadOut("{", 1);
        PadOut("LDX # 0 // NEXT > TOP", 1); 
        PadOut("}", 1);
        PadOut("}", 0);
    
        PadOut("PHX", 0);
    }
    
    CompareGELiteral(bool isByte, uint literalValue)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// >= " + literalValue.ToString() + Bitness(isByte), 0); 
        
        PopNext(isByte);
        byte topl = literalValue.GetByte(0);
        byte toph = literalValue.GetByte(1);
        
        PadOut("LDX # 0 // NEXT < TOP", 0); 
        if (isByte)
        {
            PadOut("LDA ZP.NEXTL", 0);
            PadOut("CMP # 0x" + topl.ToHexString(2), 0);
        }
        else
        {
            PadOut("LDA ZP.NEXTH", 0);
            PadOut("CMP # 0x" + toph.ToHexString(2), 0);
            PadOut("if (Z)", 0);
            PadOut("{", 0);
            PadOut("LDA ZP.NEXTL", 1);
            PadOut("CMP # 0x" + topl.ToHexString(2), 1);
            PadOut("}", 0);
        }
            
        PadOut("if (C) // NEXT >= TOP", 0);
        PadOut("{", 0);
        PadOut("LDX # 1 // NEXT >= TOP", 1); 
        PadOut("}", 0);
        
        PadOut("PHX", 0);
    }
    
    CompareLTILiteral(uint literalValue)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// < " + literalValue.ToString() + Bitness(false), 0); 
        
        PopNext(false);
        byte topl = literalValue.GetByte(0);
        byte toph = literalValue.GetByte(1);
        
        PadOut("// NEXT < TOP?", 0);
        PadOut("// TOP - NEXT > 0", 0);
        PadOut("SEC", 0);    
        PadOut("LDA # 0x" + topl.ToHexString(2), 0);    
        PadOut("SBC ZP.NEXTL", 0);    
        PadOut("STA ZP.TOPL", 0);    
        PadOut("LDA # 0x" + toph.ToHexString(2), 0);    
        PadOut("SBC ZP.NEXTH", 0);    
        PadOut("STA ZP.TOPH", 0);    
        PadOut("", 0);    
        PadOut("ASL           // sign bit into carry", 0);    
        PadOut("", 0);    
        PadOut("LDX #0  // TOP <= 0", 0);    
        PadOut("loop", 0);    
        PadOut("{", 0);    
        PadOut("if (C) { break; }", 1);    
        PadOut("//  0 or positive", 1);    
        PadOut("LDA ZP.TOPL", 1);    
        PadOut("if (Z)", 1);    
        PadOut("{", 1);    
        PadOut("LDA ZP.TOPH", 2);    
        PadOut("if (Z) { break; }", 2);    
        PadOut("}", 1);    
        PadOut("LDX #1", 1);    
        PadOut("break;", 1);    
        PadOut("}", 0);    
        PadOut("PHX", 0);    
    }
    CompareLEILiteral(uint literalValue)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// <= " + literalValue.ToString() + Bitness(false), 0); 
        
        PopNext(false);
        byte topl = literalValue.GetByte(0);
        byte toph = literalValue.GetByte(1);
        
        PadOut("// NEXT <= TOP?", 0); 
        PadOut("// TOP - NEXT >= 0", 0); 
        PadOut("", 0); 
        PadOut("SEC", 0); 
        PadOut("LDA # 0x" + topl.ToHexString(2), 0); 
        PadOut("SBC ZP.NEXTL", 0); 
        PadOut("STA ZP.TOPL", 0); 
        PadOut("LDA # 0x" + toph.ToHexString(2), 0); 
        PadOut("SBC ZP.NEXTH", 0); 
        PadOut("STA ZP.TOPH", 0); 
        PadOut("", 0); 
        PadOut("ASL           // sign bit into carry", 0); 
        PadOut("", 0); 
        PadOut("LDX #0", 0); 
        PadOut("if (NC)", 0); 
        PadOut("{", 0); 
        PadOut("// 0 or positive", 1); 
        PadOut("LDX #1", 1); 
        PadOut("}", 0); 
        
        PadOut("PHX", 0);    
    }
    CompareGTILiteral(uint literalValue)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// > " + literalValue.ToString() + Bitness(false), 0); 
        
        PopNext(false);
        byte topl = literalValue.GetByte(0);
        byte toph = literalValue.GetByte(1);
        
        TinyCode.PadOut("// NEXT > TOP?", 0);
        TinyCode.PadOut("// NEXT - TOP > 0", 0);
        TinyCode.PadOut("", 0);
        TinyCode.PadOut("SEC", 0);
        TinyCode.PadOut("LDA ZP.NEXTL", 0);
        TinyCode.PadOut("SBC # 0x" + topl.ToHexString(2), 0);
        TinyCode.PadOut("STA ZP.TOPL", 0);
        TinyCode.PadOut("LDA ZP.NEXTH", 0);
        TinyCode.PadOut("SBC # 0x" + toph.ToHexString(2), 0);
        TinyCode.PadOut("STA ZP.TOPH", 0);
        TinyCode.PadOut("", 0);
        TinyCode.PadOut("ASL           // sign bit into carry", 0);
        TinyCode.PadOut("", 0);
        TinyCode.PadOut("LDX #0  // TOP <= 0", 0);
        TinyCode.PadOut("loop", 0);
        TinyCode.PadOut("{", 0);
        TinyCode.PadOut("if (C) { break; }", 1);
        TinyCode.PadOut("//  0 or positive", 1);
        TinyCode.PadOut("LDA ZP.TOPL", 1);
        TinyCode.PadOut("if (Z)", 1);
        TinyCode.PadOut("{", 1);
        TinyCode.PadOut("LDA ZP.TOPH", 2);
        TinyCode.PadOut("if (Z) { break; }", 2);
        TinyCode.PadOut("}", 1);
        TinyCode.PadOut("LDX #1", 1);
        TinyCode.PadOut("break;", 1);
        TinyCode.PadOut("}", 0);
        
        PadOut("PHX", 0);    
    }
    CompareGEILiteral(uint literalValue)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// >= " + literalValue.ToString() + Bitness(false), 0); 
        
        PopNext(false);
        byte topl = literalValue.GetByte(0);
        byte toph = literalValue.GetByte(1);
        
        PadOut("// NEXT >= TOP?", 0); 
        PadOut("// NEXT - TOP >= 0", 0); 
        PadOut("", 0); 
        PadOut("SEC", 0); 
        PadOut("LDA ZP.NEXTL", 0); 
        PadOut("SBC # 0x" + topl.ToHexString(2), 0); 
        PadOut("STA ZP.TOPL", 0); 
        PadOut("LDA ZP.NEXTH", 0); 
        PadOut("SBC # 0x" + toph.ToHexString(2), 0); 
        PadOut("STA ZP.TOPH", 0); 
        PadOut("", 0); 
        PadOut("ASL           // sign bit into carry", 0); 
        PadOut("", 0); 
        PadOut("LDX #0", 0); 
        PadOut("if (NC)", 0); 
        PadOut("{", 0); 
        PadOut("// 0 or positive", 1); 
        PadOut("LDX #1", 1); 
        PadOut("}", 0); 
        
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
    
    CompareEQLiteral(bool isByte, uint literalValue)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// == " + literalValue.ToString() + Bitness(isByte), 0); 
        
        // arguments
        PopNext(isByte);
        byte topl = literalValue.GetByte(0);
        byte toph = literalValue.GetByte(1);
        
        PadOut("LDX # 0 // NEXT != TOP", 0); 
        PadOut("LDA ZP.NEXTL", 0);
        if (topl != 0)
        {
            PadOut("CMP # 0x" + topl.ToHexString(2), 0);
        }
        PadOut("if (Z) // NEXT == TOP", 0);
        PadOut("{", 0);
        if (!isByte)
        {
            PadOut("LDA ZP.NEXTH", 1);
            if (toph != 0)
            {
                PadOut("CMP # 0x" + toph.ToHexString(2), 1);
            }
            PadOut("if (Z)", 1);
            PadOut("{", 1);
            PadOut("LDX # 1 // NEXT == TOP", 2);
            PadOut("}", 1);
        }
        else
        {
            PadOut("LDX # 1 // NEXT == TOP", 1); 
        }
        PadOut("}", 0);
    
        PadOut("PHX", 0);
    }
    CompareNELiteral(bool isByte, uint literalValue)
    {
        TinyCode.PadOut("", 0); 
        TinyCode.PadOut("// != " + literalValue.ToString() + Bitness(isByte), 0); 
        
        // arguments
        PopNext(isByte);
        byte topl = literalValue.GetByte(0);
        byte toph = literalValue.GetByte(1);
        
       
        PadOut("LDX # 1 // NEXT != TOP", 0); 
        PadOut("LDA ZP.NEXTL", 0);
        if (topl != 0)
        {
            PadOut("CMP # 0x" + topl.ToHexString(2), 0);
        }
        PadOut("if (Z) // NEXT == TOP", 0);
        PadOut("{", 0);
        if (!isByte)
        {
            PadOut("LDA ZP.NEXTH", 1);
            if (toph != 0)
            {
                PadOut("CMP # 0x" + toph.ToHexString(2), 1);
            }
            PadOut("if (Z)", 1);
            PadOut("{", 1);
            PadOut("LDX # 0 // NEXT == TOP", 2);
            PadOut("}", 1);
        }
        else
        {
            PadOut("LDX # 0 // NEXT == TOP", 1); 
        }
        PadOut("}", 0);
   
        PadOut("PHX", 0);
    }
    
}
