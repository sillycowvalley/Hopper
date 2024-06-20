unit TCCode
{
    uses "TCSymbols"
    uses "TCOps"
    uses "TCGen"
    
    file codeFile;
    string codePath;
    
    uint lastGlobal;
    <string,uint> globalIndex;
    <string,string> globalType;
    <string,string> globalValue;
    
    uint lastFunction;
    <string,uint> functionIndex;
    
    uint line;
    uint extra;
    
    bool generating;
    bool Generating { get { return generating; } set { generating = value; } }
    
    bool inStreamMode;
    bool InStreamMode { get { return inStreamMode; } set { inStreamMode = value; } }
    
    PadOut(string text, int delta)
    {
        if (InStreamMode)
        {
            Die(0x0B);
        }
        switch (text)
        {
            case "INX": { xOffset++; text = text + offsetComment("X"); }
            case "DEX": { xOffset--; text = text + offsetComment("X"); }
            case "INY": { yOffset++; text = text + offsetComment("Y"); }
            case "DEY": { yOffset--; text = text + offsetComment("Y"); }
        }
        if (text.EndsWith(", X")) { text = text + offsetComment("X"); }
        if (text.EndsWith(", Y")) { text = text + offsetComment("Y"); }
        
        if (generating)
        {
            if (text.Length != 0)
            {
                text = ("").Pad(' ', uint((int(BlockLevel)+int(extra)+delta) * 4)) + text;
            }
            line++;
            codeFile.Append(text + Char.EOL);
        }
    }
    
    Initialize(string path)
    {
        generating = true;
        string extension = Path.GetExtension(path);
        codePath = path.Replace(extension, ".asm");
        File.Delete(codePath);
        
        string name = Path.GetFileName(path);
        name = name.Replace(extension, "");
        name = name.ToUpper();
        
        codeFile = File.Create(codePath);
        
        PadOut("program " + name, 0);
    }
    Uses()
    {
        PadOut("#define TIGGERC", 0);
        if (!IsDefined("CPU_65C02S"))
        {
            PadOut("#define CPU_65C02S", 0);
        }
        if (!IsDefined("ROM_32K") && !IsDefined("ROM_16K") && !IsDefined("ROM_8K") && !IsDefined("ROM_4K") && !IsDefined("ROM_1K"))
        {
            PadOut("#define ROM_32K", 0);
        }
        PadOut("#define W65C22_VIA", 0);
        if (!IsDefined("ACIA_6850") && !IsDefined("APPLE_I"))
        {
            PadOut("#define ACIA_6850", 0);
        }
        PadOut("#define FASTINTS", 0);
        
        PadOut("",0);
        PadOut("uses \"resources.asm\"",0);
        PadOut("",0);
        PadOut("uses \"/Source/Runtime/6502/ZeroPage\"", 0);
        PadOut("uses \"/Source/Runtime/6502/Diagnostics\"", 0);
        PadOut("uses \"/Source/Runtime/6502/MemoryMap\"", 0);
        PadOut("uses \"/Source/Runtime/6502/Utilities\"", 0);
        PadOut("uses \"/Source/Runtime/6502/Types\"", 0);
        PadOut("uses \"/Source/Runtime/6502/IntMath\"", 0);
        PadOut("uses \"/Source/Runtime/6502/Memory\"", 0);
        PadOut("uses \"/Source/Runtime/6502/Allocate\"", 0);
        PadOut("uses \"/Source/Runtime/6502/Free\"", 0);
        PadOut("uses \"/Source/Runtime/6502/Serial\"", 0);
        PadOut("uses \"/Source/Runtime/6502/Time\"", 0);
        PadOut("uses \"/Source/Runtime/6502/Devices/W65C22\"", 0);
        PadOut("uses \"/Source/Languages/TiggerC/TCOps\"", 0);
        PadOut("uses \"/Source/Languages/TiggerC/TCSys\"", 0);
        PadOut("",0);
        PadOut("IRQ()",0);
        PadOut("{",0);
        PadOut("Serial.ISR();",1);
        PadOut("W65C22.ISR();",1);
        PadOut("}",0);
        PadOut("",0);
        PadOut("NMI()",0);
        PadOut("{",0);
        PadOut("INC ZP.SerialBreakFlag",1);
        PadOut("}",0);
        PadOut("",0);
    }
    StartUp()
    {
        PadOut("Memory.InitializeHeapSize();",0);
        PadOut("Serial.Initialize();", 0);
        PadOut("W65C22.Initialize();", 0);
        PadOut("",0);
    }
    Flush()
    {
        codeFile.Flush();
        
        string resourcesPath = "/Debug/Obj/resources.asm";
        File.Delete(resourcesPath);
        
        file rfile = File.Create(resourcesPath);
        rfile.Append("unit Resources"+ Char.EOL);
        rfile.Append("{" + Char.EOL);
        rfile.Append("    const string StrConsts = {");
        string cstr = GetStringConstants(); 
        foreach (var c in cstr)
        {
            rfile.Append("0x" + (byte(c)).ToHexString(2) +", ");
        }
        rfile.Append("};" + Char.EOL);
        rfile.Append("}" + Char.EOL);
        rfile.Flush();
    }
    <string> deferred;
    Defer(string content)
    {    
        deferred.Append(content);
    }
    Function(string functionName)
    {
        string name = "|" + functionName + "()";
        name = name.Replace("|main()", "Hopper()").Replace("|","");
        deferred.Clear();   
        deferred.Append("");
        deferred.Append(name);
    }
    EmitDeferred()
    {
        foreach (var line in deferred)
        {
            PadOut(line, -1);
        }
        deferred.Clear();
    }
    If(string comment)
    {
        PadOut("", 0);
        PadOut("PLA // bool", 0); // bool so one byte
        PadOut("if (NZ) // " + comment, 0);
    }
    Else()
    {
        PadOut("else", 0);
    }
    Loop(string comment)
    {
        PadOut("loop", 0);
        TCConstant.EnterBlock();
        TCSymbols.EnterBlock(true, comment);
    }
    EndLoop(string comment)
    {
        TCSymbols.LeaveBlock(comment, true); // end loop
        TCConstant.LeaveBlock();
    }
    Break(string comment)
    {
        if (comment.Length != 0)
        {
            PadOut("break; // " + comment, 0);
        }
        else
        {
            PadOut("break;", 0);
        }
    }
    Return(string comment)
    {
        PadOut("", 0);
        PadOut("// POP BP", 0);
        PadOut("PLX", 0); OffsetReset("X");
        PadOut("STX ZP.BP", 0);
        
        if (comment.Length != 0)
        {
            PadOut("return; // " + comment, 0);
        }
        else
        {
            PadOut("return;", 0);
        }
    }
    Continue()
    {
        PadOut("continue;", 0);
    }
    
    PushWord(uint word)
    {
        PadOut("LDA # 0x" + (word.GetByte(0)).ToHexString(2), 0);
        PadOut("PHA", 0);
        if (word.GetByte(0) != word.GetByte(1))
        {
            PadOut("LDA # 0x" + (word.GetByte(1)).ToHexString(2), 0);
        }
        PadOut("PHA", 0);
    }
    PushByte(byte value)
    {
        PadOut("LDA # 0x" + value.ToHexString(2), 0);
        PadOut("PHA", 0);
    }
    PushConst(uint word)
    {
        PadOut("LDA # ((Resources.StrConsts + 0x" + (word.GetByte(0)).ToHexString(2) + ") & 0xFF)", 0);
        PadOut("PHA", 0);
        PadOut("LDA # ((Resources.StrConsts + 0x" + (word.GetByte(1)).ToHexString(2) + "00) >> 8)", 0);
        PadOut("PHA", 0);
    }
    
    CastPad(bool doUnder)
    {
        if (InStreamMode)
        {
            if (doUnder)
            {
                TCGen.Comment("cast MSB (doUnder)");
                TCGen.PadUnder();
            }
            else
            {
                TCGen.Comment("cast MSB");
                TCGen.PushImmediate(true, 0);
            }
        }
        else
        {
            if (doUnder)
            {
                PadOut("PLA // cast MSB (doUnder)", 0);
                PadOut("STA ZP.TOPH", 0);
                PadOut("PLA", 0);
                PadOut("STA ZP.TOPL", 0);
                PadOut("LDA # 0x00", 0);
                PadOut("PHA", 0);
                PadOut("LDA ZP.TOPL", 0);
                PadOut("PHA", 0);
                PadOut("LDA ZP.TOPH", 0);
                PadOut("PHA", 0);
            }
            else
            {
                PadOut("LDA # 0x00" + " // cast MSB", 0);
                PadOut("PHA", 0);
            }
        }
    }
    WordToByte(bool doUnder, string byteType)
    {
        if (doUnder)
        {
            Die(0x0A); // WordToByte doUnder
        }
        if (InStreamMode)
        {
            TCGen.Comment("16 bit as '" + byteType + "'");
            TCGen.DecSP(1);
        }
        else
        {
            PadOut("", 0);
            PadOut("// 16 bit as '" + byteType + "'", 0);
            PadOut("PLA // MSB", 0);       
        }
    }
    ToBool(bool isByte, bool doUnder)
    {
        if (doUnder)
        {
            Die(0x0A); // ToBool doUnder
        }
        if (isByte)
        {
            PadOut("", 0);
            PadOut("// 8 bit as 'bool'", 0);
            PadOut("PLA", 0);
            PadOut("if (NZ)", 0);
            PadOut("{", 0);
            PadOut("LDA # 1 // true", 1);
            PadOut("}", 0);
            PadOut("PHA", 0);
        }
        else
        {
            PadOut("", 0);
            PadOut("// 16 bit as 'bool'", 0);
            PadOut("PLA // MSB", 0);
            PadOut("if (NZ)", 0);
            PadOut("{", 0);
            PadOut("PLA // LSB", 1);
            PadOut("LDA # 1 // true", 1);
            PadOut("}", 0);
            PadOut("else", 0);
            PadOut("{", 0);
            PadOut("PLA // LSB", 1);
            PadOut("if (NZ)", 1);
            PadOut("{", 1);
            PadOut("LDA # 1 // true", 2);
            PadOut("}", 1);
            PadOut("}", 0);
            PadOut("PHA", 0);
        }
    }
    
    Enter()
    {
        PadOut("// PUSH BP", 0);
        PadOut("LDX ZP.BP", 0);
        PadOut("PHX", 0);
        PadOut("// SP -> BP", 0);
        PadOut("TSX", 0);
        PadOut("STX ZP.BP", 0);
        PadOut("", 0);
        PadOut("// locals scope", 0);
        PadOut("", 0);
    }
    Leave()
    {
        PadOut("", 0);
        PadOut("// end of locals scope", 0);
        PadOut("", 0); 
            
        TCSymbols.FreeAutomaticAllocations(GetCurrentVariableLevel());
        TCCode.PopBytes(GetLevelBytes(GetCurrentVariableLevel()), "local variable " + VariableComment()); // method single exit loop
        
        PadOut("", 0);
        PadOut("// POP BP", 0);
        PadOut("PLX", 0); OffsetReset("X");
        PadOut("STX ZP.BP", 0);
    }
    PopBytes(string comment)
    {
        byte bytes = GetLevelBytes(GetCurrentVariableLevel());
        PopBytes(bytes, comment);
    }
    PopBytes(byte bytes, string comment)
    {
        if (bytes > 0)
        {
            PadOut("", 0);
            PadOut("// " + comment + " bytes to pop: " + bytes.ToString(), 0);
            for (byte i=0; i < bytes; i++)
            {
                PadOut("PLY", 0); OffsetReset("Y");// leaving A and X for potential return values
            }
        }
    }
    bool yOffsetGood;
    bool xOffsetGood;
    int xOffset;
    int yOffset;
    
    string offsetComment(string register)
    {
        if (InStreamMode)
        {
            Die(0x0B);
        }
        if (register == "X")
        {
            if (!xOffsetGood)
            {
                return " // X invalid";    
            }
            if (xOffset < 0)
            {
                return " //  X = BP" + xOffset.ToString();
            }
            return " //  X = BP+" + xOffset.ToString();
        }
        if (!yOffsetGood)
        {
            return " // Y invalid";    
        }
        if (yOffset < 0)
        {
            return " //  Y = BP" + yOffset.ToString();
        }
        return " //  Y = BP+" + yOffset.ToString();
    }
    OffsetReset(string register)
    {
        if (register == "X")
        {
            xOffsetGood = false;
        }
        else
        {
            yOffsetGood = false;
        }
    }
    
    
    OffsetSet(string toRegister, string fromRegister)
    {
        if (InStreamMode)
        {
            Die(0x0B);
        }
        if ((toRegister == "X") && (fromRegister == "Y"))
        {
            if (!yOffsetGood)
            {
                Die(0x0B);
            }
            xOffset = yOffset;
            xOffsetGood = true;
        }
        else if ((toRegister == "Y") && (fromRegister == "X"))
        {
            if (!xOffsetGood)
            {
                Die(0x0B);
            }
            yOffset = xOffset;
            yOffsetGood = true;
        }
        else
        {
            Die(0x0B);
        }
    }
    BPOffset(int offset, string register)
    {
        if (InStreamMode)
        {
            Die(0x0B);
        }
        
        int desired = offset;
        if (offset > 0)
        {
            // locals
            desired = -offset;
        }
        else if (offset < 0)
        {
            // arguments
            desired = -offset;
            desired += 3; // BP (1 byte) and return address (2 bytes)
        }
        loop
        {
            if ((xOffsetGood && (register == "X"))  || (yOffsetGood && (register == "Y")))
            {
                
                
                int actual;
                if (register == "X")
                {
                    actual = xOffset;
                }
                if (register == "Y")
                {
                    actual = yOffset;
                }
                
                string comment = "// Before: BP";
                if (actual < 0)
                {
                    comment += actual.ToString();    
                }
                else
                {
                    comment += "+" + actual.ToString();
                }
                comment += "  Desired: BP";
                if (desired < 0)
                {
                    comment += desired.ToString();    
                }
                else
                {
                    comment += "+" + desired.ToString();    
                }
                PadOut(comment, 0);
#ifdef XY_TRACKING                
                int delta = desired - actual;
                if (delta == 0)
                {
                    //PrintLn();
                    //Print(comment + "   ->  " + delta.ToString());
                    break;
                }
                else if (delta == -1)
                {
                    //PrintLn();
                    //Print(comment + "   ->  " + delta.ToString());
                    PadOut("DE" + register, 0);
                    break;
                }
                else if (delta == +1)
                {
                    //PrintLn();
                    //Print(comment + "   ->  " + delta.ToString());
                    PadOut("IN" + register, 0);
                    break;
                }
                else if (delta == -3)
                {
                    //PrintLn();
                    //Print(comment + "   ->  " + delta.ToString());
                    PadOut("DE" + register, 0);
                    PadOut("DE" + register, 0);
                    PadOut("DE" + register, 0);
                    break;
                }
                else if (delta == +3)
                {
                    //PrintLn();
                    //Print(comment + "   ->  " + delta.ToString());
                    PadOut("DE" + register, 0);
                    PadOut("DE" + register, 0);
                    PadOut("DE" + register, 0);
                    break;
                }
                else
                {
                    //PrintLn();
                    //Print(comment + "   ->  " + delta.ToString());
                }
#endif
            }
            
            if (register == "X")
            {
                xOffset = desired;
                xOffsetGood = true;   
            }
            else
            {
                yOffset = desired;
                yOffsetGood = true;
            }
            
            if (desired == 0)
            {
                PadOut("LD" + register + " ZP.BP" + offsetComment(register), 0);
            }
            else
            {
                PadOut("LDA ZP.BP", 0);
                if (desired < 0)
                {
                    // locals
                    int negDesired = -desired;
                    PadOut("SEC", 0);
                    PadOut("SBC # 0x" + (negDesired.GetByte(0)).ToHexString(2), 0);
                }
                else if (desired > 0)
                {
                    // arguments
                    PadOut("CLC", 0);
                    PadOut("ADC # 0x" + (desired.GetByte(0)).ToHexString(2), 0);
                }
                PadOut("TA" + register + offsetComment(register), 0);
            }
            break;
        } // loop
    }
    string nameWithOffset(string name, int offset, bool isGlobal)
    {
        if (isGlobal)
        {
            return name + " [0x01FF - " + offset.ToString() + "]";
        }
        if (offset < 0) // argument
        {
            offset = -offset;
            return name + " [0x01FF - BP + 1 + " + offset.ToString() + "]";
        }
        return name + " [0x01FF - BP -" + offset.ToString() + "]";
    }
    
    string Bitness(bool isByte)
    {
        return " (" + (isByte ? "8 bit)" : "16 bit)");
    }
    
    PushVariable(string name, int offset, bool isByte, bool isGlobal)
    {
        PadOut("// PUSH " + nameWithOffset(name, offset, isGlobal) + Bitness(isByte), 0);
        if (isGlobal)
        {
            offset = 255 - offset;
            PadOut("LDA 0x01"+ (offset.GetByte(0)).ToHexString(2), 0);  
            PadOut("PHA", 0);
            if (!isByte)
            {
                offset--;
                PadOut("LDA 0x01"+ (offset.GetByte(0)).ToHexString(2), 0);  
                PadOut("PHA", 0);
            }
        }
        else
        {
            BPOffset(offset, "X");  
            PadOut("LDA 0x0100, X", 0);  
            PadOut("PHA", 0);
            if (!isByte)
            {
                PadOut("DEX", 0);
                PadOut("LDA 0x0100, X", 0);  
                PadOut("PHA", 0);
            }
        }
    }
    PopVariable(string name, int offset, bool isByte, bool isGlobal)
    {
        PadOut("// POP " + nameWithOffset(name, offset, isGlobal) + Bitness(isByte), 0);
        if (isGlobal)
        {
            offset = 255 - offset;
            if (!isByte)
            {
                offset--;
                PadOut("PLA", 0);
                PadOut("STA 0x01"+ (offset.GetByte(0)).ToHexString(2), 0);  
                offset++;
            }
            PadOut("PLA", 0);
            PadOut("STA 0x01"+ (offset.GetByte(0)).ToHexString(2), 0);  
        }
        else
        {
            BPOffset(offset, "X");
            if (!isByte)
            {
                PadOut("DEX", 0);
                PadOut("PLA", 0);
                PadOut("STA 0x0100, X", 0);  
                PadOut("INX", 0);
            }
            PadOut("PLA", 0);
            PadOut("STA 0x0100, X", 0);  
        }
    }
    Dup(bool isByte)
    {
        PadOut("", 0);
        PadOut("// dup" +  Bitness(isByte), 0);
        
        PadOut("PLA", 0);
        if (!isByte)
        {
            PadOut("PLX", 0); OffsetReset("X");
            PadOut("PHX", 0);
            PadOut("PHA", 0);
            PadOut("PHX", 0);
            PadOut("PHA", 0);
        }
        else
        {
            PadOut("PHA", 0);
            PadOut("PHA", 0);
        }
    }
    Ret(bool isByte)
    {
        PadOut("", 0);
        PadOut("// return" +  Bitness(isByte), 0);
        if (!isByte)
        {
            PadOut("PLA", 0);
            PadOut("STA ZP.TOPH", 0);
        }
        PadOut("PLA", 0);
        PadOut("STA ZP.TOPL", 0);
    }
    
    PostIncrement(string name, int offset, bool isByte, bool inc, bool isGlobal) // i++
    {
        PadOut("", 0);
        PadOut("// " + name + (inc ? "++" : "--") + Bitness(isByte), 0);
        offsetToX(offset, isGlobal);
        
        PadOut("LDA 0x0100, X", 0);
        PadOut("PHA", 0); // A contains pre-value
        
        loop
        {
            if (!isByte)
            {
                PadOut("INX", 0);
                PadOut("LDA 0x0100, X", 0);
                PadOut("PHA", 0); // A contains pre-value
                PadOut("DEX", 0);
            }
            
            if (inc)
            {
                PadOut("INC 0x0100, X", 0);
                if (!isByte)
                {
                    PadOut("if (Z)", 0);
                    PadOut("{", 0);
                    PadOut("DEX", 1);
                    PadOut("INC 0x0100, X", 1);
                    PadOut("}", 0);
                }
            }
            else
            {
                PadOut("DEC 0x0100, X", 0);
                if (!isByte)
                {
                    PadOut("LDA # 0xFF", 0);
                    PadOut("CMP 0x0100, X", 0);
                    PadOut("if (Z)", 0);
                    PadOut("{", 0);
                    PadOut("DEX", 1);
                    PadOut("DEC 0x0100, X", 1);
                    PadOut("}", 0);
                }
            }
            break;
        } // loop
        
    }
    PreIncrement(string name, int offset, bool isByte, bool inc, bool isGlobal) // ++i
    {
        PadOut("", 0);
        PadOut("// " + (inc ? "++" : "--") + name + Bitness(isByte), 0);
        offsetToX(offset, isGlobal);
        if (inc)
        {
            PadOut("INC 0x0100, X", 0);
            if (!isByte)
            {
                PadOut("LDA # 0", 0);
                PadOut("CMP 0x0100, X", 0);
                PadOut("if (Z)", 0);
                PadOut("{", 0);
                PadOut("DEX", 1);
                PadOut("INC 0x0100, X", 1);
                PadOut("INX", 1);
                PadOut("}", 0);
            }
        }
        else
        {
            PadOut("DEC 0x0100, X", 0);
            if (!isByte)
            {
                PadOut("LDA # 0xFF", 0);
                PadOut("CMP 0x0100, X", 0);
                PadOut("if (Z)", 0);
                PadOut("{", 0);
                PadOut("DEX", 1);
                PadOut("DEC 0x0100, X", 1);
                PadOut("INX", 1);
                PadOut("}", 0);
            }
        }
        PadOut("LDA 0x0100, X", 0);
        PadOut("PHA", 0);
        if (!isByte)
        {
            PadOut("INX", 0);
            PadOut("LDA 0x0100, X", 0);
            PadOut("PHA", 0);
        }
    }
    Call(string functionName)
    {
        switch (functionName)
        {
            case "writeChar":
            {
                functionName = "TCSys.WriteChar";
            }
            case "millis":
            {
                 functionName = "TCSys.Millis";
            }
            case "delay":
            {
                 functionName = "TCSys.Delay";
            }
            case "malloc":
            {
                 functionName = "TCSys.Malloc";
            }
            case "free":
            {
                 functionName = "TCSys.Free";
            }
        }
        PadOut(functionName + "();", 0);
    }
    ReadMemory(bool indexIsByte, bool dataIsByte)
    {
        PadOut("// read memory" +  Bitness(dataIsByte), 0);
        if (indexIsByte)
        {
            PadOut("PLX", 0); OffsetReset("X");
            PadOut("LDA 0x00, X", 0);
            PadOut("PHA", 0);    
            if (!dataIsByte)
            {
                PadOut("INX", 0);
                PadOut("LDA 0x00, X", 0);
                PadOut("PHA", 0);    
            }
        } 
        else
        {
            PadOut("PLA", 0);        
            PadOut("STA ZP.TOPH", 0);        
            PadOut("PLA", 0);        
            PadOut("STA ZP.TOPL", 0);
            PadOut("LDA [ZP.TOP]", 0);
            PadOut("PHA", 0);
            if (!dataIsByte)
            {
                PadOut("LDX # 1", 0); OffsetReset("X");
                PadOut("LDA [ZP.TOP], X", 0);
                PadOut("PHA", 0);
            }
        }
    }
    WriteMemory(bool indexIsByte, bool dataIsByte)
    {
        PadOut("", 0);
        PadOut("// write memory" +  Bitness(dataIsByte), 0);
        if (indexIsByte)
        {
            if (!dataIsByte)
            {
                PadOut("PLA", 0); 
                PadOut("STA ZP.ACCH", 0);     
            }   
            PadOut("PLA", 0); 
            PadOut("PLX", 0); OffsetReset("X");
            PadOut("STA 0x00, X", 0);
            if (!dataIsByte)
            {
                PadOut("INX", 0); 
                PadOut("LDA ZP.ACCH", 0);     
                PadOut("STA 0x00, X", 0);
            }   
        } 
        else
        {
            if (!dataIsByte)
            {
                PadOut("PLA", 0); 
                PadOut("STA ZP.ACCH", 0);     
            }   
            PadOut("PLA", 0); 
            PadOut("STA ZP.ACCL", 0);     
            PadOut("PLA", 0);        
            PadOut("STA ZP.TOPH", 0);        
            PadOut("PLA", 0);        
            PadOut("STA ZP.TOPL", 0);
            PadOut("LDA ZP.ACCL", 0);        
            PadOut("STA [ZP.TOP]", 0);
            if (!dataIsByte)
            {
                PadOut("LDA ZP.ACCH", 0);     
                PadOut("LDX # 1", 0); OffsetReset("X");
                PadOut("LDA [ZP.TOP], X", 0);
            }
        }
    }
}
