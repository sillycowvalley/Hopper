unit TinyCode
{
    uses "TinySymbols"
    uses "TinyOps"
    
    file codeFile;
    file mapFile;
    string codePath;
    string tempCodePath;
    
    uint lastGlobal;
    <string,uint> globalIndex;
    <string,string> globalType;
    <string,string> globalValue;
    
    uint lastFunction;
    <string,uint> functionIndex;
    
    <string,bool> mapUsed;
    
    uint line;
    uint extra;
    
    bool capturing;
    string captured;
    bool generating;
    
    bool Generating { get { return generating; } set { generating = value; } }
    
    Capturing()
    {
        if (capturing || (captured.Length != 0))
        {
            Die(0x0B);
        }
        capturing = true;
    }
    string Captured()
    {
        if (!capturing)
        {
            Die(0x0B);
        }
        string result = captured;
        String.Build(ref captured);
        capturing = false;
        return result;
    }
    EmitCaptured(string content)
    {
        codeFile.Append(content);
    }
    
    Map(Token token)
    {
        string location = token.SourcePath + ":" + (token.Line).ToString();
        if (!mapUsed.Contains(location))
        {
            mapUsed[location] = true;
            mapFile.Append(location + Char.Tab + (line+1).ToString() + Char.EOL);
        }
    }
    PadOut(string text, int delta)
    {
        if (generating)
        {
            line++;
            if (text.Length != 0)
            {
                text = ("").Pad(' ', uint((int(BlockLevel)+int(extra)+delta) * 4)) + text;
            }
            if (capturing)
            {     
                String.Build(ref captured, text + Char.EOL);
            }
            else
            {
                codeFile.Append(text + Char.EOL);
            }
        }
    }
    
    
    Initialize(string path)
    {
        generating = true;
        string extension = Path.GetExtension(path);
        codePath = path.Replace(extension, ".asm");
        File.Delete(codePath);
        string mapPath = path.Replace(extension, ".map");
        File.Delete(mapPath);
        
        string name = Path.GetFileName(path);
        name = name.Replace(extension, "");
        name = name.ToUpper();
        
        tempCodePath = path.Replace(extension, ".tasm");
                
        codeFile = File.Create(tempCodePath);
        mapFile = File.Create(mapPath);
        
        PadOut("program " + name, 0);
    }
    Uses()
    {
        PadOut("#define TINY6502", 0);
        PadOut("#define W65C22_VIA", 0);
        
        
        //PadOut("#define ACIA_6850", 0);
        PadOut("#define APPLE_I", 0);
        PadOut("",0);
        PadOut("// PLACEHOLDER",0);
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
        PadOut("uses \"/Source/Languages/Tiny6502/TinyOps\"", 0);
        PadOut("uses \"/Source/Languages/Tiny6502/TinySys\"", 0);
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
    Append(string line)
    {
        PadOut(line, 0);
    }
    Flush()
    {
        codeFile.Flush();
        mapFile.Flush();
        
        file tfile = File.Open(tempCodePath);
        file cfile = File.Create(codePath);
        loop
        {
            string line = tfile.ReadLine();
            if (!tfile.IsValid()) { break; }
            if (line.Contains("// PLACEHOLDER"))
            {
                cfile.Append("    const string strConsts = {");
                string cstr = GetStringConstants(); 
                foreach (var c in cstr)
                {
                    cfile.Append("0x" + (byte(c)).ToHexString(2) +", ");
                }
                cfile.Append("};" + Char.EOL);
            }
            else
            {
                cfile.Append(line + Char.EOL);
            }
        }
        cfile.Flush();
        File.Delete(tempCodePath);
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
    IfExit(string comment, string condition)
    {
        PadOut("PLA", 0);
        PadOut("if (" + condition +") // " + comment, 0);
        PadOut("{", 0);
        PadOut("break;", 1);
        PadOut("}", 0);
    }
    Else()
    {
        PadOut("else", 0);
    }
    Loop(string comment)
    {
        PadOut("loop", 0);
        TinyConstant.EnterBlock();
        TinySymbols.EnterBlock(true, comment);
    }
    EndLoop(string comment)
    {
        TinySymbols.LeaveBlock(comment, true); // end loop
        TinyConstant.LeaveBlock();
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
    Continue()
    {
        PadOut("continue;", 0);
    }
    
    OfferSystemMethod(string methodName)
    {
        //PrintLn(methodName);
    }
    
    PushWord(uint word, string comment)
    {
        PadOut("", 0);
        PadOut("// PUSH 0x" + word.ToHexString(4), 0);
        PadOut("LDA # 0x" + (word.GetByte(0)).ToHexString(2) + " // " + comment + " LSB", 0);
        PadOut("PHA", 0);
        if (word.GetByte(0) != word.GetByte(1))
        {
            PadOut("LDA # 0x" + (word.GetByte(1)).ToHexString(2) + " // " + comment + " MSB", 0);
        }
        PadOut("PHA", 0);
    }
    PushByte(byte value, string comment)
    {
        PadOut("", 0);
        PadOut("// PUSH 0x" + value.ToHexString(2), 0);
        PadOut("LDA # 0x" + value.ToHexString(2) + " // " + comment, 0);
        PadOut("PHA", 0);
    }
    PushConst(uint word)
    {
        PadOut("", 0);
        PadOut("LDA # ((strConsts + 0x" + (word.GetByte(0)).ToHexString(2) + ") & 0xFF)", 0);
        PadOut("PHA", 0);
        PadOut("LDA # ((strConsts + 0x" + (word.GetByte(1)).ToHexString(2) + ") >> 8)", 0);
        PadOut("PHA", 0);
    }
    
    CastPad(bool doUnder)
    {
        if (doUnder)
        {
            PadOut("", 0);
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
    WordToByte(bool doUnder, string byteType)
    {
        if (doUnder)
        {
            Die(0x0A);
        }
        PadOut("", 0);
        PadOut("// 16 bit as '" + byteType + "'", 0);
        PadOut("PLA // MSB", 0);       
    }
    ToBool(bool isByte, bool doUnder)
    {
        if (doUnder)
        {
            Die(0x0A);
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
        PadOut("loop", 0);
        PadOut("{", 0);
        extra++;
        PadOut("// single exit loop", 0);
        PadOut("", 0);
    }
    Leave()
    {
        PadOut("", 0);
        PadOut("break; // single exit loop", 0);
        extra--;
        PadOut("}", 0); 
            
        TinySymbols.FreeAutomaticAllocations();
        TinyCode.PopBytes("local variable"); // method single exit loop
        
        PadOut("", 0);
        PadOut("// POP BP", 0);
        PadOut("PLX", 0);
        PadOut("STX ZP.BP", 0);
    }
    PopBytes(string comment)
    {
        byte bytes = GetCurrentLevelBytes();
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
                PadOut("PLY", 0); // leaving A and X for potential return values
            }
        }
    }
    offsetToX(int offset, bool isGlobal)
    {
        if (isGlobal)
        {
            offset = 255 - offset;
            PadOut("LDX # 0x"+ (offset.GetByte(0)).ToHexString(2), 0);
        }
        else
        {
            PadOut("LDA ZP.BP", 0);
            if (offset > 0)
            {
                // locals
                PadOut("SEC", 0);
                PadOut("SBC # 0x" + (offset.GetByte(0)).ToHexString(2), 0);
            }
            else if (offset < 0)
            {
                // arguments
                offset = -offset;
                offset += 3; // BP (1 byte) and return address (2 bytes)
                PadOut("CLC", 0);
                PadOut("ADC # 0x" + (offset.GetByte(0)).ToHexString(2), 0);
            }
            PadOut("TAX", 0);
        }
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
        PadOut("", 0);
        PadOut("// PUSH " + nameWithOffset(name, offset, isGlobal) + Bitness(isByte), 0);
        offsetToX(offset, isGlobal);  
        PadOut("LDA 0x0100, X", 0);  
        PadOut("PHA", 0);
        if (!isByte)
        {
            PadOut("DEX", 0);
            PadOut("LDA 0x0100, X", 0);  
            PadOut("PHA", 0);
        }
    }
    PopVariable(string name, int offset, bool isByte, bool isGlobal)
    {
        PadOut("", 0);
        PadOut("// POP " + nameWithOffset(name, offset, isGlobal) + Bitness(isByte), 0);
        offsetToX(offset, isGlobal);  
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
    Dup(bool isByte)
    {
        PadOut("", 0);
        PadOut("// dup" +  Bitness(isByte), 0);
        
        PadOut("PLA", 0);
        if (!isByte)
        {
            PadOut("PLX", 0);
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
            PadOut("STA TOPH", 0);
        }
        PadOut("PLA", 0);
        PadOut("STA TOPL", 0);
    }
    
    PostIncrement(string name, int offset, bool isByte, bool inc, bool isGlobal) // i++
    {
        PadOut("", 0);
        PadOut("// " + name + (inc ? "++" : "--") + Bitness(isByte), 0);
        offsetToX(offset, isGlobal);
        PadOut("LDA 0x0100, X", 0);
        PadOut("PHA", 0); // A contains pre-value
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
                PadOut("LDA # 0", 0);
                PadOut("CMP 0x0100, X", 0);
                PadOut("if (Z)", 0);
                PadOut("{", 0);
                PadOut("INX", 1);
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
                PadOut("INX", 1);
                PadOut("DEC 0x0100, X", 1);
                PadOut("}", 0);
            }
        }
        
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
                PadOut("INX", 1);
                PadOut("INC 0x0100, X", 1);
                PadOut("DEX", 1);
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
                PadOut("INX", 1);
                PadOut("DEC 0x0100, X", 1);
                PadOut("DEX", 1);
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
                functionName = "TinySys.WriteChar";
            }
            case "millis":
            {
                 functionName = "TinySys.Millis";
            }
            case "delay":
            {
                 functionName = "TinySys.Delay";
            }
            case "malloc":
            {
                 functionName = "TinySys.Malloc";
            }
            case "free":
            {
                 functionName = "TinySys.Free";
            }
        }
        PadOut(functionName + "();", 0);
    }
    ReadMemory(bool indexIsByte, bool dataIsByte)
    {
        PadOut("", 0);
        PadOut("// read memory" +  Bitness(dataIsByte), 0);
        if (indexIsByte)
        {
            PadOut("PLX", 0);
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
            PadOut("STA TOPH", 0);        
            PadOut("PLA", 0);        
            PadOut("STA TOPL", 0);
            PadOut("LDA [TOP]", 0);
            PadOut("PHA", 0);
            if (!dataIsByte)
            {
                PadOut("LDX # 1", 0); 
                PadOut("LDA [TOP], X", 0);
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
                PadOut("STA ACCH", 0);     
            }   
            PadOut("PLA", 0); 
            PadOut("PLX", 0);
            PadOut("STA 0x00, X", 0);
            if (!dataIsByte)
            {
                PadOut("INX", 0); 
                PadOut("LDA ACCH", 0);     
                PadOut("STA 0x00, X", 0);
            }   
        } 
        else
        {
            if (!dataIsByte)
            {
                PadOut("PLA", 0); 
                PadOut("STA ACCH", 0);     
            }   
            PadOut("PLA", 0); 
            PadOut("STA ACCL", 0);     
            PadOut("PLA", 0);        
            PadOut("STA TOPH", 0);        
            PadOut("PLA", 0);        
            PadOut("STA TOPL", 0);
            PadOut("LDA ACCL", 0);        
            PadOut("STA [TOP]", 0);
            if (!dataIsByte)
            {
                PadOut("LDA ACCH", 0);     
                PadOut("LDX # 1", 0); 
                PadOut("LDA [TOP], X", 0);
            }
        }
    }
}
