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
    
    // This is where you define the location and limit of global variables:
    
    const uint globalZeroPageStart = 0x88;
    const uint globalZeroPageLimit = (0xEB - globalZeroPageStart + 1);
    
    const uint globalAreaStart = 0x0300;
    const uint globalAreaLimit = 256;
    
    bool useZeroPageGlobals = false; // #define ZEROPAGEGLOBALS in .tc source will also define this as true
    
    uint globalStart = globalAreaStart;
    uint globalLimit = globalAreaLimit;
    uint argumentReserve = 0;
    uint ArgumentReserve { get { return argumentReserve; } set { argumentReserve = value; } }
    
    bool ZeroPageGlobals 
    { 
        get { return useZeroPageGlobals; } 
        set 
        { 
            useZeroPageGlobals = value; 
            globalStart = value ? globalZeroPageStart : globalAreaStart;
            globalLimit = value ? globalZeroPageLimit : globalAreaLimit;
        } 
    }
    uint GlobalLimit { get { return globalLimit; } }
    uint GlobalStart { get { return globalStart; } }
    
    bool generating;
    bool Generating { get { return generating; } set { generating = value; } }
    
    bool inStreamMode;
    bool InStreamMode { get { return inStreamMode; } set { inStreamMode = value; } }
    
    int GlobalMSB(int offset)
    {
        if (offset >= 0)
        {
            return offset+1;
        }
        else
        {
            return offset-1; // static argument
        }
    }
    string GlobalOperand(int offset)
    {
        uint address = globalStart;
        if (offset >= 0)
        {
            address += ArgumentReserve;
        }
        else
        {
            offset = (-offset) - 1;
        }
        address = address + UInt.FromBytes(offset.GetByte(0), offset.GetByte(1));
        string operand;
        if (ZeroPageGlobals)
        {
            operand = "0x" + address.ToHexString(2);
        }
        else
        {
            operand = "0x" + address.ToHexString(4);
        }
        return operand;
    }
    
    PadOut(string text, int delta)
    {
        if (InStreamMode)
        {
            Die(0x0B);
        }
        if (!FirstPass && Compiling && generating)
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
        
        if (!FirstPass)
        {
            string name = Path.GetFileName(path);
            name = name.Replace(extension, "");
            name = name.ToUpper();
            
            codeFile = File.Create(codePath);
            TCSymbols.ExportFunctionTable();
            PadOut("program " + name, 0);
        }
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
        PadOut("#define I2C", 0);
        if (!IsDefined("ACIA_6850") && !IsDefined("APPLE_I"))
        {
            PadOut("#define ACIA_6850", 0);                            // ZP.ACIACONTROL, ZP.ACIADATA, ZP.ACIASTATUS
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
        PadOut("uses \"/Source/Runtime/6502/IntMath\"", 0);            // FP.UWIDE0, FP.UWIDE1, FP.UWIDE2, FP.UWIDE3
        if (HeapRequired)
        {
            PadOut("uses \"/Source/Runtime/6502/Memory\"", 0);         // ZP.FREELISTL, ZP.FREELISTH, ZP.HEAPSTART, ZP.HEAPSIZE, ZP.PROGSIZE
            PadOut("uses \"/Source/Runtime/6502/Allocate\"", 0);       // ZP.M0 .. ZP.M13
            PadOut("uses \"/Source/Runtime/6502/Free\"", 0);           // ZP.M0 .. ZP.M15, ZP.IDYL, ZP.IDYL
        }
        PadOut("uses \"/Source/Runtime/6502/Serial\"", 0);         // ZP.SerialInWritePointer, ZP.SerialInReadPointer, ZP.SerialBreakFlag, 
        
        PadOut("uses \"/Source/Runtime/6502/Time\"", 0);           // ZP.TICK0, ZP.TICK1, ZP.TICK2, ZP.TICK3, ZP.TARGET0, ZP.TARGET1, ZP.TARGET2, ZP.TARGET3
        PadOut("uses \"/Source/Runtime/6502/Devices/W65C22\"", 0); // ZP.DDRA, ZP.DDRB, ZP.T1CL, ZP.T1CH, ZP.IER, ZP.ACR, ZP.IFR, ZF.PORTA, ZFPORTB
        PadOut("uses \"/Source/Runtime/6502/I2C\"", 0);
        PadOut("uses \"/Source/Runtime/6502/Devices/SerialEEPROM\"", 0);

        PadOut("uses \"/Source/Languages/TiggerC/TCOps\"", 0);         // ZP.TOPL, ZP.TOPH, ZP.TOPT, ZP.NEXTL, ZP.NEXTH, ZP.ACCL, ZP.ACCH, ZP.FSIGN
        PadOut("uses \"/Source/Languages/TiggerC/TCSys\"", 0);         // ZP.IDXL, ZP.IDXH
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
        if (HeapRequired)
        {
            PadOut("Memory.InitializeHeapSize();",0);
        }
        PadOut("Serial.Initialize();", 0);
        PadOut("W65C22.Initialize();", 0);
        PadOut("",0);
    }
    Flush()
    {
        if (!FirstPass)
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
    /*
    If(string comment)
    {
        PadOut("", 0);
        PadOut("PLA // bool", 0); // bool so one byte
        PadOut("if (NZ) // " + comment, 0);
    }
    */
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
        PadOut("PLX", 0);
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
                TCGen.PushCastZero();
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
        PadOut("", 0);
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
        PadOut("PLX", 0);
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
                PadOut("PLY", 0); // leaving A and X for potential return values
            }
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
        if (desired == 0)
        {
            PadOut("LD" + register + " ZP.BP", 0);
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
            PadOut("TA" + register, 0);
        }
    }
    BPOffset(int offset, string register, bool isByte)
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
        
        if (!isByte)
        {
            desired--; // MSB offset
        }
        
        if (desired == 0)
        {
            PadOut("LD" + register + " ZP.BP", 0);
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
            PadOut("TA" + register, 0);
        }
    }
    string nameWithOffset(string name, int offset, bool isGlobal)
    {
        if (isGlobal)
        {
            return name + " [" + GlobalOperand(offset) + "]";
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
            PadOut("LDA " + GlobalOperand(offset), 0);  
            PadOut("PHA", 0);
            if (!isByte)
            {
                PadOut("LDA " + GlobalOperand(GlobalMSB(offset)), 0);  
                PadOut("PHA", 0);
            }
        }
        else
        {
            if (isByte)
            {
                BPOffset(offset, "X", isByte); // LSB offset 
                PadOut("LDA 0x0100, X", 0);  
                PadOut("PHA", 0);
            }
            else
            {
                BPOffset(offset, "X", isByte); // MSB offset
                PadOut("LDA 0x0101, X // LSB", 0);  
                PadOut("PHA", 0);
                PadOut("LDA 0x0100, X // MSB", 0);  
                PadOut("PHA", 0);
            }
        }
    }
    PopVariable(string name, int offset, bool isByte, bool isGlobal)
    {
        PadOut("// POP " + nameWithOffset(name, offset, isGlobal) + Bitness(isByte), 0);
        if (isGlobal)
        {
            if (!isByte)
            {
                PadOut("PLA", 0);
                PadOut("STA " + GlobalOperand(GlobalMSB(offset)), 0);  
            }
            PadOut("PLA", 0);
            PadOut("STA " + GlobalOperand(offset), 0);  
        }
        else
        {
            if (isByte)
            {
                BPOffset(offset, "X", isByte); // LSB offset
                PadOut("PLA", 0);
                PadOut("STA 0x0100, X", 0);  
            }
            else
            {
                BPOffset(offset, "X", isByte); // MSB offset
                PadOut("PLA", 0);
                PadOut("STA 0x0100, X // MSB", 0);  
                PadOut("PLA", 0);
                PadOut("STA 0x0101, X // LSB", 0);  
            }
        }
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
        if (FirstPass)
        {
            TCSymbols.AddFunctionCall(CurrentFunction, functionName);
        }
        switch (functionName)
        {
            case "putc":
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
            case "heapfree":
            {
                functionName = "TCSys.Available";
            }
            case "heapmax":
            {
                functionName = "TCSys.Maximum";
            }
            case "i2cScan":
            {
                functionName = "TCSys.I2CScan";
            }
            case "writePage":
            {
                functionName = "TCSys.WritePage";
            }
            case "readPage":
            {
                functionName = "TCSys.ReadPage";
            }
            
        }
        PadOut(functionName + "();", 0);
    }
}
