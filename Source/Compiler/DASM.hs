program DASM
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    
    uses "/Source/Compiler/JSON/JSON"
    uses "/Source/Compiler/JSON/Code"
    
    uses "/Source/Compiler/CodeGen/Instructions"
    
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Compiler/Tokens/SysCalls"
    uses "/Source/Compiler/Tokens/Parser"
    
    uint codeSize = 0;
    uint longJumps = 0;
    uint allLongs = 0;
    uint actualLongJumps = 0;
    uint instructionCount = 0;
    
    string pathLoaded;
    <string> sourceLines;
    
    string GetSourceLine(string path, string lnum)
    {
        if (pathLoaded != path)
        {
            file sourceFile = File.Open(path);
            if (sourceFile.IsValid())
            {
                sourceLines.Clear();
                pathLoaded = path;
                loop
                {
                    string ln = sourceFile.ReadLine();
                    if (ln.Length == 0)
                    {
                        if (!sourceFile.IsValid())
                        {
                            break;
                        }
                    }
                    sourceLines.Append(ln);
                }
            }
        }
        string sourceLine;
        uint iline;
        if (Token.TryParseUInt(lnum, ref iline))
        {
            if (iline > 0)
            {
                iline--;
            }
            if (sourceLines.Length > iline)
            {
                sourceLine = sourceLines[iline];
            }
        }
        return sourceLine;
    }
        
    DisassembleCode(file hasmFile, <byte> code, uint startAddress, <uint,string> methodNames)
    {
        <string,string> debugInfo;
        uint doffset = 0;
        string src;
        string srcName;
        string content;
        uint length = code.Length;
        for (uint address = 0; address < length; address++)
        {
            uint actualAddress = startAddress + address;
            
            if (methodNames.Contains(actualAddress))
            {
                hasmFile.Append(""  + char(0x0A));        
                    
                string methodIndex = methodNames[actualAddress];
                <string,variant> methodSymbols = Code.GetMethodSymbols(methodIndex);
                if (methodSymbols.Count > 0)
                {
                    src = methodSymbols["source"];
                    srcName = Path.GetFileName(src);
                    string ln = methodSymbols["line"];
                    string nm = methodSymbols["name"];
                    debugInfo = methodSymbols["debug"];
                    doffset = address;
                    hasmFile.Append("" + char(0x0A)); 
                    hasmFile.Append("// " + src + ":" + ln + char(0x0A));  
                    
                    string mname = "// ####  " + nm + "(..)  ####";
                    mname = mname.Pad(' ', 80);
                    mname = mname + methodIndex + char(0x0A);
                    hasmFile.Append(mname);  
                    
                    hasmFile.Append("" + char(0x0A)); 
                }
                else
                {
                    hasmFile.Append("// " + methodIndex + char(0x0A)); 
                }
                Parser.ProgressTick(".");
            }
            uint delta = (address-doffset);
            string debugAddress = delta.ToString();
            if (debugInfo.Contains(debugAddress))
            {
                string dln = debugInfo[debugAddress];
                hasmFile.Append("" + char(0x0A)); 
                string sourceLine = GetSourceLine(src, dln);
                if (sourceLine.Length > 0)
                {
                    sourceLine = "// " + sourceLine;
                    if (sourceLine.Length < 70)
                    {
                        sourceLine = sourceLine.Pad(' ', 80);
                        sourceLine = sourceLine + srcName + ":" + dln;
                    }
                    hasmFile.Append(sourceLine + char(0x0A));  
                }
                else
                {
                    hasmFile.Append("// " + src + ":" + dln + char(0x0A));  
                }
                
                hasmFile.Append("" + char(0x0A)); 
            }
            
            content = "0x" + actualAddress.ToHexString(4);
            byte cd = code[address];
            Instruction instruction = Instruction(cd);
            string opcode = Instructions.ToString(instruction);
            content = content + "  " + opcode;
            
            instructionCount++;
            
            byte operandWidth = Instructions.OperandWidth(instruction);
            byte iSysCall;
            
            string methodKey;
            bool isStackOffset = Instructions.OperandIsStackOffset(instruction);
            bool isJumpOffset  = Instructions.OperandIsAddressOffset(instruction);
            switch (operandWidth)
            {
                case 1:
                {
                    long jumpTarget = address;
                    address++;
                    byte op = code[address];
                    iSysCall = op;
                    string offsetString;
                    if (isStackOffset || isJumpOffset)
                    {
                        int offset = op;
                        if (offset > 127)
                        {
                            offset = offset - 256; // 255 -> -1
                        }
                        string sign;
                        if (offset >= 0)
                        {
                            offsetString = "+";
                        }
                        offsetString = offsetString + offset.ToString();
                        jumpTarget = jumpTarget + offset + long(startAddress);
                    }
                    if (isJumpOffset)
                    {
                        content = content + " 0x" + jumpTarget.ToHexString(4);
                        content = content + " (" + offsetString + ")";
                    }
                    else if (isStackOffset)
                    {
                        content = content + " 0x" + op.ToHexString(2);
                        content = content + " (BP" + offsetString + ")";
                    }
                    else
                    {
                        methodKey = "0x" + op.ToHexString(4);
                        content = content + " 0x" + op.ToHexString(2);
                    }
                }
                case 2:
                {
                    long jumpTarget = address;
                    address++;
                    byte lsb = code[address];
                    address++;
                    byte msb = code[address];
                    uint op = lsb + (msb << 8);
                    string offsetString;
                    allLongs++;
                    if (isStackOffset || isJumpOffset)
                    {
                        long offset = op;
                        if (offset > 32767)
                        {
                            offset = offset - 65536; // 0x10000 -> -1
                        }
                        string sign;
                        if (offset >= 0)
                        {
                            offsetString = "+";
                        }
                        offsetString = offsetString + offset.ToString();
                        jumpTarget = jumpTarget + offset + long(startAddress);
                    }
                    if (isJumpOffset)
                    {
                        content = content + " 0x" + jumpTarget.ToHexString(4);
                        content = content + " (" + offsetString + ")";
                        longJumps++;
                        long offset = op;
                        if ((offset > 127) || (offset < -128))
                        {
                            actualLongJumps++;
                        }
                    }
                    else if (isStackOffset)
                    {
                        content = content + " 0x" + op.ToHexString(4);
                        content = content + " (BP" + offsetString  + ")";
                    }
                    else
                    {
                        methodKey = "0x" + op.ToHexString(4);
                        content = content + " " + methodKey;
                    }
                }
            }
            if ((instruction == Instruction.SYSCALL)
             || (instruction == Instruction.SYSCALL0)
             || (instruction == Instruction.SYSCALL1)
               )
            {
                string syscallName = SysCalls.GetSysCallName(iSysCall);
                content = content + "  // " + syscallName;
            }
            if ((instruction == Instruction.CALLB)
             || (instruction == Instruction.CALLW)
               )
            {
                <string,variant> methodSymbols =  Code.GetMethodSymbols(methodKey);
                if (methodSymbols.Count > 0)
                {
                    string name = methodSymbols["name"];
                    if (instruction == Instruction.CALLB)
                    {
                        content = content + "  ";
                    }
                    content = content + "   // " + name;
                }
            }
            
            content = content + char(0x0A);
            hasmFile.Append(content);
        }
    }
    
    DisassembleData(file hasmFile, <byte> code, uint startAddress)
    {
        string content;
        uint length = code.Length;
        string literal;
        for (uint address = 0; address < length; address++)
        {
            if ((address % 16) == 0)
            {
                uint actualAddress = startAddress + address;
                content = char(0x0A) + "0x" + actualAddress.ToHexString(4) + "  ";
                hasmFile.Append(content);
                literal = "";
            }
            byte cd = code[address];
            content = "0x" + cd.ToHexString(2) + " ";
            if ((address % 16) == 7)
            {
                content = content + " ";
            }
            if ((cd > 31) && (cd < 128))
            {
                literal = literal + char(cd);
            }
            else
            {
                literal = literal + ".";
            }
            if ((address % 16) == 15)
            {
                content = content + " " + literal;
                literal = "";
            }
            hasmFile.Append(content);
        }
        content = "";
        if (literal.Length > 0)
        {
            uint padding = 16 - literal.Length;
            padding = padding * 5 + 1;
            content = content.Pad(' ', padding);
            content = content + literal;
        }
        content = content + char(0x0A);
        hasmFile.Append(content);
    }
    
    EmitC()
    {
        PrintLn("POP2PUSH1   " + (byte(Instruction.POP2PUSH1)).ToHexString(2));
        PrintLn("BYTEOPERAND " + (byte(Instruction.BYTEOPERAND)).ToHexString(2));
        PrintLn("WORDOPERAND " + (byte(Instruction.WORDOPERAND)).ToHexString(2));
        PrintLn("NOOPERAND   " + (byte(Instruction.NOOPERAND)).ToHexString(2));
        
        string cPath = "/Debug/syscalls.h";
        
        File.Delete(cPath);
        file cFile = File.Create(cPath);
        cFile.Append("enum SysCall" + char(0x0A));
        cFile.Append("{" + char(0x0A));
        <string,byte> syscalls = SysCalls.GetSysCalls();
        for (uint i=0; i < 256; i++)
        {
            foreach(var kv in syscalls)
            {
                if (kv.value == i)
                {
                    string name = kv.key;
                    name = name.Replace(".", "");
                    name = name.Replace("_", "");
                    cFile.Append("    " + name + " = 0x" + i.ToHexString(2) + "," + char(0x0A));        
                }
            }
        }
        cFile.Append("};" + char(0x0A));
        cFile.Flush();
    }
    
    BadArguments()
    {
        PrintLn("Invalid arguments for DASM:");
        PrintLn("  DASM <hexe file>");
        PrintLn("    -g : called from GUI, not console");
    }
    {
        SysCalls.New();
        
        //EmitC();
        
        loop
        {
            <string> rawArgs = System.Arguments;
            <string> args;
            foreach (var arg in rawArgs)
            {
                if ((arg.Length == 2) && (arg[0] == '-'))
                {
                    arg = arg.ToLower();
                    switch (arg)
                    {
                        case "-g":
                        {
                            Parser.SetInteractive(true);    
                        }
                        default:
                        {
                            args.Clear();
                            break;
                        }
                    }
                }
                else
                {
                    args.Append(arg);
                }
            }
          
            if (args.Length != 1)
            {
                BadArguments();
                break;
            }
            string ext;
            string codePath = args[0];
            if (!File.Exists(codePath))
            {
                string rawCodePath = codePath;
                ext = Path.GetExtension(rawCodePath);
                if (ext == ".")
                {
                    codePath = rawCodePath + ".hexe2";
                    if (!File.Exists(codePath))
                    {
                        BadArguments();
                        break;
                    }
                }
            }
            ext = Path.GetExtension(codePath);
            
            long startTime = Millis;
            loop
            {
                string extension = Path.GetExtension(codePath);
                string hasmPath  = codePath.Replace(extension, ".hasm2");
                hasmPath = Path.GetFileName(hasmPath);
                hasmPath = Path.Combine("/Debug/", hasmPath);
                File.Delete(hasmPath);

                file hasmFile = File.Create(hasmPath);
                if (!hasmFile.IsValid())
                {
                    PrintLn("Failed to create '" + hasmPath + "'");
                    break;
                }
                
                string symbolsPath  = codePath.Replace(extension, ".code");
                symbolsPath = Path.GetFileName(symbolsPath);
                symbolsPath = Path.Combine("/Debug/Obj", symbolsPath);
                
                if (File.Exists(symbolsPath))
                {
                    if (!ParseCode(symbolsPath, false, true))
                    {
                        break;
                    }
                }
                
                uint address = 0;
                file binFile = File.Open(codePath);
                
                byte lsb = binFile.Read();
                if (!binFile.IsValid())
                {
                    break;
                }
                byte msb = binFile.Read();
                if (!binFile.IsValid())
                {
                    break;
                }
                uint version = lsb + (msb << 8);
                hasmFile.Append("0x" + address.ToHexString(4) + " 0x" + version.ToHexString(4) + " // binary version number" + char(0x0A));
                address = address + 2;
                                    
                lsb = binFile.Read();
                if (!binFile.IsValid())
                {
                    break;
                }
                msb = binFile.Read();
                if (!binFile.IsValid())
                {
                    break;
                }
                uint constOffset = lsb + (msb << 8);
                hasmFile.Append("0x" + address.ToHexString(4) + " 0x" + constOffset.ToHexString(4) + " // offset to constant data" + char(0x0A));
                address = address + 2;
                
                lsb = binFile.Read();
                if (!binFile.IsValid())
                {
                    break;
                }
                msb = binFile.Read();
                if (!binFile.IsValid())
                {
                    break;
                }
                
                uint codeOffset = lsb + (msb << 8);
                hasmFile.Append("0x" + address.ToHexString(4) + " 0x" + codeOffset.ToHexString(4) + " // 'main' entry point offset" + char(0x0A));
                address = address + 2;
                
                codeSize = codeSize + 6;
                
                // read the method table
                <uint,string> methodNames;
                methodNames[constOffset] = "constant data";
                methodNames[codeOffset] = "main";
                
                uint constLength = codeOffset - constOffset;
                
                uint tableEntries = (constOffset - 6) / 4;
                if (tableEntries > 0)
                {
                    hasmFile.Append(""  + char(0x0A));
                    for (uint i=0; i < tableEntries; i++)
                    {
                        lsb = binFile.Read();
                        if (!binFile.IsValid())
                        {
                            break;
                        }
                        msb = binFile.Read();
                        if (!binFile.IsValid())
                        {
                            break;
                        }
                        
                        uint methodIndex = lsb + (msb << 8);
                        lsb = binFile.Read();
                        if (!binFile.IsValid())
                        {
                            break;
                        }
                        msb = binFile.Read();
                        if (!binFile.IsValid())
                        {
                            break;
                        }
                        
                        uint methodOffset = lsb + (msb << 8);
                        
                        hasmFile.Append("0x" + address.ToHexString(4) 
                                     + " 0x" + methodIndex.ToHexString(4)
                                     + " 0x" + methodOffset.ToHexString(4) + char(0x0A));
                        codeSize = codeSize + 4;
                        address = address + 4;
                        methodNames[methodOffset] = "0x" + methodIndex.ToHexString(4);
                    }
                    Parser.ProgressTick(".");
                }
                if (!binFile.IsValid())
                {
                    break;
                }
                <byte> code;
                if (constLength > 0)
                {
                    if (methodNames.Contains(address))
                    {
                        string name = methodNames[address];
                        hasmFile.Append(""  + char(0x0A));        
                        hasmFile.Append("// " + name + "  [" + constLength.ToString() +"]" +char(0x0A));            
                    }
                    loop
                    {
                        byte b = binFile.Read();
                        if (!binFile.IsValid())
                        {
                            break;
                        }
                        code.Append(b);
                        constLength--;
                        if (constLength == 0)
                        {
                            break;
                        }   
                    }
                    DisassembleData(hasmFile, code, address);
                    address = address + code.Length;
                    codeSize = codeSize + code.Length;
                    Parser.ProgressTick(".");
                }
                code.Clear();
                loop
                {
                    byte b = binFile.Read();
                    if (!binFile.IsValid())
                    {
                        break;
                    }
                    code.Append(b);
                }
                DisassembleCode(hasmFile, code, address, methodNames);
                codeSize = codeSize + code.Length;
                Parser.ProgressTick(".");
                hasmFile.Flush();
                if (!Parser.IsInteractive())
                {
                    PrintLn();
                    Print("Success, " + codeSize.ToString() + " bytes, ", Color.DarkGreen, Color.LightGray);
                    long elapsedTime = Millis - startTime;
                    float seconds = elapsedTime / 1000.0;
                    PrintLn("  " + seconds.ToString() +"s", Color.MatrixBlue, Color.LightGray);
                    PrintLn("  " + instructionCount.ToString() + " instructions disassembled. ", Color.DarkGreen, Color.LightGray);
                    //PrintLn("  actualLongJumps=" + actualLongJumps.ToString() 
                    //      + "  longJumps=" + longJumps.ToString()
                    //      + "  allLongs=" + allLongs.ToString()
                    //);
                }
                else
                {
                    Parser.ProgressDone();
                }
                break;
            }
            break;
        }
    }
}
