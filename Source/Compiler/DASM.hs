program DASM
{
    #define JSON_EXPRESS // .code and .json are generated by us so assume .json files have no errors
    
    //#define EXPERIMENTAL

    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    
    uses "JSON/JSON"
    uses "JSON/Code"
    
    uses "CodeGen/Instructions"
    
    uses "Tokens/Token"
    uses "Tokens/Scanner"
    uses "Tokens/SysCalls"
    uses "Tokens/LibCalls"
    uses "Tokens/Parser"
    
    uint codeSize = 0;
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
        if (UInt.TryParse(lnum, ref iline))
        {
            if (iline > 0)
            {
                iline--;
            }
            if (sourceLines.Count > iline)
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
        uint length = code.Count;

        uint progressCount;
        string methodIndex;
        
        for (uint address = 0; address < length; address++)
        {
            uint actualAddress = startAddress + address;
            
            if (methodNames.Contains(actualAddress))
            {
                hasmFile.Append(""  + Char.EOL);        
                    
                methodIndex = methodNames[actualAddress];
                <string,variant> methodSymbols = Code.GetMethodSymbols(methodIndex);
                if (methodSymbols.Count != 0)
                {
                    src = methodSymbols["source"];
                    srcName = Path.GetFileName(src);
                    string ln = methodSymbols["line"];
                    string nm = methodSymbols["name"];
                    debugInfo = methodSymbols["debug"];
                    doffset = address;
                    hasmFile.Append("" + Char.EOL); 
                    hasmFile.Append("// " + src + ":" + ln + Char.EOL);  
                    
                    string returnType = Instructions.RenderReturnType(methodIndex);
                    string arguments  = Instructions.RenderArguments(methodIndex);
                    string mname = "// ####   '" + returnType + nm + "(" + arguments + ")'   ####";
                    mname = mname.Pad(' ', 90);
                    mname = mname + methodIndex + Char.EOL;
                    hasmFile.Append(mname);  
                    
                    hasmFile.Append("" + Char.EOL); 
                }
                else
                {
                    hasmFile.Append("// " + methodIndex + Char.EOL); 
                }
                if (progressCount % 64 == 0)
                {
                    Parser.ProgressTick("d"); // dasm
                }
                progressCount++;
            }
            uint delta = (address-doffset);
            string debugAddress = delta.ToString();
            if (debugInfo.Contains(debugAddress))
            {
                string debugLine = debugInfo[debugAddress];
                hasmFile.Append("" + Char.EOL); 
                string sourceLine = GetSourceLine(src, debugLine);
                if (sourceLine.Length != 0)
                {
                    string commentStart;
                    commentStart = commentStart.Pad(' ', 24) + "//";
                    sourceLine = commentStart + " " + sourceLine.Trim();
                    if (sourceLine.Length < 80)
                    {
                        sourceLine = sourceLine.Pad(' ', 90);
                        sourceLine = sourceLine + srcName + ":" + debugLine;
                    }
                    hasmFile.Append(sourceLine + Char.EOL);  
                }
                else
                {
                    hasmFile.Append("// " + src + ":" + debugLine + Char.EOL);  
                }
                hasmFile.Append("" + Char.EOL); 
            }
            <uint> jumpTargets;
            <uint> jixLabels;
            
            content = Instructions.DisassembleHopper(code, ref address, long(startAddress), ref jumpTargets, ref jixLabels, false);
            instructionCount++;
            String.Build(ref content, Char.EOL);
            hasmFile.Append(content);
        } // for (address..
    }
    
    DisassembleData(file hasmFile, <byte> code, uint startAddress)
    {
        string content;
        uint length = code.Count;
        string literal;
        for (uint address = 0; address < length; address++)
        {
            if ((address % 16) == 0)
            {
                uint actualAddress = startAddress + address;
                content = Char.EOL + "0x" + actualAddress.ToHexString(4) + "  ";
                hasmFile.Append(content);
                literal = "";
            }
            byte cd = code[address];
            content = "0x" + cd.ToHexString(2) + " ";
            if ((address % 16) == 7)
            {
                content = content + " ";
            }
            if ((cd > 31) && (cd < 127))
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
        if (literal.Length != 0)
        {
            uint padding = 16 - literal.Length;
            padding = padding * 5 + 1;
            content = content.Pad(' ', padding);
            content = content + literal;
        }
        content = content + Char.EOL;
        hasmFile.Append(content);
    }
    
    BadArguments()
    {
        PrintLn("Invalid arguments for DASM:");
        PrintLn("  DASM <hexe file>");
        PrintLn("    -g <c> <r> : called from GUI, not console");
    }
    
    {
        SysCalls.New();
        LibCalls.New();
        
        bool success = false;
        loop
        {
            <string> rawArgs = System.Arguments;
            <string> args;
            for (uint iArg = 0; iArg < rawArgs.Count; iArg++)
            {
                string arg = rawArgs[iArg];
                if ((arg.Length == 2) && (arg[0] == '-'))
                {
                    arg = arg.ToLower();
                    switch (arg)
                    {
                        case "-g":
                        {
                            uint col;
                            uint row;
                            iArg++;
                            if (UInt.TryParse(rawArgs[iArg], ref col))
                            {
                            }
                            iArg++;
                            if (UInt.TryParse(rawArgs[iArg], ref row))
                            {
                            }
                            Parser.SetInteractive(byte(col), byte(row));
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
          
            if (args.Count != 1)
            {
                BadArguments();
                break;
            }
            string ext = HexeExtension;
            string hexePath = args[0];
            if (!File.Exists(ref hexePath, ref ext, "/Bin/"))
            {
                BadArguments();
            }
            
            long startTime = Millis;
            loop
            {
                string extension = Path.GetExtension(hexePath);
                string hasmPath  = hexePath.Replace(extension, HasmExtension);
                hasmPath = Path.GetFileName(hasmPath);
                hasmPath = Path.Combine("/Debug/", hasmPath);
                File.Delete(hasmPath);

                file hasmFile = File.Create(hasmPath);
                if (!hasmFile.IsValid())
                {
                    PrintLn("Failed to create '" + hasmPath + "'");
                    break;
                }
                
                string codePath  = hexePath.Replace(extension, ".code");
                codePath = Path.GetFileName(codePath);
                codePath = Path.Combine("/Debug/Obj", codePath);
                if (File.Exists(codePath))
                {
                    if (!ParseCode(codePath, false, true))
                    {
                        break;
                    }
                }
                
                uint address = 0;
                file binFile = File.Open(hexePath);
                
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
                //extendedCodeSegment = (version & 0x0001) != 0;
                //flatStack           = (version & 0x0002) != 0;

                hasmFile.Append("0x" + address.ToHexString(4) + " 0x" + version.ToHexString(4) + " // binary version number" + Char.EOL);
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
                hasmFile.Append("0x" + address.ToHexString(4) + " 0x" + constOffset.ToHexString(4) + " // offset to constant data" + Char.EOL);
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
                hasmFile.Append("0x" + address.ToHexString(4) + " 0x" + codeOffset.ToHexString(4) + " // 'Hopper' entry point offset" + Char.EOL);
                address = address + 2;
                
                codeSize = codeSize + 6;
                
                // read the method table
                <uint,string> methodNames;
                if (constOffset != codeOffset)
                {
                    methodNames[constOffset] = "constant data";
                }
                methodNames[0] = "0x0000";
                
                uint constLength = codeOffset - constOffset;
                
                uint tableEntries = (constOffset - 6) / 4;
                if (tableEntries > 0)
                {
                    hasmFile.Append(""  + Char.EOL);
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
                                     + " 0x" + methodOffset.ToHexString(4) + Char.EOL);
                        codeSize = codeSize + 4;
                        address = address + 4;
                        methodNames[methodOffset] = "0x" + methodIndex.ToHexString(4);
                    }
                    Parser.ProgressTick("d"); // dasm
                }
                if (!binFile.IsValid())
                {
                    break;
                }
                <byte> code;
                if (constLength != 0)
                {
                    if (methodNames.Contains(address))
                    {
                        string name = methodNames[address];
                        hasmFile.Append(""  + Char.EOL);        
                        hasmFile.Append("// " + name + "  [" + constLength.ToString() +"]" +Char.EOL);            
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
                    address = address   + code.Count;
                    codeSize = codeSize + code.Count;
                    Parser.ProgressTick("d"); // dasm
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
                address = 0;
                DisassembleCode(hasmFile, code, address, methodNames);
                codeSize = codeSize + code.Count;
                Parser.ProgressTick("d"); // dasm
                hasmFile.Flush();
                if (!Parser.IsInteractive())
                {
                    PrintLn();
                    codeSize -= codeOffset;
                    Print("Success, " + codeSize.ToString() + " bytes of code, ", Colour.ProgressText, Colour.ProgressFace);
                    long elapsedTime = Millis - startTime;
                    float seconds = elapsedTime / 1000.0;
                    PrintLn("  " + seconds.ToString() +"s", Colour.ProgressHighlight, Colour.ProgressFace);
                    PrintLn("  " + instructionCount.ToString() + " instructions disassembled. ", Colour.ProgressText, Colour.ProgressFace);
                }
                else
                {
                    Parser.ProgressDone();
                }
                success = true;
                break;
            }
            break;
        }
        if (!success)
        {
            Diagnostics.SetError(0x0E);
        }
    }
}
