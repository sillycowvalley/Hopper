program DASM
{
    #define JSON_EXPRESS // .code and .json are generated by us so assume .json files have no errors
    #define ASSEMBLER
    
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    
    uses "JSON/JSON"
    uses "JSON/Code"
    
    uses "CODEGEN/AsmStream"
    
    uses "Tokens/Token"
    uses "Tokens/Scanner"
    uses "Tokens/SysCalls"
    uses "Tokens/LibCalls"
    uses "Tokens/Parser"
    
    bool extendedCodeSegment;
    
    uint codeSize = 0;
    uint instructionCount = 0;
    
    string pathLoaded;
    <string> sourceLines;
    string getSourceLine(string path, string lnum)
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
    
    <byte> readIHex(file hexFile)
    {
        <byte> code;
        loop
        {
            string ln = hexFile.ReadLine();
            if (!hexFile.IsValid()) { break; }
            string len = ln.Substring(1,2);
            uint length;
            _ = UInt.TryParse("0x" + len, ref length);
            if (length == 0) { continue; }
            ln = ln.Substring(9);
            while (length > 0)
            {
                string br = ln.Substring(0, 2);
                ln = ln.Substring(2);
                uint b;
                _ = UInt.TryParse("0x" + br, ref b);
                code.Append(byte(b));
                length--;
            }
        }
        return code;
    }
    
    BadArguments()
    {
        PrintLn("Invalid arguments for 65DASM:");
        PrintLn("  65DASM <hexe file>");
        PrintLn("    -g <c> <r> : called from GUI, not console");
    }
    
    {
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
            string ext = ".hex";
            string codePath = args[0];
            if (!File.Exists(ref codePath, ref ext, "/Bin/"))
            {
                BadArguments();
            }
            
            long startTime = Millis;
            loop
            {
                string extension = Path.GetExtension(codePath);
                string hasmPath  = codePath.Replace(extension, ".lst");
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
                file hexFile = File.Open(codePath);
                <byte> code = readIHex(hexFile);
                
                if (code.Count < 4)
                {
                    break;
                }
                byte version = code[0];
                byte arch    = code[1];
                uint org     = code[2] + code[3] << 8;
                
                Architecture = CPUArchitecture(arch);
                
                address = org;
                
                hasmFile.Append("0x" + address.ToHexString(4) + "  0x" + version.ToHexString(2) + "   // binary version number" + char(0x0A));
                address++;
                hasmFile.Append("0x" + address.ToHexString(4) + "  0x" + arch.ToHexString(2) + "   // CPU Architecture" + char(0x0A));
                address++;
                hasmFile.Append("0x" + address.ToHexString(4) + "  0x" + org.ToHexString(4) + " // ROM location" + char(0x0A));
                address += 2;
                                    
                uint iOverload = 0;
                
                <uint, uint> methodSizes = Code.GetMethodSizes();
                uint indexMax = 0;
                foreach (var sz in methodSizes)
                {
                    if (sz.key > indexMax)
                    {
                        indexMax = sz.key;
                    }
                }
                <uint,uint> methodAddresses; // <address,index>
                uint methodAddress = address;
                for (uint index = 0; index <= indexMax; index++)
                {
                    if (!methodSizes.Contains(index)) { continue; }   
                    uint methodSize = methodSizes[index];
                    methodAddresses[methodAddress] = index;
                    methodAddress += methodSize;
                }
                
                <string,string> debugInfo;
                uint doffset = 0;
                string src;
                string srcName;
                
                uint index = 4;
                loop
                {
                    if (index == code.Count-6) { break; }
                    byte instruction = code[index];
                    byte length      = AsmStream.GetInstructionLength(instruction);
                    
                    codeSize += length;
                    instructionCount++;
                    
                    uint operand;
                    string operandString;
                    if (length == 2)
                    {
                        operand = code[index+1];
                    }
                    else if (length == 3)
                    {
                        operand = code[index+1] + code[index+2] << 8; 
                    }
                    if (methodAddresses.Contains(address))
                    {
                        uint methodIndex = methodAddresses[address];
                        <string,variant> methodSymbols = Code.GetMethodSymbols(methodIndex);
                        if (methodSymbols.Count != 0)
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
                            mname = mname + "0x" + methodIndex.ToHexString(4) + char(0x0A);
                            hasmFile.Append(mname);  
                            hasmFile.Append("" + char(0x0A)); 
                            Parser.ProgressTick(".");
                        }
                    }
                    uint delta = (address-doffset);
                    string debugAddress = delta.ToString();
                    string comment;
                    if (debugInfo.Contains(debugAddress))
                    {
                        string debugLine = debugInfo[debugAddress];
                        string sourceLine = getSourceLine(src, debugLine);
                        if (sourceLine.Length != 0)
                        {
                            comment = "// " + sourceLine.Trim();
                            if (comment.Length < 34)
                            {
                                comment = comment.Pad(' ', 34);
                                comment = comment + srcName + ":" + debugLine;
                            }
                        }
                        else
                        {
                            comment = "// " + src + ":" + debugLine;  
                        }
                    }
                    
                    string disassembly = AsmStream.Disassemble(address, instruction, operand);
                    hasmFile.Append(disassembly.Pad(' ', 48) + comment + char(0x0A));
                    
                    index += length;
                    address += length;
                }
                hasmFile.Append("" +  char(0x0A));
                uint vector = code[index] + code[index+1] << 8;
                hasmFile.Append("0xFFFA 0x" + vector.ToHexString(4) + " // NMI vector" + char(0x0A));
                vector = code[index+2] + code[index+3] << 8;
                hasmFile.Append("0xFFFC 0x" + vector.ToHexString(4) + " // Reset vector" + char(0x0A));
                vector = code[index+4] + code[index+5] << 8;
                hasmFile.Append("0xFFFE 0x" + vector.ToHexString(4) + " // IRQ vector" + char(0x0A));
                              
                Parser.ProgressTick(".");
                hasmFile.Flush();
                if (!Parser.IsInteractive())
                {
                    PrintLn();
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
