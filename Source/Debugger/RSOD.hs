program  RedScreenOfDeath
{
    
    // #define JSONEXPRESS // .code and .json are generated by us so assume .json files have no errors
    
    bool extendedCodeSegment;
    uint codeSize = 0;
    <uint> stackAddresses;
    <string> stackLines;
    string errorMessage;
    
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    
    uses "/Source/Compiler/JSON/JSON"
    uses "/Source/Compiler/JSON/Code"
    
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
            if (sourceLines.Length > iline)
            {
                sourceLine = sourceLines[iline];
            }
        }
        return sourceLine;
    }
    
    DisassembleCode(<byte> code, uint startAddress, <uint,string> methodNames)
    {
        <string,string> debugInfo;
        uint doffset = 0;
        string src;
        string srcName;
        string content;
        string sourceLine;
        string methodName;
        uint length = code.Length;
        for (uint address = 0; address < length; address++)
        {
            uint actualAddress = startAddress + address;
            if (methodNames.Contains(actualAddress))
            {
                string methodIndex = methodNames[actualAddress];
                <string,variant> methodSymbols = Code.GetMethodSymbols(methodIndex);
                if (methodSymbols.Count > 0)
                {
                    src = methodSymbols["source"];
                    srcName = Path.GetFileName(src);
                    string ln = methodSymbols["line"];
                    methodName = methodSymbols["name"];
                    debugInfo = methodSymbols["debug"];
                    doffset = address;
                }
                Parser.ProgressTick(".");
            }
            uint delta = (address-doffset);
            string debugAddress = delta.ToString();
            if (debugInfo.Contains(debugAddress))
            {
                string dln = debugInfo[debugAddress];
                string sourceCode = GetSourceLine(src, dln);
                if (sourceCode.Length > 0)
                {
                    sourceLine = srcName + ":" + dln;
                    sourceLine += '`';
                    sourceLine = sourceLine + methodName +  ":";
                    sourceLine += '`';
                    sourceLine = sourceLine + sourceCode;
                }
            }
            if ((sourceLine.Length  > 0)  &&  stackAddresses.Contains(actualAddress))
            {
                for  (uint i=0; i < stackAddresses.Length; i++)
                {
                    if  (stackAddresses[i] == actualAddress)
                    {
                        stackLines[i] = sourceLine;
                    }
                }
            }
            
        } // for (address..
    }
    
    BadArguments()
    {
        PrintLn("Invalid arguments for RSOD:");
        PrintLn("  RSOD <hexe file>");
    }

    {
        bool success = false;
        loop
        {
            <string> rawArgs = System.Arguments;
            <string> args;
            for (uint iArg = 0; iArg < rawArgs.Length; iArg++)
            {
                string arg = rawArgs[iArg];
                if ((arg.Length == 2) && (arg[0] == '-'))
                {
                    arg = arg.ToLower();
                    switch (arg)
                    {
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
            string ext = hexeExtension;
            string codePath = args[0];
            if (!File.Exists(ref codePath, ref ext, "/Bin/"))
            {
                BadArguments();
            }
            
            long startTime = Millis;
            loop
            {
                
                string crashPath = "/Debug/crash.log";
                file crashFile = File.Open(crashPath);
                if (!crashFile.IsValid())
                {
                    PrintLn("Failed to open '" + crashPath + "'", Color.MatrixRed, Color.Black);
                    break;
                }
                uint li = 0;
                loop
                {
                    string crashLine = crashFile.ReadLine();
                    if (!crashFile.IsValid())  { break; }
                    crashLine = crashLine.Trim();
                    if  (crashLine.StartsWith("0x"))
                    {
                        uint stackAddress;
                        if  (UInt.TryParse(crashLine, ref stackAddress))
                        {
                            stackAddresses.Append(stackAddress);
                            stackLines.Append(""); // place holders
                        }
                    }
                    if (li == 0)
                    {
                        errorMessage = crashLine;
                    }
                    if (li == 1)
                    {
                        errorMessage = errorMessage +  ", "  +  crashLine;
                    }
                    li++;
                }
                if (stackAddresses.Length == 0)
                {
                    PrintLn("No stack in '" + crashPath + "'", Color.MatrixRed, Color.Black);
                    break;
                }
                
                string extension = Path.GetExtension(codePath);
                string hasmPath  = codePath.Replace(extension, hasmExtension);
                hasmPath = Path.GetFileName(hasmPath);
                hasmPath = Path.Combine("/Debug/", hasmPath);
                
                file hasmFile = File.Open(hasmPath);
                if (!hasmFile.IsValid())
                {
                    PrintLn("Failed to open '" + hasmPath + "'", Color.MatrixRed, Color.Black);
                    break;
                }
                
                string symbolsPath  = codePath.Replace(extension, ".code");
                symbolsPath = Path.GetFileName(symbolsPath);
                symbolsPath = Path.Combine("/Debug/Obj", symbolsPath);
                
                if (!File.Exists(symbolsPath) || !ParseCode(symbolsPath, false, true))
                {
                    PrintLn("Failed to load '" + symbolsPath + "'", Color.MatrixRed, Color.Black);
                    break;
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
                extendedCodeSegment = (version & 0x0001) != 0;
                
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
                address = address + 2;
                codeSize = codeSize + 6;
                
                // read the method table
                <uint,string> methodNames;
                methodNames[constOffset] = "constant data";
                if (extendedCodeSegment)
                {
                    methodNames[0] = "0x0000";
                }
                else
                {
                    methodNames[codeOffset] = "0x0000";
                }
                
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
                        
                        codeSize = codeSize + 4;
                        address = address + 4;
                        methodNames[methodOffset] = "0x" + methodIndex.ToHexString(4);
                    }
                    Parser.ProgressTick(".");
                }
                
                if (constLength > 0)
                {
                    address = address + constLength;
                    codeSize = codeSize + constLength;
                    
                    loop
                    {
                        byte b = binFile.Read();
                        if (!binFile.IsValid())
                        {
                            break;
                        }
                        constLength--;
                        if (constLength == 0)
                        {
                            break;
                        }   
                    }
                    Parser.ProgressTick(".");
                }
                <byte> code;
                loop
                {
                    byte b = binFile.Read();
                    if (!binFile.IsValid())
                    {
                        break;
                    }
                    code.Append(b);
                }
                if (extendedCodeSegment)
                {
                    address = 0;
                }
                
                DisassembleCode(code, address, methodNames);
                
                codeSize = codeSize + code.Length;
                Parser.ProgressTick(".");
                
                PrintLn();
                if (extendedCodeSegment)
                {
                    codeSize -= codeOffset;
                }
                Print("Success, " + codeSize.ToString() + " bytes of code, ", Color.ProgressText, Color.ProgressFace);
                long elapsedTime = Millis - startTime;
                float seconds = elapsedTime / 1000.0;
                PrintLn("  " + seconds.ToString() +"s", Color.ProgressHighlight, Color.ProgressFace);
                PrintLn(errorMessage, Color.MatrixRed,  Color.Black);
                PrintLn("Call stack:", Color.MatrixBlue,  Color.Black);
                uint max0 = 0;
                uint max1 = 0;
                for (uint i=0; i < stackAddresses.Length; i++)
                {
                   <string> parts = (stackLines[i]).Split('`'); 
                   string part0 = parts[0];
                   if (part0.Length > max0)
                   {
                       max0 = part0.Length;
                   }  
                   string part1 = parts[1];
                   if  (part1.Length > max1)
                   {
                       max1 = part1.Length;
                   }
                }
                for (uint i=0; i < stackAddresses.Length; i++)
                {
                    <string> parts = (stackLines[i]).Split('`'); 
                    string line = "  0x"  + (stackAddresses[i]).ToHexString(4) +  " ";
                    Print(line, Color.MatrixBlue,  Color.Black);
                    line = parts[0];
                    line = line.Pad(' ', max0 + 2);
                    Print(line, Color.MatrixBlue,  Color.Black);
                    line = parts[1];
                    line = line.Pad(' ', max1 + 2);
                    Print(line, Color.MatrixRed,  Color.Black);
                    Print(parts[2], Color.LightestGray,  Color.Black);
                    PrintLn();
                }
    
                success = true;
                break;
            } // loop
            break;
        } // loop
    } 
}
