unit Code
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Runtime"
    
    uses "JSON"
    
    uses "../Tokens/Token"
    uses "../Tokens/Scanner"
    uses "../Tokens/Parser"
    
#ifdef DEBUGGER
    bool IsDebugger { get { return true; } }
#else
    bool IsDebugger { get { return false; } }
    
    // placeholders for functionality in the debugger only
    bool IsHexDisplayMode { get { return false; } }
    DeleteBreakpoints() {}
#endif    
    
    uint entryIndex;
    <uint, <byte> > methodCode;
    <uint, uint>    methodSize;
    <uint, uint>    methodStart;
    <uint, string>  methodName;
    <byte> constantData;
    
    <string, <string,variant> > debugSymbols;
    
    <string, <uint, uint> > lineMap;
    <uint, string> codeMap;
    
    string sourcePathLoaded;
    <string> sourceLines;
    
    Clear()
    {
        methodCode.Clear();
        methodSize.Clear();
        methodStart.Clear();
        constantData.Clear();
        debugSymbols.Clear();
        lineMap.Clear();
        codeMap.Clear();
        sourcePathLoaded = "";
        sourceLines.Clear();
    }
    
    <byte> LoadRawCode(string codePath)
    {
        uint counter;
        if (IsDebugger)
        {
            Editor.SetStatusBarText("Loading code '" + codePath + "' ..");
        }
        <byte> code;    
        file binFile = File.Open(codePath);
        //PrintLn(codePath);
        loop
        {
            byte b = binFile.Read();
            if (!binFile.IsValid())
            {
                break;
            }
            code.Append(b);
            if (IsDebugger)
            {
                counter++;
                if (counter % 1024 == 0)
                {
                    Parser.ProgressTick("x"); // .hexe loaded by debugger
                }
            }
        }
        uint version     = code[0] + (code[1] << 8);
        uint methodCount = code[2] + (code[3] << 8);
        methodCount = methodCount - 6;
        methodCount = methodCount / 4;
        
        uint entryPoint = code[4] + (code[5] << 8);
        methodStart[0] = (version == 0) ? entryPoint : 0;
        
        for (uint iMethod = 0; iMethod < methodCount; iMethod++)
        {
            uint i = 6 + (iMethod * 4);
            uint index  = code[i+0] + (code[i+1] << 8);
            uint offset = code[i+2] + (code[i+3] << 8);
            methodStart[index] = offset;
        }
        return code;
    }
    
    uint LocationToIndex(uint location)
    {
        uint methodIndex;
        foreach (var kv in methodStart)
        {
            if (location < kv.value)
            {
                break; // entered next method after the one we need
            }
            methodIndex = kv.key;
        }
        return methodIndex;
    }
    
    DumpMap(uint pc)
    {
        PrintLn("PC: 0x" + pc.ToHexString(4));
        foreach (var kv in codeMap)
        {
            uint address = kv.key;
            int distance = int(pc) - int(address);
            if (Int.Abs(distance) < 100)
            {
                PrintLn("0x" + address.ToHexString(4) + " " + distance.ToString() + " " + kv.value);
            }
        }
    }
    string GetClosestSourceIndex(uint pc)
    {
        string sourceIndex = Code.GetSourceIndex(pc);
        if (sourceIndex.Length == 0)
        {
            uint closest = 10000;
            foreach (var kv in codeMap)
            {
                uint address = kv.key;
                if (pc >= address)
                {
                    uint distance = pc - address;
                    if ((distance < closest) && (distance < 50))
                    {
                        sourceIndex = kv.value;
                        closest = distance;
                    }
                }
            }
        }
        return sourceIndex;
    }
    
    string GetSourceIndex(uint address)
    {
        // exact match for first address of code for source line
        string sourceLine;
        if (codeMap.Contains(address))
        {
            sourceLine = codeMap[address];
        }
        return sourceLine;
    }
    
    <string, <string,variant> > GetDebugSymbols()
    {
        return debugSymbols;
    }
    SetDebugSymbols(<string, <string,variant> > newDebugSymbols)
    {
        debugSymbols = newDebugSymbols;
    }
    <string, <string> > GetLocals(<byte> code, uint methodIndex, ref uint startAddress)
    {
        string index = "0x" + methodIndex.ToHexString(4);
        <string, <string> > localsList;
        if (debugSymbols.Contains(index))
        {
            <string,variant> methodSymbols = debugSymbols[index];
            if (methodSymbols.Contains("locals"))
            {
                localsList = methodSymbols["locals"];
                startAddress = methodStart[methodIndex];
            }
        }
        return localsList;
    }
    
    <string, <string> > GetLocals(<byte> code, uint methodIndex)
    {
        string index = "0x" + methodIndex.ToHexString(4);
        <string, <string> > localsList;
        if (debugSymbols.Contains(index))
        {
            <string,variant> methodSymbols = debugSymbols[index];
            if (methodSymbols.Contains("locals"))
            {
                localsList = methodSymbols["locals"];
            }
        }
        return localsList;
    }
    
    <uint, string> GetGlobalTypes()
    {
        <uint, string> globalTypes;
        if (debugSymbols.Contains("globals"))
        {
            <string, variant > globalLists = debugSymbols["globals"];
            foreach (var kv in globalLists)
            {
                uint offset;
                if (UInt.TryParse(kv.key, ref offset))
                {
                    <string> globalList = kv.value;
                    globalTypes[offset] = globalList[0];
                }
            }
        }
        return globalTypes;
    }
    <uint, <string> > GetGlobals(<byte> code, uint methodIndex, uint pc, uint distance)
    {
        <uint, <string> > globalOffsets;
        uint aCurrent = methodStart[methodIndex];
        uint aEnd   = aCurrent + methodSize[methodIndex];
        
        uint codeOffset = 0;    
        uint version = code[0] + code[1] << 8;    
        if (version != 0)
        {
            codeOffset = code[4] + code[5] << 8;    
        }
        pc       += codeOffset;
        aCurrent += codeOffset;
        aEnd     += codeOffset;
        loop
        {
            uint operand;
            Instruction instruction = Instructions.GetOperandAndNextAddress(code, ref aCurrent, ref operand);
            
            // if pc != 0, prune the results based on proximity to current pc
            if (pc != 0)
            {
                uint delta = 0; // pc == aCurrent;
                if (pc < aCurrent)
                {
                    delta = aCurrent - pc;
                }
                else if (pc > aCurrent)
                {
                    delta = pc - aCurrent;
                }
                if (delta > distance)
                {
                    if (aCurrent >= aEnd)
                    {
                        break;
                    }
                    continue;
                }
            }
            switch (instruction)
            {
                case Instruction.POPGLOBALB:
                case Instruction.PUSHGLOBALB:
                case Instruction.POPGLOBAL:
                case Instruction.PUSHGLOBAL:
                {
                    if (!globalOffsets.Contains(operand))
                    {
                        <string> globalList;
                        if (debugSymbols.Contains("globals"))
                        {
                            <string, variant > globalLists = debugSymbols["globals"];
                            string sOffset = operand.ToString();
                            if (globalLists.Contains(sOffset))
                            {
                                globalList = globalLists[sOffset];
                            }
                        }
                        globalOffsets[operand] = globalList;
                    }
                }
                case Instruction.INCGLOBALB:
                case Instruction.DECGLOBALB:
                {
                    if (!globalOffsets.Contains(operand))
                    {
                        <string> globalList;
                        if (debugSymbols.Contains("globals"))
                        {
                            <string, variant > globalLists = debugSymbols["globals"];
                            string sOffset = operand.ToString();
                            if (globalLists.Contains(sOffset))
                            {
                                globalList = globalLists[sOffset];
                            }
                        }
                        globalOffsets[operand] = globalList;
                    }
                }
                case Instruction.PUSHGLOBALBB:
                {
                    uint lsb = (operand & 0xFF);
                    uint msb = (operand >> 8);
                            
                    if (!globalOffsets.Contains(lsb))
                    {
                        <string> globalList;
                        if (debugSymbols.Contains("globals"))
                        {
                            <string, variant > globalLists = debugSymbols["globals"];
                            string sOffset = lsb.ToString();
                            if (globalLists.Contains(sOffset))
                            {
                                globalList = globalLists[sOffset];
                            }
                        }
                        globalOffsets[lsb] = globalList;
                    }
                    if (!globalOffsets.Contains(msb))
                    {
                        <string> globalList;
                        if (debugSymbols.Contains("globals"))
                        {
                            <string, variant > globalLists = debugSymbols["globals"];
                            string sOffset = msb.ToString();
                            if (globalLists.Contains(sOffset))
                            {
                                globalList = globalLists[sOffset];
                            }
                        }
                        globalOffsets[msb] = globalList;
                    }
                }
            } // switch
            if (aCurrent >= aEnd)
            {
                break;
            }
        } // loop
        return globalOffsets;
    }
    string GetSourceIndex(uint address, uint methodIndex)
    {
        // line to which address belongs to (not just first address)
        uint aStart     = methodStart[methodIndex];
        uint aCurrent   = aStart + methodSize[methodIndex];
        string sourceLine;
        if (codeMap.Contains(address))
        {
            sourceLine = codeMap[address];
        }
        else
        {
            loop
            {
                aCurrent--;
                if (aCurrent < aStart)
                {
                    break;
                }
                if ((aCurrent < address) && codeMap.Contains(aCurrent))
                {
                    // find the first matching line before address, in this method
                    sourceLine = codeMap[aCurrent];
                    break;
                }
            }
        }
        return sourceLine;
    }
    
    string GetSourceLine(string sourceIndex)
    {
        <string> parts = sourceIndex.Split(':');
        
        string path = parts[0];
        string lnum = parts[1];
        
        if (sourcePathLoaded != path)
        {
            file sourceFile = File.Open(path);
            if (sourceFile.IsValid())
            {
                sourceLines.Clear();
                sourcePathLoaded = path;
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
        uint iLine;
        if (UInt.TryParse(lnum, ref iLine))
        {
            if (iLine > 0)
            {
                iLine--;
            }
            if (sourceLines.Count > iLine)
            {
                sourceLine = sourceLines[iLine];
            }
        }
        return sourceLine;
    }
    string GetSourceLine(uint iLine)
    {
        string sourceLine;
        if (iLine > 0)
        {
            iLine--;
        }
        if (sourceLines.Count > iLine)
        {
            sourceLine = sourceLines[iLine];
        }
        return sourceLine;
    }
    
    LoadStatements()
    {
        Runtime.ClearStatements();
        foreach (var kv in codeMap)
        {
            uint address = kv.key;
            Runtime.SetStatement(address);
        }    
    }
    <uint,string> MapZSource()
    {
        <uint,string> sourceMap;
        uint count;
        foreach (var kv in debugSymbols)
        {
            if (kv.key == "globals")
            {
                continue;
            }
            
            string hex = kv.key;
            uint index;
            if (!UInt.TryParse(hex, ref index))
            {
                Diagnostics.Die(0x03);
            }
            <string,variant> methodSymbols = kv.value;
            string path = methodSymbols["source"];
            string lpath = path.ToLower();
            
            <string,string> smap = methodSymbols["debug"];
            foreach (var kv2 in smap)
            {
                uint address;
                uint ln;
                if (UInt.TryParse(kv2.key, ref address))
                {
                }
                if (UInt.TryParse(kv2.value, ref ln))
                {
                }
                sourceMap[address] = path + ":" + kv2.value;
            }
        }
        return sourceMap;
    }
    
    MapSource()
    {
        //uint count;
        foreach (var kv in debugSymbols)
        {
            if (kv.key == "globals")
            {
                continue;
            }
            
            string hex = kv.key;
            uint index;
            if (!UInt.TryParse(hex, ref index))
            {
                Diagnostics.Die(0x03);
            }
            uint start = methodStart[index];
            <string,variant> methodSymbols = kv.value;
            string path = methodSymbols["source"];
            string lpath = path.ToLower();
            <uint,uint> map;
            if (lineMap.Contains(lpath))
            {
                map = lineMap[lpath];
            }
            <string,string> smap = methodSymbols["debug"];
            foreach (var kv2 in smap)
            {
                uint offset;
                uint ln;
                if (UInt.TryParse(kv2.key, ref offset))
                {
                }
                if (UInt.TryParse(kv2.value, ref ln))
                {
                }
                uint address = start + offset;
                map[ln] = address;
                codeMap[address] = path + ":" + kv2.value;
                /*
                if (count < 20)
                {
                    PrintLn(index.ToHexString(4) + " " + start.ToHexString(4) + " " + address.ToHexString(4) + ":" + codeMap[address]);
                    count++;
                }
                */
            }
            lineMap[lpath] = map;
        }
    }
    uint GetLineAddress(string path, uint lineNumber)
    {
        if (lineMap.Contains(path))
        {
            <uint, uint> map = lineMap[path];
            if (map.Contains(lineNumber))
            {
                return map[lineNumber];
            }
        }
        return 0;
    }
    
    <byte> GetConstantData()
    {
        return constantData;
    }
    uint GetEntryIndex()
    {
        return entryIndex;
    }
    
    uint GetMethodCount()
    {
        return methodCode.Count;
    }
    uint GetMethodSize(uint methodIndex)
    {
        return methodSize[methodIndex];
    }
    bool MethodExists(uint methodIndex)
    {
        return methodName.Contains(methodIndex);
    }
    bool MethodExists(string name)
    {
        foreach (var kv in methodName)
        {
            if (kv.value == name)
            {
                return true;
            }
        }
        return false;
    }
    string GetMethodName(uint methodIndex)
    {
        return methodName[methodIndex];
    }
    uint GetMethodIndex(string name)
    {
        foreach (var kv in methodName)
        {
            if (kv.value == name)
            {
                return kv.key;
            }
        }
        Die(0x0B);
        return 0;
    }
    <uint, uint> GetMethodSizes()
    {
        return methodSize;
    }    
    <byte> GetMethodCode(uint methodIndex)
    {
        return methodCode[methodIndex];
    }
    SetMethodCode(uint methodIndex, <byte> code)
    {
        methodCode[methodIndex] = code;
    }
    <string,string> GetMethodDebugInfo(uint methodIndex)
    {
        string key = "0x" + methodIndex.ToHexString(4);
        <string,variant> methodSymbols = debugSymbols[key];
        <string,string> debugInfo = methodSymbols["debug"];
        return debugInfo;
    }
    SetMethodDebugInfo(uint methodIndex, <string,string> debugInfo)
    {
        string key = "0x" + methodIndex.ToHexString(4);
        <string, variant> methodSymbols = debugSymbols[key];
        methodSymbols["debug"] = debugInfo;
        debugSymbols[key] = methodSymbols;
    }
    SetMethodLabelInfo(uint methodIndex, <string,string> labelInfo)
    {
        string key = "0x" + methodIndex.ToHexString(4);
        <string, variant> methodSymbols = debugSymbols[key];
        methodSymbols["labels"] = labelInfo;
        debugSymbols[key] = methodSymbols;
    }
    
    <uint> GetMethodIndices()
    {
        <uint> indices;
        foreach (var kv in debugSymbols)
        {
            string key = kv.key;
            uint index;
            if (UInt.TryParse(key, ref index))
            {
                indices.Append(index);
            }
        }
        return indices;
    }
    
    uint GetMethodSymbolsCount()
    {
        return debugSymbols.Count;
    }
    <string,variant> GetMethodSymbols(uint iMethod)
    {
        string methodIndex = "0x" + iMethod.ToHexString(4);
        return GetMethodSymbols(methodIndex);
    }
    <string,variant> GetMethodSymbols(string methodIndex)
    {
        <string,variant> methodSymbols;
        if (debugSymbols.Contains(methodIndex))
        {
            methodSymbols = debugSymbols[methodIndex];
        }
        return methodSymbols;
    }
    <uint, uint> GetFramelessMethodCandidates()
    {
        // <= 1 argument, number of locals in the result: <methodindex, localsCount>
        <uint, uint> framelessMethodCandidates;
        foreach (var kv in debugSymbols)
        {
            string methodIndex = kv.key; // hex
            if (methodIndex != "globals")
            {
                uint iMethod;
                _ = UInt.TryParse(methodIndex, ref iMethod);
                
                uint argumentCount;
                uint localsCount;
                <string,variant> methodSymbols = kv.value;
                if (methodSymbols.Contains("arguments"))
                {
                    <string, <string> > argumentInfo = methodSymbols["arguments"];
                    argumentCount = argumentInfo.Count; 
                }
                if (methodSymbols.Contains("locals"))
                {
                    <string, <string> > localInfo = methodSymbols["locals"];
                    localsCount = localInfo.Count;
                }
                if (argumentCount <= 1)
                {
                    framelessMethodCandidates[iMethod] = localsCount;
                }
            }
        }
        return framelessMethodCandidates;
    }
        
    bool ParseMethod(string methodIndex, bool keepCode, bool keepSymbols, ref string methodName, ref <byte> code, ref uint codeLength)
    {
        <string,variant> methodDictionary;
        
        bool first = true;
        bool isData;
        loop
        {
            if (Parser.HadError)
            {
                break;
            }
            if (Parser.Check(HopperToken.RBrace))
            {
                break;
            }
            if (!first)
            {
                Parser.Consume(HopperToken.Comma);
                if (Parser.HadError)
                {
                    break;
                }
            }
            Parser.Consume(HopperToken.StringConstant, "key name expected");
            if (Parser.HadError)
            {
                break;
            }
            <string,string> previousToken = PreviousToken;
            string keyName = previousToken["lexeme"];
            Parser.Consume(HopperToken.Colon);
            if (Parser.HadError)
            {
                break;
            }
            if (keyName == "data")
            {
                isData = true;
                keyName = "code";
                methodName = "const";
            }
            switch (keyName)
            {
                case "name":
                {
                    Parser.Consume(HopperToken.StringConstant, "method name expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                    previousToken = PreviousToken;
                    methodName = previousToken["lexeme"];
                    methodDictionary["name"] = previousToken["lexeme"];
                }
                case "source":
                {
                    Parser.Consume(HopperToken.StringConstant, "source path expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                    previousToken = PreviousToken;
                    methodDictionary["source"] = previousToken["lexeme"];
                }
                case "line":
                {
                    Parser.Consume(HopperToken.Integer, "line number expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                    previousToken = PreviousToken;
                    methodDictionary["line"] = previousToken["lexeme"];
                }
                case "hits":
                {
                    Parser.Consume(HopperToken.Integer, "hits expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                    previousToken = PreviousToken;
                    uint hits;
                    if (UInt.TryParse(previousToken["lexeme"], ref hits))
                    {
                    }
                    methodDictionary["hits"] = hits;
                }
                case "returntype":
                {
                    Parser.Consume(HopperToken.StringConstant, "type name expected");
                    if (Parser.HadError)
                    {
                        break;
                    }
                    previousToken = PreviousToken;
                    methodDictionary["returntype"] = previousToken["lexeme"];
                }
                case "locals":
                {
                    <string, <string> > localLists;
                    Parser.Consume(HopperToken.LBrace);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    bool fc = true;
                    loop
                    {
                        if (Parser.HadError)
                        {
                            break;
                        }    
                        if (Parser.Check(HopperToken.RBrace))
                        {
                            Parser.Advance(); // }
                            break;
                        }
                        if (!fc)
                        {
                            Parser.Consume(HopperToken.Comma);
                            if (Parser.HadError)
                            {
                                break;
                            }
                        }
                        
                        Parser.Consume(HopperToken.StringConstant, "active range expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        previousToken = PreviousToken;
                        string range = previousToken["lexeme"];
                        
                        Parser.Consume(HopperToken.Colon);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        
                        Parser.Consume(HopperToken.LBrace);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        bool fa = true;
                        string localName;
                        string localType;
                        string localOffset;
                        
                        loop
                        {
                            if (Parser.Check(HopperToken.RBrace))
                            {
                                Parser.Advance(); // }
                                break;
                            }
                            if (!fa)
                            {
                                Parser.Consume(HopperToken.Comma);
                                if (Parser.HadError)
                                {
                                    break;
                                }
                            }
                            Parser.Consume(HopperToken.StringConstant, "local member name expected");
                            if (Parser.HadError)
                            {
                                break;
                            }
                            previousToken = PreviousToken;
                            string membername = previousToken["lexeme"];
                        
                            Parser.Consume(HopperToken.Colon);
                            if (Parser.HadError)
                            {
                                break;
                            }
                            Parser.Consume(HopperToken.StringConstant, "local member value expected");
                            if (Parser.HadError)
                            {
                                break;
                            }
                            previousToken = PreviousToken;
                            string membervalue = previousToken["lexeme"];
                            switch (membername)
                            {
                                case "name":
                                {
                                    localName = membervalue;
                                }
                                case "type":
                                {
                                    localType = membervalue;
                                }
                                case "offset":
                                {
                                    localOffset = membervalue;
                                }
                            }
                            fa = false;
                        }
                        if (Parser.HadError)
                        {
                            break;
                        }
                        <string> localList;
                        localList.Append(localName);
                        localList.Append(localType);
                        localList.Append(localOffset);
                        localLists[range] = localList;
                        fc = false;
                    } // loop
                    if (keepSymbols)
                    {
                        methodDictionary["locals"] = localLists;
                    }
                }
                case "arguments":
                {
                    <string, <string> > argumentLists;
                    
                    Parser.Consume(HopperToken.LBrace);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    bool fc = true;
                    loop
                    {
                        if (Parser.HadError)
                        {
                            break;
                        }    
                        if (Parser.Check(HopperToken.RBrace))
                        {
                            Parser.Advance(); // }
                            break;
                        }
                        if (!fc)
                        {
                            Parser.Consume(HopperToken.Comma);
                            if (Parser.HadError)
                            {
                                break;
                            }
                        }
                        
                        Parser.Consume(HopperToken.StringConstant, "BP offset expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        previousToken = PreviousToken;
                        string offset = previousToken["lexeme"];
                        
                        Parser.Consume(HopperToken.Colon);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        
                        Parser.Consume(HopperToken.LBrace);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        bool fa = true;
                        string argumentName;
                        string argumentRef;
                        string argumentType;
                        
                        loop
                        {
                            if (Parser.Check(HopperToken.RBrace))
                            {
                                Parser.Advance(); // }
                                break;
                            }
                            if (!fa)
                            {
                                Parser.Consume(HopperToken.Comma);
                                if (Parser.HadError)
                                {
                                    break;
                                }
                            }
                            Parser.Consume(HopperToken.StringConstant, "argument member name expected");
                            if (Parser.HadError)
                            {
                                break;
                            }
                            previousToken = PreviousToken;
                            string membername = previousToken["lexeme"];
                        
                            Parser.Consume(HopperToken.Colon);
                            if (Parser.HadError)
                            {
                                break;
                            }
                            Parser.Consume(HopperToken.StringConstant, "argument member value expected");
                            if (Parser.HadError)
                            {
                                break;
                            }
                            previousToken = PreviousToken;
                            string membervalue = previousToken["lexeme"];
                            switch (membername)
                            {
                                case "name":
                                {
                                    argumentName = membervalue;
                                }
                                case "type":
                                {
                                    argumentType = membervalue;
                                }
                                case "ref":
                                {
                                    argumentRef = membervalue;
                                }
                            }
                            fa = false;
                        }
                        if (Parser.HadError)
                        {
                            break;
                        }
                        <string> argumentList;
                        argumentList.Append(argumentRef);
                        argumentList.Append(argumentType);
                        argumentList.Append(argumentName);
                        argumentLists[offset] = argumentList;
                    
                        
                        fc = false;
                    } // loop
                    if (keepSymbols)
                    {
                        methodDictionary["arguments"] = argumentLists;
                    }
                }
                case "debug":
                {
                    <string,string> debugInfo;
                    Parser.Consume(HopperToken.LBrace);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    bool fc = true;
                    loop
                    {
                        if (Parser.HadError)
                        {
                            break;
                        }    
                        if (Parser.Check(HopperToken.RBrace))
                        {
                            Parser.Advance(); // }
                            break;
                        }
                        if (!fc)
                        {
                            Parser.Consume(HopperToken.Comma);
                            if (Parser.HadError)
                            {
                                break;
                            }
                        }
                            
                        Parser.Consume(HopperToken.StringConstant, "offset key expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        previousToken = PreviousToken;
                        string offset = previousToken["lexeme"];
                        
                        Parser.Consume(HopperToken.Colon);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        
                        Parser.Consume(HopperToken.StringConstant, "line value expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        previousToken = PreviousToken;
                        string ln = previousToken["lexeme"];
                        
                        debugInfo[offset] = ln;
                        
                        fc = false;
                    } // loop   
                    if (keepSymbols)
                    {
                        methodDictionary["debug"] = debugInfo;
                    }
                }
                case "labels":
                {
                    <string,string> labelInfo;
                    Parser.Consume(HopperToken.LBrace);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    bool fc = true;
                    loop
                    {
                        if (Parser.HadError)
                        {
                            break;
                        }    
                        if (Parser.Check(HopperToken.RBrace))
                        {
                            Parser.Advance(); // }
                            break;
                        }
                        if (!fc)
                        {
                            Parser.Consume(HopperToken.Comma);
                            if (Parser.HadError)
                            {
                                break;
                            }
                        }
                            
                        Parser.Consume(HopperToken.StringConstant, "offset key expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        previousToken = PreviousToken;
                        string offset = previousToken["lexeme"];
                        
                        Parser.Consume(HopperToken.Colon);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        
                        Parser.Consume(HopperToken.StringConstant, "label name expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        previousToken = PreviousToken;
                        string ln = previousToken["lexeme"];
                        
                        labelInfo[offset] = ln;
                        
                        fc = false;
                    } // loop   
                    if (keepSymbols)
                    {
                        methodDictionary["labels"] = labelInfo;
                    }
                } // "labels"
                case "code":
                {
                    Parser.Consume(HopperToken.LBracket);
                    if (Parser.HadError)
                    {
                        break;
                    }
                    bool fc = true;
                    loop
                    {
                        if (Parser.HadError)
                        {
                            break;
                        }    
                        if (Parser.Check(HopperToken.RBracket))
                        {
                            Parser.Advance(); // ]
                            break;
                        }
                        if (!fc)
                        {
                            Parser.Consume(HopperToken.Comma);
                            if (Parser.HadError)
                            {
                                break;
                            }
                        }
                        Parser.Consume(HopperToken.Integer, "code expected");
                        if (Parser.HadError)
                        {
                            break;
                        }    
                        if (keepCode)
                        {
                            uint hc;
                            previousToken = PreviousToken;
                            string hexcode = previousToken["lexeme"];
                            if (UInt.TryParse(hexcode, ref hc))
                            {
                            }
                            code.Append(byte(hc));
                        }
                        codeLength++;
                        fc = false;
                    } // loop
                }
                default:
                {
                    Parser.Error("unexpected key '" + keyName + "'");
                    break;
                }
            }
            first = false;
        } // loop
        
        if (!isData && keepSymbols)
        {
            uint iMethod;
            if (UInt.TryParse(methodIndex, ref iMethod))
            {
            }
            string name = "0x" + iMethod.ToHexString(4);
            debugSymbols[name] = methodDictionary;
        }
        return !Parser.HadError;
    }
    
    bool ParseCode(string codePath, bool keepCode, bool keepSymbols)
    {
        string progMark = "c";
        if (codePath.Contains(".zcode"))
        {
            progMark = "z";
        }
        bool success;
        long pos;
        bool first;
        loop
        {
            Scanner.New();
            Parser.Reset();
            Scanner.Load(codePath);
            Scanner.Reset(pos, 1, codePath);
    
            Parser.Advance(); // load up first token
            Parser.Consume(HopperToken.LBrace);
            first = true;
            uint loopCount;
            loop
            {
                if (Parser.HadError)
                {
                    break;
                }
                if (Parser.Check(HopperToken.RBrace))
                {
                    break;
                }
                if (!first)
                {
                    Parser.Consume(HopperToken.Comma);
                }
                first = false;
                Parser.Consume(HopperToken.StringConstant, "method index expected");
                if (Parser.HadError)
                {
                    break;
                }
                <string,string> previousToken = PreviousToken;
                string methodIndex = previousToken["lexeme"];
                
                Parser.Consume(HopperToken.Colon);
                if (Parser.HadError)
                {
                    break;
                }
                Parser.Consume(HopperToken.LBrace);
                if (Parser.HadError)
                {
                    break;
                }
                
                if (methodIndex == "globals")
                {
                    <string, variant > globalLists;
                    bool fc = true;
                    loop
                    {
                        if (Parser.HadError)
                        {
                            break;
                        }    
                        if (Parser.Check(HopperToken.RBrace))
                        {
                            Parser.Advance(); // }
                            break;
                        }
                        if (!fc)
                        {
                            Parser.Consume(HopperToken.Comma);
                            if (Parser.HadError)
                            {
                                break;
                            }
                        }
                        
                        Parser.Consume(HopperToken.StringConstant, "global offset expected");
                        if (Parser.HadError)
                        {
                            break;
                        }
                        previousToken = PreviousToken;
                        string offset = previousToken["lexeme"];
                        
                        Parser.Consume(HopperToken.Colon);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        
                        Parser.Consume(HopperToken.LBrace);
                        if (Parser.HadError)
                        {
                            break;
                        }
                        bool fg = true;
                        string globalName;
                        string globalType;
                        string globalDefinition;
                        loop
                        {
                            if (Parser.Check(HopperToken.RBrace))
                            {
                                Parser.Advance(); // }
                                break;
                            }
                            if (!fg)
                            {
                                Parser.Consume(HopperToken.Comma);
                                if (Parser.HadError)
                                {
                                    break;
                                }
                            }
                            Parser.Consume(HopperToken.StringConstant, "global member name expected");
                            if (Parser.HadError)
                            {
                                break;
                            }
                            previousToken = PreviousToken;
                            string membername = previousToken["lexeme"];
                            
                            Parser.Consume(HopperToken.Colon);
                            if (Parser.HadError)
                            {
                                break;
                            }
                            Parser.Consume(HopperToken.StringConstant, "global member value expected");
                            previousToken = PreviousToken;
                            string membervalue = previousToken["lexeme"];
                            if (Parser.HadError)
                            {
                                break;
                            }
                            switch (membername)
                            {
                                case "name":
                                {
                                    globalName = membervalue;
                                }
                                case "type":
                                {
                                    globalType = membervalue;
                                }
                                case "definition":
                                {
                                    globalDefinition = membervalue;
                                }
                            }
                            fg = false;
                        }
                        <string> globalList;
                        globalList.Append(globalType);
                        globalList.Append(globalName);
                        globalList.Append(globalDefinition);
                        globalLists[offset] = globalList;
                    
                        fc = false;
                    } // loop
                    if (keepSymbols)
                    {
                        debugSymbols["globals"] = globalLists;
                    }
                }
                else
                {
                    string name;
                    <byte> code;
                    uint codeLength;
                    if (!ParseMethod(methodIndex, keepCode, keepSymbols, ref name, ref code, ref codeLength))
                    {
                        break;
                    }
                    
                    if (methodIndex == "const")
                    {
                        if (keepCode)
                        {
                            constantData = code;
                        }
                    }
                    else
                    {
                        uint index;
                        if (UInt.TryParse(methodIndex, ref index))
                        {
                        }
                        if (name.EndsWith(".Hopper"))
                        {
                            entryIndex = index;
                        }
                        if (keepCode)
                        {
                            methodCode[index] = code;
                        }
                        methodSize[index] = codeLength;
                        methodName[index] = name;
                    }
                    Parser.Consume(HopperToken.RBrace);
                    if (Parser.HadError)
                    {
                        break;
                    }
                }
                if (loopCount % 32 == 0)
                {
                    Parser.ProgressTick(progMark); // loading .code or .zcode
                }
                loopCount++;
            } // loop  
            if (Parser.HadError)
            {
                break;
            }
            success = true;
            break;
        } // loop
        if (keepSymbols && (methodStart.Count != 0))
        {
            MapSource();
        }
        return success;
    }
    bool ExportCode(string codePath)
    {
        return ExportCode(codePath, true);
    }
    bool ExportCode(string codePath, bool saveCode)
    {
        bool success = true;
        <string, variant> dict;
        
        if (constantData.Count != 0)
        {
            <string, variant> cdict;
            cdict["data"] = constantData;
            dict["const"] = cdict;
        }
        
        foreach (var kv in debugSymbols)
        {
            uint index;
            if (UInt.TryParse(kv.key, ref index))
            {
                <string,variant> methodDictionary;
                foreach (var mkv in kv.value) // just to get the order the same as the original .code
                {
                    if (mkv.key == "arguments")
                    {
                        <string, <string> > loadedArguments = mkv.value;
                        <string,variant> argumentsDictionary;
                        foreach (var akv in loadedArguments)
                        {
                            <string> alist = akv.value;
                            <string,string> adict;
                            adict["name"] = alist[2];
                            adict["type"] = alist[1];
                            adict["ref"]  = alist[0];
                            argumentsDictionary[akv.key] = adict;
                        }
                        methodDictionary[mkv.key] = argumentsDictionary;
                    }
                    else if (mkv.key == "locals")
                    {
                        <string, <string> > loadedLocals = mkv.value;
                        <string,variant> localsDictionary;
                        foreach (var lkv in loadedLocals)
                        {
                            <string> llist = lkv.value;
                            <string,string> ldict;
                            ldict["name"] = llist[0];
                            ldict["type"] = llist[1];
                            ldict["offset"]  = llist[2];
                            localsDictionary[lkv.key] = ldict;
                        }
                        methodDictionary[mkv.key] = localsDictionary;
                    }
                    else if (mkv.key == "line")
                    {
                        uint ln;
                        string sln = mkv.value;
                        if (UInt.TryParse(sln, ref ln))
                        {
                            methodDictionary[mkv.key] = ln;
                        }
                    }
                    else if (mkv.key == "returntype")
                    {
                        string tp = mkv.value;
                        methodDictionary[mkv.key] = tp;
                    }
                    else
                    {
                        if (mkv.key == "debug") // "code" comes before "debug"
                        {
                            if (saveCode && (methodCode.Count != 0)) // was code loaded?
                            {
                                methodDictionary["code"] = methodCode[index];
                            }
                        }
                        methodDictionary[mkv.key] = mkv.value;
                    }
                }
                dict[index.ToString()] = methodDictionary;
            }
            else if (kv.key == "globals")
            {
                <string,variant> globalsDictionary;
                foreach (var gkv in kv.value)
                {
                    // <type, name, definition> like <enum, opCode, OpCode);
                    <string> glist = gkv.value;
                    <string,string> gdict;
                    gdict["name"] = glist[1];
                    gdict["type"] = glist[0];
                    if (glist.Count > 2)
                    {
                        gdict["definition"] = glist[2];
                    }
                    globalsDictionary[gkv.key] = gdict;
                }
                dict[kv.key] = globalsDictionary;
            }
        }
        
        if (!JSON.Write(codePath, dict))
        {
            success = false;
        }
        
        return success;
    }
    
}
