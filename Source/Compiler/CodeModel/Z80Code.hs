unit Z80Code
{
    <uint,uint>            methodAddresses; // <index, address>
    <uint,uint>            methodLengths;   // <index, length>
    <uint,string>          methodNames;     // <index, name>
    <uint, <uint,string> > methodDebug;     // <index, <address, sourceLineNumber> >

    <byte>           preAmble;        // code before entryPoint: not optimized
    <uint, <byte> >  methodCode;      // <index, <byte> >
    
    <byte> code;
    <byte> codeAfter;
    
    uint org;
    
    byte hexCheckSum(string values)
    {
        uint sum = 0;
        for (uint i = 0; i < values.Length / 2; i++)
        {
            string substr = values.Substring(i * 2, 2);
            uint b = 0;
            if (UInt.TryParse("0x" + substr, ref b))
            {
            }
            sum = sum + b;
        }
        sum = sum % 256;
        byte chk = byte(sum);
        chk = ~chk;
        chk++;
        return chk;
    }
    
    emitBuffer(file ihexFile, uint address, string buffer)
    {
        uint bytes = buffer.Length / 2;
        string ln = bytes.ToHexString(2) + address.ToHexString(4) + "00" + buffer;
        byte chk = hexCheckSum(ln);
        ihexFile.Append(":" + ln + chk.ToHexString(2) + char(0x0A));
    }
    
    writeIHex(file ihexFile, uint romAddress, <byte> output)
    {
        // https://en.wikipedia.org/wikie/Intel_HEX#Format
        
        uint byteCount = 0;
        uint index = 0;
        
        byte currentTick = 0;
        string progressTicks = "-\\|/-\\|/";
        
        string buffer;
        uint emitAddress = 0;
        loop
        {
            if (index == output.Count)
            {
                // done
                break;
            }
            byte cb = output[index]; index++;
            
            buffer = buffer + cb.ToHexString(2);
            if (buffer.Length == 32)
            {
                emitBuffer(ihexFile, emitAddress + romAddress, buffer);
                emitAddress = emitAddress + 16;
                buffer = "";
            }
            
            byteCount++;
            if (byteCount % 32 == 0)
            {
                Parser.ProgressTick("x");
            }
        }
        if (buffer.Length != 0)
        {
            emitBuffer(ihexFile, emitAddress + romAddress, buffer);
            buffer = "";
        }
        
        ihexFile.Append(":00000001FF" + char(0x0A)); // eof
        ihexFile.Flush();
    }
    
    readIHex(file hexFile, ref uint org)
    {
        bool first = true;
        code.Clear();
        loop
        {
            string ln = hexFile.ReadLine();
            if (!hexFile.IsValid()) { break; }
            string len = ln.Substring(1,2);
            uint length;
            _ = UInt.TryParse("0x" + len, ref length);
            if (length == 0) { continue; }
            if (first)
            {
                string orgString = ln.Substring(3,4);
                _ = UInt.TryParse("0x" + orgString, ref org);
            }
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
            first = false;
        }
    }
    long Load(string codePath)
    {
        org = 0;
        file hexFile = File.Open(codePath);
        readIHex(hexFile, ref org); // -> code[]
        
        uint entryAddress = code[4] + code[5] << 8;
        
        <uint, uint> methodSizes = Code.GetMethodSizes();
        uint indexMax = 0;
        foreach (var sz in methodSizes)
        {
            if (sz.key > indexMax)
            {
                indexMax = sz.key;
            }
        }
        
        preAmble.Clear();
        for (uint i=0; i < entryAddress; i++)
        {
            preAmble.Append(code[i]);
        }
                
        <string,string> debugInfo;
        uint previousIndex;
        for (uint index = 0; index <= indexMax; index++)
        {
            <string,variant> methodSymbols = Code.GetMethodSymbols(index);
            if (methodSymbols.Count != 0)
            {
                debugInfo = methodSymbols["debug"];
                foreach (var kv in debugInfo)
                {
                    uint codeAddress;
                    if (UInt.TryParse(kv.key, ref codeAddress))
                    {
                        if (index == 0)
                        {
                            codeAddress = entryAddress; // taken from the code at the reset vector
                        }
                        else 
                        {
                            // -10 'ENTER'   preamble
                            // -18 'ENTERB'  preamble (optimized only)
                            // -14 'ENTERB' 1 ?
                            // - 0'RETFAST'  preamble (optimized only)
                            uint seek = codeAddress;
                            loop
                            {
                                OpCode opCode2 = GetOpCode(code, seek-6);  
                                OpCode opCode1 = GetOpCode(code, seek-4);  
                                OpCode opCode0 = GetOpCode(code, seek);
                                    
                                if ((opCode2 == OpCode.PUSH_IY)  && (opCode1 == OpCode.LD_inn_SP) && (opCode0 == OpCode.LD_IY_inn)) 
                                {
                                    // "PUSH BP, BP = SP" stack frame setup
                                    //Print(AsmZ80.GetName(opCode2) + "   " + AsmZ80.GetName(opCode1) + "   " + AsmZ80.GetName(opCode0) + ",   ");
                                    codeAddress = seek - 6;
                                }
                                seek--;
                                if (codeAddress - seek > 25) { break; }
                            }
                            OpCode opCode = GetOpCode(code, codeAddress-1);
                            //Print((codeAddress-1).ToHexString(4) +":" + AsmZ80.GetName(opCode) + ",   ");
                            if (opCode == OpCode.RET)
                            {
                                methodLengths[previousIndex] = codeAddress - methodAddresses[previousIndex];
                            }
                        }
                        methodAddresses[index] = codeAddress;
                        string name = methodSymbols["name"];
                        methodNames[index] = name;
                        //PrintLn(index.ToString() + ": 0x" + codeAddress.ToHexString(4));
                        previousIndex = index;
                        break;
                    }
                }
            }
        }
        methodLengths[previousIndex] = code.Count - methodAddresses[previousIndex];
        foreach (var kv in methodAddresses)
        {
            uint methodIndex = kv.key;
            uint methodAddress = kv.value;
            uint methodLength = methodLengths[methodIndex];
            uint methodEnd = methodAddress + methodLength - 1;
            PrintLn(methodIndex.ToHexString(4) + ": 0x" + methodAddress.ToHexString(4) + "-0x" + methodEnd.ToHexString(4) + "  " + (methodNames[methodIndex]).Pad(' ', 30) + " (" + methodLength.ToString() + " bytes)");
            
            <byte> method;
            methodEnd++;
            for (uint i = methodAddress; i < methodEnd; i++)
            {
                method.Append(code[i]);
            }
            methodCode[methodIndex] = method;
            
            <uint, string> debugLines;
            <string,variant> methodSymbols = Code.GetMethodSymbols(methodIndex);
            if (methodSymbols.Count != 0)
            {
                debugInfo = methodSymbols["debug"];
                foreach (var kv in debugInfo)
                {
                    uint codeAddress;
                    if (UInt.TryParse(kv.key, ref codeAddress))
                    {
                        debugLines[codeAddress - methodAddress] = kv.value; // 0-based based on the code bytes of the method
                    }
                }
            }
            methodDebug[methodIndex] = debugLines;
        }
        return long(code.Count);
    }
    
    appendBytes(<byte> this, <byte> append)
    {
        foreach (var b in append)
        {
            this.Append(b);
        }
    }
    reAssemble()
    {
        codeAfter.Clear();
        appendBytes(codeAfter, preAmble);
        foreach (var kv in methodAddresses)
        {
            uint methodIndex = kv.key;
            <byte> append = methodCode[methodIndex];
            appendBytes(codeAfter, append);
        }
    }
    bool Save(string codePath, ref long sizeAfter)
    {
        bool success;
        loop
        {
            reAssemble();
            if (code.Count != codeAfter.Count)
            {
                PrintLn("code.Count=" + (code.Count).ToString() + ", codeAfter.Count=" + (codeAfter.Count).ToString());
                break;
            }
            for (uint i = 0; i < codeAfter.Count; i++)
            {
                if (code[i] != codeAfter[i])
                {
                    PrintLn(i.ToHexString(4) + ": Failed!");
                    break;
                }
            }
            File.Delete(codePath);
            file hexFile = File.Create(codePath);   
            writeIHex(hexFile, 0x0000, codeAfter);
            
            sizeAfter = codeAfter.Count;
        
            success = true;
            break;
        }
        return success;
    }
    bool SaveSymbols(string zcodePath)
    {
        bool success;
        loop
        {
            foreach (var kv in methodAddresses)
            {
                uint methodIndex    = kv.key;
                uint methodAddress = kv.value;
                <uint, string> debugLines = methodDebug[methodIndex];
                <string,variant> methodSymbols = Code.GetMethodSymbols(methodIndex);
                <string,string> debugInfo = methodSymbols["debug"];
                <string,string> modifiedDebugInfo;
                if (debugInfo.Count != debugLines.Count)
                {
                    PrintLn("debugInfo.Count=" + (debugInfo.Count).ToString() + ", debugLines.Count=" + (debugLines.Count).ToString());
                    break;
                }    
                foreach (var kv in debugLines)
                {
                    uint   address   = kv.key + methodAddress;
                    string debugLine = kv.value;
                    bool   found;
                    uint   oldCodeAddress;
                    foreach (var kv2 in debugInfo)
                    {
                        if (kv2.value == debugLine)
                        {   
                            _ = UInt.TryParse(kv2.key, ref oldCodeAddress);
                            found = (oldCodeAddress == address);
                            break;
                        }
                    }
                    if (!found)
                    {
                        PrintLn("debugLine: " + debugLine + ", oldCodeAddress=" + oldCodeAddress.ToHexString(4)+ ", address=" + address.ToHexString(4));
                        break;
                    }
                    modifiedDebugInfo["0x" + address.ToHexString(4)] = debugLine;
                }
                Code.SetMethodDebugInfo(methodIndex, modifiedDebugInfo);
            }
            success = Code.ExportCode(zcodePath, false);
            break;
        }
        return success;
    }
                
    
}
