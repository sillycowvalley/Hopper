unit Z80Code
{
    <uint,uint>            methodAddresses; // <index, address>
    <uint,uint>            methodIndices;   // <address, index>
    <uint,uint>            methodLengths;   // <index, length>
    <uint,string>          methodNames;     // <index, name>
    <uint, <uint,uint> >   methodDebug;     // <index, <address, sourceLineNumber> >

    <byte>           preAmble;        // code before entryPoint: not optimized
    <uint, <byte> >  methodCode;      // <index, <byte> >
    
    <byte> code;
    <byte> codeAfter;
    
    uint org;
    
    uint GetPreAmbleSize() { return preAmble.Count; }
    
    <uint,string> GetMethodNames() // <index, name>
    {
        return methodNames;     
    }
    <byte> GetMethodCode(uint methodIndex)
    {
        return methodCode[methodIndex];
    }
    SetMethodCode(uint methodIndex, <byte> code)
    {
        methodCode[methodIndex] = code;
    }
    <uint,uint> GetMethodDebugLines(uint methodIndex)
    {
        return methodDebug[methodIndex];
    }
    SetMethodDebugLines(uint methodIndex, <uint,uint> debugLines)
    {
        methodDebug[methodIndex] = debugLines;
    }
    
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
        Parser.ProgressTick("x");
        
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
            if (byteCount % 1024 == 0)
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
        uint byteCount = 0;
        Parser.ProgressTick("x");
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
            byteCount += 16;
            if (byteCount % 1024 == 0)
            {
                Parser.ProgressTick("x");
            }
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
                            methodLengths[previousIndex] = codeAddress - methodAddresses[previousIndex];
                        }
                        methodAddresses[index] = codeAddress;
                        methodIndices[codeAddress] = index;
                        string name = methodSymbols["name"];
                        methodNames[index] = name;
                        previousIndex = index;
                        break;
                    }
                }
            }
        }
        //PrintLn("preAmble=" + (preAmble.Count).ToString());
        //PrintLn("length=" + (code.Count).ToString());
        methodLengths[previousIndex] = code.Count - methodAddresses[previousIndex];
        foreach (var kv in methodAddresses)
        {
            uint methodIndex = kv.key;
            uint methodAddress = kv.value;
            //Print(methodIndex.ToHexString(4) + ": ");
            uint methodLength = methodLengths[methodIndex];
            uint methodEnd = methodAddress + methodLength - 1;
            
            //PrintLn();
            //Print("  0x" + methodIndex.ToHexString(4) + ": 0x" + methodAddress.ToHexString(4) + "-0x" + methodEnd.ToHexString(4) + "  " + (methodNames[methodIndex]).Pad(' ', 30) + " (" + methodLength.ToString() + " bytes)");
            
            <byte> method;
            for (uint i = 0; i < methodLength; i++)
            {
                byte b = code[methodAddress + i];
                method.Append(b);
            }
            convertToRelative(methodIndex, method);
            
            methodCode[methodIndex] = method;
            
            <uint, uint> debugLines;
            <string,variant> methodSymbols = Code.GetMethodSymbols(methodIndex);
            if (methodSymbols.Count != 0)
            {
                debugInfo = methodSymbols["debug"];
                foreach (var kv in debugInfo)
                {
                    uint codeAddress;
                    if (UInt.TryParse(kv.key, ref codeAddress))
                    {
                        uint sourceLine;
                        _ = UInt.TryParse(kv.value, ref sourceLine);
                        debugLines[codeAddress - methodAddress] = sourceLine; // 0-based based on the code bytes of the method
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
        
        // Rebuild: methodAddresses and methodIndices based on new methodCode sizes
        uint address = preAmble.Count;
        methodAddresses.Clear();
        methodIndices.Clear();
        foreach (var kv in methodNames)
        {
            uint methodIndex = kv.key;
            <byte> method = methodCode[methodIndex];
            methodAddresses[methodIndex] = address;
            methodIndices[address] = methodIndex;
            address += method.Count;
        }
        
        foreach (var kv in methodAddresses)
        {
            uint methodIndex = kv.key;
            <byte> method = methodCode[methodIndex];
            convertToAbsolute(methodIndex, method);
            appendBytes(codeAfter, method);
        }
    }
    bool Save(string codePath, ref long sizeAfter)
    {
        bool success;
        loop
        {
            reAssemble();
            /*
            if (codeAfter.Count <= code.Count)
            {
                for (uint i = 0; i < codeAfter.Count; i++)
                {
                    if (code[i] != codeAfter[i])
                    {
                        PrintLn();
                        Print("  " + i.ToHexString(4) + ": Failed!   was 0x" + (code[i]).ToHexString(2) + ", now 0x" + (codeAfter[i]).ToHexString(2));
                        break;
                    }
                }
            }
            if (code.Count != codeAfter.Count)
            {
                PrintLn("code.Count=" + (code.Count).ToString() + ", codeAfter.Count=" + (codeAfter.Count).ToString());
                // break;
            }
            */
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
                <uint, uint> debugLines = methodDebug[methodIndex];
                <string,variant> methodSymbols = Code.GetMethodSymbols(methodIndex);
                <string,string> debugInfo = methodSymbols["debug"];
                <string,string> modifiedDebugInfo;
                foreach (var kv in debugLines)
                {
                    uint   address   = kv.key + methodAddress;
                    string debugLine = (kv.value).ToString();
                    modifiedDebugInfo["0x" + address.ToHexString(4)] = debugLine;
                }
                Code.SetMethodDebugInfo(methodIndex, modifiedDebugInfo);
            }
            success = Code.ExportCode(zcodePath, false);
            break;
        }
        return success;
    }
    
    convertToRelative(uint methodIndex, <byte> method)
    {
        // convert CALL_nn :           methodAddress -> methodIndex
        // convert DDDD, LD_DE_nn :    methodAddress -> methodIndex
        // convert JP_x_nn        :    globalAddress -> methodAddress (start of method is '0x0000')
     
        uint methodAddress = methodAddresses[methodIndex];
        uint preAmbleLast  = preAmble.Count-1;
        //PrintLn();
        //Print("  convertToRelative: 0x" + methodIndex.ToHexString(4) + " 0x" + methodAddress.ToHexString(4) + " " + (method.Count).ToString());   
        
        uint index = 0;
        bool expectDelegate;
        loop
        {
            if (index == method.Count) { break; }
            uint instructionAddress = index;
            
            OpCode instruction;
            OperandType operandType;
            byte operandLength;
            bool signed;
            
            if (index > method.Count)
            {
                PrintLn();
                Print("  " + index.ToString() + " (" + (methodAddress + index).ToHexString(4) + " bytes)");
            }
            
            byte opCodeLength = GetOpCodeLength(method[index]);
            
            if (opCodeLength == 1)
            {
                 instruction = OpCode(method[index]);
                 index++;
            }
            else if (opCodeLength == 2)
            {
                 instruction = OpCode((method[index] << 8) + method[index+1]);
                 index += 2;
            }
            string name = GetOpCodeInfo(instruction, ref operandType, ref operandLength, ref signed, false);
            uint operand = 0;
            if (operandLength == 1)
            {
                 operand = method[index];
                 index++;
            }
            else if (operandLength == 2)
            {
                 operand = method[index] + (method[index+1] << 8);
                 index += 2;
            }
            if (expectDelegate)
            {
                if (instruction != OpCode.LD_DE_nn)
                {
                    Die(0x0B);
                }
                uint targetIndex = methodIndices[operand];
                //PrintLn();
                //Print("    D: 0x" + instructionAddress.ToHexString(4) + " " + name + " " + (operandLength + opCodeLength).ToString() + " 0x" + operand.ToHexString(4) +" -> 0x" + targetIndex.ToHexString(4));
                method[index-2] = byte(targetIndex & 0xFF);
                method[index-1] = byte(targetIndex >> 8);
                expectDelegate = false;
            }
            switch (instruction)
            {
                case OpCode.CALL_nn:
                {
                    if (operand > preAmbleLast)
                    {
                        uint targetIndex = methodIndices[operand] | 0x8000;
                        //PrintLn();
                        //Print("    0x" + instructionAddress.ToHexString(4) + " " + name + " " + (operandLength + opCodeLength).ToString() + " 0x" + operand.ToHexString(4) +" -> 0x" + targetIndex.ToHexString(4));
                        method[index-2] = byte(targetIndex & 0xFF);
                        method[index-1] = byte(targetIndex >> 8);
                    }
                }
                case OpCode.JP_nn:
                case OpCode.JP_Z_nn:
                case OpCode.JP_NZ_nn:
                {
                    long jumpOffset = long(operand - methodAddress) - long(instructionAddress + 3);
                    int iJumpOffset = int(jumpOffset);
                    //PrintLn();
                    //Print("    0x" + instructionAddress.ToHexString(4) + " " + name + " " + (operandLength + opCodeLength).ToString() + " 0x" + operand.ToHexString(4) + " -> " + iJumpOffset.ToString());
                    method[index-2] = iJumpOffset.GetByte(0);
                    method[index-1] = iJumpOffset.GetByte(1);
                }
                case OpCode.DDDD:
                {
                    expectDelegate = true;
                }
            }
        } // loop
    }
    convertToAbsolute(uint methodIndex, <byte> method)
    {
        // convert CALL_nn :           methodIndex -> methodAddress
        // convert DDDD, LD_DE_nn :    methodIndex -> methodAddress
        // convert JP_x_nn        :    methodAddress -> methodAddress
        
        uint methodAddress = methodAddresses[methodIndex];
        uint index = 0;
        bool expectDelegate;
        loop
        {
            if (index == method.Count) { break; }
            uint instructionAddress = index;
            
            OpCode instruction;
            OperandType operandType;
            byte operandLength;
            bool signed;
            
            byte opCodeLength = GetOpCodeLength(method[index]);
            
            if (opCodeLength == 1)
            {
                 instruction = OpCode(method[index]);
                 index++;
            }
            else if (opCodeLength == 2)
            {
                 instruction = OpCode((method[index] << 8) + method[index+1]);
                 index += 2;
            }
            string name = GetOpCodeInfo(instruction, ref operandType, ref operandLength, ref signed, false);
            uint operand = 0;
            if (operandLength == 1)
            {
                 operand = method[index];
                 index++;
            }
            else if (operandLength == 2)
            {
                 operand = method[index] + (method[index+1] << 8);
                 index += 2;
            }
            if (expectDelegate)
            {
                if (instruction != OpCode.LD_DE_nn)
                {
                    Die(0x0B);
                }
                uint targetAddress = methodAddresses[operand];
                //PrintLn();
                //Print("    D: 0x" + instructionAddress.ToHexString(4) + " " + name + " " + (operandLength + opCodeLength).ToString() + " 0x" + operand.ToHexString(4) +" -> 0x" + targetAddress.ToHexString(4));
                method[index-2] = byte(targetAddress & 0xFF);
                method[index-1] = byte(targetAddress >> 8);               
                expectDelegate = false;
            }
            switch (instruction)
            {
                case OpCode.CALL_nn:
                {
                    if (operand & 0x8000 != 0)
                    {
                        operand &= 0x7FFF;
                        uint targetAddress = methodAddresses[operand];
                        //PrintLn();
                        //Print("    0x" + instructionAddress.ToHexString(4) + " " + name + " " + (operandLength + opCodeLength).ToString() + " 0x" + operand.ToHexString(4) +" -> 0x" + targetAddress.ToHexString(4));
                        method[index-2] = byte(targetAddress & 0xFF);
                        method[index-1] = byte(targetAddress >> 8);
                    }
                }
                case OpCode.JP_nn:
                case OpCode.JP_Z_nn:
                case OpCode.JP_NZ_nn:
                {
                    int iJumpOffset = Int.FromBytes(operand.GetByte(0), operand.GetByte(1));
                    uint lTargetAddress = long(instructionAddress) + long(iJumpOffset) + opCodeLength + operandLength; 
                    uint targetAddress = methodAddress + uint(lTargetAddress);
                    //PrintLn();
                    //Print("    0x" + instructionAddress.ToHexString(4) + " " + name + " " + (operandLength + opCodeLength).ToString() + " " + iJumpOffset.ToString() + " -> 0x" + targetAddress.ToHexString(4));
                    method[index-2] = byte(targetAddress & 0xFF);
                    method[index-1] = byte(targetAddress >> 8);
                }
                case OpCode.DDDD:
                {
                    expectDelegate = true;
                }
            }
        } // loop  
        
        //PrintLn();
        //Print("  convertToAbsolute: 0x" + methodIndex.ToHexString(4) + " " + (method.Count).ToString());   
    }
}
