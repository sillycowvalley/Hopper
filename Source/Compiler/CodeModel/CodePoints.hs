unit CodePoints
{
    uses "/Source/System/String"
    
    uses "../JSON/Code"
    uses "../CodeGen/CodeStream"
    uses "../CodeGen/Instructions"
    uses "../Tokens/SysCalls"
    uses "../Tokens/LibCalls"
    uses "ModelUtilities"
    
    <uint,uint> inlineMethodCandidates;   
    
    byte iLongMul;
    byte iLongAdd;
    byte iLongSub;
    byte iFloatMul;
    byte iFloatAdd;
    byte iUIntToLong;
    byte iIntToLong;
    byte iLongMulRef;
    byte iLongAddRef;
    byte iLongInc;
    byte iLongAddB;
    byte iLongSubB;
    
    byte iStringTrim;
    byte iStringTrimLeft;
    byte iStringToUpper;
    byte iStringToLower;
    byte iStringSubstring;
    byte iStringAppend;
    byte iStringInsertChar;
    byte iStringBuild;
    byte iStringBuildFront;
    
    LoadSysCallIndices()
    {
        if (!SysCalls.TryParseSysCall("Long.Mul", ref iLongMul)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Long.Add", ref iLongAdd)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Long.Sub", ref iLongSub)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Float.Mul", ref iFloatMul)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Float.Add", ref iFloatAdd)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("UInt.ToLong", ref iUIntToLong)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Int.ToLong", ref iIntToLong)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Long.MulRef", ref iLongMulRef)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Long.AddRef", ref iLongAddRef)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Long.Inc", ref iLongInc)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Long.AddB", ref iLongAddB)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Long.SubB", ref iLongSubB)) { Die(0x0B); }
        
        if (!SysCalls.TryParseSysCall("String.Trim", ref iStringTrim)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("String.TrimLeft", ref iStringTrimLeft)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("String.ToUpper", ref iStringToUpper)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("String.ToLower", ref iStringToLower)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("String.Substring", ref iStringSubstring)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("String.Append", ref iStringAppend)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("String.Build", ref iStringBuild)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("String.BuildFront", ref iStringBuildFront)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("String.InsertChar", ref iStringInsertChar)) { Die(0x0B); }
        
    }
    
    Reset()
    {
        inlineMethodCandidates.Clear();
    }
    bool InlineCandidatesExist { get { return inlineMethodCandidates.Count != 0; } }
    
    <Instruction> iCodes;
    <uint>        iLengths;
    <uint>        iOperands;    // original
    < <byte> >    iContent;     // wide instruction content
    < <uint> >    iJumpTargets; // where can this instruction jump to?
    
    <uint,bool>   iReachable;
    
    <uint, <byte, uint> > iJIXMaps;
    
    uint currentMethod;
    <uint,string> indexDebugInfo;
    <uint,uint> indexMap;   // index -> address
    <uint,uint> addressMap; // address -> index
    
    bool IsTargetOfJumps(uint iIndex)
    {
        foreach (var v in iJumpTargets)
        {
            <uint> jumps = v;
            foreach (var jump in jumps)
            {
                if (jump == iIndex)
                {
                    return true;
                }
            }
        }
        foreach (var kv in iJIXMaps)
        {
            foreach (var kv2 in kv.value)
            {
                uint jump = kv2.value;
                if (jump == iIndex)
                {
                    return true;
                }       
            }
        }
        ProgessNudge();
        return false;
    }
    
    DumpDebugInfo()
    {
        PrintLn("DebugInfo:");
        foreach (var kv in indexDebugInfo)
        {
            PrintLn("    " + (kv.key).ToString() + " -> " + kv.value);
        }
    }
    
    RemoveInstruction(uint iRemoval)
    {
        RemoveInstruction(iRemoval, true);
    }
    RemoveInstruction(uint iRemoval, bool keepDebugLine)
    {
        // lists:
        
        iCodes.Remove(iRemoval);
        iLengths.Remove(iRemoval);
        iOperands.Remove(iRemoval);
        iContent.Remove(iRemoval);
        iJumpTargets.Remove(iRemoval);
        
        // iReachable
        <uint,bool>   newReachable;
        foreach (var kv in iReachable)
        {
            uint iR = kv.key;
            if (iR == iRemoval)
            {
                // gone
            }
            else 
            {
                if (iR > iRemoval)
                {
                    iR--;
                }
                newReachable[iR] = kv.value;
            }
        }
        iReachable = newReachable;
        
        // iJIXMaps
        <uint, <byte, uint> > newJIXMaps;
        foreach (var kv in iJIXMaps)
        {
            uint iX = kv.key;
            if (iX ==  iRemoval)
            {
                // gone
            }
            else 
            {
                <byte, uint> newJIXMap;
                <byte, uint> iJIXMap = kv.value;
                foreach (var kv2 in iJIXMap)
                {
                    byte switchByte = kv2.key;
                    uint switchJump = kv2.value;
                    if (switchJump > iRemoval)
                    {
                        switchJump--;
                    }
                    newJIXMap[switchByte] = switchJump;
                }
                if (iX > iRemoval)
                {
                    iX--;
                }
                newJIXMaps[iX] = newJIXMap;           
            }
        }
        iJIXMaps = newJIXMaps;
        
        // indexDebugInfo
        <uint,string> newDebugInfo;
        
        foreach (var kv in indexDebugInfo)
        {
            uint iD          = kv.key;   // instruction index
            string debugLine = kv.value; // debug source location
            
            // 3 scenarios:
            //   a) instruction index is before iRemoval
            //   b) instruction index is iRemoval
            //   c) instruction index after iRemoval
            
            if (iD < iRemoval)
            {
                // a) trivial:
                newDebugInfo[iD] = debugLine;
            }
            else if ((iD == iRemoval) && keepDebugLine)
            {
                // b) absorb the debugLine from the next instruction
                if (indexDebugInfo.Contains(iD))
                {
                    newDebugInfo[iD] = debugLine;
                }
            }
            else  // (iD > iRemoval)
            {
                // c)
                iD--;
                if (newDebugInfo.Contains(iD))
                {
                    // keep the earlier line
                    // TODO : assumes keys are iterated in order
                }
                else
                {
                    newDebugInfo[iD] = debugLine;
                }
            }
        }
        indexDebugInfo = newDebugInfo;
        
        // update all jumpTargets:
        uint iCodeLength = iCodes.Count;
        for (uint iIndex = 0; iIndex < iCodeLength; iIndex++)
        {
            <uint> jumpTargets = iJumpTargets[iIndex];
            <uint> newJumpTargets;
            foreach (var v in jumpTargets)
            {
                uint jumpTarget = v;
                if (jumpTarget > iRemoval)
                {
                    jumpTarget--;
                }
                newJumpTargets.Append(jumpTarget);
            }
            iJumpTargets.SetItem(iIndex, newJumpTargets);
        }
        ProgessNudge();
    }
    
    
    bool TryGetInstructionIndex(uint seekAddress, ref uint index)
    {
        uint address;
        index = 0;
        bool found;
        loop
        {
            if (address == seekAddress)
            {
                found = true;
                break;
            }
            if (index >= iLengths.Count)
            {
                break;
            }
            address = address + iLengths[index];
            index++;
        }
        return found;
    }
    
    uint GetInstructionAddress(uint seekIndex)
    {
        uint address;
        uint iIndex;
        loop
        {
            if (iIndex == seekIndex)
            {
                break;
            }
            address = address + iLengths[iIndex];
            iIndex++;
        }
        return address;
    }
    RebuildMaps()
    {
        indexMap.Clear();   // index -> address
        addressMap.Clear(); // address -> index
        
        uint address;
        uint index;
        loop
        {
            if (index >= iLengths.Count)
            {
                break;
            }
            indexMap[index] = address;
            addressMap[address] = index;
            address = address + iLengths[index];
            index++;
        }
    }
    
    InitializeJumpTargets()
    {
        uint iCodesLength = iCodes.Count;
        uint iIndex;
        uint instructionAddress;
        
        loop
        {
            if (iIndex == iCodesLength)
            {
                break;
            }
            Instruction opCode  = iCodes[iIndex];
            if (IsJumpInstruction(opCode))
            {
                // single jump target
                uint instructionLength = iLengths[iIndex];
                uint operand           = iOperands[iIndex];
                
                long offset;
                long jumpTargetAddress;
            
                if (instructionLength == 2)
                {
                    offset = operand;
                    if (offset > 127)
                    {
                        offset = offset - 256; // 255 -> -1
                    }
                    jumpTargetAddress = long(instructionAddress) + offset;
                }
                else if (instructionLength == 3)
                {
                    // operandWidth == 2
                    offset = operand;
                    if (offset > 32767)
                    {
                        offset = offset - 65536; // 0x10000 -> -1
                    }
                    jumpTargetAddress = long(instructionAddress) + offset;
                }
                else
                {
                    Die(0x0B); 
                }
                uint jumpIndex;
                if (addressMap.Contains(jumpTargetAddress))
                {
                    jumpIndex = addressMap[jumpTargetAddress];
                }
                else
                {
                    // Most likely unreachable dead code so jumping to self
                    // does not mess up reachableness.
                    jumpIndex = iIndex;
                }
                
                <uint> jumpTargets;
                jumpTargets.Append(jumpIndex);
                iJumpTargets.SetItem(iIndex, jumpTargets);
            }
            else if (IsJumpIXInstruction(opCode))
            {
                // multiple targets from table
                <uint> jumpTargets;
                <byte, uint> jixMap;
                
                uint instructionLength = iLengths[iIndex];
                uint operand           = iOperands[iIndex];
                <byte> jumpTable       = iContent[iIndex];
                
                uint negativeOffset = jumpTable[0] + (jumpTable[1] << 8);
                
                uint myAddress   = indexMap[iIndex];
                uint baseAddress = myAddress - negativeOffset;
                uint iBase = addressMap[baseAddress];
                
                jumpTargets.Append(iBase);    // add iBase so it does not get removed (and so we can rebuild)
                jumpTargets.Append(iIndex+1); // default is the next instruction
                
                byte offsetSize = 1;
                if (opCode == Instruction.JIX)
                {
                    offsetSize = 2;
                }
                
                byte switchCase = byte(operand & 0xFF);
                uint iOffset = 2;
                uint jumpTableLength = jumpTable.Count;
                loop
                {
                    if (iOffset == jumpTableLength)
                    {
                        break;
                    }
                    uint offset = jumpTable[iOffset];
                    if (offsetSize == 2)
                    {
                        offset = offset + (jumpTable[iOffset + 1] << 8);
                    }
                    if (offset != 0) // non-default
                    {
                        uint jumpTarget = addressMap[baseAddress + offset];
                        if (!jumpTargets.Contains(jumpTarget))
                        {
                            jumpTargets.Append(jumpTarget);
                        }
                        jixMap[switchCase] = jumpTarget;
                    }
                    iOffset = iOffset  + offsetSize;
                    switchCase++;
                } // loop
                
                iJumpTargets.SetItem(iIndex, jumpTargets);
                iJIXMaps[iIndex] = jixMap;
            }
            instructionAddress += iLengths[iIndex];
            iIndex++;
        }
    }
    AppendJIXInstruction(uint iIndex, <byte> code)
    {
        // at this point only the opCode has been appended
        uint myAddress      = code.Count-1;
        
        Instruction opCode  = iCodes[iIndex];
        uint operand        = iOperands[iIndex];
        <byte, uint> jixMap = iJIXMaps[iIndex];
        <uint> jumpTargets  = iJumpTargets[iIndex];
        uint iBase          = jumpTargets[iBase];
        uint baseAddress    = GetInstructionAddress(iBase);
        uint negativeOffset = myAddress - baseAddress;
        
        byte minRange = byte(operand & 0xFF);
        byte maxRange = byte(operand >> 8);
        code.Append(minRange);
        code.Append(maxRange);
        
        code.Append(byte(negativeOffset & 0xFF));
        code.Append(byte(negativeOffset >> 8));
        
        byte offsetSize = 1;
        if (opCode == Instruction.JIX)
        {
            offsetSize = 2;
        }
        
        for (uint iOffset = minRange; iOffset <= maxRange; iOffset++)
        {
            byte switchKey = byte(iOffset);
            uint caseOffset = 0;
            if (jixMap.Contains(switchKey))
            {
                uint iCase = jixMap[switchKey];
                uint iAddress = GetInstructionAddress(iCase);
                caseOffset = iAddress - baseAddress;
            }
            code.Append(byte(caseOffset & 0xFF));
            if (offsetSize == 2)
            {
                code.Append(byte(caseOffset >> 8));
            }
        }
    }
    
#ifdef DIAGNOSTICS
    DumpInstructions(string description)
    {
        uint icodesLength = iCodes.Count;
        PrintLn(description + ", length=" + icodesLength.ToString());

        uint iIndex = 0;
        uint count = 0;
        uint address = 0;
        loop
        {
            if (iIndex == icodesLength)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            string content = "  0x" + address.ToHexString(4) + " [i"+ iIndex.ToString() + "] ";
            content = content.Pad(' ', 18);
            content = content + Instructions.ToString(opCode);
            content = content.Pad(' ', 32);
            Print(content);
            uint iLength = iLengths[iIndex];
            address += iLength;
            if (iLength == 2)
            {
                Print(" 0x" + (iOperands[iIndex]).ToHexString(2));
            }
            else if (iLength == 3)
            {
                Print(" 0x" + (iOperands[iIndex]).ToHexString(4));
            }
            Print(" (" + iLength.ToString() + ")");
            PrintLn();
            count++;
            if (count == 20)
            {
                Key k = ReadKey();
                count = 0;
            }
            iIndex++;
        }
        if (count > 0)
        {
            Key k = ReadKey();
        }
    }
#endif     
    
    uint Load(uint methodIndex, string when)
    {
        currentMethod = methodIndex;
        
        iCodes.Clear();
        iLengths.Clear();
        iOperands.Clear();
        iContent.Clear();
        iJumpTargets.Clear();
        indexDebugInfo.Clear();
        iJIXMaps.Clear();
        iReachable.Clear();
        
        <byte> code = Code.GetMethodCode(currentMethod);
        uint codeLength = code.Count;
        uint i = 0;
        loop
        {
            uint operand;
            uint jumpTableSize;
            <byte> extraContent;
            <uint> jumpTargets;
            Instruction opCode = Instruction(code[i]);
            byte operandWidth = Instructions.GetSimpleOperandWidth(opCode);
            switch (operandWidth)
            {
                case 1:
                {
                    operand = code[i+1];
                }
                case 2:
                {
                    operand = code[i+1] + (code[i+2] << 8);
                    jumpTableSize = AddJIXTableSize(opCode, operand);
                }
            }
            uint instructionLength = 1 + operandWidth + jumpTableSize;
            
            if (jumpTableSize > 0)
            {
                uint iContent = i + 1 + operandWidth;
                for (uint j = 0; j < jumpTableSize; j++)
                {
                    extraContent.Append(code[iContent]);
                    iContent++;
                }
            }
            
            iCodes.Append(opCode);
            iLengths.Append(instructionLength);
            iOperands.Append(operand);
            iContent.Append(extraContent);
            iJumpTargets.Append(jumpTargets);     // empty for now
            
            i = i + instructionLength;
            if (i == codeLength)
            {
                break;
            }
        } // loop
        
        RebuildMaps();
        
        InitializeJumpTargets();
        
        // convert the debugInfo from instruction addresses to instruction indices
        <string,string> debugInfo = Code.GetMethodDebugInfo(currentMethod);
        
        foreach (var kv in debugInfo)
        {
            string saddress = kv.key;
            uint address;
            if (UInt.TryParse(saddress, ref address))
            {
            }      
            if (address < code.Count) // why?
            {
                uint index;
                if (!addressMap.Contains(address)) // address -> index
                {
                    PrintLn();
                    PrintLn("Bad DebugInfo for method 0x" + currentMethod.ToHexString(4) + " : " + kv.key + "->" + kv.value + " " + when);
                }
                else
                {
                    indexDebugInfo[addressMap[address]] = kv.value;
                }
            }
        }
        ProgessNudge();
        return codeLength;
    }
    uint Save()
    {
        <byte> code;
        uint instructionCount = iCodes.Count;
        uint index;
        
        RebuildMaps();
        
        loop
        {
            Instruction opCode = iCodes[index];
            code.Append(byte(opCode));
            
            uint instructionLength = iLengths[index];
            if (instructionLength > 3)
            {
                if (IsJumpIXInstruction(opCode))
                {
                    AppendJIXInstruction(index, code);
                }
                else
                {
                    Die(0x0B);
                }
            }
            else if (instructionLength > 1)
            {
                uint operand = iOperands[index];
                if (Instructions.OperandIsAddressOffset(opCode))
                {
                    // use the index to calculate the new offset
                    
                    //long currentAddress = GetInstructionAddress(index);
                    long currentAddress = indexMap[index];
                    <uint> jumpTargets  = iJumpTargets[index];
                    uint jumpTarget     = jumpTargets[0];
                    //long jumpAddress    = GetInstructionAddress(jumpTarget);
                    long jumpAddress;
                    if (indexMap.Contains(jumpTarget))
                    {
                        jumpAddress = indexMap[jumpTarget];
                    }
                    else
                    {
                        if (iReachable[index])
                        {
                            Die(0x0B); // a reachable jump instruction has a target not in the map!?
                        }
                        jumpAddress = currentAddress; // jump to self, don't mess with reachability
                    }
                    
                    long offset = jumpAddress - currentAddress;
                    if (offset < 0)
                    {
                        if (instructionLength == 2)
                        {
                            if ((offset < -128) || (offset > 127))
                            {
                                //PrintLn(currentAddress.ToString() + " " + jumpAddress.ToString() + " " + offset.ToString()); 
                                PrintLn("Bad Jump:  " + offset.ToString());
                            }
                            offset = offset + 256;
                        }
                        else
                        {
                            offset = offset + 65536;
                        }
                    }
                    operand = uint(offset);
                }
                
                byte lsb = byte(operand & 0xFF);
                byte msb = byte(operand >> 8);
                code.Append(lsb);
                if (instructionLength > 2)
                {
                    code.Append(msb);
                }
                else if (msb != 0)
                {
                    Die(0x0B); // why is MSB != 0 for instructionLength == 2?
                }
            }
            
            index++;
            if (index == instructionCount)
            {
                break;
            }
        }
        
        // convert the indexDebugInfo back from instruction indices to instruction addresses
        <string,string> debugInfo;
        foreach (var kv in indexDebugInfo)
        {
            index = kv.key;
            if (indexMap.Contains(index)) // why?
            {
                uint address = indexMap[index];
                debugInfo[address.ToString()] = kv.value;
            }
        }
        
        // update the method with the optimized code            
        Code.SetMethodCode(currentMethod, code);
        Code.SetMethodDebugInfo(currentMethod, debugInfo);
        ProgessNudge();
        
        return code.Count;
    }
    
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
    
    Dump(uint pass, bool doReachable)
    {
        if (doReachable && (iReachable.Count == 0))
        {
            MarkReachableInstructions();
        }
        
        <string,variant> methodSymbols = Code.GetMethodSymbols(currentMethod);
        string srcPath = methodSymbols["source"];
        string srcName = Path.GetFileName(srcPath);
        string methodName = methodSymbols["name"];

        string path = "/Debug/Obj/methodcode-" + methodName  + pass.ToString() + ".txt";
        File.Delete(path);
        
        file dumpFile = File.Create(path);
        
        string mname = "// ####  " + methodName + "(..)  ####";
        mname = mname.Pad(' ', 80);
        mname = mname + currentMethod.ToString() + char(0x0A);
        dumpFile.Append(mname);  
        dumpFile.Append("" + char(0x0A)); 
        
        uint icodesLength = iCodes.Count;
        uint iIndex;
        loop
        {
            if (iIndex == icodesLength)
            {
                break;
            }
            
            Instruction opCode = iCodes[iIndex];
            string content;
            if (!iReachable[iIndex])
            {
                content = "// ";
            }
            else
            { 
                content = "   ";
            }
            if (IsTargetOfJumps(iIndex))
            {
                content = content + "->";
            }
            else
            {
                content = content + "  ";
            }
            string iContent = "i" + iIndex.ToString();
            iContent = iContent.LeftPad(' ', 8);
            content = content + iContent;
            
            content = content + " " + Instructions.ToString(opCode);
            content = content.Pad(' ', 20);
            
            uint instructionLength = iLengths[iIndex];
            <uint> jumpTargets = iJumpTargets[iIndex];
            if (jumpTargets.Count != 0)
            {
                foreach (var jt in jumpTargets)
                {
                    content = content + " i" + jt.ToString();
                }
            }
            else
            {
                uint operand = iOperands[iIndex];
                if (instructionLength == 1)
                {
                    
                }
                else if (instructionLength == 2)
                {
                    content = content + " 0x" + operand.ToHexString(2);
                }
                else if (instructionLength == 3)
                {
                    content = content + " 0x" + operand.ToHexString(4);
                }
                else
                {
                    //Die(0x0B);
                }
            }
            
            if (indexDebugInfo.Contains(iIndex))
            {
                string debugLine = indexDebugInfo[iIndex];
                
                dumpFile.Append("" + char(0x0A));
                string debugContent = "// " + srcPath + ":" + debugLine;
                dumpFile.Append(debugContent + char(0x0A));  
                string sourceLine = GetSourceLine(srcPath, debugLine);
                sourceLine = sourceLine.Trim();
                dumpFile.Append(sourceLine + char(0x0A));  
                dumpFile.Append("" + char(0x0A)); 
            }
            
            dumpFile.Append(content + char(0x0A));
            
            iIndex++;
        }
        dumpFile.Flush();
    }
    
    
    CollectMethodCalls(<uint,bool> methodsCalled)
    {
        uint iIndex;
        uint icodesLength = iCodes.Count;
        loop
        {
            if (iIndex == icodesLength)
            {
                break;
            }
            if (iReachable[iIndex])
            {
                Instruction opCode = iCodes[iIndex];
                if (    (opCode == Instruction.CALL)  || (opCode == Instruction.CALLB)  // method called
                     || (opCode == Instruction.PUSHD) || (opCode == Instruction.PUSHDB) // method referenced by delegate
                   )
                {
                    uint callMethodIndex = iOperands[iIndex];
                    if (!methodsCalled.Contains(callMethodIndex))
                    {
                        // Die(0x0B); //  why are we calling a method that we don't have code for anymore?
                    }
                    else
                    {
                        if (Target6502|| TargetMinimal) // delegates still use this
                        {
                            callMethodIndex = callMethodIndex & 0x3FFF;
                        }
                        methodsCalled[callMethodIndex] = true; // reachable from currentMethod
                    }
                }
            }
        
            iIndex++;
        } // loop
        ProgessNudge();
    }
    MarkReachableInstructions()
    {
        uint iIndex = 0;
        uint icodesLength = iCodes.Count;
        
        // reset
        iReachable.Clear();
        for (uint i = 0; i < icodesLength; i++)
        {
            iReachable[i] = false;
        }
        
        <uint> tailCalls;
        
        if ((icodesLength == 0) || (iIndex >= icodesLength))
        {
            return; // empty method?
        }
        loop // tailCalls
        {
            loop
            {
                if (iReachable[iIndex])
                {
                    break;
                }
                iReachable[iIndex] = true;
                Instruction opCode = iCodes[iIndex];
                if (IsConditionalJumpInstruction(opCode))
                {
                    // conditional branch: explore both paths
                    bool takeJump = true;
                    bool mainPath = true;
                    if (iIndex > 0)
                    {
                        Instruction opCodePrev = iCodes[iIndex-1];
                        if ((opCodePrev == Instruction.PUSHI0) || (IsExperimental && (opCodePrev == Instruction.ENTERB)))
                        {
                            switch (opCode)
                            {
                                case Instruction.JZ:
                                case Instruction.JZB:
                                {
                                    // always branch
                                    mainPath = false;
                                }
                                case Instruction.JNZ:
                                case Instruction.JNZB:
                                {
                                    // never branch
                                    takeJump = false;
                                }
                            }
                        }
                        else if (opCodePrev == Instruction.PUSHI1)
                        {
                            switch (opCode)
                            {
                                case Instruction.JZ:
                                case Instruction.JZB:
                                {
                                    // never branch
                                    takeJump = false;
                                }
                                case Instruction.JNZ:
                                case Instruction.JNZB:
                                {
                                    // always branch
                                    mainPath = false;
                                }
                            }
                        }
                    }
                    if (takeJump)
                    {
                        <uint> jumpTargets = iJumpTargets[iIndex];
                        uint iJumpTarget = jumpTargets[0];
                        tailCalls.Append(iJumpTarget);
                    }
                    if (!mainPath)
                    {
                        break; // end of this path   
                    }
                    // fall through to 'continue' on the non-branch path ..
                }
                else if ((opCode == Instruction.JIXB) || (opCode == Instruction.JIX))
                {
                    // JIX : always assume all the options are possible
                    <uint> jumpTargets = iJumpTargets[iIndex];
                    foreach(var iJumpTarget in jumpTargets)
                    {
                        tailCalls.Append(iJumpTarget);
                    }
                    // fall through to 'continue' on the non-branch path ..
                }
                else if ((opCode == Instruction.J) || (opCode == Instruction.JB))
                {
                    // unconditional branch: continue on the branch path only
                    <uint> jumpTargets = iJumpTargets[iIndex];
                    iIndex = jumpTargets[0];
                    continue;
                }
                else if (IsMethodExitInstruction(opCode))
                {
                    break; // end of this path
                }
                iIndex++;
            } // loop
            
            iIndex = 0;
            while (tailCalls.Count != 0)
            {
                iIndex = tailCalls[0];
                tailCalls.Remove(0);
                if (!iReachable[iIndex])
                {
                    break; // not visited yet, do it ..
                }
            }
            if (iIndex == 0)
            {
                break;
            }
        } // loop tailCalls
        ProgessNudge();
    }
    
    bool OptimizeDECSPRET()
    {
        if (iCodes.Count < 3)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 2;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode1 = iCodes[iIndex-1];  // DECSP
            Instruction opCode0 = iCodes[iIndex];    // RET0 | RETB
            if (((opCode0 == Instruction.RETB) || (opCode0 == Instruction.RET0)) 
              && (opCode1 == Instruction.DECSP)
            )
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    uint decCount = iOperands[iIndex-1];
                    if (opCode0 == Instruction.RETB)
                    {
                        decCount = decCount + iOperands[iIndex];
                    }
                    if (decCount <= 255)
                    {
                        iCodes.SetItem   (iIndex-1, Instruction.RETB);
                        iOperands.SetItem(iIndex-1, decCount);
                        iLengths.SetItem (iIndex-1, 2);
                        RemoveInstruction(iIndex);
                        modified = true;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    bool OptimizeENTERPUSHI0()
    {
        if (iCodes.Count < 2)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex == iCodes.Count)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            if ((opCode == Instruction.ENTERB) || (opCode == Instruction.ENTER))
            {
                
                uint zeroCount = 0;
                if (opCode == Instruction.ENTERB)
                {
                    zeroCount = iOperands[iIndex];
                    if (zeroCount >= 255)
                    {
                        break; // ENTERB is already 'full'
                    }
                }
                uint eIndex = iIndex;
                uint removals = 0;
                iIndex++;
                while ((iIndex < iCodes.Count) && (iCodes[iIndex] == Instruction.PUSHI0))
                {
                    if (IsTargetOfJumps(iIndex))
                    {
                        break;
                    }
                    removals++;   
                    iIndex++; 
                }
                
                if (removals > 0)
                {
                    zeroCount = zeroCount + removals;
                    iOperands.SetItem(eIndex, zeroCount);
                    iCodes.SetItem(eIndex, Instruction.ENTERB);
                    iLengths.SetItem(eIndex, 2);
                    while (removals > 0)
                    {
                        RemoveInstruction(eIndex+1);
                        removals--;
                    }
                    modified = true;
                }
                break;
            }
            iIndex++;
        } // loop
        
        return modified;
    }
    
    // remove ENTER and use RETFAST if there are no locals, no arguments and no CALLs
    bool OptimizeFrameRemoval()
    {
        if (iCodes.Count < 2)
        {
            return false;
        }
        bool modified = false;
        
        
        uint iIndex = 0;
        bool notSuitable = (currentMethod == 0); // don't mess with 'main' (which is always methodIndex == 0)
        uint hasRET0;
        
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            if (   IsLOCALInstruction( opCode)
                 || (opCode == Instruction.CALL)
                 || (opCode == Instruction.CALLB)
                 || (opCode == Instruction.CALLREL)
               )
            {
                notSuitable = true;
                break;
            }
            else if (opCode == Instruction.ENTERB)
            {
                uint operand = iOperands[iIndex];
                if (operand != 1) // ENTERB 0x01 can be replaced by PUSHI0
                {
                    notSuitable = true;
                    break;
                }
            }
            else if (opCode == Instruction.RET0)
            {
                hasRET0++;
            }
            else if (opCode == Instruction.DIE)
            {
                // Don't care
            }
            else if (IsMethodExitInstruction(opCode))
            {
                notSuitable = true; // RETxx other than RET0
                break;
            }
            iIndex++;
        } // loop    
        
        if (!notSuitable && (hasRET0 > 0))
        {
            Instruction opCode0 = iCodes[0];
            if (opCode0 == Instruction.ENTER)
            {
                iIndex = 1;
                uint byteLength = 0;
                uint retIndex = 0;
                loop
                {
                    if (iIndex >= iCodes.Count)
                    {
                        break;
                    }
                    Instruction opCode = iCodes[iIndex];
                    
                    byteLength = byteLength + iLengths[iIndex];
                    if (opCode == Instruction.RET0)
                    {
                        iCodes.SetItem(iIndex, Instruction.RETFAST);
                        retIndex = iIndex;
                        
                        // check for TESTBPB
                        if (iIndex > 0)
                        {
                            Instruction opCode1 = iCodes[iIndex-1];
                            if (opCode1 == Instruction.TESTBPB)
                            {
                                RemoveInstruction(iIndex-1);    
                                iIndex--;
                                retIndex = iIndex;   
                                byteLength = byteLength - 2; 
                            }
                        }
                    }
                    
                    iIndex++;
                } // loop
                
                RemoveInstruction(0); // ENTER
                modified = true;
                
                if ((hasRET0 == 1) && (retIndex == iCodes.Count)) 
                {
                    if (byteLength <= 4)
                    {
                        // only one RETxx that is the last instruction
                        // 4 because it won't need the RETFAST when it is inlined
                        inlineMethodCandidates[currentMethod] = byteLength-1;
                    }
                    //else if (byteLength <= 6)
                    //{
                    //    Print(" " + currentMethod.ToHexString(4) + ":" + byteLength.ToString() + " ");
                    //}
                }    
                
            } // ENTER
        }
        return modified;
    }
    bool ReplaceMergedRET0(<byte> rawCode)
    {
        bool replaced;
        uint iCodesLength = iCodes.Count;
        if (iCodesLength < 2)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex >= iCodesLength)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            if (opCode == Instruction.MERGEDRET0)
            {
                uint instructionAddress = GetInstructionAddress(iIndex);
                rawCode.SetItem(instructionAddress, byte(Instruction.RET0));
                //Print("Replaced: " + currentMethod.ToHexString(4) + ":" + instructionAddress.ToString() + " ");
                replaced = true;
            } 
            iIndex++;
        }   
        return replaced;
    }
    bool InlineSmallMethods(<byte> rawCode)
    {
        uint iCodesLength = iCodes.Count;
        if (iCodesLength < 2)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex >= iCodesLength)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            if ((opCode == Instruction.CALL) || (opCode == Instruction.CALLB))
            {
                uint callMethodIndex = iOperands[iIndex];
                if (Target6502 || TargetMinimal)
                {
                    callMethodIndex = callMethodIndex & 0x3FFF;
                }
                foreach (var kv in inlineMethodCandidates)
                {
                    uint methodIndex = kv.key;
                    if (callMethodIndex == methodIndex)
                    {
                        uint sizeInBytes = kv.value;
                        uint availableSpace = iLengths[iIndex];
                        if (sizeInBytes <= availableSpace)
                        {
                            uint inlineAddress = GetInstructionAddress(iIndex);
                            <byte> inlineCode = Code.GetMethodCode(methodIndex);
                            
                            Instruction retFast = Instruction(inlineCode[inlineCode.Count-1]);
                            if (retFast == Instruction.RETFAST)
                            {
                                // just so this never ends up inlined
                                inlineCode[inlineCode.Count-1] = byte(Instruction.NOP);
                            }
                            
                            // replace what was the method call with NOPs
                            for (uint i=0; i < availableSpace; i++)
                            {
                                rawCode.SetItem(inlineAddress+i, byte(Instruction.NOP));
                            }
                            
                            // put the NOPs in front so that unconditional branches with PUSHI0 or PUSHI1 are spotted sooner
                            uint nopOffset = availableSpace - sizeInBytes;
                            
                            // fill the space with the bytes from the method (could be trailing NOPs to remove later)
                            while (inlineCode.Count < sizeInBytes)
                            {
                                inlineCode.Append(byte(Instruction.NOP));
                            }
                            
                            for (uint i=0; i < sizeInBytes; i++)
                            {
                                byte bCode = inlineCode[i];
                                rawCode.SetItem(inlineAddress+i+nopOffset, bCode);
                            }
                            modified = true;
                        }
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeRemoveUnreachable()
    {
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            bool removeInstruction = !iReachable[iIndex];
            if (removeInstruction)
            {
                RemoveInstruction(iIndex, false); // good
                modified = true;
            }
            else
            {
                iIndex++;
            }
        } // loop
        return modified;
    }
    
    removePUSHI0orPUSHI1orReduceENTERB(uint iIndex)
    {
        if (iCodes[iIndex] == Instruction.ENTERB)
        {
            // update the ENTERB
            uint b = iOperands[iIndex];
            if (b == 1)
            {
                iCodes.SetItem   (iIndex, Instruction.ENTER);
                iLengths.SetItem (iIndex, 1);
            }
            else
            {
                iOperands.SetItem(iIndex, b-1);
            }
        }
        else if ((iCodes[iIndex] == Instruction.PUSHI0) || (iCodes[iIndex] == Instruction.PUSHI1))
        {
            // remove the PUSHI0 or PUSHI1
            iCodes.SetItem(iIndex, Instruction.NOP);
        }
        else
        {
            PrintLn(Instructions.ToString(iCodes[iIndex]));
            Die(0x0B);
        }
    }
    
    bool OptimizeUnconditionalJumps()
    {
        uint iCodesLength = iCodes.Count;
        if (iCodesLength < 2)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex == iCodesLength)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            if (IsConditionalJumpInstruction(opCode) && !IsTargetOfJumps(iIndex-0))
            {
                bool jumpPathIsValid = true;
                bool mainPathIsValid = true;
                bool invertJump = false;
                bool simplifyJump = false;
                byte nopCount = 0;
                Instruction opCodePrev     = iCodes[iIndex-1];
                Instruction opCodePrevPrev = Instruction.DIE; // unknown
                
                bool opCodePrevIsEnterB   = (opCodePrev == Instruction.ENTERB);
                bool opCodePrevIsZero     = opCodePrevIsEnterB || (opCodePrev == Instruction.PUSHI0);
                bool opCodePrevPrevIsEnterB;
                bool opCodePrevPrevIsZero;
                if ((iIndex >= 2) && !IsTargetOfJumps(iIndex-1))
                {
                    opCodePrevPrev = iCodes[iIndex-2];
                    opCodePrevPrevIsEnterB = (opCodePrevPrev == Instruction.ENTERB);
                    opCodePrevPrevIsZero   = opCodePrevPrevIsEnterB || (opCodePrevPrev == Instruction.PUSHI0);
                }
                if (opCodePrevIsZero)
                {
                    switch (opCode)
                    {
                        case Instruction.JZ:
                        case Instruction.JZB:
                        {
                            // always branch
                            mainPathIsValid = false;
                        }
                        case Instruction.JNZ:
                        case Instruction.JNZB:
                        {
                            // never branch
                            jumpPathIsValid = false;
                        }
                    }
                }
                else if (opCodePrev == Instruction.PUSHI1)
                {
                    switch (opCode)
                    {
                        case Instruction.JZ:
                        case Instruction.JZB:
                        {
                            // never branch
                            jumpPathIsValid = false;
                        }
                        case Instruction.JNZ:
                        case Instruction.JNZB:
                        {
                            // always branch
                            mainPathIsValid = false;
                        }
                    }
                }
                else if (                             (opCodePrev == Instruction.BOOLNOT)
                          || (opCodePrevPrevIsZero && (opCodePrev == Instruction.EQ)) 
                        )
                {
                    switch (opCode)
                    {
                        case Instruction.JZ:
                        {
                            opCode = Instruction.JNZ;
                            invertJump = true;
                        }
                        case Instruction.JZB:
                        {
                            opCode = Instruction.JNZB;
                            invertJump = true;
                        }
                        case Instruction.JNZ:
                        {
                            opCode = Instruction.JZ;
                            invertJump = true;
                        }
                        case Instruction.JNZB:
                        {
                            opCode = Instruction.JZB;
                            invertJump = true;
                        }
                    } // switch opCode
                    nopCount = 1;
                    if (opCodePrev == Instruction.EQ)
                    {
                        nopCount = 2;
                    }
                }
                else if (opCodePrevPrevIsZero && (opCodePrev == Instruction.NE))
                {
                    switch (opCode)
                    {
                        case Instruction.JZ:
                        case Instruction.JZB:
                        case Instruction.JNZ:
                        case Instruction.JNZB:
                        {
                            simplifyJump = true;
                        }
                    } // switch opCode
                    nopCount = 2;
                }
                
                if (!jumpPathIsValid)
                {
                    // we never branch - replace the PUSH and the J with NOPs
                    removePUSHI0orPUSHI1orReduceENTERB(iIndex-1);
                    iCodes.SetItem(iIndex,   Instruction.NOP);
                    iLengths.SetItem(iIndex, 1);
                    <uint> emptyTarget;
                    iJumpTargets.SetItem(iIndex, emptyTarget);
                    modified = true;
                }
                else if (!mainPathIsValid)
                {
                    // we always branch - replace the PUSH with NOP and the J with unconditional
                    removePUSHI0orPUSHI1orReduceENTERB(iIndex-1);
                    switch (opCode)
                    {
                        case Instruction.JZ:
                        case Instruction.JNZ:
                        {
                            opCode = Instruction.J;
                        }
                        case Instruction.JZB:
                        case Instruction.JNZB:
                        {
                            opCode = Instruction.JB;
                        }
                    }
                    iCodes.SetItem(iIndex, opCode);
                    modified = true;
                }
                else if (invertJump)
                {
                    // swap the BOOLNOT for a NOP
                    if (nopCount == 2)
                    {
                        removePUSHI0orPUSHI1orReduceENTERB(iIndex-2);
                    }
                    iCodes.SetItem(iIndex-1, Instruction.NOP);
                    iCodes.SetItem(iIndex, opCode);
                    modified = true;
                }
                else if (simplifyJump)
                {
                    if (nopCount == 2)
                    {
                        removePUSHI0orPUSHI1orReduceENTERB(iIndex-2);
                    }
                    iCodes.SetItem(iIndex-1, Instruction.NOP);
                    modified = true;
                }
            } // conditional jump
            iIndex++;
        } // loop
        return modified;    
    }
    
    // not just NOP, also JMP -> JMP + 1, can cause more short JumpToJump's to work
    bool OptimizeRemoveNOPs()
    {
        if (iCodes.Count < 1)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex == iCodes.Count)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            bool removeIt = false;
            if (opCode == Instruction.NOP)
            {
                removeIt = true;
            }
            else if ((opCode == Instruction.JB) || (opCode == Instruction.J))
            {
                <uint> jumpTargets = iJumpTargets[iIndex];
                uint jumpTarget = jumpTargets[0];
                removeIt = jumpTarget == iIndex+1;
            }
            if (removeIt)
            {
                RemoveInstruction(iIndex);
                modified = true;
            }
            else
            {
                iIndex++;
            }
        } // loop
        return modified;
    }
    bool OptimizeCOPYPOP()
    {
        if (iCodes.Count < 2)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 1;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex-1];
            if (opCode == Instruction.COPYNEXTPOP)
            {
                Instruction opCodeNext = iCodes[iIndex];
                if (IsPOPInstruction(opCodeNext))
                {
                    // switcharoo:
                    opCodeNext = SwitchToCOPYPOP(opCodeNext);
                    iCodes.SetItem(iIndex, opCodeNext);
                    
                    RemoveInstruction(iIndex-1); // good
                    modified = true;
                    continue;
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeJumpW()
    {
        if (iCodes.Count < 1)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            if (IsJumpWInstruction(opCode))
            {
                <uint> jumpTargets = iJumpTargets[iIndex];
                uint jumpTarget = jumpTargets[0];
                long myAddress     = GetInstructionAddress(iIndex);
                long targetAddress = GetInstructionAddress(jumpTarget);
                long offset = myAddress - targetAddress;
                if ((offset >= -127) && (offset <= 127))
                {
                    switch (opCode)
                    {
                        case Instruction.J:
                        {
                            opCode = Instruction.JB;
                        }
                        case Instruction.JZ:
                        {
                            opCode = Instruction.JZB;
                        }
                        case Instruction.JNZ:
                        {
                            opCode = Instruction.JNZB;
                        }
                    }
                    iCodes.SetItem(iIndex, opCode);
                    iLengths.SetItem(iIndex, 2);
                    modified = true;
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizePUSHPUSHSWAP()
    {
        if (iCodes.Count < 3)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 2;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode0 = iCodes[iIndex-2];
            Instruction opCode1 = iCodes[iIndex-1];
            Instruction opCode2 = iCodes[iIndex];
            bool pushPushSwap = IsSinglePUSHInstruction(opCode0) 
                             && IsSinglePUSHInstruction(opCode1) 
                             && (opCode2 == Instruction.SWAP);
            if (pushPushSwap && !IsTargetOfJumps(iIndex-1) && !IsTargetOfJumps(iIndex))
            {
                uint length0  = iLengths[iIndex-2];
                uint operand0 = iOperands[iIndex-2];
                uint length1  = iLengths[iIndex-1];
                uint operand1 = iOperands[iIndex-1];
                
                iCodes.SetItem(iIndex-2, opCode1);
                iLengths.SetItem(iIndex-2, length1);
                iOperands.SetItem(iIndex-2, operand1);
                
                iCodes.SetItem(iIndex-1, opCode0);
                iLengths.SetItem(iIndex-1, length0);
                iOperands.SetItem(iIndex-1, operand0);
                
                RemoveInstruction(iIndex);
                modified = true;
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeJumpToJump()
    {
        if (iCodes.Count < 2)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            if (IsJumpInstruction(opCode))
            {
                <uint> jumpTargets = iJumpTargets[iIndex];
                uint jumpTarget = jumpTargets[0];
                if (jumpTarget >= iCodes.Count)
                {
                    // BAD CODEGEN?
                }
                else
                {
                    Instruction opCodeTarget = iCodes[jumpTarget];
                    if (IsUnconditionalJumpInstruction(opCodeTarget))
                    {
                        // set target to jump past the unconditional jump target
                        <uint> targetJumpTargets = iJumpTargets[jumpTarget];
                        uint   targetJumpTarget  = targetJumpTargets[0];
                        
                        long myAddress           = GetInstructionAddress(iIndex);
                        long targetTargetAddress = GetInstructionAddress(targetJumpTarget);
                        long offset = myAddress - targetTargetAddress;
                        if (IsJumpBInstruction(opCode))
                        {
                            if ((offset >= -127) && (offset <= 127))
                            {
                                // safe B cases
                                if (offset != 0) // replacing yourself as a 'modification' would go on forever
                                {
                                    <uint> newJumpTargets;
                                    newJumpTargets.Append(targetJumpTarget);
                                    iJumpTargets.SetItem(iIndex,newJumpTargets); 
                                    modified = true;
                                }
                            }
                            else
                            {
                                // inserting code is harder than it may first appear (knock-on effects ..)
                            }
                        }
                        else
                        {
                            // W cases
                            if (offset != 0) // replacing yourself as a 'modification' would go on forever
                            {
                                <uint> newJumpTargets;
                                newJumpTargets.Append(targetJumpTarget);
                                iJumpTargets.SetItem(iIndex,newJumpTargets); 
                                modified = true;
                            }
                        }
                    } // IsUnconditionalJumpInstruction
                    
                    else if (IsUnconditionalJumpInstruction(opCode) && IsMethodExitInstruction(opCodeTarget))
                    {
                        // JB or J -> RETx  becomes RETx
                        uint jumpOperandWidth   = iLengths[iIndex] - 1;
                        uint targetOperandWidth = iLengths[jumpTarget] - 1;
                        if (targetOperandWidth <= jumpOperandWidth) // inserting code is harder than it may first appear (knock-on effects ..)
                        {    
                            // OperandWidth = 0:
                            //     Instruction.RET0
                            //     Instruction.MERGEDRET0
                            // OperandWidth = 1:
                            //     Instruction.DIE
                            //     Instruction.RETB
                            //     Instruction.RETRESB
                            // OperandWidth = 2
                            //     Instruction.RET
                            //     Instruction.RETRES
                            iCodes.SetItem(iIndex, opCodeTarget);
                            iLengths.SetItem(iIndex, iLengths[jumpTarget]);
                            iOperands.SetItem(iIndex, iOperands[jumpTarget]);
                            modified = true;
                        }
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizePUSHRETRES()
    {
        if (iCodes.Count < 3)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 2;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode0  = iCodes   [iIndex];
            uint        operand0 = iOperands[iIndex];
            if ((opCode0 == Instruction.RETRESB) && (operand0 == 1)) // RETRESB 1
            {
                // When we merge the PUSH and the RETRESB 1 into a single RET0,
                // we need to preserve the fact that this is not a normal RET0:
                // - it is RET0 on the assumption that the return value is already
                //   on the top of the stack.
                // The danger of using RET0 is that it may get merged with other instructions
                // (like DECSP for example : DECSP 1 + RET0 -> RETB 1) which would screw
                // up our assumption about what was pushed to the top of the stack.
                // MERGEDRET0 is a placeholder for RET0 that is a reminder not to touch it with
                // later optimizations. It is replaced with RET0 at the end of optimization.
                
                Instruction opCode1  = iCodes   [iIndex-1];
                uint        operand1 = iOperands[iIndex-1];
                if (opCode1 == Instruction.PUSHLOCALB00)
                {
                    if (!IsTargetOfJumps(iIndex))
                    {
                        // single local
                        iCodes.SetItem   (iIndex, Instruction.MERGEDRET0);
                        iLengths.SetItem (iIndex, 1);
                        RemoveInstruction(iIndex-1); // good
                        MergedRET0Exists = true;
                        modified = true;
                    }
                }  
                else if ((opCode1 == Instruction.PUSHLOCALB) && (operand1 == 0xFF)) // PUSHLOCALB BP-1
                {
                    // returning the only argument, no locals
                    if (!IsTargetOfJumps(iIndex))
                    {
                        iCodes.SetItem   (iIndex, Instruction.MERGEDRET0);
                        iLengths.SetItem (iIndex, 1);
                        RemoveInstruction(iIndex-1); // good
                        MergedRET0Exists = true;
                        modified = true;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizePUSH01LEEQ()
    {
        if (iCodes.Count < 3)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 2;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode1  = iCodes   [iIndex-1];
            Instruction opCode0  = iCodes   [iIndex];
            if (   ((opCode1 == Instruction.PUSHI0) || (opCode1 == Instruction.PUSHI1))
                && ((opCode0 == Instruction.LE)     || (opCode0 == Instruction.EQ))
               )
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    byte       operand = ((opCode1 == Instruction.PUSHI0) ? 0 : 1);
                    Instruction opCode = ((opCode0 == Instruction.LE) ? Instruction.PUSHIBLE : Instruction.PUSHIBEQ);
                    iCodes.SetItem   (iIndex-1, opCode);
                    iOperands.SetItem(iIndex-1, operand);
                    iLengths.SetItem (iIndex-1, 2);
                    RemoveInstruction(iIndex);
                    modified = true;
                }
            }
            if (   ((opCode1 == Instruction.PUSHI0) || (opCode1 == Instruction.PUSHI1))
                && ((opCode0 == Instruction.ADD)    || (opCode0 == Instruction.SUB))
               )
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    byte       operand = ((opCode1 == Instruction.PUSHI0) ? 0 : 1);
                    Instruction opCode = ((opCode0 == Instruction.ADD) ? Instruction.ADDB : Instruction.SUBB);
                    iCodes.SetItem   (iIndex-1, opCode);
                    iOperands.SetItem(iIndex-1, operand);
                    iLengths.SetItem (iIndex-1, 2);
                    RemoveInstruction(iIndex);
                    modified = true;
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeCommutativeSWAP()
    {
        if (iCodes.Count < 3)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 1;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex-1];
            if (opCode == Instruction.SWAP)
            {
                bool isCommutative = false;
                Instruction opCodeNext = iCodes[iIndex];
                if ((opCodeNext == Instruction.ADD) || (opCodeNext == Instruction.ADDI) ||
                    (opCodeNext == Instruction.MUL) || (opCodeNext == Instruction.MULI)
                   )
                {
                    isCommutative = true;
                }
                else if (opCodeNext == Instruction.SYSCALL0)
                {
                    byte iSysCall = byte(iOperands[iIndex]);
                    if ((iSysCall == iLongMul) ||
                        (iSysCall == iLongAdd)  ||
                        (iSysCall == iFloatMul)  ||
                        (iSysCall == iFloatAdd))
                    {
                        isCommutative = true;
                    }
                }
                if (isCommutative && !IsTargetOfJumps(iIndex))
                {
                    RemoveInstruction(iIndex-1);
                    modified = true;
                    continue;
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    // str = str.Trim() -> Trim(ref str),  str = str.Append(x) -> Build(ref str, x), etc.
    bool OptimizeStringRef()
    {
        if (iCodes.Count < 5)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 4; // start at 5th instruction
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode4 = iCodes[iIndex-4];
            Instruction opCode3 = iCodes[iIndex-3];
            Instruction opCode2 = iCodes[iIndex-2];
            Instruction opCode1 = iCodes[iIndex-1];
            Instruction opCode0 = iCodes[iIndex];
            
            if (   ((opCode1 == Instruction.SYSCALL0) || (opCode1 == Instruction.SYSCALL1)) 
                && IsLOCALCOPYInstruction(opCode0)
               )
            {
                byte iSysCall = byte(iOperands[iIndex-1]);
                bool singleArg =  (iSysCall == iStringTrim)
                               || (iSysCall == iStringTrimLeft)
                               || (iSysCall == iStringToUpper)
                               || (iSysCall == iStringToLower);
                if (singleArg)
                {
                    singleArg = (opCode1 == Instruction.SYSCALL0);
                }
                bool doubleArg = ((iSysCall == iStringSubstring) && (opCode1 == Instruction.SYSCALL0))
                               || (iSysCall == iStringAppend)
                                ;
                bool trippleArg = (iSysCall == iStringInsertChar);
                bool opCode2HasOperand;
                byte operand2;
                bool opCode3HasOperand;
                byte operand3;
                bool opCode4HasOperand;
                byte operand4;
                byte operand0;
                if (singleArg)
                {
                    singleArg = false;
                    switch (opCode0)
                    {
                        case Instruction.POPCOPYLOCALB:
                        {
                            opCode2HasOperand = true;
                            operand2 = byte(iOperands[iIndex-2]);
                            operand0 = byte(iOperands[iIndex]);
                            singleArg = (opCode2 == Instruction.PUSHLOCALB) && (operand0 == operand2);
                        }
                        case Instruction.POPCOPYLOCALB00:
                        {
                            operand2 = 0;
                            singleArg = (opCode2 == Instruction.PUSHLOCALB00);
                        }
                        case Instruction.POPCOPYLOCALB01:
                        {
                            operand2 = 1;
                            singleArg = (opCode2 == Instruction.PUSHLOCALB01);
                        }
                    }
                }
                if (doubleArg)
                {
                    doubleArg = IsSinglePUSHInstruction(opCode2);
                }
                if (doubleArg)
                {
                    doubleArg = false;
                    switch (opCode0)
                    {
                        case Instruction.POPCOPYLOCALB:
                        {
                            opCode3HasOperand = true;
                            operand3 = byte(iOperands[iIndex-3]);
                            operand0 = byte(iOperands[iIndex]);
                            doubleArg = (opCode3 == Instruction.PUSHLOCALB) && (operand0 == operand3);
                        }
                        case Instruction.POPCOPYLOCALB00:
                        {
                            operand3 = 0;
                            doubleArg = (opCode3 == Instruction.PUSHLOCALB00);
                        }
                        case Instruction.POPCOPYLOCALB01:
                        {
                            operand3 = 1;
                            doubleArg = (opCode3 == Instruction.PUSHLOCALB01);
                        }
                    }
                }
                if (trippleArg)
                {
                    trippleArg = IsSinglePUSHInstruction(opCode2) && (opCode3 == Instruction.PUSHI0);
                }
                if (trippleArg)
                {
                    switch (opCode0)
                    {
                        case Instruction.POPCOPYLOCALB:
                        {
                            opCode4HasOperand = true;
                            operand4 = byte(iOperands[iIndex-4]);
                            operand0 = byte(iOperands[iIndex]);
                            trippleArg = (opCode4 == Instruction.PUSHLOCALB) && (operand0 == operand4);
                        }
                        case Instruction.POPCOPYLOCALB00:
                        {
                            operand4 = 0;
                            trippleArg = (opCode4 == Instruction.PUSHLOCALB00);
                        }
                        case Instruction.POPCOPYLOCALB01:
                        {
                            operand4 = 1;
                            trippleArg = (opCode4 == Instruction.PUSHLOCALB01);
                        }
                    }
                }
                if (singleArg)
                {   
                    if (false)
                    {
                        string operand2Content; 
                        if (opCode2HasOperand)
                        {
                            operand2Content = " 0x" +  operand2.ToHexString(2);
                        }
                        string content = Instructions.ToString(opCode2) + operand2Content + " "
                                       + Instructions.ToString(opCode1) 
                                       + " 0x" +  iSysCall.ToHexString(2) + " (" + SysCalls.GetSysCallName(iSysCall) + ") "
                                       + Instructions.ToString(opCode0);
                        if (iLengths[iIndex] == 2)
                        {
                            byte local0 = byte(iOperands[iIndex]);
                            content = content + " 0x" + local0.ToHexString(2);    
                        }
                        
                        PrintLn();
                        Print(content);
                    }
                    
                    // String.Trim()     -> String.Trim(ref)
                    // String.TrimLeft() -> String.TrimLeft(ref)
                    // String.ToUpper()  -> String.ToUpper(ref)
                    // String.ToLower()  -> String.ToLower(ref)
                    iCodes.SetItem(iIndex-2, Instruction.PUSHSTACKADDRB);
                    if (iLengths[iIndex-2] == 1)
                    {
                        iOperands.SetItem(iIndex-2, operand2);
                        iLengths.SetItem(iIndex-2, 2);
                    }
                    iCodes.SetItem(iIndex-1, Instruction.SYSCALL1);
                    RemoveInstruction(iIndex);
                    modified = true;
                }
                if (trippleArg)
                {
                    if (false)
                    {
                        string operand4Content; 
                        if (opCode4HasOperand)
                        {
                            operand4Content = " 0x" +  operand4.ToHexString(2);
                        }
                        string content = Instructions.ToString(opCode4) + operand4Content + " "
                                       + Instructions.ToString(opCode3) + " "
                                       + Instructions.ToString(opCode2) + " "
                                       + Instructions.ToString(opCode1) 
                                       + " 0x" +  iSysCall.ToHexString(2) + " (" + SysCalls.GetSysCallName(iSysCall) + ") "
                                       + Instructions.ToString(opCode0);
                        if (iLengths[iIndex] == 2)
                        {
                            byte local0 = byte(iOperands[iIndex]);
                            content = content + " 0x" + local0.ToHexString(2);    
                        }
                        
                        PrintLn();
                        Print(content);
                    }
                    if (iSysCall == iStringInsertChar)
                    {
                        // String.Insert(0, ch) -> String.BuildFront(ref, str, ch)
                        iCodes.SetItem(iIndex-4, Instruction.PUSHSTACKADDRB);
                        if (iLengths[iIndex-4] == 1)
                        {
                            iOperands.SetItem(iIndex-4, operand4);
                            iLengths.SetItem(iIndex-4, 2);
                        }
                        iCodes.SetItem(iIndex-1, Instruction.SYSCALL0);
                        iOperands.SetItem(iIndex-1, iStringBuildFront);
                        iLengths.SetItem(iIndex-1, 2);
                        RemoveInstruction(iIndex);
                        RemoveInstruction(iIndex-3);
                        modified = true;
                    }
                }
                if (doubleArg)
                {   
                    if (false)
                    {
                        string operand3Content; 
                        if (opCode3HasOperand)
                        {
                            operand3Content = " 0x" +  operand3.ToHexString(2);
                        }
                        string content = Instructions.ToString(opCode3) + operand3Content + " "
                                       + Instructions.ToString(opCode2) + " "
                                       + Instructions.ToString(opCode1) 
                                       + " 0x" +  iSysCall.ToHexString(2) + " (" + SysCalls.GetSysCallName(iSysCall) + ") "
                                       + Instructions.ToString(opCode0);
                        if (iLengths[iIndex] == 2)
                        {
                            byte local0 = byte(iOperands[iIndex]);
                            content = content + " 0x" + local0.ToHexString(2);    
                        }
                        
                        PrintLn();
                        Print(content);
                    }
                    if ((iSysCall == iStringAppend) && (opCode1 == Instruction.SYSCALL0))
                    {
                        // String.Append(str) -> String.Build(ref, str)
                        iCodes.SetItem(iIndex-3, Instruction.PUSHSTACKADDRB);
                        if (iLengths[iIndex-3] == 1)
                        {
                            iOperands.SetItem(iIndex-3, operand3);
                            iLengths.SetItem(iIndex-3, 2);
                        }
                        iCodes.SetItem(iIndex-1, Instruction.SYSCALL0);
                        iOperands.SetItem(iIndex-1, iStringBuild);
                        iLengths.SetItem(iIndex-1, 2);
                        RemoveInstruction(iIndex);
                        modified = true;
                    }
                    if ((iSysCall == iStringAppend) && (opCode1 == Instruction.SYSCALL1))
                    {
                        // String.Append(ch) -> String.Build(ref, ch)
                        iCodes.SetItem(iIndex-3, Instruction.PUSHSTACKADDRB);
                        if (iLengths[iIndex-3] == 1)
                        {
                            iOperands.SetItem(iIndex-3, operand3);
                            iLengths.SetItem(iIndex-3, 2);
                        }
                        iCodes.SetItem(iIndex-1, Instruction.SYSCALL1);
                        iOperands.SetItem(iIndex-1, iStringBuild);
                        iLengths.SetItem(iIndex-1, 2);
                        RemoveInstruction(iIndex);
                        modified = true;
                    }
                    if ((iSysCall == iStringSubstring) && (opCode1 == Instruction.SYSCALL0))
                    {
                        // String.Substring(iStart) -> String.Substring(ref, iStart)
                        iCodes.SetItem(iIndex-3, Instruction.PUSHSTACKADDRB);
                        if (iLengths[iIndex-3] == 1)
                        {
                            iOperands.SetItem(iIndex-3, operand3);
                            iLengths.SetItem(iIndex-3, 2);
                        }
                        iCodes.SetItem(iIndex-1, Instruction.PUSHIB);
                        iOperands.SetItem(iIndex-1, 2);
                        iLengths.SetItem(iIndex-1, 2);
                        
                        iCodes.SetItem(iIndex, Instruction.SYSCALL);
                        iOperands.SetItem(iIndex, iSysCall);
                        iLengths.SetItem(iIndex, 2);
                        modified = true;
                    }
                 }
                
            } // if SYSCALL
            iIndex++;
        } // loop
        return modified;
    }
    
    bool OptimizeSetters()
    {
        if (iCodes.Count < 4)
        {
            return false;
        }
        uint iIndex = 3;
        uint hits = 0;
        bool modified = false;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            
            Instruction opCode3 = iCodes[iIndex-3]; 
            Instruction opCode2 = iCodes[iIndex-2]; 
            Instruction opCode1 = iCodes[iIndex-1]; 
            Instruction opCode0 = iCodes[iIndex]; 
            if (   (opCode3 == Instruction.ENTER) 
                && (opCode2 == Instruction.PUSHLOCALB) // -2
                && ((opCode1 == Instruction.POPGLOBALB) || (opCode1 == Instruction.POPGLOBAL))
                && (opCode0 == Instruction.RETB)
               )
            {
                if (   (iOperands[iIndex-2] == 0xFF) // PUSHLOCALB BP-1
                    && (iOperands[iIndex]   == 1) // RETB 1
                   )
                {
                    iCodes.SetItem(iIndex, Instruction.RET0);
                    iLengths.SetItem(iIndex, 1);
                    RemoveInstruction(iIndex-2); // good
                }
            }
        
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeLongAddSub()
    {
        bool modified;
        if (iCodes.Count < 3)
        {
            return false;
        }
        uint iIndex = 2;
        uint hits = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode2 = iCodes[iIndex-2];   
            Instruction opCode1 = iCodes[iIndex-1];   
            Instruction opCode0 = iCodes[iIndex];   
            if ((opCode2 == Instruction.PUSHI1) || (opCode2 == Instruction.PUSHIB))
            {
                if ((opCode1 == Instruction.SYSCALL0) && (opCode0 == Instruction.SYSCALL0))
                {
                    uint operand = 1;
                    if (opCode2 == Instruction.PUSHIB)
                    {
                        operand = iOperands[iIndex-2];
                    }
                    if ((iOperands[iIndex-1] == iUIntToLong) && (iOperands[iIndex] == iLongAdd))
                    {
                        iOperands.SetItem(iIndex, iLongAddB);
                        RemoveInstruction(iIndex-1);
                        modified = true;
                        continue;
                    }
                    if ((iOperands[iIndex-1] == iUIntToLong) && (iOperands[iIndex] == iLongSub))
                    {
                        iOperands.SetItem(iIndex, iLongSubB);
                        RemoveInstruction(iIndex-1);
                        modified = true;
                        continue;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeINCDEC()
    {
        bool modified;
        if (iCodes.Count < 4)
        {
            return false;
        }
        uint iIndex = 3;
        uint hits = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode3 = iCodes[iIndex-3];   
            Instruction opCode2 = iCodes[iIndex-2];   
            Instruction opCode1 = iCodes[iIndex-1];   
            Instruction opCode0 = iCodes[iIndex]; 
            
            bool signed;
            bool unsigned;
            bool local;
            bool global;
            uint operand;
            uint delta = 1;
            if ((opCode2 == Instruction.PUSHI1) && ((opCode1 == Instruction.ADD) || (opCode1 == Instruction.SUB)))
            {
                unsigned = true;
            }
            else if ((opCode2 == Instruction.PUSHI1) && ((opCode1 == Instruction.ADDI) || (opCode1 == Instruction.SUBI)))
            {
                signed = true;
            }
            if (signed || unsigned)
            {
                if ((opCode3 == Instruction.PUSHGLOBALB) && (opCode0 == Instruction.POPGLOBALB))
                {
                    if (iOperands[iIndex-3] == iOperands[iIndex])
                    {
                        operand = iOperands[iIndex];
                        global = true;
                    }
                }
                else if ((opCode3 == Instruction.PUSHLOCALB) && (opCode0 == Instruction.POPLOCALB))
                {
                    if (iOperands[iIndex-3] == iOperands[iIndex])
                    {
                        operand = iOperands[iIndex];
                        local = true;
                    }
                }
                else if ((opCode3 == Instruction.PUSHLOCALB00) && (opCode0 == Instruction.POPLOCALB00))
                {
                    operand = 0;
                    local = true;
                }
                else if ((opCode3 == Instruction.PUSHLOCALB01) && (opCode0 == Instruction.POPLOCALB01))
                {
                    operand = 1;
                    local = true;
                }
            }
            else
            {
                if (   ((opCode1 == Instruction.ADDB) || (opCode1 == Instruction.SUBB)) 
                    && ((iOperands[iIndex-1] == 1) || (iOperands[iIndex-1] == 2)))
                {
                    if ((opCode2 == Instruction.PUSHGLOBALB) && (opCode0 == Instruction.POPGLOBALB))
                    {
                        if (iOperands[iIndex-2] == iOperands[iIndex])
                        {
                            operand = iOperands[iIndex];
                            global = true;
                            delta = iOperands[iIndex-1];
                        }
                    }
                    else if ((opCode2 == Instruction.PUSHLOCALB) && (opCode0 == Instruction.POPLOCALB))
                    {
                        if (iOperands[iIndex-2] == iOperands[iIndex])
                        {
                            operand = iOperands[iIndex];
                            local = true;
                            delta = iOperands[iIndex-1];
                        }
                    }
                    else if ((opCode2 == Instruction.PUSHLOCALB00) && (opCode0 == Instruction.POPLOCALB00))
                    {
                        operand = 0;
                        local = true;
                        delta = iOperands[iIndex-1];
                    }
                    else if ((opCode2 == Instruction.PUSHLOCALB01) && (opCode0 == Instruction.POPLOCALB01))
                    {
                        operand = 1;
                        local = true;
                        delta = iOperands[iIndex-1];
                    }
                }
            }
            if ((local || global) && (delta <= 2))
            {
                Instruction opCode = Instruction.NOP;
                if (local)
                {
                    opCode = ((opCode1 == Instruction.ADD) || (opCode1 == Instruction.ADDI) || (opCode1 == Instruction.ADDB)) ? 
                             (signed ? Instruction.INCLOCALIB : Instruction.INCLOCALB) : 
                             (signed ? Instruction.DECLOCALIB : Instruction.DECLOCALB);
                }
                if (global)
                {
                    opCode = ((opCode1 == Instruction.ADD) || (opCode1 == Instruction.ADDI) || (opCode1 == Instruction.ADDB)) ? 
                             (signed ? Instruction.INCGLOBALIB : Instruction.INCGLOBALB) : 
                             (signed ? Instruction.DECGLOBALIB : Instruction.DECGLOBALB);
                }
                
                // IsTargetOfJumps(iIndex)) ??
                
                if (delta == 1)
                {
                    iCodes.SetItem(iIndex, opCode);
                    iOperands.SetItem(iIndex, operand);
                    iLengths.SetItem(iIndex, 2);
                    
                    if ((opCode1 == Instruction.ADDB) || (opCode1 == Instruction.SUBB))
                    {
                        // PUSH   ADDB|SUBB   POP  -> INC | DEC
                        // 3 instructions          -> 1 instruction = remove 2
                        RemoveInstruction(iIndex-2); // good
                        RemoveInstruction(iIndex-2); // good
                        modified = true;
                        continue;
                    }
                    else
                    {
                        // PUSH   PUSHI1  ADD|ADDI|SUB|SUBI  POP  -> INC | DEC
                        // 4 instructions                         -> 1 instruction = remove 3
                        RemoveInstruction(iIndex-3); // good
                        RemoveInstruction(iIndex-3); // good
                        RemoveInstruction(iIndex-3); // good
                        modified = true;
                        continue;
                    }
                }
                else if (delta == 2) 
                {
                    iCodes.SetItem(iIndex, opCode);
                    iOperands.SetItem(iIndex, operand);
                    iLengths.SetItem(iIndex, 2);
                    
                    iCodes.SetItem(iIndex-1, opCode);
                    iOperands.SetItem(iIndex-1, operand);
                    iLengths.SetItem(iIndex-1, 2);
                 
                    // PUSH   ADDB|SUBB   POP  -> INC|INC DEC|DEC   
                    // 3 instructions          -> 2 instructions = remove 1
                    RemoveInstruction(iIndex-2); // good
                    modified = true;
                    continue;
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    bool OptimizeSymmetricCompare()
    {
        bool modified;
        if (iCodes.Count < 3)
        {
            return false;
        }
        uint iIndex = 2;
        uint hits = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode2 = iCodes[iIndex-2];   
            Instruction opCode1 = iCodes[iIndex-1];   
            Instruction opCode0 = iCodes[iIndex]; 
            
            if (   ((opCode0 == Instruction.NE) || (opCode0 == Instruction.EQ))
                && IsSinglePUSHInstruction(opCode1)
                && ((opCode2 == Instruction.PUSHI0) || (opCode2 == Instruction.PUSHI0))
               )
            {
                // opCode1 and opCode0 not jump targets?
                if (!IsTargetOfJumps(iIndex) && !IsTargetOfJumps(iIndex-1))
                {
                    // swap the order of the PUSH instructions to aid EQ|NE -> JNZ|JZ optimization
                    iCodes.SetItem   (iIndex-2, opCode1);
                    iLengths.SetItem (iIndex-2, iLengths [iIndex-1]);
                    iOperands.SetItem(iIndex-2, iOperands[iIndex-1]);
                    iCodes.SetItem   (iIndex-1, opCode2);
                    iLengths.SetItem (iIndex-1, 1);
                    modified = true;
                }
            }
            if ((opCode0 == Instruction.LT) && IsSinglePUSHInstruction(opCode1) && (opCode2 == Instruction.PUSHI0))
            {
                // opCode1 and opCode0 not jump targets?
                if (!IsTargetOfJumps(iIndex) && !IsTargetOfJumps(iIndex-1))
                {
                    // swap the order of the PUSH instructions to aid "0 < A" -> "A > 0" EQ|NE -> JNZ|JZ optimization
                    iCodes.SetItem   (iIndex-2, opCode1);
                    iLengths.SetItem (iIndex-2, iLengths [iIndex-1]);
                    iOperands.SetItem(iIndex-2, iOperands[iIndex-1]);
                    iCodes.SetItem   (iIndex-1, opCode2);
                    iLengths.SetItem (iIndex-1, 1);
                    iCodes.SetItem   (iIndex-0, Instruction.GT);
                    modified = true;
                }
            }
            if ((opCode0 == Instruction.GT) && (opCode1 == Instruction.PUSHI0))
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    // for unsigned '>' (GT),  "> 0" means the same thing as "!= 0"
                    iCodes.SetItem   (iIndex, Instruction.NE);
                    modified = true;
                }
            }
            if ((opCode0 == Instruction.GE) && (opCode1 == Instruction.PUSHI0) && IsSinglePUSHInstruction(opCode2))
            {
                // for unsigned '>=' (GE),  ">= 0" is always true
                if (!IsTargetOfJumps(iIndex) && !IsTargetOfJumps(iIndex-1))
                {
                    // NOP : replace comparison with PUSHI1 (always true)
                    iCodes.SetItem   (iIndex, Instruction.PUSHI1);
                    iLengths.SetItem (iIndex, 1);
                    RemoveInstruction(iIndex-2); // good
                    RemoveInstruction(iIndex-2); // good
                    modified = true;
                }
            }
            if ((opCode0 == Instruction.LE) && IsSinglePUSHInstruction(opCode1) && (opCode2 == Instruction.PUSHI0))
            {
                // for unsigned '<=' (LE),  "0 <=" is always true
                if (!IsTargetOfJumps(iIndex) && !IsTargetOfJumps(iIndex-1))
                {
                    // NOP : replace comparison with PUSHI1 (always true)
                    iCodes.SetItem   (iIndex, Instruction.PUSHI1);
                    iLengths.SetItem (iIndex, 1);
                    RemoveInstruction(iIndex-2); // good
                    RemoveInstruction(iIndex-2); // good
                    modified = true;
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizePUSHIBB()
    {
        bool modified;
        if (iCodes.Count < 2)
        {
            return modified;
        }
        uint iIndex = 1;
        uint hits = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode1 = iCodes[iIndex-1];
            Instruction opCode0 = iCodes[iIndex]; 
            if ((opCode1 == Instruction.PUSHIB) && (opCode0 == Instruction.PUSHIB))
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    uint operand = iOperands[iIndex-1] + (iOperands[iIndex] << 8);
                    
                    iCodes.SetItem   (iIndex, Instruction.PUSHIBB);
                    iOperands.SetItem(iIndex, operand);
                    iLengths.SetItem (iIndex, 3);
                    RemoveInstruction(iIndex-1); // good
                    modified = true;
                    continue;
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizePUSHLOCALBB()
    {
        bool modified;
        if (iCodes.Count < 2)
        {
            return modified;
        }
        uint iIndex = 1;
        uint hits = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode0 = iCodes[iIndex]; 
            if ((opCode0 == Instruction.PUSHLOCALB) || (opCode0 == Instruction.PUSHLOCALB00) || (opCode0 == Instruction.PUSHLOCALB01))
            {
                Instruction opCode1 = iCodes[iIndex-1]; 
                if ((opCode1 == Instruction.PUSHLOCALB) || (opCode1 == Instruction.PUSHLOCALB00) || (opCode1 == Instruction.PUSHLOCALB01))
                {
                    if ((iLengths[iIndex]+iLengths[iIndex-1] >= 3) && !IsTargetOfJumps(iIndex) && !IsTargetOfJumps(iIndex-1))
                    {
                        byte operand0;
                        byte operand1;
                        switch (opCode0)
                        {
                            case Instruction.PUSHLOCALB:   { operand0 = byte(iOperands[iIndex]); }
                            case Instruction.PUSHLOCALB00: { operand0 = 0; }
                            case Instruction.PUSHLOCALB01: { operand0 = 1; }
                        }
                        switch (opCode1)
                        {
                            case Instruction.PUSHLOCALB:   { operand1 = byte(iOperands[iIndex-1]); }
                            case Instruction.PUSHLOCALB00: { operand1 = 0; }
                            case Instruction.PUSHLOCALB01: { operand1 = 1; }
                        }
                        uint operand = operand1 + (operand0 << 8);
                        iCodes.SetItem   (iIndex, Instruction.PUSHLOCALBB);
                        iOperands.SetItem(iIndex, operand);
                        iLengths.SetItem (iIndex, 3);
                        RemoveInstruction(iIndex-1); // good
                        modified = true;
                        continue;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeSYSCALL00()
    {
        bool modified;
        if (iCodes.Count < 2)
        {
            return modified;
        }
        uint iIndex = 1;
        uint hits = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode1 = iCodes[iIndex-1];
            Instruction opCode0 = iCodes[iIndex]; 
            if ((opCode1 == Instruction.PUSHIB) && (opCode0 == Instruction.SYSCALL0))
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    uint operand = iOperands[iIndex-1] + (iOperands[iIndex] << 8);
                    
                    iCodes.SetItem   (iIndex, Instruction.SYSCALLB0);
                    iOperands.SetItem(iIndex, operand);
                    iLengths.SetItem (iIndex, 3);
                    RemoveInstruction(iIndex-1); // good
                    modified = true;
                    continue;
                }
            }
            if ((opCode1 == Instruction.PUSHIB) && (opCode0 == Instruction.SYSCALL1))
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    uint operand = iOperands[iIndex-1] + (iOperands[iIndex] << 8);
                    
                    iCodes.SetItem   (iIndex, Instruction.SYSCALLB1);
                    iOperands.SetItem(iIndex, operand);
                    iLengths.SetItem (iIndex, 3);
                    RemoveInstruction(iIndex-1); // good
                    modified = true;
                    continue;
                }
            }
            if ((opCode1 == Instruction.SYSCALL0) && (opCode0 == Instruction.SYSCALL0))
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    uint operand = iOperands[iIndex-1] + (iOperands[iIndex] << 8);
                    iCodes.SetItem   (iIndex, Instruction.SYSCALL00);
                    iOperands.SetItem(iIndex, operand);
                    iLengths.SetItem (iIndex, 3);
                    RemoveInstruction(iIndex-1); // good
                    modified = true;
                    continue;
                }
            }
            if ((opCode1 == Instruction.SYSCALL0) && (opCode0 == Instruction.SYSCALL1))
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    uint operand = iOperands[iIndex-1] + (iOperands[iIndex] << 8);
                    iCodes.SetItem   (iIndex, Instruction.SYSCALL01);
                    iOperands.SetItem(iIndex, operand);
                    iLengths.SetItem (iIndex, 3);
                    RemoveInstruction(iIndex-1); // good
                    modified = true;
                    continue;
                }
            }
            if ((opCode1 == Instruction.SYSCALL1) && (opCode0 == Instruction.SYSCALL0))
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    uint operand = iOperands[iIndex-1] + (iOperands[iIndex] << 8);
                    iCodes.SetItem   (iIndex, Instruction.SYSCALL10);
                    iOperands.SetItem(iIndex, operand);
                    iLengths.SetItem (iIndex, 3);
                    RemoveInstruction(iIndex-1); // good
                    modified = true;
                    continue;
                }
            }
            iIndex++;
        } // loop
        return modified;  
    }
    
    bool OptimizeJZxJx()
    {
        bool modified;
        if (iCodes.Count < 2)
        {
            return modified;
        }
        uint iIndex = 1;
        uint hits = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode1 = iCodes[iIndex-1];
            Instruction opCode0 = iCodes[iIndex]; 
            
            if (IsConditionalJumpInstruction(opCode1) && IsUnconditionalJumpInstruction(opCode0) && !IsTargetOfJumps(iIndex))
            {
                <uint> jumpTargets1 = iJumpTargets[iIndex-1];
                uint jumpTarget1 = jumpTargets1[0];
                if (jumpTarget1 == iIndex + 1)
                {
                    <uint> jumpTargets0 = iJumpTargets[iIndex];
                    uint jumpTarget0 = jumpTargets0[0];
                    long offset = long(iIndex-1) - long(jumpTarget0);
                    
                    Instruction newJumpCode;
                    switch (opCode1)
                    {
                        case Instruction.JZB:
                        case Instruction.JZ:
                        {
                            newJumpCode = Instruction.JNZ;
                        }
                        case Instruction.JNZB:
                        case Instruction.JNZ:
                        {
                            newJumpCode = Instruction.JZ;
                        }
                    } 
                    // Change the conditional jump to the opposite of the conditional jump, same target as unconditional jump
                    // Change it to JZ or JNZ (since there is extra space for a 3 byte instruction after we remove the other one)
                    iCodes.SetItem(iIndex-1,       newJumpCode);
                    iLengths.SetItem(iIndex-1,     3);
                    iJumpTargets.SetItem(iIndex-1, jumpTargets0);
                    RemoveInstruction(iIndex); // good - this keeps the debug info in a better location
                    modified = true;
                }
            }        
            iIndex++;
        } // loop
        return modified;  
    }
    setPUSHI(uint iIndex, uint value)
    {
        switch (value)
        {
            case 0:
            {
                iCodes.SetItem (iIndex,  Instruction.PUSHI0);
                iLengths.SetItem(iIndex, 1);
            }
            case 1:
            {
                iCodes.SetItem (iIndex,  Instruction.PUSHI1);
                iLengths.SetItem(iIndex, 1);
            }
            default:
            {
                if (value <= 255)
                {
                    iCodes.SetItem (iIndex,   Instruction.PUSHIB);
                    iLengths.SetItem(iIndex,  2);
                    iOperands.SetItem(iIndex, value);
                }
                else
                {
                    iCodes.SetItem (iIndex,   Instruction.PUSHI);
                    iLengths.SetItem(iIndex,  3);
                    iOperands.SetItem(iIndex, value);   
                }
            }
        }
    }
    bool OptimizeMULSHL()
    {
        bool modified;
        if (iCodes.Count < 2)
        {
            return modified;
        }
        uint iIndex = 1;
        uint hits = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode1  = iCodes[iIndex-1];
            uint        operand1 = iOperands[iIndex-1];
            Instruction opCode0  = iCodes[iIndex]; 
            
            if (IsPUSHImmediateInstruction(opCode1, ref operand1) && ((opCode0 == Instruction.MUL) || (opCode0 == Instruction.DIV)))
            {
                uint shift;
                switch (operand1)
                {
                    case    2: { shift =  1; }
                    case    4: { shift =  2; }
                    case    8: { shift =  3; }
                    case   16: { shift =  4; }
                    case   32: { shift =  5; }
                    case   64: { shift =  6; }
                    case  128: { shift =  7; }
                    case  256: { shift =  8; }
                }
                if (shift != 0)
                {
                    if (opCode0 == Instruction.MUL)
                    {
                        setPUSHI(iIndex-1,     shift);
                        iCodes.SetItem(iIndex, Instruction.BITSHL);
                        modified = true;
                    }
                    else if (opCode0 == Instruction.DIV)
                    {
                        setPUSHI(iIndex-1,     shift);
                        iCodes.SetItem(iIndex, Instruction.BITSHR);
                        modified = true;
                    }
                }
                else if (((operand1 == 0) && (opCode0 == Instruction.MUL)) // *= 0
                      && ((operand1 == 1) && (opCode0 == Instruction.DIV)) // /= 1 
                        )
                {
                    RemoveInstruction(iIndex-1); // good
                    RemoveInstruction(iIndex-1); // good
                    modified = true;
                }
            }
            else if ( (operand1 == 0) &&
                      IsPUSHImmediateInstruction(opCode1, ref operand1) && 
                     ((opCode0 == Instruction.ADD)  || (opCode0 == Instruction.SUB)  || // += 0
                      (opCode0 == Instruction.ADDI) || (opCode0 == Instruction.SUBI)    // -= 0
                     )
                    )
            {
                RemoveInstruction(iIndex-1); // good
                RemoveInstruction(iIndex-1); // good
                modified = true;
            }
            iIndex++;
        } // loop
        return modified;  
    }
    
    CountPairs(<OpCodePair> pairList)
    {
        if (iCodes.Count < 3)
        {
            return;
        }
        uint iIndex = 2;
        uint hits = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            Instruction opCode2 = iCodes[iIndex-2];
            Instruction opCode1 = iCodes[iIndex-1];
            Instruction opCode0 = iCodes[iIndex]; 
            
            uint iLength2 = iLengths[iIndex-2];
            uint iLength1 = iLengths[iIndex-1];
            uint iLength0 = iLengths[iIndex];
            
            bool found = false;
            if (iLength0 + iLength1 + iLength2 - 2 <= 3)
            {
                if (   !IsTargetOfJumps(iIndex) && !IsTargetOfJumps(iIndex-1) 
                    && !IsJumpOrExitInstruction(opCode2) && !IsJumpOrExitInstruction(opCode1))
                {
                    for (uint i = 0; i < pairList.Count; i++)
                    {
                        OpCodePair current = pairList[i];
                        if ((current.OpCode0 == opCode0) && (current.OpCode1 == opCode1) && (current.OpCode2 == opCode2))
                        {
                            //current.Count++;
                            //current.Count += 2*2;
                            current.Count = current.Count + 1;
                            pairList[i] = current;
                            found = true;
                            break;
                        }
                    }
                    if (!found)
                    {
                        OpCodePair codePair;
                        codePair.OpCode0 = opCode0;
                        codePair.OpCode1 = opCode1;
                        codePair.OpCode2 = opCode2;
                        codePair.Count = 1;
                        pairList.Append(codePair);
                    }
                    found = false;
                }
            }
            if (iLength0 + iLength1 - 1 <= 3)
            {
                if (!IsTargetOfJumps(iIndex) && !IsJumpOrExitInstruction(opCode1))
                {
                    for (uint i = 0; i < pairList.Count; i++)
                    {
                        OpCodePair current = pairList[i];
                        if ((current.OpCode0 == opCode0) && (current.OpCode1 == opCode1) && (current.OpCode2 == Instruction.NOP))
                        {
                            current.Count += 1;
                            pairList[i] = current;
                            found = true;
                            break;
                        }
                    }
                    if (!found)
                    {
                        OpCodePair codePair;
                        codePair.OpCode0 = opCode0;
                        codePair.OpCode1 = opCode1;
                        codePair.OpCode2 = Instruction.NOP;
                        codePair.Count = 1;
                        pairList.Append(codePair);
                    }
                }
            }
            iIndex++;
        }
    }
}
