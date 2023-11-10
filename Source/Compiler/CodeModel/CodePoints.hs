unit CodePoints
{
    uses "/Source/System/String"
    
    uses "/Source/Compiler/JSON/Code"
    uses "/Source/Compiler/CodeGen/CodeStream"
    uses "/Source/Compiler/CodeGen/Instructions"
    uses "/Source/Compiler/Tokens/SysCalls"
    uses "/Source/Compiler/CodeModel/ModelUtilities"
    
    <uint,uint> inlineMethodCandidates;   
    
    byte iLongMul;
    byte iLongAdd;
    byte iFloatMul;
    byte iFloatAdd;
    byte iUIntToLong;
    byte iIntToLong;
    byte iLongMulRef;
    byte iLongAddRef;
    byte iLongInc;
    
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
        if (!SysCalls.TryParseSysCall("Float.Mul", ref iFloatMul)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Float.Add", ref iFloatAdd)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("UInt.ToLong", ref iUIntToLong)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Int.ToLong", ref iIntToLong)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Long.MulRef", ref iLongMulRef)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Long.AddRef", ref iLongAddRef)) { Die(0x0B); }
        if (!SysCalls.TryParseSysCall("Long.Inc", ref iLongInc)) { Die(0x0B); }
        
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
    bool InlineCandidatesExist { get { return inlineMethodCandidates.Count > 0; } }
    
    <Instruction> iCodes;
    <uint>        iLengths;
    <uint>        iOperands;    // original
    < <byte> >    iContent;     // wide instruction content
    < <uint> >    iJumpTargets; // where can this instruction jump to?
    
    <uint,bool>   iReachable;
    
    <uint, <byte, uint> > iJIXMaps;
    
    uint currentMethod;
    <uint,string> indexDebugInfo;
    
    
    
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
    
    RemoveInstruction(uint iRemoval)
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
            uint iD = kv.key; // instruction index
            if ((iD > iRemoval) && (iD > 0))
            {
                iD--;
            }
            if (indexDebugInfo.Contains(iD))
            {
                // keep the old line
                newDebugInfo[iD] = indexDebugInfo[iD];
            }
            else
            {
                newDebugInfo[iD] = kv.value; // source line number
            }
        }
        indexDebugInfo = newDebugInfo;
        
        // update all jumpTargets:
        uint iCodeLength = iCodes.Length;
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
    
    
    
    
    uint GetInstructionIndex(uint seekAddress)
    {
        uint address;
        uint index;
        loop
        {
            if (address == seekAddress)
            {
                break;
            }
            address = address + iLengths[index];
            index++;
        }
        return index;
    }
    uint GetInstructionAddress(uint seekIndex)
    {
        uint address;
        uint index;
        loop
        {
            if (index == seekIndex)
            {
                break;
            }
            address = address + iLengths[index];
            index++;
        }
        return address;
    }
    
    InitializeJumpTargets()
    {
        uint iCodesLength = iCodes.Length;
        uint iIndex;
        loop
        {
            if (iIndex == iCodesLength)
            {
                break;
            }
            
            Instruction opCode = iCodes[iIndex];
            
            if (IsJumpInstruction(opCode))
            {
                // single jump target
                uint instructionLength = iLengths[iIndex];
                uint operand           = iOperands[iIndex];
                
                long offset;
                long jumpTarget;
            
                uint address = GetInstructionAddress(iIndex);
                if (instructionLength == 2)
                {
                    offset = operand;
                    if (offset > 127)
                    {
                        offset = offset - 256; // 255 -> -1
                    }
                    jumpTarget = long(address) + offset;
                }
                else if (instructionLength == 3)
                {
                    // operandWidth == 2
                    offset = operand;
                    if (offset > 32767)
                    {
                        offset = offset - 65536; // 0x10000 -> -1
                    }
                    jumpTarget = long(address) + offset;
                }
                else
                {
                    Die(0x0B); 
                }
                
                uint jumpIndex = GetInstructionIndex(jumpTarget);
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
                uint myAddress   = GetInstructionAddress(iIndex);
                uint baseAddress = myAddress - negativeOffset;
                uint iBase       = GetInstructionIndex(baseAddress);
                
                jumpTargets.Append(iBase);    // add iBase so it does not get removed (and so we can rebuild)
                jumpTargets.Append(iIndex+1); // default is the next instruction
                
                byte offsetSize = 1;
                if (opCode == Instruction.JIXW)
                {
                    offsetSize = 2;
                }
                
                byte switchCase = byte(operand & 0xFF);
                uint iOffset = 2;
                uint jumpTableLength = jumpTable.Length;
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
                        uint jumpAddress = baseAddress + offset;
                        uint jumpTarget  = GetInstructionIndex(jumpAddress);
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
            iIndex++;
        }
    }
    AppendJIXInstruction(uint iIndex, ref <byte> code)
    {
        // at this point only the opCode has been appended
        uint myAddress      = code.Length-1;
        
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
        if (opCode == Instruction.JIXW)
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
    
    uint Load(uint methodIndex)
    {
        iCodes.Clear();
        iLengths.Clear();
        iOperands.Clear();
        iContent.Clear();
        iJumpTargets.Clear();
        indexDebugInfo.Clear();
        iJIXMaps.Clear();
        iReachable.Clear();
        
        currentMethod = methodIndex;
        
        <byte> code = Code.GetMethodCode(currentMethod);
        uint codeLength = code.Length;
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
            uint index = GetInstructionIndex(address);
            indexDebugInfo[index] = kv.value;
        }
        ProgessNudge();
        return codeLength;
    }
    uint Save()
    {
        <byte> code;
        uint instructionCount = iCodes.Length;
        uint index;
        loop
        {
            Instruction opCode = iCodes[index];
            code.Append(byte(opCode));
            
            uint instructionLength = iLengths[index];
            if (instructionLength > 3)
            {
                if (IsJumpIXInstruction(opCode))
                {
                    AppendJIXInstruction(index, ref code);
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
                    
                    long currentAddress = GetInstructionAddress(index);
                    <uint> jumpTargets  = iJumpTargets[index];
                    uint jumpTarget     = jumpTargets[0];
                    long jumpAddress    = GetInstructionAddress(jumpTarget);
                    
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
            uint address = GetInstructionAddress(index);
            debugInfo[address.ToString()] = kv.value;
        }
        
        // update the method with the optimized code            
        Code.SetMethodCode(currentMethod, code);
        Code.SetMethodDebugInfo(currentMethod, debugInfo);
        ProgessNudge();
        
        return code.Length;
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
            if (sourceLines.Length > iline)
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
        
        uint icodesLength = iCodes.Length;
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
            if (jumpTargets.Length > 0)
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
    
    
    CollectMethodCalls(ref <uint,bool> methodsCalled)
    {
        uint iIndex;
        uint icodesLength = iCodes.Length;
        loop
        {
            if (iIndex == icodesLength)
            {
                break;
            }
            if (iReachable[iIndex])
            {
                Instruction opCode = iCodes[iIndex];
                if (    (opCode == Instruction.CALLW)  || (opCode == Instruction.CALLB)  // method called
                     || (opCode == Instruction.PUSHDW) || (opCode == Instruction.PUSHDB) // method referenced by delegate
                   )
                {
                    uint callMethodIndex = iOperands[iIndex];
                    if (Target6502)
                    {
                        callMethodIndex = callMethodIndex & 0x3FFF;
                    }
                    methodsCalled[callMethodIndex] = true;
                }
            }
            iIndex++;
        } // loop
        ProgessNudge();
    }
    MarkReachableInstructions()
    {
        
        
        uint iIndex = 0;
        uint icodesLength = iCodes.Length;
        
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
                        if (opCodePrev == Instruction.PUSHI0)
                        {
                            switch (opCode)
                            {
                                case Instruction.JZW:
                                case Instruction.JZB:
                                {
                                    // always branch
                                    mainPath = false;
                                }
                                case Instruction.JNZW:
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
                                case Instruction.JZW:
                                case Instruction.JZB:
                                {
                                    // never branch
                                    takeJump = false;
                                }
                                case Instruction.JNZW:
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
                else if ((opCode == Instruction.JIXB) || (opCode == Instruction.JIXW))
                {
                    // JIX : always assume all the options are possible
                    <uint> jumpTargets = iJumpTargets[iIndex];
                    foreach(var iJumpTarget in jumpTargets)
                    {
                        tailCalls.Append(iJumpTarget);
                    }
                    // fall through to 'continue' on the non-branch path ..
                }
                else if ((opCode == Instruction.JW) || (opCode == Instruction.JB))
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
            while (tailCalls.Length > 0)
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
        if (iCodes.Length < 3)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 2;
        loop
        {
            if (iIndex >= iCodes.Length)
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
        if (iCodes.Length < 2)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex == iCodes.Length)
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
                while ((iIndex < iCodes.Length) && (iCodes[iIndex] == Instruction.PUSHI0))
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
        if (iCodes.Length < 2)
        {
            return false;
        }
        bool modified = false;
        
        
        uint iIndex = 0;
        bool notSuitable = (currentMethod == 0); // don't mess with 'main' (which is always methodIndex == 0)
        uint hasRET0;
        
        loop
        {
            if (iIndex >= iCodes.Length)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            if (   IsLOCALInstruction( opCode)
                 || (opCode == Instruction.CALLW)
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
                    if (iIndex >= iCodes.Length)
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
                            Instruction opCode = iCodes[iIndex-1];
                            if (opCode == Instruction.TESTBPB)
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
                
                RemoveInstruction(0);
                modified = true;
                
                if ((hasRET0 == 1) && (retIndex == iCodes.Length) && (byteLength <= 4)) 
                {
                    // only one RETxx that is the last instruction
                    // 4 because it won't need the RETFAST when it is inlined
                    inlineMethodCandidates[currentMethod] = byteLength-1;
                }    
            } // ENTER
        }
        return modified;
    }
    bool InlineSmallMethods(ref <byte> rawCode)
    {
        uint iCodesLength = iCodes.Length;
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
            if ((opCode == Instruction.CALLW) || (opCode == Instruction.CALLB))
            {
                uint callMethodIndex = iOperands[iIndex];
                if (Target6502)
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
                            
                            // replace what was the method call with NOPs
                            for (uint i=0; i < availableSpace; i++)
                            {
                                rawCode.SetItem(inlineAddress+i, byte(Instruction.NOP));
                            }
                            
                            // put the NOPs in front so that unconditional branches with PUSHI0 or PUSHI1 are spotted sooner
                            uint nopOffset = availableSpace - sizeInBytes;
                            
                            // fill the space with the bytes from the method (could be trailing NOPs to remove later)
                            <byte> inlineCode = Code.GetMethodCode(methodIndex);
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
            if (iIndex >= iCodes.Length)
            {
                break;
            }
            bool removeInstruction = !iReachable[iIndex];
            if (removeInstruction)
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
    
    bool OptimizeUnconditionalJumps()
    {
        uint iCodesLength = iCodes.Length;
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
            if (IsConditionalJumpInstruction(opCode) && !IsTargetOfJumps(iIndex-1))
            {
                bool jumpPathIsValid = true;
                bool mainPathIsValid = true;
                bool invertJump = false;
                bool simplifyJump = false;
                byte nopCount = 0;
                Instruction opCodePrev     = iCodes[iIndex-1];
                Instruction opCodePrevPrev = Instruction.DIE; // unknown
                if ((iIndex >= 2) && !IsTargetOfJumps(iIndex-2))
                {
                    opCodePrevPrev = iCodes[iIndex-2];
                }
                if (opCodePrev == Instruction.PUSHI0)
                {
                    switch (opCode)
                    {
                        case Instruction.JZW:
                        case Instruction.JZB:
                        {
                            // always branch
                            mainPathIsValid = false;
                        }
                        case Instruction.JNZW:
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
                        case Instruction.JZW:
                        case Instruction.JZB:
                        {
                            // never branch
                            jumpPathIsValid = false;
                        }
                        case Instruction.JNZW:
                        case Instruction.JNZB:
                        {
                            // always branch
                            mainPathIsValid = false;
                        }
                    }
                }
                else if (                                               (opCodePrev == Instruction.BOOLNOT)
                          || ((opCodePrevPrev == Instruction.PUSHI0) && (opCodePrev == Instruction.EQ))
                        )
                {
                    switch (opCode)
                    {
                        case Instruction.JZW:
                        {
                            opCode = Instruction.JNZW;
                            invertJump = true;
                        }
                        case Instruction.JZB:
                        {
                            opCode = Instruction.JNZB;
                            invertJump = true;
                        }
                        case Instruction.JNZW:
                        {
                            opCode = Instruction.JZW;
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
                else if ((opCodePrevPrev == Instruction.PUSHI0) && (opCodePrev == Instruction.NE))
                {
                    switch (opCode)
                    {
                        case Instruction.JZW:
                        case Instruction.JZB:
                        case Instruction.JNZW:
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
                    iCodes.SetItem(iIndex-1, Instruction.NOP);
                    iCodes.SetItem(iIndex,   Instruction.NOP);
                    iLengths.SetItem(iIndex, 1);
                    <uint> emptyTarget;
                    iJumpTargets.SetItem(iIndex, emptyTarget);
                    modified = true;
                }
                else if (!mainPathIsValid)
                {
                    // we always branch - replace the PUSH with NOP and the J with unconditional
                    iCodes.SetItem(iIndex-1, Instruction.NOP);
                       
                    switch (opCode)
                    {
                        case Instruction.JZW:
                        case Instruction.JNZW:
                        {
                            opCode = Instruction.JW;
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
                        iCodes.SetItem(iIndex-2, Instruction.NOP);
                    }
                    iCodes.SetItem(iIndex-1, Instruction.NOP);
                    iCodes.SetItem(iIndex, opCode);
                    modified = true;
                }
                else if (simplifyJump)
                {
                    if (nopCount == 2)
                    {
                        iCodes.SetItem(iIndex-2, Instruction.NOP);
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
        if (iCodes.Length < 1)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex == iCodes.Length)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            bool removeIt = false;
            if (opCode == Instruction.NOP)
            {
                removeIt = true;
            }
            else if ((opCode == Instruction.JB) || (opCode == Instruction.JW))
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
        if (iCodes.Length < 2)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 1;
        loop
        {
            if (iIndex >= iCodes.Length)
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
                    
                    RemoveInstruction(iIndex-1);
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
        if (iCodes.Length < 1)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex >= iCodes.Length)
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
                        case Instruction.JW:
                        {
                            opCode = Instruction.JB;
                        }
                        case Instruction.JZW:
                        {
                            opCode = Instruction.JZB;
                        }
                        case Instruction.JNZW:
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
        if (iCodes.Length < 3)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 2;
        loop
        {
            if (iIndex >= iCodes.Length)
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
        if (iCodes.Length < 2)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex >= iCodes.Length)
            {
                break;
            }
            Instruction opCode = iCodes[iIndex];
            if (IsJumpInstruction(opCode))
            {
                <uint> jumpTargets = iJumpTargets[iIndex];
                uint jumpTarget = jumpTargets[0];
                if (jumpTarget >= iCodes.Length)
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
                        // JB or JW -> RETx  becomes RETx
                        uint jumpOperandWidth   = iLengths[iIndex] - 1;
                        uint targetOperandWidth = iLengths[jumpTarget] - 1;
                        if (targetOperandWidth <= jumpOperandWidth) // inserting code is harder than it may first appear (knock-on effects ..)
                        {    
                            // OperandWidth = 0:
                            //     Instruction.RET0
                            // OperandWidth = 1:
                            //     Instruction.DIE
                            //     Instruction.RETB
                            //     Instruction.RETRETB
                            // OperandWidth = 2
                            //     Instruction.RETW
                            //     Instruction.RETRETW
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
    bool OptimizePUSHRETRET()
    {
        if (iCodes.Length < 3)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 2;
        loop
        {
            if (iIndex >= iCodes.Length)
            {
                break;
            }
            Instruction opCode0  = iCodes   [iIndex];
            uint        operand0 = iOperands[iIndex];
            if ((opCode0 == Instruction.RETRETB) && (operand0 == 2)) // RETRETB 0x02
            {
                Instruction opCode1  = iCodes   [iIndex-1];
                uint        operand1 = iOperands[iIndex-1];
                if (opCode1 == Instruction.PUSHLOCALB00)
                {
                    // single local
                    iCodes.SetItem   (iIndex-1, Instruction.RET0);
                    iLengths.SetItem (iIndex-1, 1);
                    RemoveInstruction(iIndex);
                    modified = true;
                }      
                else if ((opCode1 == Instruction.PUSHLOCALB) && (operand1 == 0xFE)) // PUSHLOCALB 0xFE
                {
                    // single local, simple value argument
                    iCodes.SetItem   (iIndex-1, Instruction.RET0);
                    iLengths.SetItem (iIndex-1, 1);
                    RemoveInstruction(iIndex);
                    modified = true;
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizePUSH01LEEQ()
    {
        if (iCodes.Length < 3)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 2;
        loop
        {
            if (iIndex >= iCodes.Length)
            {
                break;
            }
            Instruction opCode1  = iCodes   [iIndex-1];
            Instruction opCode0  = iCodes   [iIndex];
            if (   ((opCode1 == Instruction.PUSHI0) || (opCode1 == Instruction.PUSHI1))
                && ((opCode0 == Instruction.LE)     || (opCode0 == Instruction.EQ))
               )
            {
                byte       operand = ((opCode1 == Instruction.PUSHI0) ? 0 : 1);
                Instruction opCode = ((opCode0 == Instruction.LE) ? Instruction.PUSHIBLE : Instruction.PUSHIBEQ);
                iCodes.SetItem   (iIndex-1, opCode);
                iOperands.SetItem(iIndex-1, operand);
                iLengths.SetItem (iIndex-1, 2);
                RemoveInstruction(iIndex);
                modified = true;
            }
            if (   ((opCode1 == Instruction.PUSHI0) || (opCode1 == Instruction.PUSHI1))
                && ((opCode0 == Instruction.ADD)    || (opCode0 == Instruction.SUB))
               )
            {
                byte       operand = ((opCode1 == Instruction.PUSHI0) ? 0 : 1);
                Instruction opCode = ((opCode0 == Instruction.ADD) ? Instruction.ADDB : Instruction.SUBB);
                iCodes.SetItem   (iIndex-1, opCode);
                iOperands.SetItem(iIndex-1, operand);
                iLengths.SetItem (iIndex-1, 2);
                RemoveInstruction(iIndex);
                modified = true;
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeCommutativeSWAP()
    {
        if (iCodes.Length < 3)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 1;
        loop
        {
            if (iIndex >= iCodes.Length)
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
        if (iCodes.Length < 5)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 4; // start at 5th instruction
        loop
        {
            if (iIndex >= iCodes.Length)
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
                        case Instruction.POPCOPYLOCALB02:
                        {
                            operand2 = 2;
                            singleArg = (opCode2 == Instruction.PUSHLOCALB02);
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
                        case Instruction.POPCOPYLOCALB02:
                        {
                            operand3 = 2;
                            doubleArg = (opCode3 == Instruction.PUSHLOCALB02);
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
                        case Instruction.POPCOPYLOCALB02:
                        {
                            operand4 = 2;
                            trippleArg = (opCode4 == Instruction.PUSHLOCALB02);
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
    GoFishing(uint currentMethod)
    {
        if (iCodes.Length < 1)
        {
            return;
        }
        uint iIndex = 1;
        uint hits = 0;
        loop
        {
            if (iIndex >= iCodes.Length)
            {
                break;
            }
            Instruction opCode0 = iCodes[iIndex];   
            if (IsJumpIXInstruction(opCode0) || IsConditionalJumpInstruction(opCode0))
            {
                return;
            }
            if (  (opCode0 == Instruction.CALLW) 
               || (opCode0 == Instruction.CALLB)
               || (opCode0 == Instruction.CALLREL)
               || (opCode0 == Instruction.JREL)
               || (opCode0 == Instruction.SYSCALL)
               || (opCode0 == Instruction.SYSCALL0)
               || (opCode0 == Instruction.SYSCALL1)
               )
            {
                return;
            }
            iIndex++;
        } // loop
        uint length = iCodes.Length;
        //if (hits > 0)
        //{
            Print( currentMethod.ToHexString(4) + ":" + length.ToString() + " ");
        //}
    }
    
}