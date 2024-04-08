unit AsmPoints
{
    uses "../CodeGen/Asm6502"
    
    flags Flags
    {
        Unreached = 0b0000000000000000,
        Walked    = 0b0000000000000001,
        Target    = 0b0000000000000010,
    }
    
    flags WalkStats
    {
        None      = 0b0000000000000000,
        Exit      = 0b0000000000000001,
        ReadA     = 0b0000000000000010,
        ReadX     = 0b0000000000000100,
        ReadY     = 0b0000000000001000,
        WriteA    = 0b0000000000010000,
        WriteX    = 0b0000000000100000,
        WriteY    = 0b0000000001000000,
    }
    
    <OpCode> iCodes;       // 6502 instruction opCodes
    <uint>   iOperands;    // 6502 instruction operands (could be byte or word)
    <uint>   iLengths;     // 6502 instruction lengths (not operand width)
    <Flags>  iFlags;        // reachable? jump target?
    <uint>   iDebugLines;   // Hopper source line associated with this instruction (mostly 0)
    
    < <uint> > iJumpTables;
    
    uint currentMethod;
    <uint,uint> indexToAddress; // <iIndex,address>
    <uint,uint> addressToIndex; // <address,iIndex>
           
    OpCode opcodeNOP;
    OpCode opcodeCALL;
    
    OpCode opcodeBEQ;
    OpCode opcodeBNE;
    OpCode opcodeBCS;
    OpCode opcodeBCC;
    OpCode opcodeRTS;
    OpCode opcodeJSR;
    OpCode opcodeiJMP;
    OpCode opcodeRTI;
    OpCode opcodeJMPIndex;
    
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
    
    bool IsTargetOfJumps(uint iIndex)
    {
        return Flags.Target == (iFlags[iIndex] & Flags.Target);
    }
    
    Reset()
    {
        opcodeNOP  = Asm6502.GetNOPInstruction();
        opcodeBEQ  = Asm6502.GetBInstruction("Z");
        opcodeBNE  = Asm6502.GetBInstruction("NZ");
        opcodeBCS  = Asm6502.GetBInstruction("C");
        opcodeBCC  = Asm6502.GetBInstruction("NC");
        opcodeRTS  = Asm6502.GetRETInstruction();
        opcodeRTI  = Asm6502.GetRTIInstruction();
        opcodeJSR  = Asm6502.GetJSRInstruction();
        opcodeiJMP = Asm6502.GetiJMPInstruction();
        opcodeJMPIndex = GetJMPIndexInstruction();
        opcodeCALL = Asm6502.GetJSRInstruction();
    }
    
    uint Load(uint methodIndex, string when)
    {
        currentMethod = methodIndex;
        
        iCodes.Clear();
        iLengths.Clear();
        iOperands.Clear();
        iDebugLines.Clear();
        iJumpTables.Clear();
        iFlags.Clear();
        
        <byte> code = Code.GetMethodCode(currentMethod);
        
        <string,variant> methodSymbols = Code.GetMethodSymbols(methodIndex);
        <string,string> debugInfo = methodSymbols["debug"];
        
        uint codeLength = code.Count;
        uint i = 0;
        
        loop
        {
            uint operand;
                      
            
            OpCode opCode = OpCode(code[i]);
            uint instructionAddress = i;
            uint instructionLength = Asm6502.GetInstructionLength(opCode);
            switch (instructionLength)
            {
                case 2:
                {
                    operand = code[i+1];
                }
                case 3:
                {
                    operand = code[i+1] + (code[i+2] << 8);
                }
            }
            if (opCode == opcodeJMPIndex)
            {
                instructionLength += 512;
            }
            
            iCodes.Append(opCode);
            iLengths.Append(instructionLength);
            iOperands.Append(operand);
            iFlags.Append(Flags.Unreached);
            
            uint debugLine = 0;
            if (debugInfo.Contains(instructionAddress.ToString()))
            {
                _ = UInt.TryParse(debugInfo[instructionAddress.ToString()], ref debugLine);
            }
            iDebugLines.Append(debugLine);
            
            <uint> empty;
            iJumpTables.Append(empty); // place holder
            
            uint iIndex = iCodes.Count-1;
            uint iTable = i + 3;
            i += instructionLength;
            
            if (opCode == opcodeJMPIndex)
            {
                uint tableSizeInWords = 256;
                <uint> jumps;
                for (uint ii=0; ii < tableSizeInWords; ii++)
                {
                    uint mi = code[iTable] + (code[tableSizeInWords+iTable] << 8);
                    iTable++;
                    jumps.Append(mi);
                }
                iJumpTables.SetItem(iIndex, jumps);
            }
            
            if (i == codeLength)
            {
                break;
            }
            
            
        } // loop
        
        RebuildMaps();
        
        InitializeJumpTargets();
        
        ProgessNudge();
        return codeLength;
    }
    
    //#define JUMP_DIAGNOSTICS
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
            OpCode opCode  = iCodes[iIndex];
            AddressingModes addressingMode;
            bool isConditional;
            if (Asm6502.IsJumpInstruction(opCode, ref addressingMode, ref isConditional))
            {
                uint address = GetInstructionAddress(iIndex);
                uint operand           = iOperands[iIndex];
                uint instructionLength = iLengths[iIndex];
                
                long offset;
                long jumpTargetAddress;
            
                if (addressingMode == AddressingModes.Absolute) // nnnn
                {
                    jumpTargetAddress = long(operand);
                }
                else if (addressingMode == AddressingModes.Relative) // dd
                {
                    offset = operand;
                    if (offset > 127)
                    {
                        offset = offset - 256; // 255 -> -1
                    }
                    jumpTargetAddress = long(instructionAddress) + offset + instructionLength;
                }
                else if (addressingMode == AddressingModes.ZeroPageRelative) // nn,dd
                {
                    offset = byte(operand >> 8);
                    if (offset > 127)
                    {
                        offset = offset - 256; // 255 -> -1
                    }
                    jumpTargetAddress = long(instructionAddress) + offset + instructionLength;
                }
                else
                {
                    Die(0x0B); 
                }
                uint jumpIndex;
                if (addressToIndex.Contains(jumpTargetAddress))
                {
                    jumpIndex = addressToIndex[jumpTargetAddress];
                }
                else
                {
                    // Most likely unreachable dead code so jumping to self
                    // does not mess up reachableness.
                    jumpIndex = iIndex;
                }
                iOperands.SetItem(iIndex, jumpIndex);
            }
            instructionAddress += iLengths[iIndex];
            iIndex++;
        }
    }
    
    uint Save()
    {
        <byte> code;
        uint instructionCount = iCodes.Count;
        uint index;
        
        RebuildMaps();
        
        <string,string> debugInfo;
        loop
        {
            
            OpCode opCode = iCodes[index];
            uint instructionLength = iLengths[index];
            
            code.Append(byte(opCode));
            
            if (instructionLength > 1)
            {
                uint operand = iOperands[index];
                
                AddressingModes addressingMode;
                bool isConditional;
                bool isJump = Asm6502.IsJumpInstruction(opCode, ref addressingMode, ref isConditional);
                if (isJump)
                {
                    // use the index to calculate the new offset
                    long currentAddress = indexToAddress[index];
                    
                    //Print((index.ToString()).Pad(' ', 3) + (currentAddress+0xE004).ToHexString(4) + " " + opCode.ToHexString(2) + " " + Asm6502.GetName(byte(opCode)) + " ");
                    
                    uint jumpTarget     = iOperands[index];
                    long jumpAddress;
                    if (indexToAddress.Contains(jumpTarget))
                    {
                        jumpAddress = indexToAddress[jumpTarget];
                    }
                    else
                    {
                        if (Flags.Walked == (iFlags[index] & Flags.Walked))
                        {
                            Die(0x0B); // a reachable jump instruction has a target not in the map!?
                        }
                        jumpAddress = currentAddress; // jump to self, don't mess with reachability
                    }
                    if ((addressingMode == AddressingModes.Relative) || (addressingMode == AddressingModes.ZeroPageRelative))
                    {
                        long offset = jumpAddress - currentAddress - instructionLength;
                        if ((offset < -128) || (offset > 127))
                        {
                            PrintLn("Bad Jump:  " + offset.ToString());
                            Die(0x0B);
                        }
                        if (offset < 0)
                        {
                            offset = offset + 256;
                        }
                        if (addressingMode == AddressingModes.Relative) // dd
                        {
                            operand = uint(offset);
                        }
                        else if (addressingMode == AddressingModes.ZeroPageRelative) // nn,dd
                        {
                            operand = (operand & 0xFF) + (uint(offset) << 8);
                        }
                    }
                    else if (isJump && (addressingMode == AddressingModes.Absolute))
                    {
                        operand = jumpAddress;
                    }
                    else
                    {
                        Die(0x0B);
                    }
                } // isJump
                
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
                
                if (opCode == opcodeJMPIndex)
                {
                    <uint> jumps = iJumpTables[index];
                    foreach (var iMethod in jumps)
                    {
                        code.Append(byte(iMethod & 0xFF));// LSBs
                    }
                    foreach (var iMethod in jumps)
                    {
                        code.Append(byte(iMethod >> 8));// MSBs
                    }
                }
            }
            if (iDebugLines[index] != 0)
            {
                debugInfo[(indexToAddress[index]).ToString()] = (iDebugLines[index]).ToString();
            }
            
            index++;
            if (index == instructionCount)
            {
                break;
            }
        }
           
        // update the method with the optimized code            
        Code.SetMethodCode(currentMethod, code);
        Code.SetMethodDebugInfo(currentMethod, debugInfo);
        ProgessNudge();
        return code.Count;
    }
    
    RebuildMaps()
    {
        indexToAddress.Clear();   // index -> address
        addressToIndex.Clear(); // address -> index
        
        uint address;
        uint index;
        loop
        {
            if (index >= iLengths.Count)
            {
                break;
            }
            indexToAddress[index] = address;
            addressToIndex[address] = index;
            address = address + iLengths[index];
            index++;
        }
    }
    
    walkInstructions(uint iIndex)
    {
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            if (Flags.Unreached != iFlags[iIndex])
            {
                return; // already been here
            }
            iFlags[iIndex] = Flags.Walked;
            
            OpCode opCode = iCodes[iIndex];
            AddressingModes addressingMode;
            bool isConditional;
            bool isJump = Asm6502.IsJumpInstruction(opCode, ref addressingMode, ref isConditional);
            if (isJump)
            {
                if (isConditional)
                {
                    uint iTarget = iOperands[iIndex];
                    walkInstructions(iTarget); // second potential path
                }
                else
                {
                    iIndex = iOperands[iIndex]; // unconditional branch
                    continue;
                }
            }
            else if (Asm6502.IsMethodExitInstruction(opCode))
            {
                break; // end of this path
            }
            iIndex++;
        }
    }
    
    MarkInstructions()
    {
        uint iIndex = 0;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            iFlags[iIndex] = Flags.Unreached;
            iIndex++;
        }
        walkInstructions(0);        
        iIndex = 0;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            if (Flags.Unreached != iFlags[iIndex])
            {
                OpCode opCode = iCodes[iIndex];
                AddressingModes addressingMode;
                bool isConditional;
                bool isJump = Asm6502.IsJumpInstruction(opCode, ref addressingMode, ref isConditional);
                if (isJump)
                {
                    uint iTarget = iOperands[iIndex];
                    iFlags[iTarget] = iFlags[iTarget] | Flags.Target;    
                }
            }
            iIndex++;
        }
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
            if ((iFlags[iIndex] & Flags.Walked) == Flags.Walked)
            {
                OpCode opCode = iCodes[iIndex];
                if ((opCode == opcodeCALL) || (opCode == opcodeiJMP))
                {
                    uint callMethodIndex = iOperands[iIndex];
                    methodsCalled[callMethodIndex] = true;
                }
                if (opCode == opcodeJMPIndex)
                {
                    <uint> methods = iJumpTables[iIndex];
                    foreach (var method in methods)
                    {
                        methodsCalled[method] = true;
                    }
                }
            }
            iIndex++;
        } // loop
        ProgessNudge();
    }
    
    // not just NOP, also JMP -> JMP + 1, can cause more short JumpToJump's to work
    bool OptimizeRemoveJMPJMPs()
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
            OpCode opCode = iCodes[iIndex];
            bool removeIt = false;
            
            AddressingModes addressingMode;
            bool isConditional;
            
            if (Asm6502.IsJumpInstruction(opCode, ref addressingMode, ref isConditional) 
                  //&& !isConditional - makes no difference if it is conditional, still does nothing
                  && (addressingMode != AddressingModes.AbsoluteIndirect)  // [nnnn]
                  && (addressingMode != AddressingModes.AbsoluteIndirectX) // [nnnn,X]
                  )
            {
                removeIt = iOperands[iIndex] == iIndex+1;
            }
            if (removeIt)
            {
                iCodes[iIndex]   = OpCode.NOP;
                iLengths[iIndex] = 1;
                modified = true;
            }
            else
            {
                iIndex++;
            }
        } // loop
        return modified;
    }
    
    bool OptimizeJMP()
    {
        if (iCodes.Count < 1)
        {
            return false;
        }
        OpCode opcodeBRA  = Asm6502.GetBInstruction("");
        bool modified = false;
        uint iIndex = 0;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            OpCode opCode = iCodes[iIndex];
            
            AddressingModes addressingMode;
            bool isConditional;
            
            if (Asm6502.IsJumpInstruction(opCode, ref addressingMode, ref isConditional) 
                  && !isConditional
                  && (addressingMode == AddressingModes.Absolute) // nnnn
               )
            {
                long myAddress     = GetInstructionAddress(iIndex);
                long targetAddress = GetInstructionAddress(iOperands[iIndex]);
                long offset = myAddress - targetAddress;
                if ((offset >= -127) && (offset <= 127))
                {
                    opCode = opcodeBRA;
                    iCodes.SetItem(iIndex, opCode);
                    iLengths.SetItem(iIndex, 2);
                    modified = true;
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    bool OptimizeUnreachableToNOP()
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
            OpCode opCode = iCodes[iIndex];
            
            AddressingModes addressingMode;
            bool isConditional;
            
            if ((opCode != OpCode.NOP) && (iFlags[iIndex] == Flags.Unreached))
            {
                iCodes.SetItem  (iIndex, OpCode.NOP);
                iLengths.SetItem(iIndex, 1);
                modified = true;
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    bool OptimizeRemoveNOPs()
    {
        bool modified;
        if (iCodes.Count == 0) { return modified; }
        uint iIndex = 0;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode0 = iCodes[iIndex];
            if (opCode0 == OpCode.NOP)
            {
                uint debugLine = iDebugLines[iIndex];
                Flags flags0 =   iFlags[iIndex];
                
                // preserve debugLine
                if (debugLine != 0)
                {
                    if (iIndex + 1 < iCodes.Count)
                    {
                        if (iDebugLines[iIndex+1] == 0)
                        {
                            iDebugLines[iIndex+1] = debugLine;
                        }
                    }
                }
                // preserve Flags
                if (Flags.Target == (flags0 & Flags.Target))
                {
                    if (iIndex + 1 < iCodes.Count)
                    {
                        iFlags[iIndex+1] = iFlags[iIndex+1] | Flags.Target;
                    }
                } 
                
                // fix jumps
                uint jIndex = 0;
                loop
                {
                    if (jIndex == iCodes.Count) { break; }
                    OpCode opCodej = iCodes[jIndex];
                    
                    AddressingModes addressingMode;
                    bool isConditional;
                    if (Asm6502.IsJumpInstruction(opCodej, ref addressingMode, ref isConditional))
                    {
                        uint iTarget = iOperands[jIndex];
                        if (iTarget > iIndex)
                        {
                            iOperands[jIndex] = iTarget - 1; // one slot closer now
                        }
                    }
                    jIndex++;
                }
                             
                // do the deed
                iCodes.Remove(iIndex);
                iOperands.Remove(iIndex);
                iLengths.Remove(iIndex);
                iDebugLines.Remove(iIndex);
                iFlags.Remove(iIndex);
                iJumpTables.Remove(iIndex);
                              
                modified = true;
                continue;
            }            
            iIndex++;
        }
        
        return modified;
    }
    
    
    
    
    // BEQ BRA -> BNE (for example)
    bool OptimizeBEQBRA()
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
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex];
            
            AddressingModes addressingMode0;
            bool isConditional0;
            AddressingModes addressingMode1;
            bool isConditional1;
            
            if (   Asm6502.IsJumpInstruction(opCode1, ref addressingMode1, ref isConditional1) &&  isConditional1
                && Asm6502.IsJumpInstruction(opCode0, ref addressingMode0, ref isConditional0) && !isConditional0
                && (addressingMode0 == AddressingModes.Relative)
               )
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    if (iOperands[iIndex-1] == iIndex + 1)
                    {
                        if (addressingMode1 == AddressingModes.Relative)        // BnC or BnS
                        {
                            // flipping this bit switches to the opposite condition instruction
                            opCode1 = OpCode(byte(opCode1) ^ 0x20); 
                        }
                        if (addressingMode1 == AddressingModes.ZeroPageRelative) // BBSx or BBRx
                        {
                            // flipping this bit switches to the opposite condition instruction
                            opCode1 = OpCode(byte(opCode1) ^ 0x80);
                            iOperands.SetItem(iIndex, iOperands[iIndex-1]);
                            iLengths.SetItem (iIndex, iLengths[iIndex-1]);
                        }
                        iCodes.SetItem(iIndex, opCode1);
                        
                        
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                
                        modified = true;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    // BRA|JMP to RTS - > RTS
    bool OptimizeJMPRTS()
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
            OpCode opCode0 = iCodes[iIndex];
            
            AddressingModes addressingMode0;
            bool isConditional0;
            
            if (   Asm6502.IsJumpInstruction(opCode0, ref addressingMode0, ref isConditional0) && !isConditional0
                && ((addressingMode0 == AddressingModes.Relative) || (addressingMode0 == AddressingModes.Absolute))
               )
            {
                AddressingModes addressingMode1;
                bool isConditional1;
                
                uint iTarget = iOperands[iIndex];
                OpCode opCode1 = iCodes[iTarget];
                if (Asm6502.IsMethodExitInstruction(opCode1))
                {
                    iCodes.SetItem(iIndex, opCode1);
                    iLengths.SetItem(iIndex, iLengths[iTarget]);
                    iOperands.SetItem(iIndex, iOperands[iTarget]);
                    modified = true;
                }
            }        
            iIndex++;
        } // loop
        return modified;
    }
    
    // BRA|JMP to BRA|JMP -> BRA|JMP
    bool OptimizeJMPJMP()
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
            OpCode opCode0 = iCodes[iIndex];
            
            AddressingModes addressingMode0;
            bool isConditional0;
            
            if (   Asm6502.IsJumpInstruction(opCode0, ref addressingMode0, ref isConditional0) 
                && ((addressingMode0 == AddressingModes.Relative) || (addressingMode0 == AddressingModes.Absolute))
               )
            {
                AddressingModes addressingMode1;
                bool isConditional1;
                
                uint iTarget0 = iOperands[iIndex];
                OpCode opCode1 = iCodes[iTarget0];                               // the 2nd jump can be conditional - future optimization ..
                if (   Asm6502.IsJumpInstruction(opCode1, ref addressingMode1, ref isConditional1) && !isConditional1 
                    && ((addressingMode1 == AddressingModes.Relative) || (addressingMode1 == AddressingModes.Absolute))
                    && (iTarget0 != iIndex) // circular
                   )
                {
                    uint iTarget1 = iOperands[iTarget0];
                    
                    long myAddress     = GetInstructionAddress(iIndex);
                    long targetAddress = GetInstructionAddress(iTarget1);
                    if (addressingMode0 == AddressingModes.Absolute)
                    {
                        // easy case
                        iOperands.SetItem(iIndex, iTarget1);
                        modified = true;
                    }
                    else
                    {
                        // if the first branch is conditional, we need to be careful if we 
                        // also allow conditional for the second branch: same conditions for both?
                        uint instructionLength = Asm6502.GetInstructionLength(opCode0);
                        long offset = targetAddress - (myAddress+instructionLength);
                        if ((offset >= -128) && (offset <= 127))
                        {
                            iOperands.SetItem(iIndex, iTarget1);
                            modified = true;
                        }
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    // RTS RTS -> RTS
    bool OptimizeRTSRTS()
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
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex];
            if ((opCode0 == opcodeRTS) && (opCode1 == opcodeRTS))
            {
                iCodes  [iIndex-1] = OpCode.NOP;
                iLengths[iIndex-1] = 1;
                modified = true;
            }
            else if ((opCode0 == opcodeRTI) && (opCode1 == opcodeRTI))
            {
                iCodes  [iIndex-1] = OpCode.NOP;
                iLengths[iIndex-1] = 1;
                modified = true;
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    // JSR RTS -> iJMP
    bool OptimizeJSRRTS()
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
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex];
            if ((opCode0 == opcodeRTS) && (opCode1 == opcodeJSR))
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    iCodes.SetItem(iIndex-1, opcodeiJMP);
                    modified = true;
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
}
