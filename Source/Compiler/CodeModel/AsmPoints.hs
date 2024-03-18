unit AsmPoints
{
    uses "../CodeGen/AsmStream"
    uses "../CodeGen/OpCodes"
    
    
    <uint>    iCodes;
    <uint>    iLengths;
    <uint>    iOperands;    // original
    <uint>    iJumpTargets; // where can this instruction jump to?
    
    <uint,bool>   iReachable;
    
    uint currentMethod;
    <uint,string> indexDebugInfo;
    <uint,uint> indexMap;   // index -> address
    <uint,uint> addressMap; // address -> index
    
    <uint,uint> inlineMethodCandidates; 
    
    byte opcodeNOP;
    byte opcodeCALL;
    byte opcodeBRA;
    byte opcodeBEQ;
    byte opcodeBNE;
    byte opcodeBCS;
    byte opcodeBCC;
    
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
        foreach (var jump in iJumpTargets)
        {
            if (jump == iIndex)
            {
                return true;
            }
        }
        ProgessNudge();
        return false;
    }
    
    Reset()
    {
        opcodeNOP = OpCodes.GetNOPInstruction();
        opcodeBRA = OpCodes.GetBInstruction("");
        opcodeBEQ = OpCodes.GetBInstruction("Z");
        opcodeBNE = OpCodes.GetBInstruction("NZ");
        opcodeBCS = OpCodes.GetBInstruction("C");
        opcodeBCC = OpCodes.GetBInstruction("NC");
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            opcodeCALL = OpCodes.GetJSRInstruction();
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            opcodeCALL = OpCodes.GetCALLInstruction();
        }
        inlineMethodCandidates.Clear();
    }
    bool InlineCandidatesExist { get { return inlineMethodCandidates.Count != 0; } }
    
    uint Load(uint methodIndex, string when)
    {
        currentMethod = methodIndex;
        
        iCodes.Clear();
        iLengths.Clear();
        iOperands.Clear();
        iJumpTargets.Clear();
        indexDebugInfo.Clear();
        iReachable.Clear();
        
        <byte> code = Code.GetMethodCode(currentMethod);
        uint codeLength = code.Count;
        uint i = 0;
        loop
        {
            uint operand;
            uint jumpTableSize;
            <byte> extraContent;
            uint opCode = code[i];
            byte instructionLength = OpCodes.GetInstructionLength(byte(opCode));
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
            
            iCodes.Append(opCode);
            iLengths.Append(instructionLength);
            iOperands.Append(operand);
            iJumpTargets.Append(OpCodes.InvalidAddress); // invalid for now
            
            i += instructionLength;
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
    
    InitializeJumpTargets()
    {
        uint iCodesLength = iCodes.Count;
        uint iIndex;
        uint instructionAddress;
        
        //PrintLn("InitializeJumpTargets:");
        loop
        {
            if (iIndex == iCodesLength)
            {
                break;
            }
            uint opCode  = iCodes[iIndex];
            AddressingModes addressingMode;
            bool isConditional;
            if (OpCodes.IsJumpInstruction(opCode, ref addressingMode, ref isConditional))
            {
                uint address = GetInstructionAddress(iIndex);
                
                //Print((iIndex.ToString()).Pad(' ', 3) + (address+0xE004).ToHexString(4) + " " + opCode.ToHexString(2) + " " + OpCodes.GetName(byte(opCode)) + " ");
                uint operand           = iOperands[iIndex];
                
                long offset;
                long jumpTargetAddress;
            
                if (addressingMode == AddressingModes.Relative) // dd
                {
                    offset = operand;
                    if (offset > 127)
                    {
                        offset = offset - 256; // 255 -> -1
                    }
                    //Print(offset.ToString() + " ");
                    
                    jumpTargetAddress = long(instructionAddress) + offset + 2;
                    //Print((jumpTargetAddress+0xE004).ToHexString(4) + " ");
                }
                else if (addressingMode == AddressingModes.Absolute) // nnnn
                {
                    //Print((operand+0xE004).ToHexString(4) + " ");
                    uint jumpIndex;
                    jumpTargetAddress = long(operand);
                }
                else if (addressingMode == AddressingModes.ZeroPageRelative) // nn,dd
                {
                    offset = byte(operand >> 8);
                    if (offset > 127)
                    {
                        offset = offset - 256; // 255 -> -1
                    }
                    //Print((operand & 0xFF).ToHexString(2) + " " + offset.ToString() + " ");
                    
                    jumpTargetAddress = long(instructionAddress) + offset + 2;
                    //Print((jumpTargetAddress+0xE004).ToHexString(4) + " ");
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
                //PrintLn("-> " + jumpIndex.ToString());
                
                
                iJumpTargets.SetItem(iIndex, jumpIndex);
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
        
        //PrintLn("Save:");
        
        loop
        {
            uint opCode = iCodes[index];
            code.Append(byte(opCode));
            
            uint instructionLength = iLengths[index];
            if (instructionLength > 3)
            {
                Die(0x0B);
            }
            else if (instructionLength > 1)
            {
                uint operand = iOperands[index];
                
                AddressingModes addressingMode;
                bool isConditional;
                bool isJump = OpCodes.IsJumpInstruction(opCode, ref addressingMode, ref isConditional);
                if (isJump)
                {
                    // use the index to calculate the new offset
                    long currentAddress = indexMap[index];
                    
                    //Print((index.ToString()).Pad(' ', 3) + (currentAddress+0xE004).ToHexString(4) + " " + opCode.ToHexString(2) + " " + OpCodes.GetName(byte(opCode)) + " ");
                    
                    uint jumpTarget     = iJumpTargets[index];
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
                    //Print(jumpTarget.ToString() + " -> " + (jumpAddress + 0xE004).ToHexString(4) + " ");
                    
                    if ((addressingMode == AddressingModes.Relative) || (addressingMode == AddressingModes.ZeroPageRelative))
                    {
                        long offset = jumpAddress - currentAddress - 2;
                        if ((offset < -128) || (offset > 127))
                        {
                            PrintLn("Bad Jump:  " + offset.ToString());
                            Die(0x0B);
                        }
                        //Print(offset.ToString() + " ");
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
                    //PrintLn();
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
                uint opCode = iCodes[iIndex];
                AddressingModes addressingMode;
                bool isConditional;
                bool isJump = OpCodes.IsJumpInstruction(opCode, ref addressingMode, ref isConditional);
                if (isJump && isConditional)
                {
                    // conditional branch: explore both paths
                    tailCalls.Append(iJumpTargets[iIndex]);
                    
                    // fall through to 'continue' on the non-branch path ..
                }
                else if (isJump)
                {
                    // unconditional branch: continue on the branch path only
                    iIndex = iJumpTargets[iIndex];
                    continue;
                }
                else if (OpCodes.IsMethodExitInstruction(opCode))
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
                uint opCode = iCodes[iIndex];
                if (opCode == opcodeCALL)
                {
                    uint callMethodIndex = iOperands[iIndex];
                    methodsCalled[callMethodIndex] = true;
                }
            }
            iIndex++;
        } // loop
        ProgessNudge();
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
            uint opCode = iCodes[iIndex];
            if (opCode == opcodeCALL)
            {
                uint callMethodIndex = iOperands[iIndex];
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
                            
                            //Instruction retFast = Instruction(inlineCode[inlineCode.Count-1]);
                            //if (retFast == Instruction.RETFAST)
                            //{
                            //    // just so this never ends up inlined
                            //    inlineCode[inlineCode.Count-1] = byte(Instruction.NOP);
                            //}
                            
                            // replace what was the method call with NOPs
                            for (uint i=0; i < availableSpace; i++)
                            {
                                rawCode.SetItem(inlineAddress+i, opcodeNOP);
                            }
                            
                            // put the NOPs in front so that unconditional branches with PUSHI0 or PUSHI1 are spotted sooner
                            uint nopOffset = availableSpace - sizeInBytes;
                            
                            // fill the space with the bytes from the method (could be trailing NOPs to remove later)
                            while (inlineCode.Count < sizeInBytes)
                            {
                                inlineCode.Append(byte(opcodeNOP));
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
    
    RemoveInstruction(uint iRemoval)
    {
        RemoveInstruction(iRemoval, true);
    }
    RemoveInstruction(uint iRemoval, bool keepDebugLine)
    {
        iCodes.Remove(iRemoval);
        iLengths.Remove(iRemoval);
        iOperands.Remove(iRemoval);
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
            uint jumpTarget = iJumpTargets[iIndex];
            if (jumpTarget != OpCodes.InvalidAddress)
            {
                if (jumpTarget > iRemoval)
                {
                    jumpTarget--;
                }
                iJumpTargets.SetItem(iIndex, jumpTarget);
            }
        }
        ProgessNudge();
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
            uint opCode = iCodes[iIndex];
            bool removeIt = false;
            
            AddressingModes addressingMode;
            bool isConditional;
            
            if (opCode == opcodeNOP)
            {
                removeIt = true;
            }
            else if (OpCodes.IsJumpInstruction(opCode, ref addressingMode, ref isConditional) 
                  && !isConditional
                  && (addressingMode != AddressingModes.AbsoluteIndirect) // (nnnn)
                  && (addressingMode != AddressingModes.AbsoluteIndirectX) // (nnnn,X)
                  )
            {
                removeIt = iJumpTargets[iIndex] == iIndex+1;
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
    
    bool OptimizeJMP()
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
            uint opCode = iCodes[iIndex];
            
            AddressingModes addressingMode;
            bool isConditional;
            
            if (OpCodes.IsJumpInstruction(opCode, ref addressingMode, ref isConditional) 
                  && !isConditional
                  && (addressingMode == AddressingModes.Absolute) // nnnn
               )
            {
                long myAddress     = GetInstructionAddress(iIndex);
                long targetAddress = GetInstructionAddress(iJumpTargets[iIndex]);
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
    // BEQ BRA -> BNE (for example
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
            uint opCode1 = iCodes[iIndex-1];
            uint opCode0 = iCodes[iIndex];
            
            AddressingModes addressingMode0;
            bool isConditional0;
            AddressingModes addressingMode1;
            bool isConditional1;
            
            if (   OpCodes.IsJumpInstruction(opCode1, ref addressingMode1, ref isConditional1) &&  isConditional1
                && OpCodes.IsJumpInstruction(opCode0, ref addressingMode0, ref isConditional0) && !isConditional0
                && (addressingMode0 == AddressingModes.Relative)
               )
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    if (iJumpTargets[iIndex-1] == iIndex + 1)
                    {
                        uint flipOpCode;
                             if (opCode1 == opcodeBEQ) { flipOpCode = opcodeBNE; }
                        else if (opCode1 == opcodeBNE) { flipOpCode = opcodeBEQ; }
                        else if (opCode1 == opcodeBCC) { flipOpCode = opcodeBCS; }
                        else if (opCode1 == opcodeBCS) { flipOpCode = opcodeBCC; }
                        else                           { Die(0x0A);              }
                        
                        iCodes.SetItem(iIndex, flipOpCode);
                        RemoveInstruction(iIndex-1);
                        modified = true;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
}
