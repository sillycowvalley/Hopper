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
        CallRet   = 0b0000000000000010,
        ReadA     = 0b0000000000000100,
        ReadX     = 0b0000000000001000,
        ReadY     = 0b0000000000010000,
        WriteA    = 0b0000000000100000,
        WriteX    = 0b0000000001000000,
        WriteY    = 0b0000000010000000,
    }
    
    <OpCode> iCodes;        // 6502 instruction opCodes
    <uint>   iOperands;     // 6502 instruction operands (could be byte or word)
    <uint>   iLengths;      // 6502 instruction lengths (not operand width)
    <Flags>  iFlags;        // reachable? jump target?
    <uint>   iDebugLines;   // Hopper source line associated with this instruction (mostly 0)
    <string> iLabels;       // lable associated with this instruction (mostly "")
    <byte>   iExtras;       // used by BBRx and BBSz
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
    
    OpCode GetOpcodeiJMP()
    {
        return opcodeiJMP;
    }
    OpCode GetOpcodeRTS()
    {
        return opcodeRTS;
    }
    
    
    
    bool walkVerbose;   
    WalkVerbose(uint iIndex, WalkStats searchFor, WalkStats searchAgainst, uint pointLimit)
    {
        walkVerbose = true;
        _ = WalkAhead(iIndex, searchFor, searchAgainst, pointLimit, "");
        walkVerbose = false;
    }
    bool WalkAhead(uint iIndex, WalkStats searchFor, WalkStats searchAgainst, uint pointLimit)
    {
        return WalkAhead(iIndex, searchFor, searchAgainst, pointLimit, "");
    }
    bool WalkAhead(uint iIndex, WalkStats searchFor, WalkStats searchAgainst, uint pointLimit, string indent)
    {
        bool success = false;
        WalkStats walkStats;
        
        loop
        {
            if ((pointLimit == 0) || (iIndex == iCodes.Count)) 
            { 
                if (walkVerbose)
                {
                    Print("   -> " + currentMethod.ToHexString(4) + ":" + iIndex.ToString() + " -ve Exit Limit");
                }
                break; 
            }
            
            OpCode opCode0 = iCodes[iIndex];
            bool noIncrement;
            if (walkVerbose)
            {
                Print("  " + GetName(opCode0));
            }
            switch (opCode0)
            {
                // Exit
                case OpCode.RTI:
                case OpCode.STP:
                case OpCode.HLT:
                case OpCode.BRK:
                {
                    walkStats |= WalkStats.Exit;
                }
                case OpCode.RTS:
                case OpCode.JSR_nn:
                case OpCode.iJMP_nn:
                case OpCode.JMP_inn:
                {
                    walkStats |= WalkStats.CallRet;
                }
                
                // Write:
                case OpCode.LDA_n:
                case OpCode.LDA_nn:
                case OpCode.PLA:
                case OpCode.LDA_z:
                case OpCode.LDA_iz:
                {
                    walkStats |= WalkStats.WriteA;
                }
                case OpCode.LDX_n:
                case OpCode.PLX:
                case OpCode.LDX_z:
                case OpCode.TSX:
                {
                    walkStats |= WalkStats.WriteX;
                }
                case OpCode.LDY_n:
                case OpCode.PLY:
                case OpCode.LDY_z:
                {
                    walkStats |= WalkStats.WriteY;
                }
                case OpCode.LDA_nnX:
                {
                    walkStats |= WalkStats.WriteA;
                    walkStats |= WalkStats.ReadX;
                }
                case OpCode.LDA_nnY:
                {
                    walkStats |= WalkStats.WriteA;
                    walkStats |= WalkStats.ReadY;
                }
                
                // Read:
                case OpCode.STA_nn:
                case OpCode.STA_z:
                case OpCode.STA_iz:
                case OpCode.PHA:
                case OpCode.CMP_n:
                case OpCode.CMP_z:
                {
                    walkStats |= WalkStats.ReadA;
                }
                case OpCode.STA_nnX:
                case OpCode.STA_zX:
                case OpCode.STA_izX:
                {
                    walkStats |= WalkStats.ReadA;
                    walkStats |= WalkStats.ReadX;
                }
                case OpCode.STA_nnY:
                case OpCode.STA_izY:
                {
                    walkStats |= WalkStats.ReadA;
                    walkStats |= WalkStats.ReadY;
                }
                case OpCode.STX_nn:
                case OpCode.STX_z:
                case OpCode.PHX:
                case OpCode.CPX_n:
                case OpCode.CPX_z:
                case OpCode.DEC_nnX:
                case OpCode.INC_nnX:
                {
                    walkStats |= WalkStats.ReadX;
                }
                case OpCode.STX_zY:
                case OpCode.STY_zX:
                {
                    walkStats |= WalkStats.ReadX;
                    walkStats |= WalkStats.ReadY;
                }
                case OpCode.STY_nn:
                case OpCode.STY_z:
                case OpCode.CPY_n:
                case OpCode.CPY_z:
                case OpCode.PHY:
                {
                    walkStats |= WalkStats.ReadY;
                }
                
                case OpCode.LDA_izY:
                {
                    walkStats |= WalkStats.ReadY;
                    walkStats |= WalkStats.WriteA;
                }
                case OpCode.LDA_izX:
                case OpCode.LDA_zX:
                {
                    walkStats |= WalkStats.ReadX;
                    walkStats |= WalkStats.WriteA;
                }
                
                // Read and Write
                case OpCode.ADC_n:
                case OpCode.ADC_nn:
                case OpCode.ADC_z:
                case OpCode.ADC_iz:
                case OpCode.SBC_n:
                case OpCode.SBC_nn:
                case OpCode.SBC_z:
                case OpCode.SBC_iz:
                case OpCode.ROR:
                case OpCode.AND_n:
                case OpCode.ORA_n:
                case OpCode.ASL:
                {
                    walkStats |= WalkStats.ReadA;
                    walkStats |= WalkStats.WriteA;
                }
                
                case OpCode.ADC_izX:
                case OpCode.ADC_nnX:
                case OpCode.ADC_zX:
                case OpCode.SBC_zX:
                case OpCode.SBC_izX:
                case OpCode.SBC_nnX:
                {
                    walkStats |= WalkStats.ReadX;
                    walkStats |= WalkStats.ReadA;
                    walkStats |= WalkStats.WriteA;
                }
                case OpCode.CMP_nnX:
                {
                    walkStats |= WalkStats.ReadX;
                    walkStats |= WalkStats.ReadA;
                }
                case OpCode.ADC_nnY:
                case OpCode.ADC_izY:
                case OpCode.SBC_izY:
                case OpCode.SBC_nnY:
                {
                    walkStats |= WalkStats.ReadY;
                    walkStats |= WalkStats.ReadA;
                    walkStats |= WalkStats.WriteA;
                }
                
                case OpCode.TAY:
                {
                    walkStats |= WalkStats.ReadA;
                    walkStats |= WalkStats.WriteY;
                }
                case OpCode.TAX:
                {
                    walkStats |= WalkStats.ReadA;
                    walkStats |= WalkStats.WriteX;
                }
                case OpCode.TYA:
                {
                    walkStats |= WalkStats.ReadY;
                    walkStats |= WalkStats.WriteA;
                }
                case OpCode.TXA:
                {
                    walkStats |= WalkStats.ReadX;
                    walkStats |= WalkStats.WriteA;
                }
                
                case OpCode.INX:
                case OpCode.DEX:
                {
                    walkStats |= WalkStats.ReadX;
                    walkStats |= WalkStats.WriteX;
                }
                
                case OpCode.INY:
                case OpCode.DEY:
                {
                    walkStats |= WalkStats.ReadY;
                    walkStats |= WalkStats.WriteY;
                }
                
                // Ignore:
                case OpCode.RMB0_z:
                case OpCode.RMB1_z:
                case OpCode.RMB2_z:
                case OpCode.RMB3_z:
                case OpCode.RMB4_z:
                case OpCode.RMB5_z:
                case OpCode.RMB6_z:
                case OpCode.RMB7_z:
                case OpCode.SMB0_z:
                case OpCode.SMB1_z:
                case OpCode.SMB2_z:
                case OpCode.SMB3_z:
                case OpCode.SMB4_z:
                case OpCode.SMB5_z:
                case OpCode.SMB6_z:
                case OpCode.SMB7_z:
                
                case OpCode.INC_z:
                case OpCode.DEC_z:
                case OpCode.LSR_z:
                case OpCode.NOP:
                case OpCode.ROR_z:
                case OpCode.ROL_z:
                case OpCode.ASL_z:
                
                case OpCode.CLC:
                case OpCode.SEC:
                
                case OpCode.CLI:
                case OpCode.SEI:
                
                case OpCode.INC_nn:
                case OpCode.DEC_nn:
                
                case OpCode.STZ_z:
                case OpCode.STZ_zX:
                case OpCode.STZ_nn:
                case OpCode.STZ_nnX:
                {
                }
                
                // Jump!
                case OpCode.BRA_e:
                case OpCode.JMP_nn:
                {
                    iIndex = iOperands[iIndex];
                    noIncrement = true;
                }
                
                // Branch:
                case OpCode.BCC_e:
                case OpCode.BCS_e:
                case OpCode.BEQ_e:
                case OpCode.BMI_e:
                case OpCode.BNE_e:
                case OpCode.BPL_e:
                case OpCode.BVC_e:
                case OpCode.BVS_e:
                {
                    bool success0 = WalkAhead(iIndex+1,          searchFor, searchAgainst, pointLimit, indent + "  ");
                    bool success1 = WalkAhead(iOperands[iIndex], searchFor, searchAgainst, pointLimit, indent + "  ");
                    success = success0 && success1;
                    return success; 
                }
                 // Abandon:
                default:
                {
                    string name = Asm6502.ToString(opCode0);
                    PrintLn();
                    Print("'" + name + "' not supported in WalkAhead");
                    walkStats = WalkStats.None;
                    break;
                }
            }
            if (WalkStats.None != (walkStats & searchAgainst))
            {
                success = false; // any negative hit is a failure
                if (walkVerbose)
                {
                    string reason = "Registers";
                    if (WalkStats.Exit == (walkStats & searchAgainst))
                    {
                        reason = "Exit";
                    }
                    if (WalkStats.CallRet == (walkStats & searchAgainst))
                    {
                        reason = "Call|Ret";
                    }
                    Print("   -> " + currentMethod.ToHexString(4) + ":" + iIndex.ToString() + " -ve " + reason);
                }
                break;
            }
            if (WalkStats.Exit == (walkStats & searchFor))
            {
                success = true; // positive exit is always a success
                if (walkVerbose)
                {
                    Print("   -> " + currentMethod.ToHexString(4) + ":" + iIndex.ToString() + " +ve Exit");
                }
                break;
            }    
            if (WalkStats.None != (walkStats & searchFor))
            {
                if (walkVerbose)
                {
                    string reason = "Registers";
                    if (WalkStats.Exit == (walkStats & searchFor))
                    {
                        reason = "Exit";
                    }
                    if (WalkStats.CallRet == (walkStats & searchFor))
                    {
                        reason = "Call|Ret";
                    }
                    Print("   -> " + currentMethod.ToHexString(4) + ":" + iIndex.ToString() + " +ve " + reason);
                }
                success = true;
                break;
            }
            if (!noIncrement)
            {
                iIndex++;
            }
            pointLimit--;
        }
        return success;
    }       
    bool WithinShortJumpLimit(long offset)
    {
        return (offset >= -126) && (offset <= 125);
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
    uint GetLengthAt(uint index)
    {
        return iLengths[index];
    }
    uint GetInstructionCount()
    {
        return iCodes.Count;
    }
    
    OpCode GetInstructionAt(uint index)
    {
        return iCodes[index];
    }
    
    uint GetOperandAt(uint index)
    {
        return iOperands[index];
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
        iLabels.Clear();
        iJumpTables.Clear();
        iFlags.Clear();
        iExtras.Clear();
        
        <byte> code = Code.GetMethodCode(currentMethod);
        
        <string,variant> methodSymbols = Code.GetMethodSymbols(methodIndex);
        <string,string> debugInfo = methodSymbols["debug"];
        <string,string> labelInfo;
        if (methodSymbols.Contains("labels"))
        {
            labelInfo = methodSymbols["labels"];
        }
        
        uint codeLength = code.Count;
        uint i = 0;
        
        uint tableSizeInWords;
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
            if ((opCode == OpCode.CPX_n) || (opCode == OpCode.CPY_n))
            {
                tableSizeInWords = operand + 1;
            }
            if (opCode == opcodeJMPIndex)
            {
                instructionLength += tableSizeInWords * 2;
            }
            
            byte extra = 0;
            AddressingModes addressingMode = GetAddressingMode(opCode);
            if (AddressingModes.ZeroPageRelative == addressingMode)
            {
                // nn,dd 
                extra = byte(operand & 0xFF);
                operand = (operand >> 8);
            }
            iExtras.Append(extra);
            
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
            string label;
            if (labelInfo.Contains(instructionAddress.ToString()))
            {
                label = labelInfo[instructionAddress.ToString()];
            }
            iLabels.Append(label);
            
            
            <uint> empty;
            iJumpTables.Append(empty); // place holder
            
            uint iIndex = iCodes.Count-1;
            uint iTable = i + 3;
            i += instructionLength;
            
            if (opCode == opcodeJMPIndex)
            {
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
                
                bool fixedAddress;
                           
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
                    offset = operand;
                    if (offset > 127)
                    {
                        offset = offset - 256; // 255 -> -1
                    }
                    jumpTargetAddress = long(instructionAddress) + offset + instructionLength;
                }
                else if (addressingMode == AddressingModes.AbsoluteIndirect) // [nnnn]
                {
                    fixedAddress = true;
                }
                else
                {
                    Die(0x0B); 
                }
                if (!fixedAddress)
                {
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
        <string,string> labelInfo;
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
                    isJump = (addressingMode != AddressingModes.AbsoluteIndirect); // [nnnn]
                }
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
                            PrintLn("Bad Jump:  " + offset.ToString() + ", currentMethod=" + currentMethod.ToHexString(4) + ", address=" + (code.Count).ToString());
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
                            operand = iExtras[index] + (uint(offset) << 8);
                        }
                    }
                    else if (addressingMode == AddressingModes.Absolute)
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
            if (iLabels[index] != "")
            {
                labelInfo[(indexToAddress[index]).ToString()] = iLabels[index];
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
        Code.SetMethodLabelInfo(currentMethod, labelInfo);
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
                if (WithinShortJumpLimit(offset))
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
                string label   = iLabels[iIndex];
                Flags flags0   = iFlags[iIndex];
                
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
                // preserve label
                if (label != "")
                {
                    if (iIndex + 1 < iCodes.Count)
                    {
                        if (iLabels[iIndex+1] == "")
                        {
                            iLabels[iIndex+1] = label;
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
                iLabels.Remove(iIndex);
                iFlags.Remove(iIndex);
                iJumpTables.Remove(iIndex);
                iExtras.Remove(iIndex);
                              
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
                        else if (addressingMode1 == AddressingModes.ZeroPageRelative) // BBSx or BBRx
                        {
                            // flipping this bit switches to the opposite condition instruction
                            opCode1 = OpCode(byte(opCode1) ^ 0x80);
                                                        
                            // Keep the length and the zero page address (iExtras) from the BBSx|BBRx instruction (iIndex-1)
                            // but use the jump index from the branch instruction (iIndex-0):
                            iLengths.SetItem (iIndex, iLengths[iIndex-1]);
                            iExtras.SetItem  (iIndex, iExtras[iIndex-1]);
                        }
                        else
                        {
                            Die(0x0B);
                        }
                        iCodes  [iIndex]   = opCode1;
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
    
    // BEQ JMP -> BNE (for example)
    bool OptimizeBEQJMP()
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
            
            AddressingModes addressingMode1;
            bool isConditional1;
            
            if (   Asm6502.IsJumpInstruction(opCode1, ref addressingMode1, ref isConditional1) &&  isConditional1
                && (opCode0 == OpCode.JMP_nn)
               )
            {
                if (!IsTargetOfJumps(iIndex))
                {
                    if (iOperands[iIndex-1] == iIndex + 1)
                    {
                        uint address0 = indexToAddress[iIndex-0];
                        uint addressJ = indexToAddress[iOperands[iIndex-0]];
                        
                        long distance = long(addressJ) - long(address0);
                        if (WithinShortJumpLimit(distance))
                        {
                            // even if all the instructions inbetween are 3 bytes long,
                            // this is still within range for the conditional relative offset
                            if (addressingMode1 == AddressingModes.Relative)        // BnC or BnS
                            {
                                // flipping this bit switches to the opposite condition instruction
                                opCode1 = OpCode(byte(opCode1) ^ 0x20); 
                            }
                            else if (addressingMode1 == AddressingModes.ZeroPageRelative) // BBSx or BBRx
                            {
                                // flipping this bit switches to the opposite condition instruction
                                opCode1 = OpCode(byte(opCode1) ^ 0x80);
                                                            
                                // Keep the length and the zero page address (iExtras) from the BBSx|BBRx instruction (iIndex-1)
                                // but use the jump index from the branch instruction (iIndex-0):
                                iLengths.SetItem (iIndex, iLengths[iIndex-1]);
                                iExtras.SetItem  (iIndex, iExtras[iIndex-1]);
                            }
                            else
                            {
                                Die(0x0B);
                            }
                            iCodes  [iIndex]   = opCode1;
                            iLengths[iIndex]   = 2;
                            iCodes  [iIndex-1] = OpCode.NOP;
                            iLengths[iIndex-1] = 1;
                            modified = true;
                        }
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
                        if (WithinShortJumpLimit(offset))
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
                    uint iTarget1 = iOperands[iIndex-1];
                    
                    string targetMethodName = Code.GetMethodName(iTarget1);
                    if (!targetMethodName.StartsWith("EEPROM.")) // because of the emulator ..
                    {
                        iCodes.SetItem(iIndex-1, opcodeiJMP);
                        modified = true;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    bool OptimizeInlineSmallMethods(<uint, <OpCode> > smallMethodOpCodes, <uint, <uint> > smallMethodOperands, <uint, <uint> > smallMethodLengths)
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
            if (opCode == opcodeCALL)
            {
                uint callMethodIndex = iOperands[iIndex];
                if (smallMethodOpCodes.Contains(callMethodIndex))
                {
                    <OpCode> opCodes  = smallMethodOpCodes [callMethodIndex];
                    <uint>   operands = smallMethodOperands[callMethodIndex];
                    <uint>   lengths  = smallMethodLengths [callMethodIndex];
                    
                    switch(opCodes.Count)
                    {
                        case 0:
                        {
                            iCodes[iIndex]   = OpCode.NOP;
                            iLengths[iIndex] = 1;
                            modified = true;
                            continue;
                        }
                        case 1:
                        {
                            string currentMethodName = Code.GetMethodName(currentMethod);
                            string callMethodName = Code.GetMethodName(callMethodIndex);
                            string name = Asm6502.GetName(opCodes[0]);
                            switch (name)
                            {
                                case "LDA":
                                case "STA":
                                case "CLC":
                                case "STZ":
                                {
                                    iCodes[iIndex]    = opCodes[0];
                                    iLengths[iIndex]  = lengths[0];
                                    iOperands[iIndex] = operands[0];
                                    modified = true;
                                    //PrintLn(" " + currentMethodName + "->" + callMethodName + ":" + Asm6502.GetName(opCodes[0]));
                                    continue;
                                }
                                case "iJMP":
                                {
                                    // will be caught by OptimizeiJMPOnlyCallsInMethod
                                }
                                default:
                                {    
                                    if (opCodes[0] == OpCode.JMP_inn)
                                    {
                                    }
                                    else
                                    {
                                        if (IsExperimental)
                                        {
                                            PrintLn(" Inline? " + currentMethodName + "->" + callMethodName + ":" + Asm6502.GetName(opCodes[0]));
                                        }
                                    }
                                }
                            }
                        }
                        default:
                        {
                            // TODO
                            // string currentMethodName = Code.GetMethodName(currentMethod);
                            // string callMethodName = Code.GetMethodName(callMethodIndex);
                            // PrintLn(" Inline? " + currentMethodName + "->" + callMethodName + ":" + (opCodes.Count).ToString());
                        }
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    // iJMP-only method call optimization: replace calls to iJMP-only methods with direct iJMP
    bool OptimizeiJMPOnlyCallsInMethod(<uint,uint> ijmpOnlyMethods)
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
            if ((opCode == opcodeCALL) || (opCode == opcodeiJMP))
            {
                uint callMethodIndex = iOperands[iIndex];
                
                // Check if this method is an iJMP-only method
                if (ijmpOnlyMethods.Contains(callMethodIndex))
                {
                    uint targetMethodIndex = ijmpOnlyMethods[callMethodIndex];
                    
                    // Replace with what the method actually does: iJMP to target
                    //iCodes.SetItem(iIndex, opcodeiJMP);
                    iOperands.SetItem(iIndex, targetMethodIndex);
                    modified = true;
                    
                    /*
                    string currentMethodName = Code.GetMethodName(currentMethod);
                    string callMethodName = Code.GetMethodName(callMethodIndex);
                    string targetMethodName = Code.GetMethodName(targetMethodIndex);
                    PrintLn(currentMethodName + ": " + callMethodName + ":" + Asm6502.ToString(opCode) + "->" + targetMethodName);
                    */
                }
            }
            iIndex++;
        }
        return modified;
    }
    
    bool OptimizeLDASTAtoSTZ()
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
            if (!IsTargetOfJumps(iIndex))
            {
                OpCode opCode1 = iCodes[iIndex-1];
                if ((opCode1 == OpCode.LDA_n) && (iOperands[iIndex-1] == 0) && (Asm6502.GetName(opCode0) == "STA")) // TODO : loop on consecutive STA's
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteA, WalkStats.ReadA | WalkStats.CallRet | WalkStats.Exit, 10))
                    {
                        if (opCode0 == OpCode.STA_z)
                        {
                            iCodes  [iIndex-1] = OpCode.NOP;
                            iLengths[iIndex-1] = 1;
                            iCodes  [iIndex]   = OpCode.STZ_z;
                            modified = true;
                        }
                        else if (opCode0 == OpCode.STA_nnX)
                        {
                            iCodes  [iIndex-1] = OpCode.NOP;
                            iLengths[iIndex-1] = 1;
                            iCodes  [iIndex]   = OpCode.STZ_nnX;
                            modified = true;
                        }
                        else if ((opCode0 == OpCode.STA_iz) || (opCode0 == OpCode.STA_izX) || (opCode0 == OpCode.STA_izY))
                        {
                            // no STZ version
                        }
                        else
                        {
                            // TODO
                            //PrintLn();
                            //Print("STZ:"); WalkVerbose(iIndex+1, WalkStats.WriteA, WalkStats.ReadA | WalkStats.CallRet | WalkStats.Exit, 10);
                        }
                    }
                    else
                    {
                        //PrintLn();
                        //Print("STZ: " + iIndex.ToString() + " "); WalkVerbose(iIndex+1, WalkStats.WriteA, WalkStats.ReadA | WalkStats.CallRet| WalkStats.Exit, 10);
                    }
                } 
                else if ((opCode1 == OpCode.LDX_n) && (iOperands[iIndex-1] == 0) && (Asm6502.GetName(opCode0) == "STX"))
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteX, WalkStats.ReadX | WalkStats.CallRet | WalkStats.Exit, 10))
                    {
                        // TODO
                        PrintLn();
                        Print("X " + Asm6502.ToString(opCode0) + ": " + iIndex.ToString() + " "); WalkVerbose(iIndex+1, WalkStats.WriteX, WalkStats.Exit | WalkStats.ReadX | WalkStats.CallRet, 10);
                    }
                    else
                    {
                        //PrintLn();
                        //Print("X " + Asm6502.ToString(opCode0) + ": " + iIndex.ToString() + " "); WalkVerbose(iIndex+1, WalkStats.WriteX, WalkStats.Exit | WalkStats.ReadX | WalkStats.CallRet, 10);
                    }
                }
                else if ((opCode1 == OpCode.LDY_n) && (iOperands[iIndex-1] == 0) && (Asm6502.GetName(opCode0) == "STY"))
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteY, WalkStats.Exit | WalkStats.ReadY | WalkStats.CallRet, 10))
                    {
                        // TODO
                        PrintLn();
                        Print("Y " + Asm6502.ToString(opCode0) + ": " + iIndex.ToString() + " "); WalkVerbose(iIndex+1, WalkStats.WriteY, WalkStats.Exit | WalkStats.ReadY | WalkStats.CallRet, 10);
                    }
                    else
                    {
                        //PrintLn();
                        //Print("Y " + Asm6502.ToString(opCode0) + ": " + iIndex.ToString() + " "); WalkVerbose(iIndex+1, WalkStats.WriteY, WalkStats.Exit | WalkStats.ReadY | WalkStats.CallRet, 10);
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeSMBandRMB()
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
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex];
            if (!IsTargetOfJumps(iIndex) && !IsTargetOfJumps(iIndex-1))
            {
                if ((opCode2  == OpCode.LDA_z) && (opCode1 == OpCode.ORA_n) && (opCode0 == OpCode.STA_z) &&
                    (iOperands[iIndex-2] == iOperands[iIndex-0]))
                {
                    if (WalkAhead(iIndex+1, WalkStats.Exit | WalkStats.WriteA, WalkStats.ReadA | WalkStats.CallRet, 10))
                    {
                        OpCode newOpCode = OpCode.NOP;
                        switch (iOperands[iIndex-1])
                        {
                            case 0b00000001: { newOpCode = OpCode.SMB0_z; }
                            case 0b00000010: { newOpCode = OpCode.SMB1_z; }
                            case 0b00000100: { newOpCode = OpCode.SMB2_z; }
                            case 0b00001000: { newOpCode = OpCode.SMB3_z; }
                            case 0b00010000: { newOpCode = OpCode.SMB4_z; }
                            case 0b00100000: { newOpCode = OpCode.SMB5_z; }
                            case 0b01000000: { newOpCode = OpCode.SMB6_z; }
                            case 0b10000000: { newOpCode = OpCode.SMB7_z; }
                        }
                        if (newOpCode != OpCode.NOP)
                        {
                            iCodes  [iIndex-2] = OpCode.NOP;
                            iLengths[iIndex-2] = 1;
                            iCodes  [iIndex-1] = OpCode.NOP;
                            iLengths[iIndex-1] = 1;
                            iCodes  [iIndex-0] = newOpCode;
                            iLengths[iIndex-0] = 2;
                            modified = true;
                        }
                            
                    }
                }
                if ((opCode2  == OpCode.LDA_z) && (opCode1 == OpCode.AND_n) && (opCode0 == OpCode.STA_z) &&
                    (iOperands[iIndex-2] == iOperands[iIndex-0]))
                {
                    if (WalkAhead(iIndex+1, WalkStats.Exit | WalkStats.WriteA, WalkStats.ReadA | WalkStats.CallRet, 10))
                    {
                        OpCode newOpCode = OpCode.NOP;
                        switch (iOperands[iIndex-1])
                        {
                            case 0b11111110: { newOpCode = OpCode.RMB0_z; }
                            case 0b11111101: { newOpCode = OpCode.RMB1_z; }
                            case 0b11111011: { newOpCode = OpCode.RMB2_z; }
                            case 0b11110111: { newOpCode = OpCode.RMB3_z; }
                            case 0b11101111: { newOpCode = OpCode.RMB4_z; }
                            case 0b11011111: { newOpCode = OpCode.RMB5_z; }
                            case 0b10111111: { newOpCode = OpCode.RMB6_z; }
                            case 0b01111111: { newOpCode = OpCode.RMB7_z; }
                        }
                        if (newOpCode != OpCode.NOP)
                        {
                            iCodes  [iIndex-2] = OpCode.NOP;
                            iLengths[iIndex-2] = 1;
                            iCodes  [iIndex-1] = OpCode.NOP;
                            iLengths[iIndex-1] = 1;
                            iCodes  [iIndex-0] = newOpCode;
                            iLengths[iIndex-0] = 2;
                            modified = true;
                        }
                            
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeNOPExists()
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
            OpCode opCode0 = iCodes[iIndex];
            if (opCode0 == OpCode.NOP)
            {
                modified = true;
                break;
            }
            iIndex++;
        }
        return modified;
    }
    bool OptimizePeep()
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
            if (!IsTargetOfJumps(iIndex) )
            {
                OpCode opCode1 = iCodes[iIndex-1];
                OpCode opCode0 = iCodes[iIndex];
                if ((opCode1 == OpCode.PHA) && (opCode0 == OpCode.PLA))
                {
                    iCodes  [iIndex-1] = OpCode.NOP;
                    iLengths[iIndex-1] = 1;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    iLengths[iIndex-0] = 1;
                    modified = true;
                }
                if ((opCode1 == OpCode.PHX) && (opCode0 == OpCode.PLA))
                {
                    iCodes  [iIndex-1] = OpCode.TXA;
                    iLengths[iIndex-1] = 1;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    iLengths[iIndex-0] = 1;
                    modified = true;
                }
                if ((opCode1 == OpCode.PHA) && (opCode0 == OpCode.PLY))
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteA | WalkStats.Exit | WalkStats.CallRet, WalkStats.ReadA, 20))
                    {
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.TAY;
                        modified = true;
                    }
                }
                if ((opCode1 == OpCode.LDY_z) && (opCode0 == OpCode.PLY))
                {
                    iCodes  [iIndex-1] = OpCode.NOP;
                    iLengths[iIndex-1] = 1;
                    modified = true;
                }
                if ((opCode1 == OpCode.LDX_z) && (opCode0 == OpCode.LDA_z) && (iOperands[iIndex-1] == iOperands[iIndex-0]))
                {
                    iCodes  [iIndex-0] = OpCode.TXA;
                    iLengths[iIndex-0] = 1;
                    modified = true;
                }
                if ((opCode1 == OpCode.LDY_z) && (opCode0 == OpCode.LDA_z) && (iOperands[iIndex-1] == iOperands[iIndex-0]))
                {
                    iCodes  [iIndex-0] = OpCode.TYA;
                    iLengths[iIndex-0] = 1;
                    modified = true;
                }
                if ((opCode1 == OpCode.LDA_z) && (opCode0 == OpCode.TAX))
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteA | WalkStats.Exit | WalkStats.CallRet, WalkStats.ReadA, 20))
                    {
                        iCodes  [iIndex-1] = OpCode.LDX_z;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
                if ((opCode1 == OpCode.LDA_z) && (opCode0 == OpCode.TAY))
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteA | WalkStats.Exit | WalkStats.CallRet, WalkStats.ReadA, 20))
                    {
                        iCodes  [iIndex-1] = OpCode.LDY_z;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
                if ((opCode1 == OpCode.LDA_z) && (opCode0 == OpCode.STA_z) && (iOperands[iIndex-1] == iOperands[iIndex-0]))
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteA | WalkStats.Exit | WalkStats.CallRet, WalkStats.ReadA, 20))
                    {
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
                if ((opCode1 == OpCode.STX_z) && (opCode0 == OpCode.LDA_z))
                {
                    if (iOperands[iIndex-1] == iOperands[iIndex-0])
                    {
                        iCodes  [iIndex-0] = OpCode.TXA;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
                if ((opCode1 == OpCode.TXA) && (opCode0 == OpCode.STA_z))
                {
                    
                    if (WalkAhead(iIndex+1, WalkStats.WriteA | WalkStats.Exit | WalkStats.CallRet, WalkStats.ReadA, 20))
                    {
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.STX_z;
                        modified = true;
                    }
                }
                
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeOver()
    {
        if (iCodes.Count < 5)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 4;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            if (!IsTargetOfJumps(iIndex-3) && !IsTargetOfJumps(iIndex-2) && !IsTargetOfJumps(iIndex-1) && !IsTargetOfJumps(iIndex))
            {
                OpCode opCode4 = iCodes[iIndex-4];
                OpCode opCode3 = iCodes[iIndex-3];
                OpCode opCode2 = iCodes[iIndex-2];
                OpCode opCode1 = iCodes[iIndex-1];
                OpCode opCode0 = iCodes[iIndex];
                
                // 5 instructions
                if ((opCode4 == OpCode.PHA) && (opCode3 == OpCode.LDA_n) && (opCode2 == OpCode.STA_z) && (opCode1 == OpCode.PLA) && (opCode0 == OpCode.STA_z))
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteA | WalkStats.Exit | WalkStats.CallRet, WalkStats.ReadA, 20))
                    {
                        iCodes   [iIndex-4] = OpCode.STA_z;
                        iOperands[iIndex-4] = iOperands[iIndex-0];
                        iLengths [iIndex-4] = 2;
                        
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeSextet()
    {
        if (iCodes.Count < 6)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 5;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            if (!IsTargetOfJumps(iIndex-4) && !IsTargetOfJumps(iIndex-3) && !IsTargetOfJumps(iIndex-2) && !IsTargetOfJumps(iIndex-1) && !IsTargetOfJumps(iIndex))
            {
                OpCode opCode5 = iCodes[iIndex-5];
                OpCode opCode4 = iCodes[iIndex-4];
                OpCode opCode3 = iCodes[iIndex-3];
                OpCode opCode2 = iCodes[iIndex-2];
                OpCode opCode1 = iCodes[iIndex-1];
                OpCode opCode0 = iCodes[iIndex];
                
                // 6 instructions
                if ((opCode5 == OpCode.PHA) && (opCode4 == OpCode.DEX) && (opCode3 == OpCode.LDA_nnX) && (opCode2 == OpCode.STA_z) && (opCode1 == OpCode.PLA) && (opCode0 == OpCode.STA_z))
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteA | WalkStats.Exit | WalkStats.CallRet, WalkStats.ReadA, 20))
                    {
                        iCodes   [iIndex-5] = OpCode.STA_z;
                        iOperands[iIndex-5] = iOperands[iIndex-0];
                        iLengths [iIndex-5] = 2;
                        
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
                if ((opCode5 == OpCode.PHA) && (opCode4 == OpCode.LDA_n) && (opCode3 == OpCode.STZ_z) && (opCode2 == OpCode.STA_z) && (opCode1 == OpCode.PLA) && (opCode0 == OpCode.STA_z))
                {
                    if ( (iOperands[iIndex-3] != iOperands[iIndex-0]) && (iOperands[iIndex-2] != iOperands[iIndex-0]) )
                    {
                        if (WalkAhead(iIndex+1, WalkStats.WriteA | WalkStats.Exit | WalkStats.CallRet, WalkStats.ReadA, 20))
                        {
                            iCodes   [iIndex-5] = OpCode.STA_z;
                            iOperands[iIndex-5] = iOperands[iIndex-0];
                            iLengths [iIndex-5] = 2;
                            
                            iCodes  [iIndex-1] = OpCode.NOP;
                            iLengths[iIndex-1] = 1;
                            iCodes  [iIndex-0] = OpCode.NOP;
                            iLengths[iIndex-0] = 1;
                            modified = true;
                        }
                    }
                }
                if ((opCode5 == OpCode.LDA_n) && (opCode4 == OpCode.PHA) && (opCode3 == OpCode.PHA) && (opCode2 == OpCode.PHA) && (opCode1 == OpCode.PHA) && (opCode0 == OpCode.LDA_n))
                {
                    if (iOperands[iIndex-5] == iOperands[iIndex-0])    
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
                if ((opCode5 == OpCode.LDA_z) && (opCode4 == OpCode.PHA) && (opCode3 == OpCode.LDA_z) && (opCode2 == OpCode.STA_z) && (opCode1 == OpCode.PLA) && (opCode0 == OpCode.STA_z))
                {
                    iCodes   [iIndex-5] = OpCode.NOP;
                    iLengths [iIndex-5] = 1;
                    iCodes   [iIndex-4] = OpCode.NOP;
                    iLengths [iIndex-4] = 1;
                    
                    iCodes   [iIndex-1] = OpCode.LDA_z;
                    iLengths [iIndex-1] = 2;
                    iOperands[iIndex-1] = iOperands[iIndex-5];
                    modified = true;
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    bool OptimizeOctet()
    {
        if (iCodes.Count < 8)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 7;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            if (!IsTargetOfJumps(iIndex-6) && !IsTargetOfJumps(iIndex-5) && !IsTargetOfJumps(iIndex-4) && !IsTargetOfJumps(iIndex-3) && !IsTargetOfJumps(iIndex-2) && !IsTargetOfJumps(iIndex-1) && !IsTargetOfJumps(iIndex))
            {
                OpCode opCode7 = iCodes[iIndex-7];
                OpCode opCode6 = iCodes[iIndex-6];
                OpCode opCode5 = iCodes[iIndex-5];
                OpCode opCode4 = iCodes[iIndex-4];
                OpCode opCode3 = iCodes[iIndex-3];
                OpCode opCode2 = iCodes[iIndex-2];
                OpCode opCode1 = iCodes[iIndex-1];
                OpCode opCode0 = iCodes[iIndex];
                
                // 8 instructions
                if ((opCode7 == OpCode.LDA_z) && (opCode6 == OpCode.PHA) && (opCode5 == OpCode.LDA_z) && (opCode4 == OpCode.SEC) && (opCode3 == OpCode.SBC_n) && (opCode2 == OpCode.TAX) && (opCode1 == OpCode.DEX) && (opCode0 == OpCode.PLA))
                {
                    if (iOperands[iIndex-7] != iOperands[iIndex-5])
                    {
                        iCodes   [iIndex-7] = OpCode.NOP;
                        iLengths [iIndex-7] = 1;
                        iCodes   [iIndex-6] = OpCode.NOP;
                        iLengths [iIndex-6] = 1;
                        iCodes   [iIndex-0] = OpCode.LDA_z;
                        iOperands[iIndex-0] = iOperands[iIndex-7];
                        iLengths [iIndex-0] = 2;
                        modified = true;
                    }
                }
                if ((opCode6 == OpCode.LDA_z) && (opCode5 == OpCode.PHA) && (opCode4 == OpCode.LDX_z) && (opCode3 == OpCode.DEX) && (opCode2 == OpCode.DEX) && (opCode1 == OpCode.DEX) && (opCode0 == OpCode.PLA))
                {
                    if (iOperands[iIndex-6] != iOperands[iIndex-4])
                    {
                        iCodes   [iIndex-6] = OpCode.NOP;
                        iLengths [iIndex-6] = 1;
                        iCodes   [iIndex-5] = OpCode.NOP;
                        iLengths [iIndex-5] = 1;
                        iCodes   [iIndex-0] = OpCode.LDA_z;
                        iOperands[iIndex-0] = iOperands[iIndex-6];
                        iLengths [iIndex-0] = 2;
                        modified = true;
                    }
                }
                if ((opCode6 == OpCode.PHA) && (opCode5 == OpCode.DEX) && (opCode4 == OpCode.LDA_n) && (opCode3 == OpCode.ADC_nnX) && (opCode2 == OpCode.STA_z) && (opCode1 == OpCode.PLA) && (opCode0 == OpCode.STA_z))
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteA | WalkStats.Exit | WalkStats.CallRet, WalkStats.ReadA, 20))
                    {
                        iCodes   [iIndex-6] = OpCode.STA_z;
                        iLengths [iIndex-6] = 2;
                        iOperands[iIndex-6] = iOperands[iIndex-0];
                        iCodes   [iIndex-1] = OpCode.NOP;
                        iLengths [iIndex-1] = 1;
                        iCodes   [iIndex-0] = OpCode.NOP;
                        iLengths [iIndex-0] = 1;
                        modified = true;
                    }
                }
                if ((opCode7 == OpCode.CLC) && (opCode6 == OpCode.LDA_n) && (opCode5 == OpCode.ADC_nnX) && (opCode4 == OpCode.STA_z) && (opCode3 == OpCode.DEX) && (opCode2 == OpCode.LDA_n) && (opCode1 == OpCode.ADC_nnX) && (opCode0 == OpCode.STA_z))
                {
                    if ((iOperands[iIndex-6] == 0) && (iOperands[iIndex-2] == 0))
                    {
                        iCodes   [iIndex-7] = OpCode.NOP;
                        iLengths [iIndex-7] = 1;
                        iCodes   [iIndex-6] = OpCode.NOP;
                        iLengths [iIndex-6] = 1;
                        iCodes   [iIndex-5] = OpCode.LDA_nnX;
                        iCodes   [iIndex-2] = OpCode.NOP;
                        iLengths [iIndex-2] = 1;
                        iCodes   [iIndex-1] = OpCode.LDA_nnX;
                        modified = true;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    bool OptimizeTen()
    {
        if (iCodes.Count < 10)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 9;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            if (!IsTargetOfJumps(iIndex-8) && !IsTargetOfJumps(iIndex-7) && !IsTargetOfJumps(iIndex-6) && !IsTargetOfJumps(iIndex-5) && !IsTargetOfJumps(iIndex-4) && !IsTargetOfJumps(iIndex-3) && !IsTargetOfJumps(iIndex-2) && !IsTargetOfJumps(iIndex-1) && !IsTargetOfJumps(iIndex))
            {
                OpCode opCode9 = iCodes[iIndex-9];
                OpCode opCode8 = iCodes[iIndex-8];
                OpCode opCode7 = iCodes[iIndex-7];
                OpCode opCode6 = iCodes[iIndex-6];
                OpCode opCode5 = iCodes[iIndex-5];
                OpCode opCode4 = iCodes[iIndex-4];
                OpCode opCode3 = iCodes[iIndex-3];
                OpCode opCode2 = iCodes[iIndex-2];
                OpCode opCode1 = iCodes[iIndex-1];
                OpCode opCode0 = iCodes[iIndex];
                
                // 10 instructions
                if ((opCode9 == OpCode.LDA_z) && (opCode8 == OpCode.PHA) && (opCode7 == OpCode.LDX_z) && (opCode6 == OpCode.DEX) && (opCode5 == OpCode.DEX) && 
                    (opCode4 == OpCode.DEX) && (opCode3 == OpCode.LDA_z) && (opCode2 == OpCode.STA_nnX) && (opCode1 == OpCode.INX) && (opCode0 == OpCode.PLA))
                {
                    if ((iOperands[iIndex-9] != iOperands[iIndex-7]) && (iOperands[iIndex-9] != iOperands[iIndex-3]))
                    {
                        iCodes   [iIndex-9] = OpCode.NOP;
                        iLengths [iIndex-9] = 1;
                        iCodes   [iIndex-8] = OpCode.NOP;
                        iLengths [iIndex-8] = 1;
                        iCodes   [iIndex-0] = OpCode.LDA_z;
                        iOperands[iIndex-0] = iOperands[iIndex-9];
                        iLengths [iIndex-0] = 2;
                        modified = true;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    bool OptimizeQuad()
    {
        if (iCodes.Count < 4)
        {
            return false;
        }
        bool modified = false;
        uint iIndex = 3;
        loop
        {
            if (iIndex >= iCodes.Count)
            {
                break;
            }
            if (!IsTargetOfJumps(iIndex-2) && !IsTargetOfJumps(iIndex-1) && !IsTargetOfJumps(iIndex))
            {
                OpCode opCode3 = iCodes[iIndex-3];
                OpCode opCode2 = iCodes[iIndex-2];
                OpCode opCode1 = iCodes[iIndex-1];
                OpCode opCode0 = iCodes[iIndex];
                
                // 4 instructions
                if ((opCode3 == OpCode.LDA_z) && (opCode2 == OpCode.SEC) && (opCode1 == OpCode.SBC_n) && (opCode0 == OpCode.TAX)
                                                                         && ((iOperands[iIndex-1] == 0x02) || (iOperands[iIndex-1] == 0x01))
                   )
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteA | WalkStats.Exit | WalkStats.CallRet, WalkStats.ReadA, 20))
                    {
                        iCodes  [iIndex-3] = OpCode.LDX_z;
                        iCodes  [iIndex-2] = OpCode.DEX;
                        iLengths[iIndex-2] = 1;
                        iCodes  [iIndex-1] = (iOperands[iIndex-1] == 0x02) ? OpCode.DEX : OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
                if ((opCode3 == OpCode.LDA_z) && (opCode2 == OpCode.SEC) && (opCode1 == OpCode.SBC_n) && (opCode0 == OpCode.TAY)
                                                                         && ((iOperands[iIndex-1] == 0x02) || (iOperands[iIndex-1] == 0x01))
                   )
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteA | WalkStats.Exit | WalkStats.CallRet, WalkStats.ReadA, 20))
                    {
                        iCodes  [iIndex-3] = OpCode.LDY_z;
                        iCodes  [iIndex-2] = OpCode.DEY;
                        iLengths[iIndex-2] = 1;
                        iCodes  [iIndex-1] = (iOperands[iIndex-1] == 0x02) ? OpCode.DEY : OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
                if ((opCode3 == OpCode.LDA_n) && (opCode2 == OpCode.PHA) && (opCode1 == OpCode.PHA) && (opCode0 == OpCode.LDA_n))
                {
                    if (iOperands[iIndex-3] == iOperands[iIndex-0])    
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    bool OptimizeTrip65()
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
            if (!IsTargetOfJumps(iIndex-1) && !IsTargetOfJumps(iIndex))
            {
                OpCode opCode2 = iCodes[iIndex-2];
                OpCode opCode1 = iCodes[iIndex-1];
                OpCode opCode0 = iCodes[iIndex];
                
                // 3 instructions
                if ((opCode2 == OpCode.PHA) && (opCode1 == OpCode.STZ_z) && (opCode0 == OpCode.PLA))
                {
                    iCodes  [iIndex-2] = OpCode.NOP;
                    iLengths[iIndex-2] = 1;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    iLengths[iIndex-0] = 1;
                    modified = true;
                }
                if ((opCode2 == OpCode.LDA_z) && (opCode0 == OpCode.LDA_z))
                {
                    if (iOperands[iIndex-2] == iOperands[iIndex-0])
                    {
                        string name = GetName(opCode1);
                        switch (name)
                        {
                            case "STA":
                            {
                                iCodes  [iIndex-0] = OpCode.NOP;
                                iLengths[iIndex-0] = 1;
                                modified = true;
                            }
                            case "JSR":
                            case "BBS5": // this would actually work (but needs to be tested)
                            {
                                // nope
                            }
                            default:
                            {    
                                Print(" Optimize?" + name + ":" + (iOperands[iIndex-2]).ToHexString(2) + "," + (iOperands[iIndex-0]).ToHexString(2));
                            }
                        }
                    }
                }
            } 
            iIndex++;
        }
        return modified;   
    }
    
    bool OptimizeTrip()
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
            if (!IsTargetOfJumps(iIndex-1) && !IsTargetOfJumps(iIndex))
            {
                OpCode opCode2 = iCodes[iIndex-2];
                OpCode opCode1 = iCodes[iIndex-1];
                OpCode opCode0 = iCodes[iIndex];
                
                // 3 instructions
                if ((opCode2 == OpCode.PHA) && (opCode1 == OpCode.STZ_z) && (opCode0 == OpCode.PLA))
                {
                    iCodes  [iIndex-2] = OpCode.NOP;
                    iLengths[iIndex-2] = 1;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    iLengths[iIndex-0] = 1;
                    modified = true;
                }
                if ((opCode2 == OpCode.STA_z) && (opCode1 == OpCode.CLC) && (opCode0 == OpCode.LDA_z))
                {
                    if (iOperands[iIndex-2] == iOperands[iIndex-0])
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
                if ((opCode2 == OpCode.SBC_n) && (opCode1 == OpCode.TAX) && (opCode0 == OpCode.DEX))
                {
                    if (iOperands[iIndex-2] < 255)
                    {
                        iOperands[iIndex-2] = iOperands[iIndex-2] + 1;
                        iCodes   [iIndex-0] = OpCode.NOP;
                        iLengths [iIndex-0] = 1;
                        modified = true;
                    }
                }
                if ((opCode2 == OpCode.SBC_n) && (opCode1 == OpCode.TAY) && (opCode0 == OpCode.DEY))
                {
                    if (iOperands[iIndex-2] < 255)
                    {
                        iOperands[iIndex-2] = iOperands[iIndex-2] + 1;
                        iCodes   [iIndex-0] = OpCode.NOP;
                        iLengths [iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            iIndex++;
        } // loop
        return modified;
    }
    
    // 2. CMP #0 after LDA, LDX, LDY, INC, INX, INY, DEC, DEX, DEY, INA, DEA, AND, ORA, EOR, ASL, LSR, ROL, 
    //              ROR, PLA, PLX, PLY, SBC, ADC, TAX, TXA, TAY, TYA, and TSX
    // is redundant if checking Z or V. They all set the Z and V flags. C is a different story.
    bool OptimizeCMP()
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
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex];
            if (!IsTargetOfJumps(iIndex) )
            {
                if (!IsTargetOfJumps(iIndex-1) )
                {
                    if  ((opCode0 == OpCode.BEQ_e) || (opCode0 == OpCode.BNE_e)|| (opCode0 == OpCode.BMI_e) || (opCode0 == OpCode.BPL_e))
                    {
                        string name = GetName(opCode2);
                        if ((opCode1 == OpCode.CMP_n) && (iOperands[iIndex-1] == 0))
                        {
                            switch (name)
                            {
                                case "LDA":
                                case "PLA":
                                case "TXA":
                                case "TYA":
                                case "INC":
                                case "DEC":
                                case "AND":
                                case "ORA":
                                case "EOR":
                                {
                                    iCodes   [iIndex-1] = OpCode.NOP;
                                    iLengths [iIndex-1] = 1;
                                    modified = true;
                                }
                            }
                        }
                        else if ((opCode1 == OpCode.CPX_n) && (iOperands[iIndex-1] == 0))
                        {
                            switch (name)
                            {
                                case "LDX":
                                case "PLX":
                                case "TAX":
                                case "TSX":
                                case "INX":
                                case "DEX":
                                {
                                    iCodes   [iIndex-1] = OpCode.NOP;
                                    iLengths [iIndex-1] = 1;
                                    modified = true;
                                }
                            }
                        }
                        else if ((opCode1 == OpCode.CPY_n) && (iOperands[iIndex-1] == 0))
                        {
                            switch (name)
                            {
                                case "LDY":
                                case "PLY":
                                case "TAY":
                                case "INY":
                                case "DEY":
                                {
                                    iCodes   [iIndex-1] = OpCode.NOP;
                                    iLengths [iIndex-1] = 1;
                                    modified = true;
                                }
                            }
                        }
                    }
                }       
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeCLCBCS()
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
            if (!IsTargetOfJumps(iIndex) )
            {
                /*
                if  ((opCode0 == OpCode.BCS_e) && (opCode1 == OpCode.CLC))
                {
                    // CLC
                    // BCS
                    
                    // tempting ..
                    //iCodes   [iIndex-1] = OpCode.NOP;
                    //iLengths [iIndex-1] = 1;
                    
                    iCodes   [iIndex-0] = OpCode.NOP;
                    iLengths [iIndex-0] = 1;
                    modified = true;
                }
                if  ((opCode0 == OpCode.BCC_e) && (opCode1 == OpCode.CLC))
                {
                    // CLC
                    // BCC
                    
                    // tempting ..
                    //iCodes   [iIndex-1] = OpCode.NOP;
                    //iLengths [iIndex-1] = 1;
                    
                    iCodes   [iIndex-0] = OpCode.BRA_e;
                    iLengths [iIndex-0] = 1;
                    modified = true;
                    Print(" B");
                }
                if  ((opCode0 == OpCode.BCC_e) && (opCode1 == OpCode.SEC))
                {
                    // SEC
                    // BCC
                    
                    // tempting ..
                    //iCodes   [iIndex-1] = OpCode.NOP;
                    //iLengths [iIndex-1] = 1;
                    
                    iCodes   [iIndex-0] = OpCode.NOP;
                    iLengths [iIndex-0] = 1;
                    modified = true;
                    Print(" C");
                }
                if  ((opCode0 == OpCode.BCS_e) && (opCode1 == OpCode.SEC))
                {
                    // SEC
                    // BCS
                    
                    // tempting ..
                    //iCodes   [iIndex-1] = OpCode.NOP;
                    //iLengths [iIndex-1] = 1;
                    
                    iCodes   [iIndex-0] = OpCode.BRA_e;
                    iLengths [iIndex-0] = 1;
                    modified = true;
                    Print(" D");
                }
                */
            }
            iIndex++;
        } // loop
        return modified;
    }
    bool OptimizeHunt()
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
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex];
            if (!IsTargetOfJumps(iIndex) )
            {
                if (!IsTargetOfJumps(iIndex-1))
                {
                    if ((opCode2 == OpCode.PHA) && (opCode0 == OpCode.PLA))
                    {
                        string name = GetName(opCode1);
                        Print(" " + name);
                    }
                }
            }
           iIndex++;
        } // loop
        return modified;
    }
}

