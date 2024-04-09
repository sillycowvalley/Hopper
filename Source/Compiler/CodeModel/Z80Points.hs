unit CodePoints
{
    uint currentMethod;
    string currentReturnType;
    uint currentPass;
    
    // Z80Code interface to get and set method bits:
    //
    //   Note:
    //     JP_x_nn operands are relative
    //     CALL_nn operands are methodIndex | 0x8000 (to distinguish them from system CALL's which should be ignored)
    //     DDDD operand is always followed my LD_DE_nn that loads a delegate methodIndex which should not be 'optimized'
    //
    // <uint,string> Z80Code.GetMethodNames() // <index, name>
    // <byte> Z80Code.GetMethodCode(uint methodIndex)
    // Z80Code.SetMethodCode(uint methodIndex, <byte> code)
    // <uint,uint> GetMethodDebugLines(uint methodIndex)                // <address,line>
    // SetMethodDebugLines(uint methodIndex, <uint,uint> debugLines)    // <address,line>
    
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
        ReadE     = 0b0000000000000010,
        ReadD     = 0b0000000000000100,
        ReadDE    = 0b0000000000000110,
        WriteE    = 0b0000000000001000,
        WriteD    = 0b0000000000010000,
        WriteDE   = 0b0000000000011000,
        ReadL     = 0b0000000000100000,
        ReadH     = 0b0000000001000000,
        ReadHL    = 0b0000000001100000,
        WriteL    = 0b0000000010000000,
        WriteH    = 0b0000000100000000,
        WriteHL   = 0b0000000110000000,
        ReadC     = 0b0000001000000000,
        ReadB     = 0b0000010000000000,
        ReadBC    = 0b0000011000000000,
        WriteC    = 0b0000100000000000,
        WriteB    = 0b0001000000000000,
        WriteBC   = 0b0001100000000000,
    }
    
    // CodePoints:
    //
    <OpCode> iCodes;        // Z80 instruction opCodes
    <uint>   iOperands;     // Z80 instruction operands
    <uint>   iLengths;      // Z80 instruction lengths (not operand width)
    <uint>   iDebugLines;   // Hopper source line associated with this instruction (mostly 0)
    <Flags>  iFlags;        // reachable? jump target?
    
    <uint,uint> indexToAddress; // <iIndex,address>
    <uint,uint> addressToIndex; // <address,iIndex> 
    
                     
    uint total;
    bool firstInMethod; 
    
    <uint, <OpCode> > sysCallStreams;
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
        <OpCode> sysCallStream;
        
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
            
            OpCode opCode0;
            bool skipIncrement;
            if (sysCallStream.Count != 0)
            {
                opCode0 = sysCallStream[0];
                sysCallStream.Remove(0);
                skipIncrement = true;
            }
            else
            {
                opCode0 = iCodes[iIndex];
            }
            if (walkVerbose)
            {
                if(skipIncrement)
                {
                    Print("  [" + GetName(opCode0) +"]");
                }
                else
                {
                    Print("  " + GetName(opCode0));
                }
            }
            switch (opCode0)
            {
                // Exit
                case OpCode.RET:
                case OpCode.HALT:
                {
                    walkStats |= WalkStats.Exit;
                }
                case OpCode.CALL_nn:
                {
                    if (iOperands[iIndex] & 0x8000 != 0)
                    {
                        // user methods
                        walkStats |= WalkStats.Exit;
                    }
                    else 
                    {
                        // system methods
                        uint callAddress = iOperands[iIndex];
                        if (walkVerbose) { Print(" (sys:"+ callAddress.ToHexString(4) +")"); }
                        if (sysCallStreams.Contains(callAddress))
                        {
                            sysCallStream = sysCallStreams[callAddress];
                        }
                        else 
                        {
                            sysCallStream = Z80Code.GetCallStream(callAddress);
                            sysCallStreams[callAddress] = sysCallStream;
                        }
                    }
                }
                
                // Write:
                case OpCode.LD_DE_nn:
                case OpCode.LD_DE_inn:
                case OpCode.POP_DE:
                {
                    walkStats |= WalkStats.WriteDE;
                }
                case OpCode.LD_D_iIY_d:
                case OpCode.LD_D_iIX_d:
                case OpCode.LD_D_n:
                {
                    walkStats |= WalkStats.WriteD;
                }
                case OpCode.LD_E_iIY_d:
                case OpCode.LD_E_iIX_d:
                case OpCode.LD_E_n:
                case OpCode.LD_E_A:
                {
                    walkStats |= WalkStats.WriteE;
                }
                case OpCode.LD_L_iIY_d:
                case OpCode.LD_L_iIX_d:
                case OpCode.LD_L_n:
                case OpCode.LD_L_A:
                {
                    walkStats |= WalkStats.WriteL;
                }
                case OpCode.LD_H_iIY_d:
                case OpCode.LD_H_iIX_d:
                case OpCode.LD_H_n:
                case OpCode.LD_H_A:
                {
                    walkStats |= WalkStats.WriteH;
                }
                case OpCode.LD_HL_nn:
                case OpCode.LD_HL_inn:
                case OpCode.POP_HL:
                {
                    walkStats |= WalkStats.WriteHL;
                }
                
                case OpCode.LD_C_iIY_d:
                case OpCode.LD_C_n:
                {
                    walkStats |= WalkStats.WriteC;
                }
                case OpCode.LD_B_iIY_d:
                case OpCode.LD_B_iIX_d:
                case OpCode.LD_B_n:
                case OpCode.DJNZ_e:
                {
                    walkStats |= WalkStats.WriteB;
                }
                case OpCode.LD_BC_nn:
                case OpCode.LD_BC_inn:
                case OpCode.POP_BC:
                {
                    walkStats |= WalkStats.WriteBC;
                }
                
                // Read and Write:
                case OpCode.SRL_H:
                case OpCode.RL_H:
                {
                    walkStats |= WalkStats.WriteH;
                    walkStats |= WalkStats.ReadH;
                }
                case OpCode.SLA_L:
                case OpCode.RR_L:
                {
                    walkStats |= WalkStats.WriteL;
                    walkStats |= WalkStats.ReadL;
                }
                case OpCode.LD_L_D:
                {
                    walkStats |= WalkStats.WriteL;
                    walkStats |= WalkStats.ReadD;
                }
                case OpCode.LD_L_E:
                {
                    walkStats |= WalkStats.WriteL;
                    walkStats |= WalkStats.ReadE;
                }
                case OpCode.EX_iSP_HL:
                {
                    walkStats |= WalkStats.ReadHL;
                    walkStats |= WalkStats.WriteHL;
                }
                case OpCode.LD_H_L:
                {
                    walkStats |= WalkStats.ReadL;
                    walkStats |= WalkStats.WriteH;
                }
                case OpCode.LD_L_H:
                {
                    walkStats |= WalkStats.ReadH;
                    walkStats |= WalkStats.WriteL;
                }
                case OpCode.LD_E_L:
                {
                    walkStats |= WalkStats.ReadL;
                    walkStats |= WalkStats.WriteE;
                }
                case OpCode.LD_E_D:
                {
                    walkStats |= WalkStats.ReadD;
                    walkStats |= WalkStats.WriteE;
                }
                case OpCode.LD_D_H:
                {
                    walkStats |= WalkStats.ReadH;
                    walkStats |= WalkStats.WriteD;
                }
                case OpCode.LD_B_C:
                {
                    walkStats |= WalkStats.ReadC;
                    walkStats |= WalkStats.WriteB;
                }
                case OpCode.ADD_HL_DE:
                case OpCode.ADC_HL_DE:
                case OpCode.SBC_HL_DE:
                {
                    walkStats |= WalkStats.WriteHL;
                    walkStats |= WalkStats.ReadHL;
                    walkStats |= WalkStats.ReadDE;
                }
                case OpCode.ADD_HL_BC:
                case OpCode.ADC_HL_BC:
                case OpCode.SBC_HL_BC:
                {
                    walkStats |= WalkStats.WriteHL;
                    walkStats |= WalkStats.ReadHL;
                    walkStats |= WalkStats.ReadBC;
                }
                
                case OpCode.LD_L_iHL:
                {
                    walkStats |= WalkStats.ReadHL;
                    walkStats |= WalkStats.WriteL;
                }
                case OpCode.LD_H_iHL:
                {
                    walkStats |= WalkStats.ReadHL;
                    walkStats |= WalkStats.WriteH;
                }
                
                case OpCode.ADD_HL_HL:
                case OpCode.ADC_HL_HL:
                case OpCode.SBC_HL_HL: 
                case OpCode.INC_HL: 
                case OpCode.DEC_HL: 
                {
                    walkStats |= WalkStats.WriteHL;
                    walkStats |= WalkStats.ReadHL;
                }
                case OpCode.LD_H_E:
                {
                    walkStats |= WalkStats.WriteH;
                    walkStats |= WalkStats.ReadE;
                }
                
                
                // Read:
                case OpCode.LD_iIX_d_E:
                case OpCode.LD_iIY_d_E:
                case OpCode.CP_A_E:
                case OpCode.LD_A_E:
                {
                    walkStats |= WalkStats.ReadE;
                }
                case OpCode.LD_iIY_d_D:
                case OpCode.LD_iIX_d_D:
                {
                    walkStats |= WalkStats.ReadD;
                }
                case OpCode.PUSH_DE:
                case OpCode.LD_inn_DE:
                case OpCode.ADD_IX_DE:
                {
                    walkStats |= WalkStats.ReadDE;
                }
                case OpCode.LD_iIY_d_L:
                case OpCode.LD_iIX_d_L:
                case OpCode.CP_A_L:
                case OpCode.LD_A_L:
                {
                    walkStats |= WalkStats.ReadL;
                }
                case OpCode.LD_iIY_d_H:
                case OpCode.LD_iIX_d_H:
                case OpCode.LD_A_H:
                case OpCode.CP_A_H:
                {
                    walkStats |= WalkStats.ReadH;
                }
                case OpCode.PUSH_HL:
                case OpCode.LD_inn_HL:
                {
                    walkStats |= WalkStats.ReadHL;
                }
                case OpCode.LD_iIY_d_C:
                case OpCode.LD_iIX_d_C:
                case OpCode.AND_A_C:
                case OpCode.OR_A_C:
                case OpCode.XOR_A_C:
                case OpCode.LD_A_C:
                {
                    walkStats |= WalkStats.ReadC;
                }
                case OpCode.LD_iIY_d_B:
                case OpCode.LD_iIX_d_B:
                case OpCode.AND_A_B:
                case OpCode.OR_A_B:
                case OpCode.XOR_A_B:
                case OpCode.LD_A_B:
                case OpCode.CP_A_B:
                {
                    walkStats |= WalkStats.ReadB;
                }
                case OpCode.PUSH_BC:
                case OpCode.LD_inn_BC:
                {
                    walkStats |= WalkStats.ReadBC;
                }
                
                                
                                
                // Ignore:
                case OpCode.OUT_n_A:
                case OpCode.NOP:
                case OpCode.XOR_A:
                case OpCode.CPL:
                case OpCode.RLA:
                case OpCode.AND_A:
                case OpCode.SUB_A_n:
                case OpCode.SUB_A_iIY_d:
                case OpCode.AND_A_n:
                case OpCode.OR_A_n:
                case OpCode.LD_A_n:
                case OpCode.CP_A_iIX_d:
                case OpCode.CP_A_iIY_d:
                case OpCode.LD_IX_nn:
                case OpCode.LD_IY_nn:
                case OpCode.LD_iIX_d_n:
                case OpCode.LD_iIY_d_n:
                case OpCode.DEC_iHL:
                case OpCode.INC_iHL:
                case OpCode.INC_iIX_d:
                case OpCode.INC_iIY_d:
                case OpCode.DEC_iIX_d:
                case OpCode.DEC_iIY_d:
                case OpCode.POP_IX:
                case OpCode.POP_IY:
                case OpCode.PUSH_IX:
                case OpCode.PUSH_IY:
                case OpCode.EX_iSP_IX:
                case OpCode.EX_iSP_IY:
                case OpCode.DDDD:
                case OpCode.LD_SP_nn:
                case OpCode.LD_inn_SP:
                case OpCode.LD_inn_A:
                case OpCode.LD_IY_inn:
                case OpCode.LD_SP_IY:
                {
                }
                
                // Jump!
                case OpCode.JP_nn:
                {
                    iIndex = iOperands[iIndex];
                    skipIncrement = true;
                }
                
                // Branch:
                case OpCode.JP_Z_nn:
                case OpCode.JP_NZ_nn:
                {
                    bool success0 = WalkAhead(iIndex+1,          searchFor, searchAgainst, pointLimit, indent + "  ");
                    bool success1 = WalkAhead(iOperands[iIndex], searchFor, searchAgainst, pointLimit, indent + "  ");
                    success = success0 && success1;
                    return success; 
                }
                
                
                // Abandon:
                default:
                {
                    string name = AsmZ80.GetName(opCode0);
                    PrintLn();
                    Print(name + " not supported in WalkAhead");
                    walkStats = WalkStats.None;
                    break;
                }
            }
            if (WalkStats.None != (walkStats & searchAgainst))
            {
                success = false; // any negative hit is a failure
                if (walkVerbose)
                {
                    Print("   -> " + currentMethod.ToHexString(4) + ":" + iIndex.ToString() + " -ve " + ((WalkStats.Exit == (walkStats & searchAgainst)) ? "Exit" : "Registers"));
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
                bool hit = true;
                // no partials for these:
                if (WalkStats.ReadDE == (WalkStats.ReadDE & searchFor))
                {
                    hit = WalkStats.ReadDE == (WalkStats.ReadDE & walkStats);
                }
                if (WalkStats.ReadHL == (WalkStats.ReadHL & searchFor))
                {
                    hit = WalkStats.ReadHL == (WalkStats.ReadHL & walkStats);
                }
                if (WalkStats.ReadBC == (WalkStats.ReadBC & searchFor))
                {
                    hit = WalkStats.ReadBC == (WalkStats.ReadBC & walkStats);
                }
                if (WalkStats.WriteDE == (WalkStats.WriteDE & searchFor))
                {
                    hit = WalkStats.WriteDE == (WalkStats.WriteDE & walkStats);
                }
                if (WalkStats.WriteHL == (WalkStats.WriteHL & searchFor))
                {
                    hit = WalkStats.WriteHL == (WalkStats.WriteHL & walkStats);
                }
                if (WalkStats.WriteBC == (WalkStats.WriteBC & searchFor))
                {
                    hit = WalkStats.WriteBC == (WalkStats.WriteBC & walkStats);
                }
                if (hit)
                {
                    if (walkVerbose)
                    {
                        Print("   -> " + currentMethod.ToHexString(4) + ":" + iIndex.ToString() + " +ve Registers");
                    }
                    success = true;
                    break;
                }
            }
            if (!skipIncrement)
            {
                iIndex++;
            }
            pointLimit--;
        }
        return success;
    }
    
    incTotal(byte gain, uint iIndex)
    {
        if (firstInMethod)
        {
            PrintLn();
            Print(currentMethod.ToHexString(4) + "("+ (iCodes.Count).ToString()+"):");
            firstInMethod = false;
        }
        total += gain;
        Print(" " + total.ToString() + "[" + iIndex.ToString() + "]");
    }
    
       CollectMethodCalls(<uint,bool> methodsCalled)
    {
        bool expectDelegate;
        for (uint iIndex = 0; iIndex < iCodes.Count; iIndex++)
        {
            OpCode opCode = iCodes[iIndex];
            uint methodIndex;
            if (expectDelegate)
            {
                if (opCode != OpCode.LD_DE_nn) { Die(0x0B); }
                methodIndex = iOperands[iIndex];
                expectDelegate = false;  
            }
            else if (opCode == OpCode.CALL_nn)
            {
                methodIndex = iOperands[iIndex];
                if (methodIndex & 0x8000 != 0)
                {
                    methodIndex = methodIndex & 0x7FFF;
                }
                else
                {
                    methodIndex = 0; // ignore system methods
                }
            }
            else if (opCode == OpCode.DDDD)
            {
                expectDelegate = true;
            }
            
            if (methodIndex != 0)
            {
                if (!methodsCalled.Contains(methodIndex)) { Die(0x0B); } // where did this method come from? why was it not already known?
                methodsCalled[methodIndex] = true; 
            }            
        }
    }
    uint Load(uint methodIndex, uint pass)
    {
        iCodes.Clear();
        iOperands.Clear();
        iLengths.Clear();
        iDebugLines.Clear();
        iFlags.Clear();
        
        indexToAddress.Clear();
        addressToIndex.Clear();
        
        //PrintLn();
        //Print("Load " + methodIndex.ToHexString(4) + ": ");
        
        currentMethod = methodIndex;
        currentReturnType = Z80Code.GetMethodReturnType(methodIndex);
        currentPass   = pass;
        
        <byte> method = Z80Code.GetMethodCode(methodIndex);
        <uint,uint> debugLines = Z80Code.GetMethodDebugLines(methodIndex);
        
        uint sizeBefore = method.Count;
        uint index = 0;
        loop
        {
            if (index == sizeBefore) { break; }
            uint instructionAddress = index;
            
            uint iIndex = iCodes.Count;
            addressToIndex[instructionAddress] = iIndex;
            indexToAddress[iIndex] = instructionAddress;
            
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
            iCodes.Append(instruction);
            iOperands.Append(operand);
            iLengths.Append(opCodeLength + operandLength);
            iFlags.Append(Flags.Unreached);
            uint debugLine = 0;
            if (debugLines.Contains(instructionAddress))
            {
                debugLine = debugLines[instructionAddress];
            }
            iDebugLines.Append(debugLine);
        } // loop
        
        uint iIndex = 0;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode instruction = iCodes[iIndex];
            uint   operand     = iOperands[iIndex];
            switch (instruction)
            {
                case OpCode.JP_nn:
                case OpCode.JP_Z_nn:
                case OpCode.JP_NZ_nn:
                {
                    uint myAddress = indexToAddress[iIndex];
                    int ioperand = Int.FromBytes(operand.GetByte(0), operand.GetByte(1));
                    uint myLength  = iLengths[iIndex];
                    uint targetAddress = uint(int(myAddress + myLength) + ioperand);
                    if (!addressToIndex.Contains(targetAddress))
                    {
                        PrintLn("0x" + currentMethod.ToHexString(4) + " " + iIndex.ToString() + " 0x" + myAddress.ToHexString(4));
                    }
                    uint iTarget = addressToIndex[targetAddress]; // <address,iIndex>
                    iOperands[iIndex] = iTarget; 
                }
                
                case OpCode.JR_e:
                case OpCode.JR_Z_e:
                case OpCode.JR_NZ_e:
                {
                    uint myAddress = indexToAddress[iIndex];
                    int ioperand = int(operand);
                    if (ioperand > 127)
                    {
                        ioperand -= 256; // 0xFF -> -1    
                    }
                    uint myLength  = iLengths[iIndex];
                    uint targetAddress = uint(int(myAddress + myLength) + ioperand);
                    uint iTarget = addressToIndex[targetAddress]; // <address,iIndex>
                    iOperands[iIndex] = iTarget; 
                    iLengths [iIndex] = 3;
                    switch (instruction)
                    {
                        case OpCode.JR_e:     { iCodes[iIndex] = OpCode.JP_nn;    }
                        case OpCode.JR_Z_e:   { iCodes[iIndex] = OpCode.JP_Z_nn;  }
                        case OpCode.JR_NZ_e:  { iCodes[iIndex] = OpCode.JP_NZ_nn; }
                    }
                }
                case OpCode.JP_C_nn:
                case OpCode.JP_NC_nn:
                case OpCode.JR_C_e:
                case OpCode.JR_NC_e:
                {
                    Die(0x0B); // never used, yet
                }
                case OpCode.DJNZ_e:
                {
                    uint myAddress = indexToAddress[iIndex];
                    int ioperand = int(operand);
                    if (ioperand > 127)
                    {
                        ioperand -= 256; // 0xFF -> -1    
                    }
                    uint myLength  = iLengths[iIndex];
                    uint targetAddress = uint(int(myAddress + myLength) + ioperand);
                    uint iTarget = addressToIndex[targetAddress]; // <address,iIndex>
                    iOperands[iIndex] = iTarget;  
                }
            }
            iIndex++;
        }
        return sizeBefore; // size in bytes before
    }
    rebuildAddressIndex()
    {
        indexToAddress.Clear();
        addressToIndex.Clear();
        
        uint iIndex = 0;
        uint address = 0;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode instruction = iCodes[iIndex];
            uint   length      = iLengths[iIndex];
            
            addressToIndex[address] = iIndex;
            indexToAddress[iIndex] = address;
            
            address += length;
            iIndex++;
        }
    }
    bool considerShortJumps()
    {
        bool modified;
        uint iIndex = 0;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode0   = iCodes[iIndex];
            
            if ((opCode0 == OpCode.JP_nn) || (opCode0 == OpCode.JP_Z_nn) || (opCode0 == OpCode.JP_NZ_nn))
            {
                uint operand0  = iOperands[iIndex];
                uint instructionAddress = indexToAddress[iIndex];
                uint length0   = iLengths[iIndex];
                
                uint iTarget       = operand0;
                uint targetAddress = indexToAddress[iTarget];
                long lOffset       = long(targetAddress) - instructionAddress - length0;
                int  iOffset       = int(lOffset);
                if ((iOffset > -126) && (iOffset < 125)) // 3 bytes of wiggle room in each direction for now
                {
                    iLengths[iIndex]  = 2;
                    if (opCode0 == OpCode.JP_nn)
                    {
                        iCodes[iIndex] = OpCode.JR_e;
                    }
                    else if (opCode0 == OpCode.JP_Z_nn)
                    {
                        iCodes[iIndex] = OpCode.JR_Z_e;
                    }
                    else if (opCode0 == OpCode.JP_NZ_nn)
                    {
                        iCodes[iIndex] = OpCode.JR_NZ_e;
                    }
                    modified = true;
                }
            }
            iIndex++;
        }
        return modified;   
    }
    uint Save()
    {
        //PrintLn();
        //Print("Save " + currentMethod.ToHexString(4) + ": ");
        
        rebuildAddressIndex();
        while (considerShortJumps())
        {
            rebuildAddressIndex();
        }
        
        <byte> method;
        <uint,uint> debugLines; 
        
        uint iIndex = 0;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode instruction   = iCodes[iIndex];
            uint   operand       = iOperands[iIndex];
            uint   length        = iLengths[iIndex];
            uint   debugLine     = iDebugLines[iIndex];
            byte   opCodeLength  = GetOpCodeLength(instruction);
            byte   operandLength = byte(length - opCodeLength);
            
            uint instructionAddress = indexToAddress[iIndex];
            if (instructionAddress != method.Count) { Die(0x0B); }
            
            switch (instruction)
            {
                case OpCode.JP_nn:
                case OpCode.JP_Z_nn:
                case OpCode.JP_NZ_nn:
                {
                    uint iTarget       = operand;
                    uint targetAddress = indexToAddress[iTarget];
                    long lOffset       = long(targetAddress) - instructionAddress - length;
                    int  iOffset       = int(lOffset);
                    operand            = UInt.FromBytes(iOffset.GetByte(0), iOffset.GetByte(1));
                    iOperands[iIndex]  = operand;
                }
                
                case OpCode.JR_e:
                case OpCode.JR_Z_e:
                case OpCode.JR_NZ_e:
                case OpCode.DJNZ_e:
                {
                    uint iTarget       = operand;
                    uint targetAddress = indexToAddress[iTarget];
                    long lOffset       = long(targetAddress) - instructionAddress - length;
                    int  iOffset       = int(lOffset);
                    operand            = iOffset.GetByte(0);
                    iOperands[iIndex]  = operand;
                }
            }
            if (opCodeLength > 1)
            {
                method.Append(byte(byte(instruction) >> 8));
            }
            method.Append(byte(byte(instruction) & 0xFF));
            if (operandLength > 0)
            {
                method.Append(byte(operand & 0xFF));
            }
            if (operandLength > 1)
            {
                method.Append(byte(operand >> 8));
            }
            
            if (debugLine != 0)
            {
                debugLines[instructionAddress] = debugLine;
            }
            iIndex++;
        }
        
        Z80Code.SetMethodCode(currentMethod, method);
        Z80Code.SetMethodDebugLines(currentMethod, debugLines);
        
        return method.Count; // size in bytes after
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
            OpCode opCode0 = iCodes[iIndex];
            switch (opCode0)
            {
                case OpCode.RET:
                case OpCode.HALT:
                {
                    return; // end of the line
                }
                case OpCode.JP_nn:
                case OpCode.JR_e:
                {
                    iIndex = iOperands[iIndex]; // unconditional branch
                    continue;
                }
                case OpCode.JP_nn:
                case OpCode.JP_Z_nn:
                case OpCode.JP_NZ_nn:
                case OpCode.DJNZ_e:
                {
                    uint iTarget = iOperands[iIndex];
                    walkInstructions(iTarget); // second potential path
                }
                case OpCode.JR_e: 
                case OpCode.JR_NZ_e: 
                case OpCode.JR_Z_e: 
                {
                    Die(0x0B); // should not exist in Z80Points
                }
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
                OpCode opCode0 = iCodes[iIndex];
                switch (opCode0)
                {
                    case OpCode.JP_nn:
                    case OpCode.JP_Z_nn:
                    case OpCode.JP_NZ_nn:
                    case OpCode.DJNZ_e: 
                    {
                        uint iTarget = iOperands[iIndex];
                        iFlags[iTarget] = iFlags[iTarget] | Flags.Target;
                    }
                    case OpCode.JR_e: 
                    case OpCode.JR_Z_e: 
                    case OpCode.JR_NZ_e: 
                    {
                        Die(0x0B); // should not be in use in Z80Points
                    }
                }
            }
            iIndex++;
        }
    }
    
    bool InlineCandidatesExist { get { return false; } } // TODO
    bool InlineSmallMethods(<byte> code)
    {
        bool inlined = false;
        
        // TODO
        return inlined;
    }
    
    
    bool OptimizeLDtoNOP()
    {
        bool modified;
        if (iCodes.Count < 4) { return modified; }
        uint iIndex = 3;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            
            if ((opCode3 == OpCode.LD_DE_nn) && (opCode2 == OpCode.PUSH_DE))
            {
                uint operand3 = iOperands[iIndex-3];
                loop
                {
                    OpCode opCode1 = iCodes[iIndex-1];
                    OpCode opCode0 = iCodes[iIndex-0];
                    if ((opCode1 == OpCode.LD_DE_nn) && (opCode0 == OpCode.PUSH_DE))
                    {
                        uint operand1 = iOperands[iIndex-1];
                        if ((operand1 == operand3) && (Flags.Target != iFlags[iIndex-1] & Flags.Target))
                        {
                            // LD DE, xx   PUSH DE    LD DE, xx   PUSH DE    ->      LD DE, xx   PUSH DE  PUSH DE  
                            iCodes  [iIndex-1] = OpCode.NOP;
                            iLengths[iIndex-1] = 1;
                            modified = true;
                            if (iIndex < iCodes.Count-3)
                            {
                                iIndex += 2;
                                continue;
                            }
                        }
                    }
                    break;
                }
            }
            iIndex++;
        }
        return modified;
    }
    bool OptimizeIncrement()
    {
        bool modified;
        if (iCodes.Count < 5) { return modified; }
        uint iIndex = 4;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
    
            OpCode opCode4 = iCodes[iIndex-4];
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            
            if ((opCode3 == OpCode.LD_DE_inn) && (opCode2 == OpCode.PUSH_DE) && (opCode1 == OpCode.LD_DE_nn) && (opCode0 == OpCode.POP_HL))
            {
                if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    // LD_DE_inn,   PUSH_DE,  LD_DE_nn,  POP_HL    ->  LD_HL_inn,  LD_DE_nn
                    iCodes  [iIndex-3] = OpCode.LD_HL_inn;
                    iLengths[iIndex-3] = 3;
                    iCodes  [iIndex-2] = OpCode.NOP;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    modified = true;
                }
            }
            if ((opCode4 == OpCode.LD_DE_inn) && (opCode3 == OpCode.PUSH_DE) && (opCode2 == OpCode.LD_E_iIY_d) && (opCode1 == OpCode.LD_D_iIY_d) && (opCode0 == OpCode.POP_HL))
            {
                if (   (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    iCodes  [iIndex-4] = OpCode.LD_HL_inn;
                    iLengths[iIndex-4] = 3;
                    iCodes  [iIndex-3] = OpCode.NOP;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    modified = true;
                }
            }
            iIndex++;
        
        }
        return modified;
    }     
    
    bool OptimizeAdd()
    {
        bool modified;
        if (iCodes.Count < 6) { return modified; }
        uint iIndex = 5;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode4 = iCodes[iIndex-4];
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
                
            if (opCode0 == OpCode.ADD_HL_DE)
            {
                OpCode opCode5 = iCodes[iIndex-5];
                
                if ((opCode5 == OpCode.LD_E_iIY_d) && (opCode4 == OpCode.LD_D_iIY_d) && (opCode3 == OpCode.PUSH_DE) && (opCode2 == OpCode.LD_DE_nn) && (opCode1 == OpCode.POP_HL))
                {
                    if (   (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                        && (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                        && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                        && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                        && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                    {
                        iCodes  [iIndex-5] = OpCode.LD_L_iIY_d;
                        iCodes  [iIndex-4] = OpCode.LD_H_iIY_d;
                        iCodes  [iIndex-3] = OpCode.NOP;
                        iCodes  [iIndex-1] = OpCode.NOP;
                        modified = true;
                    }
                }
            }
            if ((opCode4 == OpCode.LD_E_iIY_d) && (opCode3 == OpCode.LD_D_iIY_d) && (opCode2 == OpCode.PUSH_DE) && (opCode1 == OpCode.LD_BC_nn) && (opCode0 == OpCode.POP_HL))
            {
                if (   (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if (WalkAhead(iIndex+1, WalkStats.Exit | WalkStats.WriteDE, WalkStats.ReadDE, 20))
                    {
                        iCodes  [iIndex-4] = OpCode.LD_L_iIY_d;
                        iCodes  [iIndex-3] = OpCode.LD_H_iIY_d;
                        iCodes  [iIndex-2] = OpCode.NOP;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        modified = true;
                    }
                }
            }
            if ((opCode3 == OpCode.LD_E_iIY_d) && (opCode2 == OpCode.LD_D_iIY_d) && (opCode1 == OpCode.PUSH_DE) && (opCode0 == OpCode.POP_BC))
            {
                if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if (WalkAhead(iIndex+1, WalkStats.Exit | WalkStats.WriteDE, WalkStats.ReadDE, 25))
                    {
                        iCodes  [iIndex-3] = OpCode.LD_C_iIY_d;
                        iCodes  [iIndex-2] = OpCode.LD_B_iIY_d;
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        modified = true;
                    }
                }
            }
            if ((opCode3 == OpCode.LD_E_iIY_d) && (opCode2 == OpCode.LD_D_iIY_d) && (opCode1 == OpCode.PUSH_DE) && (opCode0 == OpCode.POP_HL))
            {
                if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if (WalkAhead(iIndex+1, WalkStats.Exit | WalkStats.WriteDE, WalkStats.ReadDE, 25))
                    {
                        iCodes  [iIndex-3] = OpCode.LD_L_iIY_d;
                        iCodes  [iIndex-2] = OpCode.LD_H_iIY_d;
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        modified = true;
                    }
                }
            }
            iIndex++;
        
        }
        return modified;
    }    
    
    bool OptimizeAnd()
    {
        bool modified;
        if (iCodes.Count < 7) { return modified; }
        uint iIndex = 6;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode6 = iCodes[iIndex-6];
            OpCode opCode5 = iCodes[iIndex-5];
            OpCode opCode4 = iCodes[iIndex-4];
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
                
                       
            if ((opCode6 == OpCode.LD_BC_nn) && (opCode5 == OpCode.LD_A_L) && (opCode4 == OpCode.AND_A_C) && (opCode3 == OpCode.LD_L_A) && (opCode2 == OpCode.LD_A_H) && (opCode1 == OpCode.AND_A_B) && (opCode0 == OpCode.LD_H_A))
            {
                if (   (Flags.Target != iFlags[iIndex-5] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if (WalkAhead(iIndex+1, WalkStats.Exit | WalkStats.WriteBC, WalkStats.ReadBC, 30))
                    {
                        uint lsb = (iOperands[iIndex-6] & 0xFF);
                        uint msb = (iOperands[iIndex-6] >> 8);
                        iCodes   [iIndex-6] = OpCode.NOP;
                        iLengths [iIndex-6] = 1;
                        
                        if (lsb == 0xFF)
                        {
                            iCodes   [iIndex-5] = OpCode.NOP;
                            iLengths [iIndex-5] = 1;
                            iCodes   [iIndex-4] = OpCode.NOP;
                            iLengths [iIndex-4] = 1;
                            iCodes   [iIndex-3] = OpCode.NOP;
                            iLengths [iIndex-3] = 1;
                        }
                        else if (lsb != 0)
                        {
                            iCodes   [iIndex-4] = OpCode.AND_A_n;
                            iOperands[iIndex-4] = lsb;
                            iLengths [iIndex-4] = 2;
                        }
                        else
                        {
                            iCodes   [iIndex-5] = OpCode.NOP;
                            iLengths [iIndex-5] = 1;
                            iCodes   [iIndex-4] = OpCode.NOP;
                            iLengths [iIndex-4] = 1;
                            iCodes   [iIndex-3] = OpCode.LD_L_n;
                            iOperands[iIndex-3] = lsb;
                            iLengths [iIndex-3] = 2;
                        }
                        if (msb == 0xFF)
                        {
                            iCodes   [iIndex-2] = OpCode.NOP;
                            iLengths [iIndex-2] = 1;
                            iCodes   [iIndex-1] = OpCode.NOP;
                            iLengths [iIndex-1] = 1;
                            iCodes   [iIndex-0] = OpCode.NOP;
                            iLengths [iIndex-0] = 1;
                        }
                        else if (msb != 0)
                        {
                            iCodes   [iIndex-1] = OpCode.AND_A_n;
                            iOperands[iIndex-1] = msb;
                            iLengths [iIndex-1] = 2;
                        }
                        else
                        {
                            iCodes   [iIndex-2] = OpCode.NOP;
                            iLengths [iIndex-2] = 1;
                            iCodes   [iIndex-1] = OpCode.NOP;
                            iLengths [iIndex-1] = 1;
                            iCodes   [iIndex-0] = OpCode.LD_H_n;
                            iOperands[iIndex-0] = msb;
                            iLengths [iIndex-0] = 2;
                        }
                        modified = true;
                    }
                }
            }
            
            if ((opCode6 == OpCode.LD_BC_nn) && (opCode5 == OpCode.LD_A_L) && (opCode4 == OpCode.OR_A_C) && (opCode3 == OpCode.LD_L_A) && (opCode2 == OpCode.LD_A_H) && (opCode1 == OpCode.OR_A_B) && (opCode0 == OpCode.LD_H_A))
            {
                if (   (Flags.Target != iFlags[iIndex-5] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if (WalkAhead(iIndex+1, WalkStats.Exit | WalkStats.WriteBC, WalkStats.ReadBC, 30))
                    {
                        uint lsb = (iOperands[iIndex-6] & 0xFF);
                        uint msb = (iOperands[iIndex-6] >> 8);
                        iCodes   [iIndex-6] = OpCode.NOP;
                        iLengths [iIndex-6] = 1;
                        
                        if (lsb != 0)
                        {
                            iCodes   [iIndex-4] = OpCode.OR_A_n;
                            iOperands[iIndex-4] = lsb;
                            iLengths [iIndex-4] = 2;
                        }
                        else
                        {
                            iCodes   [iIndex-5] = OpCode.NOP;
                            iLengths [iIndex-5] = 1;
                            iCodes   [iIndex-4] = OpCode.NOP;
                            iLengths [iIndex-4] = 1;
                            iCodes   [iIndex-3] = OpCode.NOP;
                            iLengths [iIndex-3] = 1;
                        }
                        if (msb != 0)
                        {
                            iCodes   [iIndex-1] = OpCode.OR_A_n;
                            iOperands[iIndex-1] = msb;
                            iLengths [iIndex-1] = 2;
                        }
                        else
                        {
                            iCodes   [iIndex-2] = OpCode.NOP;
                            iLengths [iIndex-2] = 1;
                            iCodes   [iIndex-1] = OpCode.NOP;
                            iLengths [iIndex-1] = 1;
                            iCodes   [iIndex-0] = OpCode.NOP;
                            iLengths [iIndex-0] = 1;
                        }
                        modified = true;
                    }
                }
            }
            iIndex++;
        
        }
        return modified;
    }    
    
    bool OptimizeUnreachableToNOP()
    {
        bool modified;
        if (iCodes.Count == 0) { return modified; }
        uint iIndex = 0;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            if (iFlags[iIndex] == Flags.Unreached)
            {
                OpCode opCode0 = iCodes[iIndex];
                if (opCode0 != OpCode.NOP)
                {
                    iCodes [iIndex] = OpCode.NOP;
                    iLengths[iIndex] = 1;
                    modified = true;
                }
            }
            iIndex++;
        }
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
                    if ((opCodej == OpCode.JP_nn) || (opCodej == OpCode.JP_Z_nn) || (opCodej == OpCode.JP_NZ_nn) || (opCodej == OpCode.DJNZ_e))
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
                              
                modified = true;
                continue;
            }            
            iIndex++;
        }
        return modified;
    }
    bool OptimizeReturn()
    {
        bool modified;
        if (iCodes.Count < 4) { return modified; }
        uint iIndex = 3;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            OpCode opCode0 = iCodes[iIndex-0];
                
            if (opCode0 == OpCode.RET)
            {
                OpCode opCode3 = iCodes[iIndex-3];
                OpCode opCode2 = iCodes[iIndex-2];
                OpCode opCode1 = iCodes[iIndex-1];
                
                if ((opCode3 == OpCode.LD_DE_nn) && (opCode2 == OpCode.PUSH_DE) && (opCode1 == OpCode.POP_HL))
                {
                    if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                        && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                        && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                    {
                        // short properties like Colour.MatrixGreen_Get()
                        // LD_DE_nn, PUSH_DE, POP_HL, RET     ->      LD_DE_nn, RET
                        iCodes[iIndex-3] = OpCode.LD_HL_nn;
                        iCodes[iIndex-2] = OpCode.NOP;
                        iCodes[iIndex-1] = OpCode.NOP;
                        modified = true;
                        continue;
                    }
                }
                
                if ((opCode3 == OpCode.POP_DE) && (opCode2 == OpCode.LD_SP_IY) && (opCode1 == OpCode.POP_IY))
                {
                    // It does not matter if they are jump targets: there is no escape!
                    //
                    //if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    //    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    //    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                    //{
                        // POP_DE on exit before resetting SP is pointless
                        iCodes[iIndex-3] = OpCode.NOP;
                        modified = true;
                        continue;
                    //}
                }
                if (iIndex > 4)
                {
                    OpCode opCode4 = iCodes[iIndex-4];
                    if ((opCode4 == OpCode.POP_DE) && (opCode3 == OpCode.LD_HL_nn) && (opCode2 == OpCode.LD_SP_IY) && (opCode1 == OpCode.POP_IY))
                    {
                        if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                        {
                            // POP_DE on exit before resetting SP is pointless
                            iCodes[iIndex-4] = OpCode.NOP;
                            modified = true;
                            continue;
                        }
                    }
                    
                    if (iIndex > 7)
                    {
                        OpCode opCode7 = iCodes[iIndex-7];
                        OpCode opCode6 = iCodes[iIndex-6];
                        OpCode opCode5 = iCodes[iIndex-5];
                        if ((opCode7 == OpCode.PUSH_HL)    && (opCode6 == OpCode.POP_DE) &&
                            (opCode5 == OpCode.LD_iIY_d_E) && (opCode4 == OpCode.LD_iIY_d_D) &&
                            (opCode3 == OpCode.PUSH_DE)    && (opCode2 == OpCode.POP_HL) && (opCode1 == OpCode.POP_IY))
                        {
                            if (   (Flags.Target != iFlags[iIndex-6] & Flags.Target)
                                && (Flags.Target != iFlags[iIndex-5] & Flags.Target)
                                && (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                                && (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                                && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                                && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                                && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                            {
                                iCodes[iIndex-7] = OpCode.NOP;
                                iCodes[iIndex-6] = OpCode.NOP;
                                iCodes[iIndex-3] = OpCode.NOP;
                                iCodes[iIndex-2] = OpCode.NOP;
                                
                                iCodes[iIndex-5] = OpCode.LD_iIY_d_L;
                                iCodes[iIndex-4] = OpCode.LD_iIY_d_H;
                                
                                modified = true;
                                continue;
                            }
                        }
                    }
                    
                    
                }
            } // RET
            iIndex++;
        }
        return modified;
    }
    bool OptimizePopLocalPushLocal()
    {
        bool modified;
        if (iCodes.Count < 4) { return modified; }
        uint iIndex = 3;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
    
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            
            if ((opCode3 == OpCode.LD_iIY_d_E) && (opCode2 == OpCode.LD_iIY_d_D) && (opCode1 == OpCode.LD_E_iIY_d) && (opCode0 == OpCode.LD_D_iIY_d))
            {
                if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if ((iOperands[iIndex-3] == iOperands[iIndex-1]) && (iOperands[iIndex-2] == iOperands[iIndex-0]))
                    {
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            if ((opCode3 == OpCode.LD_iIY_d_L) && (opCode2 == OpCode.LD_iIY_d_H) && (opCode1 == OpCode.LD_L_iIY_d) && (opCode0 == OpCode.LD_H_iIY_d))
            {
                if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if ((iOperands[iIndex-3] == iOperands[iIndex-1]) && (iOperands[iIndex-2] == iOperands[iIndex-0]))
                    {
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            if ((opCode3 == OpCode.LD_iIY_d_C) && (opCode2 == OpCode.LD_iIY_d_B) && (opCode1 == OpCode.LD_C_iIY_d) && (opCode0 == OpCode.LD_B_iIY_d))
            {
                if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if ((iOperands[iIndex-3] == iOperands[iIndex-1]) && (iOperands[iIndex-2] == iOperands[iIndex-0]))
                    {
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            if ((opCode1 == OpCode.LD_inn_DE) && (opCode0 == OpCode.LD_DE_inn))
            {
                if (   (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if (iOperands[iIndex-1] == iOperands[iIndex-0])
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            if ((opCode1 == OpCode.LD_inn_HL) && (opCode0 == OpCode.LD_HL_inn))
            {
                if (   (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if (iOperands[iIndex-1] == iOperands[iIndex-0])
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            if ((opCode1 == OpCode.LD_inn_BC) && (opCode0 == OpCode.LD_BC_inn))
            {
                if (   (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if (iOperands[iIndex-1] == iOperands[iIndex-0])
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            if ((opCode1 == OpCode.LD_inn_IX) && (opCode0 == OpCode.LD_IX_inn))
            {
                if (   (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if (iOperands[iIndex-1] == iOperands[iIndex-0])
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            if ((opCode1 == OpCode.LD_inn_IY) && (opCode0 == OpCode.LD_IY_inn))
            {
                if (   (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    if (iOperands[iIndex-1] == iOperands[iIndex-0])
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            iIndex++;
        
        }
        return modified;
    }  
    
    bool OptimizeWriteByte()
    {
        bool modified;
        if (iCodes.Count < 5) { return modified; }
        uint iIndex = 4;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
    
            OpCode opCode4 = iCodes[iIndex-4];
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            
            if ((opCode4 == OpCode.PUSH_HL) && (opCode3 == OpCode.LD_iIX_d_L) && (opCode2 == OpCode.LD_iIX_d_n) && (opCode1 == OpCode.POP_DE) && (opCode0 == OpCode.POP_DE))
            {
                if (   (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    iCodes  [iIndex-4] = OpCode.NOP;
                    iLengths[iIndex-4] = 1;
                    iCodes  [iIndex-1] = OpCode.NOP;
                    iLengths[iIndex-1] = 1;
                    modified = true;
                }
            }
            if ((opCode3 == OpCode.PUSH_HL) && (opCode2 == OpCode.LD_iIX_d_L) && (opCode1 == OpCode.POP_DE) && (opCode0 == OpCode.POP_DE))
            {
                if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                {
                    iCodes  [iIndex-3] = OpCode.NOP;
                    iLengths[iIndex-3] = 1;
                    iCodes  [iIndex-1] = OpCode.NOP;
                    iLengths[iIndex-1] = 1;
                    modified = true;
                }
            }
            iIndex++;
        
        }
        return modified;
    }  
    
    bool OptimizePushPop()
    {
        bool modified = false;
        if (iCodes.Count < 4) { return modified; }
        uint iIndex = 3;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            
                   
            if ((opCode3 == OpCode.PUSH_HL) && (opCode2 == OpCode.POP_DE) && (opCode1 == OpCode.LD_iIY_d_E) && (opCode0 == OpCode.LD_iIY_d_D))
            {
                if (    (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    // alters remaining value in DE .. need to look forward
                    if (WalkAhead(iIndex+1, WalkStats.Exit | WalkStats.WriteDE, WalkStats.ReadDE, 10))
                    {
                        iCodes  [iIndex-3] = OpCode.NOP;
                        iCodes  [iIndex-2] = OpCode.NOP;
                        iCodes  [iIndex-1] = OpCode.LD_iIY_d_L;
                        iCodes  [iIndex-0] = OpCode.LD_iIY_d_H;
                        modified = true;
                    }
                }
            }
            if ((opCode2 == OpCode.LD_DE_nn) && (opCode1 == OpCode.PUSH_DE) && (opCode0 == OpCode.POP_BC))
            {
                if (    (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    // alters remaining value in DE .. need to look forward
                    if (WalkAhead(iIndex+1, WalkStats.Exit | WalkStats.WriteDE, WalkStats.ReadDE, 10))
                    {
                        iCodes  [iIndex-2] = OpCode.LD_BC_nn;
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        modified = true;
                    }
                }
            }
            if ((opCode2 == OpCode.LD_DE_nn) && (opCode1 == OpCode.PUSH_DE) && (opCode0 == OpCode.POP_HL))
            {
                if (    (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    // alters remaining value in DE .. need to look forward
                    if (WalkAhead(iIndex+1, WalkStats.Exit | WalkStats.WriteDE, WalkStats.ReadDE, 10))
                    {
                        iCodes  [iIndex-2] = OpCode.LD_HL_nn;
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        modified = true;
                    }
                }
            }
            if ((opCode3 == OpCode.LD_iIY_d_L) && (opCode2 == OpCode.LD_iIY_d_H) && (opCode1 == OpCode.LD_E_iIY_d) && (opCode0 == OpCode.LD_D_iIY_d))
            {
                if (    (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    if ((iOperands[iIndex-3] == iOperands[iIndex-1]) && (iOperands[iIndex-2] == iOperands[iIndex-0]))
                    {
                        iCodes  [iIndex-1] = OpCode.LD_E_L;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.LD_D_H;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            iIndex++;
        }
        return modified;
    }
    
    bool OptimizePushPopAdd()
    {
        bool modified = false;
        if (iCodes.Count < 4) { return modified; }
        uint iIndex = 3;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            
                   
            if ((opCode3 == OpCode.PUSH_HL) && (opCode2 == OpCode.POP_DE) && (opCode1 == OpCode.POP_HL) && (opCode0 == OpCode.ADD_HL_DE))
            {
                // changes value of DE .. need to look forward
                if (    (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    // alters remaining value in DE .. need to look forward
                    if (WalkAhead(iIndex+1, WalkStats.WriteDE | WalkStats.Exit, WalkStats.ReadDE, 15))
                    {
                        iCodes  [iIndex-3] = OpCode.NOP;
                        iCodes  [iIndex-1] = OpCode.NOP;
                        modified = true;
                    }
                }
            }
            if ((opCode3 == OpCode.LD_DE_nn) && (opCode2 == OpCode.PUSH_DE) && (opCode1 == OpCode.LD_DE_nn) && (opCode0 == OpCode.POP_HL))
            {
                if (    (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    iCodes  [iIndex-3] = OpCode.LD_HL_nn;
                    iCodes  [iIndex-2] = OpCode.NOP;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    modified = true;
                }
            }
            if ((opCode3 == OpCode.LD_DE_inn) && (opCode2 == OpCode.PUSH_DE) && (opCode1 == OpCode.LD_DE_inn) && (opCode0 == OpCode.POP_HL))
            {
                if (    (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    iCodes  [iIndex-3] = OpCode.LD_HL_inn;
                    iLengths[iIndex-3] = 3;
                    iCodes  [iIndex-2] = OpCode.NOP;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    modified = true;
                }
            }
            
            iIndex++;
        }
        return modified;
    }
    bool OptimizeShift8()
    {
        bool modified = false;
        if (iCodes.Count < 6) { return modified; }
        uint iIndex = 5;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode5 = iCodes[iIndex-5];
            OpCode opCode4 = iCodes[iIndex-4];
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            if ((opCode5 == OpCode.PUSH_DE) && (opCode4 == OpCode.EX_iSP_HL) && (opCode3 == OpCode.LD_L_H) && (opCode2 == OpCode.LD_H_n) && (opCode1 == OpCode.EX_iSP_HL) && (opCode0 == OpCode.POP_HL))
            {
                if (    (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    iCodes  [iIndex-5] = OpCode.NOP;
                    iCodes  [iIndex-4] = OpCode.NOP;
                    iCodes  [iIndex-3] = OpCode.LD_L_D;
                    iCodes  [iIndex-1] = OpCode.NOP;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    modified = true;
                }
            }
            if ((opCode4 == OpCode.PUSH_DE) && (opCode3 == OpCode.EX_iSP_HL) && (opCode2 == OpCode.LD_H_n) && (opCode1 == OpCode.EX_iSP_HL) && (opCode0 == OpCode.POP_HL))
            {
                if (    (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    iCodes  [iIndex-4] = OpCode.NOP;
                    iCodes  [iIndex-3] = OpCode.LD_L_E;
                    iCodes  [iIndex-1] = OpCode.NOP;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    modified = true;
                }
            }
            if ((opCode3 == OpCode.PUSH_DE) && (opCode2 == OpCode.POP_HL) && (opCode1 == OpCode.LD_H_L) && (opCode0 == OpCode.LD_L_n))
            {
                if (    (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    iCodes  [iIndex-3] = OpCode.NOP;
                    iCodes  [iIndex-2] = OpCode.NOP;
                    iCodes  [iIndex-1] = OpCode.LD_H_E;
                    modified = true;
                }
            }
            if ((opCode3 == OpCode.LD_E_iIY_d) && (opCode2 == OpCode.LD_D_iIY_d) && (opCode1 == OpCode.LD_H_E) && (opCode0 == OpCode.LD_L_n))
            {
                if (    (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    // alters remaining value in DE .. need to look forward
                    if (WalkAhead(iIndex+1, WalkStats.WriteDE | WalkStats.Exit, WalkStats.ReadDE, 10))
                    {
                        iCodes  [iIndex-3] = OpCode.LD_H_iIY_d;
                        iCodes  [iIndex-2] = OpCode.NOP;
                        iLengths[iIndex-2] = 1;
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        modified = true;
                    }
                }
            }
            
            iIndex++;
        }
        return modified;
    }
    bool OptimizePushSandwichPop()
    {
        bool modified = false;
        if (iCodes.Count < 4) { return modified; }
        uint iIndex = 3;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            
            string name2 = AsmZ80.GetName(opCode2);
            string name1 = AsmZ80.GetName(opCode1);
            
            bool nonStarter = (opCode1 == OpCode.CALL_nn) || AsmZ80.IsPush(opCode1) || AsmZ80.IsPop(opCode1) ||
                              (opCode1 == OpCode.EX_iSP_HL) || (opCode1 == OpCode.LD_DE_inn)
                              ;
            if (!nonStarter)
            {                 
                nonStarter = (    (Flags.Target == iFlags[iIndex-1] & Flags.Target)
                               || (Flags.Target == iFlags[iIndex-0] & Flags.Target));
            }
            if (!nonStarter)
            {
                if ((opCode2 == OpCode.PUSH_DE) && (opCode0 == OpCode.POP_DE))
                {
                    if (opCode1 == OpCode.LD_H_E)
                    {
                        iCodes  [iIndex-2] = OpCode.NOP;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        modified = true;
                        // seen in Numbers
                        continue;
                    }
                    else if (opCode1 == OpCode.LD_BC_nn)
                    {
                        // munts Numbers.hs
                    }
                    else
                    {
                        PrintLn(" X:" + name1);
                    }
                }
                if ((opCode2 == OpCode.PUSH_HL) && (opCode0 == OpCode.POP_HL))
                {
                    if ((opCode1 == OpCode.XOR_A) || (opCode1 == OpCode.LD_DE_inn) || (opCode1 == OpCode.LD_DE_nn) || (opCode1 == OpCode.LD_BC_nn))
                    {
                        iCodes  [iIndex-2] = OpCode.NOP;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        // seen in Numbers
                        modified = true;
                        continue;
                    }
                    else
                    {
                        Print("   Z:" + name1);
                    }
                }
                nonStarter = (opCode2 == OpCode.CALL_nn) || AsmZ80.IsPush(opCode2) || AsmZ80.IsPop(opCode2) ||
                             (opCode2 == OpCode.EX_iSP_HL);
                if (!nonStarter)
                {                 
                    nonStarter = (Flags.Target == iFlags[iIndex-2] & Flags.Target);
                }
                if (!nonStarter)
                {
                    if ((opCode3 == OpCode.PUSH_DE) && (opCode0 == OpCode.POP_DE))
                    {
                        /*
                        if (   ((opCode2 == OpCode.LD_H_iIY_d) && (opCode1 == OpCode.LD_L_n))
                            //|| ((opCode2 == OpCode.LD_C_iIY_d) && (opCode1 == OpCode.LD_B_iIY_d))
                           )
                        {
                            iCodes  [iIndex-3] = OpCode.NOP;
                            iCodes  [iIndex-0] = OpCode.NOP;
                            modified = true;
                            Print(" C "); // unseen and untested?
                            continue;
                        }
                        else 
                        */
                        if ((opCode2 == OpCode.LD_C_iIY_d) && (opCode1 == OpCode.LD_B_iIY_d))
                        {
                            // munts Numbers.hs
                        }
                        else
                        {
                           Print("   W:" + name2 + "-" + name1);
                        }
                    }
                    if ((opCode3 == OpCode.PUSH_HL) && (opCode0 == OpCode.POP_HL))
                    {
                        if ( ((opCode2 == OpCode.LD_C_iIY_d) && (opCode1 == OpCode.LD_B_iIY_d))
                           )
                        {
                            iCodes  [iIndex-3] = OpCode.NOP;
                            iCodes  [iIndex-0] = OpCode.NOP;
                            modified = true;
                            // seen in Numbers
                            continue;
                        }
                        else if ( ((opCode2 == OpCode.LD_E_iIY_d) && (opCode1 == OpCode.LD_D_iIY_d))
                           )
                        {
                            iCodes  [iIndex-3] = OpCode.NOP;
                            iCodes  [iIndex-0] = OpCode.NOP;
                            modified = true;
                            // seen in Numbers
                            continue;
                        }
                        else
                        {
                            Print("   Y:" + name2 + "-" + name1);
                        }
                    }
                }
            }
                       
            iIndex++;
        }
        return modified;
    }     
    bool OptimizeMoveXOR()
    {
        bool modified = false;
        if (iCodes.Count < 2) { return modified; }
        uint iIndex = 1;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            if ((opCode1 == OpCode.PUSH_DE) && (opCode0 == OpCode.XOR_A))
            {
                if (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                {
                    iCodes[iIndex-1] = OpCode.XOR_A;
                    iCodes[iIndex-0] = OpCode.PUSH_DE;
                    modified = true;
                }
            }
            
            iIndex++;
        }
        return modified;
    }
    bool OptimizePushPopCompare()
    {
        bool modified = false;
        if (iCodes.Count < 3) { return modified; }
        uint iIndex = 2;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            if ((opCode2 == OpCode.PUSH_DE) && (opCode1 == OpCode.POP_HL) && (opCode0 == OpCode.CP_A_L))
            {
                if (   (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                    )
                {
                    // alters remaining value in HL .. need to look forward
                    if (WalkAhead(iIndex+1, WalkStats.WriteHL | WalkStats.Exit, WalkStats.ReadHL, 25))
                    {
                        iCodes[iIndex-2] = OpCode.NOP;
                        iCodes[iIndex-1] = OpCode.NOP;
                        iCodes[iIndex-0] = OpCode.CP_A_E;
                        modified = true;
                    }
                }
            }
            iIndex++;
        }
        return modified;
    }
    bool OptimizeIndirectLoadByte()
    {
        bool modified = false;
        if (iCodes.Count < 5) { return modified; }
        uint iIndex = 4;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode4 = iCodes[iIndex-4];
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            if ((opCode4 == OpCode.PUSH_HL) && (opCode3 == OpCode.EX_iSP_IX) && (opCode2 == OpCode.LD_L_iIX_d) && (opCode1 == OpCode.LD_H_n) && (opCode0 == OpCode.POP_DE))
            {
                if (   (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                    )
                {
                    // alters remaining value in HL .. need to look forward
                    if (WalkAhead(iIndex+1, WalkStats.WriteDE | WalkStats.Exit, WalkStats.ReadDE, 15))
                    {
                        iCodes  [iIndex-4] = OpCode.NOP;
                        iLengths[iIndex-4] = 1;
                        iCodes  [iIndex-3] = OpCode.NOP;
                        iLengths[iIndex-3] = 1;
                        iCodes  [iIndex-2] = OpCode.LD_L_iHL;
                        iLengths[iIndex-2] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            if ((opCode3 == OpCode.LD_L_iHL) && (opCode2 == OpCode.LD_H_n) && (opCode1 == OpCode.LD_H_L) && (opCode0 == OpCode.LD_L_n))
            {
                if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                    )
                {
                    if (iOperands[iIndex-2] == iOperands[iIndex-0])
                    {
                        iCodes  [iIndex-3] = OpCode.LD_H_iHL;
                        iLengths[iIndex-3] = 1;
                        iCodes  [iIndex-2] = OpCode.NOP;
                        iLengths[iIndex-2] = 1;
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        modified = true;
                    }
                }
            }
            iIndex++;
        }
        return modified;
    }
    bool OptimizeNOP8BitLoad()
    {
        bool modified = false;
        if (iCodes.Count < 1) { return modified; }
        uint iIndex = 0;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode0 = iCodes[iIndex-0];
            
            if ((opCode0 == OpCode.LD_D_iIY_d) || (opCode0 == OpCode.LD_D_iIX_d))
            {
                // do we ever read this value from D?
                if (WalkAhead(iIndex+1, WalkStats.WriteD | WalkStats.Exit, WalkStats.ReadD, 10))
                {
                    
                    iCodes  [iIndex-0] = OpCode.NOP;
                    iLengths[iIndex-0] = 1;
                    modified = true;
                }
            }
            else if ((opCode0 == OpCode.LD_E_iIY_d) || (opCode0 == OpCode.LD_E_iIX_d))
            {
                // do we ever read this value from E?
                if ( WalkAhead(iIndex+1, WalkStats.WriteE | WalkStats.Exit, WalkStats.ReadE, 10))
                {
                    iCodes  [iIndex-0] = OpCode.NOP;
                    iLengths[iIndex-0] = 1;
                    modified = true;
                }
            }
            else if ((opCode0 == OpCode.LD_H_iIY_d) || (opCode0 == OpCode.LD_H_iIX_d))
            {
                // do we ever read this value from H?
                if (currentReturnType != "void")
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteH, WalkStats.ReadH | WalkStats.Exit, 10)) // HL is the return value so Exit DQ's
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
                else
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteH | WalkStats.Exit, WalkStats.ReadH, 10)) // void method so Exit is +ve
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            else if ((opCode0 == OpCode.LD_L_iIY_d) || (opCode0 == OpCode.LD_L_iIX_d))
            {
                // do we ever read this value from L?
                if (currentReturnType != "void")
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteL, WalkStats.ReadL | WalkStats.Exit, 10)) // HL is the return value so Exit DQ's
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
                else
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteL | WalkStats.Exit, WalkStats.ReadL, 10)) // void method so Exit is +ve
                    {
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            iIndex++;
        }
        return modified;
    }
    bool Optimize8BitLoadLoad()
    {
        bool modified = false;
        if (iCodes.Count < 2) { return modified; }
        uint iIndex = 1;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0]; 
                       
            if ((opCode1 == OpCode.LD_D_iIY_d) && (opCode0 == OpCode.LD_L_D))
            {
                if (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                {
                    // do we ever use this value in D?
                    if (WalkAhead(iIndex+1, WalkStats.WriteD | WalkStats.Exit, WalkStats.ReadD, 10))
                    {
                        iCodes  [iIndex-1] = OpCode.LD_L_iIY_d;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        modified = true;
                    }
                }
            }
            if ((opCode1 == OpCode.LD_E_iIY_d) && (opCode0 == OpCode.LD_L_E))
            {
                if (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                {
                    // do we ever use this value in D?
                    if (WalkAhead(iIndex+1, WalkStats.WriteE | WalkStats.Exit, WalkStats.ReadE, 10))
                    {
                        iCodes  [iIndex-1] = OpCode.LD_L_iIY_d;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        modified = true;
                    }
                }
            }
            iIndex++;
        }
        return modified;
    }
    bool OptimizeTernary()
    {
        bool modified = false;
        if (iCodes.Count < 8) { return modified; }
        uint iIndex = 7;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode4 = iCodes[iIndex-4];
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            
            if ((opCode4 == OpCode.CP_A_L) && (opCode3 == OpCode.JP_NZ_nn)  && (opCode2 == OpCode.LD_DE_nn) && (opCode1 == OpCode.PUSH_DE) && (opCode0 == OpCode.POP_DE)
                                                                            && (iOperands[iIndex-2] == 0)
               )
            {
                if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                    )
                {
                    iCodes  [iIndex-2] = OpCode.LD_E_L;
                    iLengths[iIndex-2] = 1;
                    iCodes  [iIndex-1] = OpCode.LD_D_L;
                    iLengths[iIndex-1] = 1;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    iLengths[iIndex-0] = 1;
                    modified = true;
                }
            }   
            if ((opCode3 == OpCode.LD_E_L)  && (opCode2 == OpCode.LD_D_L) && (opCode1 == OpCode.LD_iIY_d_E) && (opCode0 == OpCode.LD_iIY_d_D))
            {
                if (   (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                    && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                    )
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteDE | WalkStats.Exit, WalkStats.ReadDE, 10))
                    {
                        iCodes  [iIndex-3] = OpCode.NOP;
                        iLengths[iIndex-3] = 1;
                        iCodes  [iIndex-2] = OpCode.NOP;
                        iLengths[iIndex-2] = 1;
                        iCodes  [iIndex-1] = OpCode.LD_iIY_d_L;
                        iCodes  [iIndex-0] = OpCode.LD_iIY_d_L;
                        modified = true;
                    }
                }
            }           
            iIndex++;
        }
        return modified;
   }
   bool OptimizeTrivialPushPop()
   {
        bool modified = false;
        if (iCodes.Count < 2) { return modified; }
        uint iIndex = 1;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
        
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];    
            if ((opCode1 == OpCode.PUSH_DE) && (opCode0 == OpCode.POP_DE))
            {
                if (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                {
                    iCodes  [iIndex-1] = OpCode.NOP;
                    iLengths[iIndex-1] = 1;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    iLengths[iIndex-0] = 1;
                    modified = true;
                }
            }
            if ((opCode1 == OpCode.PUSH_HL) && (opCode0 == OpCode.POP_HL))
            {
                if (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                {
                    iCodes  [iIndex-1] = OpCode.NOP;
                    iLengths[iIndex-1] = 1;
                    iCodes  [iIndex-0] = OpCode.NOP;
                    iLengths[iIndex-0] = 1;
                    modified = true;
                }
            }
            iIndex++;
        }
        return modified;   
    }
    bool OptimizeEQJ()
    {
        bool modified = false;
        if (iCodes.Count < 11) { return modified; }
        uint iIndex = 10;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
        
            OpCode opCode0  = iCodes[iIndex-0]; 
            if ( (opCode0 == OpCode.JP_Z_nn) || (opCode0 == OpCode.JP_NZ_nn) )
            {
                OpCode opCode7 = iCodes[iIndex-7];
                OpCode opCode4 = iCodes[iIndex-4]; 
                if (   (opCode7 == OpCode.JP_NZ_nn) && (opCode4 == OpCode.JP_NZ_nn)
                    && (iOperands[iIndex-7] == iIndex-2)   && (iOperands[iIndex-4] == iIndex-2)
                   )
                {
                    OpCode opCode10 = iCodes[iIndex-10];
                    OpCode opCode9  = iCodes[iIndex-9];
                    OpCode opCode8  = iCodes[iIndex-8];
                    if (   (opCode10 == OpCode.LD_DE_nn) && (opCode9 == OpCode.LD_A_B) && (opCode8 == OpCode.CP_A_H)
                        && ( (iOperands[iIndex-10] == 0) || (iOperands[iIndex-10] == 1))  
                       )
                    {
                        OpCode opCode6  = iCodes[iIndex-6];
                        OpCode opCode5  = iCodes[iIndex-5];
                        OpCode opCode3  = iCodes[iIndex-3];
                        OpCode opCode2  = iCodes[iIndex-2];
                        OpCode opCode1  = iCodes[iIndex-1];
                        if ((opCode6 == OpCode.LD_A_C) && (opCode5 == OpCode.CP_A_L) && (opCode3 == OpCode.LD_E_n) && (opCode2 == OpCode.XOR_A) && (opCode1 == OpCode.CP_A_E))
                        {
                            if (    (Flags.Target != iFlags[iIndex-10] & Flags.Target)
                                 && (Flags.Target != iFlags[iIndex-9] & Flags.Target)
                                 && (Flags.Target != iFlags[iIndex-8] & Flags.Target)
                                 && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                               )
                            {       
                                iCodes  [iIndex-10] = OpCode.NOP;
                                iLengths[iIndex-10] = 1;
                                
                                iOperands[iIndex-7] = iIndex-4;
                                
                                iCodes  [iIndex-4] = OpCode.NOP;
                                iLengths[iIndex-4] = 1;
                                iCodes  [iIndex-3] = OpCode.NOP;
                                iLengths[iIndex-3] = 1;
                                iCodes  [iIndex-2] = OpCode.NOP;
                                iLengths[iIndex-2] = 1;
                                iCodes  [iIndex-1] = OpCode.NOP;
                                iLengths[iIndex-1] = 1;
                              
                                if (iOperands[iIndex-10] == 0) // EQ flips the jump condition, NE does not
                                {
                                    iCodes  [iIndex-0] = (opCode0 == OpCode.JP_Z_nn) ? OpCode.JP_NZ_nn : OpCode.JP_Z_nn;
                                }
                                
                                iFlags[iIndex-2]   = iFlags[iIndex-2]  & ~Flags.Target;
                                iFlags[iIndex-4]   = iFlags[iIndex-4]  |  Flags.Target;
                                
                                modified = true;  
                            }
                        }
                    }
                }
            }
            
            iIndex++;
        }
        return modified;   
    }
    bool OptimizeOUT()
    {
        bool modified = false;
        if (iCodes.Count < 6) { return modified; }
        uint iIndex = 5;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
        
            OpCode opCode5  = iCodes[iIndex-5];
            OpCode opCode4  = iCodes[iIndex-4];
            OpCode opCode3  = iCodes[iIndex-3];
            OpCode opCode2  = iCodes[iIndex-2];
            OpCode opCode1  = iCodes[iIndex-1];
            OpCode opCode0  = iCodes[iIndex-0];
            if ((opCode5 == OpCode.PUSH_DE) && (opCode4 == OpCode.EX_iSP_HL)  && (opCode0 == OpCode.POP_DE) )
            {
                if (    (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    if ((opCode3 == OpCode.LD_L_H) && (opCode2 == OpCode.LD_H_n) && (opCode1 == OpCode.EX_iSP_HL))
                    {
                        iCodes  [iIndex-5] = OpCode.NOP;
                        iLengths[iIndex-5] = 1;
                        iCodes  [iIndex-4] = OpCode.NOP;
                        iLengths[iIndex-4] = 1;

                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        
                        iCodes  [iIndex-3] = OpCode.LD_E_D;
                        iCodes  [iIndex-2] = OpCode.LD_D_n;
                        modified = true;
                    }
                    else
                    {
                        // there are more ..
                    }
                }
            }
            if ((opCode4 == OpCode.PUSH_DE) && (opCode3 == OpCode.EX_iSP_HL)  && (opCode0 == OpCode.POP_DE) )
            {
                if (    (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    if ((opCode2 == OpCode.LD_A_L) && (opCode1 == OpCode.OUT_n_A))
                    {
                        iCodes  [iIndex-4] = OpCode.NOP;
                        iLengths[iIndex-4] = 1;
                        iCodes  [iIndex-3] = OpCode.NOP;
                        iLengths[iIndex-3] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        
                        iCodes  [iIndex-2] = OpCode.LD_A_E;
                        modified = true;
                    }
                    else if ((opCode2 == OpCode.LD_H_n) && (opCode1 == OpCode.EX_iSP_HL))
                    {
                        iCodes  [iIndex-4] = OpCode.NOP;
                        iLengths[iIndex-4] = 1;
                        iCodes  [iIndex-3] = OpCode.NOP;
                        iLengths[iIndex-3] = 1;
                        iCodes  [iIndex-1] = OpCode.NOP;
                        iLengths[iIndex-1] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        
                        iCodes  [iIndex-2] = OpCode.LD_D_n;
                        modified = true;
                    }
                }
            }
            if ((opCode3 == OpCode.PUSH_DE) && (opCode2 == OpCode.EX_iSP_HL)  && (opCode0 == OpCode.POP_DE) )
            {
                if (    (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    if (opCode1 == OpCode.LD_inn_HL)
                    {
                        iCodes  [iIndex-2] = OpCode.POP_HL;
                        iLengths[iIndex-2] = 1;
                        iCodes  [iIndex-0] = OpCode.NOP;
                        iLengths[iIndex-0] = 1;
                        modified = true;
                    }
                }
            }
            
            iIndex++;
        }
        return modified;
    }
    bool OptimizeJumpTable()
    {
        bool modified = false;
        if (iCodes.Count < 3) { return modified; }
        uint iIndex = 2;
        
        loop
        {
            if (iIndex == iCodes.Count) { break; }
        
            OpCode opCode2  = iCodes[iIndex-2];
            OpCode opCode1  = iCodes[iIndex-1];
            OpCode opCode0  = iCodes[iIndex-0];
            
            if ((opCode2 == OpCode.POP_DE) && (opCode1 == OpCode.LD_DE_nn) && (opCode0 == OpCode.PUSH_DE))
            {
                if (    (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    if (WalkAhead(iIndex+1, WalkStats.WriteDE | WalkStats.Exit, WalkStats.ReadDE, 20))
                    {
                        if (WalkAhead(iIndex+1, WalkStats.WriteHL | WalkStats.Exit, WalkStats.ReadHL, 20))
                        {
                            iCodes  [iIndex-2] = OpCode.NOP;
                            iLengths[iIndex-2] = 1;
                            
                            iCodes  [iIndex-1] = OpCode.LD_HL_nn;
                            iCodes  [iIndex-0] = OpCode.EX_iSP_HL;
                            
                            modified = true;
                        }
                    }
                }
            }
            iIndex++;
        }
        return modified;
    }
    
    bool OptimizeUseBC()
    {
        bool modified = false;
        if (iCodes.Count < 0) { return modified; }
        uint iIndex = 0;
        
        /*
        //if (WalkAhead(iIndex+1, WalkStats.None, WalkStats.ReadBC | WalkStats.WriteBC, 100))
        //{
            PrintLn();
            Print(currentMethod.ToHexString(4) + ": " + (iCodes.Count).ToString() + " ");
            WalkVerbose(iIndex, WalkStats.None, WalkStats.ReadBC | WalkStats.WriteBC, 100);
        //}
        */
        
        return modified;
    }
}
