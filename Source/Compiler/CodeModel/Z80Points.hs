unit CodePoints
{
    uint currentMethod;
    
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
    
    CountCases()
    {
        if (iCodes.Count < 4) { return; }
        uint iIndex = 3;
        
        firstInMethod = true;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
            
            OpCode opCode3 = iCodes[iIndex-3];
            OpCode opCode2 = iCodes[iIndex-2];
            OpCode opCode1 = iCodes[iIndex-1];
            OpCode opCode0 = iCodes[iIndex-0];
            
            if (false && (opCode0 == OpCode.JP_nn) || (opCode0 == OpCode.JP_NZ_nn) || (opCode0 == OpCode.JP_Z_nn))
            {
                // found none
                uint iTarget = iOperands[iIndex-0];
                OpCode opCodeJ = iCodes[iTarget];
                if ((opCodeJ == OpCode.RET))
                {   
                    if ((opCode0 == OpCode.JP_nn) || (opCode0 == OpCode.JP_NZ_nn) || (opCode0 == OpCode.JP_Z_nn))
                    {
                        incTotal(3, iIndex);
                    }
                    else
                    {
                        incTotal(2, iIndex);
                    }
                }
            }
            
            if (false && (opCode3 == OpCode.PUSH_HL) && (opCode2 == OpCode.POP_DE) && (opCode1 == OpCode.POP_HL) && (opCode0 == OpCode.ADD_HL_DE))
            {
                // changes value of DE .. need to look forward
                if (    (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                     && (Flags.Target != iFlags[iIndex-0] & Flags.Target)
                   )
                {
                    // PUSH HL, POP DE, POP HL, ADD HL, DE    ->   POP DE, ADD HL, DE (since ADD is commutative) - saving 2 bytes
                    incTotal(2, iIndex);
                }
            }
            if (false && (opCode3 == OpCode.LD_DE_nn) && (opCode2 == OpCode.PUSH_DE) && (opCode1 == OpCode.LD_DE_nn) && (opCode0 == OpCode.POP_HL))
            {
                // LD_DE_nn,   PUSH_DE,  LD_DE_nn,  POP_HL    ->  LD_HL_nn,  LD_DE_nn   -- only 3 of these ?!
                incTotal(2, iIndex);
            }
            if (iIndex >= 5)
            {
                OpCode opCode4 = iCodes[iIndex-4];
                
                if (false && (opCode4 == OpCode.LD_L_iIX_d) && (opCode3 == OpCode.LD_H_n) && (opCode2 == OpCode.POP_DE) && (opCode1 == OpCode.LD_H_L) && (opCode0 == OpCode.LD_L_n))
                {
                    // replace with POP_DE,  LD_H_iIX_d, LD_L_n - saves 5 bytes
                    incTotal(5, iIndex);
                }
                if (false && (opCode4 == OpCode.PUSH_DE) && (opCode3 == OpCode.LD_E_iIY_d) && (opCode2 == OpCode.LD_D_iIY_d) && (opCode1 == OpCode.POP_HL) && (opCode0 == OpCode.ADD_HL_DE))
                {
                    // replace with: LD_L_iIY_d, LD_H_iIY_d, ADD_HL_DE - depends on DE being lost soon after
                    incTotal(2, iIndex);
                }
            }
            iIndex++;
        }
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
    uint Load(uint methodIndex)
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
        if (iCodes.Count < 4) { return modified; }
        uint iIndex = 3;
        loop
        {
            if (iIndex == iCodes.Count) { break; }
    
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
                    iCodes  [iIndex-2] = OpCode.NOP;
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
            OpCode opCode0 = iCodes[iIndex-0];
                
            if (opCode0 == OpCode.ADD_HL_DE)
            {
                OpCode opCode5 = iCodes[iIndex-5];
                OpCode opCode4 = iCodes[iIndex-4];
                OpCode opCode3 = iCodes[iIndex-3];
                OpCode opCode2 = iCodes[iIndex-2];
                OpCode opCode1 = iCodes[iIndex-1];
                
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
    bool OptimizeConstantProperties()
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
                if (iIndex - 4 < iCodes.Count) 
                {
                    OpCode opCode4 = iCodes[iIndex-4];
                    if ((opCode4 == OpCode.LD_DE_nn) && (opCode3 == OpCode.PUSH_DE) && (opCode2 == OpCode.POP_HL) && (opCode1 == OpCode.POP_IY))
                    {
                        if (   (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                        {
                            iCodes[iIndex-4] = OpCode.LD_HL_nn;
                            iCodes[iIndex-3] = OpCode.NOP;
                            iCodes[iIndex-2] = OpCode.NOP;
                            modified = true;
                            continue;
                        }
                    }
                }
                if (iIndex - 5 < iCodes.Count) 
                {
                    OpCode opCode5 = iCodes[iIndex-5];
                    OpCode opCode4 = iCodes[iIndex-4];
                    if ((opCode5 == OpCode.LD_DE_nn) && (opCode4 == OpCode.PUSH_DE) && (opCode3 == OpCode.POP_HL) && 
                        (opCode2 == OpCode.POP_DE) && (opCode1 == OpCode.POP_IY))
                    {
                        if (   (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                        {
                            iCodes[iIndex-5] = OpCode.LD_HL_nn;
                            iCodes[iIndex-4] = OpCode.NOP;
                            iCodes[iIndex-3] = OpCode.NOP;
                            modified = true;
                            continue;
                        }
                    }
                }
                if (iIndex - 6 < iCodes.Count) 
                {
                    OpCode opCode6 = iCodes[iIndex-6];
                    OpCode opCode5 = iCodes[iIndex-5];
                    OpCode opCode4 = iCodes[iIndex-4];
                    if ((opCode6 == OpCode.LD_DE_nn) && (opCode5 == OpCode.PUSH_DE) && (opCode4 == OpCode.POP_HL) && 
                        (opCode3 == OpCode.POP_DE) && (opCode2 == OpCode.POP_DE) && (opCode1 == OpCode.POP_IY))
                    {
                        if (   (Flags.Target != iFlags[iIndex-5] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                        {
                            iCodes[iIndex-6] = OpCode.LD_HL_nn;
                            iCodes[iIndex-5] = OpCode.NOP;
                            iCodes[iIndex-4] = OpCode.NOP;
                            modified = true;
                            continue;
                        }
                    }
                }
                if (iIndex - 7 < iCodes.Count) 
                {
                    OpCode opCode7 = iCodes[iIndex-7];
                    OpCode opCode6 = iCodes[iIndex-6];
                    OpCode opCode5 = iCodes[iIndex-5];
                    OpCode opCode4 = iCodes[iIndex-4];
                    if ((opCode7 == OpCode.LD_DE_nn) && (opCode6 == OpCode.PUSH_DE) && (opCode5 == OpCode.POP_HL) && 
                        (opCode4 == OpCode.POP_DE) && (opCode3 == OpCode.POP_DE)&& (opCode2 == OpCode.POP_DE) && (opCode1 == OpCode.POP_IY))
                    {
                        if (   (Flags.Target != iFlags[iIndex-6] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-5] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                        {
                            iCodes[iIndex-7] = OpCode.LD_HL_nn;
                            iCodes[iIndex-6] = OpCode.NOP;
                            iCodes[iIndex-5] = OpCode.NOP;
                            modified = true;
                            continue;
                        }
                    }
                    if ((opCode7 == OpCode.LD_DE_nn) && (opCode6 == OpCode.PUSH_DE) && (opCode5 == OpCode.POP_HL) && 
                        (opCode4 == OpCode.LD_B_n)   && (opCode3 == OpCode.POP_DE)  && (opCode2 == OpCode.DJNZ_e) && (opCode1 == OpCode.POP_IY))
                    {
                        if (   (Flags.Target != iFlags[iIndex-6] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-5] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                            //&& (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                        {
                            iCodes[iIndex-7] = OpCode.LD_HL_nn;
                            iCodes[iIndex-6] = OpCode.NOP;
                            iCodes[iIndex-5] = OpCode.NOP;
                            modified = true;
                            continue;
                        }
                    }
                }
                if (iIndex - 8 < iCodes.Count) 
                {
                    OpCode opCode8 = iCodes[iIndex-8];
                    OpCode opCode7 = iCodes[iIndex-7];
                    OpCode opCode6 = iCodes[iIndex-6];
                    OpCode opCode5 = iCodes[iIndex-5];
                    OpCode opCode4 = iCodes[iIndex-4];
                    if ((opCode8 == OpCode.LD_DE_nn) && (opCode7 == OpCode.PUSH_DE) && (opCode6 == OpCode.POP_HL) && 
                        (opCode5 == OpCode.POP_DE) && (opCode4 == OpCode.POP_DE)&& (opCode3 == OpCode.POP_DE) &&(opCode2 == OpCode.POP_DE) && (opCode1 == OpCode.POP_IY))
                    {
                        if (   (Flags.Target != iFlags[iIndex-7] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-6] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-5] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                        {
                            iCodes[iIndex-8] = OpCode.LD_HL_nn;
                            iCodes[iIndex-7] = OpCode.NOP;
                            iCodes[iIndex-6] = OpCode.NOP;
                            modified = true;
                            continue;
                        }
                    }
                }
                if (iIndex - 9 < iCodes.Count) 
                {
                    OpCode opCode9 = iCodes[iIndex-9];
                    OpCode opCode8 = iCodes[iIndex-8];
                    OpCode opCode7 = iCodes[iIndex-7];
                    OpCode opCode6 = iCodes[iIndex-6];
                    OpCode opCode5 = iCodes[iIndex-5];
                    OpCode opCode4 = iCodes[iIndex-4];
                    if ((opCode9 == OpCode.LD_DE_nn) && (opCode8 == OpCode.PUSH_DE) && (opCode7 == OpCode.POP_HL) && 
                        (opCode6 == OpCode.POP_DE) && (opCode5 == OpCode.POP_DE)&& (opCode4 == OpCode.POP_DE) &&(opCode3 == OpCode.POP_DE) &&(opCode2 == OpCode.POP_DE) && (opCode1 == OpCode.POP_IY))
                    {
                        if (   (Flags.Target != iFlags[iIndex-8] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-7] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-6] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-5] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-4] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-3] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-2] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-1] & Flags.Target)
                            && (Flags.Target != iFlags[iIndex-0] & Flags.Target))
                        {
                            iCodes[iIndex-9] = OpCode.LD_HL_nn;
                            iCodes[iIndex-8] = OpCode.NOP;
                            iCodes[iIndex-7] = OpCode.NOP;
                            modified = true;
                            continue;
                        }
                    }
                }
            }
            iIndex++;
        }
        return modified;
    }
    
}
