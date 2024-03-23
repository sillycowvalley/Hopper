unit AsmStream
{
    #define ASMSTREAM
    
    uses "/Source/Compiler/CODEGEN/OpCodes"
        
    <string,string> debugInfo;
    <string,bool> debugInfoLineUsed;
    <byte> currentStream;
    <byte> constantStream;
    
    <byte> CurrentStream { get { return currentStream; } }
    <string,string> DebugInfo { get { return debugInfo; } }
    ClearDebugInfo()
    {
        debugInfo.Clear();
        debugInfoLineUsed.Clear();
    }
    
    bool InUse { get { return currentStream.Count != 0; } } 
    
    uint NextAddress 
    { 
        get 
        { 
            return currentStream.Count;
        } 
    }
    byte GetCodeByte(uint index)
    {
        return currentStream[index];
    }
     
    AppendCode(<byte> code)
    {
        foreach (var b in code)
        {
            currentStream.Append(b);        
        }
    }
    AppendCode(byte b)
    {
        currentStream.Append(b);        
    }
    New()
    {
        currentStream.Clear();
    }
    New(<byte> starterStream)
    {
        currentStream = starterStream;
    }
    <byte> GetConstantStream()
    {
        return constantStream;
    }
    InsertDebugInfo(bool usePreviousToken)
    {
        <string,string> token;
        if (!usePreviousToken)
        {
            token = CurrentToken;    
        }
        else
        {
            token = PreviousToken;
        }
        uint na = NextAddress;
        string nextAddress = na.ToString();
        string ln = token["line"];
        if (!debugInfoLineUsed.Contains(ln)) // keep the one with the earliest address
        {
            debugInfo[nextAddress] = ln;       
            debugInfoLineUsed[ln] = true;
        }
    }
    
    PopTail(uint pops)
    {
        loop
        {
            uint iLast = currentStream.Count - 1;
            currentStream.Remove(iLast);
            pops--;
            if (pops == 0)
            {
                break;
            }
        }
    }
    bool LastInstructionIsRET(bool orHALT)
    {
        bool isRET;
        if (currentStream.Count > 0)
        {
            uint iLast = currentStream.Count - 1;
            byte last = currentStream[iLast];
            uint lastw = last;
            if (currentStream.Count > 1)
            {
                lastw = currentStream[iLast-1] + (last << 8);
            }
            isRET = (last == OpCodes.GetRETInstruction());
            if (!isRET)
            {
                uint ret = OpCodes.GetRETIInstruction();
                isRET = (ret <= 0xFF) ? (ret == last) : (ret == lastw);
                if (!isRET)
                {      
                    ret = OpCodes.GetRETNInstruction();
                    isRET = (ret <= 0xFF) ? (ret == last) : (ret == lastw);
                }
                if (!isRET && orHALT)
                {      
                    ret = OpCodes.GetHALTInstruction();
                    isRET = (ret <= 0xFF) ? (ret == last) : (ret == lastw);
                }
            }
        }    
        return isRET;
    }
    
    AddInstructionRESET()
    {
        // program entry code
        OpCodes.EmitInstruction("CLD");
        OpCodes.EmitInstruction("LDX", 0xFF);
        OpCodes.EmitInstruction("TXS");
    }
    AddInstructionENTER()
    {
        // method entry code
    }
    AddInstructionRET(uint bytesToPop)
    {
        uint iCurrent = Types.GetCurrentMethod();
        string name = Symbols.GetFunctionName(iCurrent);
        
        uint retw = OpCodes.GetRETInstruction();
        bool addNOP;
        if (name.EndsWith(".Hopper")) // TODO : only allow in 'program'
        {
            // this means we are exiting Hopper()
            retw = OpCodes.GetHALTInstruction();
        }
        else if (name.EndsWith(".IRQ")) // TODO : only allow in 'program'
        {
            retw = OpCodes.GetRETIInstruction();
        }
        else if (name.EndsWith(".NMI")) // TODO : only allow in 'program'
        {
            retw = OpCodes.GetRETNInstruction();
        }
        
        currentStream.Append(byte(retw & 0xFF));
        retw = retw >> 8;
        if (retw != 0)
        {
            currentStream.Append(byte(retw & 0xFF));
        }
    }
    string Disassemble(uint address, byte instruction, uint operand)
    {
        string disassembly;
        disassembly += "0x" + address.ToHexString(4);
        disassembly += " ";
        
        disassembly +=  " 0x" + instruction.ToHexString(2);
        disassembly += " ";
        
        uint length = GetInstructionLength(instruction);
        string operandString = "         "; 
        if (length == 2)
        {
            operandString = "0x" + operand.ToHexString(2) + "     "; 
        }
        else if (length >= 3)
        {
            
            operandString = "0x" + (operand & 0xFF).ToHexString(2) + " 0x" + (operand >> 8).ToHexString(2); 
        }
        disassembly += operandString;
        disassembly += "  ";
        string name = OpCodes.GetName(instruction);
        AddressingModes addressingMode = OpCodes.GetAddressingMode(instruction);
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            switch (addressingMode)
            {
                case AddressingModes.Accumulator:       { disassembly += (name + " A"); }
                case AddressingModes.Implied:           { disassembly += name; }
                case AddressingModes.Immediate:         { disassembly += (name + " #0x" + operand.ToHexString(2)); }
                case AddressingModes.Absolute:          { disassembly += (name + " 0x" + operand.ToHexString(4)); }
                case AddressingModes.AbsoluteX:         { disassembly += (name + " 0x" + operand.ToHexString(4) + ",X"); }
                case AddressingModes.AbsoluteY:         { disassembly += (name + " 0x" + operand.ToHexString(4) + ",Y"); }
                case AddressingModes.AbsoluteIndirect:  { disassembly += (name + " [0x" + operand.ToHexString(4) + "]"); }
                case AddressingModes.AbsoluteIndirectX: { disassembly += (name + " [0x" + operand.ToHexString(4) + ",X]"); }
                case AddressingModes.ZeroPage:          { disassembly += (name + " 0x" + operand.ToHexString(2)); }
                case AddressingModes.ZeroPageX:         { disassembly += (name + " 0x" + operand.ToHexString(2) + ",X"); }
                case AddressingModes.ZeroPageY:         { disassembly += (name + " 0x" + operand.ToHexString(2) + ",Y"); }
                case AddressingModes.ZeroPageIndirect:  { disassembly += (name + " [0x" + operand.ToHexString(2) +"]"); }
                case AddressingModes.XIndexedZeroPage:  { disassembly += (name + " [0x" + operand.ToHexString(2) +",X]"); }
                case AddressingModes.YIndexedZeroPage:  { disassembly += (name + " [0x" + operand.ToHexString(2) +"],Y"); }
                
                case AddressingModes.Relative:  
                { 
                    int ioperand = int(operand);
                    if (ioperand > 127)
                    {
                        ioperand = ioperand - 256; // 0xFF -> -1
                    }
                    long target = long(address) + length + ioperand;
                    disassembly += (name + " 0x" + target.ToHexString(4) + " (" + (ioperand < 0 ? "" : "+") + ioperand.ToString() + ")"); 
                }
                case AddressingModes.ZeroPageRelative:
                {
                    int ioperand = int(operand >> 8);
                    if (ioperand > 127)
                    {
                        ioperand = ioperand - 256; // 0xFF -> -1
                    }
                    long target = long(address) + length + ioperand;
                    disassembly += (name + " 0x" + (operand & 0xFF).ToHexString(2) +", 0x" + target.ToHexString(4) + " (" + (ioperand < 0 ? "" : "+") + ioperand.ToString() + ")"); 
                }
                
                default: { disassembly += name; }
            }
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            Die(0x0A);
        }
        return disassembly;
    }
                    
    uint GetInstructionLength(byte instruction)
    {
        uint length;
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            length = OpCodes.GetInstructionLength(instruction);
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            Die(0x0A); // need to consider multiple byte instructions
        }
        if (length == 0)
        {
            Die(0x0B);
        }
        return length;
    }
    
    PatchJump(uint jumpAddress, uint jumpToAddress)
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            byte braInstruction = GetBInstruction("");
            byte jmpInstruction = GetJMPInstruction();
            int offset = int(jumpToAddress) - int(jumpAddress) - 2;
            
            bool testJMP = false; //((currentStream[jumpAddress+0] == braInstruction) || (currentStream[jumpAddress+0] == jmpInstruction));
            
            if (testJMP || (offset < -128) || (offset > 127))
            {
                // long jump
                if ((currentStream[jumpAddress+0] != braInstruction) &&
                    (currentStream[jumpAddress+0] != jmpInstruction))
                {
                    Parser.Error("jump target exceeds 6502 relative limit (" + offset.ToString() + ")");
                    return;
                }
                currentStream.SetItem(jumpAddress+0, GetJMPInstruction());
                currentStream.SetItem(jumpAddress+1, jumpToAddress.GetByte(0));
                currentStream.SetItem(jumpAddress+2, jumpToAddress.GetByte(1));
            }
            else
            {
                // short jump
                if (currentStream[jumpAddress+0] == jmpInstruction)
                {
                    currentStream.SetItem(jumpAddress+0, GetBInstruction(""));
                }
                currentStream.SetItem(jumpAddress+1, offset.GetByte(0));
                currentStream.SetItem(jumpAddress+2, GetNOPInstruction());
            }
        }
        else
        {
            currentStream.SetItem(jumpAddress+1, jumpToAddress.GetByte(0));
            currentStream.SetItem(jumpAddress+2, jumpToAddress.GetByte(1));
        }
    }
    
    AddInstructionJ()
    {
        AddInstructionJ(0x0000); // placeholder address
    }
    AddInstructionJ(uint jumpToAddress)
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            uint jumpAddress = NextAddress;
            int offset = int(jumpToAddress) - int(jumpAddress) - 2;
            
            bool testJMP = false;
            
            if (testJMP || (offset < -128) || (offset > 127))
            {
                // long jump
                currentStream.Append(GetJMPInstruction());
                currentStream.Append(byte(jumpToAddress & 0xFF));
                currentStream.Append(byte(jumpToAddress >> 8));
            }
            else
            {
                // short jump
                currentStream.Append(GetBInstruction(""));
                currentStream.Append(offset.GetByte(0));
                currentStream.Append(GetNOPInstruction());
            }
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            currentStream.Append(GetJMPInstruction());
            // placeholder address
            currentStream.Append(byte(jumpToAddress & 0xFF));
            currentStream.Append(byte(jumpToAddress >> 8));
        }
    }
    AddInstructionJZ()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            currentStream.Append(GetBInstruction("Z"));
            // placeholder address
            currentStream.Append(0x00);
            currentStream.Append(GetNOPInstruction());
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            currentStream.Append(GetJPInstruction("Z"));
            // placeholder address
            currentStream.Append(0x00);
            currentStream.Append(0x00);
        }
    }
    AddInstructionJNZ()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            currentStream.Append(GetBInstruction("NZ"));
            // placeholder address
            currentStream.Append(0x00);
            currentStream.Append(GetNOPInstruction());
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            currentStream.Append(GetJPInstruction("NZ"));
            // placeholder address
            currentStream.Append(0x00);
            currentStream.Append(0x00);
        }
    }
    AddInstructionCALL(uint iOverload)
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            currentStream.Append(GetJSRInstruction());
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            currentStream.Append(GetCALLInstruction());
        }
        // unresolved method index for now
        currentStream.Append(byte(iOverload & 0xFF));
        currentStream.Append(byte(iOverload >> 8));
    }
    
    AddInstructionCMP(char register, byte operand)
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            switch (register)
            {
                case 'A':
                {
                    OpCodes.EmitInstruction("CMP", operand);
                }
                case 'X':
                {
                    OpCodes.EmitInstruction("CPX", operand);
                }
                case 'Y':
                {
                    OpCodes.EmitInstruction("CPY", operand);
                }
                default:
                {
                    IE();
                }
            }
            
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            NI();
        }
    }
    
}
