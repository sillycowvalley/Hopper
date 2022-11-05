unit CodeStream
{
    uses "/Source/Compiler/CodeGen/Instructions"
    uses "/Source/Compiler/CodeGen/Block"
    uses "/Source/Compiler/Symbols"
    
    
    <string,string> debugInfo;
    <string,bool> debugInfoLineUsed;
    <byte> currentStream;
    <byte> constantStream;
    uint lastInstruction;
    
    bool checkedBuild;
    
    bool CheckedBuild 
    { 
        get { return checkedBuild; }
        set { checkedBuild = value; }
    }
    
    byte IntToByte(int offset)
    {
        if ((offset < -128) || (offset > 127))
        {
            Die(0x0B);
        }
        if (offset < 0)
        {
            offset = 256 + offset; // -1 -> 255
        }
        byte result = byte(offset);
        return result;
    }
    uint IntToUInt(int offset)
    {
        long loffset = offset;
        if (loffset < 0)
        {
            loffset = 65536 + offset; // -1 -> 0xFFFF
        }
        uint result = uint(loffset);
        return result;    
    }
    
    New()
    {
        currentStream.Clear();
        lastInstruction = 0; // not really ..
    }
    New(<byte> starterStream)
    {
        currentStream = starterStream;
        lastInstruction = 0; // not really ..
    }
    <byte> CurrentStream { get { return currentStream; } }
    <string,string> DebugInfo { get { return debugInfo; } }
    ClearDebugInfo()
    {
        debugInfo.Clear();
        debugInfoLineUsed.Clear();
    }
    
    Instruction GetLastInstruction()
    { 
        Instruction last = Instruction.NOP;
        if (lastInstruction < currentStream.Length)
        {
            byte instr = currentStream[lastInstruction];
            last = Instruction(instr); 
        }
        return last;
    }
    
    uint NextAddress { get { return currentStream.Length; } }
        
    AppendCode(<byte> code)
    {
        foreach (var b in code)
        {
            currentStream.Append(b);        
        }
    }
    
    uint AppendConstant(<byte> data)
    {
        uint constantAddress;
        loop
        {
            uint length = constantStream.Length;
            uint candidateLength = data.Length;
            uint iStart = 0;
            bool found = false;
            loop
            {
                if (iStart + candidateLength > length)
                {
                    break;
                }
                bool match = true;   
                for (uint i = 0; i < candidateLength; i++)
                {
                    if (data[i] != constantStream[iStart+i])
                    {
                        match = false;
                        break;
                    }
                }
                if (match)
                {
                    found = true;
                    constantAddress = iStart;
                    break;    
                }
                iStart++;
            }
            if (found)
            {
                break;
            }
            // not found
            constantAddress = constantStream.Length;
            foreach (var b in data)
            {
                constantStream.Append(b);
            }
            break;
        }
        return constantAddress;
    }
    
    uint CreateFloatConstant(float value)
    {
        <byte> bytes = value.ToBytes();
        return AppendConstant(bytes);
    }
    uint CreateLongConstant(long value)
    {
        <byte> bytes = value.ToBytes();
        return AppendConstant(bytes);
    }
    uint CreateStringConstant(string value)
    {
        <byte> bytes;
        foreach (var c in value)
        {
            bytes.Append(byte(c));
        }
        return AppendConstant(bytes);
    }
    <byte> GetConstantStream()
    {
        return constantStream;
    }
    
    
    PopTail(uint pops)
    {
        loop
        {
            uint iLast = currentStream.Length - 1;
            currentStream.Remove(iLast);
            pops--;
            if (pops == 0)
            {
                break;
            }
        }
    }
    
    PatchJump(uint jumpAddress, uint jumpToAddress)
    {
        byte jumpInstr = currentStream[jumpAddress];
        Instruction jumpInstruction = Instruction(jumpInstr);
        bool isLong; 
        switch (jumpInstruction)
        {
            case Instruction.JB:
            {
            }
            case Instruction.JZB:
            {
            }
            case Instruction.JNZB:
            {
            }
            case Instruction.JW:
            {
                isLong = true;
            }
            case Instruction.JZW:
            {
                isLong = true;
            }
            case Instruction.JNZW:
            {
                isLong = true;
            }
            default:
            {
                Die(0x0B); // what's this?
            }
        }
        int offset = int(jumpToAddress) - int(jumpAddress);
        if (isLong)
        {
            uint op = IntToUInt(offset);
            
            uint lsb = op & 0xFF;
            uint msb = op >> 8;
            currentStream.SetItem(jumpAddress+1, byte(lsb));
            currentStream.SetItem(jumpAddress+2, byte(msb));
        }
        else
        {
            byte op = IntToByte(offset);
            currentStream.SetItem(jumpAddress+1, op);
        }
    }
    
    AddInstructionSysCall0(string sysCallUnit, string sysCallMethod)
    {
        byte iSysCall;
        string name = sysCallUnit + '.' + sysCallMethod;
        if (!TryParse(name, ref iSysCall))
        {
            PrintLn("'" + name + "' not found");
            Die(3); // key not found
        }
        CodeStream.AddInstruction(Instruction.SYSCALL0, iSysCall);
    }    
    
    AddInstructionJump(Instruction jumpInstruction, uint jumpToAddress)
    {
        uint jumpAddress = NextAddress;
        int offset = int(jumpToAddress) - int(jumpAddress);
        if ((offset >= -128) && (offset <= 127))
        {
            byte op = IntToByte(offset);
            switch (jumpInstruction)
            {
                case Instruction.JW:
                {
                    jumpInstruction = Instruction.JB;
                }
                case Instruction.JZW:
                {
                    jumpInstruction = Instruction.JZB;
                }
                case Instruction.JNZW:
                {
                    jumpInstruction = Instruction.JNZB;
                }
            }
            AddInstruction(jumpInstruction, op);        
        }
        else
        {
            uint op = IntToUInt(offset);
            switch (jumpInstruction)
            {
                case Instruction.JB:
                {
                    jumpInstruction = Instruction.JW;
                }
                case Instruction.JZB:
                {
                    jumpInstruction = Instruction.JZW;
                }
                case Instruction.JNZB:
                {
                    jumpInstruction = Instruction.JNZW;
                }
            }
            AddInstruction(jumpInstruction, op);   
        }
    }
    
    
    AddInstruction(Instruction instruction)
    {
        byte instr = byte(instruction);
        currentStream.Append(instr);
        lastInstruction = currentStream.Length-1;
    }
    AddInstruction(Instruction instruction, byte operand)
    {
        AddInstruction(instruction);
        currentStream.Append(operand);
    }
    AddInstruction(Instruction instruction, uint operand)
    {
        AddInstruction(instruction);
        uint lsb = operand & 0xFF;
        currentStream.Append(byte(lsb));
        uint msb = operand >> 8;
        currentStream.Append(byte(msb));
    }
    AddInstructionPUSHI(uint operand)
    {
        if (operand == 0)
        {
            CodeStream.AddInstruction(Instruction.PUSHI0);
        }
        else if (operand == 1)
        {
            CodeStream.AddInstruction(Instruction.PUSHI1);
        }
        else if (operand < 256)
        {
            CodeStream.AddInstruction(Instruction.PUSHIB, byte(operand));
        }
        else
        {
            CodeStream.AddInstruction(Instruction.PUSHIW, operand);
        }
    }
    
    AddInstructionPushVariable(string variableName)
    {
        string fullName;
        string variableType = Types.GetTypeString(variableName, true, ref fullName);
            
        if (Symbols.GlobalMemberExists(fullName))
        {
            uint globalAddress = Symbols.GetGlobalAddress(fullName);
            if (globalAddress < 256)
            {
                CodeStream.AddInstruction(Instruction.PUSHGLOBALB, byte(globalAddress));
            }
            else
            {
                CodeStream.AddInstruction(Instruction.PUSHGLOBALW, globalAddress);
            }       
        }
        else
        {
            bool isRef;
            int offset = Block.GetOffset(variableName, ref isRef);
            if ((offset > -129) && (offset < 128))
            {
                byte operand =  CodeStream.IntToByte(offset);
                if (isRef)
                {
                    CodeStream.AddInstruction(Instruction.PUSHRELB, operand);
                }
                else
                {
                    CodeStream.AddInstruction(Instruction.PUSHLOCALB, operand);
                }
            }
            else
            {
                uint operand =  CodeStream.IntToUInt(offset);  
                if (isRef)
                {
                    CodeStream.AddInstruction(Instruction.PUSHRELW, operand);
                }
                else
                {
                    CodeStream.AddInstruction(Instruction.PUSHLOCALW, operand);
                }
            }
        }
    }
    AddInstructionPopVariable(string variableType, string variableName)
    {
        if (!IsValueType(variableType))
        {
            // what follows is a pop of a reference into a variable - should we make a copy?
            CodeStream.AddInstruction(Instruction.COPYNEXTPOP);
        }
        string fullName;
        string variableType2 = Types.GetTypeString(variableName, true, ref fullName);
        
        if (Symbols.GlobalMemberExists(fullName))
        {
            uint globalAddress = Symbols.GetGlobalAddress(fullName);
            if (globalAddress < 256)
            {
                CodeStream.AddInstruction(Instruction.POPGLOBALB, byte(globalAddress));
            }
            else
            {
                CodeStream.AddInstruction(Instruction.POPGLOBALW, globalAddress);
            }       
        }
        else
        {
            bool isRef;
            int offset = Block.GetOffset(variableName, ref isRef);
            if ((offset > -129) && (offset < 128))
            {
                byte operand =  CodeStream.IntToByte(offset);
                if (isRef)
                {
                    CodeStream.AddInstruction(Instruction.POPRELB, operand);
                }
                else
                {
                    CodeStream.AddInstruction(Instruction.POPLOCALB, operand);
                }
            }
            else
            {
                uint operand =  CodeStream.IntToUInt(offset);  
                if (isRef)
                {
                    CodeStream.AddInstruction(Instruction.POPRELW, operand);
                }
                else
                {
                    CodeStream.AddInstruction(Instruction.POPLOCALW, operand);
                }
            }
        }
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
    
}
