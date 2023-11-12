unit CodeStream
{
    uses "/Source/Compiler/CodeGen/Instructions"
    uses "/Source/Compiler/CodeGen/Block"
    uses "/Source/Compiler/Symbols"
    uses "/Source/Compiler/CodeGen/Peephole"
    
    
    <string,string> debugInfo;
    <string,bool> debugInfoLineUsed;
    <byte> currentStream;
    <byte> constantStream;
    
    bool checkedBuild;
    bool shortCallsDefined;
    bool portableDefined;
    bool h6053Defined;
    
    bool CheckedBuild 
    { 
        get { return checkedBuild; }
        set { checkedBuild = value; }
    }
    bool IsShortCalls { get { return shortCallsDefined; } }
    bool IsPortable   { get { return portableDefined; } }
    bool Target6502   { get { return h6053Defined; } }
    
    InitializeSymbolShortcuts()
    {
        shortCallsDefined = DefineExists("SHORTCALLS");
        portableDefined = DefineExists("PORTABLE");
        h6053Defined = DefineExists("H6502");
    }
    bool InUse { get { return currentStream.Length > 0; } } 
    
    Instruction GetLastInstruction()
    { 
        Instruction last = Instruction.NOP;
        if (LastInstructionIndex < currentStream.Length)
        {
            byte instr = currentStream[LastInstructionIndex];
            last = Instruction(instr); 
        }
        return last;
    }
    
    uint NextAddress 
    { 
        get 
        { 
            return currentStream.Length;
        } 
    }
        
    AppendCode(<byte> code)
    {
        foreach (var b in code)
        {
            currentStream.Append(b);        
        }
        UpdatePeepholeBoundary(currentStream.Length);
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
    
    New()
    {
        currentStream.Clear();
        Peephole.Initialize();
    }
    New(<byte> starterStream)
    {
        currentStream = starterStream;
        Peephole.Initialize();
    }
    GetCurrentStream(ref <byte> rCurrentStream)
    {
        rCurrentStream = currentStream;
    }
    <byte> CurrentStream { get { return currentStream; } }
    <string,string> DebugInfo { get { return debugInfo; } }
    ClearDebugInfo()
    {
        debugInfo.Clear();
        debugInfoLineUsed.Clear();
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
        Instruction shortInstruction;
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
                shortInstruction = Instruction.JB;
            }
            case Instruction.JZW:
            {
                isLong = true;
                shortInstruction = Instruction.JZB;
            }
            case Instruction.JNZW:
            {
                isLong = true;
                shortInstruction = Instruction.JNZB;
            }
            default:
            {
                uint inst = uint(jumpInstruction);
                PrintLn("jumpInstruction=" + inst.ToHexString(2));
                Die(0x0B); // what's this?
            }
        }
        
        int offset = int(jumpToAddress) - int(jumpAddress);
        if (isLong)
        {
            uint op = Types.IntToUInt(offset);
            uint lsb = op & 0xFF;
            uint msb = op >> 8;
            if ((shortInstruction == Instruction.JB) && (offset >= 0) && (offset <= 127))
            {
                uint phb = PeepholeBoundary;
                if (jumpAddress > phb)
                {
                  UpdatePeepholeBoundary(currentStream.Length);
                }
                currentStream.SetItem(jumpAddress+0, byte(shortInstruction));
                currentStream.SetItem(jumpAddress+1, byte(lsb));
                currentStream.SetItem(jumpAddress+2, byte(Instruction.NOP));
            }
            else
            {
                currentStream.SetItem(jumpAddress+1, byte(lsb));
                currentStream.SetItem(jumpAddress+2, byte(msb));
            }
        }
        else
        {
            byte op = IntToByte(offset);
            currentStream.SetItem(jumpAddress+1, op);
        }
        
    }
    
    bool TryUserSysCall(string name)
    {
        bool userSupplied = false;
        // Are we targetting an 8 bit platform?
        // Is there a user supplied alternative to the SysCall with only one overload?
        //  (we're not checking arguments or return type : shooting from the hip ..)
        uint fIndex;
        if (CodeStream.Target6502 || CodeStream.IsPortable)
        {
            if (GetFunctionIndex(name, ref fIndex))
            {
                <uint> iOverloads = GetFunctionOverloads(fIndex);
                if (iOverloads.Length == 1)
                {
                    uint iOverload = iOverloads[0];
                    if (!IsSysCall(iOverload))
                    {
                        Symbols.OverloadToCompile(iOverload); // User supplied SysCall as Hopper source

                        if (CodeStream.Target6502)
                        {
                            if (iOverload <= 0x3FFF)
                            {
                                uint hOverload = 0xC000 | iOverload;
                                CodeStream.AddInstruction(Instruction.CALLW, hOverload);
                            }
                            else
                            {
                                Parser.Error("H6502 has a limit of 16383 for function indices, (was '" + iOverload.ToString() + "')");
                            }
                        }
                        else if (CodeStream.IsShortCalls && (iOverload < 256))
                        {
                            CodeStream.AddInstruction(Instruction.CALLB, byte(iOverload));
                        }
                        else
                        {
                            CodeStream.AddInstruction(Instruction.CALLW, iOverload);
                        }
                        userSupplied = true;
                    }
                }
            }
            //PrintLn();
        }
        return userSupplied;
    }
    
    AddInstructionSysCall0(string sysCallUnit, string sysCallMethod)
    {
        loop
        {
            byte iSysCall;
            string name = sysCallUnit + '.' + sysCallMethod;
            if (TryUserSysCall(name))
            {
                break;
            }
            if (!TryParseSysCall(name, ref iSysCall))
            {
                PrintLn("'" + name + "' not found");
                Die(0x03); // key not found
            }
            CodeStream.AddInstruction(Instruction.SYSCALL0, iSysCall);
            break;
        }
    }
    AddInstructionJump(Instruction jumpInstruction)
    {
        // before jump (since this placeholder patch location is locked in already)
        UpdatePeepholeBoundary(currentStream.Length);
        
        switch (jumpInstruction)
        {
            case Instruction.JW:
            case Instruction.JZW:
            case Instruction.JNZW:
            {
                AddInstruction(jumpInstruction, uint(0)); // place holder
            }
            case Instruction.JB:
            case Instruction.JZB:
            case Instruction.JNZB:
            {
                AddInstruction(jumpInstruction, byte(0)); // place holder
            }
            default:
            {
                Die(0x0B); // what's this?
            }
        }
        
    }    
    AddInstructionJumpOffset(Instruction jumpInstruction, byte offset)
    {
        AddInstruction(jumpInstruction, offset);
        UpdatePeepholeBoundary(currentStream.Length);
    }
    AddInstructionJump(Instruction jumpInstruction, uint jumpToAddress)
    {
        if ((jumpInstruction == Instruction.JIXW) || (jumpInstruction == Instruction.JIXB))
        {
            AddInstruction(jumpInstruction, jumpToAddress); // operand is not really an address, always a uint
        }
        else
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
                uint op = Types.IntToUInt(offset);
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
        UpdatePeepholeBoundary(currentStream.Length);
    }
    
    
    internalAddInstruction(Instruction instruction)
    {
        byte instr = byte(instruction);
        currentStream.Append(instr);
        LastInstructionIndex = currentStream.Length-1;
    }
    
    AddInstruction(Instruction instruction)
    {
        switch (instruction)
        {
            case Instruction.JB:
            case Instruction.JZB:
            case Instruction.JNZB:
            case Instruction.JW:
            case Instruction.JZW:
            case Instruction.JNZW:
            case Instruction.JIXB:
            case Instruction.JIXW:
            {
                Die(0x0B); // illegal to not use th Jump-specific AddInstructions (to update peephole boundary)
            }
        }
        internalAddInstruction(instruction);
        PeepholeOptimize(ref currentStream);
    }
    AddInstruction(Instruction instruction, byte operand)
    {
        internalAddInstruction(instruction);
        currentStream.Append(operand);
        PeepholeOptimize(ref currentStream);
    }
    AddInstruction(Instruction instruction, uint operand)
    {
        internalAddInstruction(instruction);
        uint lsb = operand & 0xFF;
        currentStream.Append(byte(lsb));
        uint msb = operand >> 8;
        currentStream.Append(byte(msb));
        PeepholeOptimize(ref currentStream);
    }
    AddInstructionPUSHI(uint operand)
    {
        if (operand == 0)
        {
            CodeStream.internalAddInstruction(Instruction.PUSHI0);
            PeepholeOptimize(ref currentStream);
        }
        else if (operand == 1)
        {
            CodeStream.internalAddInstruction(Instruction.PUSHI1);
            PeepholeOptimize(ref currentStream);
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
                uint operand = Types.IntToUInt(offset);
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
            CodeStream.internalAddInstruction(Instruction.COPYNEXTPOP);
            PeepholeOptimize(ref currentStream);
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
                uint operand = Types.IntToUInt(offset);
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
    AddString(string value)
    {
        if (value.Length == 0)
        {
            CodeStream.AddInstructionSysCall0("String", "New");
        }
        else if (value.Length == 1)
        {
            CodeStream.AddInstructionPUSHI(byte(value[0]));
            byte iSysCall;
            if (!TryParseSysCall("String.NewFromConstant", ref iSysCall))
            {
                Die(0x03); // key not found
            }
            CodeStream.AddInstruction(Instruction.SYSCALL1, iSysCall);
        }
        else if (value.Length == 2)
        {
            CodeStream.AddInstructionPUSHI(byte(value[0]) + (byte(value[1]) << 8));
            byte iSysCall;
            if (!TryParseSysCall("String.NewFromConstant", ref iSysCall))
            {
                Die(0x03); // key not found
            }
            CodeStream.AddInstruction(Instruction.SYSCALL1, iSysCall);
        }
        else
        {
            uint constantAddress = CodeStream.CreateStringConstant(value);
            CodeStream.AddInstructionPUSHI(constantAddress);
            CodeStream.AddInstructionPUSHI(value.Length);
            CodeStream.AddInstructionSysCall0("String", "NewFromConstant");
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
