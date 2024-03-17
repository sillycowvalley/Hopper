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
        
        byte length = GetInstructionLength(instruction);
        string operandString = "         "; 
        if (length == 2)
        {
            operandString = "0x" + operand.ToHexString(2) + "     "; 
        }
        else if (length == 3)
        {
            
            operandString = "0x" + (operand & 0xFF).ToHexString(2) + " 0x" + (operand >> 8).ToHexString(2); 
        }
        disassembly += operandString;
        disassembly += "  ";
        string name = OpCodes.GetName(instruction);
        AddressingModes addressingMode = OpCodes.GetAddressingMode(instruction);
        
        switch (addressingMode)
        {
            case AddressingModes.Accumulator:      { disassembly += (name + " A"); }
            case AddressingModes.Implied:           { disassembly += name; }
            case AddressingModes.Immediate:         { disassembly += (name + " #0x" + operand.ToHexString(2)); }
            case AddressingModes.Absolute:          { disassembly += (name + " 0x" + operand.ToHexString(4)); }
            case AddressingModes.AbsoluteX:         { disassembly += (name + " 0x" + operand.ToHexString(4) + ",X"); }
            case AddressingModes.AbsoluteY:         { disassembly += (name + " 0x" + operand.ToHexString(4) + ",Y"); }
            case AddressingModes.AbsoluteIndirect:  { disassembly += (name + " (0x" + operand.ToHexString(4) + ")"); }
            case AddressingModes.AbsoluteIndirectX: { disassembly += (name + " (0x" + operand.ToHexString(4) + ",X)"); }
            case AddressingModes.ZeroPage:          { disassembly += (name + " 0x" + operand.ToHexString(2)); }
            case AddressingModes.ZeroPageX:         { disassembly += (name + " 0x" + operand.ToHexString(2) + ",X"); }
            case AddressingModes.ZeroPageY:         { disassembly += (name + " 0x" + operand.ToHexString(2) + ",Y"); }
            case AddressingModes.ZeroPageIndirect:  { disassembly += (name + " (0x" + operand.ToHexString(2) +")"); }
            case AddressingModes.XIndexedZeroPage:  { disassembly += (name + " (0x" + operand.ToHexString(2) +",X)"); }
            case AddressingModes.YIndexedZeroPage:  { disassembly += (name + " (0x" + operand.ToHexString(2) +"),Y"); }
            
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
        return disassembly;
    }
                    
    byte GetInstructionLength(byte instruction)
    {
        byte length;
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            // https://llx.com/Neil/a2/opcodes.html
            byte cc  = instruction & 0b11;
            byte bbb = (instruction >> 2) & 0b111;
            switch (cc)
            {
                case 0b01: // group one
                {
                    length = 2;
                    if ((bbb == 0b011) || (bbb == 0b110) || (bbb == 0b111))
                    {
                        length = 3;
                    }
                }
                case 0b10: // group two
                {
                    length = 2;
                    if ((bbb == 0b011) || (bbb == 0b111))
                    {
                        length = 3;
                    }
                    if (bbb == 0b010)
                    {
                        length = 1;
                    }
                }
                case 0b00: // group 3
                {
                    length = 2;
                    if ((bbb == 0b011) || (bbb == 0b111))
                    {
                        length = 3;
                    }
                }
            }
            switch (instruction)
            {
                case 0x20: { length = 3; } // JSR
                
                case 0x00: // BRK
                case 0x40: // RTI
                case 0x60: // RTS
                case 0x08: // PHP
                case 0x28: // PLP
                case 0x48: // PHA
                case 0x68: // PLA
                case 0x88: // DEY
                case 0xA8: // TAY
                case 0xC8: // INX
                case 0xE8: // INY
                case 0x18: // CLC
                case 0x38: // SEC
                case 0x58: // CLI
                case 0x78: // SEI
                case 0x98: // TAY
                case 0xB8: // CLV
                case 0xD8: // CLD
                case 0xF8: // SED
                case 0x8A: // TXA
                case 0x9A: // TXS
                case 0xAA: // TAX
                case 0xBA: // TSX
                case 0xCA: // DEX
                case 0xEA: // NOP
                
                case 0x1A: // INC A
                case 0x3A: // DEC A
                case 0x5A: // PHY
                case 0x7A: // PLY
                case 0xDA: // PHX
                case 0xFA: // PLX
                { length = 1; } 
                    
                case 0x0F:
                case 0x1F:
                case 0x2F:
                case 0x3F:
                case 0x4F:
                case 0x4C: // JMP
                case 0x5F:
                case 0x6C: // JMP
                case 0x6F:
                case 0x7C: // JMP
                case 0x7F:
                case 0x8F:
                case 0x9F:
                case 0xAF:
                case 0xBF:
                case 0xCF:
                case 0xDF:
                case 0xEF:
                case 0xFF:
                { length = 3; }
                
                case 0x07:
                case 0x17:
                case 0x27:
                case 0x37:
                case 0x47:
                case 0x57:
                case 0x67:
                case 0x77:
                case 0x87:
                case 0x97:
                case 0xA7:
                case 0xB7:
                case 0xC7:
                case 0xD7:
                case 0xE7:
                case 0xF7:
                { length = 2; }
            }
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            Die(0x0A); // need to consider multiple byte instructions
        }
        if (length == 0)
        {
            PrintLn("GetInstructionLength: 0x" + instruction.ToHexString(2)); Die(0x0B);
            length = 1;
        }
        //PrintLn("0x" + instruction.ToHexString(2) + " " + length.ToString());
        return length;
    }
    
    PatchJump(uint jumpAddress, uint jumpToAddress)
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            byte braInstruction = GetBInstruction("");
            byte jmpInstruction = GetJMPInstruction();
            int offset = int(jumpToAddress) - int(jumpAddress) - 2;
            
            bool testJMP = false; // ((currentStream[jumpAddress+0] == braInstruction) || (currentStream[jumpAddress+0] == jmpInstruction));
            
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
    
}
