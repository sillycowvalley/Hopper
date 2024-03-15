unit AsmStream
{
    #define ASMSTREAM
    
    uses "CodeGen/OpCodes"
    
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
    
    PatchJump(uint jumpAddress, uint jumpToAddress)
    {
        uint lsb = jumpToAddress & 0xFF;
        uint msb = jumpToAddress >> 8;
        currentStream.SetItem(jumpAddress+1, byte(lsb));
        currentStream.SetItem(jumpAddress+2, byte(msb));
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
    
    AddInstructionENTER()
    {
        // method entry code
    }
    AddInstructionRET(uint bytesToPop)
    {
        if (bytesToPop > 0)
        {
            // this means we are exiting Hopper()
            currentStream.Append(OpCodes.GetHALTInstruction());
        }
        else
        {
            currentStream.Append(OpCodes.GetRETInstruction());
        }
    }
    AddInstructionJ()
    {
        AddInstructionJ(0x0000); // placeholder address
    }
    AddInstructionJ(uint address)
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            currentStream.Append(GetJMPInstruction());
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            currentStream.Append(GetJMPInstruction());
        }
        // placeholder address
        currentStream.Append(byte(address & 0xFF));
        currentStream.Append(byte(address > 8));
    }
    AddInstructionJZ()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            currentStream.Append(GetBInstruction("NZ"));
            currentStream.Append(0x03); // +3
            currentStream.Append(GetJMPInstruction());
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            currentStream.Append(GetJPInstruction("Z"));
        }
        // placeholder address
        currentStream.Append(0x00);
        currentStream.Append(0x00);
    }
    AddInstructionJNZ()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            currentStream.Append(GetBInstruction("Z"));
            currentStream.Append(0x03); // +3
            currentStream.Append(GetJMPInstruction());
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            currentStream.Append(GetJPInstruction("NZ"));
        }
        // placeholder address
        currentStream.Append(0x00);
        currentStream.Append(0x00);
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
        currentStream.Append(byte(iOverload > 8));
    }
    
}
