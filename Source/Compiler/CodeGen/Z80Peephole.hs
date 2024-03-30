unit Peephole
{
    <uint> instructionAddresses;
    
    bool isDisabled;
    
    bool Disabled { get { return isDisabled; } set { isDisabled = value; } }
    
    AddInstruction(uint address)
    {
        instructionAddresses.Append(address);
    }
    Reset()
    {
        instructionAddresses.Clear();
    }
    Optimize(<byte> output)
    {
        //return; // peephole off
        loop
        {
            uint instructionCount = instructionAddresses.Count;
            /*
            if (instructionCount > 1)
            {
                uint iIndex0 = instructionAddresses[instructionCount-1];
                OpCode opCode0 = OpCode(output[iIndex0]);
                if (opCode0 == OpCode.XOR_A_A)
                {
                    Print("XOR A, A ");
                    if (instructionCount > 2)
                    {
                        uint iIndex1 = instructionAddresses[instructionCount-2];
                        OpCode opCode1 = OpCode(output[iIndex1]);
                        if (opCode1 == OpCode.PUSH_DE)
                        {
                            Print(" after PUSH DE");
                        }
                    }
                    PrintLn(" : " + instructionCount.ToString());
                }
            }
            */
            
            if (instructionCount >= 2)
            {
                if (SymmetricPushPop(output))  // NOP folding
                {
                    continue;
                }
                if (PushAfterClearA(output))   // expose the PUSH to further folding
                {
                    continue;
                }
            }  
            if (instructionCount >= 3)
            {
                if (PushPopCompare(output))    // for example, PUSH DE, POP HL, CP A, L -> CP A, E
                {
                    continue;
                }
            }
            break;
        }
    }
    
    bool SymmetricPushPop(<byte> output)
    {
        // PUSH ss, POP ss -> removed
        uint instructionCount = instructionAddresses.Count;
        uint iIndex1 = instructionAddresses[instructionCount-2];
        uint iIndex0 = instructionAddresses[instructionCount-1];
        OpCode opCode1 = OpCode(output[iIndex1]);
        OpCode opCode0 = OpCode(output[iIndex0]);
        if (   ((opCode1 == OpCode.PUSH_HL) && (opCode0 == OpCode.POP_HL))
            || ((opCode1 == OpCode.PUSH_DE) && (opCode0 == OpCode.POP_DE))
            || ((opCode1 == OpCode.PUSH_BC) && (opCode0 == OpCode.POP_BC))
           )
        {
            output.Remove(iIndex0);
            output.Remove(iIndex1);
            instructionAddresses.Remove(instructionCount-1);
            instructionAddresses.Remove(instructionCount-2);
            return true;
        }
        return false;
    }
    
    bool PushAfterClearA(<byte> output)
    {
        // PUSH ss, XOR A, A -> XOR A, A, PUSH ss
        uint instructionCount = instructionAddresses.Count;
        uint iIndex0 = instructionAddresses[instructionCount-1];
        OpCode opCode0 = OpCode(output[iIndex0]);
        if (opCode0 == OpCode.XOR_A_A) // LDA A, 0
        {
            uint iIndex1 = instructionAddresses[instructionCount-2];
            OpCode opCode1 = OpCode(output[iIndex1]);
            if ((opCode1 == OpCode.PUSH_HL) || (opCode1 == OpCode.PUSH_DE) || (opCode1 == OpCode.PUSH_BC))
            {
                output.SetItem(iIndex1, byte(opCode0));
                output.SetItem(iIndex0, byte(opCode1));
                return true;
            }
        }
        return false;
    }
    bool PushPopCompare(<byte> output)    
    {
        // PUSH DE, POP HL, CP A, L -> CP A, E
        uint instructionCount = instructionAddresses.Count;
        uint address0 = instructionCount-1;
        uint iIndex0 = instructionAddresses[address0];
        OpCode opCode0 = OpCode(output[iIndex0]);
        uint address1 = instructionCount-2;
        uint iIndex1 = instructionAddresses[address1];
        OpCode opCode1 = OpCode(output[iIndex1]);
        uint address2 = instructionCount-3;
        uint iIndex2 = instructionAddresses[address2];
        OpCode opCode2 = OpCode(output[iIndex2]);
        OpCode opCodeNew0 = OpCode.NOP;
        
        if ((opCode1 == OpCode.POP_HL) && (opCode0 == OpCode.CP_A_L))
        {
            if (opCode2 == OpCode.PUSH_DE)
            {
                opCodeNew0 = OpCode.CP_A_E;
            }
            else if (opCode2 == OpCode.PUSH_BC)
            {
                opCodeNew0 = OpCode.CP_A_C;
            }
        }       
        else if ((opCode1 == OpCode.POP_DE) && (opCode0 == OpCode.CP_A_E))
        {
            if (opCode2 == OpCode.PUSH_HL)
            {
                opCodeNew0 = OpCode.CP_A_L;
            }
            else if (opCode2 == OpCode.PUSH_BC)
            {
                opCodeNew0 = OpCode.CP_A_C;
            }
        }
        else if ((opCode1 == OpCode.POP_BC) && (opCode0 == OpCode.CP_A_C))
        {
            if (opCode2 == OpCode.PUSH_DE)
            {
                opCodeNew0 = OpCode.CP_A_E;
            }
            else if (opCode2 == OpCode.PUSH_HL)
            {
                opCodeNew0 = OpCode.CP_A_L;
            }
        }
        if (opCodeNew0 != OpCode.NOP)
        {
            output.SetItem(iIndex0, byte(opCodeNew0));     
            instructionAddresses.SetItem(address0, iIndex0-2); // 2 single byte instructions removed before this one
            output.Remove(iIndex1);
            output.Remove(iIndex2);
            instructionAddresses.Remove(address1);
            instructionAddresses.Remove(address2);
            
            return true;
        }
        return false;
    }
    
}
