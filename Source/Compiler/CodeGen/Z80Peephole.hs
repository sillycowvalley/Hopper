unit Peephole
{
    // http://z80-heaven.wikidot.com/optimization#toc0
    // https://github.com/Zeda/Z80-Optimized-Routines
    <uint> instructionAddresses;
    
    bool isDisabled;
    
    bool Disabled { get { return isDisabled; } set { isDisabled = value; } }
    
    uint strictSavings;
    uint laxSavings;
    
    uint StrictSavings { get { return strictSavings; } }
    uint LaxSavings { get { return laxSavings; } }
    AddInstruction(uint address)
    {
        instructionAddresses.Append(address);
    }
    Reset()
    {
        instructionAddresses.Clear();
    }
    byte removeInstruction(<byte> output, uint index)
    {
        OpCode opCode = GetOpCode(output, index);
        
        OperandType operandType; byte operandLength; bool signed;
        byte opCodeLength = GetOpCodeLength(output[index]);
        string name = GetOpCodeInfo(opCode, ref operandType, ref operandLength, ref signed);
        byte bytes = opCodeLength + operandLength;
        byte bytesRemoved = bytes;
        loop
        {
            output.Remove(index);
            bytes--;
            if (bytes == 0) { break; }
        }
        return bytesRemoved;
    }
    
    const bool laxRules = false;
    
    Optimize(<byte> output)
    {
        //return; // peephole off
        loop
        {
            uint instructionCount = instructionAddresses.Count;
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
                if (LoadImmediatePushPop(output))  // for example, LD DE, nn, PUSH DE, POP HL -> LD HL, nn
                {
                    continue;
                }
                if (PushLoadPop(output))  // for example, PUSH HL, LD DE nn, POP HL
                {
                    continue;
                }
            }
            if (instructionCount >= 4)
            {
                if (PushPopLoad(output)) // for example: PUSH HL, POP DE, LD (nn), E, LD (nn), D -> LD (nn), L LD (nn), H
                {
                    continue;
                }
                if (PushLoadLoadPop(output))  // for example, PUSH HL, LD E, LD D, POP HL
                {
                    continue;
                }
            }
            if (instructionCount >= 5)
            {
                if (LoadPushPopLoad(output)) // for example: LD E, LD D, PUSH DE, LD E, LD D
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
        OpCode opCode1 = GetOpCode(output, iIndex1);
        OpCode opCode0 = GetOpCode(output, iIndex0);
        if (   ((opCode1 == OpCode.PUSH_HL) && (opCode0 == OpCode.POP_HL))
            || ((opCode1 == OpCode.PUSH_DE) && (opCode0 == OpCode.POP_DE))
            || ((opCode1 == OpCode.PUSH_BC) && (opCode0 == OpCode.POP_BC))
           )
        {
            _ = removeInstruction(output, iIndex0);
            _ = removeInstruction(output, iIndex1);
            instructionAddresses.Remove(instructionCount-1);
            instructionAddresses.Remove(instructionCount-2);
            strictSavings += 2;
            return true;
        }
        return false;
    }
      
    bool PushAfterClearA(<byte> output)
    {
        // PUSH ss, XOR A, A -> XOR A, A, PUSH ss
        //     expose the PUSH to further folding
        uint instructionCount = instructionAddresses.Count;
        uint iIndex0 = instructionAddresses[instructionCount-1];
        OpCode opCode0 = GetOpCode(output, iIndex0);
        if (opCode0 == OpCode.XOR_A_A) // LDA A, 0
        {
            uint iIndex1 = instructionAddresses[instructionCount-2];
            OpCode opCode1 = GetOpCode(output, iIndex1);
            if ((opCode1 == OpCode.PUSH_HL) || (opCode1 == OpCode.PUSH_DE) || (opCode1 == OpCode.PUSH_BC))
            {
                output.SetItem(iIndex1, byte(opCode0));
                output.SetItem(iIndex0, byte(opCode1));
                return true;
            }
        }
        return false;
    }
    bool PushLoadLoadPop(<byte> output)
    {
        // for example, PUSH HL, LD E, LD D, POP HL
        uint instructionCount = instructionAddresses.Count;
        
        uint iIndex0 = instructionCount-1;
        uint address0 = instructionAddresses[iIndex0];
        OpCode opCode0 = GetOpCode(output, address0);
        if (opCode0 == OpCode.POP_HL)
        {
            uint iIndex3 = instructionCount-4;
            uint address3 = instructionAddresses[iIndex3];
            OpCode opCode3 = GetOpCode(output, address3);
            if (opCode3 == OpCode.PUSH_HL)
            {
                uint iIndex2 = instructionCount-3;
                uint address2 = instructionAddresses[iIndex2];
                OpCode opCode2 = GetOpCode(output, address2);
                uint iIndex1 = instructionCount-2;
                uint address1 = instructionAddresses[iIndex1];
                OpCode opCode1 = GetOpCode(output, address1);
                if ((opCode2 == OpCode.LD_E_iIY_d) && (opCode1 == OpCode.LD_D_iIY_d))
                {
                    _ = removeInstruction(output, address0);
                    instructionAddresses.Remove(iIndex0);        
                    byte bytesRemoved = removeInstruction(output, address3);
                    instructionAddresses[iIndex1] = instructionAddresses[iIndex1] - bytesRemoved;
                    instructionAddresses[iIndex2] = instructionAddresses[iIndex2] - bytesRemoved;
                    instructionAddresses.Remove(iIndex3);   
                    strictSavings -= 2;   
                    return true;
                }   
            }
        } 
        return false;
    }
    bool PushLoadPop(<byte> output)
    {
        // for example, PUSH HL, LD DE nn, POP HL
        uint instructionCount = instructionAddresses.Count;
        
        uint iIndex0 = instructionCount-1;
        uint address0 = instructionAddresses[iIndex0];
        OpCode opCode0 = GetOpCode(output, address0);
        if (opCode0 == OpCode.POP_HL)
        {
            uint iIndex2 = instructionCount-3;
            uint address2 = instructionAddresses[iIndex2];
            OpCode opCode2 = GetOpCode(output, address2);
            if (opCode2 == OpCode.PUSH_HL)
            {
                uint iIndex1 = instructionCount-2;
                uint address1 = instructionAddresses[iIndex1];
                OpCode opCode1 = GetOpCode(output, address1);
                if ((opCode1 == OpCode.LD_DE_nn) || (opCode1 == OpCode.LD_BC_nn))
                {
                    _ = removeInstruction(output, address0);
                    instructionAddresses.Remove(iIndex0);        
                    byte bytesRemoved = removeInstruction(output, address2);
                    instructionAddresses[iIndex1] = instructionAddresses[iIndex1] - bytesRemoved;
                    instructionAddresses.Remove(iIndex2);   
                    strictSavings -= 2;     
                    return true;
                }   
            }
        } 
        else if (opCode0 == OpCode.POP_DE)
        {
            uint iIndex2 = instructionCount-3;
            uint address2 = instructionAddresses[iIndex2];
            OpCode opCode2 = GetOpCode(output, address2);
            if (opCode2 == OpCode.PUSH_DE)
            {
                uint iIndex1 = instructionCount-2;
                uint address1 = instructionAddresses[iIndex1];
                OpCode opCode1 = GetOpCode(output, address1);
                if ((opCode1 == OpCode.LD_BC_nn) || (opCode1 == OpCode.LD_IX_nn))
                {
                    _ = removeInstruction(output, address0);
                    instructionAddresses.Remove(iIndex0);        
                    byte bytesRemoved = removeInstruction(output, address2);
                    instructionAddresses[iIndex1] = instructionAddresses[iIndex1] - bytesRemoved;
                    instructionAddresses.Remove(iIndex2);        
                    strictSavings -= 2;
                    return true;
                }   
            }   
        }
        return false;
    }
    bool LoadPushPopLoad(<byte> output) 
    {
        // for example: LD E, LD D, PUSH DE, LD E, LD D
        uint instructionCount = instructionAddresses.Count;
        
        uint iIndex0 = instructionCount-1;
        uint address0 = instructionAddresses[iIndex0];
        uint iIndex1 = instructionCount-2;
        uint address1 = instructionAddresses[iIndex1];
        
        OpCode opCode0 = GetOpCode(output, address0);
        OpCode opCode1 = GetOpCode(output, address1);
        if ((opCode0 == OpCode.LD_D_iIY_d) && (opCode1 == OpCode.LD_E_iIY_d))
        {
            uint iIndex3 = instructionCount-4;
            uint address3 = instructionAddresses[iIndex3];
            uint iIndex4 = instructionCount-5;
            uint address4 = instructionAddresses[iIndex4];
            OpCode opCode3 = GetOpCode(output, address3);
            OpCode opCode4 = GetOpCode(output, address4);
            if ((opCode3 == OpCode.LD_D_iIY_d) && (opCode4 == OpCode.LD_E_iIY_d))
            {
                uint iIndex2 = instructionCount-3;
                uint address2 = instructionAddresses[iIndex2];            
                OpCode opCode2 = GetOpCode(output, address2);
                
                if (opCode2 == OpCode.PUSH_DE)
                {
                    byte operand0 = output[address0+2];
                    byte operand1 = output[address1+2];
                    byte operand3 = output[address3+2];
                    byte operand4 = output[address4+2];
                    if ((operand0 == operand3) && (operand1 == operand4))
                    {
                        _ = removeInstruction(output, address0);
                        _ = removeInstruction(output, address1);
                        instructionAddresses.Remove(iIndex0);
                        instructionAddresses.Remove(iIndex1);
                        strictSavings -=2;
                        return true;
                    }
                }
            }
        }
        return false;
    }
    bool PushPopLoad(<byte> output)
    {
        uint instructionCount = instructionAddresses.Count;
        
        uint iIndex0 = instructionCount-1;
        uint address0 = instructionAddresses[iIndex0];
        uint iIndex1 = instructionCount-2;
        uint address1 = instructionAddresses[iIndex1];
        
        OpCode opCode0 = GetOpCode(output, address0);
        OpCode opCode1 = GetOpCode(output, address1);
        
        // for example: PUSH HL, POP DE, LD (nn), E, LD (nn), D    ->     LD (nn), L LD (nn), H
        //     this changes the resulting contents of DE - NOT SAFE IN PEEPHOLE
        if (laxRules && (opCode0 == OpCode.LD_iIY_d_D) && (opCode1 == OpCode.LD_iIY_d_E))
        {
            uint iIndex2 = instructionCount-3;
            uint address2 = instructionAddresses[iIndex2];
            uint iIndex3 = instructionCount-4;
            uint address3 = instructionAddresses[iIndex3];
        
            OpCode opCode2 = GetOpCode(output, address2);
            OpCode opCode3 = GetOpCode(output, address3);
            if ((opCode2 == OpCode.POP_DE) && (opCode3 == OpCode.PUSH_HL))
            {
                output.SetItem(address1,   byte( OpCode.LD_iIY_d_L) >> 8);
                output.SetItem(address1+1, byte( OpCode.LD_iIY_d_L) & 0xFF);
                output.SetItem(address0,   byte( OpCode.LD_iIY_d_H) >> 8);
                output.SetItem(address0+1, byte( OpCode.LD_iIY_d_H) & 0xFF);
                byte bytesRemoved = removeInstruction(output, address2);
                bytesRemoved += removeInstruction(output, address3);
                
                instructionAddresses[iIndex0] = instructionAddresses[iIndex0] - bytesRemoved;
                instructionAddresses[iIndex1] = instructionAddresses[iIndex1] - bytesRemoved;
                instructionAddresses.Remove(iIndex2);
                instructionAddresses.Remove(iIndex3);
                laxSavings -=2;
                return true;
            }
        }
        // LD (IY), DE followed by LD DE (IY)
        //   safe to delete the extra load
        if ((opCode0 == OpCode.LD_D_iIY_d) && (opCode1 == OpCode.LD_E_iIY_d))
        {
            uint iIndex2 = instructionCount-3;
            uint address2 = instructionAddresses[iIndex2];
            uint iIndex3 = instructionCount-4;
            uint address3 = instructionAddresses[iIndex3];
        
            OpCode opCode2 = GetOpCode(output, address2);
            OpCode opCode3 = GetOpCode(output, address3);
            if ((opCode2 == OpCode.LD_iIY_d_D) && (opCode3 == OpCode.LD_iIY_d_E))
            {
                byte operand0 = output[address0+2];
                byte operand1 = output[address1+2];
                byte operand2 = output[address2+2];
                byte operand3 = output[address3+2];
                if ((operand0 == operand2) && (operand1 == operand3))
                {
                    
                    _ = removeInstruction(output, address0);
                    _ = removeInstruction(output, address1);
                    instructionAddresses.Remove(iIndex0);
                    instructionAddresses.Remove(iIndex1);
                    strictSavings -=2;        
                    return true;
                }
            }
        }
        // LD (IX), DE followed by LD DE (IX)
        //   safe to delete the extra load
        if ((opCode0 == OpCode.LD_D_iIX_d) && (opCode1 == OpCode.LD_E_iIX_d))
        {
            uint iIndex2 = instructionCount-3;
            uint address2 = instructionAddresses[iIndex2];
            uint iIndex3 = instructionCount-4;
            uint address3 = instructionAddresses[iIndex3];
        
            OpCode opCode2 = GetOpCode(output, address2);
            OpCode opCode3 = GetOpCode(output, address3);
            if ((opCode2 == OpCode.LD_iIX_d_D) && (opCode3 == OpCode.LD_iIX_d_E))
            {
                byte operand0 = output[address0+2];
                byte operand1 = output[address1+2];
                byte operand2 = output[address2+2];
                byte operand3 = output[address3+2];
                if ((operand0 == operand2) && (operand1 == operand3))
                {
                    
                    _ = removeInstruction(output, address0);
                    _ = removeInstruction(output, address1);
                    instructionAddresses.Remove(iIndex0);
                    instructionAddresses.Remove(iIndex1);  
                    strictSavings -=2;      
                    return true;
                }
            }
        }
        return false;       
    }
        
    
    bool LoadImmediatePushPop(<byte> output)
    {
        // for example, LD DE, nn, PUSH DE, POP HL -> LD HL, nn
        
        uint instructionCount = instructionAddresses.Count;
        uint address0 = instructionCount-1;
        uint iIndex0 = instructionAddresses[address0];
        OpCode opCode0 = GetOpCode(output, iIndex0);
        uint address1 = instructionCount-2;
        uint iIndex1 = instructionAddresses[address1];
        OpCode opCode1 = GetOpCode(output, iIndex1);
        uint address2 = instructionCount-3;
        uint iIndex2 = instructionAddresses[address2];
        OpCode opCode2 = GetOpCode(output, iIndex2);
        
        OpCode opCodeNew2 = OpCode.NOP;
        
        // this changes the resulting contents of DE - NOT SAFE IN PEEPHOLE
        if (laxRules && (opCode2 == OpCode.LD_DE_nn) && (opCode1 == OpCode.PUSH_DE))
        {
            if (opCode0 == OpCode.POP_HL)
            {
                opCodeNew2 = OpCode.LD_HL_nn;
            }
            else if (opCode0 == OpCode.POP_BC)
            {
                opCodeNew2 = OpCode.LD_BC_nn;
            }
        }
        // this changes the resulting contents of HL - NOT SAFE IN PEEPHOLE
        else if (laxRules && (opCode2 == OpCode.LD_HL_nn) && (opCode1 == OpCode.PUSH_HL))
        {
            if (opCode0 == OpCode.POP_DE)
            {
                opCodeNew2 = OpCode.LD_DE_nn;
            }
        }
        if (opCodeNew2 != OpCode.NOP)
        {
            output.SetItem(iIndex2, byte(opCodeNew2));     
            _ = removeInstruction(output, iIndex0);
            _ = removeInstruction(output, iIndex1);
            instructionAddresses.Remove(address0);
            instructionAddresses.Remove(address1);
            laxSavings -=2;
            return true;
        }
        
        return false;
    }
    
    bool PushPopCompare(<byte> output)    
    {
        // PUSH DE, POP HL, CP A, L -> CP A, E
        uint instructionCount = instructionAddresses.Count;
        uint address0 = instructionCount-1;
        uint iIndex0 = instructionAddresses[address0];
        OpCode opCode0 = GetOpCode(output, iIndex0);
        uint address1 = instructionCount-2;
        uint iIndex1 = instructionAddresses[address1];
        OpCode opCode1 = GetOpCode(output, iIndex1);
        uint address2 = instructionCount-3;
        uint iIndex2 = instructionAddresses[address2];
        OpCode opCode2 = GetOpCode(output, iIndex2);
        
        OpCode opCodeNew0 = OpCode.NOP;
        
        // this changes the resulting contents of HL - NOT SAFE IN PEEPHOLE
        if (laxRules && (opCode1 == OpCode.POP_HL) && (opCode0 == OpCode.CP_A_L))
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
        // this changes the resulting contents of DE - NOT SAFE IN PEEPHOLE
        else if (laxRules && (opCode1 == OpCode.POP_DE) && (opCode0 == OpCode.CP_A_E))
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
        // this changes the resulting contents of BC - NOT SAFE IN PEEPHOLE
        else if (laxRules && (opCode1 == OpCode.POP_BC) && (opCode0 == OpCode.CP_A_C))
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
            _ = removeInstruction(output, iIndex1);
            _ = removeInstruction(output, iIndex2);
            instructionAddresses.Remove(address1);
            instructionAddresses.Remove(address2);
            laxSavings -=2;
            return true;
        }
        return false;
    }
    
}
