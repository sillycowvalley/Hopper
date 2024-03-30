unit Z80Library
{
    <string, uint> libraryAddresses;
    
    uint GetAddress(string name)
    {
        return libraryAddresses[name];
    }
    Generate()
    {
        uint address = CurrentAddress;
        Peephole.Reset();
        EmitMUL();
        libraryAddresses["MUL"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitDIVMOD();
        libraryAddresses["DIVMOD"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitEQ();
        libraryAddresses["EQ"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitNE();
        libraryAddresses["NE"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitLE();
        libraryAddresses["LE"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitLT();
        libraryAddresses["LT"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitGT();
        libraryAddresses["GT"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitGE();
        libraryAddresses["GE"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitBITAND();
        libraryAddresses["BITAND"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitBITOR();
        libraryAddresses["BITOR"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitBITXOR();
        libraryAddresses["BITXOR"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitBITNOT();
        libraryAddresses["BITNOT"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitBITSHL();
        libraryAddresses["BITSHL"] = address;
        
        address = CurrentAddress;
        Peephole.Reset();
        EmitBITSHR();
        libraryAddresses["BITSHR"] = address;
    }
    
    bool SysCall(byte iSysCall)
    {
        // iOverload is in A if it is needed
        switch (SysCalls(iSysCall))
        {
            case SysCalls.SerialWriteChar:
            {
                Emit(OpCode.POP_DE); // character to emit is in 'E'
                // ...
            }
            case SysCalls.SerialReadChar:
            {
                // ...
                Emit(OpCode.PUSH_DE); // character read pushed t stack in 'E'
            }
            
            case SysCalls.DiagnosticsDie:
            {
                Emit(OpCode.POP_DE); // error code
                EmitWord(OpCode.LD_inn_DE, LastError);
                Emit(OpCode.HALT);
            }
            
            default:
            {
                PrintLn("iSysCall=0x" + (byte(iSysCall)).ToHexString(2) + " not implemented");
                Emit(OpCode.NOP);
                return false;
            }
        }
        return true;
    }
    
    EmitMUL()
    {
        // top -> BC, next -> DE
        // HL = next * top
        
        utilityMultiply(); // DEHL=BC*DE
        
        Emit(OpCode.RET);
    }
    EmitDIVMOD()
    {
        // top -> BC, next -> DE
        // BC = next / top
        // HL = next % top
        
        utilityDivide(); // BC = BC / DE, remainder in HL
        
        Emit(OpCode.RET);        
    }
    
    utilityDivide() // BC = BC / DE, remainder in HL
    {
        // https://map.grauw.nl/articles/mult_div_shifts.php
        Peephole.Disabled = true;
        Emit(OpCode.AND_A);
// Div16:
        EmitWord(OpCode.LD_HL_nn, 0);
        Emit(OpCode.LD_A_B);
        EmitByte(OpCode.LD_B_n, 8);
// Div16_Loop1:
        Emit(OpCode.RLA);
        Emit(OpCode.ADC_HL_HL);
        Emit(OpCode.SBC_HL_DE);
        EmitOffset(OpCode.JR_NC_e, +2); // Div16_NoAdd1
        Emit(OpCode.ADC_HL_DE);
// Div16_NoAdd1:
        EmitOffset(OpCode.DJNZ_e, -11);// Div16_Loop1
        Emit(OpCode.RLA);
        Emit(OpCode.CLP);
        Emit(OpCode.LD_B_A);
        Emit(OpCode.LD_A_C);
        Emit(OpCode.LD_C_B);
        EmitByte(OpCode.LD_B_n, 8);
// Div16_Loop2:
        Emit(OpCode.RLA);
        Emit(OpCode.ADC_HL_HL);
        Emit(OpCode.SBC_HL_DE);
        EmitOffset(OpCode.JR_NC_e, +2); // Div16_NoAdd2
        Emit(OpCode.ADC_HL_DE);
// Div16_NoAdd2:
        EmitOffset(OpCode.DJNZ_e, -11);// Div16_Loop2
        Emit(OpCode.RLA);
        Emit(OpCode.CLP);
        Emit(OpCode.LD_B_C);
        Emit(OpCode.LD_C_A); 
        Peephole.Disabled = false;
        Peephole.Reset();
    }
    
    utilityMultiply() // DEHL=BC*DE
    {
        Peephole.Disabled = true;
        // https://tutorials.eeems.ca/Z80ASM/part4.htm
        EmitWord(OpCode.LD_HL_nn, 0);                          
        EmitByte(OpCode.LD_A_n, 16);
//Mul16Loop:
        Emit(OpCode.ADD_HL_HL);
        Emit(OpCode.RL_E);
        Emit(OpCode.RL_D);
        EmitOffset(OpCode.JR_NC_e, +4); // NoMul16:
        Emit(OpCode.ADD_HL_BC);
        EmitOffset(OpCode.JR_NC_e, +1); // NoMul16:
        Emit(OpCode.INC_DE);
//NoMul16:
        Emit(OpCode.DEC_A);
        EmitOffset(OpCode.JR_NZ_e, -14); // //Mul16Loop:
        Peephole.Disabled = false;
        Peephole.Reset();
    }
    
    EmitLE()
    {
        Peephole.Disabled = true;
        // next -> HL, top -> BC
        // LE: next = HL <= BC ? 1 : 0   
        
        EmitWord(OpCode.LD_DE_nn, 1);     // LSB: result = true
        
        Emit(OpCode.LD_A_H);              // MSB
        Emit(OpCode.CP_A_B);     
        EmitOffset(OpCode.JR_NZ_e,  +2);  // UseMSB
        Emit(OpCode.LD_A_L);              // LSB
        Emit(OpCode.CP_A_C);      
//UseMSB:        
        EmitOffset(OpCode.JR_Z_e, +4);    // Exit
        EmitOffset(OpCode.JR_C_e, +2);    // Exit
        EmitByte(OpCode.LD_E_n, 0);       // LSB: result = false
// Exit:       
        Emit(OpCode.RET);
        Peephole.Disabled = false;
        Peephole.Reset();
    }
    
    EmitLT()
    {
        Peephole.Disabled = true;
        // next -> HL, top -> BC
        // LE: next = HL < BC ? 1 : 0   
        
        EmitWord(OpCode.LD_DE_nn, 0);     // LSB: result = false
        
        Emit(OpCode.LD_A_H);              // MSB
        Emit(OpCode.CP_A_B);     
        EmitOffset(OpCode.JR_NZ_e, +2);   // UseMSB
        Emit(OpCode.LD_A_L);              // LSB
        Emit(OpCode.CP_A_C);      
//UseMSB:
        EmitOffset(OpCode.JR_NC_e, +2);   // Exit
        EmitByte(OpCode.LD_E_n, 1);       // LSB: result = true
// Exit:       
        Emit(OpCode.RET); 
        Peephole.Disabled = false;
        Peephole.Reset();
    }
    
    EmitGT()
    {
        Peephole.Disabled = true;
        // next -> HL, top -> BC
        // LE: next = HL > BC ? 1 : 0   
        
        EmitWord(OpCode.LD_DE_nn, 0);     // LSB: result = false
        
        Emit(OpCode.LD_A_H);              // MSB
        Emit(OpCode.CP_A_B);     
        EmitOffset(OpCode.JR_NZ_e,  +2);  // UseMSB
        Emit(OpCode.LD_A_L);              // LSB
        Emit(OpCode.CP_A_C);      
//UseMSB:        
        EmitOffset(OpCode.JR_Z_e, +4);    // Exit
        EmitOffset(OpCode.JR_C_e, +2);    // Exit
        EmitByte(OpCode.LD_E_n, 1);       // LSB: result = true
// Exit:       
        Emit(OpCode.RET); 
        Peephole.Disabled = false;
        Peephole.Reset();
    }
    
    EmitEQ()
    {
        Peephole.Disabled = true;
        // top -> BC, next -> HL 
        
        EmitWord(OpCode.LD_DE_nn, 0);    // LSB: result = false
        
        // Compare BC and HL
        Emit(OpCode.LD_A_B);             // MSB     
        Emit(OpCode.CP_A_H);     
        EmitOffset(OpCode.JR_NZ_e,  +6); // B!= H -> Exit
        Emit(OpCode.LD_A_C);             // LSB
        Emit(OpCode.CP_A_L);      
        EmitOffset(OpCode.JR_NZ_e,  +2); // C != L -> Exit
        // BC == HL
        EmitByte(OpCode.LD_E_n, 1);      // LSB: result = true
// Exit:        
        Emit(OpCode.RET);
        Peephole.Disabled = false;
        Peephole.Reset();
    }
    EmitNE()
    {
        Peephole.Disabled = true;
        // top -> BC, next -> HL 
        
        EmitWord(OpCode.LD_DE_nn, 1);     // LSB: result = true
        
        // Compare BC and HL
        Emit(OpCode.LD_A_B);              // MSB     
        Emit(OpCode.CP_A_H);     
        EmitOffset(OpCode.JR_NZ_e,  +6);  // B!= H -> Exit
        Emit(OpCode.LD_A_C);              // LSB
        Emit(OpCode.CP_A_L);      
        EmitOffset(OpCode.JR_NZ_e,  +2);  // C != L -> Exit
        // BC == HL
        EmitByte(OpCode.LD_E_n, 0);       // LSB: result = false
// Exit:        
        Emit(OpCode.RET);
        Peephole.Disabled = false;
        Peephole.Reset();
    }
    
    EmitGE()
    {
        Peephole.Disabled = true;
        // next -> HL, top -> BC
        // GE: next = HL >= BC ? 1 : 0   
        
        EmitWord(OpCode.LD_DE_nn, 0);     // LSB: result = false
        
        Emit(OpCode.LD_A_H);              // MSB
        Emit(OpCode.CP_A_B);     
        EmitOffset(OpCode.JR_NZ_e, +2);   // UseMSB
        Emit(OpCode.LD_A_L);              // LSB
        Emit(OpCode.CP_A_C);      
//UseMSB:
        EmitOffset(OpCode.JR_C_e, +2);    // Exit
        EmitByte(OpCode.LD_E_n, 1);       // LSB: result = true
// Exit:       
        Emit(OpCode.RET); 
        Peephole.Disabled = false;
        Peephole.Reset();
    }
    
    EmitBITSHL()
    {
        // next -> HL, top -> BC 
        // HL = next << top
        
        Emit(OpCode.LD_B_C); // (assuming the shift is < 256)
        
        Peephole.Disabled = true;
        Emit(OpCode.AND_A);
        Emit(OpCode.SLA_L);
        Emit(OpCode.RL_H);
        EmitOffset(OpCode.DJNZ_e, -7);
        Emit(OpCode.RET);
        Peephole.Disabled = false;
        Peephole.Reset();
    }
    EmitBITSHR()
    {
        // next -> HL, top -> BC
        // HL = next >> top
        
        Emit(OpCode.LD_B_C); // (assuming the shift is < 256)
        
        Peephole.Disabled = true;
        Emit(OpCode.AND_A);
        Emit(OpCode.SRL_H);
        Emit(OpCode.RR_L);
        EmitOffset(OpCode.DJNZ_e, -7);
        Emit(OpCode.RET);
        Peephole.Disabled = false;
        Peephole.Reset();
    }
    
    
    EmitBITAND()
    {
        // next -> HL, top -> BC
        // HL = next & top
        
        Emit(OpCode.LD_A_L);
        Emit(OpCode.AND_A_C);
        Emit(OpCode.LD_L_A);
        Emit(OpCode.LD_A_H);
        Emit(OpCode.AND_A_B);
        Emit(OpCode.LD_H_A);
        Emit(OpCode.RET);
    }
    
    EmitBITOR()
    {
        // next -> HL, top -> BC
        // HL = next | top
        
        Emit(OpCode.LD_A_L);
        Emit(OpCode.OR_A_C);
        Emit(OpCode.LD_L_A);
        Emit(OpCode.LD_A_H);
        Emit(OpCode.OR_A_B);
        Emit(OpCode.LD_H_A);
        Emit(OpCode.RET);
    }
    
    EmitBITXOR()
    {
        // next -> HL, top -> BC
        // HL = next ^ top
        
        Emit(OpCode.LD_A_L);
        Emit(OpCode.XOR_A_C);
        Emit(OpCode.LD_L_A);
        Emit(OpCode.LD_A_H);
        Emit(OpCode.XOR_A_B);
        Emit(OpCode.LD_H_A);
        Emit(OpCode.RET);
    }
    
    EmitBITNOT()
    {
        // top -> HL
        // HL = ~top
        
        Emit(OpCode.LD_A_L);
        Emit(OpCode.CPL_A_A);
        Emit(OpCode.LD_L_A);
        Emit(OpCode.LD_A_H);
        Emit(OpCode.CPL_A_A);
        Emit(OpCode.LD_H_A);
        Emit(OpCode.RET);
    }
       
    

}
