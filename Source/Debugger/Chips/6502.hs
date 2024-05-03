unit CPU // 6502
{
    uses "/Source/Compiler/CODEGEN/Asm6502"
    uses "/Source/Debugger/6502/ZeroPage"
    
    uses "/Source/System/Screen"
    
    byte[0xFFFA] memory;
    uint vectorReset;
    uint vectorIRQ;
    uint vectorNMI;
    
    uint Entry { get { return vectorReset; } set { vectorReset = value; } }
    uint IRQ   { get { return vectorIRQ;   } set { vectorIRQ = value;   } }
    uint NMI   { get { return vectorNMI;   } set { vectorNMI = value;   } }
    uint PC    { get { return pcRegister;  } set { pcRegister = value;  } }
    
    byte LastError { get { return lastError; } }
    int nmiWaiting;
    int irqWaiting;
    
    byte lastError;
    
    // registers
    uint pcRegister;
    byte spRegister;
    byte aRegister;
    byte xRegister;
    byte yRegister;
    
    // flags
    bool nFlag;
    bool vFlag;
    bool bFlag;
    bool dFlag;
    bool iFlag;
    bool zFlag;
    bool cFlag;
    
    <byte> GetRAM()
    {
        <byte> ram;
        for (uint i = 0; i < 0x8000; i++)
        {
            ram.Append(memory[i]);
        }
        return ram;
    }
    
    CheckNZ(byte value)
    {
        zFlag = (value == 0);
        nFlag = (value & 0x80 != 0);
    }
    Compare(byte register, byte operand)
    {
        uint value = register - operand;
        nFlag = (value & 0x80 != 0);
        zFlag = (register == operand);
        cFlag = (register >= operand);
    }
    
    ADC(byte operand)
    {
        uint sum = uint(aRegister) + uint(operand) + (cFlag ? 1 : 0);
        cFlag = (sum > 255);
        aRegister = byte(sum & 0xFF);

        // TODO:        
        // The overflow flag is set when the sign or bit 7 is changed due to the 
        // result exceeding +127 or -128, otherwise overflow is reset. 
    }
    
    SBC(byte operand)
    {
        int isum = int(aRegister) - int(operand) - int(cFlag ? 0 : 1);
        cFlag = (isum >= 0);
        vFlag = (isum < -127) || (isum > 127);
        aRegister = isum.GetByte(0);
    }
    BIT(byte operand)
    {
        zFlag = (aRegister & operand  == 0);
        nFlag = (operand & 0b10000000 != 0);
        vFlag = (operand & 0b01000000 != 0);
    }
    
    byte Pop()
    {
        if (spRegister == 0xFF)
        {
            spRegister = 0;
        }
        else
        {
            spRegister++;
        }
        uint address = spRegister + 0x0100;
        return memory[address];
    }
    Push(byte argument)
    {
        uint address = spRegister + 0x0100;
        memory[address] = argument;
        if (spRegister == 0)
        {
            spRegister = 0xFF;
        }
        else
        {
            spRegister--;
        }
    }
    Push(uint argument)
    {
        Push(byte(argument >> 8));
        Push(byte(argument & 0xFF));
    }
    PushFlags()
    {
        byte allFlags;
        if (nFlag) { allFlags |= 0b10000000; }
        if (vFlag) { allFlags |= 0b01000000; }
        if (dFlag) { allFlags |= 0b00001000; }
        if (iFlag) { allFlags |= 0b00000100; }
        if (zFlag) { allFlags |= 0b00000010; }
        if (cFlag) { allFlags |= 0b00000001; }
        Push(allFlags);
    }
    PopFlags()
    {
        byte allFlags = Pop();
        nFlag = (allFlags & 0b10000000 != 0);
        vFlag = (allFlags & 0b01000000 != 0);
        dFlag = (allFlags & 0b00001000 != 0);
        iFlag = (allFlags & 0b00000100 != 0);
        zFlag = (allFlags & 0b00000010 != 0);
        cFlag = (allFlags & 0b00000001 != 0);
    }
    
    JMP(uint operand)
    {
        pcRegister = operand;
    }
    JMPIndexed(uint operand)
    {
        pcRegister = operand;
    }
    BRA(int offset)
    {
        pcRegister = uint(long(pcRegister) + 2 + offset);
    }
    
    bool BBR(byte bit, uint operand, int offset)
    {
        byte value = GetMemory(operand);
        if (value & (1 << bit) == 0)
        {
            pcRegister = uint(long(pcRegister) + 3 + offset);
            return true;
        }
        return false;
    }
    bool BBS(byte bit, uint operand, int offset)
    {
        byte value = GetMemory(operand);
        if (value & (1 << bit) != 0)
        {
            pcRegister = uint(long(pcRegister) + 3 + offset);
            return true;
        }
        return false;
    }
    RMB(byte bit, uint operand)
    {
        byte mask  = ~(1 << bit);
        byte value = GetMemory(operand);
        value &= mask;
        SetMemory(operand, value);
    }
    SMB(byte bit, uint operand)
    {
        byte mask  = (1 << bit);
        byte value = GetMemory(operand);
        value |= mask;
        SetMemory(operand, value);
    }
    
    JSR(uint operand)
    {
        pcRegister += 2;
        Push(pcRegister);
        pcRegister = operand;
    }
    RaiseIRQ()
    {
        irqWaiting++;
    }
    RaiseNMI()
    {
        nmiWaiting++;
    }
    doNMI()
    {
        Push(pcRegister);
        PushFlags();
        pcRegister = NMI;
        iFlag = true;
    }
    doIRQ()
    {
        Push(pcRegister);
        PushFlags();
        pcRegister = IRQ;
        iFlag = true;
    }
    BRK()
    {
        // #### BRK: a way for 6502 code to panic for now
        lastError = aRegister;
        PrintLn();
        PrintLn("6502 Panic at PC=0x" + pcRegister.ToHexString(4) 
                           + ", A=0x" + aRegister.ToHexString(2)
                           + ", X=0x" + xRegister.ToHexString(2)
                           + ", Y=0x" + yRegister.ToHexString(2)
                           , Colour.Red, Colour.Black);
        PrintLn();
        
        // stop e6502
        pcRegister = InvalidAddress;
        
        // stop Hopper VM
        //SetMemory(ZP.ZPCL, 0xFF);
        //SetMemory(ZP.ZPCH, 0xFF);
        
        // #### What BRK actually does:
        /*
        Push(pcRegister+1);
        PushFlags();
        pcRegister = IRQ;
        iFlag = true;
        bFlag = true;
        */
    }
    RTS()
    {
        pcRegister = Pop() + (Pop() << 8);
        pcRegister++;
    }
    RTI()
    {
        PopFlags();
        pcRegister = Pop() + (Pop() << 8);
    }
    
    OpCode GetInstruction(uint address)
    {
        return OpCode(GetMemory(address));
    }
    
    Execute(bool ignoreBreakPoints)
    {
        if (pcRegister == InvalidAddress) { return; }
        
        // check for IRQ and NMI:
        if (nmiWaiting > 0)
        {
            nmiWaiting--;
            doNMI();
            if (!ignoreBreakPoints)
            {
                if (Emulator.IsBreakPoint(pcRegister))
                {
                    return;
                }
            }
        }
        else if ((irqWaiting > 0) && !iFlag)
        {
            irqWaiting--;
            doIRQ();  
            if (!ignoreBreakPoints)
            {
                if (Emulator.IsBreakPoint(pcRegister))
                {
                    return;
                }
            } 
        }
        
        OpCode instruction             = OpCode(memory[pcRegister]);
        uint length                    = Asm6502.GetInstructionLength(instruction);
        AddressingModes addressingMode = Asm6502.GetAddressingMode(instruction);
        uint operand;
        int  offset;
        switch (addressingMode)
        {   
            case AddressingModes.Implied:
            case AddressingModes.Accumulator:
            {
                // no operand
            }
            case AddressingModes.Absolute:          //  nnnn
            {
                operand = memory[pcRegister+1] + (memory[pcRegister+2] << 8);
            }
            case AddressingModes.AbsoluteX:         //  nnnn,X
            {
                operand = memory[pcRegister+1] + (memory[pcRegister+2] << 8) + xRegister;
            }
            case AddressingModes.AbsoluteY:         //  nnnn,Y
            {
                operand = memory[pcRegister+1] + (memory[pcRegister+2] << 8) + yRegister;
            }
            case AddressingModes.AbsoluteIndirect:  // (nnnn)
            {
                uint address = memory[pcRegister+1] + (memory[pcRegister+2] << 8);
                operand      = memory[address   +0] + (memory[address   +1] << 8);
            }
            case AddressingModes.AbsoluteIndirectX: // (nnnn,X)
            {
                operand = memory[pcRegister+1] + (memory[pcRegister+2] << 8);
                operand = operand + xRegister;
                uint address = memory[operand] + (memory[operand+1] << 8);
                operand = address;
            }
            case AddressingModes.Immediate:         // #nn
            case AddressingModes.ZeroPage:          // nn
            {
                operand = memory[pcRegister+1];
            }
            case AddressingModes.ZeroPageX:         // nn,X
            {
                operand = (memory[pcRegister+1] + xRegister) & 0xFF;
            }
            case AddressingModes.ZeroPageY:         // nn,Y
            {
                operand = (memory[pcRegister+1] + yRegister) & 0xFF;
            }
            case AddressingModes.ZeroPageIndirect:  // (nn)
            {
                operand = memory[pcRegister+1];
                operand = (memory[operand] + (memory[operand+1] << 8));
            }
            case AddressingModes.XIndexedZeroPage:  // (nn,X)
            {
                operand = (memory[pcRegister+1] + xRegister) & 0xFF;
                operand = (memory[operand] + (memory[operand+1] << 8));
            }
            case AddressingModes.YIndexedZeroPage:  // (nn), Y
            {
                operand = memory[pcRegister+1];
                operand = (memory[operand] + (memory[operand+1] << 8) + yRegister);
            }
            case AddressingModes.Relative:          // dd
            {
                offset = memory[pcRegister+1];
                if (offset > 127)
                {
                    offset = offset - 256; // 0xFF -> -1
                }
            }
            case AddressingModes.ZeroPageRelative:  // nn,dd
            {
                operand = memory[pcRegister+1];
                offset  = memory[pcRegister+2];
                if (offset > 127)
                {
                    offset = offset - 256; // 0xFF -> -1
                }
            }
            default: 
            { 
                PrintLn("AddressingModes: " + (uint(addressingMode)).ToHexString(4)); Die(0x0A); 
            }
        }
        
        loop
        {
            switch (instruction)
            {
                // 'break' here means don't increment the pcRegister by instruction length
                case OpCode.NOP:      {  } 
                case OpCode.JSR_nn:   { JSR(operand); break; } 
                case OpCode.RTS:      { RTS();        break; }
                case OpCode.RTI:      { RTI();        break; }
                case OpCode.BRK:      { BRK();        break; }
                case OpCode.JMP_nn:   { JMP(operand); break; } 
                case OpCode.JMP_innX: { JMPIndexed(operand); break; } 
                case OpCode.JMP_inn:  { JMPIndexed(operand);  break; } 
                case OpCode.BRA_e:    { BRA(offset);  break; }
                
                case OpCode.BCC_e: { if (!cFlag) { BRA(offset); break; } }
                case OpCode.BCS_e: { if (cFlag)  { BRA(offset); break; } }
                case OpCode.BEQ_e: { if (zFlag)  { BRA(offset); break; } }
                case OpCode.BMI_e: { if (nFlag)  { BRA(offset); break; } }
                case OpCode.BNE_e: { if (!zFlag) { BRA(offset); break; } }
                case OpCode.BPL_e: { if (!nFlag) { BRA(offset); break; } }
                case OpCode.BVC_e: { if (!vFlag) { BRA(offset); break; } }
                case OpCode.BVS_e: { if (vFlag)  { BRA(offset); break; } }
                
                
                case OpCode.PHA: { Push(aRegister);   } 
                case OpCode.PHX: { Push(xRegister);   } 
                case OpCode.PHY: { Push(yRegister);   } 
                case OpCode.PHP: { PushFlags();       } 
                case OpCode.PLA: { aRegister = Pop(); CheckNZ(aRegister); }
                case OpCode.PLX: { xRegister = Pop(); CheckNZ(xRegister); }
                case OpCode.PLY: { yRegister = Pop(); CheckNZ(yRegister); }
                case OpCode.PLP: { PopFlags();       }
                
                case OpCode.CLC: { cFlag = false; }
                case OpCode.CLD: { dFlag = false; }
                case OpCode.CLI: { iFlag = false; }
                case OpCode.CLV: { vFlag = false; }
                
                case OpCode.SEC: { cFlag = true; }
                case OpCode.SED: { dFlag = true; }
                case OpCode.SEI: { iFlag = true; }
                
                case OpCode.TAX: { xRegister = aRegister;  CheckNZ(xRegister); }
                case OpCode.TAY: { yRegister = aRegister;  CheckNZ(yRegister); }
                case OpCode.TXA: { aRegister = xRegister;  CheckNZ(aRegister); }
                case OpCode.TYA: { aRegister = yRegister;  CheckNZ(aRegister); }
                case OpCode.TXS: { spRegister = xRegister;                     }
                case OpCode.TSX: { xRegister = spRegister; CheckNZ(xRegister); }
                
                case OpCode.AND_n:   { aRegister = aRegister & byte(operand);   CheckNZ(aRegister); }   
                case OpCode.AND_nn:                               
                case OpCode.AND_nnX:                              
                case OpCode.AND_nnY:                              
                case OpCode.AND_z:                                
                case OpCode.AND_zX:                               
                case OpCode.AND_iz:                               
                case OpCode.AND_izX:                              
                case OpCode.AND_izY: { aRegister = aRegister & GetMemory(operand); CheckNZ(aRegister); }
                
                case OpCode.BIT_n:   { BIT(byte(operand)); }
                case OpCode.BIT_nn:  
                case OpCode.BIT_nnX:  
                case OpCode.BIT_z:  
                case OpCode.BIT_zX:  { BIT(GetMemory(operand)); }
                
                
                case OpCode.ORA_n:   { aRegister = aRegister | byte(operand);   CheckNZ(aRegister); } 
                case OpCode.ORA_nn:                    
                case OpCode.ORA_nnX:                   
                case OpCode.ORA_nnY:                   
                case OpCode.ORA_z:        
                case OpCode.ORA_zX:       
                case OpCode.ORA_iz:       
                case OpCode.ORA_izX:      
                case OpCode.ORA_izY: { aRegister = aRegister | GetMemory(operand); CheckNZ(aRegister); } 
                
                case OpCode.EOR_n:   { aRegister = aRegister ^ byte(operand);   CheckNZ(aRegister); } 
                case OpCode.EOR_nn:                    
                case OpCode.EOR_nnX:                   
                case OpCode.EOR_nnY:                   
                case OpCode.EOR_z:        
                case OpCode.EOR_zX:       
                case OpCode.EOR_iz:       
                case OpCode.EOR_izX:      
                case OpCode.EOR_izY: { aRegister = aRegister ^ GetMemory(operand); CheckNZ(aRegister); } 
                
                
                case OpCode.ADC_n: { ADC(byte(operand));              CheckNZ(aRegister); } 
                case OpCode.ADC_nn:    
                case OpCode.ADC_nnX:   
                case OpCode.ADC_nnY:   
                case OpCode.ADC_z:     
                case OpCode.ADC_zX:    
                case OpCode.ADC_iz:    
                case OpCode.ADC_izX:   
                case OpCode.ADC_izY: { ADC(GetMemory(operand));         CheckNZ(aRegister); } 
                
                case OpCode.SBC_n: { SBC(byte(operand));              CheckNZ(aRegister); } 
                case OpCode.SBC_nn:        
                case OpCode.SBC_nnX:       
                case OpCode.SBC_nnY:       
                case OpCode.SBC_z:         
                case OpCode.SBC_zX:        
                case OpCode.SBC_iz:        
                case OpCode.SBC_izX:       
                case OpCode.SBC_izY: { SBC(GetMemory(operand));         CheckNZ(aRegister); } 
                
                case OpCode.LSR: 
                { 
                    cFlag = ((aRegister & 0x01) != 0);
                    aRegister = aRegister >> 1; CheckNZ(aRegister); 
                }
                case OpCode.LSR_nn: 
                case OpCode.LSR_nnX:
                case OpCode.LSR_z:  
                case OpCode.LSR_zX: 
                { 
                    byte value = GetMemory(operand);
                    cFlag = ((value & 0x01) != 0);
                    value = value >> 1; CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case OpCode.ASL: 
                { 
                    cFlag     = ((aRegister & 0x80) != 0);
                    aRegister = (aRegister << 1) & 0xFF; CheckNZ(aRegister);   
                }
                case OpCode.ASL_nn:
                case OpCode.ASL_nnX:  
                case OpCode.ASL_z:    
                case OpCode.ASL_zX:   
                { 
                    byte value = GetMemory(operand);
                    cFlag = ((value & 0x80) != 0);
                    value = (value << 1) & 0xFF; CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case OpCode.ROL: 
                { 
                    bool cAfter = ((aRegister & 0x80) != 0);
                    aRegister   = ((aRegister << 1) & 0xFF) | (cFlag ? 1 : 0); 
                    cFlag       = cAfter;
                    CheckNZ(aRegister);                                  
                }
                case OpCode.ROL_nn:     
                case OpCode.ROL_nnX:    
                case OpCode.ROL_z:      
                case OpCode.ROL_zX:     
                { 
                    byte value = GetMemory(operand);
                    bool cAfter = ((value & 0x80) != 0);
                    value = ((value << 1) & 0xFF) | (cFlag ? 1 : 0); 
                    cFlag = cAfter; 
                    CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case OpCode.ROR: 
                { 
                    bool cAfter = ((aRegister & 0x01) != 0);
                    aRegister   = (aRegister >> 1) | (cFlag ? 0x80 : 0); 
                    cFlag       = cAfter;
                    CheckNZ(aRegister);                                           
                }
                case OpCode.ROR_nn:          
                case OpCode.ROR_nnX:         
                case OpCode.ROR_z:           
                case OpCode.ROR_zX:          
                { 
                    byte value  = GetMemory(operand);
                    bool cAfter = ((value & 0x01) != 0);
                    value       = (value >> 1) | (cFlag ? 0x80 : 0); 
                    cFlag       = cAfter;
                    CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case OpCode.INC: 
                { 
                    if (aRegister == 0xFF) 
                    { aRegister = 0; } 
                    else
                    { aRegister++; }   CheckNZ(aRegister);
                }
                case OpCode.INY: 
                { 
                    if (yRegister == 0xFF) 
                    { yRegister = 0; } 
                    else
                    { yRegister++; }   CheckNZ(yRegister);  
                }
                case OpCode.INX: 
                { 
                    if (xRegister == 0xFF) 
                    { xRegister = 0; } 
                    else
                    { xRegister++; }   CheckNZ(xRegister);      
                }
                case OpCode.INC_nn:   
                case OpCode.INC_nnX:  
                case OpCode.INC_z:    
                case OpCode.INC_zX:   
                { 
                    byte value = GetMemory(operand);
                    if (value == 0xFF) 
                    { value = 0; } 
                    else
                    { value++; }   CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case OpCode.DEC: 
                { 
                    if (aRegister == 0) 
                    { aRegister = 0xFF; } 
                    else
                    { aRegister--; }   CheckNZ(aRegister);  
                }
                case OpCode.DEY: 
                { 
                    if (yRegister == 0) 
                    { yRegister = 0xFF; } 
                    else
                    { yRegister--; }   CheckNZ(yRegister);
                }
                case OpCode.DEX: 
                { 
                    if (xRegister == 0) 
                    { xRegister = 0xFF; } 
                    else
                    { xRegister--; }   CheckNZ(xRegister);  
                }
                case OpCode.DEC_nn:  
                case OpCode.DEC_nnX: 
                case OpCode.DEC_z:   
                case OpCode.DEC_zX:  
                { 
                    byte value = GetMemory(operand);
                    if (value == 0) 
                    { value = 0xFF; } 
                    else
                    { value--; }   CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case OpCode.LDA_n: { aRegister = byte(operand);   CheckNZ(aRegister); }
                case OpCode.LDA_nn:                                                       
                case OpCode.LDA_nnX:                                                      
                case OpCode.LDA_nnY:                                                      
                case OpCode.LDA_z:                                                        
                case OpCode.LDA_zX:                                                         
                case OpCode.LDA_iz:                                                         
                case OpCode.LDA_izX:                                                        
                case OpCode.LDA_izY: { aRegister = GetMemory(operand); CheckNZ(aRegister); }
                
                case OpCode.LDX_n: { xRegister = byte(operand);   CheckNZ(xRegister); }   
                case OpCode.LDX_nn:                                                        
                case OpCode.LDX_nnY:                                                       
                case OpCode.LDX_z:                                                         
                case OpCode.LDX_zY: { xRegister = GetMemory(operand); CheckNZ(xRegister); }
                
                case OpCode.LDY_n: { yRegister = byte(operand);   CheckNZ(yRegister); }
                case OpCode.LDY_nn:                                                        
                case OpCode.LDY_nnX:                                                       
                case OpCode.LDY_z:                                                         
                case OpCode.LDY_zX: { yRegister = GetMemory(operand); CheckNZ(yRegister); }
                
                case OpCode.STA_nn:                   
                case OpCode.STA_nnX:                  
                case OpCode.STA_nnY:                  
                case OpCode.STA_z:                    
                case OpCode.STA_zX:                   
                case OpCode.STA_iz:                   
                case OpCode.STA_izX:                  
                case OpCode.STA_izY: { SetMemory(operand, aRegister);                     } 
                
                case OpCode.STX_nn:                                                      
                case OpCode.STX_z:                                                       
                case OpCode.STX_zY: { SetMemory(operand, xRegister);                    }
                
                case OpCode.STY_nn:                                                      
                case OpCode.STY_z:                                                       
                case OpCode.STY_zX: { SetMemory(operand, yRegister);                    }
                
                case OpCode.STZ_nn:                                                  
                case OpCode.STZ_nnX:                                                 
                case OpCode.STZ_z:                                                   
                case OpCode.STZ_zX: { SetMemory(operand, 0);                             }
                
                case OpCode.CMP_n: { Compare(aRegister, byte(operand));               } 
                case OpCode.CMP_nn:                    
                case OpCode.CMP_nnX:                   
                case OpCode.CMP_nnY:                   
                case OpCode.CMP_z:                     
                case OpCode.CMP_zX:                    
                case OpCode.CMP_iz:                    
                case OpCode.CMP_izX:                   
                case OpCode.CMP_izY: { Compare(aRegister, GetMemory(operand));          }
                
                case OpCode.CPX_n: { Compare(xRegister, byte(operand));               } 
                case OpCode.CPX_nn:                                                    
                case OpCode.CPX_z: { Compare(xRegister, GetMemory(operand));          }
                
                case OpCode.CPY_n: { Compare(yRegister, byte(operand));               } // CPY #nn
                case OpCode.CPY_nn:                                                      // CPY nnnn
                case OpCode.CPY_z: { Compare(yRegister, GetMemory(operand));          } // CPY nn
                
                case OpCode.STP: { pcRegister = InvalidAddress; break;              } // STP
                
                case OpCode.BBR0_z_e:
                case OpCode.BBR1_z_e:
                case OpCode.BBR2_z_e:
                case OpCode.BBR3_z_e:
                case OpCode.BBR4_z_e:
                case OpCode.BBR5_z_e:
                case OpCode.BBR6_z_e:
                case OpCode.BBR7_z_e: { if (BBR((byte(instruction) & 0x70) >> 4, operand, offset)) { break; } }
                case OpCode.BBS0_z_e:
                case OpCode.BBS1_z_e:
                case OpCode.BBS2_z_e:
                case OpCode.BBS3_z_e:
                case OpCode.BBS4_z_e:
                case OpCode.BBS5_z_e:
                case OpCode.BBS6_z_e:
                case OpCode.BBS7_z_e: { if (BBS((byte(instruction) & 0x70) >> 4, operand, offset)) { break; } }
                
                
                case OpCode.RMB0_z:
                case OpCode.RMB1_z:
                case OpCode.RMB2_z:
                case OpCode.RMB3_z:
                case OpCode.RMB4_z:
                case OpCode.RMB5_z:
                case OpCode.RMB6_z:
                case OpCode.RMB7_z: { RMB((byte(instruction) & 0x70) >> 4, operand); }
                case OpCode.SMB0_z:
                case OpCode.SMB1_z:
                case OpCode.SMB2_z:
                case OpCode.SMB3_z:
                case OpCode.SMB4_z:
                case OpCode.SMB5_z:
                case OpCode.SMB6_z:
                case OpCode.SMB7_z: { SMB((byte(instruction) & 0x70) >> 4, operand); }
                                
                default: 
                { 
                    PrintLn("Instruction: " + (byte(instruction)).ToHexString(2)); Die(0x0A); 
                }
            } // switch
            pcRegister += length;
            break;
        }
    }
    
    Reset()
    {
        pcRegister = vectorReset;
    }
    
    string GetRegisterNames()
    {
        string registers = "PC   A  X  Y  SP [NV-BDIZC]  |  PC   SP  BP  CSP    ACC  TOP  NEXT IDX  IDY";
        return registers;
    }
    string GetRegisters()
    {
        string registers = pcRegister.ToHexString(4) + " " + 
                            aRegister.ToHexString(2) + " " + 
                            xRegister.ToHexString(2) + " " + 
                            yRegister.ToHexString(2) + " " + 
                           spRegister.ToHexString(2) + "  " +
                            (nFlag ? "1" : "0") +
                            (vFlag ? "1" : "0") + "-" +
                            (bFlag ? "1" : "0") +
                            (dFlag ? "1" : "0") +
                            (iFlag ? "1" : "0") +
                            (zFlag ? "1" : "0") +
                            (cFlag ? "1" : "0");
        
        uint pc   = GetMemory(ZP.ZPCL)   + (GetMemory(ZP.ZPCH) << 8);
        byte sp   = GetMemory(ZP.ZSP);
        byte bp   = GetMemory(ZP.ZBP);
        byte csp  = GetMemory(ZP.ZCSP);
        uint acc  = GetMemory(ZP.ZACCL)  + (GetMemory(ZP.ZACCH) << 8);
        uint top  = GetMemory(ZP.ZTOPL)  + (GetMemory(ZP.ZTOPH) << 8);
        uint next = GetMemory(ZP.ZNEXTL) + (GetMemory(ZP.ZNEXTH) << 8);
        uint idx  = GetMemory(ZP.ZIDXL)  + (GetMemory(ZP.ZIDXH) << 8);
        uint idy  = GetMemory(ZP.ZIDYL)  + (GetMemory(ZP.ZIDYH) << 8);
                                                
        registers += // the Hopper registers                   
        "      " + pc.ToHexString(4) + 
             " " + sp.ToHexString(2) + " " +
             " " + bp.ToHexString(2) + " " +
             " " + csp.ToHexString(2) + "    " +
             " " + acc.ToHexString(4) + 
             " " + top.ToHexString(4) + 
             " " + next.ToHexString(4) + 
             " " + idx.ToHexString(4) + 
             " " + idy.ToHexString(4) + " ";
        return registers;
    }
    
    
    
    long ms;
    byte GetMemory(uint address)
    {
        byte value;
        if (!EmulateAppleI && ((address >= ZT0) && (address <= ZT3)))
        {
            if (address == ZT3)
            {
                ms = Time.Millis; // update all 4 at the same time
            }
            byte index = byte(address-ZT0);
            value = ms.GetByte(index);
        }
        else 
        if (!ACIA.OfferRead(address, ref value))
        {
            if (EmulateAppleI && (address >= 0xD010) && (address <= 0xD013))
            {
                value = Emulator.GetAppleKBD(address);
            }
            else if (address >= 0xFFFA)
            {
                switch (address)
                {
                    case 0xFFFA: { value = byte(vectorNMI & 0xFF); }
                    case 0xFFFB: { value = byte(vectorNMI >> 8); }
                    case 0xFFFC: { value = byte(vectorReset & 0xFF); }
                    case 0xFFFD: { value = byte(vectorReset >> 8); }
                    case 0xFFFE: { value = byte(vectorIRQ & 0xFF); }
                    case 0xFFFF: { value = byte(vectorIRQ >> 8); }
                }
            }
            else
            {
                value = memory[address];
            }
        }
        return value;
    }
    SetMemory(uint address, byte value)
    {
        if (!ACIA.OfferWrite(address, value))
        {
            if (EmulateAppleI && (address >= 0xD010) && (address <= 0xD013))
            {
                Emulator.SetAppleDSP(address, value);
            }
            else
            {
                memory[address] = value;
            }
        }
        //if (address == ZCSP)
        //{
        //    Print(" " + pcRegister.ToHexString(4) + ":" + value.ToHexString(2) + " ", Colour.Red, Colour.Black);
        //}
    }
    ShowStack()
    {
        // stack is 0x0100 .. 0x01FF
        uint address = 0x0100 + spRegister;
        loop
        {
            if (address == 0x01FF) { break; }
            address++;
            PrintLn(address.ToHexString(4) + " " + (memory[address]).ToHexString(2));
        }
    }
}
