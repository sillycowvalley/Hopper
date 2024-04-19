unit CPU // Z80
{
    uses "/Source/Compiler/CODEGEN/AsmZ80"
    //uses "/Source/Debugger/6502/ZeroPage"
    
    uses "/Source/System/Screen"
    
    byte[0xFFFF] memory; // 0x0000..0xFFFE (not 0xFFFF)
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
    
    uint pcRegister;
    uint spRegister;
    
    byte  aRegister;
    uint  bcRegister;
    uint  deRegister;
    uint  hlRegister;
    uint  ixRegister;
    uint  iyRegister;
    
    byte  iRegister;
    byte  rRegister;
    
    bool sFlag;
    bool zFlag;
    bool nFlag;
    bool cFlag;
    
    bool iFF; // Interrupt Enable Flip-Flop  : true means enabled
        
    OpCode GetJSRInstruction()
    {
        return OpCode.CALL_nn;
    }
    
    <byte> GetRAM()
    {
        <byte> ram;
        for (uint i = 0; i < 0x8000; i++)
        {
            ram.Append(memory[i]);
        }
        return ram;
    }
    
    Push(uint value)
    {
        spRegister--;
        memory[spRegister] = byte(value >> 8);
        spRegister--;
        memory[spRegister] = byte(value & 0xFF);
    }
    uint Pop()
    {
        uint value = memory[spRegister];
        spRegister++;
        value += (memory[spRegister] << 8);
        spRegister++;
        return value;
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
        pcRegister = NMI;
        iFF = false;
    }
    doIRQ()
    {
        Push(pcRegister);
        pcRegister = IRQ;
        iFF = false;
    }
    
    OpCode GetOpCode(byte[] memory, uint address)
    {
        uint value = memory[address];
        if ((value == 0xFD) || (value == 0xDD) || (value == 0xCB) || (value == 0xED))
        {
            value = (value << 8) + memory[address+1];
        }
        return OpCode(value);
    }
    
    OpCode GetInstruction(uint address)
    {
        return GetOpCode(memory, address);
    }
    byte GetInstructionLength(OpCode instruction)
    {
        OperandType operandType;
        byte operandLength;
        bool signed;
        string name = GetOpCodeInfo(instruction, ref operandType, ref operandLength, ref signed, true);
        
        byte opCodeLength = GetOpCodeLength(instruction);
        
        return opCodeLength + operandLength;
    }
    
    CheckSZByte(byte value)
    {
        zFlag = (value == 0);
        sFlag = ((value & 0x80) != 0);
    }
    CheckSZWord(uint value)
    {
        zFlag = (value == 0);
        sFlag = ((value & 0x8000) != 0);
    }
    CheckSZWord(int value)
    {
        zFlag = (value == 0);
        sFlag = (value < 0);
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
        else if ((irqWaiting > 0) && iFF)
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
            
        OpCode instruction = GetOpCode(memory, pcRegister);
        OperandType operandType;
        byte operandLength;
        bool signed;
        string name = GetOpCodeInfo(instruction, ref operandType, ref operandLength, ref signed, true);
        byte opCodeLength = GetOpCodeLength(instruction);
        byte length = opCodeLength + operandLength;
        uint operand;
        int  offset;
        loop
        {
            switch (operandType)
            {
                case OperandType.Implied:
                {
                    // no operand
                }
                case OperandType.Immediate8:
                {
                    operand = memory[pcRegister + opCodeLength];
                }
                case OperandType.Immediate16:
                {
                    operand = memory[pcRegister + opCodeLength] + (memory[pcRegister + opCodeLength + 1] << 8);
                }
                case OperandType.ImmediateIndexed:
                {
                    operand = memory[pcRegister + opCodeLength] + (memory[pcRegister + opCodeLength + 1] << 8);
                }
                case OperandType.IndexedRelative: // (XY+d)
                {
                    operand = memory[pcRegister + opCodeLength];
                    if (operand & 0x80 == 0)
                    {
                        offset = Int.FromBytes(byte(operand & 0xFF), 0);
                    }
                    else
                    {
                        offset = Int.FromBytes(byte(operand & 0xFF), 0xFF);
                    }
                }
                case OperandType.Relative:
                {
                    operand = memory[pcRegister + opCodeLength];
                    if (operand & 0x80 == 0)
                    {
                        offset = Int.FromBytes(byte(operand & 0xFF), 0);
                    }
                    else
                    {
                        offset = Int.FromBytes(byte(operand & 0xFF), 0xFF);
                    }
                }
                default:
                {
                    PrintLn("OperandType: " + (byte(operandType)).ToHexString(2)); Die(0x0A); 
                }
            }
            switch (instruction)
            {
                // 'break' here means don't increment the pcRegister by instruction length
                case OpCode.NOP:         {  } 
                case OpCode.DI:          { iFF = true; }
                case OpCode.EI:          { iFF = false; }
                
                case OpCode.LD_A_n:      { aRegister  = byte(operand);                                }
                case OpCode.LD_B_n:      { bcRegister = (bcRegister & 0x00FF) + (byte(operand) << 8); }
                case OpCode.LD_C_n:      { bcRegister = (bcRegister & 0xFF00) + byte(operand);        }
                case OpCode.LD_D_n:      { deRegister = (deRegister & 0x00FF) + (byte(operand) << 8); }
                case OpCode.LD_E_n:      { deRegister = (deRegister & 0xFF00) + byte(operand);        }
                case OpCode.LD_H_n:      { hlRegister = (hlRegister & 0x00FF) + (byte(operand) << 8); }
                case OpCode.LD_L_n:      { hlRegister = (hlRegister & 0xFF00) + byte(operand);        }
                
                case OpCode.LD_SP_nn:    { spRegister = operand;       }
                case OpCode.LD_BC_nn:    { bcRegister = operand;       }
                case OpCode.LD_DE_nn:    { deRegister = operand;       }
                case OpCode.LD_HL_nn:    { hlRegister = operand;       }
                case OpCode.LD_IX_nn:    { ixRegister = operand;       }
                case OpCode.LD_IY_nn:    { iyRegister = operand;       }
                
                case OpCode.LD_A_B:      { aRegister  = byte(bcRegister >> 8);   }
                case OpCode.LD_A_C:      { aRegister  = byte(bcRegister & 0xFF); }
                case OpCode.LD_A_D:      { aRegister  = byte(deRegister >> 8);   }
                case OpCode.LD_A_E:      { aRegister  = byte(deRegister & 0xFF); }
                case OpCode.LD_A_H:      { aRegister  = byte(hlRegister >> 8);   }
                case OpCode.LD_A_L:      { aRegister  = byte(hlRegister & 0xFF); }
                
                case OpCode.LD_iIX_d_A:  { SetMemory(uint(long(ixRegister) + offset), aRegister);               }
                case OpCode.LD_iIX_d_B:  { SetMemory(uint(long(ixRegister) + offset), byte(bcRegister >> 8));   }
                case OpCode.LD_iIX_d_C:  { SetMemory(uint(long(ixRegister) + offset), byte(bcRegister & 0xFF)); }
                case OpCode.LD_iIX_d_D:  { SetMemory(uint(long(ixRegister) + offset), byte(deRegister >> 8));   }
                case OpCode.LD_iIX_d_E:  { SetMemory(uint(long(ixRegister) + offset), byte(deRegister & 0xFF)); }
                case OpCode.LD_iIX_d_H:  { SetMemory(uint(long(ixRegister) + offset), byte(hlRegister >> 8));   }
                case OpCode.LD_iIX_d_L:  { SetMemory(uint(long(ixRegister) + offset), byte(hlRegister & 0xFF)); }
                
                
                case OpCode.LD_iIY_d_A:  { SetMemory(uint(long(iyRegister) + offset), aRegister);               }
                case OpCode.LD_iIY_d_B:  { SetMemory(uint(long(iyRegister) + offset), byte(bcRegister >> 8));   }
                case OpCode.LD_iIY_d_C:  { SetMemory(uint(long(iyRegister) + offset), byte(bcRegister & 0xFF)); }
                case OpCode.LD_iIY_d_D:  { SetMemory(uint(long(iyRegister) + offset), byte(deRegister >> 8));   }
                case OpCode.LD_iIY_d_E:  { SetMemory(uint(long(iyRegister) + offset), byte(deRegister & 0xFF)); }
                case OpCode.LD_iIY_d_H:  { SetMemory(uint(long(iyRegister) + offset), byte(hlRegister >> 8));   }
                case OpCode.LD_iIY_d_L:  { SetMemory(uint(long(iyRegister) + offset), byte(hlRegister & 0xFF)); }
                
                case OpCode.LD_A_iHL:    { aRegister  = GetMemory(hlRegister);                                  }
                case OpCode.LD_B_iHL:    { bcRegister = (bcRegister & 0x00FF) + (GetMemory(hlRegister) << 8);   }
                case OpCode.LD_C_iHL:    { bcRegister = (bcRegister & 0xFF00) + GetMemory(hlRegister);          }
                case OpCode.LD_D_iHL:    { deRegister = (deRegister & 0x00FF) + (GetMemory(hlRegister) << 8);   }
                case OpCode.LD_E_iHL:    { deRegister = (deRegister & 0xFF00) + GetMemory(hlRegister);          }
                case OpCode.LD_H_iHL:    { hlRegister = (hlRegister & 0x00FF) + (GetMemory(hlRegister) << 8);   }
                case OpCode.LD_L_iHL:    { hlRegister = (hlRegister & 0xFF00) + GetMemory(hlRegister);          }
                
                case OpCode.LD_A_iIX_d:  { aRegister  = GetMemory(uint(long(ixRegister) + offset));                                }
                case OpCode.LD_B_iIX_d:  { bcRegister = (bcRegister & 0x00FF) + (GetMemory(uint(long(ixRegister) + offset)) << 8); }
                case OpCode.LD_C_iIX_d:  { bcRegister = (bcRegister & 0xFF00) + GetMemory(uint(long(ixRegister) + offset));        }
                case OpCode.LD_D_iIX_d:  { deRegister = (deRegister & 0x00FF) + (GetMemory(uint(long(ixRegister) + offset)) << 8); }
                case OpCode.LD_E_iIX_d:  { deRegister = (deRegister & 0xFF00) + GetMemory(uint(long(ixRegister) + offset));        }
                case OpCode.LD_H_iIX_d:  { hlRegister = (hlRegister & 0x00FF) + (GetMemory(uint(long(ixRegister) + offset)) << 8); }
                case OpCode.LD_L_iIX_d:  { hlRegister = (hlRegister & 0xFF00) + GetMemory(uint(long(ixRegister) + offset));        }
                
                case OpCode.LD_A_iIY_d:  { aRegister  = GetMemory(uint(long(iyRegister) + offset));                                }
                case OpCode.LD_B_iIY_d:  { bcRegister = (bcRegister & 0x00FF) + (GetMemory(uint(long(iyRegister) + offset)) << 8); }
                case OpCode.LD_C_iIY_d:  { bcRegister = (bcRegister & 0xFF00) + GetMemory(uint(long(iyRegister) + offset));        }
                case OpCode.LD_D_iIY_d:  { deRegister = (deRegister & 0x00FF) + (GetMemory(uint(long(iyRegister) + offset)) << 8); }
                case OpCode.LD_E_iIY_d:  { deRegister = (deRegister & 0xFF00) + GetMemory(uint(long(iyRegister) + offset));        }
                case OpCode.LD_H_iIY_d:  { hlRegister = (hlRegister & 0x00FF) + (GetMemory(uint(long(iyRegister) + offset)) << 8); }
                case OpCode.LD_L_iIY_d:  { hlRegister = (hlRegister & 0xFF00) + GetMemory(uint(long(iyRegister) + offset));        }
                
                case OpCode.LD_I_A:      { iRegister  = aRegister;     }
                case OpCode.LD_R_A:      { rRegister  = aRegister;     }
                
                case OpCode.LD_inn_A:    { SetMemory(operand, aRegister); } 
                
                case OpCode.LD_inn_SP:   { SetMemory(operand, byte(spRegister & 0xFF)); SetMemory(operand+1, byte(spRegister >> 8));  } 
                case OpCode.LD_inn_BC:   { SetMemory(operand, byte(bcRegister & 0xFF)); SetMemory(operand+1, byte(bcRegister >> 8));  } 
                case OpCode.LD_inn_DE:   { SetMemory(operand, byte(deRegister & 0xFF)); SetMemory(operand+1, byte(deRegister >> 8));  } 
                case OpCode.LD_inn_HL:   { SetMemory(operand, byte(hlRegister & 0xFF)); SetMemory(operand+1, byte(hlRegister >> 8));  } 
                case OpCode.LD_inn_IX:   { SetMemory(operand, byte(ixRegister & 0xFF)); SetMemory(operand+1, byte(ixRegister >> 8));  } 
                case OpCode.LD_inn_IY:   { SetMemory(operand, byte(iyRegister & 0xFF)); SetMemory(operand+1, byte(iyRegister >> 8));  }
                
                case OpCode.LD_SP_inn:   { spRegister = GetMemory(operand) + (GetMemory(operand+1) << 8);                             }
                case OpCode.LD_BC_inn:   { bcRegister = GetMemory(operand) + (GetMemory(operand+1) << 8);                             }
                case OpCode.LD_DE_inn:   { deRegister = GetMemory(operand) + (GetMemory(operand+1) << 8);                             }
                case OpCode.LD_HL_inn:   { hlRegister = GetMemory(operand) + (GetMemory(operand+1) << 8);                             }
                case OpCode.LD_IX_inn:   { ixRegister = GetMemory(operand) + (GetMemory(operand+1) << 8);                             }
                case OpCode.LD_IY_inn:   { iyRegister = GetMemory(operand) + (GetMemory(operand+1) << 8);                             }
                
                case OpCode.XOR_A_A:     { aRegister  = aRegister ^ aRegister;               CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.XOR_A_B:     { aRegister  = aRegister ^ byte(bcRegister >> 8);   CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.XOR_A_C:     { aRegister  = aRegister ^ byte(bcRegister & 0xFF); CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.XOR_A_D:     { aRegister  = aRegister ^ byte(deRegister >> 8);   CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.XOR_A_E:     { aRegister  = aRegister ^ byte(deRegister & 0xFF); CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.XOR_A_H:     { aRegister  = aRegister ^ byte(hlRegister >> 8);   CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.XOR_A_L:     { aRegister  = aRegister ^ byte(hlRegister & 0xFF); CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                
                case OpCode.OR_A_A:      { aRegister  = aRegister | aRegister;               CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.OR_A_B:      { aRegister  = aRegister | byte(bcRegister >> 8);   CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.OR_A_C:      { aRegister  = aRegister | byte(bcRegister & 0xFF); CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.OR_A_D:      { aRegister  = aRegister | byte(deRegister >> 8);   CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.OR_A_E:      { aRegister  = aRegister | byte(deRegister & 0xFF); CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.OR_A_H:      { aRegister  = aRegister | byte(hlRegister >> 8);   CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.OR_A_L:      { aRegister  = aRegister | byte(hlRegister & 0xFF); CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                
                case OpCode.AND_A_A:     { aRegister  = aRegister & aRegister;               CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.AND_A_B:     { aRegister  = aRegister & byte(bcRegister >> 8);   CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.AND_A_C:     { aRegister  = aRegister & byte(bcRegister & 0xFF); CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.AND_A_D:     { aRegister  = aRegister & byte(deRegister >> 8);   CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.AND_A_E:     { aRegister  = aRegister & byte(deRegister & 0xFF); CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.AND_A_H:     { aRegister  = aRegister & byte(hlRegister >> 8);   CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                case OpCode.AND_A_L:     { aRegister  = aRegister & byte(hlRegister & 0xFF); CheckSZByte(aRegister); nFlag = false; cFlag = false; }
                
                case OpCode.RLA:         { bool cNew = ((aRegister & 0x80) != 0); aRegister = byte(((aRegister << 1) & 0xFF) + (cFlag ? 1 : 0)); nFlag = false; cFlag = cNew; }
                case OpCode.CLP:         { aRegister = ~aRegister; nFlag = true; }
                
                case OpCode.ADC_HL_BC:   { long result = long(hlRegister) + long(bcRegister) + (cFlag ? 1 : 0); 
                                           cFlag = (result > 0xFFFF);
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           CheckSZWord(hlRegister); nFlag = false; 
                                         }
                case OpCode.ADC_HL_DE:   { long result = long(hlRegister) + long(deRegister) + (cFlag ? 1 : 0); 
                                           cFlag = (result > 0xFFFF);
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           CheckSZWord(hlRegister); nFlag = false; 
                                         }
                case OpCode.ADC_HL_SP:   { long result = long(hlRegister) + long(spRegister) + (cFlag ? 1 : 0); 
                                           cFlag = (result > 0xFFFF);
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           CheckSZWord(hlRegister); nFlag = false; 
                                         }
                case OpCode.ADC_HL_HL:   { long result = long(hlRegister) + long(hlRegister) + (cFlag ? 1 : 0); 
                                           cFlag = (result > 0xFFFF);
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           CheckSZWord(hlRegister); nFlag = false; 
                                         }
                
                case OpCode.ADD_HL_BC:   { long result = long(hlRegister) + long(bcRegister); 
                                           cFlag = (result > 0xFFFF);
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           nFlag = false; 
                                         }
                case OpCode.ADD_HL_DE:   { long result = long(hlRegister) + long(deRegister); 
                                           cFlag = (result > 0xFFFF);
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           nFlag = false; 
                                         }
                case OpCode.ADD_HL_SP:   { long result = long(hlRegister) + long(spRegister); 
                                           cFlag = (result > 0xFFFF);
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           nFlag = false; 
                                         }
                case OpCode.ADD_HL_HL:   { long result = long(hlRegister) + long(hlRegister); 
                                           cFlag = (result > 0xFFFF);
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           nFlag = false; 
                                         }
                case OpCode.SBC_HL_BC:   { long result = long(hlRegister) - long(bcRegister) - (cFlag ? 1 : 0);
                                           cFlag = (bcRegister > hlRegister);
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           CheckSZWord(hlRegister); nFlag = false; 
                                         }
                case OpCode.SBC_HL_DE:   { long result = long(hlRegister) - long(deRegister) - (cFlag ? 1 : 0);
                                           cFlag = (deRegister > hlRegister);
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           CheckSZWord(hlRegister); nFlag = false; 
                                         }
                case OpCode.SBC_HL_HL:   { long result = long(hlRegister) - long(hlRegister) - (cFlag ? 1 : 0);
                                           cFlag = false;
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           CheckSZWord(hlRegister); nFlag = false; 
                                         }
                case OpCode.SBC_HL_SP:   { long result = long(hlRegister) - long(spRegister) - (cFlag ? 1 : 0);
                                           cFlag = (spRegister > hlRegister);
                                           hlRegister = UInt.FromBytes(result.GetByte(0), result.GetByte(1));
                                           CheckSZWord(hlRegister); nFlag = false; 
                                         }
                
                case OpCode.CP_A_B:      { int  value = int(aRegister) - byte(bcRegister >> 8);   CheckSZWord(value); nFlag = false; cFlag = (value < 0); }
                case OpCode.CP_A_C:      { int  value = int(aRegister) - byte(bcRegister & 0xFF); CheckSZWord(value); nFlag = false; cFlag = (value < 0); }
                case OpCode.CP_A_D:      { int  value = int(aRegister) - byte(deRegister >> 8);   CheckSZWord(value); nFlag = false; cFlag = (value < 0); }
                case OpCode.CP_A_E:      { int  value = int(aRegister) - byte(deRegister & 0xFF); CheckSZWord(value); nFlag = false; cFlag = (value < 0); }
                case OpCode.CP_A_H:      { int  value = int(aRegister) - byte(hlRegister >> 8);   CheckSZWord(value); nFlag = false; cFlag = (value < 0); }
                case OpCode.CP_A_L:      { int  value = int(aRegister) - byte(hlRegister & 0xFF); CheckSZWord(value); nFlag = false; cFlag = (value < 0); }
                
                
                case OpCode.CALL_nn:     { Push(pcRegister + length); pcRegister = operand; break; }
                case OpCode.RET:         { pcRegister = Pop();                              break; }
                
                case OpCode.JP_nn:       { pcRegister = operand; break;                            }
                
                case OpCode.JR_Z_e:      { if (zFlag)  { pcRegister = uint(long(pcRegister + length) + offset); break; } }
                case OpCode.JR_NZ_e:     { if (!zFlag) { pcRegister = uint(long(pcRegister + length) + offset); break; } }
                case OpCode.JR_C_e:      { if (cFlag)  { pcRegister = uint(long(pcRegister + length) + offset); break; } }
                case OpCode.JR_NC_e:     { if (!cFlag) { pcRegister = uint(long(pcRegister + length) + offset); break; } }
                
                case OpCode.DJNZ_e:      { byte b = byte(bcRegister >> 8);
                                           if (b != 0) 
                                           {
                                               b--;
                                               bcRegister = (bcRegister & 0x00FF) + (b << 8);
                                               pcRegister = uint(long(pcRegister + length) + offset);
                                               break; 
                                           } 
                                         }
                
                case OpCode.PUSH_BC:     { Push(bcRegister);                                       }
                case OpCode.PUSH_DE:     { Push(deRegister);                                       }
                case OpCode.PUSH_HL:     { Push(hlRegister);                                       }
                case OpCode.PUSH_IY:     { Push(iyRegister);                                       }
                case OpCode.PUSH_IX:     { Push(ixRegister);                                       }
                
                case OpCode.POP_BC:      { bcRegister = Pop();                                     }
                case OpCode.POP_DE:      { deRegister = Pop();                                     }
                case OpCode.POP_HL:      { hlRegister = Pop();                                     }
                case OpCode.POP_IY:      { iyRegister = Pop();                                     }
                case OpCode.POP_IX:      { ixRegister = Pop();                                     }
                
                default: 
                { 
                    PrintLn("Instruction: " + (byte(instruction)).ToHexString(2) + ", OperandType: " + (byte(operandType)).ToHexString(2)); Die(0x0A); 
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
        // TODO
         //  = "PC   A  X  Y  SP [SZ----NC]  |  PC   SP  BP  CSP    ACC  TOP  NEXT IDX  IDY";
        string registers = "PC   A  BC   DE   HL   IX   IY   SP  [SZ----NC]";
        return registers;
    }
    string GetRegisters()
    {
        // TODO
        string registers = pcRegister.ToHexString(4) + " " 
                         + aRegister.ToHexString(2) + " " 
                         + bcRegister.ToHexString(4) + " " 
                         + deRegister.ToHexString(4) + " " 
                         + hlRegister.ToHexString(4) + " " 
                         + ixRegister.ToHexString(4) + " " 
                         + iyRegister.ToHexString(4) + " " 
                         + spRegister.ToHexString(4) + " " +
                            (sFlag ? "1" : "0") +
                            (zFlag ? "1" : "0") + "----" +
                            (nFlag ? "1" : "0") +
                            (cFlag ? "1" : "0");
        
       return registers;
    }
    
    
    
    long ms;
    byte GetMemory(uint address)
    {
        byte value;
        if (!EmulateAppleI && ((address >= ZT0) && (address <= ZT3)))
        {
            // TODO: IO vs Memory
            if (address == ZT0)
            {
                ms = Time.Millis; // update all 4 at the same time
            }
            byte index = byte(address-ZT0);
            value = ms.GetByte(index);
        }
        else 
        if (!ACIA.OfferRead(address, ref value))
        {
            // TODO: IO vs Memory
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
            // TODO: IO vs Memory
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
        // TODO: stack location
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
