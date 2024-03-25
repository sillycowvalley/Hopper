unit W65C02
{
    uses "/Source/Compiler/CODEGEN/Asm6502"
    uses "/Source/Debugger/6502/ZeroPage"
    
    byte[0xFFFA] memory;
    uint vectorReset;
    uint vectorIRQ;
    uint vectorNMI;
    
    uint Reset { get { return vectorReset; } set { vectorReset = value; } }
    uint IRQ   { get { return vectorIRQ;   } set { vectorIRQ = value;   } }
    uint NMI   { get { return vectorNMI;   } set { vectorNMI = value;   } }
    uint PC    { get { return pcRegister;  } set { pcRegister = value;  } }
    
    int nmiWaiting;
    int irqWaiting;
    
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
        PrintLn();
        PrintLn("6502 Panic at PC=0x" + pcRegister.ToHexString(4) 
                           + ", A=0x" + aRegister.ToHexString(2)
                           + ", X=0x" + xRegister.ToHexString(2)
                           + ", Y=0x" + yRegister.ToHexString(2)
                           , Colour.Red, Colour.Black);
        PrintLn();
        ACIA.Close();
        Die(0x0B); 
        // ####
        
        Push(pcRegister+1);
        PushFlags();
        pcRegister = IRQ;
        iFlag = true;
        bFlag = true;
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
    
    Execute()
    {
        if (pcRegister == InvalidAddress) { return; }
        
        // check for IRQ and NMI:
        if (nmiWaiting > 0)
        {
            nmiWaiting--;
            doNMI();
        }
        else if ((irqWaiting > 0) && !iFlag)
        {
            irqWaiting--;
            doIRQ();   
        }
        
        
        byte instruction               = memory[pcRegister];
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
                operand = memory[pcRegister+1] + (memory[pcRegister+2] << 8);
                Die(0x0A);
            }
            case AddressingModes.AbsoluteIndirectX: // (nnnn,X)
            {
                operand = memory[pcRegister+1] + (memory[pcRegister+2] << 8);
                //PrintLn("operand=" + operand.ToHexString(4));
                operand = operand + xRegister;
                //PrintLn("operand+xRegister=" + operand.ToHexString(4));
                uint address = memory[operand] + (memory[operand+1] << 8);
                //PrintLn("address=" + address.ToHexString(4));
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
                case 0x20: { JSR(operand); break; } 
                case 0x60: { RTS();        break; }
                case 0x40: { RTI();        break; }
                case 0x00: { BRK();        break; }
                case 0x4C: { JMP(operand); break; } 
                case 0x7C: { JMPIndexed(operand); break; } 
                case 0x80: { BRA(offset);  break; }
                
                case 0x90: { if (!cFlag) { BRA(offset); break; } } // BCC
                case 0xB0: { if (cFlag)  { BRA(offset); break; } } // BCS
                case 0xF0: { if (zFlag)  { BRA(offset); break; } } // BEQ
                case 0x30: { if (nFlag)  { BRA(offset); break; } } // BMI
                case 0xD0: { if (!zFlag) { BRA(offset); break; } } // BNE
                case 0x10: { if (!nFlag) { BRA(offset); break; } } // BPL
                case 0x50: { if (!vFlag) { BRA(offset); break; } } // BVC
                case 0x70: { if (vFlag)  { BRA(offset); break; } } // BVS
                
                
                case 0x48: { Push(aRegister);   } // PHA
                case 0xDA: { Push(xRegister);   } // PHX
                case 0x5A: { Push(yRegister);   } // PHY
                case 0x08: { PushFlags();       } // PHP
                case 0x68: { aRegister = Pop(); CheckNZ(aRegister); } // PLA
                case 0xFA: { xRegister = Pop(); CheckNZ(xRegister); } // PLX
                case 0x7A: { yRegister = Pop(); CheckNZ(yRegister); } // PLY
                case 0x28: { PopFlags();       } // PLP
                
                case 0x18: { cFlag = false; } // CLC
                case 0xD8: { dFlag = false; } // CLD
                case 0x58: { iFlag = false; } // CLI
                case 0xB8: { vFlag = false; } // CLV
                
                case 0x38: { cFlag = true; } // SEC
                case 0xF8: { dFlag = true; } // SED
                case 0x78: { iFlag = true; } // SEI
                
                case 0xAA: { xRegister = aRegister;  CheckNZ(xRegister); } // TAX
                case 0xA8: { yRegister = aRegister;  CheckNZ(yRegister); } // TAY
                case 0x8A: { aRegister = xRegister;  CheckNZ(aRegister); } // TXA
                case 0x98: { aRegister = yRegister;  CheckNZ(aRegister); } // TYA
                case 0x9A: { spRegister = xRegister;                     } // TXS
                case 0xBA: { xRegister = spRegister; CheckNZ(xRegister); } // TSX
                
                case 0x29: { aRegister &= byte(operand);   CheckNZ(aRegister); }    // AND #nn
                case 0x2D:                                                          // AND nnnn
                case 0x3D:                                                          // AND nnnn,X
                case 0x39:                                                          // AND nnnn,Y
                case 0x25:                                                          // AND nn
                case 0x35:                                                          // AND nn,X
                case 0x32:                                                          // AND (nn)
                case 0x21:                                                          // AND (nn,X)
                case 0x31: { aRegister &= GetMemory(operand); CheckNZ(aRegister); } // AND (nn),Y
                
                case 0x09: { aRegister |= byte(operand);   CheckNZ(aRegister); }    // OR #nn
                case 0x0D:                                                          // OR nnnn
                case 0x1D:                                                          // OR nnnn,X
                case 0x19:                                                          // OR nnnn,Y
                case 0x05:                                                          // OR nn
                case 0x15:                                                          // OR nn,X
                case 0x12:                                                          // OR (nn)
                case 0x01:                                                          // OR (nn,X)
                case 0x11: { aRegister |= GetMemory(operand); CheckNZ(aRegister); } // OR (nn),Y
                
                case 0x69: { ADC(byte(operand));              CheckNZ(aRegister); } // ADC #nn
                case 0x6D:                                                          // ADC nnnn
                case 0x7D:                                                          // ADC nnnn,X
                case 0x79:                                                          // ADC nnnn,Y
                case 0x65:                                                          // ADC nn
                case 0x75:                                                          // ADC nn,X
                case 0x72:                                                          // ADC (nn)
                case 0x61:                                                          // ADC (nn,X)
                case 0x71: { ADC(GetMemory(operand));         CheckNZ(aRegister); } // ADC (nn),Y
                
                case 0xE9: { SBC(byte(operand));              CheckNZ(aRegister); } // SBC #nn
                case 0xED:                                                          // SBC nnnn
                case 0xFD:                                                          // SBC nnnn,X
                case 0xF9:                                                          // SBC nnnn,Y
                case 0xE5:                                                          // SBC nn
                case 0xF5:                                                          // SBC nn,X
                case 0xF2:                                                          // SBC (nn)
                case 0xE1:                                                          // SBC (nn,X)
                case 0xF1: { SBC(GetMemory(operand));         CheckNZ(aRegister); } // SBC (nn),Y
                
                case 0x4A: 
                { 
                    cFlag = ((aRegister & 0x01) != 0);
                    aRegister = aRegister >> 1; CheckNZ(aRegister);                 // LSR A
                }
                case 0x4E:                                                          // LSR nnnn
                case 0x5E:                                                          // LSR nnnn,X
                case 0x46:                                                          // LSR nn
                case 0x56:                                                          // LSR nn, X
                { 
                    byte value = GetMemory(operand);
                    cFlag = ((value & 0x01) != 0);
                    value = value >> 1; CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case 0x0A: 
                { 
                    cFlag     = ((aRegister & 0x80) != 0);
                    aRegister = (aRegister << 1) & 0xFF; CheckNZ(aRegister);        // ASL A
                }
                case 0x0E:                                                          // ASL nnnn
                case 0x1E:                                                          // ASL nnnn,X
                case 0x06:                                                          // ASL nn
                case 0x16:                                                          // ASL nn, X
                { 
                    byte value = GetMemory(operand);
                    cFlag = ((value & 0x80) != 0);
                    value = (value << 1) & 0xFF; CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case 0x2A: 
                { 
                    bool cAfter = ((aRegister & 0x80) != 0);
                    aRegister   = ((aRegister << 1) & 0xFF) | (cFlag ? 1 : 0); 
                    cFlag       = cAfter;
                    CheckNZ(aRegister);                                             // ROL A
                }
                case 0x2E:                                                          // ROL nnnn
                case 0x3E:                                                          // ROL nnnn,X
                case 0x26:                                                          // ROL nn
                case 0x36:                                                          // ROL nn, X
                { 
                    byte value = GetMemory(operand);
                    bool cAfter = ((value & 0x80) != 0);
                    value = ((value << 1) & 0xFF) | (cFlag ? 1 : 0); 
                    cFlag = cAfter; 
                    CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case 0x6A: 
                { 
                    bool cAfter = ((aRegister & 0x01) != 0);
                    aRegister   = (aRegister >> 1) | (cFlag ? 0x80 : 0); 
                    cFlag       = cAfter;
                    CheckNZ(aRegister);                                             // ROR A
                }
                case 0x6E:                                                          // ROR nnnn
                case 0x7E:                                                          // ROR nnnn,X
                case 0x66:                                                          // ROR nn
                case 0x76:                                                          // ROR nn, X
                { 
                    byte value  = GetMemory(operand);
                    bool cAfter = ((value & 0x01) != 0);
                    value       = (value >> 1) | (cFlag ? 0x80 : 0); 
                    cFlag       = cAfter;
                    CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case 0x1A: 
                { 
                    if (aRegister == 0xFF) 
                    { aRegister = 0; } 
                    else
                    { aRegister++; }   CheckNZ(aRegister);                          // INC A
                }
                case 0xC8: 
                { 
                    if (yRegister == 0xFF) 
                    { yRegister = 0; } 
                    else
                    { yRegister++; }   CheckNZ(yRegister);                          // INC Y
                }
                case 0xE8: 
                { 
                    if (xRegister == 0xFF) 
                    { xRegister = 0; } 
                    else
                    { xRegister++; }   CheckNZ(xRegister);                          // INC X
                }
                case 0xEE:                                                          // INC nnnn
                case 0xFE:                                                          // INC nnnn,X
                case 0xE6:                                                          // INC nn
                case 0xF6:                                                          // INC nn, X
                { 
                    byte value = GetMemory(operand);
                    if (value == 0xFF) 
                    { value = 0; } 
                    else
                    { value++; }   CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case 0x3A: 
                { 
                    if (aRegister == 0) 
                    { aRegister = 0xFF; } 
                    else
                    { aRegister--; }   CheckNZ(aRegister);                          // DEC A
                }
                case 0x88: 
                { 
                    if (yRegister == 0) 
                    { yRegister = 0xFF; } 
                    else
                    { yRegister--; }   CheckNZ(yRegister);                          // DEC Y
                }
                case 0xCA: 
                { 
                    if (xRegister == 0) 
                    { xRegister = 0xFF; } 
                    else
                    { xRegister--; }   CheckNZ(xRegister);                          // DEC X
                }
                case 0xCE:                                                          // DEC nnnn
                case 0xDE:                                                          // DEC nnnn,X
                case 0xC6:                                                          // DEC nn
                case 0xD6:                                                          // DEC nn, X
                { 
                    byte value = GetMemory(operand);
                    if (value == 0) 
                    { value = 0xFF; } 
                    else
                    { value--; }   CheckNZ(value); 
                    SetMemory(operand, value);                         
                }
                
                case 0xA9: { aRegister = byte(operand);   CheckNZ(aRegister); }    // LDA #nn
                case 0xAD:                                                         // LDA nnnn
                case 0xBD:                                                         // LDA nnnn,X
                case 0xB9:                                                         // LDA nnnn,Y
                case 0xA5:                                                         // LDA nn
                case 0xB5:                                                         // LDA nn,X
                case 0xB2:                                                         // LDA (nn)
                case 0xA1:                                                         // LDA (nn,X)
                case 0xB1: { aRegister = GetMemory(operand); CheckNZ(aRegister); } // LDA (nn),Y
                
                case 0xA2: { xRegister = byte(operand);   CheckNZ(xRegister); }    // LDX #nn
                case 0xAE:                                                         // LDX nnnn
                case 0xBE:                                                         // LDX nnnn,Y
                case 0xA6:                                                         // LDX nn
                case 0xB6: { xRegister = GetMemory(operand); CheckNZ(xRegister); } // LDX nn,Y
                
                case 0xA0: { yRegister = byte(operand);   CheckNZ(yRegister); }    // LDY #nn
                case 0xAC:                                                         // LDY nnnn
                case 0xBC:                                                         // LDY nnnn,X
                case 0xA4:                                                         // LDY nn
                case 0xB4: { yRegister = GetMemory(operand); CheckNZ(yRegister); } // LDY nn,X
                
                case 0x8D:                                                        // STA nnnn
                case 0x9D:                                                        // STA nnnn,X
                case 0x99:                                                        // STA nnnn,Y
                case 0x85:                                                        // STA nn
                case 0x95:                                                        // STA nn,X
                case 0x92:                                                        // STA (nn)
                case 0x81:                                                        // STA (nn,X)
                case 0x91: { SetMemory(operand, aRegister);                     } // STA (nn),Y
                
                case 0x8E:                                                        // STX nnnn
                case 0x86:                                                        // STX nn
                case 0x96: { SetMemory(operand, xRegister);                    }  // STX nn,Y
                
                case 0x8C:                                                        // STY nnnn
                case 0x84:                                                        // STY nn
                case 0x94: { SetMemory(operand, yRegister);                    }  // STY nn,X
                
                case 0x9C:                                                        // STZ nnnn
                case 0x9E:                                                        // STZ nnnn,X
                case 0x64:                                                        // STZ nn
                case 0x74: { SetMemory(operand, 0);                             } // STZ nn,X
                
                case 0xC9: { Compare(aRegister, byte(operand));               } // CMP #nn
                case 0xCD:                                                      // CMP nnnn
                case 0xDD:                                                      // CMP nnnn,X
                case 0xD9:                                                      // CMP nnnn,Y
                case 0xC5:                                                      // CMP nn
                case 0xD5:                                                      // CMP nn,X
                case 0xD2:                                                      // CMP (nn)
                case 0xC1:                                                      // CMP (nn,X)
                case 0xD1: { Compare(aRegister, GetMemory(operand));          } // CMP (nn),Y
                
                case 0xE0: { Compare(xRegister, byte(operand));               } // CPX #nn
                case 0xEC:                                                      // CPX nnnn
                case 0xE4: { Compare(xRegister, GetMemory(operand));          } // CPX nn
                
                case 0xC0: { Compare(yRegister, byte(operand));               } // CPY #nn
                case 0xCC:                                                      // CPY nnnn
                case 0xC4: { Compare(yRegister, GetMemory(operand));          } // CPY nn
                
                case 0xDB: { pcRegister = InvalidAddress; break;              } // STP
                
                case 0x0F:
                case 0x1F:
                case 0x2F:
                case 0x3F:
                case 0x4F:
                case 0x5F:
                case 0x6F:
                case 0x7F: { if (BBR((instruction & 0x70) >> 4, operand, offset)) { break; } }
                case 0x8F:
                case 0x9F:
                case 0xAF:
                case 0xBF:
                case 0xCF:
                case 0xDF:
                case 0xEF:
                case 0xFF: { if (BBS((instruction & 0x70) >> 4, operand, offset)) { break; } }
                
                case 0x07:
                case 0x17:
                case 0x27:
                case 0x37:
                case 0x47:
                case 0x57:
                case 0x67:
                case 0x77: { RMB((instruction & 0x70) >> 4, operand); }
                case 0x87:
                case 0x97:
                case 0xA7:
                case 0xB7:
                case 0xC7:
                case 0xD7:
                case 0xE7:
                case 0xF7: { SMB((instruction & 0x70) >> 4, operand); }
                                
                default: 
                { 
                    PrintLn("Instruction: " + instruction.ToHexString(2)); Die(0x0A); 
                }
            }
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
    byte GetMemory(uint address)
    {
        byte value;
        if (!ACIA.OfferRead(address, ref value))
        {
            if (address >= 0xFFFA)
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
            memory[address] = value;
        }
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
