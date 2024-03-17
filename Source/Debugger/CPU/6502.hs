unit W65C02
{
    uses "/Source/Compiler/CODEGEN/OpCodes"
    
    byte[0xFFFA] memory;
    uint vectorReset;
    uint vectorIRQ;
    uint vectorNMI;
    
    uint Reset { get { return vectorReset; } set { vectorReset = value; } }
    uint IRQ   { get { return vectorIRQ;   } set { vectorIRQ = value;   } }
    uint NMI   { get { return vectorNMI;   } set { vectorNMI = value;   } }
    uint PC    { get { return pcRegister;  } set { pcRegister = value;  } }
    
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
        zFlag = (value == 0);
        nFlag = (value & 0x80 != 0);
        cFlag = (value >= 0);
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
    
    JMP(uint operand)
    {
        pcRegister = operand;
    }
    BRA(int offset)
    {
        pcRegister = uint(long(pcRegister) + 2 + offset);
    }
    JSR(uint operand)
    {
        pcRegister += 2;
        Push(pcRegister);
        pcRegister = operand;
    }
    RTS()
    {
        pcRegister = Pop() + (Pop() << 8);
        pcRegister++;
    }
    
    Execute()
    {
        byte instruction = memory[pcRegister];
        byte length                    = AsmStream.GetInstructionLength(instruction);
        AddressingModes addressingMode = OpCodes.GetAddressingMode(instruction);
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
                operand = memory[pcRegister+1] + memory[pcRegister+2] << 8;
            }
            case AddressingModes.AbsoluteX:         //  nnnn,X
            {
                operand = memory[pcRegister+1] + memory[pcRegister+2] << 8 + xRegister;
            }
            case AddressingModes.AbsoluteY:         //  nnnn,Y
            {
                operand = memory[pcRegister+1] + memory[pcRegister+2] << 8 + yRegister;
            }
            case AddressingModes.AbsoluteIndirect:  // (nnnn)
            {
                operand = memory[pcRegister+1] + memory[pcRegister+2] << 8;
                Die(0x0A);
            }
            case AddressingModes.AbsoluteIndirectX: // (nnnn,X)
            {
                operand = memory[pcRegister+1] + memory[pcRegister+2] << 8;
                Die(0x0A);
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
                operand = (memory[operand] + memory[operand+1] << 8);
            }
            case AddressingModes.XIndexedZeroPage:  // (nn,X)
            {
                operand = (memory[pcRegister+1] + xRegister) & 0xFF;
                operand = (memory[operand] + memory[operand+1] << 8);
            }
            case AddressingModes.YIndexedZeroPage:  // (nn), Y
            {
                operand = memory[pcRegister+1];
                operand = (memory[operand] + memory[operand+1] << 8 + yRegister);
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
                case 0x4C: { JMP(operand); break; } 
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
                case 0x68: { aRegister = Pop(); CheckNZ(aRegister); } // PLA
                case 0xFA: { xRegister = Pop(); CheckNZ(xRegister); } // PLX
                case 0x7A: { yRegister = Pop(); CheckNZ(yRegister); } // PLY
                
                case 0x18: { cFlag = false; } // CLC
                case 0xD8: { dFlag = false; } // CLD
                case 0x58: { iFlag = false; } // CLI
                case 0xB8: { vFlag = false; } // CLV
                
                case 0x38: { cFlag = true; } // SEC
                case 0xF8: { dFlag = true; } // SED
                case 0x78: { iFlag = true; } // SEI
                
                case 0xAA: { xRegister = aRegister; CheckNZ(xRegister); } // TAX
                case 0xA8: { yRegister = aRegister; CheckNZ(yRegister); } // TAY
                case 0x8A: { aRegister = xRegister; CheckNZ(aRegister); } // TXA
                case 0x9A: { aRegister = yRegister; CheckNZ(aRegister); } // TYA
                
                case 0xA9: { aRegister = byte(operand);   CheckNZ(aRegister); } // LDA #nn
                case 0xAD:                                                      // LDA nnnn
                case 0xBD:                                                      // LDA nnnn,X
                case 0xB9:                                                      // LDA nnnn,Y
                case 0xA5:                                                      // LDA nn
                case 0xB5:                                                      // LDA nn,X
                case 0xB2:                                                      // LDA (nn)
                case 0xA1:                                                      // LDA (nn,X)
                case 0xB1: { aRegister = memory[operand]; CheckNZ(aRegister); } // LDA (nn),Y
                
                case 0xA2: { xRegister = byte(operand);   CheckNZ(xRegister); } // LDX #nn
                case 0xAE:                                                      // LDX nnnn
                case 0xBE:                                                      // LDX nnnn,Y
                case 0xA6:                                                      // LDX nn
                case 0xB6: { xRegister = memory[operand]; CheckNZ(xRegister); } // LDX nn,Y
                
                case 0xA0: { yRegister = byte(operand);   CheckNZ(yRegister); } // LDY #nn
                case 0xAC:                                                      // LDY nnnn
                case 0xBC:                                                      // LDY nnnn,X
                case 0xA4:                                                      // LDY nn
                case 0xB4: { yRegister = memory[operand]; CheckNZ(yRegister); } // LDY nn,X
                
                case 0x8D:                                                      // STA nnnn
                case 0x9D:                                                      // STA nnnn,X
                case 0x99:                                                      // STA nnnn,Y
                case 0x85:                                                      // STA nn
                case 0x95:                                                      // STA nn,X
                case 0x92:                                                      // STA (nn)
                case 0x81:                                                      // STA (nn,X)
                case 0x91: { memory[operand] = aRegister;                     } // STA (nn),Y
                
                case 0x8E:                                                      // STX nnnn
                case 0x86:                                                      // STX nn
                case 0x96: { memory[operand] = xRegister;                     } // STX nn,Y
                
                case 0x8C:                                                      // STY nnnn
                case 0x84:                                                      // STY nn
                case 0x94: { memory[operand] = yRegister;                     } // STY nn,X
                
                case 0x9C:                                                      // STZ nnnn
                case 0x9E:                                                      // STZ nnnn,X
                case 0x64:                                                      // STZ nn
                case 0x74: { memory[operand] = 0;                             } // STZ nn,X
                
                case 0xC9: { Compare(aRegister, byte(operand));               } // CMP #nn
                case 0xCD:                                                      // CMP nnnn
                case 0xDD:                                                      // CMP nnnn,X
                case 0xD9:                                                      // CMP nnnn,Y
                case 0xC5:                                                      // CMP nn
                case 0xD5:                                                      // CMP nn,X
                case 0xD2:                                                      // CMP (nn)
                case 0xC1:                                                      // CMP (nn,X)
                case 0xD1: { Compare(aRegister, memory[operand]);             } // CMP (nn),Y
                
                case 0xE0: { Compare(xRegister, byte(operand));               } // CPX #nn
                case 0xEC:                                                      // CPX nnnn
                case 0xE4: { Compare(xRegister, memory[operand]);             } // CPX nn
                
                case 0xC0: { Compare(yRegister, byte(operand));               } // CPY #nn
                case 0xCC:                                                      // CPY nnnn
                case 0xC4: { Compare(yRegister, memory[operand]);             } // CPY nn
                                
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
        string registers = "PC   A  X  I  SP [NV-BDIZC]";
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
                            (vFlag ? "1-" : "0-") +
                            (bFlag ? "1" : "0") +
                            (dFlag ? "1" : "0") +
                            (iFlag ? "1" : "0") +
                            (zFlag ? "1" : "0") +
                            (cFlag ? "1" : "0");
        return registers;
    }
    byte GetMemory(uint address)
    {
        if (address >= 0xFFFA)
        {
            switch (address)
            {
                case 0xFFFA: { return byte(vectorNMI & 0xFF); }
                case 0xFFFB: { return byte(vectorNMI >> 8); }
                case 0xFFFC: { return byte(vectorReset & 0xFF); }
                case 0xFFFD: { return byte(vectorReset >> 8); }
                case 0xFFFE: { return byte(vectorIRQ & 0xFF); }
                case 0xFFFF: { return byte(vectorIRQ >> 8); }
            }
        }
        return memory[address];
    }
    SetMemory(uint address, byte value)
    {
        memory[address] = value;
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
