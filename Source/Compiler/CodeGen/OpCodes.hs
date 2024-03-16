unit OpCodes
{
    CPUArchitecture cpuArchitecture;
    CPUArchitecture Architecture { get { return cpuArchitecture; } set { cpuArchitecture = value; } }
    
    flags AddressingModes
    {
        None=0,
        Implied=0x0001,
        Accumulator=0x0002,       // A
        Immediate=0x0004,         // #nn
        Absolute=0x0008,          // nnnn
        AbsoluteX=0x0010,         // nnnn,X
        AbsoluteY=0x0020,         // nnnn,Y
        AbsoluteIndirect=0x0040,  // (nnnn)
        AbsoluteIndirectX=0x0080, // (nnnn,X)
        ZeroPage=0x0100,          // nn
        ZeroPageX=0x0200,         // nn,X
        ZeroPageY=0x0400,         // nn,Y
        ZeroPageIndirect=0x0800,  // (nn)
        XIndexedZeroPage=0x1000,  // (nn,X)
        YIndexedZeroPage=0x2000,  // (nn), Y
        Relative=0x4000,          // dd
        ZeroPageRelative=0x8000,  // nn,dd
    }
    
    AddressingModes GetAddressingModes(string instructionName)
    {
        // https://llx.com/Neil/a2/opcodes.html
        AddressingModes addressingModes = AddressingModes.None;
        switch (instructionName)
        {
            case "BBR0":
            case "BBR1":
            case "BBR2":
            case "BBR3":
            case "BBR4":
            case "BBR5":
            case "BBR6":
            case "BBR7":
            case "BBS0":
            case "BBS1":
            case "BBS2":
            case "BBS3":
            case "BBS4":
            case "BBS5":
            case "BBS6":
            case "BBS7":
            {
                addressingModes = AddressingModes.ZeroPageRelative;
            }
            
            case "SMB0":
            case "SMB1":
            case "SMB2":
            case "SMB3":
            case "SMB4":
            case "SMB5":
            case "SMB6":
            case "SMB7":
            case "RMB0":
            case "RMB1":
            case "RMB2":
            case "RMB3":
            case "RMB4":
            case "RMB5":
            case "RMB6":
            case "RMB7":
            {
                addressingModes = AddressingModes.ZeroPage;
            }
            case "BRK":
            case "CLC":
            case "CLD":
            case "CLI":
            case "CLV":
            case "DEX":
            case "DEY":
            case "INX":
            case "INY":
            case "NOP":
            case "PHA":
            case "PHP":
            case "PHX":
            case "PHY":
            case "PLA":
            case "PLP":
            case "PLX":
            case "PLY":
            case "RTI":
            case "RTS":
            case "SEC":
            case "SED":
            case "SEI":
            case "TAX":
            case "TAY":
            case "TSX":
            case "TXA":
            case "TXS":
            case "TYA":   
            {
                addressingModes = AddressingModes.Implied;
            }
            
            case "BCC":
            case "BCS":
            case "BEQ":
            case "BMI":
            case "BNE":
            case "BPL":
            case "BRA":
            case "BVC":
            case "BVS":
            {
                addressingModes = AddressingModes.Relative;
            }
            case "ORA":
            case "AND":
            case "EOR":
            case "ADC":
            case "LDA":
            case "CMP":
            case "SBC":
            {
                addressingModes = AddressingModes.XIndexedZeroPage
                                | AddressingModes.ZeroPage
                                | AddressingModes.Immediate
                                | AddressingModes.Absolute
                                | AddressingModes.YIndexedZeroPage
                                | AddressingModes.AbsoluteY
                                | AddressingModes.AbsoluteX;
            }
            case "STA":
            {
                addressingModes = AddressingModes.Absolute
                                | AddressingModes.AbsoluteY
                                | AddressingModes.AbsoluteX
                                | AddressingModes.ZeroPage
                                | AddressingModes.ZeroPageX
                                | AddressingModes.ZeroPageIndirect
                                | AddressingModes.XIndexedZeroPage
                                | AddressingModes.YIndexedZeroPage;
                                
            }
            case "STX":
            {
                addressingModes = AddressingModes.ZeroPage
                                | AddressingModes.Absolute
                                | AddressingModes.YIndexedZeroPage;
            }
            case "STY":
            {
                addressingModes = AddressingModes.ZeroPage
                                | AddressingModes.Absolute
                                | AddressingModes.XIndexedZeroPage;
            }
            case "STZ":
            {
                addressingModes = AddressingModes.ZeroPage
                                | AddressingModes.Absolute
                                | AddressingModes.AbsoluteX
                                | AddressingModes.XIndexedZeroPage;
            }
            
            case "ASL":
            case "ROL":
            case "LSR":
            case "ROR":
            {
                addressingModes = AddressingModes.ZeroPage
                                | AddressingModes.Accumulator
                                | AddressingModes.Absolute
                                | AddressingModes.ZeroPageX
                                | AddressingModes.AbsoluteX;
            }
            case "DEC":
            case "INC":
            {
                addressingModes = AddressingModes.Accumulator
                                | AddressingModes.Absolute
                                | AddressingModes.AbsoluteX
                                | AddressingModes.ZeroPage
                                | AddressingModes.ZeroPageX;
            }
            case "LDX":
            {
                addressingModes = AddressingModes.Immediate
                                | AddressingModes.ZeroPage
                                | AddressingModes.Absolute
                                | AddressingModes.ZeroPageY
                                | AddressingModes.AbsoluteY;
            }
            case "LDY":
            {
                addressingModes = AddressingModes.Immediate
                                | AddressingModes.ZeroPage
                                | AddressingModes.Absolute
                                | AddressingModes.ZeroPageX
                                | AddressingModes.AbsoluteX;
            }
            case "JMP":
            {
                addressingModes = AddressingModes.Absolute
                                | AddressingModes.AbsoluteIndirect;
            }
            default:
            {
                PrintLn(instructionName);
                Die(0x0A);
            }
        }
        return addressingModes;
    }
    AddressingModes GetAddressingMode(byte instruction)
    {
        AddressingModes addressingMode = AddressingModes.None;
        switch (instruction)
        {
            case 0x0F:
            case 0x1F:
            case 0x2F:
            case 0x3F:
            case 0x4F:
            case 0x5F:
            case 0x6F:
            case 0x7F:
            case 0x8F:
            case 0x9F:
            case 0xAF:
            case 0xBF:
            case 0xCF:
            case 0xDF:
            case 0xEF:
            case 0xFF:
            {
                addressingMode = AddressingModes.ZeroPageRelative;
            }
            
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
            {
                addressingMode = AddressingModes.ZeroPage;
            }
            
            case 0x71:
            case 0x31:
            case 0xD1:
            case 0x51:
            case 0xB1:
            case 0x11:
            case 0xF1:
            case 0x91: { addressingMode = AddressingModes.YIndexedZeroPage; }
            
            case 0x61:
            case 0x21:
            case 0xC1:
            case 0x41:
            case 0xA1:
            case 0x01:
            case 0xE1:
            case 0x81: { addressingMode = AddressingModes.XIndexedZeroPage; }
            
            case 0x72:
            case 0x32:
            case 0xD2:
            case 0x52:
            case 0xB2:
            case 0x12:
            case 0xF2:
            case 0x92: { addressingMode = AddressingModes.ZeroPageIndirect; }
            
            case 0x75:
            case 0x35:
            case 0x16:
            case 0x34:
            case 0xD5:
            case 0xD6:
            case 0x55:
            case 0xF6:
            case 0xB5:
            case 0xB4:
            case 0x56:
            case 0x15:
            case 0x36:
            case 0x76:
            case 0xF5:
            case 0x95:
            case 0x94:
            case 0x74: { addressingMode = AddressingModes.ZeroPageX; }
            
            case 0xB6:
            case 0x96: { addressingMode = AddressingModes.ZeroPageY; }
            
            case 0x79:
            case 0x39:
            case 0xD9:
            case 0x59:
            case 0xB9:
            case 0xBE:
            case 0x19:
            case 0xF9:
            case 0x99: { addressingMode = AddressingModes.AbsoluteY; }
            
            case 0x7D:
            case 0x3D:
            case 0x1E:
            case 0x3C:
            case 0xDD:
            case 0xDE:
            case 0x5D:
            case 0xFE:
            case 0xBD:
            case 0xBC:
            case 0x5E:
            case 0x1D:
            case 0x3E:
            case 0x7E:
            case 0xFD:
            case 0x9D:
            case 0x9E: { addressingMode = AddressingModes.AbsoluteX; }
            
            case 0x6C: { addressingMode = AddressingModes.AbsoluteIndirect; }
            case 0x7C: { addressingMode = AddressingModes.AbsoluteIndirectX; }
            
            case 0x00:
            case 0x18:
            case 0xD8:
            case 0x58:
            case 0xB8:
            case 0xCA:
            case 0x88:
            case 0xE8:
            case 0xC8:
            case 0xEA:
            case 0x48:
            case 0x08:
            case 0xDA:
            case 0x5A:
            case 0x68:
            case 0x28:
            case 0xFA:
            case 0x7A:
            case 0x40:
            case 0x60:
            case 0x38:
            case 0xF8:
            case 0x78:
            case 0xAA:
            case 0xA8:
            case 0xBA:
            case 0x8A:
            case 0x9A:
            case 0x98: { addressingMode = AddressingModes.Implied; }
            
            case 0x0A:
            case 0x3A:
            case 0x1A:
            case 0x4A:
            case 0x2A:
            case 0x6A: { addressingMode = AddressingModes.Accumulator; }
            
            case 0x69:
            case 0x29:
            case 0x89:
            case 0xC9:
            case 0xE0:
            case 0xC0:
            case 0x49:
            case 0xA9:
            case 0xA2:
            case 0xA0:
            case 0x09:
            case 0xE9:{ addressingMode = AddressingModes.Immediate; }
            
            case 0x6D: 
            case 0x2D: 
            case 0x0E: 
            case 0x2C: 
            case 0xCD: 
            case 0xEC: 
            case 0xCC: 
            case 0xCE: 
            case 0x4D: 
            case 0xEE: 
            case 0x4C: 
            case 0x20: 
            case 0xAD: 
            case 0xAE: 
            case 0xAC: 
            case 0x4E: 
            case 0x0D: 
            case 0x2E: 
            case 0x6E: 
            case 0xED: 
            case 0x8D: 
            case 0x8E: 
            case 0x8C: 
            case 0x9C: 
            case 0x1C: 
            case 0x0C: { addressingMode = AddressingModes.Absolute; }
            
            case 0x65: 
            case 0x25: 
            case 0x06: 
            case 0x24: 
            case 0xC5: 
            case 0xE4: 
            case 0xC4: 
            case 0xC6: 
            case 0x45: 
            case 0xE6: 
            case 0xA5: 
            case 0xA6: 
            case 0xA4: 
            case 0x46: 
            case 0x05: 
            case 0x26: 
            case 0x66: 
            case 0xE5: 
            case 0x85: 
            case 0x86: 
            case 0x84: 
            case 0x64: 
            case 0x14: 
            case 0x04: { addressingMode = AddressingModes.ZeroPage; }
            
            case 0x90:
            case 0xB0:
            case 0xF0:
            case 0x30:
            case 0xD0:
            case 0x10:
            case 0x80:
            case 0x50:
            case 0x70: { addressingMode = AddressingModes.Relative; }
        }
        return addressingMode;
    }
    
    string GetName(byte instruction)
    {
        string name = "???";
        switch (instruction)
        {
            case 0x00: { name = "BRK"; }
            case 0x01: { name = "ORA"; }
            case 0x04: { name = "TSB"; }
            case 0x05: { name = "ORA"; }
            case 0x06: { name = "ASL"; }
            case 0x08: { name = "PHP"; }
            case 0x09: { name = "ORA"; }
            case 0x0A: { name = "ASL"; }
            case 0x0C: { name = "TSB"; }
            case 0x0D: { name = "ORA"; }
            case 0x0E: { name = "ASL"; }
            
            case 0x10: { name = "BPL"; }
            case 0x11: { name = "ORA"; }
            case 0x12: { name = "ORA"; }
            case 0x14: { name = "TRB"; }
            case 0x15: { name = "ORA"; }
            case 0x16: { name = "ASL"; }
            case 0x18: { name = "CLC"; }
            case 0x19: { name = "ORA"; }
            case 0x1A: { name = "INC"; }
            case 0x1C: { name = "TRB"; }
            case 0x1D: { name = "ORA"; }
            case 0x1E: { name = "ASL"; }
            
            case 0x20: { name = "JSR"; }
            case 0x21: { name = "AND"; }
            case 0x24: { name = "BIT"; }
            case 0x25: { name = "AND"; }
            case 0x26: { name = "ROL"; }
            case 0x28: { name = "PLP"; }
            case 0x29: { name = "AND"; }
            case 0x2A: { name = "ROL"; }
            case 0x2C: { name = "BIT"; }
            case 0x2D: { name = "AND"; }
            case 0x2E: { name = "ROL"; }
            
            case 0x30: { name = "BMI"; }
            case 0x31: { name = "AND"; }
            case 0x32: { name = "AND"; }
            case 0x34: { name = "BIT"; }
            case 0x35: { name = "AND"; }
            case 0x36: { name = "ROL"; }
            case 0x38: { name = "SEC"; }
            case 0x39: { name = "AND"; }
            case 0x3A: { name = "DEC"; }
            case 0x3C: { name = "BIT"; }
            case 0x3D: { name = "AND"; }
            case 0x3E: { name = "ROL"; }
            
            case 0x40: { name = "RTI"; }
            case 0x41: { name = "EOR"; }
            case 0x45: { name = "EOR"; }
            case 0x46: { name = "LSR"; }
            case 0x48: { name = "PHA"; }
            case 0x49: { name = "EOR"; }
            case 0x4A: { name = "LSR"; }
            case 0x4C: { name = "JMP"; }
            case 0x4D: { name = "EOR"; }
            case 0x4E: { name = "LSR"; }
            
            case 0x50: { name = "BVC"; }
            case 0x51: { name = "EOR"; }
            case 0x52: { name = "EOR"; }
            case 0x55: { name = "EOR"; }
            case 0x56: { name = "LSR"; }
            case 0x58: { name = "CLI"; }
            case 0x59: { name = "EOR"; }
            case 0x5A: { name = "PHY"; }
            case 0x5D: { name = "EOR"; }
            case 0x5E: { name = "LSR"; }
            
            case 0x60: { name = "RTS"; }
            case 0x61: { name = "ADC"; }
            case 0x64: { name = "STZ"; }
            case 0x65: { name = "ADC"; }
            case 0x66: { name = "ROR"; }
            case 0x68: { name = "PLA"; }
            case 0x69: { name = "ADC"; }
            case 0x6A: { name = "ROR"; }
            case 0x6C: { name = "JMP"; }
            case 0x6D: { name = "ADC"; }
            case 0x6E: { name = "ROR"; }
            
            case 0x70: { name = "BVS"; }
            case 0x71: { name = "ADC"; }
            case 0x72: { name = "ADC"; }
            case 0x74: { name = "STZ"; }
            case 0x75: { name = "ADC"; }
            case 0x76: { name = "ROR"; }
            case 0x78: { name = "SEI"; }
            case 0x79: { name = "ADC"; }
            case 0x7A: { name = "PLY"; }
            case 0x7C: { name = "JMP"; }
            case 0x7D: { name = "ADC"; }
            case 0x7E: { name = "ROR"; }
            
            case 0x80: { name = "BRA"; }
            case 0x81: { name = "STA"; }
            case 0x84: { name = "STY"; }
            case 0x85: { name = "STA"; }
            case 0x86: { name = "STX"; }
            case 0x88: { name = "DEY"; }
            case 0x89: { name = "BIT"; }
            case 0x8A: { name = "TXA"; }
            case 0x8C: { name = "STY"; }
            case 0x8D: { name = "STA"; }
            case 0x8E: { name = "STX"; }
            
            case 0x90: { name = "BCC"; }
            case 0x91: { name = "STA"; }
            case 0x92: { name = "STA"; }
            case 0x94: { name = "STY"; }
            case 0x95: { name = "STA"; }
            case 0x96: { name = "STX"; }
            case 0x98: { name = "TYA"; }
            case 0x99: { name = "STA"; }
            case 0x9A: { name = "TXS"; }
            case 0x9C: { name = "STZ"; }
            case 0x9D: { name = "STA"; }
            case 0x9E: { name = "STZ"; }
            
            case 0xA0: { name = "LDY"; }
            case 0xA1: { name = "LDA"; }
            case 0xA2: { name = "LDX"; }
            case 0xA4: { name = "LDY"; }
            case 0xA5: { name = "LDA"; }
            case 0xA6: { name = "LDX"; }
            case 0xA8: { name = "TAY"; }
            case 0xA9: { name = "LDA"; }
            case 0xAA: { name = "TAX"; }
            case 0xAC: { name = "LDY"; }
            case 0xAD: { name = "LDA"; }
            case 0xAE: { name = "LDX"; }
            
            case 0xB0: { name = "BCS"; }
            case 0xB1: { name = "LDA"; }
            case 0xB2: { name = "LDA"; }
            case 0xB4: { name = "LDY"; }
            case 0xB5: { name = "LDA"; }
            case 0xB6: { name = "LDX"; }
            case 0xB8: { name = "CLV"; }
            case 0xB9: { name = "LDA"; }
            case 0xBA: { name = "TSX"; }
            case 0xBC: { name = "LDY"; }
            case 0xBD: { name = "LDA"; }
            case 0xBE: { name = "LDX"; }
            
            case 0xC0: { name = "CPY"; }
            case 0xC1: { name = "CMP"; }
            case 0xC4: { name = "CPY"; }
            case 0xC5: { name = "CMP"; }
            case 0xC6: { name = "DEC"; }
            case 0xC8: { name = "INY"; }
            case 0xC9: { name = "CMP"; }
            case 0xCA: { name = "DEX"; }
            case 0xCC: { name = "CPY"; }
            case 0xCD: { name = "CMP"; }
            case 0xCE: { name = "DEC"; }
            
            case 0xD0: { name = "BNE"; }
            case 0xD1: { name = "CMP"; }
            case 0xD2: { name = "CMP"; }
            case 0xD5: { name = "CMP"; }
            case 0xD6: { name = "DEC"; }
            case 0xD8: { name = "CLD"; }
            case 0xD9: { name = "CMP"; }
            case 0xDA: { name = "PHX"; }
            case 0xDD: { name = "CMP"; }
            case 0xDE: { name = "DEC"; }
            
            case 0xE0: { name = "CPX"; }
            case 0xE1: { name = "SBC"; }
            case 0xE4: { name = "CPX"; }
            case 0xE5: { name = "SBC"; }
            case 0xE6: { name = "INC"; }
            case 0xE8: { name = "INX"; }
            case 0xE9: { name = "SBC"; }
            case 0xEA: { name = "NOP"; }
            case 0xEC: { name = "CPX"; }
            case 0xED: { name = "SBC"; }
            case 0xEE: { name = "INC"; }
            
            case 0xF0: { name = "BEQ"; }
            case 0xF1: { name = "SBC"; }
            case 0xF2: { name = "SBC"; }
            case 0xF5: { name = "SBC"; }
            case 0xF6: { name = "INC"; }
            case 0xF8: { name = "SED"; }
            case 0xF9: { name = "SBC"; }
            case 0xFA: { name = "PLX"; }
            case 0xFD: { name = "SBC"; }
            case 0xFE: { name = "INC"; }
            
            case 0x0F: { name = "BBR0"; }
            case 0x1F: { name = "BBR1"; }
            case 0x2F: { name = "BBR2"; }
            case 0x3F: { name = "BBR3"; }
            case 0x4F: { name = "BBR4"; }
            case 0x5F: { name = "BBR5"; }
            case 0x6F: { name = "BBR6"; }
            case 0x7F: { name = "BBR7"; }
            case 0x8F: { name = "BBS0"; }
            case 0x9F: { name = "BBS1"; }
            case 0xAF: { name = "BBS2"; }
            case 0xBF: { name = "BBS3"; }
            case 0xCF: { name = "BBS4"; }
            case 0xDF: { name = "BBS5"; }
            case 0xEF: { name = "BBS6"; }
            case 0xFF: { name = "BBS7"; }
            
            case 0x07: { name = "RMB0"; }
            case 0x17: { name = "RMB1"; }
            case 0x27: { name = "RMB2"; }
            case 0x37: { name = "RMB3"; }
            case 0x47: { name = "RMB4"; }
            case 0x57: { name = "RMB5"; }
            case 0x67: { name = "RMB6"; }
            case 0x77: { name = "RMB7"; }
            case 0x87: { name = "SMB0"; }
            case 0x97: { name = "SMB1"; }
            case 0xA7: { name = "SMB2"; }
            case 0xB7: { name = "SMB3"; }
            case 0xC7: { name = "SMB4"; }
            case 0xD7: { name = "SMB5"; }
            case 0xE7: { name = "SMB6"; }
            case 0xF7: { name = "SMB7"; }
        }
        return name;
    }
    
    EmitInstruction(string instructionName, byte operand)
    {
        // Immediate AddressingMode
        byte code;
        switch (instructionName)
        {
            case "ADC": { code = 0x69; }
            case "AND": { code = 0x29; }
            case "BIT": { code = 0x89; }
            case "CMP": { code = 0xC9; }
            case "CPX": { code = 0xE0; }
            case "CPY": { code = 0xC0; }
            case "EOR": { code = 0x49; }
            case "LDA": { code = 0xA9; }
            case "LDX": { code = 0xA2; }
            case "LDY": { code = 0xA0; }
            case "ORA": { code = 0x09; }
            case "SBC": { code = 0xE9; }
            
            default:
            {
                Die(0x0A);
            }
        }
        AsmStream.AppendCode(code);
        AsmStream.AppendCode(operand);
    }
    EmitInstructionZeroPageRelative(string instructionName, byte immediateValue, int offset)
    {
        // ZeroPageRelative
        byte code;
        switch (instructionName)
        {
            case "BBR0": { code = 0x0F; }
            case "BBR1": { code = 0x1F; }
            case "BBR2": { code = 0x2F; }
            case "BBR3": { code = 0x3F; }
            case "BBR4": { code = 0x4F; }
            case "BBR5": { code = 0x5F; }
            case "BBR6": { code = 0x6F; }
            case "BBR7": { code = 0x7F; }
            case "BBS0": { code = 0x8F; }
            case "BBS1": { code = 0x9F; }
            case "BBS2": { code = 0xAF; }
            case "BBS3": { code = 0xBF; }
            case "BBS4": { code = 0xCF; }
            case "BBS5": { code = 0xDF; }
            case "BBS6": { code = 0xEF; }
            case "BBS7": { code = 0xFF; }
                       
            default:
            {
                Die(0x0A);
            }
        }
        AsmStream.AppendCode(code);
        AsmStream.AppendCode(immediateValue);
        byte b = offset.GetByte(0);
        AsmStream.AppendCode(b);
    }
    
    EmitInstruction(string instructionName, int offset)
    {
        // Relative AddressingMode
        byte code;
        switch (instructionName)
        {
            case "BCC": { code = 0x90; }
            case "BCS": { code = 0xB0; }
            case "BEQ": { code = 0xF0; }
            case "BMI": { code = 0x30; }
            case "BNE": { code = 0xD0; }
            case "BPL": { code = 0x10; }
            case "BRA": { code = 0x80; }
            case "BVC": { code = 0x50; }
            case "BVS": { code = 0x70; }
            default:
            {
                Die(0x0A);
            }
        }
        AsmStream.AppendCode(code);
        byte b = offset.GetByte(0);
        AsmStream.AppendCode(b);
    }
    EmitInstructionAbsolute(string instructionName, uint operand, AddressingModes addressingMode)
    {
        byte code;
        if (addressingMode == AddressingModes.Absolute)
        {
            // Absolute=0x0008,          // nnnn
            switch (instructionName)
            {
                case "ADC": { code = 0x6D; }
                case "AND": { code = 0x2D; }
                case "ASL": { code = 0x0E; }
                case "BIT": { code = 0x2C; }
                case "CMP": { code = 0xCD; }
                case "CPX": { code = 0xEC; }
                case "CPY": { code = 0xCC; }
                case "DEC": { code = 0xCE; }
                case "EOR": { code = 0x4D; }
                case "INC": { code = 0xEE; }
                case "JMP": { code = 0x4C; }
                case "JSR": { code = 0x20; }
                case "LDA": { code = 0xAD; }
                case "LDX": { code = 0xAE; }
                case "LDY": { code = 0xAC; }
                case "LSR": { code = 0x4E; }
                case "ORA": { code = 0x0D; }
                case "ROL": { code = 0x2E; }
                case "ROR": { code = 0x6E; }
                case "SBC": { code = 0xED; }
                case "STA": { code = 0x8D; }    
                case "STX": { code = 0x8E; }
                case "STY": { code = 0x8C; }
                case "STZ": { code = 0x9C; }
                case "TRB": { code = 0x1C; }
                case "TSB": { code = 0x0C; }
            }
        }
        else if (addressingMode == AddressingModes.AbsoluteX)
        {
            switch (instructionName)
            {
                case "ADC": { code = 0x7D; }
                case "AND": { code = 0x3D; }
                case "ASL": { code = 0x1E; }
                case "BIT": { code = 0x3C; }
                case "CMP": { code = 0xDD; }
                case "DEC": { code = 0xDE; }
                case "EOR": { code = 0x5D; }
                case "INC": { code = 0xFE; }
                case "LDA": { code = 0xBD; }
                case "LDY": { code = 0xBC; }
                case "LSR": { code = 0x5E; }
                case "ORA": { code = 0x1D; }
                case "ROL": { code = 0x3E; }
                case "ROR": { code = 0x7E; }
                case "SBC": { code = 0xFD; }
                case "STA": { code = 0x9D; }    
                case "STZ": { code = 0x9E; }
            }
        }
        else if (addressingMode == AddressingModes.AbsoluteY)
        {
            switch (instructionName)
            {
                case "ADC": { code = 0x79; }
                case "AND": { code = 0x39; }
                case "CMP": { code = 0xD9; }
                case "EOR": { code = 0x59; }
                case "LDA": { code = 0xB9; }
                case "LDX": { code = 0xBE; }
                case "ORA": { code = 0x19; }
                case "SBC": { code = 0xF9; }
                case "STA": { code = 0x99; }    
            }
        }
        else if (addressingMode == AddressingModes.AbsoluteIndirectX)
        {
            switch (instructionName)
            {
                case "JMP": { code = 0x7C; }
                default:
                {
                    Die(0x0A);
                }
            }
        }
        else if (addressingMode == AddressingModes.AbsoluteIndirect)
        {
            switch (instructionName)
            {
                case "JMP": { code = 0x6C; }
                default:
                {
                    Die(0x0A);
                }
            }
        }
        else
        {
            Die(0x0B);
        }
        AsmStream.AppendCode(code);
        AsmStream.AppendCode(byte(operand & 0xFF));
        AsmStream.AppendCode(byte(operand >> 8));
    }
    EmitInstructionZeroPage(string instructionName, byte operand, AddressingModes addressingMode)
    {
        byte code;
        if (addressingMode == AddressingModes.XIndexedZeroPage)
        {
            // XIndexedZeroPage=0x1000,  // (nn,X)
            switch (instructionName)
            {
                case "ADC": { code = 0x61; }
                case "AND": { code = 0x21; }
                case "CMP": { code = 0xC1; }
                case "EOR": { code = 0x41; }
                case "LDA": { code = 0xA1; }
                case "ORA": { code = 0x01; }
                case "SBC": { code = 0xE1; }
                case "STA": { code = 0x81; }
                default: { PrintLn(instructionName); Die(0x0B); }
            }
        }
        else if (addressingMode == AddressingModes.XIndexedZeroPage)
        {
            // YIndexedZeroPage=0x2000,  // (nn), Y
            switch (instructionName)
            {
                case "ADC": { code = 0x71; }
                case "AND": { code = 0x31; }
                case "CMP": { code = 0xD1; }
                case "EOR": { code = 0x51; }
                case "LDA": { code = 0xB1; }
                case "ORA": { code = 0x11; }
                case "SBC": { code = 0xF1; }
                case "STA": { code = 0x91; }
                default: { PrintLn(instructionName); Die(0x0B); }
            }
        }
        else if (addressingMode == AddressingModes.ZeroPageIndirect)
        {
            // ZeroPageIndirect=0x0800,  // (nn)        
            switch (instructionName)
            {
                case "ADC": { code = 0x72; }
                case "AND": { code = 0x32; }
                case "CMP": { code = 0xD2; }
                case "EOR": { code = 0x52; }
                case "LDA": { code = 0xB2; }
                case "ORA": { code = 0x12; }
                case "SBC": { code = 0xF2; }
                case "STA": { code = 0x92; }
                default: { PrintLn(instructionName); Die(0x0B); }
            }
        }
        else if (addressingMode == AddressingModes.ZeroPageY)
        {
            // ZeroPageY,  // nn, Y        
            switch (instructionName)
            {
                case "LDX": { code = 0xB6; }
                case "STX": { code = 0x96; }
                default: { PrintLn(instructionName); Die(0x0B); }
            }
        }
        else if (addressingMode == AddressingModes.ZeroPageX)
        {
            // ZeroPageX,  // nn, X        
            switch (instructionName)
            {
                case "ADC": { code = 0x75; }
                case "AND": { code = 0x35; }
                case "ASL": { code = 0x16; }
                case "BIT": { code = 0x34; }
                case "CMP": { code = 0xD5; }
                case "DEC": { code = 0xD6; }
                case "EOR": { code = 0x55; }
                case "INC": { code = 0xF6; }
                case "LDA": { code = 0xB5; }
                case "LDY": { code = 0xB4; }
                case "LSR": { code = 0x56; }
                case "ORA": { code = 0x15; }
                case "ROL": { code = 0x36; }
                case "ROR": { code = 0x76; }
                case "SBC": { code = 0xF5; }
                case "STA": { code = 0x95; }
                case "STY": { code = 0x94; }
                case "STZ": { code = 0x74; }
                default: { PrintLn(instructionName); Die(0x0B); }
            }
        }
        else if (addressingMode == AddressingModes.ZeroPage)
        {
            // ZeroPage=0x0100,          // nn       
            switch (instructionName)
            {
                case "ADC": { code = 0x65; }
                case "AND": { code = 0x25; }
                case "ASL": { code = 0x06; }
                case "BIT": { code = 0x24; }
                case "CMP": { code = 0xC5; }
                case "CPX": { code = 0xE4; }
                case "CPY": { code = 0xC4; }
                case "DEC": { code = 0xC6; }
                case "EOR": { code = 0x45; }
                case "INC": { code = 0xE6; }
                case "LDA": { code = 0xA5; }
                case "LDX": { code = 0xA6; }
                case "LDY": { code = 0xA4; }
                case "LSR": { code = 0x46; }
                case "ORA": { code = 0x05; }
                case "ROL": { code = 0x26; }
                case "ROR": { code = 0x66; }
                case "SBC": { code = 0xE5; }
                case "STA": { code = 0x85; }
                case "STX": { code = 0x86; }
                case "STY": { code = 0x84; }
                case "STZ": { code = 0x64; }
                case "TRB": { code = 0x14; }
                case "TSB": { code = 0x04; }
                
                case "RMB0": { code = 0x07; }
                case "RMB1": { code = 0x17; }
                case "RMB2": { code = 0x27; }
                case "RMB3": { code = 0x37; }
                case "RMB4": { code = 0x47; }
                case "RMB5": { code = 0x57; }
                case "RMB6": { code = 0x67; }
                case "RMB7": { code = 0x77; }
                case "SMB0": { code = 0x87; }
                case "SMB1": { code = 0x97; }
                case "SMB2": { code = 0xA7; }
                case "SMB3": { code = 0xB7; }
                case "SMB4": { code = 0xC7; }
                case "SMB5": { code = 0xD7; }
                case "SMB6": { code = 0xE7; }
                case "SMB7": { code = 0xF7; }
                
                default: { PrintLn(instructionName); Die(0x0B); }
            }
        }
        
        else
        {
            PrintLn(instructionName); Die(0x0B);
        }
        AsmStream.AppendCode(code);
        AsmStream.AppendCode(operand);
    }
    EmitInstruction(string instructionName)
    {
        // Implied AddressingMode
        // Accumulator AddressingMode
        byte code;
        switch (instructionName)
        {
            case "ASL": { code = 0x0A; }
            case "DEC": { code = 0x3A; }
            case "INC": { code = 0x1A; }
            case "LSR": { code = 0x4A; }
            case "ROL": { code = 0x2A; }
            case "ROR": { code = 0x6A; }
            
            case "BRK": { code = 0x00; }
            case "CLC": { code = 0x18; }
            case "CLD": { code = 0xD8; }
            case "CLI": { code = 0x58; }
            case "CLV": { code = 0xB8; }
            case "DEX": { code = 0xCA; }
            case "DEY": { code = 0x88; }
            case "INX": { code = 0xE8; }
            case "INY": { code = 0xC8; }
            case "NOP": { code = 0xEA; }
            case "PHA": { code = 0x48; }
            case "PHP": { code = 0x08; }
            case "PHX": { code = 0xDA; }
            case "PHY": { code = 0x5A; }
            case "PLA": { code = 0x68; }
            case "PLP": { code = 0x28; }
            case "PLX": { code = 0xFA; }
            case "PLY": { code = 0x7A; }
            case "RTI": { code = 0x40; }
            case "RTS": { code = 0x60; }
            case "SEC": { code = 0x38; }
            case "SED": { code = 0xF8; }
            case "SEI": { code = 0x78; }
            case "TAX": { code = 0xAA; }
            case "TAY": { code = 0xA8; }
            case "TSX": { code = 0xBA; }
            case "TXA": { code = 0x8A; }
            case "TXS": { code = 0x9A; }
            case "TYA": { code = 0x98; }
            default:
            {
                Die(0x0A);
            }
        }
        AsmStream.AppendCode(code);
    }
    
    // Direct support for a small subset of instructions that 
    // are used directly by the assembler:
    byte GetRETInstruction()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            return 0x60; // RTS
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            return 0xC9; // RET
        }
        Die(0x0B);
        return 0;
    }
    uint GetRETIInstruction()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            return 0x40; // RTI
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            return 0xED + 0x4D << 8; // RETI is 0xED 0x4D
        }
        Die(0x0B);
        return 0;
    }
    uint GetRETNInstruction()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            return 0x40; // RTI
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            return 0xED + 0x44 << 8; // RETN is 0xED 0x45
        }
        Die(0x0B);
        return 0;
    }
    
    uint GetHALTInstruction()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            return 0x80 + 0xFE << 8; // BRA -2 (to self)
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            return 0x76; // HALT
        }
        Die(0x0B);
        return 0;
    }
    
    byte GetJMPInstruction()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            return 0x4C; // JMP
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            PrintLn("HERE!");
            return 0xC3; // JP
        }
        Die(0x0B);
        return 0;
    }
    byte GetNOPInstruction()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            return 0xEA; // NOP
        }
        if (Architecture == CPUArchitecture.Z80A)
        {
            return 0x00; // NOP
        }
        Die(0x0B);
        return 0;
    }
    byte GetBInstruction(string condition)
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            switch (condition)
            {
                case "Z":  { return 0xF0; } // BEQ
                case "NZ": { return 0xD0; } // BNE
                case "C":  { return 0xB0; } // BCS
                case "NC": { return 0x90; } // BCC
                case "":   { return 0x80; } // BRA
                default:   { Die(0x0A);   }
            }
        }
        Die(0x0B);
        return 0;
    }
    
    byte GetJPInstruction(string condition)
    {
        if (Architecture == CPUArchitecture.Z80A)
        {
            switch (condition)
            {
                case "Z":  { return 0xCA; } // JP Z
                case "NZ": { return 0xC2; } // JP NZ
                default:   { Die(0x0A);   }
            }
        }
        Die(0x0B);
        return 0;
    }
    byte GetJSRInstruction()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            return 0x20; // JSR
        }
        Die(0x0B);
        return 0;
    }
    byte GetCALLInstruction()
    {
        if (Architecture == CPUArchitecture.Z80A)
        {
            return 0xCD; // CALL
        }
        Die(0x0B);
        return 0;
    }
    
}
