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
        AbsoluteY=0x0010,         // nnnn,X
        AbsoluteX=0x0020,         // nnnn,Y
        AbsoluteIndirect=0x0040,  // (nnnn)
        AbsoluteIndirectX=0x0080, // (nnnn,X)
        ZeroPage=0x0100,          // nn
        ZeroPageX=0x0200,         // nn,X
        ZeroPageY=0x0400,         // nn,Y
        ZeroPageIndirect=0x0800,  // (nn)
        XIndexedZeroPage=0x1000,  // (nn,X)
        YIndexedZeroPage=0x2000,  // (nn), Y
        Relative=0x4000,          // dd
    }
    
    AddressingModes GetAddressingModes(string instructionName)
    {
        // https://llx.com/Neil/a2/opcodes.html
        AddressingModes addressingModes = AddressingModes.None;
        switch (instructionName)
        {
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
                addressingModes = AddressingModes.XIndexedZeroPage
                                | AddressingModes.ZeroPage
                                | AddressingModes.Absolute
                                | AddressingModes.YIndexedZeroPage
                                | AddressingModes.AbsoluteY
                                | AddressingModes.AbsoluteX;
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
                Die(0x0A);
            }
        }
        return addressingModes;
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
            }
        }
        else if (addressingMode == AddressingModes.ZeroPageY)
        {
            // ZeroPageY,  // nn, Y        
            switch (instructionName)
            {
                case "LDX": { code = 0xB6; }
                case "STX": { code = 0x96; }
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
            }
        }
        
        else
        {
            Die(0x0B);
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
    
    byte GetHALTInstruction()
    {
        if (Architecture & CPUArchitecture.M6502 != CPUArchitecture.None)
        {
            return 0xEA; // NOP : TODO : JMP to self?
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
            return 0xC3; // JP
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
