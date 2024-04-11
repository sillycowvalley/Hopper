unit Asm6502
{
    <string,string> debugInfo;
    <string,bool> debugInfoLineUsed;
    <byte> currentStream;
    <byte> constantStream;
    
    CPUArchitecture cpuArchitecture;
    CPUArchitecture Architecture { get { return cpuArchitecture; } set { cpuArchitecture = value; } }
    
    uint InvalidAddress { get { return 0xFFFF; } }
    
    uint SwitchJumpAddress { get { return 0x0020; } } // W0 and W1 on Zero page used by switch statements
    
    flags AddressingModes
    {
        None=0,
        Implied=0x0001,
        Accumulator=0x0002,       // A
        Immediate=0x0004,         // #nn       n
        Absolute=0x0008,          // nnnn      nn
        AbsoluteX=0x0010,         // nnnn,X    nnX
        AbsoluteY=0x0020,         // nnnn,Y    nnY
        AbsoluteIndirect=0x0040,  // [nnnn]    inn
        AbsoluteIndirectX=0x0080, // [nnnn,X]  innX
        ZeroPage=0x0100,          // nn        z
        ZeroPageX=0x0200,         // nn,X      zX
        ZeroPageY=0x0400,         // nn,Y      zY
        ZeroPageIndirect=0x0800,  // [nn]      iz
        XIndexedZeroPage=0x1000,  // [nn,X]    izX
        YIndexedZeroPage=0x2000,  // [nn], Y   izY
        Relative=0x4000,          // dd        e
        ZeroPageRelative=0x8000,  // nn,dd     z_e
    }
    
    string ToString(OpCode instruction)
    {
        string name = Asm6502.GetName(instruction);
        AddressingModes addressingMode = Asm6502.GetAddressingMode(instruction);
        switch (addressingMode)
        {
            case AddressingModes.Implied: {}
            case AddressingModes.Accumulator: { name += " A"; }
            case AddressingModes.Immediate: { name += " #nn"; }
            case AddressingModes.Absolute: { name += " nnnn"; }
            case AddressingModes.AbsoluteX: { name += " nnnn,X"; }
            case AddressingModes.AbsoluteY: { name += " nnnn,Y"; }
            case AddressingModes.AbsoluteIndirect: { name += " [nnnn]"; }
            case AddressingModes.AbsoluteIndirectX: { name += " [nnnn,X]"; }
            case AddressingModes.ZeroPage: { name += " zz"; }
            case AddressingModes.ZeroPageX: { name += " zz,X"; }
            case AddressingModes.ZeroPageY: { name += " zz,Y"; }
            case AddressingModes.ZeroPageIndirect: { name += " [zz]"; }
            case AddressingModes.XIndexedZeroPage: { name += " [zz,X]"; }
            case AddressingModes.YIndexedZeroPage: { name += " [zz],Y"; }
            case AddressingModes.Relative: { name += " dd"; }
            case AddressingModes.ZeroPageRelative: { name += " zz,dd"; }
        }
        return name;
    }
                    
    
    enum OpCode
    {
        BRK      = 0x00,   
        BPL_e    = 0x10,  
        JSR_nn   = 0x20,  
        BMI_e    = 0x30,  
        RTI      = 0x40,
        BVC_e    = 0x50,
        RTS      = 0x60,  
        BVS_e    = 0x70,
        BRA_e    = 0x80,
        BCC_e    = 0x90,
        LDY_n    = 0xA0,
        BCS_e    = 0xB0,
        CPY_n    = 0xC0,        
        BNE_e    = 0xD0,
        CPX_n    = 0xE0,
        BEQ_e    = 0xF0,
        
        ORA_izX  = 0x01,
        ORA_izY  = 0x11,
        AND_izX  = 0x21,
        AND_izY  = 0x31,
        EOR_izX  = 0x41,
        EOR_izY  = 0x51,
        ADC_izX  = 0x61,
        ADC_izY  = 0x71,
        STA_izX  = 0x81,
        STA_izY  = 0x91,
        LDA_izX  = 0xA1,
        LDA_izY  = 0xB1,
        CMP_izX  = 0xC1,
        CMP_izY  = 0xD1,
        SBC_izX  = 0xE1,
        SBC_izY  = 0xF1,
        
        ORA_iz   = 0x12,
        
        AND_iz   = 0x32,
        
        EOR_iz   = 0x52,
        
        ADC_iz   = 0x72,
        
        STA_iz   = 0x92,
        LDX_n    = 0xA2,
        LDA_iz   = 0xB2,
        
        CMP_iz   = 0xD2,
        
        SBC_iz   = 0xF2,
        
        TSB_z    = 0x04,
        TRB_z    = 0x14,
        BIT_z    = 0x24,
        BIT_zX   = 0x34,
        
        STZ_z    = 0x64,
        STZ_zX   = 0x74,
        STY_z    = 0x84,
        STY_zX   = 0x94,
        LDY_z    = 0xA4,
        LDY_zX   = 0xB4,
        CPY_z    = 0xC4,
        
        CPX_z    = 0xE4,
        
        ORA_z    = 0x05,
        ORA_zX   = 0x15,
        AND_z    = 0x25,
        AND_zX   = 0x35,
        EOR_z    = 0x45,
        EOR_zX   = 0x55,
        ADC_z    = 0x65,
        ADC_zX   = 0x75,
        STA_z    = 0x85,
        STA_zX   = 0x95,
        LDA_z    = 0xA5,
        LDA_zX   = 0xB5,
        CMP_z    = 0xC5,
        CMP_zX   = 0xD5,
        SBC_z    = 0xE5,
        SBC_zX   = 0xF5,
        
        ASL_z    = 0x06,
        ASL_zX   = 0x16,
        ROL_z    = 0x26,
        ROL_zX   = 0x36,
        LSR_z    = 0x46,
        LSR_zX   = 0x56,
        ROR_z    = 0x66,
        ROR_zX   = 0x76,
        STX_z    = 0x86,
        STX_zY   = 0x96,
        LDX_z    = 0xA6,
        LDX_zY   = 0xB6,
        DEC_z    = 0xC6,
        DEC_zX   = 0xD6,
        INC_z    = 0xE6,
        INC_zX   = 0xF6,
        
        
        RMB0_z   = 0x07,
        RMB1_z   = 0x17,
        RMB2_z   = 0x27,
        RMB3_z   = 0x37,
        RMB4_z   = 0x47,
        RMB5_z   = 0x57,
        RMB6_z   = 0x67,
        RMB7_z   = 0x77,
        SMB0_z   = 0x87,
        SMB1_z   = 0x97,
        SMB2_z   = 0xA7,
        SMB3_z   = 0xB7,
        SMB4_z   = 0xC7,
        SMB5_z   = 0xD7,
        SMB6_z   = 0xE7,
        SMB7_z   = 0xF7,
        
        PHP      = 0x08,
        CLC      = 0x18,
        PLP      = 0x28,
        SEC      = 0x38,
        PHA      = 0x48,
        CLI      = 0x58,
        PLA      = 0x68,
        SEI      = 0x78,
        DEY      = 0x88,
        TYA      = 0x98,
        TAY      = 0xA8,
        CLV      = 0xB8,
        INY      = 0xC8,
        CLD      = 0xD8,
        INX      = 0xE8,
        SED      = 0xF8,
        
        ORA_n    = 0x09,
        ORA_nnY  = 0x19,
        AND_n    = 0x29,
        AND_nnY  = 0x39,
        EOR_n    = 0x49,
        EOR_nnY  = 0x59,
        ADC_n    = 0x69,
        ADC_nnY  = 0x79,
        BIT_n    = 0x89,
        STA_nnY  = 0x99,
        LDA_n    = 0xA9,
        LDA_nnY  = 0xB9,
        CMP_n    = 0xC9,
        CMP_nnY  = 0xD9,
        SBC_n    = 0xE9,
        SBC_nnY  = 0xF9,
        
        ASL      = 0x0A,
        INC      = 0x1A,
        ROL      = 0x2A,
        DEC      = 0x3A,
        LSR      = 0x4A,
        PHY      = 0x5A,
        ROR      = 0x6A,
        PLY      = 0x7A,
        TXA      = 0x8A,
        TXS      = 0x9A,
        TAX      = 0xAA,
        TSX      = 0xBA,
        DEX      = 0xCA,
        PHX      = 0xDA,
        NOP      = 0xEA,
        PLX      = 0xFA,
        
        WAI      = 0xCB,
        STP      = 0xDB,
        
        TSB_nn   = 0x0C,
        TRB_nn   = 0x1C,
        BIT_nn   = 0x2C,
        BIT_nnX  = 0x3C,
        JMP_nn   = 0x4C,
        
        JMP_inn  = 0x6C,
        JMP_innX = 0x7C,
        STY_nn   = 0x8C,
        STZ_nn   = 0x9C,
        LDY_nn   = 0xAC,
        LDY_nnX  = 0xBC,
        CPY_nn   = 0xCC,
        
        CPX_nn   = 0xEC,
        
        sJMP_inn = 0xFB, // internal instruction : used for switch statements
        iJMP_nn  = 0xFC, // internal instruction : jump to methodIndex place holder
        
        ORA_nn   = 0x0D,
        ORA_nnX  = 0x1D,
        AND_nn   = 0x2D,
        AND_nnX  = 0x3D,
        EOR_nn   = 0x4D,
        EOR_nnX  = 0x5D,
        ADC_nn   = 0x6D,
        ADC_nnX  = 0x7D,
        STA_nn   = 0x8D,
        STA_nnX  = 0x9D,
        LDA_nn   = 0xAD,
        LDA_nnX  = 0xBD,
        CMP_nn   = 0xCD,
        CMP_nnX  = 0xDD,
        SBC_nn   = 0xED,
        SBC_nnX  = 0xFD,
        
        ASL_nn   = 0x0E,
        ASL_nnX  = 0x1E,
        ROL_nn   = 0x2E,
        ROL_nnX  = 0x3E,
        LSR_nn   = 0x4E,
        LSR_nnX  = 0x5E,
        ROR_nn   = 0x6E,
        ROR_nnX  = 0x7E,
        STX_nn   = 0x8E,
        STZ_nnX  = 0x9E,
        LDX_nn   = 0xAE,
        LDX_nnY  = 0xBE,
        DEC_nn   = 0xCE,
        DEC_nnX  = 0xDE,
        INC_nn   = 0xEE,
        INC_nnX  = 0xFE,
        
        BBR0_z_e = 0x0F,
        BBR1_z_e = 0x1F,
        BBR2_z_e = 0x2F,
        BBR3_z_e = 0x3F,
        BBR4_z_e = 0x4F,
        BBR5_z_e = 0x5F,
        BBR6_z_e = 0x6F,
        BBR7_z_e = 0x7F,
        BBS0_z_e = 0x8F,
        BBS1_z_e = 0x9F,
        BBS2_z_e = 0xAF,
        BBS3_z_e = 0xBF,
        BBS4_z_e = 0xCF,
        BBS5_z_e = 0xDF,
        BBS6_z_e = 0xEF,
        BBS7_z_e = 0xFF,
        
        
    }
    
    IE()
    {
        Parser.Error("internal error"); Die(0x0B);
    }
    NI()
    {
        Parser.Error("not implemented"); Die(0x0A);
    }
    
    ValidateInstruction(OpCode instruction)
    {
        if (!IsValidInstruction(instruction))
        {
            if (Architecture == CPUArchitecture.M6502)
            {
                Parser.Error("Invalid instruction for original 6502 (CPU_6502). ");
            }
            else
            {
                Die(0x0A);
            }
        }
    }
    
    
    bool IsValidInstruction(OpCode instruction)
    {
        bool valid = true;
        
        if (Architecture == CPUArchitecture.M6502)
        {
            switch (instruction)
            {
                case OpCode.ORA_iz:
                case OpCode.AND_iz:
                case OpCode.EOR_iz:
                case OpCode.ADC_iz:
                case OpCode.STA_iz:
                case OpCode.LDA_iz:
                case OpCode.CMP_iz:
                case OpCode.SBC_iz:
                
                case OpCode.TSB_z:
                case OpCode.TRB_z:
                case OpCode.BIT_zX:
                
                case OpCode.STZ_z:
                case OpCode.STZ_zX:
                
                case OpCode.BIT_n:
                
                case OpCode.INC:
                case OpCode.DEC:
                case OpCode.PHY:
                case OpCode.PLY:
                case OpCode.PHX:
                case OpCode.PLX:
                
                case OpCode.STZ_nnX:
                
                case OpCode.BBR0_z_e:
                case OpCode.BBR1_z_e:
                case OpCode.BBR2_z_e:
                case OpCode.BBR3_z_e:
                case OpCode.BBR4_z_e:
                case OpCode.BBR5_z_e:
                case OpCode.BBR6_z_e:
                case OpCode.BBR7_z_e:
                case OpCode.BBS0_z_e:
                case OpCode.BBS1_z_e:
                case OpCode.BBS2_z_e:
                case OpCode.BBS3_z_e:
                case OpCode.BBS4_z_e:
                case OpCode.BBS5_z_e:
                case OpCode.BBS6_z_e:
                case OpCode.BBS7_z_e:
                
                case OpCode.RMB0_z:
                case OpCode.RMB1_z:
                case OpCode.RMB2_z:
                case OpCode.RMB3_z:
                case OpCode.RMB4_z:
                case OpCode.RMB5_z:
                case OpCode.RMB6_z:
                case OpCode.RMB7_z:
                case OpCode.SMB0_z:
                case OpCode.SMB1_z:
                case OpCode.SMB2_z:
                case OpCode.SMB3_z:
                case OpCode.SMB4_z:
                case OpCode.SMB5_z:
                case OpCode.SMB6_z:
                case OpCode.SMB7_z:
                
                case OpCode.BRA_e:
                case OpCode.JMP_innX:
                {
                    valid = false;
                }
                
            }
        }
        return valid;
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
            
            case "BIT":
            {
                addressingModes = AddressingModes.ZeroPage
                                | AddressingModes.Absolute;
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
            case "STP":
            case "TAX":
            case "TAY":
            case "TSX":
            case "TXA":
            case "TXS":
            case "TYA":   
            case "WAI":   
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
            case "CMP":
            case "SBC":
            case "ADC":
            case "EOR":
            case "AND":
            case "ORA":
            case "LDA":
            {
                addressingModes = AddressingModes.Immediate        // #nn
                                | AddressingModes.Absolute         // nnnn
                                | AddressingModes.AbsoluteY        // nnnn,X
                                | AddressingModes.AbsoluteX        // nnnn,Y
                                | AddressingModes.ZeroPage         // nn
                                | AddressingModes.ZeroPageX        // nn,X
                                | AddressingModes.ZeroPageIndirect // [nn]
                                | AddressingModes.XIndexedZeroPage // [nn,X]
                                | AddressingModes.YIndexedZeroPage // [nn],Y
                                ;
            }
            case "CPX":
            case "CPY":
            {
                addressingModes = AddressingModes.ZeroPage
                                | AddressingModes.Immediate
                                | AddressingModes.Absolute;
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
                                | AddressingModes.ZeroPageX;
            }
            case "STY":
            {
                addressingModes = AddressingModes.ZeroPage
                                | AddressingModes.Absolute
                                | AddressingModes.ZeroPageY;
            }
            case "STZ":
            {
                addressingModes = AddressingModes.ZeroPage
                                | AddressingModes.Absolute
                                | AddressingModes.AbsoluteX
                                | AddressingModes.ZeroPageX;
            }
            
            case "ASL":
            case "ROL":
            case "LSR":
            case "ROR":
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
            case "JSR":
            case "iJMP":
            {
                addressingModes = AddressingModes.Absolute;
            }
            default:
            {
                NI();
            }
        }
        return addressingModes;
    }
    
    uint GetInstructionLength(OpCode instruction)
    {
        uint length;
        switch (GetAddressingMode(instruction))
        {
            case AddressingModes.Implied:           { length = 1; }
            case AddressingModes.Accumulator:       { length = 1; }
            case AddressingModes.Immediate:         { length = 2; }
            case AddressingModes.Absolute:          { length = 3; }
            case AddressingModes.AbsoluteX:         { length = 3; }
            case AddressingModes.AbsoluteY:         { length = 3; }
            case AddressingModes.AbsoluteIndirect:  { length = 3; }
            case AddressingModes.AbsoluteIndirectX: { length = 3; }
            case AddressingModes.ZeroPage:          { length = 2; }
            case AddressingModes.ZeroPageX:         { length = 2; }
            case AddressingModes.ZeroPageY:         { length = 2; }
            case AddressingModes.ZeroPageIndirect:  { length = 2; }
            case AddressingModes.XIndexedZeroPage:  { length = 2; }
            case AddressingModes.YIndexedZeroPage:  { length = 2; }
            case AddressingModes.Relative:          { length = 2; }
            case AddressingModes.ZeroPageRelative:  { length = 3; }
            default:
            { 
                string name = GetName(instruction); Print("0x" + (uint(instruction)).ToHexString(2) + ":" + name); Die(0x0B); 
            }
        }
        return length;
    }
    
    AddressingModes GetAddressingMode(OpCode instruction)
    {
        AddressingModes addressingMode = AddressingModes.None;
        switch (instruction)
        {
            case OpCode.BBR0_z_e:
            case OpCode.BBR1_z_e:
            case OpCode.BBR2_z_e:
            case OpCode.BBR3_z_e:
            case OpCode.BBR4_z_e:
            case OpCode.BBR5_z_e:
            case OpCode.BBR6_z_e:
            case OpCode.BBR7_z_e:
            case OpCode.BBS0_z_e:
            case OpCode.BBS1_z_e:
            case OpCode.BBS2_z_e:
            case OpCode.BBS3_z_e:
            case OpCode.BBS4_z_e:
            case OpCode.BBS5_z_e:
            case OpCode.BBS6_z_e:
            case OpCode.BBS7_z_e:
            {
                addressingMode = AddressingModes.ZeroPageRelative;
            }
            
            case OpCode.RMB0_z:
            case OpCode.RMB1_z:
            case OpCode.RMB2_z:
            case OpCode.RMB3_z:
            case OpCode.RMB4_z:
            case OpCode.RMB5_z:
            case OpCode.RMB6_z:
            case OpCode.RMB7_z:
            case OpCode.SMB0_z:
            case OpCode.SMB1_z:
            case OpCode.SMB2_z:
            case OpCode.SMB3_z:
            case OpCode.SMB4_z:
            case OpCode.SMB5_z:
            case OpCode.SMB6_z:
            case OpCode.SMB7_z:
            {
                addressingMode = AddressingModes.ZeroPage;
            }
        
            case OpCode.ORA_izY:
            case OpCode.AND_izY:
            case OpCode.ADC_izY:
            case OpCode.EOR_izY:
            case OpCode.STA_izY:
            case OpCode.LDA_izY:
            case OpCode.CMP_izY:
            case OpCode.SBC_izY: { addressingMode = AddressingModes.YIndexedZeroPage; }
            
            case OpCode.ORA_izX:
            case OpCode.AND_izX:
            case OpCode.EOR_izX:
            case OpCode.ADC_izX:
            case OpCode.STA_izX:
            case OpCode.LDA_izX:
            case OpCode.SBC_izX:
            case OpCode.CMP_izX: { addressingMode = AddressingModes.XIndexedZeroPage; }
            
            case OpCode.ORA_iz:
            case OpCode.AND_iz:
            case OpCode.EOR_iz:
            case OpCode.ADC_iz:
            case OpCode.STA_iz:
            case OpCode.LDA_iz:
            case OpCode.CMP_iz:
            case OpCode.SBC_iz: { addressingMode = AddressingModes.ZeroPageIndirect; }
            
            case OpCode.BIT_zX:
            case OpCode.STZ_zX:
            case OpCode.STY_zX:
            case OpCode.LDY_zX:
            case OpCode.ORA_zX:
            case OpCode.AND_zX:
            case OpCode.EOR_zX:
            case OpCode.ADC_zX:
            case OpCode.STA_zX:
            case OpCode.LDA_zX:
            case OpCode.CMP_zX:
            case OpCode.SBC_zX:
            case OpCode.ASL_zX:
            case OpCode.ROL_zX:
            case OpCode.LSR_zX:
            case OpCode.ROR_zX:
            case OpCode.DEC_zX:
            case OpCode.INC_zX: { addressingMode = AddressingModes.ZeroPageX; }
            
            case OpCode.STX_zY:
            case OpCode.LDX_zY: { addressingMode = AddressingModes.ZeroPageY; }
            
            case OpCode.LDX_nnY:
            case OpCode.AND_nnY:
            case OpCode.EOR_nnY:
            case OpCode.ADC_nnY:
            case OpCode.STA_nnY:
            case OpCode.ORA_nnY:
            case OpCode.LDA_nnY:
            case OpCode.CMP_nnY:
            case OpCode.SBC_nnY: { addressingMode = AddressingModes.AbsoluteY; }
            
            case OpCode.EOR_nnX:
            case OpCode.ADC_nnX:
            case OpCode.STA_nnX:
            case OpCode.LDA_nnX:
            case OpCode.ASL_nnX:
            case OpCode.SBC_nnX:
            case OpCode.CMP_nnX:
            case OpCode.ROL_nnX:
            case OpCode.LSR_nnX:
            case OpCode.DEC_nnX:
            case OpCode.STZ_nnX:
            case OpCode.ROR_nnX:
            case OpCode.INC_nnX:
            case OpCode.BIT_nnX:
            case OpCode.AND_nnX:
            case OpCode.ORA_nnX:
            case OpCode.LDY_nnX:  { addressingMode = AddressingModes.AbsoluteX; }
            
            case OpCode.sJMP_inn:
            case OpCode.JMP_inn:  { addressingMode = AddressingModes.AbsoluteIndirect; }
            
            case OpCode.JMP_innX: { addressingMode = AddressingModes.AbsoluteIndirectX; }
            
            case OpCode.PHY:
            case OpCode.PLY:
            case OpCode.SED:
            case OpCode.TXA:
            case OpCode.INX:
            case OpCode.TXS:
            case OpCode.TAX:
            case OpCode.TSX:
            case OpCode.DEX:
            case OpCode.PHX:
            case OpCode.NOP:
            case OpCode.PLX:
            case OpCode.WAI:
            case OpCode.STP:
            case OpCode.BRK:
            case OpCode.RTI:
            case OpCode.RTS:
            case OpCode.CLD:
            case OpCode.INY:
            case OpCode.CLV:
            case OpCode.TAY:
            case OpCode.TYA:
            case OpCode.DEY:
            case OpCode.SEI:
            case OpCode.PLA:
            case OpCode.CLI:
            case OpCode.PLP:
            case OpCode.CLC:
            case OpCode.SEC:
            case OpCode.PHA:
            case OpCode.PHP: { addressingMode = AddressingModes.Implied; }
            
            case OpCode.ASL:
            case OpCode.INC:
            case OpCode.ROL:
            case OpCode.DEC:
            case OpCode.ROR:
            case OpCode.LSR: { addressingMode = AddressingModes.Accumulator; }
            
            case OpCode.ORA_n:
            case OpCode.AND_n:
            case OpCode.EOR_n:
            case OpCode.ADC_n:
            case OpCode.BIT_n:
            case OpCode.LDA_n:
            case OpCode.CMP_n:
            case OpCode.SBC_n:
            case OpCode.LDX_n:
            case OpCode.LDY_n:
            case OpCode.CPY_n:
            case OpCode.CPX_n:{ addressingMode = AddressingModes.Immediate; }
            
            case OpCode.ORA_nn: 
            case OpCode.AND_nn: 
            case OpCode.EOR_nn: 
            case OpCode.INC_nn: 
            case OpCode.DEC_nn: 
            case OpCode.LDX_nn: 
            case OpCode.STX_nn: 
            case OpCode.ROR_nn: 
            case OpCode.LSR_nn: 
            case OpCode.ROL_nn: 
            case OpCode.ASL_nn: 
            case OpCode.SBC_nn: 
            case OpCode.CMP_nn: 
            case OpCode.LDA_nn: 
            case OpCode.STA_nn: 
            case OpCode.ADC_nn: 
            case OpCode.CPX_nn: 
            case OpCode.CPY_nn: 
            case OpCode.LDY_nn: 
            case OpCode.STZ_nn: 
            case OpCode.STY_nn: 
            case OpCode.JMP_nn: 
            case OpCode.iJMP_nn: 
            case OpCode.BIT_nn: 
            case OpCode.TRB_nn: 
            case OpCode.TSB_nn: 
            case OpCode.JSR_nn: { addressingMode = AddressingModes.Absolute; }
            
            case OpCode.TSB_z: 
            case OpCode.TRB_z: 
            case OpCode.BIT_z: 
            case OpCode.STZ_z: 
            case OpCode.STY_z: 
            case OpCode.LDY_z: 
            case OpCode.CPY_z: 
            case OpCode.CPX_z: 
            case OpCode.ORA_z: 
            case OpCode.AND_z: 
            case OpCode.EOR_z: 
            case OpCode.ADC_z: 
            case OpCode.STA_z: 
            case OpCode.LDA_z: 
            case OpCode.CMP_z: 
            case OpCode.SBC_z: 
            case OpCode.ASL_z: 
            case OpCode.ROL_z: 
            case OpCode.LSR_z: 
            case OpCode.ROR_z: 
            case OpCode.STX_z: 
            case OpCode.LDX_z: 
            case OpCode.DEC_z: 
            case OpCode.INC_z: { addressingMode = AddressingModes.ZeroPage; }
            
            case OpCode.BPL_e:
            case OpCode.BMI_e:
            case OpCode.BVC_e:
            case OpCode.BVS_e:
            case OpCode.BRA_e:
            case OpCode.BCC_e:
            case OpCode.BCS_e:
            case OpCode.BNE_e:
            case OpCode.BEQ_e: { addressingMode = AddressingModes.Relative; }
        }
        return addressingMode;
    }
    
    
    string GetName(OpCode instruction)
    {
        string name = "???";
        switch (instruction)
        {
            case OpCode.BRK:    { name = "BRK"; }
            case OpCode.BPL_e:  { name = "BPL"; }
            case OpCode.JSR_nn: { name = "JSR"; }
            case OpCode.BMI_e:  { name = "BMI"; }
            case OpCode.RTI:    { name = "RTI"; }
            case OpCode.BVC_e:  { name = "BVC"; }
            case OpCode.RTS:    { name = "RTS"; }
            case OpCode.BVS_e:  { name = "BVS"; }
            case OpCode.BRA_e:  { name = "BRA"; }
            case OpCode.BCC_e:  { name = "BCC"; }
            case OpCode.LDY_n:  { name = "LDY"; }
            case OpCode.BCS_e:  { name = "BCS"; }
            case OpCode.CPY_n:  { name = "CPY"; }
            case OpCode.BNE_e:  { name = "BNE"; }
            case OpCode.CPX_n:  { name = "CPX"; }
            case OpCode.BEQ_e:  { name = "BEQ"; }
            case OpCode.ORA_izX:{ name = "ORA"; }
            case OpCode.ORA_izY:{ name = "ORA"; }
            case OpCode.AND_izX:{ name = "AND"; }
            case OpCode.AND_izY:{ name = "AND"; }
            case OpCode.EOR_izX:{ name = "EOR"; }
            case OpCode.EOR_izY:{ name = "EOR"; }
            case OpCode.ADC_izX:{ name = "ADC"; }
            case OpCode.ADC_izY:{ name = "ADC"; }
            case OpCode.STA_izX:{ name = "STA"; }
            case OpCode.STA_izY:{ name = "STA"; }
            case OpCode.LDA_izX:{ name = "LDA"; }
            case OpCode.LDA_izY:{ name = "LDA"; }
            case OpCode.CMP_izX:{ name = "CMP"; }
            case OpCode.CMP_izY:{ name = "CMP"; }
            case OpCode.SBC_izX:{ name = "SBC"; }
            case OpCode.SBC_izY:{ name = "SBC"; }
            case OpCode.ORA_iz: { name = "ORA"; }
            case OpCode.AND_iz: { name = "AND"; }
            case OpCode.EOR_iz: { name = "EOR"; }
            case OpCode.ADC_iz: { name = "ADC"; }
            case OpCode.STA_iz: { name = "STA"; }
            case OpCode.LDX_n:  { name = "LDX"; }
            case OpCode.LDA_iz: { name = "LDA"; }
            case OpCode.CMP_iz: { name = "CMP"; }
            case OpCode.SBC_iz: { name = "SBC"; }
            case OpCode.TSB_z:  { name = "TSB"; }
            case OpCode.TRB_z:  { name = "TRB"; }
            case OpCode.BIT_z:  { name = "BIT"; }
            case OpCode.BIT_zX: { name = "BIT"; }
            case OpCode.STZ_z:  { name = "STZ"; }
            case OpCode.STZ_zX: { name = "STZ"; }
            case OpCode.STY_z:  { name = "STY"; }
            case OpCode.STY_zX: { name = "STY"; }
            case OpCode.LDY_z:  { name = "LDY"; }
            case OpCode.LDY_zX: { name = "LDY"; }
            case OpCode.CPY_z:  { name = "CPY"; }
            case OpCode.CPX_z:  { name = "CPX"; }
            case OpCode.ORA_z:  { name = "ORA"; }
            case OpCode.ORA_zX: { name = "ORA"; }
            case OpCode.AND_z:  { name = "AND"; }
            case OpCode.AND_zX: { name = "AND"; }
            case OpCode.EOR_z:  { name = "EOR"; }
            case OpCode.EOR_zX: { name = "EOR"; }
            case OpCode.ADC_z:  { name = "ADC"; }
            case OpCode.ADC_zX: { name = "ADC"; }
            case OpCode.STA_z:  { name = "STA"; }
            case OpCode.STA_zX: { name = "STA"; }
            case OpCode.LDA_z:  { name = "LDA"; }
            case OpCode.LDA_zX: { name = "LDA"; }
            case OpCode.CMP_z:  { name = "CMP"; }
            case OpCode.CMP_zX: { name = "CMP"; }
            case OpCode.SBC_z:  { name = "SBC"; }
            case OpCode.SBC_zX: { name = "SBC"; }
            case OpCode.ASL_z:  { name = "ASL"; }
            case OpCode.ASL_zX: { name = "ASL"; }
            case OpCode.ROL_z:  { name = "ROL"; }
            case OpCode.ROL_zX: { name = "ROL"; }
            case OpCode.LSR_z:  { name = "LSR"; }
            case OpCode.LSR_zX: { name = "LSR"; }
            case OpCode.ROR_z:  { name = "ROR"; }
            case OpCode.ROR_zX: { name = "ROR"; }
            case OpCode.STX_z:  { name = "STX"; }
            case OpCode.STX_zY: { name = "STX"; }
            case OpCode.LDX_z:  { name = "LDX"; }
            case OpCode.LDX_zY: { name = "LDX"; }
            case OpCode.DEC_z:  { name = "DEC"; }
            case OpCode.DEC_zX: { name = "DEC"; }
            case OpCode.INC_z:  { name = "INC"; }
            case OpCode.INC_zX: { name = "INC"; }
            case OpCode.PHP:     { name = "PHP"; }
            case OpCode.CLC:     { name = "CLC"; }
            case OpCode.PLP:     { name = "PLP"; }
            case OpCode.SEC:     { name = "SEC"; }
            case OpCode.PHA:     { name = "PHA"; }
            case OpCode.CLI:     { name = "CLI"; }
            case OpCode.PLA:     { name = "PLA"; }
            case OpCode.SEI:     { name = "SEI"; }
            case OpCode.DEY:     { name = "DEY"; }
            case OpCode.TYA:     { name = "TYA"; }
            case OpCode.TAY:     { name = "TAY"; }
            case OpCode.CLV:     { name = "CLV"; }
            case OpCode.INY:     { name = "INY"; }
            case OpCode.CLD:     { name = "CLD"; }
            case OpCode.INX:     { name = "INX"; }
            case OpCode.SED:     { name = "SED"; }
            case OpCode.ORA_n:   { name = "ORA"; }
            case OpCode.ORA_nnY: { name = "ORA"; }
            case OpCode.AND_n:   { name = "AND"; }
            case OpCode.AND_nnY: { name = "AND"; }
            case OpCode.EOR_n:   { name = "EOR"; }
            case OpCode.EOR_nnY: { name = "EOR"; }
            case OpCode.ADC_n:   { name = "ADC"; }
            case OpCode.ADC_nnY: { name = "ADC"; }
            case OpCode.BIT_n:   { name = "BIT"; }
            case OpCode.STA_nnY: { name = "STA"; }
            case OpCode.LDA_n:   { name = "LDA"; }
            case OpCode.LDA_nnY: { name = "LDA"; }
            case OpCode.CMP_n:   { name = "CMP"; }
            case OpCode.CMP_nnY: { name = "CMP"; }
            case OpCode.SBC_n:   { name = "SBC"; }
            case OpCode.SBC_nnY: { name = "SBC"; }
            case OpCode.ASL:     { name = "ASL"; }
            case OpCode.INC:     { name = "INC"; }
            case OpCode.ROL:     { name = "ROL"; }
            case OpCode.DEC:     { name = "DEC"; }
            case OpCode.LSR:     { name = "LSR"; }
            case OpCode.PHY:     { name = "PHY"; }
            case OpCode.ROR:     { name = "ROR"; }
            case OpCode.PLY:     { name = "PLY"; }
            case OpCode.TXA:     { name = "TXA"; }
            case OpCode.TXS:     { name = "TXS"; }
            case OpCode.TAX:     { name = "TAX"; }
            case OpCode.TSX:     { name = "TSX"; }
            case OpCode.DEX:     { name = "DEX"; }
            case OpCode.PHX:     { name = "PHX"; }
            case OpCode.NOP:     { name = "NOP"; }
            case OpCode.PLX:     { name = "PLX"; }
            case OpCode.WAI:     { name = "WAI"; }
            case OpCode.STP:     { name = "STP"; }
            case OpCode.TSB_nn:  { name = "TSB"; }
            case OpCode.TRB_nn:  { name = "TRB"; }
            case OpCode.BIT_nn:  { name = "BIT"; }
            case OpCode.BIT_nnX: { name = "BIT"; }
            case OpCode.JMP_nn:  { name = "JMP"; }
            case OpCode.JMP_inn: { name = "JMP"; }
            case OpCode.sJMP_inn: { name = "sJMP"; }
            case OpCode.JMP_innX:{ name = "JMP"; }
            case OpCode.STY_nn:  { name = "STY"; }
            case OpCode.STZ_nn:  { name = "STZ"; }
            case OpCode.LDY_nn:  { name = "LDY"; }
            case OpCode.LDY_nnX: { name = "LDY"; }
            case OpCode.CPY_nn:  { name = "CPY"; }
            case OpCode.CPX_nn:  { name = "CPX"; }
            case OpCode.ORA_nn:  { name = "ORA"; }
            case OpCode.ORA_nnX: { name = "ORA"; }
            case OpCode.AND_nn:  { name = "AND"; }
            case OpCode.AND_nnX: { name = "AND"; }
            case OpCode.EOR_nn:  { name = "EOR"; }
            case OpCode.EOR_nnX: { name = "EOR"; }
            case OpCode.ADC_nn:  { name = "ADC"; }
            case OpCode.ADC_nnX: { name = "ADC"; }
            case OpCode.STA_nn:  { name = "STA"; }
            case OpCode.STA_nnX: { name = "STA"; }
            case OpCode.LDA_nn:  { name = "LDA"; }
            case OpCode.LDA_nnX: { name = "LDA"; }
            case OpCode.CMP_nn:  { name = "CMP"; }
            case OpCode.CMP_nnX: { name = "CMP"; }
            case OpCode.SBC_nn:  { name = "SBC"; }
            case OpCode.SBC_nnX: { name = "SBC"; }
            case OpCode.ASL_nn:  { name = "ASL"; }
            case OpCode.ASL_nnX: { name = "ASL"; }
            case OpCode.ROL_nn:  { name = "ROL"; }
            case OpCode.ROL_nnX: { name = "ROL"; }
            case OpCode.LSR_nn:  { name = "LSR"; }
            case OpCode.LSR_nnX: { name = "LSR"; }
            case OpCode.ROR_nn:  { name = "ROR"; }
            case OpCode.ROR_nnX: { name = "ROR"; }
            case OpCode.STX_nn:  { name = "STX"; }
            case OpCode.STZ_nnX: { name = "STZ"; }
            case OpCode.LDX_nn:  { name = "LDX"; }
            case OpCode.LDX_nnY: { name = "LDX"; }
            case OpCode.DEC_nn:  { name = "DEC"; }
            case OpCode.DEC_nnX: { name = "DEC"; }
            case OpCode.INC_nn:  { name = "INC"; }
            case OpCode.INC_nnX: { name = "INC"; }
        
        
            
            case OpCode.BBR0_z_e: { name = "BBR0"; }
            case OpCode.BBR1_z_e: { name = "BBR1"; }
            case OpCode.BBR2_z_e: { name = "BBR2"; }
            case OpCode.BBR3_z_e: { name = "BBR3"; }
            case OpCode.BBR4_z_e: { name = "BBR4"; }
            case OpCode.BBR5_z_e: { name = "BBR5"; }
            case OpCode.BBR6_z_e: { name = "BBR6"; }
            case OpCode.BBR7_z_e: { name = "BBR7"; }
            case OpCode.BBS0_z_e: { name = "BBS0"; }
            case OpCode.BBS1_z_e: { name = "BBS1"; }
            case OpCode.BBS2_z_e: { name = "BBS2"; }
            case OpCode.BBS3_z_e: { name = "BBS3"; }
            case OpCode.BBS4_z_e: { name = "BBS4"; }
            case OpCode.BBS5_z_e: { name = "BBS5"; }
            case OpCode.BBS6_z_e: { name = "BBS6"; }
            case OpCode.BBS7_z_e: { name = "BBS7"; }
            
            case OpCode.RMB0_z: { name = "RMB0"; }
            case OpCode.RMB1_z: { name = "RMB1"; }
            case OpCode.RMB2_z: { name = "RMB2"; }
            case OpCode.RMB3_z: { name = "RMB3"; }
            case OpCode.RMB4_z: { name = "RMB4"; }
            case OpCode.RMB5_z: { name = "RMB5"; }
            case OpCode.RMB6_z: { name = "RMB6"; }
            case OpCode.RMB7_z: { name = "RMB7"; }
            case OpCode.SMB0_z: { name = "SMB0"; }
            case OpCode.SMB1_z: { name = "SMB1"; }
            case OpCode.SMB2_z: { name = "SMB2"; }
            case OpCode.SMB3_z: { name = "SMB3"; }
            case OpCode.SMB4_z: { name = "SMB4"; }
            case OpCode.SMB5_z: { name = "SMB5"; }
            case OpCode.SMB6_z: { name = "SMB6"; }
            case OpCode.SMB7_z: { name = "SMB7"; }
            
            case OpCode.iJMP_nn: { name = "iJMP"; }
        }
        return name;
    }
    
    EmitInstruction(string instructionName, byte operand)
    {
        // Immediate AddressingMode
        OpCode code;
        switch (instructionName)
        {
            case "ADC": { code = OpCode.ADC_n; }
            case "AND": { code = OpCode.AND_n; }
            case "BIT": { code = OpCode.BIT_n; }
            case "CMP": { code = OpCode.CMP_n; }
            case "CPX": { code = OpCode.CPX_n; }
            case "CPY": { code = OpCode.CPY_n; }
            case "EOR": { code = OpCode.EOR_n; }
            case "LDA": { code = OpCode.LDA_n; }
            case "LDX": { code = OpCode.LDX_n; }
            case "LDY": { code = OpCode.LDY_n; }
            case "ORA": { code = OpCode.ORA_n; }
            case "SBC": { code = OpCode.SBC_n; }
            
            default:
            {
                NI();
            }
        }
        ValidateInstruction(code);
        Asm6502.AppendCode(byte(code));
        Asm6502.AppendCode(operand);
    }
    EmitInstructionZeroPageRelative(string instructionName, byte immediateValue, int offset)
    {
        // ZeroPageRelative
        OpCode code;
        switch (instructionName)
        {
            case "BBR0": { code = OpCode.BBR0_z_e; }
            case "BBR1": { code = OpCode.BBR1_z_e; }
            case "BBR2": { code = OpCode.BBR2_z_e; }
            case "BBR3": { code = OpCode.BBR3_z_e; }
            case "BBR4": { code = OpCode.BBR4_z_e; }
            case "BBR5": { code = OpCode.BBR5_z_e; }
            case "BBR6": { code = OpCode.BBR6_z_e; }
            case "BBR7": { code = OpCode.BBR7_z_e; }
            case "BBS0": { code = OpCode.BBS0_z_e; }
            case "BBS1": { code = OpCode.BBS1_z_e; }
            case "BBS2": { code = OpCode.BBS2_z_e; }
            case "BBS3": { code = OpCode.BBS3_z_e; }
            case "BBS4": { code = OpCode.BBS4_z_e; }
            case "BBS5": { code = OpCode.BBS5_z_e; }
            case "BBS6": { code = OpCode.BBS6_z_e; }
            case "BBS7": { code = OpCode.BBS7_z_e; }
                       
            default:
            {
                NI();
            }
        }
        ValidateInstruction(code);
        Asm6502.AppendCode(byte(code));
        Asm6502.AppendCode(immediateValue);
        byte b = offset.GetByte(0);
        Asm6502.AppendCode(b);
    }
    
    EmitInstruction(string instructionName, int offset)
    {
        // Relative AddressingMode
        OpCode code;
        switch (instructionName)
        {
            case "BCC": { code = OpCode.BCC_e; }
            case "BCS": { code = OpCode.BCS_e; }
            case "BEQ": { code = OpCode.BEQ_e; }
            case "BMI": { code = OpCode.BMI_e; }
            case "BNE": { code = OpCode.BNE_e; }
            case "BPL": { code = OpCode.BPL_e; }
            case "BRA": { code = OpCode.BRA_e; }
            case "BVC": { code = OpCode.BRA_e; }
            case "BVS": { code = OpCode.BVS_e; }
            default:
            {
                NI();
            }
        }
        ValidateInstruction(code);
        Asm6502.AppendCode(byte(code));
        byte b = offset.GetByte(0);
        Asm6502.AppendCode(b);
    }
    EmitInstructionAbsolute(string instructionName, uint operand, AddressingModes addressingMode)
    {
        OpCode code;
        if (addressingMode == AddressingModes.Absolute)
        {
            // Absolute=0x0008,          // nnnn
            switch (instructionName)
            {
                case "ADC": { code = OpCode.ADC_nn; }
                case "AND": { code = OpCode.AND_nn; }
                case "ASL": { code = OpCode.ASL_nn; }
                case "BIT": { code = OpCode.BIT_nn; }
                case "CMP": { code = OpCode.CMP_nn; }
                case "CPX": { code = OpCode.CPX_nn; }
                case "CPY": { code = OpCode.CPY_nn; }
                case "DEC": { code = OpCode.DEC_nn; }
                case "EOR": { code = OpCode.EOR_nn; }
                case "INC": { code = OpCode.INC_nn; }
                case "JMP": { code = OpCode.JMP_nn; }
                case "JSR": { code = OpCode.JSR_nn; }
                case "iJMP":{ code = OpCode.iJMP_nn; }
                case "LDA": { code = OpCode.LDA_nn; }
                case "LDX": { code = OpCode.LDX_nn; }
                case "LDY": { code = OpCode.LDY_nn; }
                case "LSR": { code = OpCode.LSR_nn; }
                case "ORA": { code = OpCode.ORA_nn; }
                case "ROL": { code = OpCode.ROL_nn; }
                case "ROR": { code = OpCode.ROR_nn; }
                case "SBC": { code = OpCode.SBC_nn; }
                case "STA": { code = OpCode.STA_nn; }    
                case "STX": { code = OpCode.STX_nn; }
                case "STY": { code = OpCode.STY_nn; }
                case "STZ": { code = OpCode.STZ_nn; }
                case "TRB": { code = OpCode.TRB_nn; }
                case "TSB": { code = OpCode.TSB_nn; }
            }
        }
        else if (addressingMode == AddressingModes.AbsoluteX)
        {
            switch (instructionName)
            {
                case "ADC": { code = OpCode.ADC_nnX; }
                case "AND": { code = OpCode.AND_nnX; }
                case "ASL": { code = OpCode.ASL_nnX; }
                case "BIT": { code = OpCode.BIT_nnX; }
                case "CMP": { code = OpCode.CMP_nnX; }
                case "DEC": { code = OpCode.DEC_nnX; }
                case "EOR": { code = OpCode.EOR_nnX; }
                case "INC": { code = OpCode.INC_nnX; }
                case "LDA": { code = OpCode.LDA_nnX; }
                case "LDY": { code = OpCode.LDY_nnX; }
                case "LSR": { code = OpCode.LSR_nnX; }
                case "ORA": { code = OpCode.ORA_nnX; }
                case "ROL": { code = OpCode.ROL_nnX; }
                case "ROR": { code = OpCode.ROR_nnX; }
                case "SBC": { code = OpCode.SBC_nnX; }
                case "STA": { code = OpCode.STA_nnX; }    
                case "STZ": { code = OpCode.STZ_nnX; }
            }
        }
        else if (addressingMode == AddressingModes.AbsoluteY)
        {
            switch (instructionName)
            {
                case "ADC": { code = OpCode.ADC_nnY; }
                case "AND": { code = OpCode.AND_nnY; }
                case "CMP": { code = OpCode.CMP_nnY; }
                case "EOR": { code = OpCode.EOR_nnY; }
                case "LDA": { code = OpCode.LDA_nnY; }
                case "LDX": { code = OpCode.LDX_nnY; }
                case "ORA": { code = OpCode.ORA_nnY; }
                case "SBC": { code = OpCode.SBC_nnY; }
                case "STA": { code = OpCode.STA_nnY; }    
            }
        }
        else if (addressingMode == AddressingModes.AbsoluteIndirectX)
        {
            switch (instructionName)
            {
                case "JMP": { code = OpCode.JMP_innX; }
                default:
                {
                    NI();
                }
            }
        }
        else if (addressingMode == AddressingModes.AbsoluteIndirect)
        {
            switch (instructionName)
            {
                case "JMP": { code = OpCode.JMP_inn; }
                case "sJMP": { code = OpCode.sJMP_inn; }
                default:
                {
                    NI();
                }
            }
        }
        else
        {
            IE();
        }
        ValidateInstruction(code);
        Asm6502.AppendCode(byte(code));
        Asm6502.AppendCode(byte(operand & 0xFF));
        Asm6502.AppendCode(byte(operand >> 8));
    }
    EmitInstructionZeroPage(string instructionName, byte operand, AddressingModes addressingMode)
    {
        OpCode code;
        if (addressingMode == AddressingModes.XIndexedZeroPage)
        {
            // XIndexedZeroPage=0x1000,  // [nn,X]
            switch (instructionName)
            {
                case "ADC": { code = OpCode.ADC_izX; }
                case "AND": { code = OpCode.AND_izX; }
                case "CMP": { code = OpCode.CMP_izX; }
                case "EOR": { code = OpCode.EOR_izX; }
                case "LDA": { code = OpCode.LDA_izX; }
                case "ORA": { code = OpCode.ORA_izX; }
                case "SBC": { code = OpCode.SBC_izX; }
                case "STA": { code = OpCode.STA_izX; }
                default: { IE(); }
            }
        }
        else if (addressingMode == AddressingModes.YIndexedZeroPage)
        {
            // YIndexedZeroPage=0x2000,  // [nn], Y
            switch (instructionName)
            {
                case "ADC": { code = OpCode.ADC_izY; }
                case "AND": { code = OpCode.AND_izY; }
                case "CMP": { code = OpCode.CMP_izY; }
                case "EOR": { code = OpCode.EOR_izY; }
                case "LDA": { code = OpCode.LDA_izY; }
                case "ORA": { code = OpCode.ORA_izY; }
                case "SBC": { code = OpCode.SBC_izY; }
                case "STA": { code = OpCode.STA_izY; }
                default: { IE(); }
            }
        }
        else if (addressingMode == AddressingModes.ZeroPageIndirect)
        {
            // ZeroPageIndirect=0x0800,  // [nn]        
            switch (instructionName)
            {
                case "ADC": { code = OpCode.ADC_iz; }
                case "AND": { code = OpCode.AND_iz; }
                case "CMP": { code = OpCode.CMP_iz; }
                case "EOR": { code = OpCode.EOR_iz; }
                case "LDA": { code = OpCode.LDA_iz; }
                case "ORA": { code = OpCode.ORA_iz; }
                case "SBC": { code = OpCode.SBC_iz; }
                case "STA": { code = OpCode.STA_iz; }
                default: { IE(); }
            }
        }
        else if (addressingMode == AddressingModes.ZeroPageY)
        {
            // ZeroPageY,  // nn, Y        
            switch (instructionName)
            {
                case "LDX": { code = OpCode.LDX_zY; }
                case "STX": { code = OpCode.STX_zY; }
                default: { IE(); }
            }
        }
        else if (addressingMode == AddressingModes.ZeroPageX)
        {
            // ZeroPageX,  // nn, X        
            switch (instructionName)
            {
                case "ADC": { code = OpCode.ADC_zX; }
                case "AND": { code = OpCode.AND_zX; }
                case "ASL": { code = OpCode.ASL_zX; }
                case "BIT": { code = OpCode.BIT_zX; }
                case "CMP": { code = OpCode.CMP_zX; }
                case "DEC": { code = OpCode.DEC_zX; }
                case "EOR": { code = OpCode.EOR_zX; }
                case "INC": { code = OpCode.INC_zX; }
                case "LDA": { code = OpCode.LDA_zX; }
                case "LDY": { code = OpCode.LDY_zX; }
                case "LSR": { code = OpCode.LSR_zX; }
                case "ORA": { code = OpCode.ORA_zX; }
                case "ROL": { code = OpCode.ROL_zX; }
                case "ROR": { code = OpCode.ROR_zX; }
                case "SBC": { code = OpCode.SBC_zX; }
                case "STA": { code = OpCode.STA_zX; }
                case "STY": { code = OpCode.STY_zX; }
                case "STZ": { code = OpCode.STZ_zX; }
                default: { IE(); }
            }
        }
        else if (addressingMode == AddressingModes.ZeroPage)
        {
            // ZeroPage=0x0100,          // nn       
            switch (instructionName)
            {
                case "ADC": { code = OpCode.ADC_z; }
                case "AND": { code = OpCode.AND_z; }
                case "ASL": { code = OpCode.ASL_z; }
                case "BIT": { code = OpCode.BIT_z; }
                case "CMP": { code = OpCode.CMP_z; }
                case "CPX": { code = OpCode.CPX_z; }
                case "CPY": { code = OpCode.CPY_z; }
                case "DEC": { code = OpCode.DEC_z; }
                case "EOR": { code = OpCode.EOR_z; }
                case "INC": { code = OpCode.INC_z; }
                case "LDA": { code = OpCode.LDA_z; }
                case "LDX": { code = OpCode.LDX_z; }
                case "LDY": { code = OpCode.LDY_z; }
                case "LSR": { code = OpCode.LSR_z; }
                case "ORA": { code = OpCode.ORA_z; }
                case "ROL": { code = OpCode.ROL_z; }
                case "ROR": { code = OpCode.ROR_z; }
                case "SBC": { code = OpCode.SBC_z; }
                case "STA": { code = OpCode.STA_z; }
                case "STX": { code = OpCode.STX_z; }
                case "STY": { code = OpCode.STY_z; }
                case "STZ": { code = OpCode.STZ_z; }
                case "TRB": { code = OpCode.TRB_z; }
                case "TSB": { code = OpCode.TSB_z; }
                
                case "RMB0": { code = OpCode.RMB0_z; }
                case "RMB1": { code = OpCode.RMB1_z; }
                case "RMB2": { code = OpCode.RMB2_z; }
                case "RMB3": { code = OpCode.RMB3_z; }
                case "RMB4": { code = OpCode.RMB4_z; }
                case "RMB5": { code = OpCode.RMB5_z; }
                case "RMB6": { code = OpCode.RMB6_z; }
                case "RMB7": { code = OpCode.RMB7_z; }
                case "SMB0": { code = OpCode.SMB0_z; }
                case "SMB1": { code = OpCode.SMB1_z; }
                case "SMB2": { code = OpCode.SMB2_z; }
                case "SMB3": { code = OpCode.SMB3_z; }
                case "SMB4": { code = OpCode.SMB4_z; }
                case "SMB5": { code = OpCode.SMB5_z; }
                case "SMB6": { code = OpCode.SMB6_z; }
                case "SMB7": { code = OpCode.SMB7_z; }
                
                default: { IE(); }
            }
        }
        
        else
        {
            IE();
        }
        ValidateInstruction(code);
        Asm6502.AppendCode(byte(code));
        Asm6502.AppendCode(operand);
    }
    EmitInstruction(string instructionName)
    {
        // Implied AddressingMode
        // Accumulator AddressingMode
        OpCode code;
        switch (instructionName)
        {
            case "ASL": { code = OpCode.ASL; }
            case "DEC": { code = OpCode.DEC; }
            case "INC": { code = OpCode.INC; }
            case "LSR": { code = OpCode.LSR; }
            case "ROL": { code = OpCode.ROL; }
            case "ROR": { code = OpCode.ROR; }
            
            case "BRK": { code = OpCode.BRK; }
            case "CLC": { code = OpCode.CLC; }
            case "CLD": { code = OpCode.CLD; }
            case "CLI": { code = OpCode.CLI; }
            case "CLV": { code = OpCode.CLV; }
            case "DEX": { code = OpCode.DEX; }
            case "DEY": { code = OpCode.DEY; }
            case "INX": { code = OpCode.INX; }
            case "INY": { code = OpCode.INY; }
            case "NOP": { code = OpCode.NOP; }
            case "PHA": { code = OpCode.PHA; }
            case "PHP": { code = OpCode.PHP; }
            case "PHX": { code = OpCode.PHX; }
            case "PHY": { code = OpCode.PHY; }
            case "PLA": { code = OpCode.PLA; }
            case "PLP": { code = OpCode.PLP; }
            case "PLX": { code = OpCode.PLX; }
            case "PLY": { code = OpCode.PLY; }
            case "RTI": { code = OpCode.RTI; }
            case "RTS": { code = OpCode.RTS; }
            case "SEC": { code = OpCode.SEC; }
            case "SED": { code = OpCode.SED; }
            case "SEI": { code = OpCode.SEI; }
            case "TAX": { code = OpCode.TAX; }
            case "TAY": { code = OpCode.TAY; }
            case "TSX": { code = OpCode.TSX; }
            case "TXA": { code = OpCode.TXA; }
            case "TXS": { code = OpCode.TXS; }
            case "TYA": { code = OpCode.TYA; }
            default:
            {
                NI();
            }
        }
        ValidateInstruction(code);
        Asm6502.AppendCode(byte(code));
    }
    
    // Direct support for a small subset of instructions that 
    // are used directly by the assembler:
    OpCode GetRETInstruction()
    {
        return OpCode.RTS; // RTS
    }
    OpCode GetRTIInstruction()
    {
        return OpCode.RTI; // RTI
    }
    
    OpCode GetHALTInstruction()
    {
        return OpCode.STP; // STP (stop)
    }
    
    OpCode GetJMPInstruction()
    {
        return OpCode.JMP_nn; // JMP
    }
    OpCode GetJMPIndexInstruction()
    {
        return OpCode.sJMP_inn;
    }
    OpCode GetNOPInstruction()
    {
        return OpCode.NOP; // NOP
    }
    OpCode GetBInstruction(string condition)
    {
        OpCode code;
        switch (condition)
        {
            case "Z":  { code = OpCode.BEQ_e; }
            case "NZ": { code = OpCode.BNE_e; }
            case "C":  { code = OpCode.BCS_e; }
            case "NC": { code = OpCode.BCC_e; }
            case "MI": { code = OpCode.BMI_e; }
            case "PL": { code = OpCode.BPL_e; }
            case "":   { code = OpCode.BRA_e; }
            default:   { NI();   }
        }
        ValidateInstruction(code);
        return code;
    }
    
    OpCode GetJPInstruction(string condition)
    {
        switch (condition)
        {
            case "Z":  { return OpCode.BEQ_e; } // JP Z
            case "NZ": { return OpCode.BNE_e; } // JP NZ
            default:   { NI(); }
        }
        return OpCode.NOP;
    }
    OpCode GetJSRInstruction()
    {
        return OpCode.JSR_nn; // JSR
    }
    OpCode GetiJMPInstruction()
    {
        return OpCode.iJMP_nn; // iJMP - fake internal instruction : JMP to an unresolved methodIndex
    }
    OpCode GetCALLInstruction()
    {
        return OpCode.JSR_nn; // CALL
    }
    
    bool IsMethodExitInstruction(OpCode opCode)
    {
        switch (opCode)
        {
            case OpCode.RTI: // RTI
            case OpCode.RTS: // RTS
            case OpCode.STP: // STP
            case OpCode.iJMP_nn: // iJMP
            case OpCode.sJMP_inn:
            {
                return true;
            }
        }
        return false;
    }
    
    bool IsJumpInstruction(OpCode instruction, ref AddressingModes addressingMode, ref bool isConditional)
    {
        addressingMode = GetAddressingMode(instruction);
        isConditional = false;
        switch (instruction)
        {
            case OpCode.JMP_nn: //  JMP nnnn
            {
                return true;
            }
            case OpCode.JMP_inn:  // JMP [nnnn]
            case OpCode.JMP_innX: // JMP [nnnn, X]
            case OpCode.BRA_e:
            {
                return true;
            }
            
            case OpCode.BCC_e:
            case OpCode.BCS_e:
            case OpCode.BEQ_e:
            case OpCode.BMI_e:
            case OpCode.BNE_e:
            case OpCode.BPL_e:
            case OpCode.BVC_e:
            case OpCode.BVS_e:
            
            case OpCode.BBR0_z_e: // BBR0
            case OpCode.BBR1_z_e: // BBR1
            case OpCode.BBR2_z_e: // BBR2
            case OpCode.BBR3_z_e: // BBR3
            case OpCode.BBR4_z_e: // BBR4
            case OpCode.BBR5_z_e: // BBR5
            case OpCode.BBR6_z_e: // BBR6
            case OpCode.BBR7_z_e: // BBR7
            case OpCode.BBS0_z_e: // BBS0
            case OpCode.BBS1_z_e: // BBS1
            case OpCode.BBS2_z_e: // BBS2
            case OpCode.BBS3_z_e: // BBS3
            case OpCode.BBS4_z_e: // BBS4
            case OpCode.BBS5_z_e: // BBS5
            case OpCode.BBS6_z_e: // BBS6
            case OpCode.BBS7_z_e: // BBS7
            {
                isConditional = true;
                return true;
            }
            
        }
        return false;
    }
    
    
    <byte> CurrentStream { get { return currentStream; } }
    <string,string> DebugInfo { get { return debugInfo; } }
    ClearDebugInfo()
    {
        debugInfo.Clear();
        debugInfoLineUsed.Clear();
    }
    
    bool InUse { get { return currentStream.Count != 0; } } 
    
    uint NextAddress 
    { 
        get 
        { 
            return currentStream.Count;
        } 
    }
    byte GetCodeByte(uint index)
    {
        return currentStream[index];
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
    AppendCode(OpCode code)
    {
        ValidateInstruction(code);
        currentStream.Append(byte(code));
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
    
    bool LastInstructionIsRET(bool orHALT) // to make Block happy
    {
        bool isRET;
        if (currentStream.Count > 0)
        {
            uint iLast = currentStream.Count - 1;
            OpCode last = OpCode(currentStream[iLast]);
            isRET = (last == Asm6502.GetRETInstruction()) || (last == Asm6502.GetRTIInstruction());
            if (!isRET && orHALT)
            {      
                isRET = (last == Asm6502.GetHALTInstruction());
            }
        }    
        return isRET;
    }
    
    AddInstructionRESET()
    {
        // program entry code
        Asm6502.EmitInstruction("CLD");
        Asm6502.EmitInstruction("LDX", 0xFF);
        Asm6502.EmitInstruction("TXS");
    }
    AddInstructionENTER()
    {
        // method entry code
    }
    AddInstructionRET(uint slotsToPop)
    {
        uint iCurrent = Types.GetCurrentMethod();
        string name = Symbols.GetFunctionName(iCurrent);
        
        OpCode retw = Asm6502.GetRETInstruction();
        bool addNOP;
        if (name.EndsWith(".Hopper")) // TODO : only allow in 'program'
        {
            // this means we are exiting Hopper()
            retw = Asm6502.GetHALTInstruction();
        }
        else if (name.EndsWith(".IRQ") || name.EndsWith(".NMI")) // TODO : only allow in 'program'
        {
            retw = Asm6502.GetRTIInstruction();
        }
        
        currentStream.Append(byte(retw));
    }
    string Disassemble(uint address, OpCode instruction, uint operand)
    {
        string disassembly;
        disassembly += "0x" + address.ToHexString(4);
        disassembly += " ";
        
        disassembly +=  " 0x" + (byte(instruction)).ToHexString(2);
        disassembly += " ";
        
        uint length = GetInstructionLength(instruction);
        string operandString = "         "; 
        if (length == 2)
        {
            operandString = "0x" + operand.ToHexString(2) + "     "; 
        }
        else if (length >= 3)
        {
            
            operandString = "0x" + (operand & 0xFF).ToHexString(2) + " 0x" + (operand >> 8).ToHexString(2); 
        }
        disassembly += operandString;
        disassembly += "  ";
        string name = Asm6502.GetName(instruction);
        AddressingModes addressingMode = Asm6502.GetAddressingMode(instruction);
        switch (addressingMode)
        {
            case AddressingModes.Accumulator:       { disassembly += (name + " A"); }
            case AddressingModes.Implied:           { disassembly += name; }
            case AddressingModes.Immediate:         { disassembly += (name + " #0x" + operand.ToHexString(2)); }
            case AddressingModes.Absolute:          { disassembly += (name + " 0x" + operand.ToHexString(4)); }
            case AddressingModes.AbsoluteX:         { disassembly += (name + " 0x" + operand.ToHexString(4) + ",X"); }
            case AddressingModes.AbsoluteY:         { disassembly += (name + " 0x" + operand.ToHexString(4) + ",Y"); }
            case AddressingModes.AbsoluteIndirect:  { disassembly += (name + " [0x" + operand.ToHexString(4) + "]"); }
            case AddressingModes.AbsoluteIndirectX: { disassembly += (name + " [0x" + operand.ToHexString(4) + ",X]"); }
            case AddressingModes.ZeroPage:          { disassembly += (name + " 0x" + operand.ToHexString(2)); }
            case AddressingModes.ZeroPageX:         { disassembly += (name + " 0x" + operand.ToHexString(2) + ",X"); }
            case AddressingModes.ZeroPageY:         { disassembly += (name + " 0x" + operand.ToHexString(2) + ",Y"); }
            case AddressingModes.ZeroPageIndirect:  { disassembly += (name + " [0x" + operand.ToHexString(2) +"]"); }
            case AddressingModes.XIndexedZeroPage:  { disassembly += (name + " [0x" + operand.ToHexString(2) +",X]"); }
            case AddressingModes.YIndexedZeroPage:  { disassembly += (name + " [0x" + operand.ToHexString(2) +"],Y"); }
            
            case AddressingModes.Relative:  
            { 
                int ioperand = int(operand);
                if (ioperand > 127)
                {
                    ioperand = ioperand - 256; // 0xFF -> -1
                }
                long target = long(address) + length + ioperand;
                disassembly += (name + " 0x" + target.ToHexString(4) + " (" + (ioperand < 0 ? "" : "+") + ioperand.ToString() + ")"); 
            }
            case AddressingModes.ZeroPageRelative:
            {
                int ioperand = int(operand >> 8);
                if (ioperand > 127)
                {
                    ioperand = ioperand - 256; // 0xFF -> -1
                }
                long target = long(address) + length + ioperand;
                disassembly += (name + " 0x" + (operand & 0xFF).ToHexString(2) +", 0x" + target.ToHexString(4) + " (" + (ioperand < 0 ? "" : "+") + ioperand.ToString() + ")"); 
            }
            
                default: { disassembly += name; }
            }
        return disassembly;
    }
    
    PatchJump(uint jumpAddress, uint jumpToAddress) 
    {
        PatchJump(jumpAddress, jumpToAddress, false);
    }  
    PatchJump(uint jumpAddress, uint jumpToAddress, bool forceLong) 
    {
        if (!forceLong && (Architecture != CPUArchitecture.M6502))       
        {
            OpCode braInstruction = GetBInstruction("");
            OpCode jmpInstruction = GetJMPInstruction();
            int offset = int(jumpToAddress) - int(jumpAddress) - 2;
            
            if ((offset < -128) || (offset > 127))
            {
                // long jump
                if ((OpCode(currentStream[jumpAddress+0]) != braInstruction) &&
                    (OpCode(currentStream[jumpAddress+0]) != jmpInstruction))
                {
                    Parser.Error("jump target exceeds 6502 relative limit (" + offset.ToString() + ")"); Die(0x0B);
                    return;
                }
                currentStream.SetItem(jumpAddress+0, byte(GetJMPInstruction()));
                currentStream.SetItem(jumpAddress+1, jumpToAddress.GetByte(0));
                currentStream.SetItem(jumpAddress+2, jumpToAddress.GetByte(1));
            }
            else
            {
                // short jump
                if (OpCode(currentStream[jumpAddress+0]) == jmpInstruction)
                {
                    currentStream.SetItem(jumpAddress+0, byte(GetBInstruction("")));
                }
                currentStream.SetItem(jumpAddress+1, offset.GetByte(0));
                currentStream.SetItem(jumpAddress+2, byte(GetNOPInstruction()));
            }
        }
        else
        {
            currentStream.SetItem(jumpAddress+1, jumpToAddress.GetByte(0));
            currentStream.SetItem(jumpAddress+2, jumpToAddress.GetByte(1));
        }
    }
    
    AddInstructionJ()
    {
        AddInstructionJ(0x0000); // placeholder address
    }
    AddInstructionJ(uint jumpToAddress)
    {
        uint jumpAddress = NextAddress;
        int offset = int(jumpToAddress) - int(jumpAddress) - 2;
        
        if ((Architecture == CPUArchitecture.M6502) || (offset < -128) || (offset > 127))
        {
            // long jump
            currentStream.Append(byte(GetJMPInstruction()));
            currentStream.Append(byte(jumpToAddress & 0xFF));
            currentStream.Append(byte(jumpToAddress >> 8));
        }
        else
        {
            // short jump
            currentStream.Append(byte(GetBInstruction("")));
            currentStream.Append(offset.GetByte(0));
            currentStream.Append(byte(GetNOPInstruction()));
        }
    }
    AddInstructionJZ()
    {
        currentStream.Append(byte(GetBInstruction("Z")));
        // placeholder address
        currentStream.Append(0x00);
        currentStream.Append(byte(GetNOPInstruction()));
    }
    AddInstructionJNZ()
    {
        currentStream.Append(byte(GetBInstruction("NZ")));
        // placeholder address
        currentStream.Append(0x00);
        currentStream.Append(byte(GetNOPInstruction()));
    }
    AddInstructionCALL(uint iOverload)
    {
        currentStream.Append(byte(GetJSRInstruction()));

        // unresolved method index for now
        currentStream.Append(byte(iOverload & 0xFF));
        currentStream.Append(byte(iOverload >> 8));
    }
    
    AddInstructionCMP(char register, byte operand)
    {
        switch (register)
        {
            case 'A':
            {
                Asm6502.EmitInstruction("CMP", operand);
            }
            case 'X':
            {
                Asm6502.EmitInstruction("CPX", operand);
            }
            case 'Y':
            {
                Asm6502.EmitInstruction("CPY", operand);
            }
            default:
            {
                IE();
            }
        }
    }
}
