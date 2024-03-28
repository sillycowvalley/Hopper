unit AsmZ80
{
    const uint CallStackAddress  = 0xFC00; // 512 bytes
    const uint CallStackSize     = 0x0200;
    
    const uint ValueStackAddress = 0xFE00; // 512 bytes
        
    enum OpCode
    {
        NOP         = 0x00,
        
        CLP         = 0x2F,
        
        RST_Reset         = 0xC7, // 0x00
        RST_PopAbsolute   = 0xCF, // 0x08
        RST_PushAbsolute  = 0xD7, // 0x10
        RST_PushImmediate = 0xDF, // 0x18
        RST_PushOffset    = 0xE7, // 0x20
        RST_PopOffset     = 0xEF, // 0x28
        RST_SysCall0      = 0xF7, // 0x30
        RST_Instruction   = 0xFF, // 0x38
        
        LD_A_n = 0x3E,
        LD_B_n = 0x06,
        LD_C_n = 0x0E,
        LD_D_n = 0x16,
        LD_E_n = 0x1E,
        LD_H_n = 0x26,
        LD_L_n = 0x2E,
        
        LD_B_B = 0x40,
        LD_B_C = 0x41,
        LD_B_D = 0x42,
        LD_B_E = 0x43,
        LD_B_H = 0x44,
        LD_B_L = 0x45,
        LD_B_A = 0x47,
        
        LD_C_B = 0x48,
        LD_C_C = 0x49,
        LD_C_D = 0x4A,
        LD_C_E = 0x4B,
        LD_C_H = 0x4C,
        LD_C_L = 0x4D,
        LD_C_A = 0x4F,
        
        LD_D_B = 0x50,
        LD_D_C = 0x51,
        LD_D_D = 0x52,
        LD_D_E = 0x53,
        LD_D_H = 0x54,
        LD_D_IYH = 0xFD54,   // #FD #54       LD   D,IYH - http://www.z80.info/z80undoc.htm
        LD_D_L = 0x55,
        LD_D_A = 0x57,
        
        LD_E_B = 0x58,
        LD_E_C = 0x59,
        LD_E_D = 0x5A,
        LD_E_E = 0x5B,
        LD_E_H = 0x5C,
        LD_E_L = 0x5D,
        LD_E_IYL = 0xFD5D,   // #FD #5D       LD   E,IYL - http://www.z80.info/z80undoc.htm
        LD_E_A = 0x5F,
        
        
        
        
        LD_H_B = 0x60,
        LD_H_C = 0x61,
        LD_H_D = 0x62,
        LD_H_E = 0x63,
        LD_H_H = 0x64,
        LD_H_L = 0x65,
        LD_H_A = 0x67,
        
        LD_L_B = 0x68,
        LD_L_C = 0x69,
        LD_L_D = 0x6A,
        LD_L_E = 0x6B,
        LD_L_H = 0x6C,
        LD_L_L = 0x6D,
        LD_L_A = 0x6F,
        
        LD_A_B = 0x78,
        LD_A_C = 0x79,
        LD_A_D = 0x7A,
        LD_A_E = 0x7B,
        LD_A_H = 0x7C,
        LD_A_L = 0x7D,
        LD_A_A = 0x7F,
        
        
        
        
        LD_A_iHL = 0x7E,
        LD_B_iHL = 0x46,
        LD_C_iHL = 0x4E,
        LD_D_iHL = 0x56,
        LD_E_iHL = 0x5E,
        LD_H_iHL = 0x66,
        LD_L_iHL = 0x6E,
        
        LD_A_iIX_d = 0xDD7E,
        LD_B_iIX_d = 0xDD46,
        LD_C_iIX_d = 0xDD4E,
        LD_D_iIX_d = 0xDD56,
        LD_E_iIX_d = 0xDD5E,
        LD_H_iIX_d = 0xDD66,
        LD_L_iIX_d = 0xDD6E,
        
        LD_A_iIY_d = 0xFD7E,
        LD_B_iIY_d = 0xFD46,
        LD_C_iIY_d = 0xFD4E,
        LD_D_iIY_d = 0xFD56,
        LD_E_iIY_d = 0xFD5E,
        LD_H_iIY_d = 0xFD66,
        LD_L_iIY_d = 0xFD6E,
        
        LD_iIX_d_A = 0xDD77,
        LD_iIX_d_B = 0xDD70,
        LD_iIX_d_C = 0xDD71,
        LD_iIX_d_D = 0xDD72,
        LD_iIX_d_E = 0xDD73,
        LD_iIX_d_H = 0xDD74,
        LD_iIX_d_L = 0xDD75,
        
        LD_iIY_d_A = 0xFD77,
        LD_iIY_d_B = 0xFD70,
        LD_iIY_d_C = 0xFD71,
        LD_iIY_d_D = 0xFD72,
        LD_iIY_d_E = 0xFD73,
        LD_iIY_d_H = 0xFD74,
        LD_iIY_d_L = 0xFD75,
        
        LD_iIX_d_n = 0xDD36,
        LD_iIY_d_n = 0xFD36,
        
        LD_BC_nn = 0x01,
        LD_DE_nn = 0x11,
        LD_HL_nn = 0x21,
        LD_IX_nn = 0xDD21,
        LD_IY_nn = 0xFD21,
        LD_SP_nn = 0x31,
        
        LD_SP_HL = 0xF9,
        
        LD_iHL_n = 0x36,
        
        LD_iHL_B = 0x70,
        LD_iHL_C = 0x71,
        LD_iHL_D = 0x72,
        LD_iHL_E = 0x73,
        LD_iHL_H = 0x74,
        LD_iHL_L = 0x75,
        LD_iHL_A = 0x77,
                
        INC_DE  = 0x03,
        INC_BC  = 0x13,
        INC_HL  = 0x23,
        INC_IX  = 0xDD23,
        INC_IY  = 0xFD23,
        
        INC_iIX_d = 0xDD34,
        INC_iIY_d = 0xFD34,
        
        INC_A = 0x3C,
        INC_B = 0x04,
        INC_C = 0x0C,
        INC_D = 0x14,
        INC_E = 0x1C,
        INC_H = 0x24,
        INC_L = 0x2C,
        
        DEC_A = 0x3D,
        DEC_B = 0x05,
        DEC_C = 0x0D,
        DEC_D = 0x15,
        DEC_E = 0x1D,
        DEC_H = 0x25,
        DEC_L = 0x2D,
        
        DEC_iHL   = 0x35,
        DEC_iIX_d = 0xDD35,
        DEC_iIY_d = 0xFD35,
        
        DEC_BC  = 0x0B,
        DEC_HL  = 0x2B,
        DEC_IX  = 0xDD2B,
        DEC_IY  = 0xFD2B,
        
        ADD_A_B = 0x80,
        ADD_A_C = 0x81,
        ADD_A_D = 0x82,
        ADD_A_E = 0x83,
        ADD_A_H = 0x84,
        ADD_A_L = 0x85,
        ADD_A_A = 0x87,
        ADD_A_iHL = 0x86,
        ADD_A_iIX_d = 0xDD86,
        ADD_A_iIY_d = 0xFD86,
        ADD_A_n = 0xC6,
        
        SUB_A_B = 0x90,
        SUB_A_C = 0x91,
        SUB_A_D = 0x92,
        SUB_A_E = 0x93,
        SUB_A_H = 0x94,
        SUB_A_L = 0x95,
        SUB_A_A = 0x97,
        SUB_A_iHL = 0x96,
        SUB_A_iIX_d = 0xDD96,
        SUB_A_iIY_d = 0xFD96,
        SUB_A_n = 0xD6,
        
        SBC_A_B = 0x98,
        SBC_A_C = 0x99,
        SBC_A_D = 0x9A,
        SBC_A_E = 0x9B,
        SBC_A_H = 0x9C,
        SBC_A_L = 0x9D,
        SBC_A_A = 0x9F,
        SBC_A_iHL = 0x9E,
        SBC_A_iIX_d = 0xDD9E,
        SBC_A_iIY_d = 0xFD9E,
        SBC_A_n = 0xDE,
        
        AND_A_B = 0xA0,
        AND_A_C = 0xA1,
        AND_A_D = 0xA2,
        AND_A_E = 0xA3,
        AND_A_H = 0xA4,
        AND_A_L = 0xA5,
        AND_A_A = 0xA7,
        AND_A_iHL = 0xA6,
        AND_A_iIX_d = 0xDDA6,
        AND_A_iIY_d = 0xFDA6,
        
        XOR_A_B = 0xA8,
        XOR_A_C = 0xA9,
        XOR_A_D = 0xAA,
        XOR_A_E = 0xAB,
        XOR_A_H = 0xAC,
        XOR_A_L = 0xAD,
        XOR_A_A = 0xAF,
        XOR_A_iHL = 0xAE,
        XOR_A_iIX_d = 0xDDAE,
        XOR_A_iIY_d = 0xFDAE,
        
        OR_A_B = 0xB0,
        OR_A_C = 0xB1,
        OR_A_D = 0xB2,
        OR_A_E = 0xB3,
        OR_A_H = 0xB4,
        OR_A_L = 0xB5,
        OR_A_A = 0xB7,
        OR_A_iHL = 0xB6,
        OR_A_iIX_d = 0xDDB6,
        OR_A_iIY_d = 0xFDB6,
        
        CP_A_B = 0xB8,
        CP_A_C = 0xB9,
        CP_A_D = 0xBA,
        CP_A_E = 0xBB,
        CP_A_H = 0xBC,
        CP_A_L = 0xBD,
        CP_A_A = 0xBF,
        CP_A_iHL = 0xBE,
        CP_A_iIX_d = 0xDDBE,
        CP_A_iIY_d = 0xFDBE,
        
                
        ADD_IX_BC = 0xDD09,
        ADD_IX_DE = 0xDD19,
        ADD_IY_BC = 0xFD09,
        ADD_IY_DE = 0xFD19,
        
        ADC_A_B = 0x88,
        ADC_A_C = 0x89,
        ADC_A_D = 0x8A,
        ADC_A_E = 0x8B,
        ADC_A_H = 0x8C,
        ADC_A_L = 0x8D,
        ADC_A_A = 0x8F,
        
        ADC_A_n = 0xCE,
        ADC_A_iHL = 0x8E,
        ADC_A_iIX_d = 0xDD8E,
        ADC_A_iIY_d = 0xFD8E,
        
        
        XOR_A = 0xAF,
        XOR_B = 0xA8,
        XOR_C = 0xA9,
        XOR_D = 0xAA,
        XOR_E = 0xAB,
        XOR_H = 0xAC,
        XOR_L = 0xAD,
        
        CP_A_n = 0xFE,
        
        SLA_B = 0xCB20,
        SLA_C = 0xCB21,
        SLA_D = 0xCB22,
        SLA_E = 0xCB23,
        SLA_H = 0xCB24,
        SLA_L = 0xCB25,
        SLA_iHL = 0xCB26,
        SLA_A = 0xCB27,
        
        SRL_B = 0xCB38,
        SRL_C = 0xCB39,
        SRL_D = 0xCB3A,
        SRL_E = 0xCB3B,
        SRL_H = 0xCB3C,
        SRL_L = 0xCB3D,
        SRL_iHL = 0xCB3E,
        SRL_A = 0xCB3F,
        
        
        RLC_B = 0xCB00,
        RLC_C = 0xCB01,
        RLC_D = 0xCB02,
        RLC_E = 0xCB03,
        RLC_H = 0xCB04,
        RLC_L = 0xCB05,
        RLC_iHL = 0xCB06,
        RLC_A = 0xCB07,
        
        RL_B = 0xCB10,
        RL_C = 0xCB11,
        RL_D = 0xCB12,
        RL_E = 0xCB13,
        RL_H = 0xCB14,
        RL_L = 0xCB15,
        RL_iHL = 0xCB16,
        RL_A = 0xCB17,
        
        RR_B = 0xCB18,
        RR_C = 0xCB19,
        RR_D = 0xCB1A,
        RR_E = 0xCB1B,
        RR_H = 0xCB1C,
        RR_L = 0xCB1D,
        RR_iHL = 0xCB1E,
        RR_A = 0xCB1F,
        
        
        RLA  = 0x17,
        RRA  = 0x1F,
        
        CCF = 0x3F,
        SCF = 0x37,
        
        AND_A    = 0xA7, // clears the carry flag
        
        SBC_HL_BC = 0xED42,
        SBC_HL_DE = 0xED52,
        SBC_HL_HL = 0xED62,
        SBC_HL_SP = 0xED72,
        
        ADD_HL_BC = 0x09,
        ADD_HL_DE = 0x19,
        ADD_HL_HL = 0x29,
        ADD_HL_SP = 0x39,
        
        ADC_HL_BC = 0xED4A,
        ADC_HL_DE = 0xED5A,
        ADC_HL_HL = 0xED6A,
        ADC_HL_SP = 0xED7A,
        
        JP_nn    = 0xC3,
        JP_HL    = 0xE9,
        JR_NZ_e  = 0x20,
        JR_Z_e   = 0x28,
        JR_NC_e  = 0x30,
        JR_C_e   = 0x38,
        JR_e     = 0x18,

        DJNZ_e  = 0x10,
        
        CALL_nn = 0xCD,
        RET     = 0xC9,
        
        HALT    = 0x76,
        
        PUSH_DE = 0xD5,
        POP_DE  = 0xD1,
        PUSH_IX = 0xDDE5,
        PUSH_IY = 0xFDE5,
        POP_IX  = 0xDDE1,
        POP_IY  = 0xFDE1,
        
        
    }
    flags OperandType
    {
        None,
        Immediate8         = 0x01, //  n
        Immediate16        = 0x02, // aa | nn
        ImmediateIndirect  = 0x04, // (aa)
        Indexed            = 0x08, // (rr)
        Relative           = 0x10, // d
        IndexedRelative    = 0x20, // (XY+d)
        Implied            = 0x40,
        RelativeImmediate8 = 0x80, // (XY+d), n
    }
    
    
    byte GetOpCodeLength(byte leadByte)
    {
        switch (leadByte)
        {
            case 0xDD:
            case 0xFD:
            case 0xCB:
            case 0xED: // can be 3
            {
                return 2;
            }   
        }
        return 1;        
    }
    string GetOpCodeInfo(OpCode opCode, ref OperandType operandType, ref byte operandLength, ref bool signed)
    {
        string name;
        operandType = OperandType.None;
        if (uint(opCode) > 256)
        {
            switch (opCode)
            {
            
                case OpCode.RL_B:
                {
                    name = "RL B";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.RL_C:
                {
                    name = "RL C";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_D:
                {
                    name = "RL D";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_E:
                {
                    name = "RL E";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_H:
                {
                    name = "RL H";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_L:
                {
                    name = "RL L";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_iHL:
                {
                    name = "RL (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_A:
                {
                    name = "RL A";
                    operandType = OperandType.Implied;
                }
                case OpCode.RR_B:
                {
                    name = "RR B";
                    operandType = OperandType.Implied;
                }
                case OpCode.RR_C:
                {
                    name = "RR C";
                    operandType = OperandType.Implied;
                }
                case OpCode.RR_D:
                {
                    name = "RR D";
                    operandType = OperandType.Implied;
                }
                case OpCode.RR_E:
                {
                    name = "RR E";
                    operandType = OperandType.Implied;
                }
                case OpCode.RR_H:
                {
                    name = "RR H";
                    operandType = OperandType.Implied;
                }
                case OpCode.RR_L:
                {
                    name = "RR L";
                    operandType = OperandType.Implied;
                }
                case OpCode.RR_iHL:
                {
                    name = "RR (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.RR_A:
                {
                    name = "RR A";
                    operandType = OperandType.Implied;
                }
                
                
                case OpCode.SLA_iHL:
                {
                    name = "SLA (HL)";
                    operandType = OperandType.Implied;
                }    
                case OpCode.SRL_iHL:
                {
                    name = "SRL (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.SRL_B:
                {
                    name = "SRL B";
                    operandType = OperandType.Implied;
                }
                case OpCode.SRL_C:
                {
                    name = "SRL C";
                    operandType = OperandType.Implied;
                }
                case OpCode.SRL_D:
                {
                    name = "SRL D";
                    operandType = OperandType.Implied;
                }
                case OpCode.SRL_E:
                {
                    name = "SRL E";
                    operandType = OperandType.Implied;
                }
                case OpCode.SRL_H:
                {
                    name = "SRL H";
                    operandType = OperandType.Implied;
                }
                case OpCode.SRL_L:
                {
                    name = "SRL L";
                    operandType = OperandType.Implied;
                }
                case OpCode.SRL_A:
                {
                    name = "SRL A";
                    operandType = OperandType.Implied;
                }

                case OpCode.LD_D_IYH:
                {
                    name = "LD D, IYh";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_E_IYL:
                {
                    name = "LD E, IYl";
                    operandType = OperandType.Implied;
                }
                case OpCode.DEC_iIX_d:
                {
                    name = "DEC (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.DEC_iIY_d:
                {
                    name = "DEC (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                
                case OpCode.PUSH_IX:
                {
                    name = "PUSH IX";
                    operandType = OperandType.Implied;
                }
                case OpCode.POP_IX:
                {
                    name = "POP IX";
                    operandType = OperandType.Implied;
                }
                case OpCode.PUSH_IY:
                {
                    name = "PUSH IY";
                    operandType = OperandType.Implied;
                }
                case OpCode.POP_IY:
                {
                    name = "POP IY";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.SBC_HL_BC:
                {
                    name = "SBC HL, BC";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_HL_DE:
                {
                    name = "SBC HL, DE";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_HL_HL:
                {
                    name = "SBC HL, HL";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_HL_SP:
                {
                    name = "SBC HL, SP";
                    operandType = OperandType.Implied;
                }
                        
                case OpCode.LD_IX_nn:
                {
                    name = "LD IX, nn";
                    operandType = OperandType.Immediate16;
                }
                case OpCode.LD_IY_nn:
                {
                    name = "LD IY, nn";
                    operandType = OperandType.Immediate16;
                }
                case OpCode.LD_A_iIY_d:
                {
                    name = "LD A, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_B_iIY_d:
                {
                    name = "LD B, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_C_iIY_d:
                {
                    name = "LD C, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_D_iIY_d:
                {
                    name = "LD D, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_E_iIY_d:
                {
                    name = "LD E, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_H_iIY_d:
                {
                    name = "LD H, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_L_iIY_d:
                {
                    name = "LD L, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                  
                case OpCode.LD_A_iIY_d:
                {
                    name = "LD A, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_B_iIY_d:
                {
                    name = "LD B, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_C_iIY_d:
                {
                    name = "LD C, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_D_iIY_d:
                {
                    name = "LD D, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_E_iIY_d:
                {
                    name = "LD E, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_H_iIY_d:
                {
                    name = "LD H, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_L_iIY_d:
                {
                    name = "LD L, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                
                case OpCode.ADC_HL_BC:
                {
                    name = "ADC HL, BC";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADC_HL_DE:
                {
                    name = "ADC HL, DE";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADC_HL_HL:
                {
                    name = "ADC HL, HL";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADC_HL_SP:
                {
                    name = "ADC HL, SP";
                    operandType = OperandType.Implied;
                }
                         
                            
                case OpCode.OR_A_iIX_d:
                {
                    name = "OR A, (IX+d)";
                    operandType = OperandType.Implied;
                }
                case OpCode.OR_A_iIY_d:
                {
                    name = "OR A, (IY+d)";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_A_iIX_d:
                {
                    name = "LD A, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_B_iIX_d:
                {
                    name = "LD B, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_C_iIX_d:
                {
                    name = "LD C, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_D_iIX_d:
                {
                    name = "LD D, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_E_iIX_d:
                {
                    name = "LD E, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_H_iIX_d:
                {
                    name = "LD H, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_L_iIX_d:
                {
                    name = "LD L, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                
                
                case OpCode.LD_iIX_d_A:
                {
                    name = "LD (IX+d), A";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIX_d_B:
                {
                    name = "LD (IX+d), B";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIX_d_C:
                {
                    name = "LD (IX+d), C";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIX_d_D:
                {
                    name = "LD (IX+d), D";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIX_d_E:
                {
                    name = "LD (IX+d), E";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIX_d_H:
                {
                    name = "LD (IX+d), H";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIX_d_L:
                {
                    name = "LD (IX+d), L";
                    operandType = OperandType.IndexedRelative;
                }
                
                case OpCode.SUB_A_iIX_d:
                {
                    name = "SUB A, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.SUB_A_iIY_d:
                {
                    name = "SUB A, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.INC_IX:
                {
                    name = "INC IX";
                    operandType = OperandType.Implied;
                }
                case OpCode.INC_IY:
                {
                    name = "INC IY";
                    operandType = OperandType.Implied;
                }
                case OpCode.DEC_IX:
                {
                    name = "DEC IX";
                    operandType = OperandType.Implied;
                }
                case OpCode.DEC_IY:
                {
                    name = "DEC IY";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.ADD_IX_BC:
                {
                    name = "ADD IX, BC";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_IX_DE:
                {
                    name = "ADD IX, DE";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_IY_BC:
                {
                    name = "ADD IY, BC";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_IY_DE:
                {
                    name = "ADD IY, DE";
                    operandType = OperandType.Implied;
                }
                case OpCode.AND_A_iIX_d:
                {
                    name = "AND A, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                    
                }
                case OpCode.AND_A_iIY_d:
                {
                    name = "AND A, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                    
                }
                case OpCode.SBC_A_iIX_d:
                {
                    name = "SBC A, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.SBC_A_iIY_d:
                {
                    name = "SBC A, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.ADD_A_iIX_d:
                {
                    name = "ADD A, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.ADD_A_iIY_d:
                {
                    name = "ADD A, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.ADC_A_iIX_d:
                {
                    name = "ADC A, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.ADC_A_iIY_d:
                {
                    name = "ADC A, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                }
                
                case OpCode.LD_iIY_d_A:
                {
                    name = "LD (IY+d), A";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIY_d_B:
                {
                    name = "LD (IY+d), B";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIY_d_C:
                {
                    name = "LD (IY+d), C";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIY_d_D:
                {
                    name = "LD (IY+d), D";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIY_d_E:
                {
                    name = "LD (IY+d), E";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIY_d_H:
                {
                    name = "LD (IY+d), H";
                    operandType = OperandType.IndexedRelative;
                }
                case OpCode.LD_iIY_d_L:
                {
                    name = "LD (IY+d), L";
                    operandType = OperandType.IndexedRelative;
                }
                
                case OpCode.LD_iIX_d_n:
                {
                    name = "LD (IY+d), n";
                    operandType = OperandType.RelativeImmediate8;
                }
                case OpCode.LD_iIY_d_n:
                {
                    name = "LD (IY+d), n";
                    operandType = OperandType.RelativeImmediate8;
                }
                case OpCode.OR_A_iIX_d:
                {
                    name = "OR A, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                    
                }
                 case OpCode.XOR_A_iIX_d:
                {
                    name = "XOR A, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                    
                }
                case OpCode.XOR_A_iIY_d:
                {
                    name = "XOR A, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                    
                }
                case OpCode.CP_A_iIX_d:
                {
                    name = "CP A, (IX+d)";
                    operandType = OperandType.IndexedRelative;
                    
                }
                case OpCode.CP_A_iIY_d:
                {
                    name = "CP A, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                    
                }
                case OpCode.OR_A_iIY_d:
                {
                    name = "OR A, (IY+d)";
                    operandType = OperandType.IndexedRelative;
                    
                }
                case OpCode.INC_iIX_d:
                {
                    name = "INC (IX+d)";
                    operandType = OperandType.Relative;
                }
                case OpCode.INC_iIY_d:
                {
                    name = "INC (IY+d)";
                    operandType = OperandType.Relative;
                }
                case OpCode.RL_A:
                {
                    name = "RL A";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_B:
                {
                    name = "RL B";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_C:
                {
                    name = "RL C";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_D:
                {
                    name = "RL D";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_E:
                {
                    name = "RL E";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_H:
                {
                    name = "RL H";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_L:
                {
                    name = "RL L";
                    operandType = OperandType.Implied;
                }
                case OpCode.RL_iHL:
                {
                    name = "RL (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.SLA_B:
                {
                    name = "SLA_B";
                    operandType = OperandType.Implied;
                }
                case OpCode.SLA_C:
                {
                    name = "SLA_C";
                    operandType = OperandType.Implied;
                }
                case OpCode.SLA_D:
                {
                    name = "SLA_D";
                    operandType = OperandType.Implied;
                }
                case OpCode.SLA_E:
                {
                    name = "SLA_E";
                    operandType = OperandType.Implied;
                }
                case OpCode.SLA_H:
                {
                    name = "SLA_H";
                    operandType = OperandType.Implied;
                }
                case OpCode.SLA_L:
                {
                    name = "SLA_L";
                    operandType = OperandType.Implied;
                }
                case OpCode.SLA_A:
                {
                    name = "SLA_A";
                    operandType = OperandType.Implied;
                }
                
                default:
                {
                    Print("OpCode.GetName(0x" + (uint(opCode)).ToHexString(4) +") not implemented (long)"); Die(0x0A);
                }
            }
        }
        else
        {
            switch (opCode)
            {
                case OpCode.DEC_iHL:
                {
                    name = "DEC (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.JP_HL:
                {
                    name = "JP HL";
                    operandType = OperandType.Implied;
                }
                                            
                case OpCode.ADD_HL_BC:
                {
                    name = "ADD HL, BC";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_HL_DE:
                {
                    name = "ADD HL, DE";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_HL_HL:
                {
                    name = "ADD HL, HL";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_HL_SP:
                {
                    name = "ADD HL, SP";
                    operandType = OperandType.Implied;
                }
                                                      
                case OpCode.OR_A_iHL:
                {
                    name = "OR A, (HL)";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.LD_B_B:
                {
                    name = "LD B, B";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_C_B:
                {
                    name = "LD C, B";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_D_B:
                {
                    name = "LD D, B";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_E_B:
                {
                    name = "LD E, B";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_H_B:
                {
                    name = "LD H, B";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_L_B:
                {
                    name = "LD L, B";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_A_B:
                {
                    name = "LD A, B";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_B_C:
                {
                    name = "LD B, C";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_C_C:
                {
                    name = "LD C, C";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_D_C:
                {
                    name = "LD D, C";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_E_C:
                {
                    name = "LD E, C";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_H_C:
                {
                    name = "LD H, C";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_L_C:
                {
                    name = "LD L, C";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_A_C:
                {
                    name = "LD A, C";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_B_D:
                {
                    name = "LD B, D";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_C_D:
                {
                    name = "LD C, D";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_D_D:
                {
                    name = "LD D, D";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_E_D:
                {
                    name = "LD E, D";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_H_D:
                {
                    name = "LD H, D";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_L_D:
                {
                    name = "LD L, D";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_A_D:
                {
                    name = "LD A, D";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_B_E:
                {
                    name = "LD B, E";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_C_E:
                {
                    name = "LD C, E";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_D_E:
                {
                    name = "LD D, E";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_E_E:
                {
                    name = "LD E, E";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_H_E:
                {
                    name = "LD H, E";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_L_E:
                {
                    name = "LD L, E";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_A_E:
                {
                    name = "LD A, E";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_B_H:
                {
                    name = "LD B, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_C_H:
                {
                    name = "LD C, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_D_H:
                {
                    name = "LD D, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_E_H:
                {
                    name = "LD E, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_H_H:
                {
                    name = "LD H, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_L_H:
                {
                    name = "LD L, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_A_H:
                {
                    name = "LD A, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_B_L:
                {
                    name = "LD B, L";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_C_L:
                {
                    name = "LD C, L";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_D_L:
                {
                    name = "LD D, L";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_E_L:
                {
                    name = "LD E, L";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_H_L:
                {
                    name = "LD H, L";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_L_L:
                {
                    name = "LD L, L";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_A_L:
                {
                    name = "LD A, L";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_B_A:
                {
                    name = "LD B, A";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_C_A:
                {
                    name = "LD C, A";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_D_A:
                {
                    name = "LD D, A";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_E_A:
                {
                    name = "LD E, A";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_H_A:
                {
                    name = "LD H, A";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_L_A:
                {
                    name = "LD L, A";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_A_A:
                {
                    name = "LD A, A";
                    operandType = OperandType.Implied;
                }

            
                case OpCode.XOR_A_B:
                {
                    name = "XOR A, B";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.XOR_A_C:
                {
                    name = "XOR A, C";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.XOR_A_D:
                {
                    name = "XOR A, D";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.XOR_A_E:
                {
                    name = "XOR A, E";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.XOR_A_H:
                {
                    name = "XOR A, H";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.XOR_A_L:
                {
                    name = "XOR A, L";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.XOR_A_A:
                {
                    name = "XOR A, A";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.XOR_A_iHL:
                {
                    name = "XOR A, (HL)";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.CP_A_B:
                {
                    name = "CP A, B";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.CP_A_C:
                {
                    name = "CP A, C";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.CP_A_D:
                {
                    name = "CP A, D";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.CP_A_E:
                {
                    name = "CP A, E";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.CP_A_H:
                {
                    name = "CP A, H";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.CP_A_L:
                {
                    name = "CP A, L";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.CP_A_A:
                {
                    name = "CP A, A";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.CP_A_iHL:
                {
                    name = "CP A, (HL)";
                    operandType = OperandType.Implied;
                    
                }
               case OpCode.OR_A_B:
                {
                    name = "OR A, B";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.OR_A_C:
                {
                    name = "OR A, C";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.OR_A_D:
                {
                    name = "OR A, D";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.OR_A_E:
                {
                    name = "OR A, E";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.OR_A_H:
                {
                    name = "OR A, H";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.OR_A_L:
                {
                    name = "OR A, L";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.OR_A_A:
                {
                    name = "OR A, A";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.AND_A_B:
                {
                    name = "AND A, B";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.AND_A_C:
                {
                    name = "AND A, C";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.AND_A_D:
                {
                    name = "AND A, D";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.AND_A_E:
                {
                    name = "AND A, E";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.AND_A_H:
                {
                    name = "AND A, H";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.AND_A_L:
                {
                    name = "AND A, L";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.AND_A_A:
                {
                    name = "AND A, A";
                    operandType = OperandType.Implied;
                    
                }
                case OpCode.AND_A_iHL:
                {
                    name = "AND A, (HL)";
                    operandType = OperandType.Implied;
                    
                }
                
                            
                
                case OpCode.SUB_A_B:
                {
                    name = "SUB A, B";
                    operandType = OperandType.Implied;
                }
                case OpCode.SUB_A_C:
                {
                    name = "SUB A, C";
                    operandType = OperandType.Implied;
                }
                case OpCode.SUB_A_D:
                {
                    name = "SUB A, D";
                    operandType = OperandType.Implied;
                }
                case OpCode.SUB_A_E:
                {
                    name = "SUB A, E";
                    operandType = OperandType.Implied;
                }
                case OpCode.SUB_A_H:
                {
                    name = "SUB A, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.SUB_A_L:
                {
                    name = "SUB A, L";
                    operandType = OperandType.Implied;
                }
                case OpCode.SUB_A_A:
                {
                    name = "SUB A, A";
                    operandType = OperandType.Implied;
                }
                case OpCode.SUB_A_iHL:
                {
                    name = "SUB A, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.SUB_A_n:
                {
                    name = "SUB A, n";
                    operandType = OperandType.Immediate8;
                }
                case OpCode.SBC_A_B:
                {
                    name = "SBC A, B";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_A_C:
                {
                    name = "SBC A, C";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_A_D:
                {
                    name = "SBC A, D";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_A_E:
                {
                    name = "SBC A, E";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_A_H:
                {
                    name = "SBC A, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_A_L:
                {
                    name = "SBC A, L";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_A_A:
                {
                    name = "SBC A, A";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_A_iHL:
                {
                    name = "SBC A, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_A_n:
                {
                    name = "SBC A, n";
                    operandType = OperandType.Immediate8;
                }
                case OpCode.ADD_A_B:
                {
                    name = "ADD A, B";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_A_C:
                {
                    name = "ADD A, C";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_A_D:
                {
                    name = "ADD A, D";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_A_E:
                {
                    name = "ADD A, E";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_A_H:
                {
                    name = "ADD A, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_A_L:
                {
                    name = "ADD A, L";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_A_A:
                {
                    name = "ADD A, A";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_A_iHL:
                {
                    name = "ADD A, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADD_A_n:
                {
                    name = "ADD A, n";
                    operandType = OperandType.Immediate8;
                }
                            
                case OpCode.ADC_A_B:
                {
                    name = "ADC A, B";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADC_A_C:
                {
                    name = "ADC A, C";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADC_A_D:
                {
                    name = "ADC A, D";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADC_A_E:
                {
                    name = "ADC A, E";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADC_A_H:
                {
                    name = "ADC A, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADC_A_L:
                {
                    name = "ADC A, L";
                    operandType = OperandType.Implied;
                }
                case OpCode.ADC_A_A:
                {
                    name = "ADC A, A";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.ADC_A_n:
                {
                    name = "ADC A, n";
                    operandType = OperandType.Immediate8;
                }
                case OpCode.ADC_A_iHL:
                {
                    name = "ADC A, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.DJNZ_e:
                {
                    name = "DJNZ, e";
                    operandType = OperandType.Relative;
                }
                
                case OpCode.RLC_B:
                {
                    name = "RLC_B";
                    operandType = OperandType.Implied;
                }
                case OpCode.RLC_C:
                {
                    name = "RLC_C";
                    operandType = OperandType.Implied;
                }
                case OpCode.RLC_D:
                {
                    name = "RLC_D";
                    operandType = OperandType.Implied;
                }
                case OpCode.RLC_E:
                {
                    name = "RLC_E";
                    operandType = OperandType.Implied;
                }
                case OpCode.RLC_H:
                {
                    name = "RLC_H";
                    operandType = OperandType.Implied;
                }
                case OpCode.RLC_L:
                {
                    name = "RLC_L";
                    operandType = OperandType.Implied;
                }
                case OpCode.RLC_A:
                {
                    name = "RLC_A";
                    operandType = OperandType.Implied;
                }
                
                
                            
                case OpCode.CP_A_n:
                {
                    name = "CP A, n";
                    operandType = OperandType.Immediate8;
                }
                
                case OpCode.NOP:
                {
                    name = "NOP";
                    operandType = OperandType.Implied;
                }
                case OpCode.INC_HL:
                {
                    name = "INC HL";
                    operandType = OperandType.Implied;
                }
                case OpCode.INC_DE:
                {
                    name = "INC DE";
                    operandType = OperandType.Implied;
                }
                case OpCode.INC_BC:
                {
                    name = "INC BC";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.DEC_BC:
                {
                    name = "DEC BC";
                    operandType = OperandType.Implied;
                }
                case OpCode.DEC_HL:
                {
                    name = "DEC HL";
                    operandType = OperandType.Implied;
                }
                
                
                case OpCode.ADD_A_iHL:
                {
                    name = "ADD A, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.SUB_A_iHL:
                {
                    name = "SUB A, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.SBC_A_iHL:
                {
                    name = "SBC A, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.AND_A_iHL:
                {
                    name = "AND A, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.XOR_A_iHL:
                {
                    name = "XOR A, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.OR_A_iHL:
                {
                    name = "OR A, (HL)";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.CP_A_iHL:
                {
                    name = "CP A, (HL)";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.XOR_A:
                {
                    name = "XOR A";
                    operandType = OperandType.Implied;
                }
                case OpCode.XOR_B:
                {
                    name = "XOR B";
                    operandType = OperandType.Implied;
                }
                case OpCode.XOR_C:
                {
                    name = "XOR C";
                    operandType = OperandType.Implied;
                }
                case OpCode.XOR_D:
                {
                    name = "XOR D";
                    operandType = OperandType.Implied;
                }
                case OpCode.XOR_E:
                {
                    name = "XOR E";
                    operandType = OperandType.Implied;
                }
                case OpCode.XOR_H:
                {
                    name = "XOR H";
                    operandType = OperandType.Implied;
                }
                case OpCode.XOR_L:
                {
                    name = "XOR L";
                    operandType = OperandType.Implied;
                }
                
                
                case OpCode.AND_A:
                {
                    name = "AND A";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.SBC_HL_BC:
                {
                    name = "SBC HL, BC";
                    operandType = OperandType.Implied;
                }

                case OpCode.JP_nn:
                {
                    name = "JP nn";
                    operandType = OperandType.Immediate16;
                }
                case OpCode.JR_Z_e:
                {
                    name = "JR Z e";
                    operandType = OperandType.Relative;
                }
                case OpCode.JR_NZ_e:
                {
                    name = "JR NZ e";
                    operandType = OperandType.Relative;
                }
                case OpCode.JR_C_e:
                {
                    name = "JR C e";
                    operandType = OperandType.Relative;
                }
                case OpCode.JR_NC_e:
                {
                    name = "JR NC e";
                    operandType = OperandType.Relative;
                }
                case OpCode.JR_e:
                {
                    name = "JR e";
                    operandType = OperandType.Relative;
                }
                
                case OpCode.RST_Reset:
                {
                    name = "RST 0x00";
                    operandType = OperandType.Implied;
                }
                case OpCode.RST_PopAbsolute:
                {
                    name = "RST 0x08";
                    operandType = OperandType.Implied;
                }
                case OpCode.RST_PushAbsolute:
                {
                    name = "RST 0x10";
                    operandType = OperandType.Implied;
                }
                case OpCode.RST_PushImmediate:
                {
                    name = "RST 0x18";
                    operandType = OperandType.Implied;
                }
                case OpCode.RST_PushOffset:
                {
                    name = "RST 0x20";
                    operandType = OperandType.Implied;
                }
                case OpCode.RST_PopOffset:
                {
                    name = "RST 0x28";
                    operandType = OperandType.Implied;
                }
                case OpCode.RST_SysCall0:
                {
                    name = "RST 0x30";
                    operandType = OperandType.Implied;
                }
                case OpCode.RST_Instruction:
                {
                    name = "RST 0x38";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.LD_A_n:
                {
                    name = "LD A, n";
                    operandType = OperandType.Immediate8;
                }
                case OpCode.LD_B_n:
                {
                    name = "LD B, n";
                    operandType = OperandType.Immediate8;
                }
                case OpCode.LD_C_n:
                {
                    name = "LD C, n";
                    operandType = OperandType.Immediate8;
                }
                case OpCode.LD_D_n:
                {
                    name = "LD D, n";
                    operandType = OperandType.Immediate8;
                }
                case OpCode.LD_E_n:
                {
                    name = "LD E, n";
                    operandType = OperandType.Immediate8;
                }
                case OpCode.LD_H_n:
                {
                    name = "LD H, n";
                    operandType = OperandType.Immediate8;
                }
                case OpCode.LD_L_n:
                {
                    name = "LD L, n";
                    operandType = OperandType.Immediate8;
                }
                
                case OpCode.LD_iHL_B:
                {
                    name = "LD (HL), B";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_iHL_C:
                {
                    name = "LD (HL), C";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_iHL_D:
                {
                    name = "LD (HL), D";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_iHL_E:
                {
                    name = "LD (HL), E";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_iHL_H:
                {
                    name = "LD (HL), H";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_iHL_L:
                {
                    name = "LD (HL), L";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_iHL_A:
                {
                    name = "LD (HL), A";
                    operandType = OperandType.Implied;
                }
                
                
                
                case OpCode.LD_A_iHL:
                {
                    name = "LD A, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_B_iHL:
                {
                    name = "LD B, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_C_iHL:
                {
                    name = "LD C, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_D_iHL:
                {
                    name = "LD D, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_E_iHL:
                {
                    name = "LD E, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_H_iHL:
                {
                    name = "LD H, (HL)";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_L_iHL:
                {
                    name = "LD L, (HL)";
                    operandType = OperandType.Implied;
                }

                case OpCode.LD_BC_nn:
                {
                    name = "LD BC, nn";
                    operandType = OperandType.Immediate16;
                }
                case OpCode.LD_DE_nn:
                {
                    name = "LD DE, nn";
                    operandType = OperandType.Immediate16;
                }
                case OpCode.LD_HL_nn:
                {
                    name = "LD HL, nn";
                    operandType = OperandType.Immediate16;
                }
                case OpCode.LD_SP_nn:
                {
                    name = "LD SP, nn";
                    operandType = OperandType.Immediate16;
                }
                
                case OpCode.LD_C_B:
                {
                    name = "LD C, B";
                    operandType = OperandType.Implied;
                }

                case OpCode.LD_D_H:
                {
                    name = "LD D, H";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_E_L:
                {
                    name = "LD E, L";
                    operandType = OperandType.Implied;
                }

                case OpCode.LD_SP_HL:
                {
                    name = "LD SP, HL";
                    operandType = OperandType.Implied;
                }
                case OpCode.LD_iHL_n:
                {
                    name = "LD (HL), n";
                    operandType = OperandType.Immediate8;
                }
                
                case OpCode.LD_iHL_B:
                {
                    name = "LD (HL), B";
                    operandType = OperandType.Indexed;
                }
                case OpCode.LD_iHL_C:
                {
                    name = "LD (HL), C";
                    operandType = OperandType.Indexed;
                }
                case OpCode.LD_iHL_D:
                {
                    name = "LD (HL), D";
                    operandType = OperandType.Indexed;
                }
                case OpCode.LD_iHL_E:
                {
                    name = "LD (HL), E";
                    operandType = OperandType.Indexed;
                }
                case OpCode.LD_iHL_H:
                {
                    name = "LD (HL), H";
                    operandType = OperandType.Indexed;
                }
                case OpCode.LD_iHL_L:
                {
                    name = "LD (HL), L";
                    operandType = OperandType.Indexed;
                }
                case OpCode.LD_iHL_A:
                {
                    name = "LD (HL), A";
                    operandType = OperandType.Indexed;
                }
                
                
                case OpCode.LD_SP_HL:
                {
                    name = "LD SP,HL";
                    operandType = OperandType.Implied;
                }
                case OpCode.PUSH_DE:
                {
                    name = "PUSH DE";
                    operandType = OperandType.Implied;
                }
                case OpCode.POP_DE:
                {
                    name = "POP DE";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.RET:
                {
                    name = "RET";
                    operandType = OperandType.Implied;
                }
                case OpCode.HALT:
                {
                    name = "HALT";
                    operandType = OperandType.Implied;
                }
                case OpCode.CALL_nn:
                {
                    name = "CALL nn";
                    operandType = OperandType.Immediate16;
                }
                                
                case OpCode.INC_A:
                {
                    name = "INC A";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.INC_B:
                {
                    name = "INC B";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.INC_C:
                {
                    name = "INC C";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.INC_D:
                {
                    name = "INC D";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.INC_E:
                {
                    name = "INC E";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.INC_H:
                {
                    name = "INC H";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.INC_L:
                {
                    name = "INC L";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.DEC_A:
                {
                    name = "DEC A";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.DEC_B:
                {
                    name = "DEC B";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.DEC_C:
                {
                    name = "DEC C";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.DEC_D:
                {
                    name = "DEC D";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.DEC_E:
                {
                    name = "DEC E";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.DEC_H:
                {
                    name = "DEC H";
                    operandType = OperandType.Implied;
                }
                
                case OpCode.DEC_L:
                {
                    name = "DEC L";
                    operandType = OperandType.Implied;
                }
                case OpCode.RLA:
                {
                    name = "RLA";
                    operandType = OperandType.Implied;
                }
                case OpCode.RRA:
                {
                    name = "RRA";
                    operandType = OperandType.Implied;
                }
                case OpCode.CLP:
                {
                    name = "CLP";
                    operandType = OperandType.Implied;
                }
                case OpCode.CCF:
                {
                    name = "CCF";
                    operandType = OperandType.Implied;
                }
                case OpCode.SCF:
                {
                    name = "SCF";
                    operandType = OperandType.Implied;
                }
                
                default:
                {
                    Print("OpCode.GetName(0x" + (uint(opCode)).ToHexString(4) +") not implemented (short)"); Die(0x0A);
                }
            }
        }
        if (operandType == OperandType.None)
        {
            Print("OpCode.GetName(0x" + (uint(opCode)).ToHexString(4) +") not implemented (operandType)"); Die(0x0A);
        }
        signed = false;
        switch (operandType)
        {
            case OperandType.Relative:
            case OperandType.IndexedRelative:
            {
                operandLength = 1;
                signed = true;
            }
            case OperandType.Immediate8:
            {
                operandLength = 1;
            }
            case OperandType.Immediate16:
            case OperandType.ImmediateIndirect:
            {
                operandLength = 2;
            }
            
            case OperandType.Indexed:
            case OperandType.Implied   :
            {
                operandLength = 0;
            }
            case OperandType.RelativeImmediate8:
            {
                operandLength = 2;
            }
        }
        return name;
    }
    string Disassemble(uint address, OpCode instruction, uint operand, bool bare)
    {
        OperandType operandType;
        byte operandLength;
        bool signed;
        byte opCodeLength = GetOpCodeLength(byte(uint(instruction) >> 8));
        string name = GetOpCodeInfo(instruction, ref operandType, ref operandLength, ref signed);
        
        string disassembly = "0x" + address.ToHexString(4);
        if (opCodeLength == 2)
        {
            disassembly += (" 0x" + (uint(instruction) >> 8).ToHexString(2));
        }
        disassembly += (" 0x" + (uint(instruction) & 0xFF).ToHexString(2) + " ");
        if (opCodeLength == 1)
        {
            disassembly += "     ";
        }
        
        switch (operandLength)
        {
            case 0:
            {
                disassembly += "          ";
            }
            case 2:
            {
                disassembly += (" 0x" + (operand & 0xFF).ToHexString(2));
                disassembly += (" 0x" + (operand >> 8).ToHexString(2));
            }
            case 1:
            {
                disassembly += (" 0x" + (operand & 0xFF).ToHexString(2));
                disassembly += ("     ");
            }
        }
        disassembly += ("  " + name);
        switch (operandType)
        {
            // signed
            
            case OperandType.Immediate8: // n
            {
                disassembly = disassembly.Replace("n", "0x" + operand.ToHexString(2));
            }
            case OperandType.Immediate16:
            case OperandType.ImmediateIndirect:
            {
                disassembly = disassembly.Replace("nn", "0x" + operand.ToHexString(4));
            }
            case OperandType.Relative:
            case OperandType.IndexedRelative:
            {
                int offset = int(operand);
                if (offset > 127) { offset -= 256; }
                string sign = "";
                if (offset >= 0)
                {
                    sign = "+";
                }
                bool jumpAddress = disassembly.Contains('e');
                disassembly = disassembly.Replace("+d", "d");
                disassembly = disassembly.Replace('e', 'd');
                disassembly = disassembly.Replace("d", sign + offset.ToString());
                if (jumpAddress && !bare)
                {
                    long targetAddress = long(address) + offset + operandLength + opCodeLength;
                    disassembly = disassembly + "  -> 0x" + targetAddress.ToHexString(4);  
                }
            }
            case OperandType.RelativeImmediate8:
            {
                disassembly = disassembly.Replace("n", "0x" + (operand >> 8).ToHexString(2));
                int offset = int(operand & 0xFF);
                if (offset > 127) { offset -= 256; }
                string sign = "";
                if (offset >= 0)
                {
                    sign = "+";
                }
                disassembly = disassembly.Replace("+d", "d");
                disassembly = disassembly.Replace("d", sign + offset.ToString());
            }
        }
        return disassembly;
    }
    
}
