unit AsmZ80
{
    const uint StackAddress  = 0xFC00; // 512 bytes
    const uint StackSize     = 0x0200;
    
    // global addresses
    const uint SPBPSwapper       = 0xFE00; // 2 bytes
    const uint LastError         = 0xFE02; // 1 bytes
    const uint Sign              = 0xFE03; // 1 bytes
        
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
        
        BIT_0_A = 0xCB47,
        BIT_0_B = 0xCB40,
        BIT_0_C = 0xCB41,
        BIT_0_D = 0xCB42,
        BIT_0_E = 0xCB43,
        BIT_0_H = 0xCB44,
        BIT_0_L = 0xCB45,
        BIT_0_iHL = 0xCB46,
        
        BIT_1_B = 0xCB48,
        BIT_1_C = 0xCB49,
        BIT_1_D = 0xCB4A,
        BIT_1_E = 0xCB4B,
        BIT_1_H = 0xCB4C,
        BIT_1_L = 0xCB4D,
        BIT_1_iHL = 0xCB4E,
        BIT_1_A = 0xCB4F,
        
        BIT_2_B = 0xCB50,
        BIT_2_C = 0xCB51,
        BIT_2_D = 0xCB52,
        BIT_2_E = 0xCB53,
        BIT_2_H = 0xCB54,
        BIT_2_L = 0xCB55,
        BIT_2_iHL = 0xCB56,
        BIT_2_A = 0xCB57,
        
        BIT_3_B = 0xCB58,
        BIT_3_C = 0xCB59,
        BIT_3_D = 0xCB5A,
        BIT_3_E = 0xCB5B,
        BIT_3_H = 0xCB5C,
        BIT_3_L = 0xCB5D,
        BIT_3_iHL = 0xCB5E,
        BIT_3_A = 0xCB5F,
        
        BIT_4_B = 0xCB60,
        BIT_4_C = 0xCB61,
        BIT_4_D = 0xCB62,
        BIT_4_E = 0xCB63,
        BIT_4_H = 0xCB64,
        BIT_4_L = 0xCB65,
        BIT_4_iHL = 0xCB66,
        BIT_4_A = 0xCB67,
        
        BIT_5_B = 0xCB68,
        BIT_5_C = 0xCB69,
        BIT_5_D = 0xCB6A,
        BIT_5_E = 0xCB6B,
        BIT_5_H = 0xCB6C,
        BIT_5_L = 0xCB6D,
        BIT_5_iHL = 0xCB6E,
        BIT_5_A = 0xCB6F,
        
        BIT_6_B = 0xCB70,
        BIT_6_C = 0xCB71,
        BIT_6_D = 0xCB72,
        BIT_6_E = 0xCB73,
        BIT_6_H = 0xCB74,
        BIT_6_L = 0xCB75,
        BIT_6_iHL = 0xCB76,
        BIT_6_A = 0xCB77,
        
        BIT_7_B = 0xCB78,
        BIT_7_C = 0xCB79,
        BIT_7_D = 0xCB7A,
        BIT_7_E = 0xCB7B,
        BIT_7_H = 0xCB7C,
        BIT_7_L = 0xCB7D,
        BIT_7_iHL = 0xCB7E,
        BIT_7_A = 0xCB7F,
        
        RES_0_B = 0xCB80,
        RES_0_C = 0xCB81,
        RES_0_D = 0xCB82,
        RES_0_E = 0xCB83,
        RES_0_H = 0xCB84,
        RES_0_L = 0xCB85,
        RES_0_iHL = 0xCB86,
        RES_0_A = 0xCB87,
        
        RES_1_B = 0xCB88,
        RES_1_C = 0xCB89,
        RES_1_D = 0xCB8A,
        RES_1_E = 0xCB8B,
        RES_1_H = 0xCB8C,
        RES_1_L = 0xCB8D,
        RES_1_iHL = 0xCB8E,
        RES_1_A = 0xCB8F,
        
        RES_2_B = 0xCB90,
        RES_2_C = 0xCB91,
        RES_2_D = 0xCB92,
        RES_2_E = 0xCB93,
        RES_2_H = 0xCB94,
        RES_2_L = 0xCB95,
        RES_2_iHL = 0xCB96,
        RES_2_A = 0xCB97,
        
        RES_3_B = 0xCB98,
        RES_3_C = 0xCB99,
        RES_3_D = 0xCB9A,
        RES_3_E = 0xCB9B,
        RES_3_H = 0xCB9C,
        RES_3_L = 0xCB9D,
        RES_3_iHL = 0xCB9E,
        RES_3_A = 0xCB9F,
        
        RES_4_B = 0xCBA0,
        RES_4_C = 0xCBA1,
        RES_4_D = 0xCBA2,
        RES_4_E = 0xCBA3,
        RES_4_H = 0xCBA4,
        RES_4_L = 0xCBA5,
        RES_4_iHL = 0xCBA6,
        RES_4_A = 0xCBA7,
        
        RES_5_B = 0xCBA8,
        RES_5_C = 0xCBA9,
        RES_5_D = 0xCBAA,
        RES_5_E = 0xCBAB,
        RES_5_H = 0xCBAC,
        RES_5_L = 0xCBAD,
        RES_5_iHL = 0xCBAE,
        RES_5_A = 0xCBAF,
        
        RES_6_B = 0xCBB0,
        RES_6_C = 0xCBB1,
        RES_6_D = 0xCBB2,
        RES_6_E = 0xCBB3,
        RES_6_H = 0xCBB4,
        RES_6_L = 0xCBB5,
        RES_6_iHL = 0xCBB6,
        RES_6_A = 0xCBB7,
        
        RES_7_B = 0xCBB8,
        RES_7_C = 0xCBB9,
        RES_7_D = 0xCBBA,
        RES_7_E = 0xCBBB,
        RES_7_H = 0xCBBC,
        RES_7_L = 0xCBBD,
        RES_7_iHL = 0xCBBE,
        RES_7_A = 0xCBBF,
        
        SET_0_B = 0xCBC0,
        SET_0_C = 0xCBC1,
        SET_0_D = 0xCBC2,
        SET_0_E = 0xCBC3,
        SET_0_H = 0xCBC4,
        SET_0_L = 0xCBC5,
        SET_0_iHL = 0xCBC6,
        SET_0_A = 0xCBC7,
        
        SET_1_B = 0xCBC8,
        SET_1_C = 0xCBC9,
        SET_1_D = 0xCBCA,
        SET_1_E = 0xCBCB,
        SET_1_H = 0xCBCC,
        SET_1_L = 0xCBCD,
        SET_1_iHL = 0xCBCE,
        SET_1_A = 0xCBCF,
        
        SET_2_B = 0xCBD0,
        SET_2_C = 0xCBD1,
        SET_2_D = 0xCBD2,
        SET_2_E = 0xCBD3,
        SET_2_H = 0xCBD4,
        SET_2_L = 0xCBD5,
        SET_2_iHL = 0xCBD6,
        SET_2_A = 0xCBD7,
        
        SET_3_B = 0xCBD8,
        SET_3_C = 0xCBD9,
        SET_3_D = 0xCBDA,
        SET_3_E = 0xCBDB,
        SET_3_H = 0xCBDC,
        SET_3_L = 0xCBDD,
        SET_3_iHL = 0xCBDE,
        SET_3_A = 0xCBDF,
        
        SET_4_B = 0xCBE0,
        SET_4_C = 0xCBE1,
        SET_4_D = 0xCBE2,
        SET_4_E = 0xCBE3,
        SET_4_H = 0xCBE4,
        SET_4_L = 0xCBE5,
        SET_4_iHL = 0xCBE6,
        SET_4_A = 0xCBE7,
        
        SET_5_B = 0xCBE8,
        SET_5_C = 0xCBE9,
        SET_5_D = 0xCBEA,
        SET_5_E = 0xCBEB,
        SET_5_H = 0xCBEC,
        SET_5_L = 0xCBED,
        SET_5_iHL = 0xCBEE,
        SET_5_A = 0xCBEF,
        
        SET_6_B = 0xCBF0,
        SET_6_C = 0xCBF1,
        SET_6_D = 0xCBF2,
        SET_6_E = 0xCBF3,
        SET_6_H = 0xCBF4,
        SET_6_L = 0xCBF5,
        SET_6_iHL = 0xCBF6,
        SET_6_A = 0xCBF7,
        
        SET_7_B = 0xCBF8,
        SET_7_C = 0xCBF9,
        SET_7_D = 0xCBFA,
        SET_7_E = 0xCBFB,
        SET_7_H = 0xCBFC,
        SET_7_L = 0xCBFD,
        SET_7_iHL = 0xCBFE,
        SET_7_A = 0xCBFF,
        
        LD_inn_A = 0x32,
        LD_A_inn = 0x3A,
        
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
        
        LD_BC_inn = 0xED4B,
        LD_DE_inn = 0xED5B,
        LD_HL_inn = 0xED6B,
        LD_SP_inn = 0xED7B,
        LD_IX_inn = 0xDD2A,
        LD_IY_inn = 0xFD2A,
        
        LD_inn_BC = 0xED43,
        LD_inn_DE = 0xED53,
        LD_inn_HL = 0xED63,
        LD_inn_SP = 0xED73,
        LD_inn_IX = 0xDD22,
        LD_inn_IY = 0xFD22,
        
        DDDD      = 0xDDDD,
        
        LD_BC_nn = 0x01,
        LD_DE_nn = 0x11,
        LD_HL_nn = 0x21,
        LD_IX_nn = 0xDD21,
        LD_IY_nn = 0xFD21,
        LD_SP_nn = 0x31,
        
        LD_SP_HL = 0xF9,
        
        EX_DE_HL  = 0xEB,
        EX_iSP_HL = 0xE3,
        EX_iSP_IX = 0xDDE3,
        EX_iSP_IY = 0xFDE3,
        
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
        
        INC_iHL   = 0x34,
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
        
        AND_A_n = 0xE6,
        
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
        
        CPL_A_A = 0x2F,
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
        
        CPL = 0x2F,
        
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
        JP_M_nn  = 0xFA,
        JP_Z_nn = 0xCA,
        JP_NZ_nn = 0xC2,
        JP_C_nn = 0xDA,
        JP_NC_nn = 0xD2,
        JP_P_nn = 0xF2,
        JP_PE_nn = 0xEA,
        JP_PO_nn = 0xE2,
        
        
        JP_HL    = 0xE9,
        JR_NZ_e  = 0x20,
        JR_Z_e   = 0x28,
        JR_NC_e  = 0x30,
        JR_C_e   = 0x38,
        JR_e     = 0x18,

        DJNZ_e  = 0x10,
        
        CALL_nn = 0xCD,
        RET     = 0xC9,
        
        RET_NZ = 0xC0,
        RET_Z = 0xC8,
        RET_NC = 0xD0,
        RET_C = 0xD8,
        RET_PO = 0xE0,
        RET_PE = 0xE8,
        RET_P = 0xF0,
        RET_M = 0xF8,
        
        
        HALT    = 0x76,
        
        PUSH_IX = 0xDDE5,
        PUSH_IY = 0xFDE5,
        POP_IX  = 0xDDE1,
        POP_IY  = 0xFDE1,
        
        // PUSH
        PUSH_BC = 0xC5,
        PUSH_DE = 0xD5,
        PUSH_HL = 0xE5,
        PUSH_AF = 0xF5,
        
        // POP
        POP_BC = 0xC1,
        POP_DE = 0xD1,
        POP_HL = 0xE1,
        POP_AF = 0xF1
        
        
        
    }
    flags OperandType
    {
        None,
        Immediate8         = 0x01,  //  n
        Immediate16        = 0x02,  // aa | nn
        ImmediateIndirect  = 0x04,  // (aa)
        Indexed            = 0x08,  // (ss)
        Relative           = 0x10,  // d
        IndexedRelative    = 0x18,  // (XY+d)
        ImmediateIndexed   = 0x0C,  // (nn)
        Implied            = 0x40,
        RelativeImmediate8 = 0x80, // (XY+d), n
    }
    
    <OpCode, string>      z80InstructionName;
    <OpCode, OperandType> z80OperandType;
    bool initialized;

    byte GetOpCodeLength(OpCode instruction)
    {
        uint ui = uint(instruction);
        byte leadByte = ui.GetByte(1);
        if (leadByte == 0)
        {
            leadByte = ui.GetByte(0);
        }
        return GetOpCodeLength(leadByte);
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
    bool IsPush(OpCode opCode)
    {
        switch (opCode)
        {
            case OpCode.PUSH_AF:
            case OpCode.PUSH_DE:
            case OpCode.PUSH_HL:
            case OpCode.PUSH_BC:
            case OpCode.PUSH_IX:
            case OpCode.PUSH_IY:
            {
                return true;
            }
        }
        return false;
    }
    bool IsPop(OpCode opCode)
    {
        switch (opCode)
        {
            case OpCode.POP_AF:
            case OpCode.POP_DE:
            case OpCode.POP_HL:
            case OpCode.POP_BC:
            case OpCode.POP_IX:
            case OpCode.POP_IY:
            {
                return true;
            }
        }
        return false;
    }
    string GetName(OpCode opCode) 
    { 
        if (!initialized)
        {
            Initialize();
            initialized = true;
        }
        if (z80InstructionName.Contains(opCode))
        {
            return z80InstructionName[opCode]; 
        }
        return "0x" + (uint(opCode)).ToHexString(4);
    } 
    OpCode GetOpCode(<byte> code, uint index)
    {
        if (!initialized)
        {
            Initialize();
            initialized = true;
        }
        uint value = code[index];
        if ((value == 0xFD) || (value == 0xDD) || (value == 0xCB) || (value == 0xED))
        {
            value = (value << 8) + code[index+1];
        }
        return OpCode(value);
    }
    
    string GetOpCodeInfo(OpCode opCode, ref OperandType operandType, ref byte operandLength, ref bool signed)
    {
        return GetOpCodeInfo(opCode, ref operandType, ref operandLength, ref signed, true);
    }
    string GetOpCodeInfo(OpCode opCode, ref OperandType operandType, ref byte operandLength, ref bool signed, bool strict)
    {
        if (!initialized)
        {
            Initialize();
            initialized = true;
        }
        string name;
        operandType = OperandType.None;
        if (z80InstructionName.Contains(opCode))
        {
            name = z80InstructionName[opCode];
            operandType = z80OperandType[opCode];
        }
        
        if (operandType == OperandType.None)
        {
            if (!strict)
            {
                return name;
            }
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
            case OperandType.ImmediateIndexed:
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
        byte opCodeLength = GetOpCodeLength(instruction);
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
            case OperandType.ImmediateIndexed:
            {
                disassembly = disassembly.Replace("(nn)", "(0x" + operand.ToHexString(4) + ")");
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

    Initialize()
    {
        z80InstructionName[OpCode.NOP] = "NOP";                           // 0x0000
        z80OperandType    [OpCode.NOP] = OperandType.Implied;

        z80InstructionName[OpCode.CPL_A_A] = "CPL A, A";                  // 0x002F
        z80OperandType    [OpCode.CPL_A_A] = OperandType.Implied;

        z80InstructionName[OpCode.RST_Reset] = "RST Reset";               // 0x00C7
        z80OperandType    [OpCode.RST_Reset] = OperandType.Implied;

        z80InstructionName[OpCode.RST_PopAbsolute] = "RST PopAbsolute";   // 0x00CF
        z80OperandType    [OpCode.RST_PopAbsolute] = OperandType.Implied;

        z80InstructionName[OpCode.RST_PushAbsolute] = "RST PushAbsolute"; // 0x00D7
        z80OperandType    [OpCode.RST_PushAbsolute] = OperandType.Implied;

        z80InstructionName[OpCode.RST_PushImmediate] = "RST PushImmediate";// 0x00DF
        z80OperandType    [OpCode.RST_PushImmediate] = OperandType.Implied;

        z80InstructionName[OpCode.RST_PushOffset] = "RST PushOffset";     // 0x00E7
        z80OperandType    [OpCode.RST_PushOffset] = OperandType.Implied;

        z80InstructionName[OpCode.RST_PopOffset] = "RST PopOffset";       // 0x00EF
        z80OperandType    [OpCode.RST_PopOffset] = OperandType.Implied;

        z80InstructionName[OpCode.RST_SysCall0] = "RST SysCall0";         // 0x00F7
        z80OperandType    [OpCode.RST_SysCall0] = OperandType.Implied;

        z80InstructionName[OpCode.RST_Instruction] = "RST Instruction";   // 0x00FF
        z80OperandType    [OpCode.RST_Instruction] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_0_A] = "BIT 0, A";                  // 0xCB47
        z80OperandType    [OpCode.BIT_0_A] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_0_B] = "BIT 0, B";                  // 0xCB40
        z80OperandType    [OpCode.BIT_0_B] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_0_C] = "BIT 0, C";                  // 0xCB41
        z80OperandType    [OpCode.BIT_0_C] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_0_D] = "BIT 0, D";                  // 0xCB42
        z80OperandType    [OpCode.BIT_0_D] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_0_E] = "BIT 0, E";                  // 0xCB43
        z80OperandType    [OpCode.BIT_0_E] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_0_H] = "BIT 0, H";                  // 0xCB44
        z80OperandType    [OpCode.BIT_0_H] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_0_L] = "BIT 0, L";                  // 0xCB45
        z80OperandType    [OpCode.BIT_0_L] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_0_iHL] = "BIT 0, (HL)";             // 0xCB46
        z80OperandType    [OpCode.BIT_0_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_1_B] = "BIT 1, B";                  // 0xCB48
        z80OperandType    [OpCode.BIT_1_B] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_1_C] = "BIT 1, C";                  // 0xCB49
        z80OperandType    [OpCode.BIT_1_C] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_1_D] = "BIT 1, D";                  // 0xCB4A
        z80OperandType    [OpCode.BIT_1_D] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_1_E] = "BIT 1, E";                  // 0xCB4B
        z80OperandType    [OpCode.BIT_1_E] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_1_H] = "BIT 1, H";                  // 0xCB4C
        z80OperandType    [OpCode.BIT_1_H] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_1_L] = "BIT 1, L";                  // 0xCB4D
        z80OperandType    [OpCode.BIT_1_L] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_1_iHL] = "BIT 1, (HL)";             // 0xCB4E
        z80OperandType    [OpCode.BIT_1_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_1_A] = "BIT 1, A";                  // 0xCB4F
        z80OperandType    [OpCode.BIT_1_A] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_2_B] = "BIT 2, B";                  // 0xCB50
        z80OperandType    [OpCode.BIT_2_B] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_2_C] = "BIT 2, C";                  // 0xCB51
        z80OperandType    [OpCode.BIT_2_C] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_2_D] = "BIT 2, D";                  // 0xCB52
        z80OperandType    [OpCode.BIT_2_D] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_2_E] = "BIT 2, E";                  // 0xCB53
        z80OperandType    [OpCode.BIT_2_E] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_2_H] = "BIT 2, H";                  // 0xCB54
        z80OperandType    [OpCode.BIT_2_H] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_2_L] = "BIT 2, L";                  // 0xCB55
        z80OperandType    [OpCode.BIT_2_L] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_2_iHL] = "BIT 2, (HL)";             // 0xCB56
        z80OperandType    [OpCode.BIT_2_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_2_A] = "BIT 2, A";                  // 0xCB57
        z80OperandType    [OpCode.BIT_2_A] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_3_B] = "BIT 3, B";                  // 0xCB58
        z80OperandType    [OpCode.BIT_3_B] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_3_C] = "BIT 3, C";                  // 0xCB59
        z80OperandType    [OpCode.BIT_3_C] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_3_D] = "BIT 3, D";                  // 0xCB5A
        z80OperandType    [OpCode.BIT_3_D] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_3_E] = "BIT 3, E";                  // 0xCB5B
        z80OperandType    [OpCode.BIT_3_E] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_3_H] = "BIT 3, H";                  // 0xCB5C
        z80OperandType    [OpCode.BIT_3_H] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_3_L] = "BIT 3, L";                  // 0xCB5D
        z80OperandType    [OpCode.BIT_3_L] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_3_iHL] = "BIT 3, (HL)";             // 0xCB5E
        z80OperandType    [OpCode.BIT_3_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_3_A] = "BIT 3, A";                  // 0xCB5F
        z80OperandType    [OpCode.BIT_3_A] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_4_B] = "BIT 4, B";                  // 0xCB60
        z80OperandType    [OpCode.BIT_4_B] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_4_C] = "BIT 4, C";                  // 0xCB61
        z80OperandType    [OpCode.BIT_4_C] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_4_D] = "BIT 4, D";                  // 0xCB62
        z80OperandType    [OpCode.BIT_4_D] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_4_E] = "BIT 4, E";                  // 0xCB63
        z80OperandType    [OpCode.BIT_4_E] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_4_H] = "BIT 4, H";                  // 0xCB64
        z80OperandType    [OpCode.BIT_4_H] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_4_L] = "BIT 4, L";                  // 0xCB65
        z80OperandType    [OpCode.BIT_4_L] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_4_iHL] = "BIT 4, (HL)";             // 0xCB66
        z80OperandType    [OpCode.BIT_4_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_4_A] = "BIT 4, A";                  // 0xCB67
        z80OperandType    [OpCode.BIT_4_A] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_5_B] = "BIT 5, B";                  // 0xCB68
        z80OperandType    [OpCode.BIT_5_B] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_5_C] = "BIT 5, C";                  // 0xCB69
        z80OperandType    [OpCode.BIT_5_C] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_5_D] = "BIT 5, D";                  // 0xCB6A
        z80OperandType    [OpCode.BIT_5_D] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_5_E] = "BIT 5, E";                  // 0xCB6B
        z80OperandType    [OpCode.BIT_5_E] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_5_H] = "BIT 5, H";                  // 0xCB6C
        z80OperandType    [OpCode.BIT_5_H] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_5_L] = "BIT 5, L";                  // 0xCB6D
        z80OperandType    [OpCode.BIT_5_L] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_5_iHL] = "BIT 5, (HL)";             // 0xCB6E
        z80OperandType    [OpCode.BIT_5_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_5_A] = "BIT 5, A";                  // 0xCB6F
        z80OperandType    [OpCode.BIT_5_A] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_6_B] = "BIT 6, B";                  // 0xCB70
        z80OperandType    [OpCode.BIT_6_B] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_6_C] = "BIT 6, C";                  // 0xCB71
        z80OperandType    [OpCode.BIT_6_C] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_6_D] = "BIT 6, D";                  // 0xCB72
        z80OperandType    [OpCode.BIT_6_D] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_6_E] = "BIT 6, E";                  // 0xCB73
        z80OperandType    [OpCode.BIT_6_E] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_6_H] = "BIT 6, H";                  // 0xCB74
        z80OperandType    [OpCode.BIT_6_H] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_6_L] = "BIT 6, L";                  // 0xCB75
        z80OperandType    [OpCode.BIT_6_L] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_6_iHL] = "BIT 6, (HL)";             // 0xCB76
        z80OperandType    [OpCode.BIT_6_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_6_A] = "BIT 6, A";                  // 0xCB77
        z80OperandType    [OpCode.BIT_6_A] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_7_B] = "BIT 7, B";                  // 0xCB78
        z80OperandType    [OpCode.BIT_7_B] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_7_C] = "BIT 7, C";                  // 0xCB79
        z80OperandType    [OpCode.BIT_7_C] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_7_D] = "BIT 7, D";                  // 0xCB7A
        z80OperandType    [OpCode.BIT_7_D] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_7_E] = "BIT 7, E";                  // 0xCB7B
        z80OperandType    [OpCode.BIT_7_E] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_7_H] = "BIT 7, H";                  // 0xCB7C
        z80OperandType    [OpCode.BIT_7_H] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_7_L] = "BIT 7, L";                  // 0xCB7D
        z80OperandType    [OpCode.BIT_7_L] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_7_iHL] = "BIT 7, (HL)";             // 0xCB7E
        z80OperandType    [OpCode.BIT_7_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.BIT_7_A] = "BIT 7, A";                  // 0xCB7F
        z80OperandType    [OpCode.BIT_7_A] = OperandType.Implied;

        z80InstructionName[OpCode.RES_0_B] = "RES 0, B";                  // 0xCB80
        z80OperandType    [OpCode.RES_0_B] = OperandType.Implied;

        z80InstructionName[OpCode.RES_0_C] = "RES 0, C";                  // 0xCB81
        z80OperandType    [OpCode.RES_0_C] = OperandType.Implied;

        z80InstructionName[OpCode.RES_0_D] = "RES 0, D";                  // 0xCB82
        z80OperandType    [OpCode.RES_0_D] = OperandType.Implied;

        z80InstructionName[OpCode.RES_0_E] = "RES 0, E";                  // 0xCB83
        z80OperandType    [OpCode.RES_0_E] = OperandType.Implied;

        z80InstructionName[OpCode.RES_0_H] = "RES 0, H";                  // 0xCB84
        z80OperandType    [OpCode.RES_0_H] = OperandType.Implied;

        z80InstructionName[OpCode.RES_0_L] = "RES 0, L";                  // 0xCB85
        z80OperandType    [OpCode.RES_0_L] = OperandType.Implied;

        z80InstructionName[OpCode.RES_0_iHL] = "RES 0, (HL)";             // 0xCB86
        z80OperandType    [OpCode.RES_0_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.RES_0_A] = "RES 0, A";                  // 0xCB87
        z80OperandType    [OpCode.RES_0_A] = OperandType.Implied;

        z80InstructionName[OpCode.RES_1_B] = "RES 1, B";                  // 0xCB88
        z80OperandType    [OpCode.RES_1_B] = OperandType.Implied;

        z80InstructionName[OpCode.RES_1_C] = "RES 1, C";                  // 0xCB89
        z80OperandType    [OpCode.RES_1_C] = OperandType.Implied;

        z80InstructionName[OpCode.RES_1_D] = "RES 1, D";                  // 0xCB8A
        z80OperandType    [OpCode.RES_1_D] = OperandType.Implied;

        z80InstructionName[OpCode.RES_1_E] = "RES 1, E";                  // 0xCB8B
        z80OperandType    [OpCode.RES_1_E] = OperandType.Implied;

        z80InstructionName[OpCode.RES_1_H] = "RES 1, H";                  // 0xCB8C
        z80OperandType    [OpCode.RES_1_H] = OperandType.Implied;

        z80InstructionName[OpCode.RES_1_L] = "RES 1, L";                  // 0xCB8D
        z80OperandType    [OpCode.RES_1_L] = OperandType.Implied;

        z80InstructionName[OpCode.RES_1_iHL] = "RES 1, (HL)";             // 0xCB8E
        z80OperandType    [OpCode.RES_1_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.RES_1_A] = "RES 1, A";                  // 0xCB8F
        z80OperandType    [OpCode.RES_1_A] = OperandType.Implied;

        z80InstructionName[OpCode.RES_2_B] = "RES 2, B";                  // 0xCB90
        z80OperandType    [OpCode.RES_2_B] = OperandType.Implied;

        z80InstructionName[OpCode.RES_2_C] = "RES 2, C";                  // 0xCB91
        z80OperandType    [OpCode.RES_2_C] = OperandType.Implied;

        z80InstructionName[OpCode.RES_2_D] = "RES 2, D";                  // 0xCB92
        z80OperandType    [OpCode.RES_2_D] = OperandType.Implied;

        z80InstructionName[OpCode.RES_2_E] = "RES 2, E";                  // 0xCB93
        z80OperandType    [OpCode.RES_2_E] = OperandType.Implied;

        z80InstructionName[OpCode.RES_2_H] = "RES 2, H";                  // 0xCB94
        z80OperandType    [OpCode.RES_2_H] = OperandType.Implied;

        z80InstructionName[OpCode.RES_2_L] = "RES 2, L";                  // 0xCB95
        z80OperandType    [OpCode.RES_2_L] = OperandType.Implied;

        z80InstructionName[OpCode.RES_2_iHL] = "RES 2, (HL)";             // 0xCB96
        z80OperandType    [OpCode.RES_2_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.RES_2_A] = "RES 2, A";                  // 0xCB97
        z80OperandType    [OpCode.RES_2_A] = OperandType.Implied;

        z80InstructionName[OpCode.RES_3_B] = "RES 3, B";                  // 0xCB98
        z80OperandType    [OpCode.RES_3_B] = OperandType.Implied;

        z80InstructionName[OpCode.RES_3_C] = "RES 3, C";                  // 0xCB99
        z80OperandType    [OpCode.RES_3_C] = OperandType.Implied;

        z80InstructionName[OpCode.RES_3_D] = "RES 3, D";                  // 0xCB9A
        z80OperandType    [OpCode.RES_3_D] = OperandType.Implied;

        z80InstructionName[OpCode.RES_3_E] = "RES 3, E";                  // 0xCB9B
        z80OperandType    [OpCode.RES_3_E] = OperandType.Implied;

        z80InstructionName[OpCode.RES_3_H] = "RES 3, H";                  // 0xCB9C
        z80OperandType    [OpCode.RES_3_H] = OperandType.Implied;

        z80InstructionName[OpCode.RES_3_L] = "RES 3, L";                  // 0xCB9D
        z80OperandType    [OpCode.RES_3_L] = OperandType.Implied;

        z80InstructionName[OpCode.RES_3_iHL] = "RES 3, (HL)";             // 0xCB9E
        z80OperandType    [OpCode.RES_3_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.RES_3_A] = "RES 3, A";                  // 0xCB9F
        z80OperandType    [OpCode.RES_3_A] = OperandType.Implied;

        z80InstructionName[OpCode.RES_4_B] = "RES 4, B";                  // 0xCBA0
        z80OperandType    [OpCode.RES_4_B] = OperandType.Implied;

        z80InstructionName[OpCode.RES_4_C] = "RES 4, C";                  // 0xCBA1
        z80OperandType    [OpCode.RES_4_C] = OperandType.Implied;

        z80InstructionName[OpCode.RES_4_D] = "RES 4, D";                  // 0xCBA2
        z80OperandType    [OpCode.RES_4_D] = OperandType.Implied;

        z80InstructionName[OpCode.RES_4_E] = "RES 4, E";                  // 0xCBA3
        z80OperandType    [OpCode.RES_4_E] = OperandType.Implied;

        z80InstructionName[OpCode.RES_4_H] = "RES 4, H";                  // 0xCBA4
        z80OperandType    [OpCode.RES_4_H] = OperandType.Implied;

        z80InstructionName[OpCode.RES_4_L] = "RES 4, L";                  // 0xCBA5
        z80OperandType    [OpCode.RES_4_L] = OperandType.Implied;

        z80InstructionName[OpCode.RES_4_iHL] = "RES 4, (HL)";             // 0xCBA6
        z80OperandType    [OpCode.RES_4_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.RES_4_A] = "RES 4, A";                  // 0xCBA7
        z80OperandType    [OpCode.RES_4_A] = OperandType.Implied;

        z80InstructionName[OpCode.RES_5_B] = "RES 5, B";                  // 0xCBA8
        z80OperandType    [OpCode.RES_5_B] = OperandType.Implied;

        z80InstructionName[OpCode.RES_5_C] = "RES 5, C";                  // 0xCBA9
        z80OperandType    [OpCode.RES_5_C] = OperandType.Implied;

        z80InstructionName[OpCode.RES_5_D] = "RES 5, D";                  // 0xCBAA
        z80OperandType    [OpCode.RES_5_D] = OperandType.Implied;

        z80InstructionName[OpCode.RES_5_E] = "RES 5, E";                  // 0xCBAB
        z80OperandType    [OpCode.RES_5_E] = OperandType.Implied;

        z80InstructionName[OpCode.RES_5_H] = "RES 5, H";                  // 0xCBAC
        z80OperandType    [OpCode.RES_5_H] = OperandType.Implied;

        z80InstructionName[OpCode.RES_5_L] = "RES 5, L";                  // 0xCBAD
        z80OperandType    [OpCode.RES_5_L] = OperandType.Implied;

        z80InstructionName[OpCode.RES_5_iHL] = "RES 5, (HL)";             // 0xCBAE
        z80OperandType    [OpCode.RES_5_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.RES_5_A] = "RES 5, A";                  // 0xCBAF
        z80OperandType    [OpCode.RES_5_A] = OperandType.Implied;

        z80InstructionName[OpCode.RES_6_B] = "RES 6, B";                  // 0xCBB0
        z80OperandType    [OpCode.RES_6_B] = OperandType.Implied;

        z80InstructionName[OpCode.RES_6_C] = "RES 6, C";                  // 0xCBB1
        z80OperandType    [OpCode.RES_6_C] = OperandType.Implied;

        z80InstructionName[OpCode.RES_6_D] = "RES 6, D";                  // 0xCBB2
        z80OperandType    [OpCode.RES_6_D] = OperandType.Implied;

        z80InstructionName[OpCode.RES_6_E] = "RES 6, E";                  // 0xCBB3
        z80OperandType    [OpCode.RES_6_E] = OperandType.Implied;

        z80InstructionName[OpCode.RES_6_H] = "RES 6, H";                  // 0xCBB4
        z80OperandType    [OpCode.RES_6_H] = OperandType.Implied;

        z80InstructionName[OpCode.RES_6_L] = "RES 6, L";                  // 0xCBB5
        z80OperandType    [OpCode.RES_6_L] = OperandType.Implied;

        z80InstructionName[OpCode.RES_6_iHL] = "RES 6, (HL)";             // 0xCBB6
        z80OperandType    [OpCode.RES_6_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.RES_6_A] = "RES 6, A";                  // 0xCBB7
        z80OperandType    [OpCode.RES_6_A] = OperandType.Implied;

        z80InstructionName[OpCode.RES_7_B] = "RES 7, B";                  // 0xCBB8
        z80OperandType    [OpCode.RES_7_B] = OperandType.Implied;

        z80InstructionName[OpCode.RES_7_C] = "RES 7, C";                  // 0xCBB9
        z80OperandType    [OpCode.RES_7_C] = OperandType.Implied;

        z80InstructionName[OpCode.RES_7_D] = "RES 7, D";                  // 0xCBBA
        z80OperandType    [OpCode.RES_7_D] = OperandType.Implied;

        z80InstructionName[OpCode.RES_7_E] = "RES 7, E";                  // 0xCBBB
        z80OperandType    [OpCode.RES_7_E] = OperandType.Implied;

        z80InstructionName[OpCode.RES_7_H] = "RES 7, H";                  // 0xCBBC
        z80OperandType    [OpCode.RES_7_H] = OperandType.Implied;

        z80InstructionName[OpCode.RES_7_L] = "RES 7, L";                  // 0xCBBD
        z80OperandType    [OpCode.RES_7_L] = OperandType.Implied;

        z80InstructionName[OpCode.RES_7_iHL] = "RES 7, (HL)";             // 0xCBBE
        z80OperandType    [OpCode.RES_7_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.RES_7_A] = "RES 7, A";                  // 0xCBBF
        z80OperandType    [OpCode.RES_7_A] = OperandType.Implied;

        z80InstructionName[OpCode.SET_0_B] = "SET 0, B";                  // 0xCBC0
        z80OperandType    [OpCode.SET_0_B] = OperandType.Implied;

        z80InstructionName[OpCode.SET_0_C] = "SET 0, C";                  // 0xCBC1
        z80OperandType    [OpCode.SET_0_C] = OperandType.Implied;

        z80InstructionName[OpCode.SET_0_D] = "SET 0, D";                  // 0xCBC2
        z80OperandType    [OpCode.SET_0_D] = OperandType.Implied;

        z80InstructionName[OpCode.SET_0_E] = "SET 0, E";                  // 0xCBC3
        z80OperandType    [OpCode.SET_0_E] = OperandType.Implied;

        z80InstructionName[OpCode.SET_0_H] = "SET 0, H";                  // 0xCBC4
        z80OperandType    [OpCode.SET_0_H] = OperandType.Implied;

        z80InstructionName[OpCode.SET_0_L] = "SET 0, L";                  // 0xCBC5
        z80OperandType    [OpCode.SET_0_L] = OperandType.Implied;

        z80InstructionName[OpCode.SET_0_iHL] = "SET 0, (HL)";             // 0xCBC6
        z80OperandType    [OpCode.SET_0_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SET_0_A] = "SET 0, A";                  // 0xCBC7
        z80OperandType    [OpCode.SET_0_A] = OperandType.Implied;

        z80InstructionName[OpCode.SET_1_B] = "SET 1, B";                  // 0xCBC8
        z80OperandType    [OpCode.SET_1_B] = OperandType.Implied;

        z80InstructionName[OpCode.SET_1_C] = "SET 1, C";                  // 0xCBC9
        z80OperandType    [OpCode.SET_1_C] = OperandType.Implied;

        z80InstructionName[OpCode.SET_1_D] = "SET 1, D";                  // 0xCBCA
        z80OperandType    [OpCode.SET_1_D] = OperandType.Implied;

        z80InstructionName[OpCode.SET_1_E] = "SET 1, E";                  // 0xCBCB
        z80OperandType    [OpCode.SET_1_E] = OperandType.Implied;

        z80InstructionName[OpCode.SET_1_H] = "SET 1, H";                  // 0xCBCC
        z80OperandType    [OpCode.SET_1_H] = OperandType.Implied;

        z80InstructionName[OpCode.SET_1_L] = "SET 1, L";                  // 0xCBCD
        z80OperandType    [OpCode.SET_1_L] = OperandType.Implied;

        z80InstructionName[OpCode.SET_1_iHL] = "SET 1, (HL)";             // 0xCBCE
        z80OperandType    [OpCode.SET_1_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SET_1_A] = "SET 1, A";                  // 0xCBCF
        z80OperandType    [OpCode.SET_1_A] = OperandType.Implied;

        z80InstructionName[OpCode.SET_2_B] = "SET 2, B";                  // 0xCBD0
        z80OperandType    [OpCode.SET_2_B] = OperandType.Implied;

        z80InstructionName[OpCode.SET_2_C] = "SET 2, C";                  // 0xCBD1
        z80OperandType    [OpCode.SET_2_C] = OperandType.Implied;

        z80InstructionName[OpCode.SET_2_D] = "SET 2, D";                  // 0xCBD2
        z80OperandType    [OpCode.SET_2_D] = OperandType.Implied;

        z80InstructionName[OpCode.SET_2_E] = "SET 2, E";                  // 0xCBD3
        z80OperandType    [OpCode.SET_2_E] = OperandType.Implied;

        z80InstructionName[OpCode.SET_2_H] = "SET 2, H";                  // 0xCBD4
        z80OperandType    [OpCode.SET_2_H] = OperandType.Implied;

        z80InstructionName[OpCode.SET_2_L] = "SET 2, L";                  // 0xCBD5
        z80OperandType    [OpCode.SET_2_L] = OperandType.Implied;

        z80InstructionName[OpCode.SET_2_iHL] = "SET 2, (HL)";             // 0xCBD6
        z80OperandType    [OpCode.SET_2_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SET_2_A] = "SET 2, A";                  // 0xCBD7
        z80OperandType    [OpCode.SET_2_A] = OperandType.Implied;

        z80InstructionName[OpCode.SET_3_B] = "SET 3, B";                  // 0xCBD8
        z80OperandType    [OpCode.SET_3_B] = OperandType.Implied;

        z80InstructionName[OpCode.SET_3_C] = "SET 3, C";                  // 0xCBD9
        z80OperandType    [OpCode.SET_3_C] = OperandType.Implied;

        z80InstructionName[OpCode.SET_3_D] = "SET 3, D";                  // 0xCBDA
        z80OperandType    [OpCode.SET_3_D] = OperandType.Implied;

        z80InstructionName[OpCode.SET_3_E] = "SET 3, E";                  // 0xCBDB
        z80OperandType    [OpCode.SET_3_E] = OperandType.Implied;

        z80InstructionName[OpCode.SET_3_H] = "SET 3, H";                  // 0xCBDC
        z80OperandType    [OpCode.SET_3_H] = OperandType.Implied;

        z80InstructionName[OpCode.SET_3_L] = "SET 3, L";                  // 0xCBDD
        z80OperandType    [OpCode.SET_3_L] = OperandType.Implied;

        z80InstructionName[OpCode.SET_3_iHL] = "SET 3, (HL)";             // 0xCBDE
        z80OperandType    [OpCode.SET_3_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SET_3_A] = "SET 3, A";                  // 0xCBDF
        z80OperandType    [OpCode.SET_3_A] = OperandType.Implied;

        z80InstructionName[OpCode.SET_4_B] = "SET 4, B";                  // 0xCBE0
        z80OperandType    [OpCode.SET_4_B] = OperandType.Implied;

        z80InstructionName[OpCode.SET_4_C] = "SET 4, C";                  // 0xCBE1
        z80OperandType    [OpCode.SET_4_C] = OperandType.Implied;

        z80InstructionName[OpCode.SET_4_D] = "SET 4, D";                  // 0xCBE2
        z80OperandType    [OpCode.SET_4_D] = OperandType.Implied;

        z80InstructionName[OpCode.SET_4_E] = "SET 4, E";                  // 0xCBE3
        z80OperandType    [OpCode.SET_4_E] = OperandType.Implied;

        z80InstructionName[OpCode.SET_4_H] = "SET 4, H";                  // 0xCBE4
        z80OperandType    [OpCode.SET_4_H] = OperandType.Implied;

        z80InstructionName[OpCode.SET_4_L] = "SET 4, L";                  // 0xCBE5
        z80OperandType    [OpCode.SET_4_L] = OperandType.Implied;

        z80InstructionName[OpCode.SET_4_iHL] = "SET 4, (HL)";             // 0xCBE6
        z80OperandType    [OpCode.SET_4_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SET_4_A] = "SET 4, A";                  // 0xCBE7
        z80OperandType    [OpCode.SET_4_A] = OperandType.Implied;

        z80InstructionName[OpCode.SET_5_B] = "SET 5, B";                  // 0xCBE8
        z80OperandType    [OpCode.SET_5_B] = OperandType.Implied;

        z80InstructionName[OpCode.SET_5_C] = "SET 5, C";                  // 0xCBE9
        z80OperandType    [OpCode.SET_5_C] = OperandType.Implied;

        z80InstructionName[OpCode.SET_5_D] = "SET 5, D";                  // 0xCBEA
        z80OperandType    [OpCode.SET_5_D] = OperandType.Implied;

        z80InstructionName[OpCode.SET_5_E] = "SET 5, E";                  // 0xCBEB
        z80OperandType    [OpCode.SET_5_E] = OperandType.Implied;

        z80InstructionName[OpCode.SET_5_H] = "SET 5, H";                  // 0xCBEC
        z80OperandType    [OpCode.SET_5_H] = OperandType.Implied;

        z80InstructionName[OpCode.SET_5_L] = "SET 5, L";                  // 0xCBED
        z80OperandType    [OpCode.SET_5_L] = OperandType.Implied;

        z80InstructionName[OpCode.SET_5_iHL] = "SET 5, (HL)";             // 0xCBEE
        z80OperandType    [OpCode.SET_5_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SET_5_A] = "SET 5, A";                  // 0xCBEF
        z80OperandType    [OpCode.SET_5_A] = OperandType.Implied;

        z80InstructionName[OpCode.SET_6_B] = "SET 6, B";                  // 0xCBF0
        z80OperandType    [OpCode.SET_6_B] = OperandType.Implied;

        z80InstructionName[OpCode.SET_6_C] = "SET 6, C";                  // 0xCBF1
        z80OperandType    [OpCode.SET_6_C] = OperandType.Implied;

        z80InstructionName[OpCode.SET_6_D] = "SET 6, D";                  // 0xCBF2
        z80OperandType    [OpCode.SET_6_D] = OperandType.Implied;

        z80InstructionName[OpCode.SET_6_E] = "SET 6, E";                  // 0xCBF3
        z80OperandType    [OpCode.SET_6_E] = OperandType.Implied;

        z80InstructionName[OpCode.SET_6_H] = "SET 6, H";                  // 0xCBF4
        z80OperandType    [OpCode.SET_6_H] = OperandType.Implied;

        z80InstructionName[OpCode.SET_6_L] = "SET 6, L";                  // 0xCBF5
        z80OperandType    [OpCode.SET_6_L] = OperandType.Implied;

        z80InstructionName[OpCode.SET_6_iHL] = "SET 6, (HL)";             // 0xCBF6
        z80OperandType    [OpCode.SET_6_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SET_6_A] = "SET 6, A";                  // 0xCBF7
        z80OperandType    [OpCode.SET_6_A] = OperandType.Implied;

        z80InstructionName[OpCode.SET_7_B] = "SET 7, B";                  // 0xCBF8
        z80OperandType    [OpCode.SET_7_B] = OperandType.Implied;

        z80InstructionName[OpCode.SET_7_C] = "SET 7, C";                  // 0xCBF9
        z80OperandType    [OpCode.SET_7_C] = OperandType.Implied;

        z80InstructionName[OpCode.SET_7_D] = "SET 7, D";                  // 0xCBFA
        z80OperandType    [OpCode.SET_7_D] = OperandType.Implied;

        z80InstructionName[OpCode.SET_7_E] = "SET 7, E";                  // 0xCBFB
        z80OperandType    [OpCode.SET_7_E] = OperandType.Implied;

        z80InstructionName[OpCode.SET_7_H] = "SET 7, H";                  // 0xCBFC
        z80OperandType    [OpCode.SET_7_H] = OperandType.Implied;

        z80InstructionName[OpCode.SET_7_L] = "SET 7, L";                  // 0xCBFD
        z80OperandType    [OpCode.SET_7_L] = OperandType.Implied;

        z80InstructionName[OpCode.SET_7_iHL] = "SET 7, (HL)";             // 0xCBFE
        z80OperandType    [OpCode.SET_7_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SET_7_A] = "SET 7, A";                  // 0xCBFF
        z80OperandType    [OpCode.SET_7_A] = OperandType.Implied;

        z80InstructionName[OpCode.LD_inn_A] = "LD (nn), A";               // 0x0032
        z80OperandType    [OpCode.LD_inn_A] = OperandType.ImmediateIndexed;

        z80InstructionName[OpCode.LD_A_inn] = "LD A, (nn)";               // 0x003A
        z80OperandType    [OpCode.LD_A_inn] = OperandType.ImmediateIndexed;

        z80InstructionName[OpCode.LD_A_n] = "LD A, n";                    // 0x003E
        z80OperandType    [OpCode.LD_A_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.LD_B_n] = "LD B, n";                    // 0x0006
        z80OperandType    [OpCode.LD_B_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.LD_C_n] = "LD C, n";                    // 0x000E
        z80OperandType    [OpCode.LD_C_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.LD_D_n] = "LD D, n";                    // 0x0016
        z80OperandType    [OpCode.LD_D_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.LD_E_n] = "LD E, n";                    // 0x001E
        z80OperandType    [OpCode.LD_E_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.LD_H_n] = "LD H, n";                    // 0x0026
        z80OperandType    [OpCode.LD_H_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.LD_L_n] = "LD L, n";                    // 0x002E
        z80OperandType    [OpCode.LD_L_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.LD_B_B] = "LD B, B";                    // 0x0040
        z80OperandType    [OpCode.LD_B_B] = OperandType.Implied;

        z80InstructionName[OpCode.LD_B_C] = "LD B, C";                    // 0x0041
        z80OperandType    [OpCode.LD_B_C] = OperandType.Implied;

        z80InstructionName[OpCode.LD_B_D] = "LD B, D";                    // 0x0042
        z80OperandType    [OpCode.LD_B_D] = OperandType.Implied;

        z80InstructionName[OpCode.LD_B_E] = "LD B, E";                    // 0x0043
        z80OperandType    [OpCode.LD_B_E] = OperandType.Implied;

        z80InstructionName[OpCode.LD_B_H] = "LD B, H";                    // 0x0044
        z80OperandType    [OpCode.LD_B_H] = OperandType.Implied;

        z80InstructionName[OpCode.LD_B_L] = "LD B, L";                    // 0x0045
        z80OperandType    [OpCode.LD_B_L] = OperandType.Implied;

        z80InstructionName[OpCode.LD_B_A] = "LD B, A";                    // 0x0047
        z80OperandType    [OpCode.LD_B_A] = OperandType.Implied;

        z80InstructionName[OpCode.LD_C_B] = "LD C, B";                    // 0x0048
        z80OperandType    [OpCode.LD_C_B] = OperandType.Implied;

        z80InstructionName[OpCode.LD_C_C] = "LD C, C";                    // 0x0049
        z80OperandType    [OpCode.LD_C_C] = OperandType.Implied;

        z80InstructionName[OpCode.LD_C_D] = "LD C, D";                    // 0x004A
        z80OperandType    [OpCode.LD_C_D] = OperandType.Implied;

        z80InstructionName[OpCode.LD_C_E] = "LD C, E";                    // 0x004B
        z80OperandType    [OpCode.LD_C_E] = OperandType.Implied;

        z80InstructionName[OpCode.LD_C_H] = "LD C, H";                    // 0x004C
        z80OperandType    [OpCode.LD_C_H] = OperandType.Implied;

        z80InstructionName[OpCode.LD_C_L] = "LD C, L";                    // 0x004D
        z80OperandType    [OpCode.LD_C_L] = OperandType.Implied;

        z80InstructionName[OpCode.LD_C_A] = "LD C, A";                    // 0x004F
        z80OperandType    [OpCode.LD_C_A] = OperandType.Implied;

        z80InstructionName[OpCode.LD_D_B] = "LD D, B";                    // 0x0050
        z80OperandType    [OpCode.LD_D_B] = OperandType.Implied;

        z80InstructionName[OpCode.LD_D_C] = "LD D, C";                    // 0x0051
        z80OperandType    [OpCode.LD_D_C] = OperandType.Implied;

        z80InstructionName[OpCode.LD_D_D] = "LD D, D";                    // 0x0052
        z80OperandType    [OpCode.LD_D_D] = OperandType.Implied;

        z80InstructionName[OpCode.LD_D_E] = "LD D, E";                    // 0x0053
        z80OperandType    [OpCode.LD_D_E] = OperandType.Implied;

        z80InstructionName[OpCode.LD_D_H] = "LD D, H";                    // 0x0054
        z80OperandType    [OpCode.LD_D_H] = OperandType.Implied;

        z80InstructionName[OpCode.LD_D_IYH] = "LD D, IYh";                // 0xFD54
        z80OperandType    [OpCode.LD_D_IYH] = OperandType.Implied;

        z80InstructionName[OpCode.LD_D_L] = "LD D, L";                    // 0x0055
        z80OperandType    [OpCode.LD_D_L] = OperandType.Implied;

        z80InstructionName[OpCode.LD_D_A] = "LD D, A";                    // 0x0057
        z80OperandType    [OpCode.LD_D_A] = OperandType.Implied;

        z80InstructionName[OpCode.LD_E_B] = "LD E, B";                    // 0x0058
        z80OperandType    [OpCode.LD_E_B] = OperandType.Implied;

        z80InstructionName[OpCode.LD_E_C] = "LD E, C";                    // 0x0059
        z80OperandType    [OpCode.LD_E_C] = OperandType.Implied;

        z80InstructionName[OpCode.LD_E_D] = "LD E, D";                    // 0x005A
        z80OperandType    [OpCode.LD_E_D] = OperandType.Implied;

        z80InstructionName[OpCode.LD_E_E] = "LD E, E";                    // 0x005B
        z80OperandType    [OpCode.LD_E_E] = OperandType.Implied;

        z80InstructionName[OpCode.LD_E_H] = "LD E, H";                    // 0x005C
        z80OperandType    [OpCode.LD_E_H] = OperandType.Implied;

        z80InstructionName[OpCode.LD_E_L] = "LD E, L";                    // 0x005D
        z80OperandType    [OpCode.LD_E_L] = OperandType.Implied;

        z80InstructionName[OpCode.LD_E_IYL] = "LD E, IYl";                // 0xFD5D
        z80OperandType    [OpCode.LD_E_IYL] = OperandType.Implied;

        z80InstructionName[OpCode.LD_E_A] = "LD E, A";                    // 0x005F
        z80OperandType    [OpCode.LD_E_A] = OperandType.Implied;

        z80InstructionName[OpCode.LD_H_B] = "LD H, B";                    // 0x0060
        z80OperandType    [OpCode.LD_H_B] = OperandType.Implied;

        z80InstructionName[OpCode.LD_H_C] = "LD H, C";                    // 0x0061
        z80OperandType    [OpCode.LD_H_C] = OperandType.Implied;

        z80InstructionName[OpCode.LD_H_D] = "LD H, D";                    // 0x0062
        z80OperandType    [OpCode.LD_H_D] = OperandType.Implied;

        z80InstructionName[OpCode.LD_H_E] = "LD H, E";                    // 0x0063
        z80OperandType    [OpCode.LD_H_E] = OperandType.Implied;

        z80InstructionName[OpCode.LD_H_H] = "LD H, H";                    // 0x0064
        z80OperandType    [OpCode.LD_H_H] = OperandType.Implied;

        z80InstructionName[OpCode.LD_H_L] = "LD H, L";                    // 0x0065
        z80OperandType    [OpCode.LD_H_L] = OperandType.Implied;

        z80InstructionName[OpCode.LD_H_A] = "LD H, A";                    // 0x0067
        z80OperandType    [OpCode.LD_H_A] = OperandType.Implied;

        z80InstructionName[OpCode.LD_L_B] = "LD L, B";                    // 0x0068
        z80OperandType    [OpCode.LD_L_B] = OperandType.Implied;

        z80InstructionName[OpCode.LD_L_C] = "LD L, C";                    // 0x0069
        z80OperandType    [OpCode.LD_L_C] = OperandType.Implied;

        z80InstructionName[OpCode.LD_L_D] = "LD L, D";                    // 0x006A
        z80OperandType    [OpCode.LD_L_D] = OperandType.Implied;

        z80InstructionName[OpCode.LD_L_E] = "LD L, E";                    // 0x006B
        z80OperandType    [OpCode.LD_L_E] = OperandType.Implied;

        z80InstructionName[OpCode.LD_L_H] = "LD L, H";                    // 0x006C
        z80OperandType    [OpCode.LD_L_H] = OperandType.Implied;

        z80InstructionName[OpCode.LD_L_L] = "LD L, L";                    // 0x006D
        z80OperandType    [OpCode.LD_L_L] = OperandType.Implied;

        z80InstructionName[OpCode.LD_L_A] = "LD L, A";                    // 0x006F
        z80OperandType    [OpCode.LD_L_A] = OperandType.Implied;

        z80InstructionName[OpCode.LD_A_B] = "LD A, B";                    // 0x0078
        z80OperandType    [OpCode.LD_A_B] = OperandType.Implied;

        z80InstructionName[OpCode.LD_A_C] = "LD A, C";                    // 0x0079
        z80OperandType    [OpCode.LD_A_C] = OperandType.Implied;

        z80InstructionName[OpCode.LD_A_D] = "LD A, D";                    // 0x007A
        z80OperandType    [OpCode.LD_A_D] = OperandType.Implied;

        z80InstructionName[OpCode.LD_A_E] = "LD A, E";                    // 0x007B
        z80OperandType    [OpCode.LD_A_E] = OperandType.Implied;

        z80InstructionName[OpCode.LD_A_H] = "LD A, H";                    // 0x007C
        z80OperandType    [OpCode.LD_A_H] = OperandType.Implied;

        z80InstructionName[OpCode.LD_A_L] = "LD A, L";                    // 0x007D
        z80OperandType    [OpCode.LD_A_L] = OperandType.Implied;

        z80InstructionName[OpCode.LD_A_A] = "LD A, A";                    // 0x007F
        z80OperandType    [OpCode.LD_A_A] = OperandType.Implied;

        z80InstructionName[OpCode.LD_A_iHL] = "LD A, (HL)";               // 0x007E
        z80OperandType    [OpCode.LD_A_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.LD_B_iHL] = "LD B, (HL)";               // 0x0046
        z80OperandType    [OpCode.LD_B_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.LD_C_iHL] = "LD C, (HL)";               // 0x004E
        z80OperandType    [OpCode.LD_C_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.LD_D_iHL] = "LD D, (HL)";               // 0x0056
        z80OperandType    [OpCode.LD_D_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.LD_E_iHL] = "LD E, (HL)";               // 0x005E
        z80OperandType    [OpCode.LD_E_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.LD_H_iHL] = "LD H, (HL)";               // 0x0066
        z80OperandType    [OpCode.LD_H_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.LD_L_iHL] = "LD L, (HL)";               // 0x006E
        z80OperandType    [OpCode.LD_L_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.LD_A_iIX_d] = "LD A, (IX+d)";           // 0xDD7E
        z80OperandType    [OpCode.LD_A_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_B_iIX_d] = "LD B, (IX+d)";           // 0xDD46
        z80OperandType    [OpCode.LD_B_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_C_iIX_d] = "LD C, (IX+d)";           // 0xDD4E
        z80OperandType    [OpCode.LD_C_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_D_iIX_d] = "LD D, (IX+d)";           // 0xDD56
        z80OperandType    [OpCode.LD_D_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_E_iIX_d] = "LD E, (IX+d)";           // 0xDD5E
        z80OperandType    [OpCode.LD_E_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_H_iIX_d] = "LD H, (IX+d)";           // 0xDD66
        z80OperandType    [OpCode.LD_H_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_L_iIX_d] = "LD L, (IX+d)";           // 0xDD6E
        z80OperandType    [OpCode.LD_L_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_A_iIY_d] = "LD A, (IY+d)";           // 0xFD7E
        z80OperandType    [OpCode.LD_A_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_B_iIY_d] = "LD B, (IY+d)";           // 0xFD46
        z80OperandType    [OpCode.LD_B_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_C_iIY_d] = "LD C, (IY+d)";           // 0xFD4E
        z80OperandType    [OpCode.LD_C_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_D_iIY_d] = "LD D, (IY+d)";           // 0xFD56
        z80OperandType    [OpCode.LD_D_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_E_iIY_d] = "LD E, (IY+d)";           // 0xFD5E
        z80OperandType    [OpCode.LD_E_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_H_iIY_d] = "LD H, (IY+d)";           // 0xFD66
        z80OperandType    [OpCode.LD_H_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_L_iIY_d] = "LD L, (IY+d)";           // 0xFD6E
        z80OperandType    [OpCode.LD_L_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIX_d_A] = "LD (IX+d), A";           // 0xDD77
        z80OperandType    [OpCode.LD_iIX_d_A] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIX_d_B] = "LD (IX+d), B";           // 0xDD70
        z80OperandType    [OpCode.LD_iIX_d_B] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIX_d_C] = "LD (IX+d), C";           // 0xDD71
        z80OperandType    [OpCode.LD_iIX_d_C] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIX_d_D] = "LD (IX+d), D";           // 0xDD72
        z80OperandType    [OpCode.LD_iIX_d_D] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIX_d_E] = "LD (IX+d), E";           // 0xDD73
        z80OperandType    [OpCode.LD_iIX_d_E] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIX_d_H] = "LD (IX+d), H";           // 0xDD74
        z80OperandType    [OpCode.LD_iIX_d_H] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIX_d_L] = "LD (IX+d), L";           // 0xDD75
        z80OperandType    [OpCode.LD_iIX_d_L] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIY_d_A] = "LD (IY+d), A";           // 0xFD77
        z80OperandType    [OpCode.LD_iIY_d_A] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIY_d_B] = "LD (IY+d), B";           // 0xFD70
        z80OperandType    [OpCode.LD_iIY_d_B] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIY_d_C] = "LD (IY+d), C";           // 0xFD71
        z80OperandType    [OpCode.LD_iIY_d_C] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIY_d_D] = "LD (IY+d), D";           // 0xFD72
        z80OperandType    [OpCode.LD_iIY_d_D] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIY_d_E] = "LD (IY+d), E";           // 0xFD73
        z80OperandType    [OpCode.LD_iIY_d_E] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIY_d_H] = "LD (IY+d), H";           // 0xFD74
        z80OperandType    [OpCode.LD_iIY_d_H] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIY_d_L] = "LD (IY+d), L";           // 0xFD75
        z80OperandType    [OpCode.LD_iIY_d_L] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.LD_iIX_d_n] = "LD (IX+d), n";           // 0xDD36
        z80OperandType    [OpCode.LD_iIX_d_n] = OperandType.RelativeImmediate8;

        z80InstructionName[OpCode.LD_iIY_d_n] = "LD (IY+d), n";           // 0xFD36
        z80OperandType    [OpCode.LD_iIY_d_n] = OperandType.RelativeImmediate8;

        z80InstructionName[OpCode.LD_BC_inn] = "LD BC, (nn)";             // 0xED4B
        z80OperandType    [OpCode.LD_BC_inn] = OperandType.ImmediateIndexed;

        z80InstructionName[OpCode.LD_DE_inn] = "LD DE, (nn)";             // 0xED5B
        z80OperandType    [OpCode.LD_DE_inn] = OperandType.ImmediateIndexed;

        z80InstructionName[OpCode.LD_HL_inn] = "LD HL, (nn)";             // 0xED6B
        z80OperandType    [OpCode.LD_HL_inn] = OperandType.ImmediateIndexed;

        z80InstructionName[OpCode.LD_SP_inn] = "LD SP, (nn)";             // 0xED7B
        z80OperandType    [OpCode.LD_SP_inn] = OperandType.ImmediateIndexed;

        z80InstructionName[OpCode.LD_inn_IX] = "LD (nn), IX";             // 0xDD22
        z80OperandType    [OpCode.LD_inn_IX] = OperandType.ImmediateIndexed;

        z80InstructionName[OpCode.LD_inn_IY] = "LD (nn), IY";             // 0xFD22
        z80OperandType    [OpCode.LD_inn_IY] = OperandType.ImmediateIndexed;
        
        z80InstructionName[OpCode.LD_IX_inn] = "LD IX, (nn)";             // 0xDD2A
        z80OperandType    [OpCode.LD_IX_inn] = OperandType.ImmediateIndexed;
        z80InstructionName[OpCode.LD_IY_inn] = "LD IY, (nn)";             // 0xFD2A
        z80OperandType    [OpCode.LD_IY_inn] = OperandType.ImmediateIndexed;
        

        z80InstructionName[OpCode.LD_inn_BC] = "LD (nn), BC";             // 0xED43
        z80OperandType    [OpCode.LD_inn_BC] = OperandType.ImmediateIndexed;

        z80InstructionName[OpCode.LD_inn_DE] = "LD (nn), DE";             // 0xED53
        z80OperandType    [OpCode.LD_inn_DE] = OperandType.ImmediateIndexed;

        z80InstructionName[OpCode.LD_inn_HL] = "LD (nn), HL";             // 0xED63
        z80OperandType    [OpCode.LD_inn_HL] = OperandType.ImmediateIndexed;

        z80InstructionName[OpCode.LD_inn_SP] = "LD (nn), SP";             // 0xED73
        z80OperandType    [OpCode.LD_inn_SP] = OperandType.ImmediateIndexed;

        z80InstructionName[OpCode.LD_BC_nn] = "LD BC, nn";                // 0x0001
        z80OperandType    [OpCode.LD_BC_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.LD_DE_nn] = "LD DE, nn";                // 0x0011
        z80OperandType    [OpCode.LD_DE_nn] = OperandType.Immediate16;
        
        z80InstructionName[OpCode.DDDD]     = "DDDD";                     // 0xDDDD
        z80OperandType    [OpCode.DDDD]     = OperandType.Implied;

        z80InstructionName[OpCode.LD_HL_nn] = "LD HL, nn";                // 0x0021
        z80OperandType    [OpCode.LD_HL_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.LD_IX_nn] = "LD IX, nn";                // 0xDD21
        z80OperandType    [OpCode.LD_IX_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.LD_IY_nn] = "LD IY, nn";                // 0xFD21
        z80OperandType    [OpCode.LD_IY_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.LD_SP_nn] = "LD SP, nn";                // 0x0031
        z80OperandType    [OpCode.LD_SP_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.LD_SP_HL] = "LD SP, HL";                // 0x00F9
        z80OperandType    [OpCode.LD_SP_HL] = OperandType.Implied;
        
        z80InstructionName[OpCode.EX_DE_HL] = "EX DE, HL";                // 0x00EB
        z80OperandType    [OpCode.EX_DE_HL] = OperandType.Implied;
        

        z80InstructionName[OpCode.EX_iSP_HL] = "EX (SP), HL";             // 0x00E3
        z80OperandType    [OpCode.EX_iSP_HL] = OperandType.Implied;

        z80InstructionName[OpCode.EX_iSP_IX] = "EX (SP), IX";             // 0xDDE3
        z80OperandType    [OpCode.EX_iSP_IX] = OperandType.Implied;

        z80InstructionName[OpCode.EX_iSP_IY] = "EX (SP), IY";             // 0xFDE3
        z80OperandType    [OpCode.EX_iSP_IY] = OperandType.Implied;

        z80InstructionName[OpCode.LD_iHL_n] = "LD (HL), n";               // 0x0036
        z80OperandType    [OpCode.LD_iHL_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.LD_iHL_B] = "LD (HL), B";               // 0x0070
        z80OperandType    [OpCode.LD_iHL_B] = OperandType.Implied;

        z80InstructionName[OpCode.LD_iHL_C] = "LD (HL), C";               // 0x0071
        z80OperandType    [OpCode.LD_iHL_C] = OperandType.Implied;

        z80InstructionName[OpCode.LD_iHL_D] = "LD (HL), D";               // 0x0072
        z80OperandType    [OpCode.LD_iHL_D] = OperandType.Implied;

        z80InstructionName[OpCode.LD_iHL_E] = "LD (HL), E";               // 0x0073
        z80OperandType    [OpCode.LD_iHL_E] = OperandType.Implied;

        z80InstructionName[OpCode.LD_iHL_H] = "LD (HL), H";               // 0x0074
        z80OperandType    [OpCode.LD_iHL_H] = OperandType.Implied;

        z80InstructionName[OpCode.LD_iHL_L] = "LD (HL), L";               // 0x0075
        z80OperandType    [OpCode.LD_iHL_L] = OperandType.Implied;

        z80InstructionName[OpCode.LD_iHL_A] = "LD (HL), A";               // 0x0077
        z80OperandType    [OpCode.LD_iHL_A] = OperandType.Implied;

        z80InstructionName[OpCode.INC_DE] = "INC DE";                     // 0x0003
        z80OperandType    [OpCode.INC_DE] = OperandType.Implied;

        z80InstructionName[OpCode.INC_BC] = "INC BC";                     // 0x0013
        z80OperandType    [OpCode.INC_BC] = OperandType.Implied;

        z80InstructionName[OpCode.INC_HL] = "INC HL";                     // 0x0023
        z80OperandType    [OpCode.INC_HL] = OperandType.Implied;

        z80InstructionName[OpCode.INC_IX] = "INC IX";                     // 0xDD23
        z80OperandType    [OpCode.INC_IX] = OperandType.Implied;

        z80InstructionName[OpCode.INC_IY] = "INC IY";                     // 0xFD23
        z80OperandType    [OpCode.INC_IY] = OperandType.Implied;

        z80InstructionName[OpCode.INC_iHL] = "INC (HL)";                  // 0x0034
        z80OperandType    [OpCode.INC_iHL] = OperandType.Implied;
        
        z80InstructionName[OpCode.INC_iIX_d] = "INC (IX+d)";              // 0xDD34
        z80OperandType    [OpCode.INC_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.INC_iIY_d] = "INC (IY+d)";              // 0xFD34
        z80OperandType    [OpCode.INC_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.INC_A] = "INC A";                       // 0x003C
        z80OperandType    [OpCode.INC_A] = OperandType.Implied;

        z80InstructionName[OpCode.INC_B] = "INC B";                       // 0x0004
        z80OperandType    [OpCode.INC_B] = OperandType.Implied;

        z80InstructionName[OpCode.INC_C] = "INC C";                       // 0x000C
        z80OperandType    [OpCode.INC_C] = OperandType.Implied;

        z80InstructionName[OpCode.INC_D] = "INC D";                       // 0x0014
        z80OperandType    [OpCode.INC_D] = OperandType.Implied;

        z80InstructionName[OpCode.INC_E] = "INC E";                       // 0x001C
        z80OperandType    [OpCode.INC_E] = OperandType.Implied;

        z80InstructionName[OpCode.INC_H] = "INC H";                       // 0x0024
        z80OperandType    [OpCode.INC_H] = OperandType.Implied;

        z80InstructionName[OpCode.INC_L] = "INC L";                       // 0x002C
        z80OperandType    [OpCode.INC_L] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_A] = "DEC A";                       // 0x003D
        z80OperandType    [OpCode.DEC_A] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_B] = "DEC B";                       // 0x0005
        z80OperandType    [OpCode.DEC_B] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_C] = "DEC C";                       // 0x000D
        z80OperandType    [OpCode.DEC_C] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_D] = "DEC D";                       // 0x0015
        z80OperandType    [OpCode.DEC_D] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_E] = "DEC E";                       // 0x001D
        z80OperandType    [OpCode.DEC_E] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_H] = "DEC H";                       // 0x0025
        z80OperandType    [OpCode.DEC_H] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_L] = "DEC L";                       // 0x002D
        z80OperandType    [OpCode.DEC_L] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_iHL] = "DEC (HL)";                  // 0x0035
        z80OperandType    [OpCode.DEC_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_iIX_d] = "DEC (IX+d)";              // 0xDD35
        z80OperandType    [OpCode.DEC_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.DEC_iIY_d] = "DEC (IY+d)";              // 0xFD35
        z80OperandType    [OpCode.DEC_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.DEC_BC] = "DEC BC";                     // 0x000B
        z80OperandType    [OpCode.DEC_BC] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_HL] = "DEC HL";                     // 0x002B
        z80OperandType    [OpCode.DEC_HL] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_IX] = "DEC IX";                     // 0xDD2B
        z80OperandType    [OpCode.DEC_IX] = OperandType.Implied;

        z80InstructionName[OpCode.DEC_IY] = "DEC IY";                     // 0xFD2B
        z80OperandType    [OpCode.DEC_IY] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_A_B] = "ADD A, B";                  // 0x0080
        z80OperandType    [OpCode.ADD_A_B] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_A_C] = "ADD A, C";                  // 0x0081
        z80OperandType    [OpCode.ADD_A_C] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_A_D] = "ADD A, D";                  // 0x0082
        z80OperandType    [OpCode.ADD_A_D] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_A_E] = "ADD A, E";                  // 0x0083
        z80OperandType    [OpCode.ADD_A_E] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_A_H] = "ADD A, H";                  // 0x0084
        z80OperandType    [OpCode.ADD_A_H] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_A_L] = "ADD A, L";                  // 0x0085
        z80OperandType    [OpCode.ADD_A_L] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_A_A] = "ADD A, A";                  // 0x0087
        z80OperandType    [OpCode.ADD_A_A] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_A_iHL] = "ADD A, (HL)";             // 0x0086
        z80OperandType    [OpCode.ADD_A_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_A_iIX_d] = "ADD A, (IX+d)";         // 0xDD86
        z80OperandType    [OpCode.ADD_A_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.ADD_A_iIY_d] = "ADD A, (IY+d)";         // 0xFD86
        z80OperandType    [OpCode.ADD_A_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.ADD_A_n] = "ADD A, n";                  // 0x00C6
        z80OperandType    [OpCode.ADD_A_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.SUB_A_B] = "SUB A, B";                  // 0x0090
        z80OperandType    [OpCode.SUB_A_B] = OperandType.Implied;

        z80InstructionName[OpCode.SUB_A_C] = "SUB A, C";                  // 0x0091
        z80OperandType    [OpCode.SUB_A_C] = OperandType.Implied;

        z80InstructionName[OpCode.SUB_A_D] = "SUB A, D";                  // 0x0092
        z80OperandType    [OpCode.SUB_A_D] = OperandType.Implied;

        z80InstructionName[OpCode.SUB_A_E] = "SUB A, E";                  // 0x0093
        z80OperandType    [OpCode.SUB_A_E] = OperandType.Implied;

        z80InstructionName[OpCode.SUB_A_H] = "SUB A, H";                  // 0x0094
        z80OperandType    [OpCode.SUB_A_H] = OperandType.Implied;

        z80InstructionName[OpCode.SUB_A_L] = "SUB A, L";                  // 0x0095
        z80OperandType    [OpCode.SUB_A_L] = OperandType.Implied;

        z80InstructionName[OpCode.SUB_A_A] = "SUB A, A";                  // 0x0097
        z80OperandType    [OpCode.SUB_A_A] = OperandType.Implied;

        z80InstructionName[OpCode.SUB_A_iHL] = "SUB A, (HL)";             // 0x0096
        z80OperandType    [OpCode.SUB_A_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SUB_A_iIX_d] = "SUB A, (IX+d)";         // 0xDD96
        z80OperandType    [OpCode.SUB_A_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.SUB_A_iIY_d] = "SUB A, (IY+d)";         // 0xFD96
        z80OperandType    [OpCode.SUB_A_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.SUB_A_n] = "SUB A, n";                  // 0x00D6
        z80OperandType    [OpCode.SUB_A_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.SBC_A_B] = "SBC A, B";                  // 0x0098
        z80OperandType    [OpCode.SBC_A_B] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_A_C] = "SBC A, C";                  // 0x0099
        z80OperandType    [OpCode.SBC_A_C] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_A_D] = "SBC A, D";                  // 0x009A
        z80OperandType    [OpCode.SBC_A_D] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_A_E] = "SBC A, E";                  // 0x009B
        z80OperandType    [OpCode.SBC_A_E] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_A_H] = "SBC A, H";                  // 0x009C
        z80OperandType    [OpCode.SBC_A_H] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_A_L] = "SBC A, L";                  // 0x009D
        z80OperandType    [OpCode.SBC_A_L] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_A_A] = "SBC A, A";                  // 0x009F
        z80OperandType    [OpCode.SBC_A_A] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_A_iHL] = "SBC A, (HL)";             // 0x009E
        z80OperandType    [OpCode.SBC_A_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_A_iIX_d] = "SBC A, (IX+d)";         // 0xDD9E
        z80OperandType    [OpCode.SBC_A_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.SBC_A_iIY_d] = "SBC A, (IY+d)";         // 0xFD9E
        z80OperandType    [OpCode.SBC_A_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.SBC_A_n] = "SBC A, n";                  // 0x00DE
        z80OperandType    [OpCode.SBC_A_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.AND_A_n] = "AND A, n";                  // 0x00E6
        z80OperandType    [OpCode.AND_A_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.AND_A_B] = "AND A, B";                  // 0x00A0
        z80OperandType    [OpCode.AND_A_B] = OperandType.Implied;

        z80InstructionName[OpCode.AND_A_C] = "AND A, C";                  // 0x00A1
        z80OperandType    [OpCode.AND_A_C] = OperandType.Implied;

        z80InstructionName[OpCode.AND_A_D] = "AND A, D";                  // 0x00A2
        z80OperandType    [OpCode.AND_A_D] = OperandType.Implied;

        z80InstructionName[OpCode.AND_A_E] = "AND A, E";                  // 0x00A3
        z80OperandType    [OpCode.AND_A_E] = OperandType.Implied;

        z80InstructionName[OpCode.AND_A_H] = "AND A, H";                  // 0x00A4
        z80OperandType    [OpCode.AND_A_H] = OperandType.Implied;

        z80InstructionName[OpCode.AND_A_L] = "AND A, L";                  // 0x00A5
        z80OperandType    [OpCode.AND_A_L] = OperandType.Implied;

        z80InstructionName[OpCode.AND_A] = "AND A";                       // 0x00A7
        z80OperandType    [OpCode.AND_A] = OperandType.Implied;

        z80InstructionName[OpCode.AND_A_iHL] = "AND A, (HL)";             // 0x00A6
        z80OperandType    [OpCode.AND_A_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.AND_A_iIX_d] = "AND A, (IX+d)";         // 0xDDA6
        z80OperandType    [OpCode.AND_A_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.AND_A_iIY_d] = "AND A, (IY+d)";         // 0xFDA6
        z80OperandType    [OpCode.AND_A_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.XOR_B] = "XOR B";                       // 0x00A8
        z80OperandType    [OpCode.XOR_B] = OperandType.Implied;

        z80InstructionName[OpCode.XOR_C] = "XOR C";                       // 0x00A9
        z80OperandType    [OpCode.XOR_C] = OperandType.Implied;

        z80InstructionName[OpCode.XOR_D] = "XOR D";                       // 0x00AA
        z80OperandType    [OpCode.XOR_D] = OperandType.Implied;

        z80InstructionName[OpCode.XOR_E] = "XOR E";                       // 0x00AB
        z80OperandType    [OpCode.XOR_E] = OperandType.Implied;

        z80InstructionName[OpCode.XOR_H] = "XOR H";                       // 0x00AC
        z80OperandType    [OpCode.XOR_H] = OperandType.Implied;

        z80InstructionName[OpCode.XOR_L] = "XOR L";                       // 0x00AD
        z80OperandType    [OpCode.XOR_L] = OperandType.Implied;

        z80InstructionName[OpCode.XOR_A] = "XOR A";                       // 0x00AF
        z80OperandType    [OpCode.XOR_A] = OperandType.Implied;

        z80InstructionName[OpCode.XOR_A_iHL] = "XOR A, (HL)";             // 0x00AE
        z80OperandType    [OpCode.XOR_A_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.XOR_A_iIX_d] = "XOR A, (IX+d)";         // 0xDDAE
        z80OperandType    [OpCode.XOR_A_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.XOR_A_iIY_d] = "XOR A, (IY+d)";         // 0xFDAE
        z80OperandType    [OpCode.XOR_A_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.OR_A_B] = "OR A, B";                    // 0x00B0
        z80OperandType    [OpCode.OR_A_B] = OperandType.Implied;

        z80InstructionName[OpCode.OR_A_C] = "OR A, C";                    // 0x00B1
        z80OperandType    [OpCode.OR_A_C] = OperandType.Implied;

        z80InstructionName[OpCode.OR_A_D] = "OR A, D";                    // 0x00B2
        z80OperandType    [OpCode.OR_A_D] = OperandType.Implied;

        z80InstructionName[OpCode.OR_A_E] = "OR A, E";                    // 0x00B3
        z80OperandType    [OpCode.OR_A_E] = OperandType.Implied;

        z80InstructionName[OpCode.OR_A_H] = "OR A, H";                    // 0x00B4
        z80OperandType    [OpCode.OR_A_H] = OperandType.Implied;

        z80InstructionName[OpCode.OR_A_L] = "OR A, L";                    // 0x00B5
        z80OperandType    [OpCode.OR_A_L] = OperandType.Implied;

        z80InstructionName[OpCode.OR_A_A] = "OR A, A";                    // 0x00B7
        z80OperandType    [OpCode.OR_A_A] = OperandType.Implied;

        z80InstructionName[OpCode.OR_A_iHL] = "OR A, (HL)";               // 0x00B6
        z80OperandType    [OpCode.OR_A_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.OR_A_iIX_d] = "OR A, (IX+d)";           // 0xDDB6
        z80OperandType    [OpCode.OR_A_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.OR_A_iIY_d] = "OR A, (IY+d)";           // 0xFDB6
        z80OperandType    [OpCode.OR_A_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.CP_A_B] = "CP A, B";                    // 0x00B8
        z80OperandType    [OpCode.CP_A_B] = OperandType.Implied;

        z80InstructionName[OpCode.CP_A_C] = "CP A, C";                    // 0x00B9
        z80OperandType    [OpCode.CP_A_C] = OperandType.Implied;

        z80InstructionName[OpCode.CP_A_D] = "CP A, D";                    // 0x00BA
        z80OperandType    [OpCode.CP_A_D] = OperandType.Implied;

        z80InstructionName[OpCode.CP_A_E] = "CP A, E";                    // 0x00BB
        z80OperandType    [OpCode.CP_A_E] = OperandType.Implied;

        z80InstructionName[OpCode.CP_A_H] = "CP A, H";                    // 0x00BC
        z80OperandType    [OpCode.CP_A_H] = OperandType.Implied;

        z80InstructionName[OpCode.CP_A_L] = "CP A, L";                    // 0x00BD
        z80OperandType    [OpCode.CP_A_L] = OperandType.Implied;

        z80InstructionName[OpCode.CP_A_A] = "CP A, A";                    // 0x00BF
        z80OperandType    [OpCode.CP_A_A] = OperandType.Implied;

        z80InstructionName[OpCode.CP_A_iHL] = "CP A, (HL)";               // 0x00BE
        z80OperandType    [OpCode.CP_A_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.CP_A_iIX_d] = "CP A, (IX+d)";           // 0xDDBE
        z80OperandType    [OpCode.CP_A_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.CP_A_iIY_d] = "CP A, (IY+d)";           // 0xFDBE
        z80OperandType    [OpCode.CP_A_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.ADD_IX_BC] = "ADD IX, BC";              // 0xDD09
        z80OperandType    [OpCode.ADD_IX_BC] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_IX_DE] = "ADD IX, DE";              // 0xDD19
        z80OperandType    [OpCode.ADD_IX_DE] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_IY_BC] = "ADD IY, BC";              // 0xFD09
        z80OperandType    [OpCode.ADD_IY_BC] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_IY_DE] = "ADD IY, DE";              // 0xFD19
        z80OperandType    [OpCode.ADD_IY_DE] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_A_B] = "ADC A, B";                  // 0x0088
        z80OperandType    [OpCode.ADC_A_B] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_A_C] = "ADC A, C";                  // 0x0089
        z80OperandType    [OpCode.ADC_A_C] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_A_D] = "ADC A, D";                  // 0x008A
        z80OperandType    [OpCode.ADC_A_D] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_A_E] = "ADC A, E";                  // 0x008B
        z80OperandType    [OpCode.ADC_A_E] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_A_H] = "ADC A, H";                  // 0x008C
        z80OperandType    [OpCode.ADC_A_H] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_A_L] = "ADC A, L";                  // 0x008D
        z80OperandType    [OpCode.ADC_A_L] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_A_A] = "ADC A, A";                  // 0x008F
        z80OperandType    [OpCode.ADC_A_A] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_A_n] = "ADC A, n";                  // 0x00CE
        z80OperandType    [OpCode.ADC_A_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.ADC_A_iHL] = "ADC A, (HL)";             // 0x008E
        z80OperandType    [OpCode.ADC_A_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_A_iIX_d] = "ADC A, (IX+d)";         // 0xDD8E
        z80OperandType    [OpCode.ADC_A_iIX_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.ADC_A_iIY_d] = "ADC A, (IY+d)";         // 0xFD8E
        z80OperandType    [OpCode.ADC_A_iIY_d] = OperandType.IndexedRelative;

        z80InstructionName[OpCode.CP_A_n] = "CP A, n";                    // 0x00FE
        z80OperandType    [OpCode.CP_A_n] = OperandType.Immediate8;

        z80InstructionName[OpCode.SLA_B] = "SLA B";                       // 0xCB20
        z80OperandType    [OpCode.SLA_B] = OperandType.Implied;

        z80InstructionName[OpCode.SLA_C] = "SLA C";                       // 0xCB21
        z80OperandType    [OpCode.SLA_C] = OperandType.Implied;

        z80InstructionName[OpCode.SLA_D] = "SLA D";                       // 0xCB22
        z80OperandType    [OpCode.SLA_D] = OperandType.Implied;

        z80InstructionName[OpCode.SLA_E] = "SLA E";                       // 0xCB23
        z80OperandType    [OpCode.SLA_E] = OperandType.Implied;

        z80InstructionName[OpCode.SLA_H] = "SLA H";                       // 0xCB24
        z80OperandType    [OpCode.SLA_H] = OperandType.Implied;

        z80InstructionName[OpCode.SLA_L] = "SLA L";                       // 0xCB25
        z80OperandType    [OpCode.SLA_L] = OperandType.Implied;

        z80InstructionName[OpCode.SLA_iHL] = "SLA (HL)";                  // 0xCB26
        z80OperandType    [OpCode.SLA_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SLA_A] = "SLA A";                       // 0xCB27
        z80OperandType    [OpCode.SLA_A] = OperandType.Implied;

        z80InstructionName[OpCode.SRL_B] = "SRL B";                       // 0xCB38
        z80OperandType    [OpCode.SRL_B] = OperandType.Implied;

        z80InstructionName[OpCode.SRL_C] = "SRL C";                       // 0xCB39
        z80OperandType    [OpCode.SRL_C] = OperandType.Implied;

        z80InstructionName[OpCode.SRL_D] = "SRL D";                       // 0xCB3A
        z80OperandType    [OpCode.SRL_D] = OperandType.Implied;

        z80InstructionName[OpCode.SRL_E] = "SRL E";                       // 0xCB3B
        z80OperandType    [OpCode.SRL_E] = OperandType.Implied;

        z80InstructionName[OpCode.SRL_H] = "SRL H";                       // 0xCB3C
        z80OperandType    [OpCode.SRL_H] = OperandType.Implied;

        z80InstructionName[OpCode.SRL_L] = "SRL L";                       // 0xCB3D
        z80OperandType    [OpCode.SRL_L] = OperandType.Implied;

        z80InstructionName[OpCode.SRL_iHL] = "SRL (HL)";                  // 0xCB3E
        z80OperandType    [OpCode.SRL_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.SRL_A] = "SRL A";                       // 0xCB3F
        z80OperandType    [OpCode.SRL_A] = OperandType.Implied;

        z80InstructionName[OpCode.RLC_B] = "RLC B";                       // 0xCB00
        z80OperandType    [OpCode.RLC_B] = OperandType.Implied;

        z80InstructionName[OpCode.RLC_C] = "RLC C";                       // 0xCB01
        z80OperandType    [OpCode.RLC_C] = OperandType.Implied;

        z80InstructionName[OpCode.RLC_D] = "RLC D";                       // 0xCB02
        z80OperandType    [OpCode.RLC_D] = OperandType.Implied;

        z80InstructionName[OpCode.RLC_E] = "RLC E";                       // 0xCB03
        z80OperandType    [OpCode.RLC_E] = OperandType.Implied;

        z80InstructionName[OpCode.RLC_H] = "RLC H";                       // 0xCB04
        z80OperandType    [OpCode.RLC_H] = OperandType.Implied;

        z80InstructionName[OpCode.RLC_L] = "RLC L";                       // 0xCB05
        z80OperandType    [OpCode.RLC_L] = OperandType.Implied;

        z80InstructionName[OpCode.RLC_iHL] = "RLC (HL)";                  // 0xCB06
        z80OperandType    [OpCode.RLC_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.RLC_A] = "RLC A";                       // 0xCB07
        z80OperandType    [OpCode.RLC_A] = OperandType.Implied;

        z80InstructionName[OpCode.RL_B] = "RL B";                         // 0xCB10
        z80OperandType    [OpCode.RL_B] = OperandType.Implied;

        z80InstructionName[OpCode.RL_C] = "RL C";                         // 0xCB11
        z80OperandType    [OpCode.RL_C] = OperandType.Implied;

        z80InstructionName[OpCode.RL_D] = "RL D";                         // 0xCB12
        z80OperandType    [OpCode.RL_D] = OperandType.Implied;

        z80InstructionName[OpCode.RL_E] = "RL E";                         // 0xCB13
        z80OperandType    [OpCode.RL_E] = OperandType.Implied;

        z80InstructionName[OpCode.RL_H] = "RL H";                         // 0xCB14
        z80OperandType    [OpCode.RL_H] = OperandType.Implied;

        z80InstructionName[OpCode.RL_L] = "RL L";                         // 0xCB15
        z80OperandType    [OpCode.RL_L] = OperandType.Implied;

        z80InstructionName[OpCode.RL_iHL] = "RL (HL)";                    // 0xCB16
        z80OperandType    [OpCode.RL_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.RL_A] = "RL A";                         // 0xCB17
        z80OperandType    [OpCode.RL_A] = OperandType.Implied;

        z80InstructionName[OpCode.RR_B] = "RR B";                         // 0xCB18
        z80OperandType    [OpCode.RR_B] = OperandType.Implied;

        z80InstructionName[OpCode.RR_C] = "RR C";                         // 0xCB19
        z80OperandType    [OpCode.RR_C] = OperandType.Implied;

        z80InstructionName[OpCode.RR_D] = "RR D";                         // 0xCB1A
        z80OperandType    [OpCode.RR_D] = OperandType.Implied;

        z80InstructionName[OpCode.RR_E] = "RR E";                         // 0xCB1B
        z80OperandType    [OpCode.RR_E] = OperandType.Implied;

        z80InstructionName[OpCode.RR_H] = "RR H";                         // 0xCB1C
        z80OperandType    [OpCode.RR_H] = OperandType.Implied;

        z80InstructionName[OpCode.RR_L] = "RR L";                         // 0xCB1D
        z80OperandType    [OpCode.RR_L] = OperandType.Implied;

        z80InstructionName[OpCode.RR_iHL] = "RR (HL)";                    // 0xCB1E
        z80OperandType    [OpCode.RR_iHL] = OperandType.Implied;

        z80InstructionName[OpCode.RR_A] = "RR A";                         // 0xCB1F
        z80OperandType    [OpCode.RR_A] = OperandType.Implied;

        z80InstructionName[OpCode.RLA] = "RLA";                           // 0x0017
        z80OperandType    [OpCode.RLA] = OperandType.Implied;

        z80InstructionName[OpCode.RRA] = "RRA";                           // 0x001F
        z80OperandType    [OpCode.RRA] = OperandType.Implied;
        
        z80InstructionName[OpCode.CPL] = "CPL";                           // 0x002F
        z80OperandType    [OpCode.CPL] = OperandType.Implied;

        z80InstructionName[OpCode.CCF] = "CCF";                           // 0x003F
        z80OperandType    [OpCode.CCF] = OperandType.Implied;

        z80InstructionName[OpCode.SCF] = "SCF";                           // 0x0037
        z80OperandType    [OpCode.SCF] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_HL_BC] = "SBC HL, BC";              // 0xED42
        z80OperandType    [OpCode.SBC_HL_BC] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_HL_DE] = "SBC HL, DE";              // 0xED52
        z80OperandType    [OpCode.SBC_HL_DE] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_HL_HL] = "SBC HL, HL";              // 0xED62
        z80OperandType    [OpCode.SBC_HL_HL] = OperandType.Implied;

        z80InstructionName[OpCode.SBC_HL_SP] = "SBC HL, SP";              // 0xED72
        z80OperandType    [OpCode.SBC_HL_SP] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_HL_BC] = "ADD HL, BC";              // 0x0009
        z80OperandType    [OpCode.ADD_HL_BC] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_HL_DE] = "ADD HL, DE";              // 0x0019
        z80OperandType    [OpCode.ADD_HL_DE] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_HL_HL] = "ADD HL, HL";              // 0x0029
        z80OperandType    [OpCode.ADD_HL_HL] = OperandType.Implied;

        z80InstructionName[OpCode.ADD_HL_SP] = "ADD HL, SP";              // 0x0039
        z80OperandType    [OpCode.ADD_HL_SP] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_HL_BC] = "ADC HL, BC";              // 0xED4A
        z80OperandType    [OpCode.ADC_HL_BC] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_HL_DE] = "ADC HL, DE";              // 0xED5A
        z80OperandType    [OpCode.ADC_HL_DE] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_HL_HL] = "ADC HL, HL";              // 0xED6A
        z80OperandType    [OpCode.ADC_HL_HL] = OperandType.Implied;

        z80InstructionName[OpCode.ADC_HL_SP] = "ADC HL, SP";              // 0xED7A
        z80OperandType    [OpCode.ADC_HL_SP] = OperandType.Implied;

        z80InstructionName[OpCode.JP_nn] = "JP nn";                       // 0x00C3
        z80OperandType    [OpCode.JP_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.JP_M_nn] = "JP M, nn";                  // 0x00FA
        z80OperandType    [OpCode.JP_M_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.JP_Z_nn] = "JP Z, nn";                  // 0x00CA
        z80OperandType    [OpCode.JP_Z_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.JP_NZ_nn] = "JP NZ, nn";                // 0x00C2
        z80OperandType    [OpCode.JP_NZ_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.JP_C_nn] = "JP C, nn";                  // 0x00DA
        z80OperandType    [OpCode.JP_C_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.JP_NC_nn] = "JP NC, nn";                // 0x00D2
        z80OperandType    [OpCode.JP_NC_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.JP_P_nn] = "JP P, nn";                  // 0x00F2
        z80OperandType    [OpCode.JP_P_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.JP_PE_nn] = "JP PE, nn";                // 0x00EA
        z80OperandType    [OpCode.JP_PE_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.JP_PO_nn] = "JP PO, nn";                // 0x00E2
        z80OperandType    [OpCode.JP_PO_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.JP_HL] = "JP HL";                       // 0x00E9
        z80OperandType    [OpCode.JP_HL] = OperandType.Implied;

        z80InstructionName[OpCode.JR_NZ_e] = "JR NZ, e";                  // 0x0020
        z80OperandType    [OpCode.JR_NZ_e] = OperandType.Relative;

        z80InstructionName[OpCode.JR_Z_e] = "JR Z, e";                    // 0x0028
        z80OperandType    [OpCode.JR_Z_e] = OperandType.Relative;

        z80InstructionName[OpCode.JR_NC_e] = "JR NC, e";                  // 0x0030
        z80OperandType    [OpCode.JR_NC_e] = OperandType.Relative;

        z80InstructionName[OpCode.JR_C_e] = "JR C, e";                    // 0x0038
        z80OperandType    [OpCode.JR_C_e] = OperandType.Relative;

        z80InstructionName[OpCode.JR_e] = "JR e";                         // 0x0018
        z80OperandType    [OpCode.JR_e] = OperandType.Relative;

        z80InstructionName[OpCode.DJNZ_e] = "DJNZ e";                     // 0x0010
        z80OperandType    [OpCode.DJNZ_e] = OperandType.Relative;

        z80InstructionName[OpCode.CALL_nn] = "CALL nn";                   // 0x00CD
        z80OperandType    [OpCode.CALL_nn] = OperandType.Immediate16;

        z80InstructionName[OpCode.RET] = "RET";                           // 0x00C9
        z80OperandType    [OpCode.RET] = OperandType.Implied;

        z80InstructionName[OpCode.RET_NZ] = "RET NZ";                     // 0x00C0
        z80OperandType    [OpCode.RET_NZ] = OperandType.Implied;

        z80InstructionName[OpCode.RET_Z] = "RET Z";                       // 0x00C8
        z80OperandType    [OpCode.RET_Z] = OperandType.Implied;

        z80InstructionName[OpCode.RET_NC] = "RET NC";                     // 0x00D0
        z80OperandType    [OpCode.RET_NC] = OperandType.Implied;

        z80InstructionName[OpCode.RET_C] = "RET C";                       // 0x00D8
        z80OperandType    [OpCode.RET_C] = OperandType.Implied;

        z80InstructionName[OpCode.RET_PO] = "RET PO";                     // 0x00E0
        z80OperandType    [OpCode.RET_PO] = OperandType.Implied;

        z80InstructionName[OpCode.RET_PE] = "RET PE";                     // 0x00E8
        z80OperandType    [OpCode.RET_PE] = OperandType.Implied;

        z80InstructionName[OpCode.RET_P] = "RET P";                       // 0x00F0
        z80OperandType    [OpCode.RET_P] = OperandType.Implied;

        z80InstructionName[OpCode.RET_M] = "RET M";                       // 0x00F8
        z80OperandType    [OpCode.RET_M] = OperandType.Implied;

        z80InstructionName[OpCode.HALT] = "HALT";                         // 0x0076
        z80OperandType    [OpCode.HALT] = OperandType.Implied;

        z80InstructionName[OpCode.PUSH_IX] = "PUSH IX";                   // 0xDDE5
        z80OperandType    [OpCode.PUSH_IX] = OperandType.Implied;

        z80InstructionName[OpCode.PUSH_IY] = "PUSH IY";                   // 0xFDE5
        z80OperandType    [OpCode.PUSH_IY] = OperandType.Implied;

        z80InstructionName[OpCode.POP_IX] = "POP IX";                     // 0xDDE1
        z80OperandType    [OpCode.POP_IX] = OperandType.Implied;

        z80InstructionName[OpCode.POP_IY] = "POP IY";                     // 0xFDE1
        z80OperandType    [OpCode.POP_IY] = OperandType.Implied;

        z80InstructionName[OpCode.PUSH_BC] = "PUSH BC";                   // 0x00C5
        z80OperandType    [OpCode.PUSH_BC] = OperandType.Implied;

        z80InstructionName[OpCode.PUSH_DE] = "PUSH DE";                   // 0x00D5
        z80OperandType    [OpCode.PUSH_DE] = OperandType.Implied;

        z80InstructionName[OpCode.PUSH_HL] = "PUSH HL";                   // 0x00E5
        z80OperandType    [OpCode.PUSH_HL] = OperandType.Implied;

        z80InstructionName[OpCode.PUSH_AF] = "PUSH AF";                   // 0x00F5
        z80OperandType    [OpCode.PUSH_AF] = OperandType.Implied;

        z80InstructionName[OpCode.POP_BC] = "POP BC";                     // 0x00C1
        z80OperandType    [OpCode.POP_BC] = OperandType.Implied;

        z80InstructionName[OpCode.POP_DE] = "POP DE";                     // 0x00D1
        z80OperandType    [OpCode.POP_DE] = OperandType.Implied;

        z80InstructionName[OpCode.POP_HL] = "POP HL";                     // 0x00E1
        z80OperandType    [OpCode.POP_HL] = OperandType.Implied;

        z80InstructionName[OpCode.POP_AF] = "POP AF";                     // 0x00F1
        z80OperandType    [OpCode.POP_AF] = OperandType.Implied;
    }

    
}
