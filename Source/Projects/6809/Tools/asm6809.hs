unit Asm6809
{
    <string,string> debugInfo;
    <string,string> labelInfo;
    <string,bool> debugInfoLineUsed;
    <byte> currentStream;
    
    <byte> constantStream;
    <string,uint> usedConstants;
    uint romStart;
    
    CPUArchitecture cpuArchitecture;
    CPUArchitecture Architecture { get { return cpuArchitecture; } set { cpuArchitecture = value; } }
    
    uint InvalidAddress { get { return 0xFFFF; } }
    
    enum AddressingMode
    {
        None       = 0,
        Inherent   = 0x0001,
        Immediate  = 0x0002,  // nn        0x34
        Direct     = 0x0008,  // aa        [0x20]
        Indexed    = 0x0010,  // ioooo     [i+32] - index register determined from operand byte
        Extended   = 0x0040,  // aaaa      [0x1234]
        Relative8  = 0x0080,  // oo        +32
        Relative16 = 0x0100,  // oooo      +2660
    }
    

    AddressingMode GetAddressingMode(OpCode opCode)
    {
        AddressingMode result = AddressingMode.None;
        switch (uint(opCode) & 0x0F)
        {
            case 0x00:
            case 0x90:
            case 0xD0:
            {
                result = AddressingMode.Direct;
            }
            case 0x10:
            {
                switch (opCode)
                {
                    case OpCode.LBRA_oooo:
                    case OpCode.LBSR_oooo:
                    {
                        result = AddressingMode.Relative16;
                    }
                    case OpCode.ORCC_nn:
                    case OpCode.EXG_nn:
                    case OpCode.TFR_nn:
                    {
                        result = AddressingMode.Immediate;
                    }
                    default:
                    {
                        result = AddressingMode.Inherent;
                    }
                }
            }
            case 0x40:
            case 0x50:
            {
                result = AddressingMode.Inherent;
            }
            case 0x60:
            case 0xA0:
            case 0xE0:
            {
                result = AddressingMode.Indexed;
            }
            case 0x70:
            case 0xB0:
            case 0xF0:
            {
                result = AddressingMode.Extended;
            }
            default:
            {
                Die(0x0A); // not implemented
            }
        }
        return result;
    }
    

    enum OpCode
    {
        // Direct 0x0_
        NEG_aa = 0x00,          // NEG direct
        COM_aa = 0x03,          // COM direct
        LSR_aa = 0x04,          // LSR direct
        ROR_aa = 0x06,          // ROR direct
        ASR_aa = 0x07,          // ASR direct
        LSL_aa = 0x08,          // LSL direct
        ROL_aa = 0x09,          // ROL direct
        DEC_aa = 0x0A,          // DEC direct
        INC_aa = 0x0C,          // INC direct
        TST_aa = 0x0D,          // TST direct
        JMP_aa = 0x0E,          // JMP direct
        CLR_aa = 0x0F,          // CLR direct
    
        // Inherent 0x1_
        NOP = 0x12,             // NOP inherent
        SYNC = 0x13,            // SYNC inherent
        LBRA_oooo = 0x16,       // LBRA relative 16
        LBSR_oooo = 0x17,       // LBSR relative 16
        DAA = 0x19,             // DAA inherent
        ORCC_nn = 0x1C,         // ORCC immediate
        SEX = 0x1D,             // SEX inherent
        EXG_nn = 0x1E,          // EXG immediate
        TFR_nn = 0x1F,          // TFR immediate
    
        // Inherent 0x4_
        NEGA = 0x40,            // NEGA inherent
        COMA = 0x43,            // COMA inherent
        LSRA = 0x44,            // LSRA inherent
        RORA = 0x46,            // RORA inherent
        ASRA = 0x47,            // ASRA inherent
        LSLA = 0x48,            // LSLA inherent
        ROLA = 0x49,            // ROLA inherent
        DECA = 0x4A,            // DECA inherent
        INCA = 0x4C,            // INCA inherent
        TSTA = 0x4D,            // TSTA inherent
        CLRA = 0x4F,            // CLRA inherent
    
        // Inherent 0x5_
        NEGB = 0x50,            // NEGB inherent
        COMB = 0x53,            // COMB inherent
        LSRB = 0x54,            // LSRB inherent
        RORB = 0x56,            // RORB inherent
        ASRB = 0x57,            // ASRB inherent
        LSLB = 0x58,            // LSLB inherent
        ROLB = 0x59,            // ROLB inherent
        DECB = 0x5A,            // DECB inherent
        INCB = 0x5C,            // INCB inherent
        TSTB = 0x5D,            // TSTB inherent
        CLRB = 0x5F,            // CLRB inherent
    
        // Indexed 0x6_
        NEG_i = 0x60,           // NEG indexed
        COM_i = 0x63,           // COM indexed
        LSR_i = 0x64,           // LSR indexed
        ROR_i = 0x66,           // ROR indexed
        ASR_i = 0x67,           // ASR indexed
        LSL_i = 0x68,           // LSL indexed
        ROL_i = 0x69,           // ROL indexed
        DEC_i = 0x6A,           // DEC indexed
        INC_i = 0x6C,           // INC indexed
        TST_i = 0x6D,           // TST indexed
        JMP_i = 0x6E,           // JMP indexed
        CLR_i = 0x6F,           // CLR indexed
    
        // Extended 0x7_
        NEG_aaaa = 0x70,        // NEG extended
        COM_aaaa = 0x73,        // COM extended
        LSR_aaaa = 0x74,        // LSR extended
        ROR_aaaa = 0x76,        // ROR extended
        ASR_aaaa = 0x77,        // ASR extended
        LSL_aaaa = 0x78,        // LSL extended
        ROL_aaaa = 0x79,        // ROL extended
        DEC_aaaa = 0x7A,        // DEC extended
        INC_aaaa = 0x7C,        // INC extended
        TST_aaaa = 0x7D,        // TST extended
        JMP_aaaa = 0x7E,        // JMP extended
        CLR_aaaa = 0x7F,        // CLR extended
    
        // Immediate 0x8_
        SUBA_nn = 0x80,         // SUBA immediate
        CMPA_nn = 0x81,         // CMPA immediate
        SBCA_nn = 0x82,         // SBCA immediate
        SUBD_nn = 0x83,         // SUBD immediate
        ANDA_nn = 0x84,         // ANDA immediate
        BITA_nn = 0x85,         // BITA immediate
        LDA_nn = 0x86,          // LDA immediate
        EORA_nn = 0x88,         // EORA immediate
        ADCA_nn = 0x89,         // ADCA immediate
        ORAA_nn = 0x8A,         // ORAA immediate
        ADDA_nn = 0x8B,         // ADDA immediate
        CMPX_nn = 0x8C,         // CMPX immediate
        LDX_nn = 0x8E,          // LDX immediate
    
        // Direct 0x9_
        SUBA_aa = 0x90,         // SUBA direct
        CMPA_aa = 0x91,         // CMPA direct
        SBCA_aa = 0x92,         // SBCA direct
        SUBD_aa = 0x93,         // SUBD direct
        ANDA_aa = 0x94,         // ANDA direct
        BITA_aa = 0x95,         // BITA direct
        LDA_aa = 0x96,          // LDA direct
        STA_aa = 0x97,          // STA direct
        EORA_aa = 0x98,         // EORA direct
        ADCA_aa = 0x99,         // ADCA direct
        ORAA_aa = 0x9A,         // ORAA direct
        ADDA_aa = 0x9B,         // ADDA direct
        CMPX_aa = 0x9C,         // CMPX direct
        JSR_aa = 0x9D,          // JSR direct
        LDX_aa = 0x9E,          // LDX direct
        STX_aa = 0x9F,          // STX direct
    
        // Indexed 0xA_
        SUBA_i = 0xA0,          // SUBA indexed
        CMPA_i = 0xA1,          // CMPA indexed
        SBCA_i = 0xA2,          // SBCA indexed
        SUBD_i = 0xA3,          // SUBD indexed
        ANDA_i = 0xA4,          // ANDA indexed
        BITA_i = 0xA5,          // BITA indexed
        LDA_i = 0xA6,           // LDA indexed
        STA_i = 0xA7,           // STA indexed
        EORA_i = 0xA8,          // EORA indexed
        ADCA_i = 0xA9,          // ADCA indexed
        ORAA_i = 0xAA,          // ORAA indexed
        ADDA_i = 0xAB,          // ADDA indexed
        CMPX_i = 0xAC,          // CMPX indexed
        JSR_i = 0xAD,           // JSR indexed
        LDX_i = 0xAE,           // LDX indexed
        STX_i = 0xAF,           // STX indexed
    
        // Extended 0xB_
        SUBA_aaaa = 0xB0,       // SUBA extended
        CMPA_aaaa = 0xB1,       // CMPA extended
        SBCA_aaaa = 0xB2,       // SBCA extended
        SUBD_aaaa = 0xB3,       // SUBD extended
        ANDA_aaaa = 0xB4,       // ANDA extended
        BITA_aaaa = 0xB5,       //BITA extended
        LDA_aaaa = 0xB6,        // LDA extended
        STA_aaaa = 0xB7,        // STA extended
        EORA_aaaa = 0xB8,       // EORA extended
        ADCA_aaaa = 0xB9,       // ADCA extended
        ORAA_aaaa = 0xBA,       // ORAA extended
        ADDA_aaaa = 0xBB,       // ADDA extended
        CMPX_aaaa = 0xBC,       // CMPX extended
        JSR_aaaa = 0xBD,        // JSR extended
        LDX_aaaa = 0xBE,        // LDX extended
        STX_aaaa = 0xBF,        // STX extended
    
        // Immediate 0xC_
        SUBB_nn = 0xC0,         // SUBB immediate
        CMPB_nn = 0xC1,         // CMPB immediate
        SBCB_nn = 0xC2,         // SBCB immediate
        ADDD_nn = 0xC3,         // ADDD immediate
        ANDB_nn = 0xC4,         // ANDB immediate
        BITB_nn = 0xC5,         // BITB immediate
        LDB_nn = 0xC6,          // LDB immediate
        EORB_nn = 0xC8,         // EORB immediate
        ADCB_nn = 0xC9,         // ADCB immediate
        ORAB_nn = 0xCA,         // ORAB immediate
        ADDB_nn = 0xCB,         // ADDB immediate
        LDD_nn = 0xCC,          // LDD immediate
        STD_nn = 0xCD,          // STD immediate
        CMPU_nn = 0xCE,         // CMPU immediate
        STU_nn = 0xCF,          // STU immediate
    
        // Direct 0xD_
        SUBB_aa = 0xD0,         // SUBB direct
        CMPB_aa = 0xD1,         // CMPB direct
        SBCB_aa = 0xD2,         // SBCB direct
        ADDD_aa = 0xD3,         // ADDD direct
        ANDB_aa = 0xD4,         // ANDB direct
        BITB_aa = 0xD5,         // BITB direct
        LDB_aa = 0xD6,          // LDB direct
        STB_aa = 0xD7,          // STB direct
        EORB_aa = 0xD8,         // EORB direct
        ADCB_aa = 0xD9,         // ADCB direct
        ORAB_aa = 0xDA,         // ORAB direct
        ADDB_aa = 0xDB,         // ADDB direct
        LDD_aa = 0xDC,          // LDD direct
        STD_aa = 0xDD,          // STD direct
        LDU_aa = 0xDE,          // LDU direct
        STU_aa = 0xDF,          // STU direct
    
        // Indexed 0xE_
        SUBB_i = 0xE0,          // SUBB indexed
        CMPB_i = 0xE1,          // CMPB indexed
        SBCB_i = 0xE2,          // SBCB indexed
        ADDD_i = 0xE3,          // ADDD indexed
        ANDB_i = 0xE4,          // ANDB indexed
        BITB_i = 0xE5,          // BITB indexed
        LDB_i = 0xE6,           // LDB indexed
        STB_i = 0xE7,           // STB indexed
        EORB_i = 0xE8,          // EORB indexed
        ADCB_i = 0xE9,          // ADCB indexed
        ORAB_i = 0xEA,          // ORAB indexed
        ADDB_i = 0xEB,          // ADDB indexed
        LDD_i = 0xEC,           // LDD indexed
        STD_i = 0xED,           // STD indexed
        LDU_i = 0xEE,           // LDU indexed
        STU_i = 0xEF,           // STU indexed
    
        // Extended 0xF_
        SUBB_aaaa = 0xF0,       // SUBB extended
        CMPB_aaaa = 0xF1,       // CMPB extended
        SBCB_aaaa = 0xF2,       // SBCB extended
        ADDD_aaaa = 0xF3,       // ADDD extended
        ANDB_aaaa = 0xF4,       // ANDB extended
        BITB_aaaa = 0xF5,       // BITB extended
        LDB_aaaa = 0xF6,        // LDB extended
        STB_aaaa = 0xF7,        // STB extended
        EORB_aaaa = 0xF8,       // EORB extended
        ADCB_aaaa = 0xF9,       // ADCB extended
        ORAB_aaaa = 0xFA,       // ORAB extended
        ADDB_aaaa = 0xFB,       // ADDB extended
        LDD_aaaa = 0xFC,        // LDD extended
        STD_aaaa = 0xFD,        // STD extended
        LDU_aaaa = 0xFE,        // LDU extended
        STU_aaaa = 0xFF,        // STU extended
    }
    
    IE()
    {
        Parser.Error("internal error"); Die(0x0B);
    }
    NI()
    {
        Parser.Error("not implemented"); Die(0x0A);
    }
    
    AddressingMode GetAddressingModes(string opCode)
    {
        AddressingMode addressingModes = AddressingMode.None;
        switch (OpCode(uint(opCode) & 0xFF))
        {
            // Group for Direct, Indexed, and Extended addressing modes
            case "NEG":
            case "COM":
            case "LSR":
            case "ROR":
            case "ASR":
            case "LSL":
            case "ROL":
            case "DEC":
            case "INC":
            case "TST":
            case "CLR":
            {
                addressingModes = AddressingMode.Direct
                                | AddressingMode.Indexed
                                | AddressingMode.Extended;
            }
    
            // Special cases for JMP
            case "JMP":
            {
                addressingModes = AddressingMode.Direct
                                | AddressingMode.Indexed
                                | AddressingMode.Extended
                                | AddressingMode.Indirect;
            }
    
            // Group for Relative8 addressing modes
            case "BRA":
            case "BSR":
            case "BEQ":
            case "BNE":
            case "BPL":
            case "BMI":
            case "BCC":
            case "BCS":
            case "BVC":
            case "BVS":
            case "BHI":
            case "BLS":
            case "BGE":
            case "BLT":
            case "BGT":
            case "BLE":
            {
                addressingModes = AddressingMode.Relative8;
            }
    
            // Group for Relative16 addressing modes
            case "LBRA":
            case "LBSR":
            {
                addressingModes = AddressingMode.Relative16;
            }
    
            // Group for Immediate, Direct, Indexed, and Extended addressing modes
            case "SUBA":
            case "CMPA":
            case "SBCA":
            case "ANDA":
            case "BITA":
            case "LDA":
            case "STA":
            case "EORA":
            case "ADCA":
            case "ORAA":
            case "ADDA":
            case "SUBB":
            case "CMPB":
            case "SBCB":
            case "ANDB":
            case "BITB":
            case "LDB":
            case "STB":
            case "EORB":
            case "ADCB":
            case "ORAB":
            case "ADDB":
            case "SUBD":
            case "ADDD":
            case "CMPD":
            case "LDD":
            case "STD":
            case "CMPX":
            case "LDX":
            case "STX":
            case "CMPY":
            case "LDY":
            case "STY":
            case "CMPU":
            case "CMPS":
            case "LDS":
            case "STS":
            case "LDU":
            case "STU":
            {
                addressingModes = AddressingMode.Immediate
                                | AddressingMode.Direct
                                | AddressingMode.Indexed
                                | AddressingMode.Extended;
            }
    
            // Group for Inherent addressing modes
            case "RTS":
            case "RTI":
            case "SWI":
            case "SWI2":
            case "SWI3":
            case "NOP":
            case "SYNC":
            case "SEX":
            case "EXG":
            case "TFR":
            case "MUL":
            {
                addressingModes = AddressingMode.Inherent;
            }
    
            default:
            {
                Die(0x0A); // not implemented
            }
        }
        return addressingModes;
    }
        
    byte GetInstructionLength(OpCode instruction)
    {
        byte length;
        switch (GetAddressingMode(instruction))
        {
            case AddressingMode.Implied:           { length = 1; }
            case AddressingMode.Accumulator:       { length = 1; }
            case AddressingMode.Immediate:         { length = 2; }
            case AddressingMode.Absolute:          { length = 3; }
            case AddressingMode.AbsoluteX:         { length = 3; }
            case AddressingMode.AbsoluteY:         { length = 3; }
            case AddressingMode.AbsoluteIndirect:  { length = 3; }
            case AddressingMode.AbsoluteIndirectX: { length = 3; }
            case AddressingMode.ZeroPage:          { length = 2; }
            case AddressingMode.ZeroPageX:         { length = 2; }
            case AddressingMode.ZeroPageY:         { length = 2; }
            case AddressingMode.ZeroPageIndirect:  { length = 2; }
            case AddressingMode.XIndexedZeroPage:  { length = 2; }
            case AddressingMode.YIndexedZeroPage:  { length = 2; }
            case AddressingMode.Relative:          { length = 2; }
            case AddressingMode.ZeroPageRelative:  { length = 3; }
            default:
            { 
                string name = GetName(instruction); Print("0x" + (uint(instruction)).ToHexString(2) + ":" + name); Die(0x0B); 
            }
        }
        return length;
    }
    
    string GetName(OpCode opCode)
    {
        string name = "???";
        switch (OpCode(uint(opCode) & 0xFF))
        {
            case OpCode.NEG_aa:   { name = "NEG"; }
            case OpCode.COM_aa:   { name = "COM"; }
            case OpCode.LSR_aa:   { name = "LSR"; }
            case OpCode.ROR_aa:   { name = "ROR"; }
            case OpCode.ASR_aa:   { name = "ASR"; }
            case OpCode.LSL_aa:   { name = "LSL"; }
            case OpCode.ROL_aa:   { name = "ROL"; }
            case OpCode.DEC_aa:   { name = "DEC"; }
            case OpCode.INC_aa:   { name = "INC"; }
            case OpCode.TST_aa:   { name = "TST"; }
            case OpCode.JMP_aa:   { name = "JMP"; }
            case OpCode.CLR_aa:   { name = "CLR"; }
            case OpCode.NOP:      { name = "NOP"; }
            case OpCode.SYNC:     { name = "SYNC"; }
            case OpCode.LBRA_oooo:{ name = "LBRA"; }
            case OpCode.LBSR_oooo:{ name = "LBSR"; }
            case OpCode.DAA:      { name = "DAA"; }
            case OpCode.ORCC_nn:  { name = "ORCC"; }
            case OpCode.SEX:      { name = "SEX"; }
            case OpCode.EXG_nn:   { name = "EXG"; }
            case OpCode.TFR_nn:   { name = "TFR"; }
            case OpCode.NEGA:     { name = "NEGA"; }
            case OpCode.COMA:     { name = "COMA"; }
            case OpCode.LSRA:     { name = "LSRA"; }
            case OpCode.RORA:     { name = "RORA"; }
            case OpCode.ASRA:     { name = "ASRA"; }
            case OpCode.LSLA:     { name = "LSLA"; }
            case OpCode.ROLA:     { name = "ROLA"; }
            case OpCode.DECA:     { name = "DECA"; }
            case OpCode.INCA:     { name = "INCA"; }
            case OpCode.TSTA:     { name = "TSTA"; }
            case OpCode.CLRA:     { name = "CLRA"; }
            case OpCode.NEGB:     { name = "NEGB"; }
            case OpCode.COMB:     { name = "COMB"; }
            case OpCode.LSRB:     { name = "LSRB"; }
            case OpCode.RORB:     { name = "RORB"; }
            case OpCode.ASRB:     { name = "ASRB"; }
            case OpCode.LSLB:     { name = "LSLB"; }
            case OpCode.ROLB:     { name = "ROLB"; }
            case OpCode.DECB:     { name = "DECB"; }
            case OpCode.INCB:     { name = "INCB"; }
            case OpCode.TSTB:     { name = "TSTB"; }
            case OpCode.CLRB:     { name = "CLRB"; }
            case OpCode.NEG_i:    { name = "NEG"; }
            case OpCode.COM_i:    { name = "COM"; }
            case OpCode.LSR_i:    { name = "LSR"; }
            case OpCode.ROR_i:    { name = "ROR"; }
            case OpCode.ASR_i:    { name = "ASR"; }
            case OpCode.LSL_i:    { name = "LSL"; }
            case OpCode.ROL_i:    { name = "ROL"; }
            case OpCode.DEC_i:    { name = "DEC"; }
            case OpCode.INC_i:    { name = "INC"; }
            case OpCode.TST_i:    { name = "TST"; }
            case OpCode.JMP_i:    { name = "JMP"; }
            case OpCode.CLR_i:    { name = "CLR"; }
            case OpCode.NEG_aaaa: { name = "NEG"; }
            case OpCode.COM_aaaa: { name = "COM"; }
            case OpCode.LSR_aaaa: { name = "LSR"; }
            case OpCode.ROR_aaaa: { name = "ROR"; }
            case OpCode.ASR_aaaa: { name = "ASR"; }
            case OpCode.LSL_aaaa: { name = "LSL"; }
            case OpCode.ROL_aaaa: { name = "ROL"; }
            case OpCode.DEC_aaaa: { name = "DEC"; }
            case OpCode.INC_aaaa: { name = "INC"; }
            case OpCode.TST_aaaa: { name = "TST"; }
            case OpCode.JMP_aaaa: { name = "JMP"; }
            case OpCode.CLR_aaaa: { name = "CLR"; }
            case OpCode.SUBA_nn:  { name = "SUBA"; }
            case OpCode.CMPA_nn:  { name = "CMPA"; }
            case OpCode.SBCA_nn:  { name = "SBCA"; }
            case OpCode.SUBD_nn:  { name = "SUBD"; }
            case OpCode.ANDA_nn:  { name = "ANDA"; }
            case OpCode.BITA_nn:  { name = "BITA"; }
            case OpCode.LDA_nn:   { name = "LDA"; }
            case OpCode.STA_nn:   { name = "STA"; }
            case OpCode.EORA_nn:  { name = "EORA"; }
            case OpCode.ADCA_nn:  { name = "ADCA"; }
            case OpCode.ORAA_nn:  { name = "ORAA"; }
            case OpCode.ADDA_nn:  { name = "ADDA"; }
            case OpCode.CMPX_nn:  { name = "CMPX"; }
            case OpCode.JSR_nn:   { name = "JSR"; }
            case OpCode.LDX_nn:   { name = "LDX"; }
            case OpCode.STX_nn:   { name = "STX"; }
            case OpCode.SUBA_aa:  { name = "SUBA"; }
            case OpCode.CMPA_aa:  { name = "CMPA"; }
            case OpCode.SBCA_aa:  { name = "SBCA"; }
            case OpCode.SUBD_aa:  { name = "SUBD"; }
            case OpCode.ANDA_aa:  { name = "ANDA"; }
            case OpCode.BITA_aa:  { name = "BITA"; }
            case OpCode.LDA_aa:   { name = "LDA"; }
            case OpCode.STA_aa:   { name = "STA"; }
            case OpCode.EORA_aa:  { name = "EORA"; }
            case OpCode.ADCA_aa:  { name = "ADCA"; }
            case OpCode.ORAA_aa:  { name = "ORAA"; }
            case OpCode.ADDA_aa:  { name = "ADDA"; }
            case OpCode.CMPX_aa:  { name = "CMPX"; }
            case OpCode.JSR_aa:   { name = "JSR"; }
            case OpCode.LDX_aa:   { name = "LDX"; }
            case OpCode.STX_aa:   { name = "STX"; }
            case OpCode.SUBA_i:   { name = "SUBA"; }
            case OpCode.CMPA_i:   { name = "CMPA"; }
            case OpCode.SBCA_i:   { name = "SBCA"; }
            case OpCode.SUBD_i:   { name = "SUBD"; }
            case OpCode.ANDA_i:   { name = "ANDA"; }
            case OpCode.BITA_i:   { name = "BITA"; }
            case OpCode.LDA_i:    { name = "LDA"; }
            case OpCode.STA_i:    { name = "STA"; }
            case OpCode.EORA_i:   { name = "EORA"; }
            case OpCode.ADCA_i:   { name = "ADCA"; }
            case OpCode.ORAA_i:   { name = "ORAA"; }
            case OpCode.ADDA_i:   { name = "ADDA"; }
            case OpCode.CMPX_i:   { name = "CMPX"; }
            case OpCode.JSR_i:    { name = "JSR"; }
            case OpCode.LDX_i:    { name = "LDX"; }
            case OpCode.STX_i:    { name = "STX"; }
            case OpCode.SUBA_aaaa:{ name = "SUBA"; }
            case OpCode.CMPA_aaaa:{ name = "CMPA"; }
            case OpCode.SBCA_aaaa:{ name = "SBCA"; }
            case OpCode.SUBD_aaaa:{ name = "SUBD"; }
            case OpCode.ANDA_aaaa:{ name = "ANDA"; }
            case OpCode.BITA_aaaa:{ name = "BITA"; }
            case OpCode.LDA_aaaa: { name = "LDA"; }
            case OpCode.STA_aaaa: { name = "STA"; }
            case OpCode.EORA_aaaa:{ name = "EORA"; }
            case OpCode.ADCA_aaaa:{ name = "ADCA"; }
            case OpCode.ORAA_aaaa:{ name = "ORAA"; }
            case OpCode.ADDA_aaaa:{ name = "ADDA"; }
            case OpCode.CMPX_aaaa:{ name = "CMPX"; }
            case OpCode.JSR_aaaa: { name = "JSR"; }
            case OpCode.LDX_aaaa: { name = "LDX"; }
            case OpCode.STX_aaaa: { name = "STX"; }
            case OpCode.SUBB_nn:  { name = "SUBB"; }
            case OpCode.CMPB_nn:  { name = "CMPB"; }
            case OpCode.SBCB_nn:  { name = "SBCB"; }
            case OpCode.ADDD_nn:  { name = "ADDD"; }
            case OpCode.ANDB_nn:  { name = "ANDB"; }
            case OpCode.BITB_nn:  { name = "BITB"; }
            case OpCode.LDB_nn:   { name = "LDB"; }
            case OpCode.STB_nn:   { name = "STB"; }
            case OpCode.EORB_nn:  { name = "EORB"; }
            case OpCode.ADCB_nn:  { name = "ADCB"; }
            case OpCode.ORAB_nn:  { name = "ORAB"; }
            case OpCode.ADDB_nn:  { name = "ADDB"; }
            case OpCode.LDD_nn:   { name = "LDD"; }
            case OpCode.STD_nn:   { name = "STD"; }
            case OpCode.CMPU_nn:  { name = "CMPU"; }
            case OpCode.STU_nn:   { name = "STU"; }
            case OpCode.SUBB_aa:  { name = "SUBB"; }
            case OpCode.CMPB_aa:  { name = "CMPB"; }
            case OpCode.SBCB_aa:  { name = "SBCB"; }
            case OpCode.ADDD_aa:  { name = "ADDD"; }
            case OpCode.ANDB_aa:  { name = "ANDB"; }
            case OpCode.BITB_aa:  { name = "BITB"; }
            case OpCode.LDB_aa:   { name = "LDB"; }
            case OpCode.STB_aa:   { name = "STB"; }
            case OpCode.EORB_aa:  { name = "EORB"; }
            case OpCode.ADCB_aa:  { name = "ADCB"; }
            case OpCode.ORAB_aa:  { name = "ORAB"; }
            case OpCode.ADDB_aa:  { name = "ADDB"; }
            case OpCode.LDD_aa:   { name = "LDD"; }
            case OpCode.STD_aa:   { name = "STD"; }
            case OpCode.LDU_aa:   { name = "LDU"; }
            case OpCode.STU_aa:   { name = "STU"; }
            case OpCode.SUBB_i:   { name = "SUBB"; }
            case OpCode.CMPB_i:   { name = "CMPB"; }
            case OpCode.SBCB_i:   { name = "SBCB"; }
            case OpCode.ADDD_i:   { name = "ADDD"; }
            case OpCode.ANDB_i:   { name = "ANDB"; }
            case OpCode.BITB_i:   { name = "BITB"; }
            case OpCode.LDB_i:    { name = "LDB"; }
            case OpCode.STB_i:    { name = "STB"; }
            case OpCode.EORB_i:   { name = "EORB"; }
            case OpCode.ADCB_i:   { name = "ADCB"; }
            case OpCode.ORAB_i:   { name = "ORAB"; }
            case OpCode.ADDB_i:   { name = "ADDB"; }
            case OpCode.LDD_i:    { name = "LDD"; }
            case OpCode.STD_i:    { name = "STD"; }
            case OpCode.LDU_i:    { name = "LDU"; }
            case OpCode.STU_i:    { name = "STU"; }
            case OpCode.SUBB_aaaa:{ name = "SUBB"; }
            case OpCode.CMPB_aaaa:{ name = "CMPB"; }
            case OpCode.SBCB_aaaa:{ name = "SBCB"; }
            case OpCode.ADDD_aaaa:{ name = "ADDD"; }
            case OpCode.ANDB_aaaa:{ name = "ANDB"; }
            case OpCode.BITB_aaaa:{ name = "BITB"; }
            case OpCode.LDB_aaaa: { name = "LDB"; }
            case OpCode.STB_aaaa: { name = "STB"; }
            case OpCode.EORB_aaaa:{ name = "EORB"; }
            case OpCode.ADCB_aaaa:{ name = "ADCB"; }
            case OpCode.ORAB_aaaa:{ name = "ORAB"; }
            case OpCode.ADDB_aaaa:{ name = "ADDB"; }
            case OpCode.LDD_aaaa: { name = "LDD"; }
            case OpCode.STD_aaaa: { name = "STD"; }
            case OpCode.LDU_aaaa: { name = "LDU"; }
            case OpCode.STU_aaaa: { name = "STU"; }
            default:              { Die(0x0A);    }
        }
        return name;
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
        if (Is65uino)
        {
            return OpCode.HLT; // HLT (undocumented lock up)
        }
        else
        {
            return OpCode.STP; // STP (stop)
        }
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
            case "V":  { code = OpCode.BVS_e; }
            case "NV": { code = OpCode.BVC_e; }
            case "MI": { code = OpCode.BMI_e; }
            case "PL": { code = OpCode.BPL_e; }
            case "":   { code = OpCode.BRA_e; }
            default:   { NI();                }
        }
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
            case OpCode.RTI:
            case OpCode.RTS:
            case OpCode.STP:
            case OpCode.HLT:
            case OpCode.iJMP_nn:
            case OpCode.sJMP_inn:
            {
                return true;
            }
        }
        return false;
    }
    
    bool IsJumpInstruction(OpCode instruction, ref AddressingMode addressingMode, ref bool isConditional)
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
    <string,string> LabelInfo { get { return labelInfo; } }
    ClearDebugInfo()
    {
        debugInfo.Clear();
        labelInfo.Clear();
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
    SetOrg(uint org)
    {
        romStart = org;
    }
    uint GetConstantAddress(string name, string value)
    {
        uint address;
        if (usedConstants.Contains(name))
        {
            address = usedConstants[name];
        }
        else
        {
            address = constantStream.Count + romStart;
            usedConstants[name] = address;
            foreach (var c in value)
            {
                constantStream.Append(byte(c));
            }
        }
        return address;
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
    InsertLabel(string label)
    {
        uint na = NextAddress;
        string nextAddress = na.ToString();
        labelInfo[nextAddress] = label;
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
        if (Is65uino)
        {
            Asm6502.EmitInstruction("CLD");       // Clear decimal mode flag
            Asm6502.EmitInstruction("SEI");       // Disable interrupts (may not be necessary with 6507)
            
            // Set stack pointer and clear zero page RAM
            Asm6502.EmitInstruction("LDX", 0x7F); // Load X register with 127 (stack starts from top of memory)
            Asm6502.EmitInstruction("TXS");       // Transfer X to stack pointer
            
            // Clear zero page RAM from $007F to $0000
            Asm6502.EmitInstruction("LDA", 0);
            Asm6502.EmitInstructionZeroPage("STA", 0, AddressingModes.ZeroPageX);
            Asm6502.EmitInstruction("DEX");
            Asm6502.EmitInstruction("BNE", int(-5));
        }
        else
        {
            Asm6502.EmitInstruction("CLD");
            Asm6502.EmitInstruction("LDX", 0xFF);
            Asm6502.EmitInstruction("TXS");
        }
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
        string hexPrefix = "0x";
        string immediatePrefix = "# ";
        if (OGMode)
        {
            hexPrefix = "";
            immediatePrefix = "#";
        }
        disassembly += hexPrefix + address.ToHexString(4);
        disassembly += " ";
        
        disassembly +=  " " + hexPrefix + (byte(instruction)).ToHexString(2);
        disassembly += " ";
        
        uint length = GetInstructionLength(instruction);
        string operandString = "     "; 
        if (length == 2)
        {
            operandString = hexPrefix + operand.ToHexString(2) + "   "; 
            if (!OGMode)
            {
                operandString += "  ";
            }
        }
        else if (length >= 3)
        {
            operandString = hexPrefix + (operand & 0xFF).ToHexString(2) + " " + hexPrefix + (operand >> 8).ToHexString(2); 
        }
        else if (!OGMode)
        {
            operandString += "    ";
        }
        
        disassembly += operandString;
        disassembly += "  ";
        string name = Asm6502.GetName(instruction);
        
        if (OGMode)
        {
            hexPrefix = "$";
        }
        
        AddressingModes addressingMode = Asm6502.GetAddressingMode(instruction);
        switch (addressingMode)
        {
            case AddressingModes.Accumulator:       { disassembly += (name + " A"); }
            case AddressingModes.Implied:           { disassembly += name; }
            case AddressingModes.Immediate:         
            { 
                if ((operand == 0) || (operand == 1))
                {
                    disassembly += (name + " " + immediatePrefix + operand.ToString()); 
                }
                else
                {
                    disassembly += (name + " " + immediatePrefix + hexPrefix + operand.ToHexString(2)); 
                }
            }
            case AddressingModes.Absolute:          { disassembly += (name + " " + hexPrefix + operand.ToHexString(4)); }
            case AddressingModes.AbsoluteX:         { disassembly += (name + " " + hexPrefix + operand.ToHexString(4) + ",X"); }
            case AddressingModes.AbsoluteY:         { disassembly += (name + " " + hexPrefix + operand.ToHexString(4) + ",Y"); }
            case AddressingModes.AbsoluteIndirect:  { disassembly += (name + " [" + hexPrefix + operand.ToHexString(4) + "]"); }
            case AddressingModes.AbsoluteIndirectX: { disassembly += (name + " [" + hexPrefix + operand.ToHexString(4) + ",X]"); }
            case AddressingModes.ZeroPage:          { disassembly += (name + " " + hexPrefix + operand.ToHexString(2)); }
            case AddressingModes.ZeroPageX:         { disassembly += (name + " " + hexPrefix + operand.ToHexString(2) + ",X"); }
            case AddressingModes.ZeroPageY:         { disassembly += (name + " " + hexPrefix + operand.ToHexString(2) + ",Y"); }
            case AddressingModes.ZeroPageIndirect:  { disassembly += (name + " [" + hexPrefix + operand.ToHexString(2) +"]"); }
            case AddressingModes.XIndexedZeroPage:  { disassembly += (name + " [" + hexPrefix + operand.ToHexString(2) +",X]"); }
            case AddressingModes.YIndexedZeroPage:  { disassembly += (name + " [" + hexPrefix + operand.ToHexString(2) +"],Y"); }
            
            case AddressingModes.Relative:  
            { 
                int ioperand = int(operand);
                if (ioperand > 127)
                {
                    ioperand = ioperand - 256; // 0xFF -> -1
                }
                long target = long(address) + length + ioperand;
                disassembly += (name + " " + hexPrefix + target.ToHexString(4) + " (" + (ioperand < 0 ? "" : "+") + ioperand.ToString() + ")"); 
            }
            case AddressingModes.ZeroPageRelative:
            {
                int ioperand = int(operand >> 8);
                if (ioperand > 127)
                {
                    ioperand = ioperand - 256; // 0xFF -> -1
                }
                long target = long(address) + length + ioperand;
                disassembly += (name + " " + hexPrefix + (operand & 0xFF).ToHexString(2) +", " + hexPrefix + target.ToHexString(4) + " (" + (ioperand < 0 ? "" : "+") + ioperand.ToString() + ")"); 
            }
            
            default: { disassembly += name; }
        }
        if (OGMode)
        {
            disassembly = disassembly.Replace("[", "(");
            disassembly = disassembly.Replace("]", ")");
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
