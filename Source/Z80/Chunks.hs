unit Chunks
{
  uses "/Source/System/System"
  uses "/Source/System/Screen"
  
  enum OpCode
  {
    NOP     = 0x00,
    INCBC   = 0x03,   // INC BC
    LDB     = 0x06,   // LD B, n
    DECBC   = 0x0B,   // INC BC
    ADDHLBC = 0x09,   // ADD HL, BC
    DJNZ    = 0x10,   // DJNZ, +e
    LDDE    = 0x11,   // LD DE, nn
    INCDE   = 0x13,   // INC DE
    LDD     = 0x16,   // LD D, n
    RLA     = 0x17,   // RLA
    JR      = 0x18,   // JR +e
    ADDHLDE = 0x19,   // ADD HL, DE
    DECDE   = 0x1B,   // DEC DE
    LDE     = 0x1E,   // LD D, n
    
    JRNZ    = 0x20,   // JR NZ, +e
    
    LDHL    = 0x21,   // LD HL, nn
    LDINDHL = 0x22,   // LD (nn), HL
    INCHL   = 0x23,   // INC HL
    LDH     = 0x26,   // LD H, n
    ADDHLHL = 0x29,   // ADD HL, HL
    
    JRZ     = 0x28,   // JR Z, +e
    
    
    LDHLIND = 0x2A,   // LD HL,(nn)
    DECHL   = 0x2B,   // DEC HL
    
    INCL    = 0x2C,   // INC L
    DECL    = 0x2D,   // DEC L
    LDL     = 0x2E,   // LD L, n
    
    CPL     = 0x2F,   // CPL
    
    JRNC    = 0x30,   // JR NC, +e
    
    INCSP   = 0x33,   // INC SP
    
    INCHLIND = 0x34,  // INC (HL)
    
    LDHLN   = 0x36,   // LD (HL), n
    
    DECSP   = 0x3B,   // DEC SP
    
    INCA    = 0x3C,   // INC A
    DECA    = 0x3D,   // DEC A
    
    LDA     = 0x3E,   // LD A, n
      
    
    LDBC    = 0x41,   // LD B, C
    LDBD    = 0x42,   // LD B, D
    LDBH    = 0x44,   // LD B, H
    
    LDBA    = 0x47,   // LD B, A
    LDCB    = 0x48,   // LD C, B
    
    LDCE    = 0x4B,   // LD C, B
    
    LDCL    = 0x4D,   // LD C, L
    LDCA    = 0x4F,   // LD C, A
    
    LDDH    = 0x54,   // LD D, H
    
    LDDHL   = 0x56,   // LD D, (HL)
    
    LDEL    = 0x5D,   // LD E, L
    
    LDEHL   = 0x5E,   // LD E, (HL)
    
    LDHB    = 0x60,
    
    LDHD    = 0x62,   // LD H, D
    LDHCL   = 0x65,   // LD H, L
    
    LDHHL   = 0x66,    // LD H, (HL)
    
    LDHA    = 0x67,
    LDLC    = 0x69,
    LDLD    = 0x6A,   // LD L, D
    LDLE    = 0x6B,   // LD L, E
    LDLH    = 0x6C,   // LD L, H
    LDLHL   = 0x6E,   // LD L, (HL)
    LDLA    = 0x6F,
    
    LDHLD   = 0x72,   // LD (HL), D
    LDHLE   = 0x73,   // LD (HL), E
    HALT    = 0x76,
    
    LDHLA   = 0x77,   // LD (HL), A
    
    LDAB    = 0x78,   // LD A, B
    LDAC    = 0x79,   // LD A, C
    LDAD    = 0x7A,   // LD A, D
    
    LDAH    = 0x7C,
    LDAL    = 0x7D,
    
    LDAHL   = 0x7E,    // LD A, (HL)
    
    SUBAE   = 0x93,    // SUB A, E
    SUBAL   = 0x95,    // SUB A, L
    
    ANDAD   = 0xA2,
    ANDAE   = 0xA3,
    
    ANDA    = 0xA7,    // AND A, A (clears the carry flag)
    
    XORE    = 0xAB,    // XOR A, E
    
    ORAD    = 0xB2,
    ORAE    = 0xB3,
    ORAL    = 0xB5,
    ORAA    = 0xB7,
    
    POPBC   = 0xC1,
    JPNZ    = 0xC2,
    JP      = 0xC3,
    PUSHBC  = 0xC5,
    RET     = 0xC9,
    JPZ     = 0xCA,
    CALL    = 0xCD,
    
    POPDE   = 0xD1,
    PUSHDE  = 0xD5,
    
    POPHL   = 0xE1,
    PUSHHL  = 0xE5,
    
    XOR     = 0xEE,    // XOR A, n

    UNUSED  = 0xE8,    // RET PO
    JPHL    = 0xE9,    // JP (HL)

    EXDEHL  = 0xEB,    // EX DE, HL

    JPP     = 0xF2,
    JPM     = 0xFA,
    
    RRH      = 0xCB1C, // RR H
    RRL      = 0xCB1D, // RR L
    SLAC     = 0xCB21, // SLA C
    SRLH     = 0xCB3C, // SRL H
    
    LDIXINDN = 0xDD36, // LD (IX+d), n
    
    LDHIXIND = 0xDD66, // LD H, (IX+d)
    LDLIXIND = 0xDD6E, // LD L, (IX+d)
    
    LDDIXIND = 0xDD56, // LD D, (IX+d)
    LDEIXIND = 0xDD5E, // LD E, (IX+d)
    
    LDIXINDD = 0xDD72, // LD (IX+d), D
    LDIXINDE = 0xDD73, // LD (IX+d), E
    LDIXINDH = 0xDD74, // LD (IX+d), H
    LDIXINDL = 0xDD75, // LD (IX+d), L
    
    LDAIXIND = 0xDD7E, // LD A, (IX+d)
    
    LDIX    = 0xDD21,   // LD IX, nn
    ADDIXSP = 0xDD39,   // ADD IX, SP
    
    ORAIXIND = 0xDDB6, // OR A, (IX+d)
    
    POPIX   = 0xDDE1,
    PUSHIX  = 0xDDE5,
    LDSPIX  = 0xDDF9,   // LD SP, IX
    
    ADCHLDE = 0xED5A,   // ADC HL, DE
    LDDEIND = 0xED5B,   // LD DE, (nnnn)
    ADCHLHL = 0xED6A,   // ADC HL, HL
    SBCHLDE = 0xED52,   // SDC HL, DE
    
    INEC    = 0xED58,   // IN E, (C)
    OUTCE   = 0xED59,   // OUT (C), E
    
    LDI     = 0xEDA0,   // LDI (DE), (HL)
    
    LDIY    = 0xFD21,   // LD IX, nn
    
    LDINDIY = 0xFD22,   // LD (nn), IY
    
    INCIY   = 0xFD23,   // INC IY
    DECIY   = 0xFD2B,   // DEC IY
    ADDIYSP = 0xFD39,   // ADD IY, SP
    
    LDDIYIND = 0xFD56, // LD D, (IY+d)
    LDEIYIND = 0xFD5E, // LD E, (IY+d)
    
    LDHIYIND = 0xFD66, // LD H, (IY+d)
    LDLIYIND = 0xFD6E, // LD L, (IY+d)
    
    LDIYINDD = 0xFD72, // LD (IY+d), D
    LDIYINDE = 0xFD73, // LD (IY+d), E
    LDIYINDH = 0xFD74, // LD (IY+d), H
    LDIYINDL = 0xFD75, // LD (IY+d), L
    
    
    
    PUSHIY  = 0xFDE5,   // PUSH IY
    POPIY   = 0xFDE1,
    
  }
  
  <byte> code; // the code
  <string> lines; // the corresponding line in the source for debugging, etc.
  <uint> opCodeAddresses;
  //<byte> opCodeLengths;
  
  <uint> continueAddresses;
  < < uint > > breakAddressPatches;
  
  <string, string> sourceLines;
  
  <OpCode, string> opCodeToStrings;
  
  file zasmFile;
  string previousLocation;
  string previousSource;
  bool previousLocationSet;
  uint peepholeBoundary;
  
  string OpCodeToString(OpCode opCode)
  {
    string result;
    if (opCodeToStrings.Contains(opCode))
    {
        result = opCodeToStrings[opCode];
    }
    return result;
  }
  
  New()
  {
      opCodeToStrings.Clear();
      
      opCodeToStrings[OpCode.UNUSED] = "Undefined";
      
      opCodeToStrings[OpCode.ADCHLDE] = "ADC HL, DE";
      opCodeToStrings[OpCode.ADCHLHL] = "ADC HL, HL";
      
      opCodeToStrings[OpCode.ADDHLDE] = "ADC HL, DE";
      opCodeToStrings[OpCode.ADDHLBC] = "ADC HL, BC";
      opCodeToStrings[OpCode.ADDHLHL] = "ADC HL, HL";
      
      opCodeToStrings[OpCode.ADDIXSP] = "ADC IX, SP";
      opCodeToStrings[OpCode.ADDIYSP] = "ADC IY, SP";
      
      opCodeToStrings[OpCode.ANDA]  = "AND A, A";
      opCodeToStrings[OpCode.ANDAD] = "AND A, D";
      opCodeToStrings[OpCode.ANDAE] = "AND A, E";
      
      opCodeToStrings[OpCode.CALL]  = "CALL nn";

      opCodeToStrings[OpCode.CPL]   = "CPL";
      
      opCodeToStrings[OpCode.DECA]  = "DEC A";
      opCodeToStrings[OpCode.DECL]  = "DEC L";
      
      opCodeToStrings[OpCode.DECBC] = "DEC BC";
      opCodeToStrings[OpCode.DECDE] = "DEC DE";
      opCodeToStrings[OpCode.DECIY] = "DEC IY";
      opCodeToStrings[OpCode.DECHL] = "DEC HL";
      opCodeToStrings[OpCode.DECSP] = "DEC SP";

      opCodeToStrings[OpCode.EXDEHL] = "EX DE, HL";
      
      opCodeToStrings[OpCode.HALT]   = "HALT";
      
      opCodeToStrings[OpCode.INCA]  = "INC A";
      
      opCodeToStrings[OpCode.INCBC] = "INC BC";
      opCodeToStrings[OpCode.INCDE] = "INC DE";
      opCodeToStrings[OpCode.INCHL] = "INC HL";
      opCodeToStrings[OpCode.INCIY] = "INC IY";
      opCodeToStrings[OpCode.INCSP] = "INC SP";
      
      opCodeToStrings[OpCode.INCHLIND] = "INC (HL)";
      
      opCodeToStrings[OpCode.INCL]  = "INC L";

      opCodeToStrings[OpCode.INEC] = "IN E, (C)";
      
      opCodeToStrings[OpCode.JP]   = "JP nn";
      opCodeToStrings[OpCode.JPHL] = "JP (HL)";
      opCodeToStrings[OpCode.JPZ]  = "JP Z nn";
      opCodeToStrings[OpCode.JPNZ] = "JP NZ nn";
      opCodeToStrings[OpCode.JPP]  = "JP P nn";
      opCodeToStrings[OpCode.JPM]  = "JP M nn";
      
      opCodeToStrings[OpCode.JR]   = "JR e";
      opCodeToStrings[OpCode.JRZ]  = "JR Z e";
      opCodeToStrings[OpCode.JRNZ] = "JR NZ e";
      opCodeToStrings[OpCode.JRNC] = "JR NC e";
      opCodeToStrings[OpCode.DJNZ] = "DJNZ e";
      
      opCodeToStrings[OpCode.LDA]   = "LD A, n";
      opCodeToStrings[OpCode.LDB]   = "LD B, n";
      opCodeToStrings[OpCode.LDD]   = "LD D, n";
      opCodeToStrings[OpCode.LDE]   = "LD E, n";
      opCodeToStrings[OpCode.LDH]   = "LD H, n";
      opCodeToStrings[OpCode.LDL]   = "LD L, n";
      
      opCodeToStrings[OpCode.LDAB]  = "LD A, B";
      opCodeToStrings[OpCode.LDAC]  = "LD A, C";
      opCodeToStrings[OpCode.LDAD]  = "LD A, D";
      opCodeToStrings[OpCode.LDAH]  = "LD A, H";
      opCodeToStrings[OpCode.LDAL]  = "LD A, L";
      
      opCodeToStrings[OpCode.LDBA]  = "LD B, A";
      opCodeToStrings[OpCode.LDBC]  = "LD B, C";
      opCodeToStrings[OpCode.LDBD]  = "LD B, D";
      opCodeToStrings[OpCode.LDBH]  = "LD B, H";
      
      opCodeToStrings[OpCode.LDCA]  = "LD C, A";
      opCodeToStrings[OpCode.LDCB]  = "LD C, B";
      opCodeToStrings[OpCode.LDCE]  = "LD C, E";
      opCodeToStrings[OpCode.LDCL]  = "LD C, L";
      
      opCodeToStrings[OpCode.LDDH]  = "LD D, H";
      
      opCodeToStrings[OpCode.LDEL]  = "LD E, L";
      
      opCodeToStrings[OpCode.LDHA]  = "LD H, A";
      opCodeToStrings[OpCode.LDHB]  = "LD H, B";
      opCodeToStrings[OpCode.LDHD]  = "LD H, D";
      opCodeToStrings[OpCode.LDHCL] = "LD H, L";
      
      opCodeToStrings[OpCode.LDLA]  = "LD L, A";
      opCodeToStrings[OpCode.LDLC]  = "LD L, C";
      opCodeToStrings[OpCode.LDLD]  = "LD L, D";
      opCodeToStrings[OpCode.LDLE]  = "LD L, E";
      opCodeToStrings[OpCode.LDLH]  = "LD L, H";
      
      opCodeToStrings[OpCode.LDDE]    = "LD DE, nn";
      opCodeToStrings[OpCode.LDDEIND] = "LD DE, (nn)";
      
      opCodeToStrings[OpCode.LDHL]    = "LD HL, nn";
      
      opCodeToStrings[OpCode.LDIX]     = "LD IX, nn";
      opCodeToStrings[OpCode.LDIY]     = "LD IY, nn";
      
      opCodeToStrings[OpCode.LDINDIY]  = "LD (nn), IY";
      
      opCodeToStrings[OpCode.LDHLIND] = "LD HL, (nn)";
      opCodeToStrings[OpCode.LDINDHL] = "LD (nn), HL";
      
      opCodeToStrings[OpCode.LDAHL] = "LD A, (HL)";
      opCodeToStrings[OpCode.LDDHL] = "LD D, (HL)";
      opCodeToStrings[OpCode.LDEHL] = "LD E, (HL)";
      opCodeToStrings[OpCode.LDHHL] = "LD H, (HL)";
      opCodeToStrings[OpCode.LDLHL] = "LD L, (HL)";

      opCodeToStrings[OpCode.LDHLN] = "LD (HL), n";
      
      opCodeToStrings[OpCode.LDHLA] = "LD (HL), A";
      opCodeToStrings[OpCode.LDHLD] = "LD (HL), D";
      opCodeToStrings[OpCode.LDHLE] = "LD (HL), E";
      
      opCodeToStrings[OpCode.LDAIXIND]  = "LD A, (IXd)";
      opCodeToStrings[OpCode.LDDIXIND]  = "LD D, (IXd)";
      opCodeToStrings[OpCode.LDEIXIND]  = "LD E, (IXd)";
      opCodeToStrings[OpCode.LDHIXIND]  = "LD H, (IXd)";
      opCodeToStrings[OpCode.LDLIXIND]  = "LD L, (IXd)";
      
      opCodeToStrings[OpCode.LDDIYIND]  = "LD D, (IYd)";
      opCodeToStrings[OpCode.LDEIYIND]  = "LD E, (IYd)";
      opCodeToStrings[OpCode.LDHIYIND]  = "LD H, (IYd)";
      opCodeToStrings[OpCode.LDLIYIND]  = "LD L, (IYd)";
      
      opCodeToStrings[OpCode.LDIXINDN]  = "(IXd), n";
      
      opCodeToStrings[OpCode.LDIXINDD]  = "LD (IXd), D";
      opCodeToStrings[OpCode.LDIXINDE]  = "LD (IXd), E";
      opCodeToStrings[OpCode.LDIXINDH]  = "LD (IXd), H";
      opCodeToStrings[OpCode.LDIXINDL]  = "LD (IXd), L";
      
      opCodeToStrings[OpCode.LDIYINDE]  = "LD (IYd), E";
      opCodeToStrings[OpCode.LDIYINDD]  = "LD (IYd), D";
      opCodeToStrings[OpCode.LDIYINDH]  = "LD (IYd), H";
      opCodeToStrings[OpCode.LDIYINDL]  = "LD (IYd), L";
      
      opCodeToStrings[OpCode.LDSPIX]    = "LD SP, IX";
      
      opCodeToStrings[OpCode.LDI]     = "LDI (DE), (HL)"; // Load location [DE] with location [HL], incr DE,HL; decr BC.
      
      opCodeToStrings[OpCode.NOP] = "NOP";
      
      opCodeToStrings[OpCode.ORAA]  = "OR A, A";
      opCodeToStrings[OpCode.ORAD]  = "OR A, D";
      opCodeToStrings[OpCode.ORAE]  = "OR A, E";
      opCodeToStrings[OpCode.ORAL]  = "OR A, L";
      
      opCodeToStrings[OpCode.ORAIXIND] = "OR A, (IXd)";
      
      opCodeToStrings[OpCode.OUTCE] = "OUT (C), E";
      
      opCodeToStrings[OpCode.POPBC]  = "POP BC";
      opCodeToStrings[OpCode.POPDE]  = "POP DE";
      opCodeToStrings[OpCode.POPHL]  = "POP HL";
      opCodeToStrings[OpCode.POPIX]  = "POP IX";
      opCodeToStrings[OpCode.POPIY]  = "POP IY";
      
      opCodeToStrings[OpCode.PUSHBC]  = "PUSH BC";
      opCodeToStrings[OpCode.PUSHDE]  = "PUSH DE";
      opCodeToStrings[OpCode.PUSHHL]  = "PUSH HL";
      opCodeToStrings[OpCode.PUSHIX]  = "PUSH IX";
      opCodeToStrings[OpCode.PUSHIY]  = "PUSH IY";
      
      opCodeToStrings[OpCode.RLA] = "RLA";
      
      opCodeToStrings[OpCode.RRH] = "RR H";
      opCodeToStrings[OpCode.RRL] = "RR L";
      
      opCodeToStrings[OpCode.RET] = "RET";
      
      opCodeToStrings[OpCode.SLAC] = "SLA C";

      opCodeToStrings[OpCode.SRLH] = "SRL H";
      
      opCodeToStrings[OpCode.SUBAE] = "SUB A, E";
      opCodeToStrings[OpCode.SUBAL] = "SUB A, L";
      
      opCodeToStrings[OpCode.SBCHLDE] = "SBC HL, DE";
      
      opCodeToStrings[OpCode.XOR]   = "XOR A, n";
      opCodeToStrings[OpCode.XORE]  = "XOR A, E";
    
      code.Clear();
      lines.Clear();
      opCodeAddresses.Clear();
  }
  
  UpdatePeepholeBoundary()
  {
    peepholeBoundary = code.Length - 1; // instructions at this address and earlier are untouchable
  }
  
  uint GetNextAddress()
  {
    uint nextAddress = code.Length;
    return nextAddress;
  }
  OpCode GetLastOpCode()
  {
    OpCode opCode = OpCode(code[code.Length-1]);
    return opCode;
  }
    OpCode GetOpCode(uint address)
    {
        OpCode opCode = OpCode(code[address]);
        return opCode;
    }
    byte GetCodeByte(uint address)
    {
        return code[address];
    }
    
    SetCodeByte(uint address, byte value)
    {
        code.SetItem(address, value);
    }
  
    PushBreak()
    {
        <uint> addresses;
        breakAddressPatches.Append(addresses);
    }
    
  PatchCall(uint location, uint address)
  {
    uint lsb = (address % 256);
    uint msb = (address / 256);
    code.SetItem(location,   byte(lsb));
    code.SetItem(location+1, byte(msb));
  }
  
  bool PatchBreak(uint address)
  {
    //PrintLn("PatchBreak = " + address.ToHexString(4));
            
    bool result = false;
    uint length = breakAddressPatches.Length;
    if (length > 0)
    {
        <uint> addresses = breakAddressPatches[length-1];
        addresses.Append(address);
        breakAddressPatches.SetItem(length-1, addresses);
        result = true;
    }
    return result;
  }
  
  PatchJumpToHere(uint jumpAddress)
  {
    //PrintLn("PatchJumpToHere = " + jumpAddress.ToHexString(4));
    
    uint hereAddress = code.Length;
    uint lsb = (hereAddress % 256);
    uint msb = (hereAddress / 256);
    code.SetItem(jumpAddress,   byte(lsb));
    code.SetItem(jumpAddress+1, byte(msb));
  }
  
  PopAndPatchBreak(uint breakAddress)
  {
    //PrintLn("PopAndPatchBreak = " + breakAddress.ToHexString(4));
    uint last = breakAddressPatches.Length-1;
    <uint> addresses = breakAddressPatches[last];
    breakAddressPatches.Remove(last);
    foreach (var address in addresses)
    {
        uint lsb = (breakAddress % 256);
        uint msb = (breakAddress / 256);
        code.SetItem(address,   byte(lsb));
        code.SetItem(address+1, byte(msb));
    }
  }
  
  PushContinue(uint continueAddress)
  {
    continueAddresses.Append(continueAddress);
  }
  PopContinue()
  {
    uint last = continueAddresses.Length -1;
    continueAddresses.Remove(last);
  }
  bool GetContinueAddress(ref uint address)
  {
    bool found = false;
    uint last = continueAddresses.Length - 1;
    if (last >= 0)
    {
        address = continueAddresses[last];
        found = true;
    }
    return found;
  }
  PatchChunk(uint address, byte value, <string, string> token)
  {
    PatchChunk(address, value, token["source"], token["line"]);
  }
  PatchChunk(uint address, OpCode opCode, <string, string> token)
  {
    PatchChunk(address, opCode, token["source"], token["line"]);
  }
  PatchChunk(uint address, byte value, string path, string sline)
  {
    code.SetItem(address, value);
    sline = sline.LeftPad('0', 5);
    lines.SetItem(address, path + ":" + sline);
  }
  PatchChunk(uint address, OpCode opCode, string path, string sline)
  {
    byte value = byte(opCode); // does not work with long opcodes
    PatchChunk(address, value, path, sline);
  }
  WriteChunk(byte value, <string, string> token)
  {
    WriteChunk(value, token["source"], token["line"]);
  }
  WriteChunk(OpCode opCode, <string, string> token)
  {
    WriteChunk(opCode, token["source"], token["line"]);
  }
  WriteChunk(byte value, string path, string sline)
  {
    code.Append(value);
    sline = sline.LeftPad('0', 5);
    lines.Append(path + ":" + sline);
  }
  WriteChunk(OpCode opCode, string path, string sline)
  {
    WriteChunk(opCode, path, sline, true);
  }
  WriteChunk(OpCode opCode, string path, string sline, bool peepHoleOk)
  {
    uint instruction = uint(opCode);
    
    uint lsb = (instruction % 256);
    uint msb = (instruction / 256);

    opCodeAddresses.Append(code.Length);
    
    sline = sline.LeftPad('0', 5);
    uint opLength = 1;
    if (msb != 0)
    {
        byte bmsb = byte(msb);
        code.Append(bmsb);
        lines.Append(path + ":" + sline);
        opLength = 2;
    }
        
    byte blsb = byte(lsb);
    code.Append(blsb);
    lines.Append(path + ":" + sline);
    
    if (peepHoleOk)
    {
        PeepholeOptimizer();
    }
  }
  OpCode BuildOpCode(uint address)
  {
    long instruction = code[address];
    if ((instruction == 0xCB) || (instruction == 0xDD) || (instruction == 0xED) || (instruction == 0xFD)) // prefix
    {
        address++;
        instruction = instruction * 256 + code[address];
    }
    OpCode opCode = OpCode(instruction);
    return opCode;
  }
  PeepholeOptimizer()
  {
    loop
    {
        uint addressesLength = opCodeAddresses.Length;
        if (addressesLength < 4)
        {
            break;
        }
        uint prev3Address = opCodeAddresses[addressesLength-4];
        uint prev2Address = opCodeAddresses[addressesLength-3];
        uint prev1Address = opCodeAddresses[addressesLength-2];
        uint prev0Address = opCodeAddresses[addressesLength-1];
        OpCode prev1Op     = BuildOpCode(prev1Address);
        OpCode prev0Op     = BuildOpCode(prev0Address);
        
        if ((prev1Address > peepholeBoundary))
        {
            if (false && (prev1Op == OpCode.PUSHHL) && (prev0Op == OpCode.POPHL))
            {
                // this breaks '<=' comparison, more specifically, the subsequent boolean check after the INCL
                code.Remove(code.Length-1);
                code.Remove(code.Length-1);
                lines.Remove(lines.Length-1);
                lines.Remove(lines.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                
                continue; // try for another
            }
            if ((prev1Op == OpCode.PUSHDE) && (prev0Op == OpCode.POPDE))
            {
                code.Remove(code.Length-1);
                code.Remove(code.Length-1);
                lines.Remove(lines.Length-1);
                lines.Remove(lines.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                
                continue; // try for another
            }
            
            if (   (prev1Op == OpCode.PUSHHL)
                && (prev0Op == OpCode.POPBC)
               )
            {
                code.SetItem(prev1Address, byte(OpCode.LDCL));
                code.SetItem(prev0Address, byte(OpCode.LDBH));
                continue;
            }
            if (   (prev1Op == OpCode.PUSHDE)
                && (prev0Op == OpCode.POPBC)
               )
            {
                code.SetItem(prev1Address, byte(OpCode.LDCE));
                code.SetItem(prev0Address, byte(OpCode.LDBD));
                continue;
            }
            
            
            //if (   (prev1Op == OpCode.PUSHBC)
            //    && (prev0Op == OpCode.POPHL)
            //  )
            //{
            //    code.SetItem(prev1Address, byte(OpCode.LDLC));
            //    code.SetItem(prev0Address, byte(OpCode.LDHB));
            //    continue;
            //}
            //if (   (prev1Op == OpCode.PUSHHL)
            //    && (prev0Op == OpCode.POPDE)
            //    )
            //{
            //    code.SetItem(prev1Address, byte(OpCode.LDEL));
            //    code.SetItem(prev0Address, byte(OpCode.LDDH));
            //    continue;
            //}
            //if (   (prev1Op == OpCode.PUSHDE)
            //    && (prev0Op == OpCode.POPHL)
            //    )
            //{
            //    code.SetItem(prev1Address, byte(OpCode.LDLE));
            //    code.SetItem(prev0Address, byte(OpCode.LDHD));
            //    continue;
            //}
        }
        if (prev2Address > peepholeBoundary)
        {
            OpCode prev2Op     = BuildOpCode(prev2Address);
            
            if (   (prev2Op == OpCode.PUSHBC)
                && (prev1Op == OpCode.POPHL)
                && (prev0Op == OpCode.INCHL)
                )
            {
                code.SetItem(prev2Address, byte(OpCode.LDLC));
                code.SetItem(prev1Address, byte(OpCode.LDHB));

                continue;
            }
            if ((prev2Op == OpCode.PUSHHL) && (prev1Op == OpCode.LDDE) && (prev0Op == OpCode.POPHL))
            {
                // Replace:
                // PUSH HL
                // LD DE, nn - 3 bytes
                // POP HL
                //
                // With:
                // LD DE, nn
                
                // Remove PUSH HL
                code.Remove(prev2Address);
                lines.Remove(prev2Address);
                opCodeAddresses.Remove(opCodeAddresses.Length-3);
                
                // Remove POP HL
                code.Remove(code.Length-1);
                lines.Remove(lines.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                
                // change of address for LD DE, nn
                opCodeAddresses.SetItem(opCodeAddresses.Length-1, prev2Address);
                
                continue; // try for another
            }
        }
        if (prev3Address > peepholeBoundary)
        {
            OpCode prev3Op     = BuildOpCode(prev3Address);
            OpCode prev2Op     = BuildOpCode(prev2Address);
            
            if ((prev3Op == OpCode.LDHLIND) && (prev2Op == OpCode.PUSHHL) && (prev1Op == OpCode.LDHLIND) && (prev0Op == OpCode.POPDE))
            {
                // Replace:
                // LD HL, (nn) - 3 bytes
                // PUSH HL
                // LD HL, (mm)
                // POP DE
                //
                // With:
                // LD DE, (nn) - 4 bytes (0xFD 0x4B, 0xnn, 0xnn,
                // LD HL, (mm)
                
                code.SetItem(prev3Address+3, code[prev3Address+2]);
                code.SetItem(prev3Address+2, code[prev3Address+1]);
                code.SetItem(prev3Address+1, 0x5B);
                code.SetItem(prev3Address, 0xED);
                
                code.Remove(code.Length-1);
                lines.Remove(lines.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1); // POP DE
                opCodeAddresses.Remove(opCodeAddresses.Length-2); // address of PUSH HL
                
                continue; // try for another
            }
            
            if ((prev3Op == OpCode.LDEHL) 
             && (prev2Op == OpCode.LDD) 
             && (prev1Op == OpCode.PUSHDE) 
             && (prev0Op == OpCode.POPHL))
            {
                // Replace:
                // LD E, (HL)
                // LD D, 00 - 2 bytes
                // PUSH DE
                // POP HL
                //
                // With:
                // LD L, (HL)
                // LD H, 00 - 2bytes
                
                code.SetItem(prev3Address, byte(OpCode.LDLHL));
                code.SetItem(prev2Address, byte(OpCode.LDH));
                
                code.Remove(code.Length-1);
                lines.Remove(lines.Length-1);
                code.Remove(code.Length-1);
                lines.Remove(lines.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                
                continue; // try for another
            }

            if ((prev3Op == OpCode.PUSHHL) && (prev2Op == OpCode.LDLC) && (prev1Op == OpCode.LDHB) && (prev0Op == OpCode.POPDE))
            {
                // Replace:
                // PUSH HL
                // LD L, C
                // LD H, B
                // POP DE
                //
                // With:
                // EX DE, HL
                // LD L, C
                // LD H, B
                
                code.SetItem(prev3Address, byte(OpCode.EXDEHL));
                code.Remove(code.Length-1);
                lines.Remove(lines.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                continue; // try for another
            }
            if ((prev3Op == OpCode.LDHL) && (prev2Op == OpCode.PUSHHL) && (prev1Op == OpCode.LDHL))
            {
                // Replace:
                // LD HL nnnn
                // PUSH HL
                // LD HL nnnn
                // XX
                //
                // With:
                // LD HL nnnn
                // PUSH HL
                // XX
                
                uint operand3 = code[prev3Address+1] + (code[prev3Address+2] << 8);
                uint operand1 = code[prev1Address+1] + (code[prev1Address+2] << 8);
                if (operand3 == operand1)
                {
                    // remove prev1Op
                    code.Remove(prev1Address);
                    code.Remove(prev1Address);
                    code.Remove(prev1Address);
                    lines.Remove(prev1Address);
                    lines.Remove(prev1Address);
                    lines.Remove(prev1Address);
                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                    
                    continue; // try for another
                }
            }
            
            if ((prev1Op == OpCode.LDDE) && (prev0Op == OpCode.ADDHLDE))
            {
                // Replace:
                //
                // LD DE nnnn
                // ADD HL, DE
                
                uint operand2 = code[prev1Address+1] + (code[prev1Address+2] << 8);
                if (operand2 == 1)
                {
                    // With:
                    //
                    // INC HL
                    
                    // remove 3 bytes
                    code.Remove(prev1Address);
                    code.Remove(prev1Address);
                    code.Remove(prev1Address);
                    lines.Remove(prev1Address);
                    lines.Remove(prev1Address);
                    lines.Remove(prev1Address);
                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                    
                    code.SetItem(prev1Address, byte(OpCode.INCHL));
                    
                    opCodeAddresses.SetItem(opCodeAddresses.Length-1, code.Length-1);
                    
                    continue; // try for another
                }
                else if (operand2 == 2)
                {
                    // With:
                    //
                    // INC HL
                    // INC HL
                    
                    // remove 3 bytes
                    code.Remove(prev1Address);
                    code.Remove(prev1Address);
                    lines.Remove(prev1Address);
                    lines.Remove(prev1Address);
                    
                    code.SetItem(prev1Address, byte(OpCode.INCHL));
                    code.SetItem(prev1Address+1, byte(OpCode.INCHL));
                    
                    opCodeAddresses.SetItem(opCodeAddresses.Length-2, code.Length-2);
                    opCodeAddresses.SetItem(opCodeAddresses.Length-1, code.Length-1);
                    
                    continue; // try for another
                }
            }
            if ((prev3Op == OpCode.PUSHIY) && 
                (prev2Op == OpCode.POPHL) && 
                (prev1Op == OpCode.LDINDHL)
                )
            {
                
                // Replace:
                //
                // PUSH IY - 2 bytes
                // POP HL
                // LD (FFF6), HL - 3 bytes
                // XX
                //
                // With:
                //
                // LD (FFF6), IY - 4 bytes
                // XX
                
                byte msb = code[prev1Address+1];
                byte lsb = code[prev1Address+2];
                
                code.Remove(prev3Address);
                lines.Remove(prev3Address);
                code.Remove(prev3Address);
                lines.Remove(prev3Address);
                opCodeAddresses.Remove(opCodeAddresses.Length-2);
                opCodeAddresses.Remove(opCodeAddresses.Length-3);
                
                opCodeAddresses.SetItem(opCodeAddresses.Length-2, prev3Address);
                opCodeAddresses.SetItem(opCodeAddresses.Length-1, prev3Address+4);
                
                code.SetItem(prev3Address, 0xFD);
                code.SetItem(prev3Address+1, 0x22);
                code.SetItem(prev3Address+2, msb);
                code.SetItem(prev3Address+3, lsb);
                
                continue; // try for another
            }
            
            
        }
        
        if (addressesLength > 6)
        {
            uint prev4Address = opCodeAddresses[addressesLength-5];
            if (prev4Address > peepholeBoundary)
            {
                OpCode prev4Op     = BuildOpCode(prev4Address);
                OpCode prev3Op     = BuildOpCode(prev3Address);
                OpCode prev2Op     = BuildOpCode(prev2Address);
            
                if ((prev4Op == OpCode.INCHL) && 
                    (prev3Op == OpCode.LDINDHL) && 
                    (prev2Op == OpCode.LDHLIND) && 
                    (prev1Op == OpCode.INCHL) && 
                    (prev0Op == OpCode.LDINDHL) && 
                    (prev0Address == code.Length-3) // operands must exist
                    )
                {
                    // Replace:
                    //
                    // INC HL        - 1 byte  
                    // LD (FFF8), HL - 3 bytes
                    // LD HL, (FFF8) - 3 bytes
                    // INC HL        - 1 byte  
                    // LD (FFF8), HL - 3 bytes
                    //
                    // With:
                    //
                    // INC HL
                    // INC HL
                    // LD (FFF8), HL
                    
                    uint operand3 = code[prev3Address+1] + (code[prev3Address+2] << 8);
                    uint operand2 = code[prev2Address+1] + (code[prev2Address+2] << 8);
                    uint operand0 = code[prev0Address+1] + (code[prev0Address+2] << 8);
                    if ((operand3 == operand2) && (operand3 == operand0))
                    {
                        // remove prev2Op
                        code.Remove(code.Length-5);
                        code.Remove(code.Length-5);
                        code.Remove(code.Length-5);
                        lines.Remove(lines.Length-5);
                        lines.Remove(lines.Length-5);
                        lines.Remove(lines.Length-5);
                        opCodeAddresses.Remove(opCodeAddresses.Length-3);
                        
                        // remove prev3Op
                        code.Remove(code.Length-5);
                        code.Remove(code.Length-5);
                        code.Remove(code.Length-5);
                        lines.Remove(lines.Length-5);
                        lines.Remove(lines.Length-5);
                        lines.Remove(lines.Length-5);
                        opCodeAddresses.Remove(opCodeAddresses.Length-3);
                        
                        // adjust the addresses of the last two instructions
                        opCodeAddresses.SetItem(opCodeAddresses.Length-2, code.Length-4);
                        opCodeAddresses.SetItem(opCodeAddresses.Length-1, code.Length-3);
                        
                        continue; // try for another
                    }
                }
                if ((prev4Op == OpCode.DECHL) && 
                     (prev3Op == OpCode.LDINDHL) && 
                     (prev2Op == OpCode.LDHLIND) && 
                     (prev1Op == OpCode.DECHL) && 
                     (prev0Op == OpCode.LDINDHL) && 
                     (prev0Address == code.Length-3) // operands must exist
                    )
                {
                    // Replace:
                    //
                    // DEC HL        - 1 byte  
                    // LD (FFF8), HL - 3 bytes
                    // LD HL, (FFF8) - 3 bytes
                    // DEC HL        - 1 byte  
                    // LD (FFF8), HL - 3 bytes
                    //
                    // With:
                    //
                    // DEC HL
                    // DEC HL
                    // LD (FFF8), HL
                    
                    uint operand3 = code[prev3Address+1] + (code[prev3Address+2] << 8);
                    uint operand2 = code[prev2Address+1] + (code[prev2Address+2] << 8);
                    uint operand0 = code[prev0Address+1] + (code[prev0Address+2] << 8);
                    if ((operand3 == operand2) && (operand3 == operand0))
                    {
                        // remove prev2Op
                        code.Remove(code.Length-5);
                        code.Remove(code.Length-5);
                        code.Remove(code.Length-5);
                        lines.Remove(lines.Length-5);
                        lines.Remove(lines.Length-5);
                        lines.Remove(lines.Length-5);
                        opCodeAddresses.Remove(opCodeAddresses.Length-3);
                        
                        // remove prev3Op
                        code.Remove(code.Length-5);
                        code.Remove(code.Length-5);
                        code.Remove(code.Length-5);
                        lines.Remove(lines.Length-5);
                        lines.Remove(lines.Length-5);
                        lines.Remove(lines.Length-5);
                        opCodeAddresses.Remove(opCodeAddresses.Length-3);
                        
                        // adjust the addresses of the last two instructions
                        opCodeAddresses.SetItem(opCodeAddresses.Length-2, code.Length-4);
                        opCodeAddresses.SetItem(opCodeAddresses.Length-1, code.Length-3);
                        
                        continue; // try for another
                    }
                }
                if ((prev4Op == OpCode.LDHL) && (prev3Op == OpCode.PUSHHL)&& (prev2Op == OpCode.PUSHHL) && (prev1Op == OpCode.LDHL))
                {
                    // Replace:
                    // LD HL nnnn
                    // PUSH HL
                    // PUSH HL
                    // LD HL nnnn
                    // XX
                    //
                    // With:
                    // LD HL nnnn
                    // PUSH HL
                    // PUSH HL
                    // XX
                    
                    uint operand4 = code[prev4Address+1] + (code[prev4Address+2] << 8);
                    uint operand1 = code[prev1Address+1] + (code[prev1Address+2] << 8);
                    if (operand4 == operand1)
                    {
                        // remove prev1Op
                        code.Remove(prev1Address);
                        code.Remove(prev1Address);
                        code.Remove(prev1Address);
                        lines.Remove(prev1Address);
                        lines.Remove(prev1Address);
                        lines.Remove(prev1Address);
                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                        
                        continue; // try for another
                    }
                }
                if ((prev4Op == OpCode.LDLIXIND) && (prev3Op == OpCode.LDHIXIND)&& (prev2Op == OpCode.LDLH) && (prev1Op == OpCode.LDH))
                {
                    uint op1 = code[prev1Address+1];
                    uint op2 = code[prev2Address+1];
                    if (op1 == 0)
                    {
                        // Replace:
                        // LD L, (IX-10)
                        // LD H, (IX-9)
                        // LD L, H
                        // LD H, 00
                        // XX
                        //
                        // With:
                        // LD L, (IX-9)
                        // LD H, 00
                        // XX
                    
                        code.SetItem(prev3Address+1, 0x6E);
                        
                        code.Remove(prev2Address);
                        lines.Remove(prev2Address);
                        
                        code.Remove(prev4Address);
                        lines.Remove(prev4Address);
                        code.Remove(prev4Address);
                        lines.Remove(prev4Address);
                        code.Remove(prev4Address);
                        lines.Remove(prev4Address);
                        
                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                        
                        opCodeAddresses.SetItem(opCodeAddresses.Length-1, opCodeAddresses[opCodeAddresses.Length-1]-1);
                        
                        continue; // try for another
                    }
                }
                if ((prev4Op == OpCode.LDLIXIND) && (prev3Op == OpCode.LDHIXIND)&& (prev2Op == OpCode.LDHCL) && (prev1Op == OpCode.LDL))
                {
                    uint op1 = code[prev1Address+1];
                    
                    if (op1 == 0)
                    {
                        // Replace:
                        // LD L, (IX-18)
                        // LD H, (IX-17)
                        // LD H, L
                        // LD L, 00
                        // XX
                        //
                        // With:
                        // LD H, (IX-18)
                        // LD L, 00
                        // XX                    

                        code.SetItem(prev4Address+1, 0x66);
                        
                        code.Remove(prev2Address);
                        lines.Remove(prev2Address);
                        
                        code.Remove(prev3Address);
                        lines.Remove(prev3Address);
                        code.Remove(prev3Address);
                        lines.Remove(prev3Address);
                        code.Remove(prev3Address);
                        lines.Remove(prev3Address);
                        
                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                        
                        opCodeAddresses.SetItem(opCodeAddresses.Length-1, opCodeAddresses[opCodeAddresses.Length-1]-1);
                        
                        continue; // try for another
                    }
                }
                if ((prev3Op == OpCode.LDD) && (prev2Op == OpCode.LDEHL)&& (prev1Op == OpCode.PUSHDE) && (prev0Op == OpCode.POPHL))
                {
                    uint op1 = code[prev3Address+1];
                    if (op1 == 0)
                    {
                        // Replace:
                        // LD D, 00
                        // LD E, (HL)
                        // PUSH DE
                        // POP HL                    
                        //
                        // With:
                        // LD L, (HL)
                        // LD H, 00
                        
                        code.SetItem(prev3Address,   0x6E);
                        code.SetItem(prev3Address+1, 0x26);
                        code.SetItem(prev3Address+2, 0x00);
                        
                        code.Remove(prev1Address);
                        lines.Remove(prev1Address);
                        code.Remove(prev1Address);
                        lines.Remove(prev1Address);
                        
                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                        
                        opCodeAddresses.SetItem(opCodeAddresses.Length-1, opCodeAddresses[opCodeAddresses.Length-1]-1);
                        
                        continue; // try for another
                    }
                }
                if ((prev4Op == OpCode.LDLIXIND) && 
                    (prev3Op == OpCode.LDHIXIND) && 
                    (prev2Op == OpCode.PUSHHL)&& 
                    (prev1Op == OpCode.POPDE) && 
                    (prev0Op == OpCode.ADDHLDE))
                {
                    // Replace:
                    //
                    // LD L, (IX-8) - 3 bytes
                    // LD H, (IX-7) - 3 bytes
                    // PUSH HL
                    // POP DE
                    // ADD HL, DE

                    // With:
                    //
                    // LD L, (IX-8) - 3 bytes
                    // LD H, (IX-7) - 3 bytes
                    // ADD HL, HL
                    
                    code.Remove(prev2Address);
                    lines.Remove(prev2Address);
                    code.Remove(prev2Address);
                    lines.Remove(prev2Address);
                    
                    code.SetItem(code.Length-1, byte(OpCode.ADDHLHL));
                    
                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                    
                    continue; // try for another
                }
                if ((prev4Op == OpCode.PUSHHL) && 
                    (prev3Op == OpCode.POPDE) && 
                    (prev2Op == OpCode.LDIYINDE)&& 
                    (prev1Op == OpCode.LDIYINDD))
                {
                    // Replace:
                    //
                    // PUSH HL
                    // POP DE
                    // LD (IY-2), E
                    // LD (IY-1), D
                    // XX

                    // With:
                    //
                    // PUSH HL
                    // POP HL
                    // LD (IY-2), L
                    // LD (IY-1), H
                    // XX
                    
                    code.SetItem(prev3Address, byte(OpCode.POPHL));
                    code.SetItem(prev2Address+1, 0x75);
                    code.SetItem(prev1Address+1, 0x74);
                    
                    continue; // try for another
                }
                
                if ((prev3Op == OpCode.PUSHDE) && 
                    (prev2Op == OpCode.POPHL) && 
                    (prev1Op == OpCode.LDIXINDL)&& 
                    (prev0Op == OpCode.LDIXINDH))
                {
                    // Replace:
                    //
                    // PUSH DE
                    // POP HL
                    // LD (IX-6), L - 3 bytes
                    // LD (IX-5), H - 3 bytes
                    //
                    // With:
                    //
                    // LD (IY-6), E
                    // LD (IY-5), D
                    
                    code.Remove(prev3Address);
                    lines.Remove(prev3Address);
                    code.Remove(prev3Address);
                    lines.Remove(prev3Address);
                    
                    opCodeAddresses.Remove(opCodeAddresses.Length-3);
                    opCodeAddresses.Remove(opCodeAddresses.Length-3);
                    
                    code.SetItem(prev3Address+1, 0x73);
                    code.SetItem(prev3Address+4, 0x72);
                    
                    opCodeAddresses.SetItem(opCodeAddresses.Length-2, opCodeAddresses[opCodeAddresses.Length-2] - 2);
                    opCodeAddresses.SetItem(opCodeAddresses.Length-1, opCodeAddresses[opCodeAddresses.Length-1] - 2);
                    
                    continue; // try for another
                }
                
                if ((prev3Op == OpCode.LDD) && 
                    (prev2Op == OpCode.LDIXINDE) && 
                    (prev1Op == OpCode.LDIXINDD))
                {
                    // Replace:
                    //
                    // LD D, 00     - 2 bytes
                    // LD (IX-6), E - 3 bytes
                    // LD (IX-5), D - 3 bytes
                    // XX
                    //
                    // With:
                    //
                    // LD (IX-6), E - 3 bytes
                    // LD (IX-5), 0 - 4 bytes
                    // XX
                    
                    byte op2 = code[prev2Address+2];
                    byte op1 = code[prev1Address+2];
                    
                    code.Remove(prev3Address);
                    lines.Remove(prev3Address);
                    code.Remove(prev3Address);
                    lines.Remove(prev3Address);
                    opCodeAddresses.Remove(opCodeAddresses.Length-4);
                    opCodeAddresses.SetItem(opCodeAddresses.Length-3, opCodeAddresses[opCodeAddresses.Length-3] - 2);
                    
                    prev1Address = prev1Address - 2;
                    code.SetItem(prev1Address+1, 0x36);
                    code.SetItem(prev1Address+2, op1);
                    code.Insert (prev1Address+3, 0);
                    lines.Insert(prev1Address+3, lines[prev1Address+3]);
                    
                    opCodeAddresses.SetItem(opCodeAddresses.Length-2, opCodeAddresses[opCodeAddresses.Length-2] - 2);
                    opCodeAddresses.SetItem(opCodeAddresses.Length-1, opCodeAddresses[opCodeAddresses.Length-1] - 1);
                    
                    continue; // try for another
                }
                
                if ((prev4Op == OpCode.LDIXINDL) && 
                    (prev3Op == OpCode.LDIXINDH) && 
                    (prev2Op == OpCode.LDLIXIND)&& 
                    (prev1Op == OpCode.LDHIXIND))
                {
                    // Replace:
                    //
                    // LD (IX+4), L
                    // LD (IX+5), H
                    // LD L, (IX+4)
                    // LD H, (IX+5)
                    // XX

                    // With:
                    //
                    // LD (IX+4), L
                    // LD (IX+5), H
                    // XX
                    
                    byte op4 = code[prev4Address+2];
                    byte op3 = code[prev3Address+2];
                    byte op2 = code[prev2Address+2];
                    byte op1 = code[prev1Address+2];
                    
                    if ((op4 == op2) && (op3 == op1))
                    {
                        code.Remove(prev2Address);
                        lines.Remove(prev2Address);
                        code.Remove(prev2Address);
                        lines.Remove(prev2Address);
                        code.Remove(prev2Address);
                        lines.Remove(prev2Address);
                        code.Remove(prev2Address);
                        lines.Remove(prev2Address);
                        code.Remove(prev2Address);
                        lines.Remove(prev2Address);
                        code.Remove(prev2Address);
                        lines.Remove(prev2Address);
                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                        
                        continue; // try for another
                    }
                }
                
                if ((prev3Op == OpCode.PUSHHL) && 
                    (prev2Op == OpCode.POPDE) && 
                    (prev1Op == OpCode.LDIYINDE) &&
                    (prev0Op != OpCode.LDIYINDD))
                {
                    // Replace:
                    //
                    // PUSH HL
                    // POP DE
                    // LD (IY+0), E                    
                    // XX

                    // With:
                    //
                    // PUSH HL
                    // POP HL
                    // LD (IY+0), L
                    // XX
                    
                    code.SetItem(prev2Address, byte(OpCode.POPHL));
                    code.SetItem(prev1Address+1, 0x75);
                    
                    continue; // try for another
                }
                
                
                if (    ((prev4Op == OpCode.PUSHHL) && (prev3Op == OpCode.POPHL))
                     || ((prev4Op == OpCode.PUSHDE) && (prev3Op == OpCode.POPDE))
                   )
                {
                    // Replace:
                    //
                    // PUSH HL
                    // POP HL
                    // XX
                    // XX
                    // XX

                    // With:
                    //
                    // XX
                    // XX
                    // XX
                    
                    code.Remove(prev4Address);
                    lines.Remove(prev4Address);
                    code.Remove(prev4Address);
                    lines.Remove(prev4Address);
                    opCodeAddresses.Remove(opCodeAddresses.Length-4);
                    opCodeAddresses.Remove(opCodeAddresses.Length-4);
                    
                    opCodeAddresses.SetItem(opCodeAddresses.Length-3, opCodeAddresses[opCodeAddresses.Length-3] - 2);
                    opCodeAddresses.SetItem(opCodeAddresses.Length-2, opCodeAddresses[opCodeAddresses.Length-2] - 2);
                    opCodeAddresses.SetItem(opCodeAddresses.Length-1, opCodeAddresses[opCodeAddresses.Length-1] - 2);
                    
                    continue; // try for another
                }
                
                if (    ((prev3Op == OpCode.PUSHHL) && (prev2Op == OpCode.POPHL))
                     || ((prev3Op == OpCode.PUSHDE) && (prev2Op == OpCode.POPDE))
                   )
                {
                    // Replace:
                    //
                    // PUSH HL
                    // POP HL
                    // XX
                    // XX
                    
                    // With:
                    //
                    // XX
                    // XX
                    
                    code.Remove(prev3Address);
                    lines.Remove(prev3Address);
                    code.Remove(prev3Address);
                    lines.Remove(prev3Address);
                    opCodeAddresses.Remove(opCodeAddresses.Length-3);
                    opCodeAddresses.Remove(opCodeAddresses.Length-3);
                    
                    opCodeAddresses.SetItem(opCodeAddresses.Length-2, opCodeAddresses[opCodeAddresses.Length-2] - 2);
                    opCodeAddresses.SetItem(opCodeAddresses.Length-1, opCodeAddresses[opCodeAddresses.Length-1] - 2);
                    
                    continue; // try for another
                }
                
                if (    ((prev2Op == OpCode.PUSHHL) && (prev1Op == OpCode.POPHL))
                     || ((prev2Op == OpCode.PUSHDE) && (prev1Op == OpCode.POPDE))
                   )
                {
                    // Replace:
                    //
                    // PUSH HL
                    // POP HL
                    // XX
                    
                    // With:
                    //
                    // XX
                    
                    code.Remove(prev2Address);
                    lines.Remove(prev2Address);
                    code.Remove(prev2Address);
                    lines.Remove(prev2Address);
                    opCodeAddresses.Remove(opCodeAddresses.Length-2);
                    opCodeAddresses.Remove(opCodeAddresses.Length-2);
                    
                    opCodeAddresses.SetItem(opCodeAddresses.Length-1, opCodeAddresses[opCodeAddresses.Length-1] - 2);
                    
                    continue; // try for another
                }
                
                if ( (prev4Op == OpCode.PUSHIY) && 
                     (prev3Op == OpCode.POPHL) && 
                     (prev2Op == OpCode.LDEHL) && 
                     (prev1Op == OpCode.INCHL) && 
                     (prev0Op == OpCode.LDDHL)
                     )
                {
                    
                    // Replace:
                    //
                    // PUSH IY - 2 bytes
                    // POP HL
                    // LD E, (HL)
                    // INC HL
                    // LD D, (HL)
                    //
                    // With:
                    //
                    // LD E, (IY+0)
                    // LD D, (IY+1)
                    
                    string lastLine = lines[lines.Length-1];
                    <string> sourceLocation = lastLine.Split(':');
                    code.Remove(code.Length-1);
                    lines.Remove(lines.Length-1);
                    code.Remove(code.Length-1);
                    lines.Remove(lines.Length-1);
                    code.Remove(code.Length-1);
                    lines.Remove(lines.Length-1);
                    code.Remove(code.Length-1);
                    lines.Remove(lines.Length-1);
                    code.Remove(code.Length-1);
                    lines.Remove(lines.Length-1);
                    code.Remove(code.Length-1);
                    lines.Remove(lines.Length-1);
                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                    
                    WriteChunk(OpCode.LDEIYIND, sourceLocation[0], sourceLocation[1], false);
                    WriteChunk(0, sourceLocation[0], sourceLocation[1]);
                    WriteChunk(OpCode.LDDIYIND, sourceLocation[0], sourceLocation[1], false);
                    WriteChunk(1, sourceLocation[0], sourceLocation[1]);
                    
                    PeepholeOptimizer();
                    break;
                }
                                
                if (addressesLength > 7)
                {
                    uint prev5Address = opCodeAddresses[addressesLength-6];
                    if (prev5Address > peepholeBoundary)
                    {
                        OpCode prev5Op     = BuildOpCode(prev5Address);
                    
                        if (   (prev2Op == OpCode.PUSHDE)
                            && (prev2Op == OpCode.POPHL)
                            && (prev2Op == OpCode.POPDE)
                            && (prev1Op == OpCode.ADDHLDE)
                            
                            
                            )
                        {
                            //DumpTail(8);
                            //Key key = ReadKey();
                        }
                        
                        if ((prev5Op == OpCode.LDHL) && 
                            (prev4Op == OpCode.PUSHHL) && 
                            (prev3Op == OpCode.PUSHHL) && 
                            (prev2Op == OpCode.PUSHHL) && 
                            (prev1Op == OpCode.LDHL))
                        {
                            // Replace:
                            // LD HL nnnn
                            // PUSH HL
                            // PUSH HL
                            // PUSH HL
                            // LD HL nnnn
                            // XX
                            //
                            // With:
                            // LD HL nnnn
                            // PUSH HL
                            // PUSH HL
                            // PUSH HL
                            // XX
                            
                            uint operand5 = code[prev5Address+1] + (code[prev5Address+2] << 8);
                            uint operand1 = code[prev1Address+1] + (code[prev1Address+2] << 8);
                            if (operand5 == operand1)
                            {
                                // remove prev1Op
                                code.Remove(prev1Address);
                                code.Remove(prev1Address);
                                code.Remove(prev1Address);
                                lines.Remove(prev1Address);
                                lines.Remove(prev1Address);
                                lines.Remove(prev1Address);
                                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                
                                continue; // try for another
                            }
                        }
                        if ((prev5Op == OpCode.PUSHBC) && 
                            (prev4Op == OpCode.LDDE) && 
                            (prev3Op == OpCode.PUSHDE) && 
                            (prev2Op == OpCode.POPHL) && 
                            (prev1Op == OpCode.POPDE) && 
                            (prev0Op == OpCode.ADDHLDE))
                        {
                            // Replace:
                            // PUSH BC
                            // LD DE, 0001
                            // PUSH DE
                            // POP HL
                            // POP DE
                            // ADD HL, DE
                            //
                            // With:
                            //
                            // PUSH BC
                            // POP HL
                            // INC HL
                            
                            uint lsb4 = code[prev4Address+1];
                            uint msb4 = code[prev4Address+2];
                            if ((msb4 == 0) && (lsb4 <= 3))
                            {
                                string lastLine = lines[lines.Length-1];
                                <string> sourceLocation = lastLine.Split(':');
                                
                                code.Remove(prev4Address);
                                lines.Remove(prev4Address);
                                code.Remove(prev4Address);
                                lines.Remove(prev4Address);
                                code.Remove(prev4Address);
                                lines.Remove(prev4Address);
                                code.Remove(prev4Address);
                                lines.Remove(prev4Address);
                                code.Remove(prev4Address);
                                lines.Remove(prev4Address);
                                code.Remove(prev4Address);
                                lines.Remove(prev4Address);
                                code.Remove(prev4Address);
                                lines.Remove(prev4Address);
                                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                
                                WriteChunk(OpCode.POPHL, sourceLocation[0], sourceLocation[1], false);
                                loop
                                {
                                    if (lsb4 == 0)
                                    {
                                        break;
                                    }
                                    WriteChunk(OpCode.INCHL, sourceLocation[0], sourceLocation[1], false);
                                    lsb4--;
                                }
                               
                                PeepholeOptimizer();
                                break;
                            }
                        }
                        
                        if ((prev3Op == OpCode.PUSHHL) && 
                            (prev2Op == OpCode.LDHLIND) && 
                            (prev1Op == OpCode.POPDE)&& 
                            (prev0Op == OpCode.ADDHLDE)
                            )
                        {
                            // Replace:
                            // PUSH HL
                            // LD HL, (nnnn)
                            // POP DE
                            // ADD HL, DE
                            //
                            // With:
                            // LD DE, (nnnn)
                            // ADD HL, DE
                            
                            byte lsb2 = code[prev2Address+1];
                            byte msb2 = code[prev2Address+2];
                            
                            // POP DE
                            code.Remove(prev1Address);
                            lines.Remove(prev1Address);
                            opCodeAddresses.Remove(opCodeAddresses.Length-2);
                            
                            // LDDEIND
                            code.SetItem(prev3Address,   0xED);
                            code.SetItem(prev3Address+1, 0x5B);
                            code.SetItem(prev3Address+2, lsb2);
                            code.SetItem(prev3Address+3, msb2);
                            opCodeAddresses.Remove(opCodeAddresses.Length-2);
                            
                            opCodeAddresses.SetItem(opCodeAddresses.Length-1, opCodeAddresses[opCodeAddresses.Length-1] - 1);
                            
                            continue; // try for another
                        }
                        if (
                             (prev5Op == OpCode.LDLIXIND) && 
                             (prev4Op == OpCode.LDHIXIND) && 
                             (prev3Op == OpCode.PUSHHL)&& 
                             (prev2Op == OpCode.LDLIXIND) && 
                             (prev1Op == OpCode.LDHIXIND)
                             )
                        {
                            // Replace:
                            // LD L, (IX-8)
                            // LD H, (IX-7)
                            // PUSH HL
                            // LD L, (IX-8)
                            // LD H, (IX-7)
                            // XX
                            //
                            // With:
                            // LD L, (IX-8)
                            // LD H, (IX-7)
                            // PUSH HL
                            // XX
                            
                            byte op5 = code[prev5Address+2];
                            byte op4 = code[prev4Address+2];
                            byte op2 = code[prev2Address+2];
                            byte op1 = code[prev1Address+2];
                            
                            if ((op5 == op2) && (op4 == op1))
                            {
                                code.Remove(prev2Address);
                                code.Remove(prev2Address);
                                code.Remove(prev2Address);
                                lines.Remove(prev2Address);
                                lines.Remove(prev2Address);
                                lines.Remove(prev2Address);
                                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                
                                code.Remove(prev2Address);
                                code.Remove(prev2Address);
                                code.Remove(prev2Address);
                                lines.Remove(prev2Address);
                                lines.Remove(prev2Address);
                                lines.Remove(prev2Address);
                                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    
                                continue; // try for another
                            }
                        }
                        if ( 
                             (prev4Op == OpCode.PUSHHL) && 
                             (prev3Op == OpCode.LDLIXIND) && 
                             (prev2Op == OpCode.LDHIXIND) && 
                             (prev1Op == OpCode.POPDE) && 
                             (prev0Op == OpCode.ADDHLDE)
                             )
                        {
                            // Replace:
                            // PUSH HL
                            // LD L, (IX-8)
                            // LD H, (IX-7)
                            // POP DE
                            // ADD HL, DE
                            //
                            // With:
                            // LD E, (IX-8)
                            // LD D, (IX-7)
                            // ADD HL, DE
                            
                            code.SetItem(prev3Address+1, 0x5E);
                            code.SetItem(prev2Address+1, 0x56);
                            
                            code.Remove(prev1Address);
                            lines.Remove(prev1Address);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            
                            code.Remove(prev4Address);
                            lines.Remove(prev4Address);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            
                            opCodeAddresses.SetItem(opCodeAddresses.Length-2, opCodeAddresses[opCodeAddresses.Length-2]+2);
                            opCodeAddresses.SetItem(opCodeAddresses.Length-1, code.Length-1);
                                
                            continue; // try for another
                        }
                        if (
                            (prev5Op == OpCode.LDHLIND) && 
                            (prev4Op == OpCode.PUSHHL) && 
                            (prev3Op == OpCode.LDLIXIND)&& 
                            (prev2Op == OpCode.LDHIXIND) && 
                            (prev1Op == OpCode.POPDE)
                            )
                        {
                            // Replace:
                            // LD HL, (FFF0) - 3 bytes
                            // PUSH HL
                            // LD L, (IX-16)
                            // LD H, (IX-15)
                            // POP DE
                            // XX
                            //
                            // With:
                            // LD DE, (FFF0) - 4 bytes
                            // LD L, (IX-16)
                            // LD H, (IX-15)
                            // XX
                            
                            byte lsb5 = code[prev5Address+1];
                            byte msb5 = code[prev5Address+2];
                            byte op3  = code[prev3Address+2];
                            byte op2  = code[prev2Address+2];
                            
                            // LDDEIND
                            code.SetItem(prev5Address,   0xED);
                            code.SetItem(prev5Address+1, 0x5B);
                            code.SetItem(prev5Address+2, lsb5);
                            code.SetItem(prev5Address+3, msb5);
                            
                            // POP DE
                            code.Remove(prev1Address);
                            lines.Remove(prev1Address);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            
                            // PUSHHL
                            opCodeAddresses.Remove(opCodeAddresses.Length-3);
                            
                            opCodeAddresses.SetItem(opCodeAddresses.Length-3, opCodeAddresses[opCodeAddresses.Length-3]+1);
                                                        
                            continue; // try for another
                        }
                        if (
                            (prev5Op == OpCode.LDHLIND) && 
                            (prev4Op == OpCode.PUSHHL) && 
                            (prev3Op == OpCode.LDDE)&& 
                            (prev2Op == OpCode.PUSHDE) && 
                            (prev1Op == OpCode.POPHL) && 
                            (prev0Op == OpCode.POPDE))
                        {
                            // Replace:
                            //
                            // LD HL, (nnnn) - 3 bytes
                            // PUSH HL       - 1 byte
                            // LD DE, mmmm   - 3 bytes
                            // PUSH DE       - 1 byte
                            // POP HL        - 1 byte
                            // POP DE        - 1 byte
                            //
                            // With:
                            //
                            // LD HL,  mmmm  - 3 bytes
                            // LD DE, (nnnn) - 4 bytes : 0xED 0x5B nn nn
                            
                            byte lsb5 = code[prev5Address+1];
                            byte msb5 = code[prev5Address+2];
                            uint operand5 = lsb5 + (msb5 << 8);
                            
                            uint operand3 = code[prev3Address+1] + (code[prev3Address+2] << 8);
                            
                            code.SetItem(prev5Address, byte(OpCode.LDHL));
                            code.SetItem(prev5Address+1, code[prev3Address+1]);
                            code.SetItem(prev5Address+2, code[prev3Address+2]);
                            code.SetItem(prev4Address,   0xED);
                            code.SetItem(prev4Address+1, 0x5B);
                            code.SetItem(prev4Address+2, lsb5);
                            code.SetItem(prev4Address+3, msb5);
                            
                            code.Remove(code.Length-1);
                            code.Remove(code.Length-1);
                            code.Remove(code.Length-1);
                            lines.Remove(code.Length-1);
                            lines.Remove(code.Length-1);
                            lines.Remove(code.Length-1);
                            
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            
                            opCodeAddresses.SetItem(opCodeAddresses.Length-1, code.Length-4);
                            
                            continue; // try for another
                        }
                        if (
                             (prev5Op == OpCode.PUSHIY) && 
                             (prev4Op == OpCode.LDDE) && 
                             (prev3Op == OpCode.POPHL) && 
                             (prev2Op == OpCode.LDHLE) && 
                             (prev1Op == OpCode.INCHL) && 
                             (prev0Op == OpCode.LDHLD)
                             )
                        {
                            
                            // Replace:
                            //
                            // PUSH IY
                            // LD DE, 0000
                            // POP HL
                            // LD (HL), E
                            // INC HL
                            // LD (HL), D
                            //
                            // With:
                            //
                            // LD DE, 0000
                            // LD (IY+0), E
                            // LD (IY+1), D
                            
                            byte lsb4 = code[prev4Address+1];
                            byte msb4 = code[prev4Address+2];
                            
                            string lastLine = lines[lines.Length-1];
                            <string> sourceLocation = lastLine.Split(':');
                            
                            code.Remove(code.Length-1);
                            lines.Remove(lines.Length-1);
                            code.Remove(code.Length-1);
                            lines.Remove(lines.Length-1);
                            code.Remove(code.Length-1);
                            lines.Remove(lines.Length-1);
                            code.Remove(code.Length-1);
                            lines.Remove(lines.Length-1);
                            code.Remove(code.Length-1);
                            lines.Remove(lines.Length-1);
                            code.Remove(code.Length-1);
                            lines.Remove(lines.Length-1);
                            code.Remove(code.Length-1);
                            lines.Remove(lines.Length-1);
                            code.Remove(code.Length-1);
                            lines.Remove(lines.Length-1);
                            code.Remove(code.Length-1);
                            lines.Remove(lines.Length-1);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            opCodeAddresses.Remove(opCodeAddresses.Length-1);
                            
                            WriteChunk(OpCode.LDDE, sourceLocation[0], sourceLocation[1], false);
                            WriteChunk(lsb4, sourceLocation[0], sourceLocation[1]);
                            WriteChunk(msb4, sourceLocation[0], sourceLocation[1]);
                            WriteChunk(OpCode.LDIYINDE, sourceLocation[0], sourceLocation[1], false);
                            WriteChunk(0, sourceLocation[0], sourceLocation[1]);
                            WriteChunk(OpCode.LDIYINDD, sourceLocation[0], sourceLocation[1], false);
                            WriteChunk(1, sourceLocation[0], sourceLocation[1]);
                            
                            PeepholeOptimizer();
                            break;
                        }
                        if ( 
                             (prev5Op == OpCode.LDEIYIND) && 
                             (prev4Op == OpCode.LDDIYIND) && 
                             (prev3Op == OpCode.PUSHDE) && 
                             (prev2Op == OpCode.LDEIYIND) && 
                             (prev1Op == OpCode.LDDIYIND) && 
                             (prev0Op == OpCode.PUSHDE)
                             )
                        {
                            
                            // Replace:
                            //
                            // LD E, (IY+0) [next]
                            // LD D, (IY+1)
                            // PUSH DE
                            // LD E, (IY+2) [top]
                            // LD D, (IY+3)
                            // PUSH DE
                            //
                            // With:
                            //
                            // LD E, (IY+0) [next]
                            // LD D, (IY+1)
                            // LD L, (IY+2) [top]
                            // LD H, (IY+3)
                            // PUSH DE      [next]
                            // PUSH HL      [top]
                            
                            string lastLine = lines[lines.Length-1];
                            <string> sourceLocation = lastLine.Split(':');
                            
                            code.SetItem(prev2Address+1, 0x6E);
                            code.SetItem(prev1Address+1, 0x66);
                            
                            code.Remove(prev3Address);
                            lines.Remove(prev3Address);
                                                                
                            opCodeAddresses.Remove(opCodeAddresses.Length-4);
                            opCodeAddresses.SetItem(opCodeAddresses.Length-3, opCodeAddresses[opCodeAddresses.Length-3]-1);
                            opCodeAddresses.SetItem(opCodeAddresses.Length-2, opCodeAddresses[opCodeAddresses.Length-2]-1);
                            opCodeAddresses.SetItem(opCodeAddresses.Length-1, opCodeAddresses[opCodeAddresses.Length-1]-1);
                            
                            WriteChunk(OpCode.PUSHHL, sourceLocation[0], sourceLocation[1], false);
                            PeepholeOptimizer();
                            
                            break;
                        }
                        
                        
                        if (addressesLength > 8)
                        {
                            uint prev6Address = opCodeAddresses[addressesLength-7];
                            if (prev6Address > peepholeBoundary)
                            {
                                OpCode prev6Op     = BuildOpCode(prev6Address);
                                
                                if (   (prev6Op == OpCode.PUSHHL)
                                    && (prev5Op == OpCode.PUSHBC)
                                    && (prev4Op == OpCode.POPHL)
                                    && (prev3Op == OpCode.POPDE)
                                    && (prev2Op == OpCode.ADDHLDE)
                                    && (prev1Op == OpCode.PUSHHL)
                                    && (prev0Op == OpCode.POPBC)
                                    )
                                {
                                    // Replace
                                    // 
                                    // PUSH HL
                                    // PUSH BC
                                    // POP HL
                                    // POP DE
                                    // ADD HL, DE
                                    // PUSH HL
                                    // POP BC
                                    // 
                                    // With
                                    // 
                                    // ADD HL, BC
                                    // LD C, L
                                    // LD B, H
                                    // 
                                    
                                    code.SetItem(prev2Address, byte(OpCode.ADDHLBC));
                                    code.SetItem(prev1Address, byte(OpCode.LDCL));
                                    code.SetItem(prev0Address, byte(OpCode.LDBH));
                                    
                                    code.Remove(prev6Address);
                                    lines.Remove(prev6Address);
                                    code.Remove(prev6Address);
                                    lines.Remove(prev6Address);
                                    code.Remove(prev6Address);
                                    lines.Remove(prev6Address);
                                    code.Remove(prev6Address);
                                    lines.Remove(prev6Address);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    
                                    continue;
                                }
                                if ((prev5Op == OpCode.PUSHHL) && 
                                    (prev4Op == OpCode.LDDE) && 
                                    (prev3Op == OpCode.PUSHDE) && 
                                    (prev2Op == OpCode.POPHL)&& 
                                    (prev1Op == OpCode.POPDE) && 
                                    (prev0Op == OpCode.ADDHLDE))
                                {
                                    // Replace:
                                    //
                                    // PUSH HL
                                    // LD DE, nnnn - 3 bytes
                                    // PUSH DE
                                    // POP HL
                                    // POP DE
                                    // ADD HL, DE
                                    //
                                    // With:
                                    //
                                    // LD DE, nnnn
                                    // ADD HL, DE
                                    //
                                    // order of DE and HL are reversed but it doesn't matter if followed by ADD
                                    
                                    code.Remove(prev5Address);
                                    lines.Remove(prev5Address);
                                    code.Remove(prev3Address-1);
                                    lines.Remove(prev3Address-1);
                                    code.Remove(prev3Address-1);
                                    lines.Remove(prev3Address-1);
                                    code.Remove(prev3Address-1);
                                    lines.Remove(prev3Address-1);
                                                                        
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    
                                    opCodeAddresses.SetItem(opCodeAddresses.Length-2, prev5Address);
                                    opCodeAddresses.SetItem(opCodeAddresses.Length-1, code.Length-1);
                                    
                                    continue; // try for another
                                }
                                
                                if ((prev6Op == OpCode.LDD) && 
                                 (prev5Op == OpCode.LDEHL) && 
                                 (prev4Op == OpCode.PUSHDE)&& 
                                 (prev3Op == OpCode.POPHL) && 
                                 (prev2Op == OpCode.LDHCL) && 
                                 (prev1Op == OpCode.LDL))
                                {
                                    // Replace:
                                    //
                                    // LD D, 0    - 2 bytes
                                    // LD E, (HL) 
                                    // PUSH DE
                                    // POP HL
                                    // LD H, L
                                    // LD L, 0
                                    // XX
                                    //
                                    // With:
                                    //
                                    // LD H, (HL)
                                    // LD L, 0
                                    // XX
                                    
                                    byte op6  = code[prev6Address+1];
                                    byte op1  = code[prev1Address+1];
                                    if ((op1 == 0) && (op6 == 0))
                                    {
                                        code.SetItem(prev6Address,   byte(OpCode.LDHHL));
                                        code.SetItem(prev6Address+1, byte(OpCode.LDL));
                                        code.SetItem(prev6Address+2, 0);
                                        opCodeAddresses.SetItem(opCodeAddresses.Length-6, opCodeAddresses[opCodeAddresses.Length-6]-1);
                                        
                                        code.Remove(prev4Address);
                                        lines.Remove(prev4Address);
                                        code.Remove(prev4Address);
                                        lines.Remove(prev4Address);
                                        code.Remove(prev4Address);
                                        lines.Remove(prev4Address);
                                        code.Remove(prev4Address);
                                        lines.Remove(prev4Address);
                                        code.Remove(prev4Address);
                                        lines.Remove(prev4Address);
                                        
                                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                            
                                        continue; // try for another
                                    }
                                }
                                if ((prev6Op == OpCode.LDLIXIND) && 
                                     (prev5Op == OpCode.LDHIXIND) && 
                                     (prev4Op == OpCode.PUSHHL) && 
                                     (prev3Op == OpCode.LDDE)&& 
                                     (prev2Op == OpCode.PUSHDE) && 
                                     (prev1Op == OpCode.POPHL) && 
                                     (prev0Op == OpCode.POPDE)
                                     )
                                {
                                    // Replace:
                                    //
                                    // LD L, (IX+d) - 3 bytes
                                    // LD H, (IX+d) - 3 bytes
                                    // PUSH HL
                                    // LD DE, nnnn  - 3 bytes
                                    // PUSH DE
                                    // POP HL
                                    // POP DE
                                    //
                                    // With:
                                    //
                                    // LD E, (IX+d)
                                    // LD D, (IX+d)
                                    // LD HL, nnnn
                                    //
                                    
                                    byte lsb3 = code[prev3Address+1];
                                    byte msb3 = code[prev3Address+2];
                                    
                                    code.SetItem(prev6Address+0, 0xDD); // LDEIXIND
                                    code.SetItem(prev6Address+1, 0x5E); 
                                    code.SetItem(prev5Address+0, 0xDD); // LDDIXIND
                                    code.SetItem(prev5Address+1, 0x56); 
                                    code.SetItem(prev4Address,   byte(OpCode.LDHL));
                                    code.SetItem(prev4Address+1, lsb3);
                                    code.SetItem(prev4Address+2, msb3);
                                    
                                    //uint operand3 = lsb3 + (msb3 << 8);
                                    
                                    code.Remove(code.Length-1);
                                    lines.Remove(code.Length-1);
                                    code.Remove(code.Length-1);
                                    lines.Remove(code.Length-1);
                                    code.Remove(code.Length-1);
                                    lines.Remove(code.Length-1);
                                    code.Remove(code.Length-1);
                                    lines.Remove(code.Length-1);
                                                                        
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    
                                    continue; // try for another
                                }
                                if ((prev6Op == OpCode.LDIXINDL) && 
                                     (prev5Op == OpCode.LDIXINDH) && 
                                     (prev4Op == OpCode.LDHIXIND) && 
                                     (prev3Op == OpCode.LDL)&& 
                                     (prev2Op == OpCode.LDEIXIND) && 
                                     (prev1Op == OpCode.LDDIXIND) && 
                                     (prev0Op == OpCode.ADDHLDE)
                                     )
                                {
                                    // Replace:
                                    //
                                    // LD (IX-20), L - 3 bytes
                                    // LD (IX-19), H - 3 bytes
                                    // LD H, (IX-18) - 3 bytes
                                    // LD L, 00      - 2 bytes
                                    // LD E, (IX-20) - 3 bytes
                                    // LD D, (IX-19) - 3 bytes
                                    // ADD HL, DE    - 1 byte
                                    //
                                    // With:
                                    //
                                    // LD (IX-20), L - 3 bytes
                                    // LD (IX-19), H - 3 bytes
                                    // LD D, (IX-18) - 3 bytes
                                    // LD E, 00      - 2 bytes
                                    // ADD HL, DE    - 1 byte
                                    //
                                    
                                    byte op6 = code[prev6Address+2];
                                    byte op5 = code[prev5Address+2];
                                    byte op4 = code[prev4Address+2];
                                    byte op3 = code[prev3Address+1];
                                    byte op2 = code[prev2Address+2];
                                    byte op1 = code[prev1Address+2];
                                    if ((op6 == op2) && (op5 == op1) && (op3 == 0))
                                    {
                                        code.SetItem(prev4Address+0, 0xDD); // LDDIXIND
                                        code.SetItem(prev4Address+1, 0x56); 
                                        code.SetItem(prev3Address,   byte(OpCode.LDE));
                                        code.SetItem(prev3Address+1, 0);
                                        
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                                                            
                                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                        
                                        opCodeAddresses.SetItem(opCodeAddresses.Length-1, code.Length-1);
                                        
                                        continue; // try for another
                                    }
                                }
                                if ((prev5Op == OpCode.LDEIXIND) && 
                                     (prev4Op == OpCode.LDDIXIND) && 
                                     (prev3Op == OpCode.ADDHLDE) && 
                                     (prev2Op == OpCode.LDEIXIND) && 
                                     (prev1Op == OpCode.LDDIXIND) && 
                                     (prev0Op == OpCode.ADDHLDE)
                                     )
                                {
                                    // Replace:
                                    //
                                    // LD E, (IX-8) - 3 bytes
                                    // LD D, (IX-7) - 3 bytes
                                    // ADD HL, DE   - 1 byte
                                    // LD E, (IX-8) - 3 bytes
                                    // LD D, (IX-7) - 3 bytes
                                    // ADD HL, DE   - 1 byte
                                    //
                                    // With:
                                    //
                                    // LD E, (IX-8) - 3 bytes
                                    // LD D, (IX-7) - 3 bytes
                                    // ADD HL, DE   - 1 byte
                                    // ADD HL, DE   - 1 byte
                                    //
                                    
                                    byte op5 = code[prev5Address+2];
                                    byte op4 = code[prev4Address+2];
                                    byte op2 = code[prev2Address+2];
                                    byte op1 = code[prev1Address+2];
                                    if ((op5 == op2) && (op4 == op1))
                                    {
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                        code.Remove(prev2Address);
                                        lines.Remove(prev2Address);
                                                                            
                                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                        opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                        
                                        opCodeAddresses.SetItem(opCodeAddresses.Length-2, code.Length-2);
                                        opCodeAddresses.SetItem(opCodeAddresses.Length-1, code.Length-1);
                                        
                                        continue; // try for another
                                    }
                                }
                                if ((prev6Op == OpCode.LDEHL) && 
                                     (prev5Op == OpCode.INCHL) && 
                                     (prev4Op == OpCode.LDDHL) && 
                                     (prev3Op == OpCode.POPHL) && 
                                     (prev2Op == OpCode.LDHLE) && 
                                     (prev1Op == OpCode.INCHL) && 
                                     (prev0Op == OpCode.LDHLD)
                                     )
                                {
                                    // Replace:
                                    //
                                    // LD E, (HL)
                                    // INC HL
                                    // LD D, (HL)
                                    // POP HL
                                    // LD (HL), E
                                    // INC HL
                                    // LD (HL), D
                                    //
                                    // With:
                                    // 
                                    // POP DE
                                    // LDI
                                    // LDI
                                    
                                    code.SetItem(prev6Address, byte(OpCode.POPDE));
                                    code.SetItem(prev5Address, 0xED);
                                    code.SetItem(prev4Address, 0xA0);
                                    code.SetItem(prev3Address, 0xED);
                                    code.SetItem(prev2Address, 0xA0);
                                    code.Remove(code.Length-1);
                                    lines.Remove(lines.Length-1);
                                    code.Remove(code.Length-1);
                                    lines.Remove(lines.Length-1);
                                    
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    
                                    opCodeAddresses.SetItem(opCodeAddresses.Length-1, prev3Address);
                                    
                                    continue; // try for another
                                }
                                
                                if ((prev3Op == OpCode.PUSHDE) && 
                                    (prev2Op == OpCode.POPHL) && 
                                    (prev1Op == OpCode.LDEL)&& 
                                    (prev0Op == OpCode.LDDH)
                                    )
                                {
                                    // Replace:
                                    // PUSH DE
                                    // POP HL
                                    // LD E, L
                                    // LD D, H
                                    //
                                    // With:
                                    // LD L, E
                                    // LD H, D
                                    
                                    code.SetItem(prev3Address, byte(OpCode.LDLE));
                                    code.SetItem(prev2Address, byte(OpCode.LDHD));
                                    
                                    code.Remove(code.Length-1);
                                    lines.Remove(lines.Length-1);
                                    code.Remove(code.Length-1);
                                    lines.Remove(lines.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    
                                    continue; // try for another
                                }
                                
                                if (   (prev5Op == OpCode.PUSHBC)
                                    && (prev4Op == OpCode.POPHL)
                                    )
                                {
                                    // Replace:
                                    // PUSH BC
                                    // POP HL
                                    // XX
                                    // XX
                                    // XX
                                    // XX
                                    //
                                    // With:
                                    // LD L, C
                                    // LD H, B
                                    
                                    code.SetItem(prev5Address, byte(OpCode.LDLC));
                                    code.SetItem(prev4Address, byte(OpCode.LDHB));
                                    
                                    continue; // try for another
                                }
                                
                                if (   (prev6Op == OpCode.PUSHDE)
                                    && (prev5Op == OpCode.LDLC)
                                    && (prev4Op == OpCode.LDHB)
                                    && (prev3Op == OpCode.POPDE)
                                    && (prev2Op == OpCode.ADDHLDE)
                                    && (prev1Op == OpCode.LDCL)
                                    && (prev0Op == OpCode.LDBH)
                                    )
                                {
                                    // Replace:
                                    //
                                    // PUSH DE
                                    // LD L, C
                                    // LD H, B
                                    // POP DE
                                    // ADD HL, DE
                                    // LD C, L
                                    // LD B, H
                                    //
                                    // With:
                                    //
                                    // EX DE, HL
                                    // ADD HL, BC
                                    // LD C, L
                                    // LD B, H
                                    
                                    code.Remove(prev6Address);
                                    lines.Remove(prev6Address);
                                    code.Remove(prev6Address);
                                    lines.Remove(prev6Address);
                                    code.Remove(prev6Address);
                                    lines.Remove(prev6Address);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-1);
                                    code.SetItem(prev6Address, byte(OpCode.EXDEHL));
                                    code.SetItem(prev5Address, byte(OpCode.ADDHLBC));
                                    
                                    continue; // try for another
                                }
                                
                                if (   (prev4Op == OpCode.PUSHDE)
                                    && (prev3Op == OpCode.POPHL)
                                    )
                                {
                                    // Replace:
                                    // PUSH DE
                                    // POP HL
                                    // XX
                                    // XX
                                    // XX
                                    //
                                    // With:
                                    // LD L, E
                                    // LD H, D
                                    
                                    code.SetItem(prev4Address, byte(OpCode.LDLE));
                                    code.SetItem(prev3Address, byte(OpCode.LDHD));
                                    
                                    continue; // try for another
                                }
                                
                                if (   (prev4Op == OpCode.PUSHDE)
                                    && (prev3Op == OpCode.LDHLIND)
                                    && (prev2Op == OpCode.POPDE)
                                    )
                                {
                                    // Replace:
                                    // PUSH DE
                                    // LD HL, (nnnn) - 3 bytes
                                    // POP DE
                                    // XX
                                    // XX
                                    //
                                    // With:
                                    // LD HL, (nnnn) - 3 bytes
                                    // XX
                                    // XX
                                    
                                    code.Remove(prev2Address);
                                    lines.Remove(prev2Address);
                                    code.Remove(prev4Address);
                                    lines.Remove(prev4Address);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-5);
                                    opCodeAddresses.Remove(opCodeAddresses.Length-3);
                                    
                                    opCodeAddresses.SetItem(opCodeAddresses.Length-3, prev4Address);
                                    opCodeAddresses.SetItem(opCodeAddresses.Length-2, opCodeAddresses[opCodeAddresses.Length-2] - 2);
                                    opCodeAddresses.SetItem(opCodeAddresses.Length-1, opCodeAddresses[opCodeAddresses.Length-1] - 2);
                                    
                                    continue; // try for another
                                }
                                
                                
                                
                                
                            }
                        }
                    }
                }
            }
        }
        
        break;
    }
  }
  
  DumpTail(uint tailLength)
  {
    //PrintLn((code.Length).ToHexString(4));
    //PrintLn((opCodeAddresses[opCodeAddresses.Length-1]).ToHexString(4));
    //PrintLn((code[code.Length-1]).ToHexString(2));
    
    uint tail = opCodeAddresses.Length-tailLength;
    if (opCodeAddresses.Length < tailLength)
    {
        tail = 0;
    }
    PrintLn();
    loop
    {
        if (tail == opCodeAddresses.Length)
        {
            break;
        }
        uint address = opCodeAddresses[tail];
        uint color = Color.MatrixBlue;
        if (address <= peepholeBoundary)
        {
            color = Color.MatrixRed;
        }
        string line = address.ToHexString(4) + " ";
        long instruction;
        OpCode opCode = OpCode.UNUSED; 
        string instructionHex = "??     ";
        if (address < code.Length)
        {
            instruction = code[address];
            instructionHex = (code[address]).ToHexString(2) + " ";
            address++;
            if ((instruction == 0xCB) || (instruction == 0xDD) || (instruction == 0xED) || (instruction == 0xFD)) // prefix
            {
                instruction = instruction * 256 + code[address];
                instructionHex = instructionHex + (code[address]).ToHexString(2) + "  ";
                address++;
            }
            else
            {
                instructionHex = instructionHex + "    ";
            }
            OpCode oc = OpCode(instruction);
            opCode = oc;
        }
        string opCodeString = OpCodeToString(opCode);
        if (opCode == OpCode.UNUSED)
        {
            // no substitutions
        }
        else if (opCodeString.Contains("nn"))
        {
            if (address < code.Length-1)
            {
                uint operand      = code[address];
                address++;
                operand = operand + (code[address] << 8);
                address++;
                string operandstring = operand.ToHexString(4);
                opCodeString = opCodeString.Replace("nn", operandstring);
            }
        }
        else if (opCodeString.Contains("d"))
        {
            if (address < code.Length)
            {
                uint operand      = code[address];
                address++;
                string operandstring = operand.ToHexString(2);
                opCodeString = opCodeString.Replace("d", operandstring);
                if ((opCode == OpCode.LDIXINDN) && (address < code.Length))
                {
                    operand      = code[address];
                    address++;
                    operandstring = operand.ToHexString(2);
                    opCodeString = opCodeString.Replace("n", operandstring);
                }
            }
        }
        else if (opCodeString.Contains("n"))
        {
            if (address < code.Length)
            {
                uint operand      = code[address];
                address++;
                string operandstring = operand.ToHexString(2);
                opCodeString = opCodeString.Replace("n", operandstring);
            }
        }
        line = line + instructionHex + opCodeString;
        PrintLn(line, color, Color.Black);
        tail++;
    }
  }
  
  bool RemoveZero()
  {
    bool removedZero = false;
    if ((code.Length > 4) && (code.Length-4 > peepholeBoundary))
    {
        OpCode prevOp = OpCode(code[code.Length-4]);
        OpCode currOp = OpCode(code[code.Length-1]);
        if ((prevOp == OpCode.LDDE) && (currOp == OpCode.PUSHDE))
        {
            byte op0 = code[code.Length-3];
            byte op1 = code[code.Length-2];
            if ((op0 == 0) && (op1 == 0))
            {
                //PrintLn(OpCodeToString(prevOp) + " 0x" + op1.ToHexString(2) + op0.ToHexString(2) + " -> " + OpCodeToString(currOp));
                code.Remove(code.Length-1);
                code.Remove(code.Length-1);
                code.Remove(code.Length-1);
                code.Remove(code.Length-1);
                lines.Remove(lines.Length-1);
                lines.Remove(lines.Length-1);
                lines.Remove(lines.Length-1);
                lines.Remove(lines.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                opCodeAddresses.Remove(opCodeAddresses.Length-1);
                removedZero = true;
            }
        }
    }
    return removedZero;
  }
    
  string InsertOpCode(OpCode opCode)
  {
    string opcodestring;
    long instruction = long(opCode);
    
    long lsb = (instruction % 256);
    long msb = (instruction / 256);
    
    uint ilsb = uint(lsb);
    uint imsb = uint(msb);
    
    if (imsb != 0)
    {
        opcodestring = imsb.ToHexString(2) + " ";
    }
    opcodestring = opcodestring + ilsb.ToHexString(2);
    return opcodestring;
  }
  
  string InsertHex(string opcode, uint address, uint bytes)
  {
    string hex = opcode;
    for (uint i=0; i < bytes; i++)
    {
        uint b = code[address+i];
        hex = hex + " " + b.ToHexString(2);
    }
    hex = hex.Pad(' ', 12);
    return hex;
  }
  
  dumpData(uint offset, uint dataLength)
  {
    string linetext;
    string ascii;
    uint counter = 0;
    loop
    {
        if (counter == 0)
        {
            linetext = "       " + offset.ToHexString(4) + "  ";
        }
        byte db = code[offset];
        linetext = linetext + db.ToHexString(2) + " ";
        if ((db < 32) || (db > 127))
        {
            ascii = ascii + ".";
        }
        else
        {
            ascii = ascii + char(db);
        }
        counter++;
        if (counter == 8)
        {
            linetext = linetext + " ";
        }
        if (counter == 16)
        {
            if (zasmFile.IsValid())
            {
                linetext = linetext + "  " + ascii;
                zasmFile.Append(linetext + char(0x0A));
            }
            linetext = "";
            ascii = "";
            counter = 0;
        }
        offset++;
        dataLength--;
        if (dataLength == 0)
        {
            break;
        }
    }
    if (counter > 0)
    {
        if (zasmFile.IsValid())
        {
            linetext = linetext.Pad(' ', 60);
            linetext = linetext + "  " + ascii;
            zasmFile.Append(linetext + char(0x0A));
        }
    }
  }
  PrintWithLine(string output, string location)
  {
    string eol = "" + char(0x0A);
    <string> locationParts = location.Split(':');
    
    string linetext = locationParts[1];
    linetext = linetext.LeftPad('0', 5);
    string sourceName = locationParts[0];
    
    if (!previousLocationSet || (location != previousLocation))
    {
        if (zasmFile.IsValid())
        {
            zasmFile.Append(eol);
            if (sourceLines.Contains(location))
            {
                if (previousSource != sourceName)
                {
                    zasmFile.Append("// " + sourceName + eol);
                }
                zasmFile.Append("// " + sourceLines[location] + eol);
                zasmFile.Append("// " + eol);
            }
            else
            {
                zasmFile.Append("// " + location + "?" + eol);
            }
        }
    }
    
    output = linetext + "  " + output;
    //PrintLn(output);
    if (zasmFile.IsValid())
    {
        zasmFile.Append(output + char(0x0A));
    }
    previousLocation = location;
    previousSource = sourceName;
    previousLocationSet = true;
  }
  uint simpleInstruction(uint address, OpCode opCode, string name, uint offset)
  {
    string output = address.ToHexString(4) + "  ";
    output = output + InsertHex(InsertOpCode(opCode), offset, 0) + name;
    PrintWithLine(output, lines[offset-1]);
    return offset;
  }
  uint operandInstructionIndirectIX(uint address, OpCode opCode, string name, uint offset)
  {
    string output = address.ToHexString(4) + "  ";
    int operand1 = code[offset];
    if (operand1 > 127)
    {
        operand1 = operand1 - 256; // 255 -> -1
    }
    output = output + InsertHex(InsertOpCode(opCode), offset, 1) + name;
    string delta = operand1.ToString();
    if (operand1 >= 0)
    {
        delta = "+" + delta;
    }
    output = output.Replace("d", delta);
    PrintWithLine(output, lines[offset-1]);
    return offset + 1;
  }
  uint operandInstructionIndirectIXN(uint address, OpCode opCode, string name, uint offset)
  {
    string output = address.ToHexString(4) + "  ";
    int operand1 = code[offset];
    int operand2 = code[offset+1];
    if (operand1 > 127)
    {
        operand1 = operand1 - 256; // 255 -> -1
    }
    output = output + InsertHex(InsertOpCode(opCode), offset, 2) + name;
    string delta = operand1.ToString();
    if (operand1 >= 0)
    {
        delta = "+" + delta;
    }
    output = output.Replace("d", delta);
    output = output.Replace("n", operand2.ToString());
    PrintWithLine(output, lines[offset-1]);
    return offset + 2;
  }
  
  
  uint operandInstructionIndirect2(uint address, OpCode opCode, string name, uint offset)
  {
    string output = address.ToHexString(4) + "  ";
    byte operand1 = code[offset];
    byte operand2 = code[offset+1];
    output = output + InsertHex(InsertOpCode(opCode), offset, 2) + name;
    output = output.Replace("nn", operand2.ToHexString(2)+ operand1.ToHexString(2));
    PrintWithLine(output, lines[offset-1]);
    return offset + 2;
  }
  uint operandInstructionAbsolute2(uint address, OpCode opCode, string name, uint offset)
  {
    string output = address.ToHexString(4) + "  ";
    byte operand1 = code[offset];
    byte operand2 = code[offset+1];
    output = output + InsertHex(InsertOpCode(opCode), offset, 2) + name;
    output = output.Replace("nn", operand2.ToHexString(2)+ operand1.ToHexString(2));
    PrintWithLine(output, lines[offset-1]);
    return offset + 2;
  }
  uint operandInstructionAbsolute1(uint address, OpCode opCode, string name, uint offset)
  {
    string output = address.ToHexString(4) + "  ";
    byte operand1 = code[offset];
    output = output + InsertHex(InsertOpCode(opCode), offset, 1) + name;
    output = output.Replace("n", operand1.ToHexString(2));
    PrintWithLine(output, lines[offset-1]);
    return offset + 1;
  }
  
  
  uint disassembleInstruction(uint offset)
  {
    uint address = offset;
    long instruction = code[offset];
    offset++;
    if ((instruction == 0xCB) || (instruction == 0xDD) || (instruction == 0xED) || (instruction == 0xFD)) // prefix
    {
        instruction = instruction * 256 + code[offset];
        offset++;
    }
    OpCode opCode = OpCode(instruction);
    string opCodeString = OpCodeToString(opCode);
    switch (opCode)
    {
        case OpCode.XOR:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.JP:
        {
            offset = operandInstructionAbsolute2(address, opCode, opCodeString, offset);
        }
        case OpCode.JPZ:
        {
            offset = operandInstructionAbsolute2(address, opCode, opCodeString, offset);
        }
        case OpCode.JPM:
        {
            offset = operandInstructionAbsolute2(address, opCode, opCodeString, offset);
        }
        case OpCode.JPP:
        {
            offset = operandInstructionAbsolute2(address, opCode, opCodeString, offset);
        }
        case OpCode.JPNZ:
        {
            offset = operandInstructionAbsolute2(address, opCode, opCodeString, offset);
        }
        case OpCode.CALL:
        {
            offset = operandInstructionAbsolute2(address, opCode, opCodeString, offset);
        }
        case OpCode.LDHLIND:
        {
            offset = operandInstructionIndirect2(address, opCode, opCodeString, offset);
        }
        case OpCode.LDDEIND:
        {
            offset = operandInstructionIndirect2(address, opCode, opCodeString, offset);
        }
        case OpCode.LDINDHL:
        {
            offset = operandInstructionIndirect2(address, opCode, opCodeString, offset);
        }
        case OpCode.LDINDIY:
        {
            offset = operandInstructionIndirect2(address, opCode, opCodeString, offset);
        }
        case OpCode.LDHL:
        {
            offset = operandInstructionAbsolute2(address, opCode, opCodeString, offset);
        }
        case OpCode.LDDE:
        {
            offset = operandInstructionAbsolute2(address, opCode, opCodeString, offset);
        }
        case OpCode.LDD:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.LDB:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.JR:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.JRZ:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.JRNZ:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.DJNZ:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.JRNC:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.LDHLN:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.LDL:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.LDA:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.LDH:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.LDE:
        {
            offset = operandInstructionAbsolute1(address, opCode, opCodeString, offset);
        }
        case OpCode.LDIX:
        {
            offset = operandInstructionAbsolute2(address, opCode, opCodeString, offset);
        }
        case OpCode.LDIY:
        {
            offset = operandInstructionAbsolute2(address, opCode, opCodeString, offset);
        }
        case OpCode.LDIXINDN:
        {
            offset = operandInstructionIndirectIXN(address, opCode, opCodeString, offset);
        }
        case OpCode.LDHIXIND:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDDIXIND:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDEIXIND:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDDIYIND:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDEIYIND:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDHIYIND:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDLIYIND:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.ORAIXIND:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDAIXIND:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDLIXIND:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDIXINDH:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDIXINDD:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDIXINDE:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDIYINDE:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDIYINDD:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDIYINDL:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDIYINDH:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        case OpCode.LDIXINDL:
        {
            offset = operandInstructionIndirectIX(address, opCode, opCodeString, offset);
        }
        default:
        {
            if (opCodeString != "")
            {
                offset = simpleInstruction(address, opCode, opCodeString, offset);
            }
            else
            {
                // probably data like the switch jump table
                uint offsetm = offset - 1;
                string output = offsetm.ToHexString(4) + "  " + instruction.ToHexString(2);
                PrintWithLine(output, lines[offset-1]);
            }
        }
    }
    return offset;
  }
  
  Disassemble(string zasmPath, <string> sourceList)
  {
    sourceLines.Clear();
    previousLocationSet = false;
    foreach (var sourcePath in sourceList)
    {
        if (File.Exists(sourcePath))
        {
            file sourceFile = File.Open(sourcePath);
            if (sourceFile.IsValid())
            {
                string lowerPath = sourcePath.ToLower();
                uint iLine = 1;
                loop
                {
                    string sourceLine = sourceFile.ReadLine();
                    if (!sourceFile.IsValid())
                    {
                        break;
                    }
                    string lineString = iLine.ToString();
                    lineString = lineString.LeftPad('0', 5);
                    string key =lowerPath + ":" + lineString;
                    sourceLines[key] = sourceLine;
                    iLine++;
                }
            }
        }
    }
    if (File.Exists(zasmPath))
    {
        File.Delete(zasmPath);
    }
    zasmFile = File.Create(zasmPath);
    if (zasmFile.IsValid())
    {
        zasmFile.Append("Line   Addr  Opcodes     Instruction    " + char(0x0A));
        zasmFile.Append("----------------------------------------" + char(0x0A));
    }
    
    uint offset = disassembleInstruction(offset); // JP past the data
    if (Parser.GetEmbeddedDataSize() > 0)
    {
        uint dataLength = (code[1] + (code[2] << 8)) - 3;
        dumpData(offset, dataLength);
        offset = offset + dataLength;
    }
    loop
    {
        if (offset == code.Length)
        {
            break;
        }
        offset = disassembleInstruction(offset);
    }
    zasmFile.Flush();
  }
  
  byte hexCheckSum(string values)
  {
      uint sum = 0;
      for (uint i = 0; i < values.Length / 2; i++)
      {
          string substr = values.Substring(i * 2, 2);
          uint b = 0;
          if (Token.TryParseHex("0x" + substr, ref b))
          {
          }
          sum = sum + b;
      }
      sum = sum % 256;
      byte chk = byte(sum);
      chk = ~chk;
      chk++;
      return chk;
  }
  emitBuffer(file ihexFile, uint address, string buffer)
  {
      uint bytes = buffer.Length / 2;
      string ln =  bytes.ToHexString(2) + address.ToHexString(4) + "00" + buffer;
      byte chk = hexCheckSum(ln);
      ihexFile.Append(":" + ln + chk.ToHexString(2) + char(0x0A));
  }
  
  uint WriteIHex(string ihexPath)
  {
    // https://en.wikipedia.org/wiki/Intel_HEX#Format
    if (File.Exists(ihexPath))
    {
        File.Delete(ihexPath);
    }
    file ihexFile = File.Create(ihexPath);
    if (ihexFile.IsValid())
    {
        ihexFile.Append(":020000040000FA" + char(0x0A));
        uint length = code.Length;
        byte checksum = 0;
        string buffer;
        uint emitAddress = 0;
        for (uint address = 0; address < length; address++)
        {
            byte cb = code[address];
            buffer = buffer + cb.ToHexString(2);
            if (buffer.Length == 32)
            {
                emitBuffer(ihexFile, emitAddress, buffer);
                emitAddress = address+1;
                buffer = "";
            }
        }
        if (buffer.Length > 0)
        {
            emitBuffer(ihexFile, emitAddress, buffer);
        }
        ihexFile.Append(":00000001FF" + char(0x0A));
    }
    ihexFile.Flush();
    return code.Length;
  }
  
  WriteObj(string objPath)
  {
    if (File.Exists(objPath))
    {
        File.Delete(objPath);
    }
    file objFile = File.Create(objPath);
    if (objFile.IsValid())
    {
        uint length = code.Length;
        for (uint address = 0; address < length; address++)
        {
            byte b = code[address];
            objFile.Append(b);
        }
    }
    objFile.Flush();
  }
  
  bool ConvertOffsetToBytes(string offset, ref byte msb, ref byte lsb)
  {
    bool success = false;
    if ((offset.Length > 1) && (offset[0] == '-'))
    {
        int ioffset;
        // if the string starts with '-', then try to parse it as a 16 bit "int"
        if (Token.TryParseInt(offset, ref ioffset))
        {
            int imsb = ioffset / 256;
            int ilsb = ioffset % 256;
            msb = byte(imsb);
            lsb = byte(ilsb);
            PrintLn("TODO: this is probably wrong in ConvertOffsetToBytes: " + offset + " -> 0x" + msb.ToHexString(2) + lsb.ToHexString(2) );
            success = true;
        }
    }
    else
    {
        uint uoffset;
        if (Token.TryParseUInt(offset, ref uoffset))
        {
            uint umsb = uoffset / 256;
            uint ulsb = uoffset % 256;
            msb = byte(umsb);
            lsb = byte(ulsb);
            success = true;
        }
    }
    return success;
  }
  
}
