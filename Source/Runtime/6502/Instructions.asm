unit Instruction
{
    uses "SysCalls"
    uses "6502/GC"
    
    enum Instructions
    {
        NOP    = 0x00,
        DUP0   = 0x01,       // push [top]
        
        PUSHR0 = 0x02,       // R0 -> [top]
        POPR0  = 0x03,       // [top] -> R0
        
        BITSHL8  = 0x04,
        BITSHR8  = 0x05,
        BITANDFF = 0x06,

        ENTER      = 0x49,
        
        CALL       = 0x34,
        CALLI      = 0x6A,
        
        SYSCALL    = 0x26,
        
        DECSP      = 0x28,
        
        RET        = 0x35,
        RETRES     = 0x36,
        
        JZ         = 0x31,
        JNZ        = 0x32,
        
        JW         = 0x33,
        
        PUSHI      = 0x37,
        POPLOCAL   = 0x38,
        PUSHLOCAL  = 0x39,
        
        POPREL     = 0x3A,
        PUSHREL    = 0x3B,
        
        POPGLOBAL  = 0x3C,
        PUSHGLOBAL = 0x3D,
        
        PUSHSTACKADDR = 0x3E, 
        
        BOOLNOT    = 0x41,
        BITNOT     = 0x42,
        
        PUSHGP     = 0x47,
        
        COPYNEXTPOP = 0x48,
        
        CALLREL    = 0x4B,
        
        CAST       = 0x51,
        
        PUSHD      = 0x60,
        
        ADD        = 0x80,
        ADDI       = 0x81,
        SUB        = 0x82,
        SUBI       = 0x83,
        DIV        = 0x84,
        DIVI       = 0x85,
        MUL        = 0x86,
        MULI       = 0x87,
        MOD        = 0x88,
        MODI       = 0x89,
        
        GT         = 0x8A,
        GTI        = 0x8B,
        LT         = 0x8C,
        LTI        = 0x8D,
        GE         = 0x8E,
        GEI        = 0x8F,
        LE         = 0x90,
        LEI        = 0x91,
        
        EQ         = 0x92,
        NE         = 0x94,
        
        BOOLOR     = 0x96,
        BOOLAND    = 0x98,
        
        BITAND     = 0x9A,
        BITOR      = 0x9C,
        BITXOR     = 0x9E,
        
        BITSHR     = 0xA0,
        BITSHL     = 0xA2,
        
        // PACKED_INSTRUCTIONS
        PUSHIB       = 0x1A,
        POPLOCALB    = 0x1B,
        PUSHLOCALB   = 0x1C,
        PUSHGLOBALB  = 0x20,
        INCLOCALB    = 0x22,
        SYSCALL0     = 0x24,
        RETB         = 0x2A,
        RETRESB      = 0x2B,
        JZB          = 0x2E,
        JB           = 0x30,
        PUSHILT      = 0x55,
        ENTERB       = 0x5F,
        PUSHIBLE     = 0x6B,
        ADDB         = 0x6D,
        SUBB         = 0x6E,
        
        INCLOCALBB   = 0x3F,
        PUSHLOCALBB  = 0x56,
        SYSCALLB0    = 0xA8,
        
        PUSHI0       = 0x44,
        PUSHI1       = 0x45,
        RET0         = 0x4A,
        POPLOCALB00  = 0x4C,
        POPLOCALB01  = 0x4D,
        PUSHLOCALB00 = 0x4E,
        PUSHLOCALB01 = 0x4F,
    }
        
    // return the width of the operand of the current opCode (in X), in A
    GetOperandWidth() // munts X
    {
        TAX
        // current instruction into X (because JMP [nnnn,X] is then possible)
        switch (X)
        {
            case Instructions.ENTER:
            
            case Instructions.PUSHGP:
            case Instructions.DUP0:
            case Instructions.COPYNEXTPOP:
            case Instructions.CALLREL:
            
            case Instructions.ADD:
            case Instructions.ADDI:
            case Instructions.SUB:
            case Instructions.SUBI:
            case Instructions.DIV:
            case Instructions.MUL:
            case Instructions.MOD:
            case Instructions.DIVI:
            case Instructions.MULI:
            case Instructions.MODI:
            
            case Instructions.BOOLAND:
            case Instructions.BOOLOR:
            case Instructions.BOOLNOT:
            case Instructions.BITAND:
            case Instructions.BITOR:
            case Instructions.BITXOR:
            case Instructions.BITNOT:
            
            case Instructions.EQ:
            case Instructions.NE:
            case Instructions.LT:
            case Instructions.LE:
            case Instructions.GT:
            case Instructions.GE:
            case Instructions.LTI:
            case Instructions.LEI:
            case Instructions.GTI:
            case Instructions.GEI:
            case Instructions.BITSHR:
            case Instructions.BITSHL:
            case Instructions.BITSHR8:
            case Instructions.BITSHL8:
            case Instructions.BITANDFF:
            
            // PACKED_INSTRUCTIONS
            case Instructions.PUSHI0:
            case Instructions.PUSHI1:
            case Instructions.RET0:
            case Instructions.POPLOCALB00:
            case Instructions.POPLOCALB01:
            case Instructions.PUSHLOCALB00:
            case Instructions.PUSHLOCALB01:
            {
                LDA #0
            }
            case Instructions.SYSCALL:
            case Instructions.CAST:
            case Instructions.DECSP:
            
            // PACKED_INSTRUCTIONS
            case Instructions.PUSHIB:
            case Instructions.POPLOCALB:
            case Instructions.PUSHLOCALB:
            case Instructions.PUSHGLOBALB:
            case Instructions.INCLOCALB:
            case Instructions.SYSCALL0:
            case Instructions.RETB:
            case Instructions.RETRESB:
            case Instructions.JZB:
            case Instructions.JB:
            case Instructions.ENTERB:
            case Instructions.PUSHIBLE:
            case Instructions.ADDB:
            case Instructions.SUBB:
            {
                LDA #1
            }
            case Instructions.CALL:
            case Instructions.CALLI:
            
            case Instructions.RET:
            case Instructions.RETRES:

            case Instructions.JZ:
            case Instructions.JNZ:
            case Instructions.JW:
            
            case Instructions.PUSHI:
            case Instructions.PUSHD:
            case Instructions.POPLOCAL:
            case Instructions.PUSHLOCAL:
            case Instructions.POPGLOBAL:
            case Instructions.PUSHGLOBAL:
            
            case Instructions.PUSHSTACKADDR:
            case Instructions.PUSHREL:
            case Instructions.POPREL:
        
            // PACKED_INSTRUCTIONS
            case Instructions.INCLOCALBB:
            case Instructions.PUSHLOCALBB:
            case Instructions.SYSCALLB0:
            case Instructions.PUSHILT:
            {
                LDA #2
            }
            default:
            {
                TXA BRK // operand length not implemented!
            }
        }
    }
    
    // Z set if next instruction is CALL, Z clear if not
    IsCurrentCALL() // munts Y
    {
        // ACC = PC + CODESTART + instruction length
        GetCurrentAddress(); // GetNextAddress();
        
        // load next instruction into A
#ifdef CPU_65C02S
        LDA [ZP.ACC]
#else
        LDY #0
        LDA [ZP.ACC], Y
#endif        
        
        CMP # Instructions.CALL
        if (NZ)
        {
            CMP # Instructions.CALLI
        }    
    }
    // load the current PC address into ACC (PC + CODESTART)
    GetCurrentAddress()
    {
        // add CODESTART offset
        CLC
        LDA ZP.PCL
        ADC ZP.CODESTARTL
        STA ZP.ACCL
        LDA ZP.PCH
        ADC ZP.CODESTARTH
        STA ZP.ACCH
    }
    
    // load the current PC address plus instruction length into ACC (PC + instruction length + CODESTART)
    GetNextAddress()
    {  
        // ACC = PC + CODESTART
        GetCurrentAddress();
        
        // load current instruction into A
#ifdef CPU_65C02S
        LDA [ZP.ACC]
#else
        LDY #0
        LDA [ZP.ACC], Y
#endif        
        
        // get the width of the operand of the current opCode (in A), in A
        GetOperandWidth(); // munts X
     
#ifdef CPU_65C02S
        INC A // add the opCode size to get instruction length
#else
        CLC
        ADC #1
#endif
        
        // add current instruction length to current address
        CLC
        ADC ZP.ACCL
        STA ZP.ACCL
        LDA # 0
        ADC ZP.ACCH
        STA ZP.ACCH
    }
    
    // Resolve method index (IDX) to method address (IDX)
    //    munts: IDY, A and Y
    lookupMethod()
    {
        
        CLC
        LDA # (Address.HopperData & 0xFF)
        ADC # 6
        STA IDYL
        LDA # (Address.HopperData >> 8)
        ADC # 0
        STA IDYH
        
        loop
        {
            LDY # 0
            LDA [IDY], Y
            CMP IDXL
            if (Z)
            {
                INY
                LDA [IDY], Y
                CMP IDXH
                if (Z)
                {
                    // we have a winner
                    INY
                    LDA [IDY], Y
                    STA IDXL
                    INY
                    LDA [IDY], Y
                    STA IDXH
                    RTS
                }
            }
            CLC
            LDA IDYL
            ADC #4
            STA IDYL
            if (NC) { continue; }
            INC IDYH
        }
    }
    
    // load the operand into IDX, increment the PC by 2
    ConsumeOperand()
    {
        Utilities.IncACC();
        
        LDY #0
        LDA [ZP.ACC], Y
        STA ZP.IDXL
        INY
        LDA [ZP.ACC], Y
        STA ZP.IDXH
        
        Utilities.IncPC();
        Utilities.IncPC();
    }
    // load the operand into IDX, increment the PC by 1
    ConsumeOperandB()
    {
        Utilities.IncACC();
        
        LDY #0
        STY ZP.IDXH
        LDA [ZP.ACC], Y
        STA ZP.IDXL
        
        Utilities.IncPC();
    }
    // load the operand into IDX, increment the PC by 1
    ConsumeOperandSB()
    {
        Utilities.IncACC();
        
        LDY #0
        STY ZP.IDXH
        LDA [ZP.ACC], Y
        STA ZP.IDXL
        if (MI)
        {
            // signed byte, extend the sign
            LDA # 0xFF
            STA ZP.IDXH  
        }
        
        Utilities.IncPC();
    }
    // increment the PC by 1, load the operand into A
    ConsumeOperandA()
    {
        Utilities.IncACC();
        LDY # 0
        LDA [ZP.ACC], Y
        Utilities.IncPC();
    }
    
       
    nop()
    {
        NOP
    }
       
    enter()
    {
        Stacks.PushBP();
        LDA ZP.SP
        STA ZP.BP
    }
    enterB()
    {
        Stacks.PushBP();
        LDA ZP.SP
        STA ZP.BP
        
        LDA # 0
        STA ZP.TOPL
        STA ZP.TOPH
        LDA #Types.Byte
        STA ZP.TOPT
            
        ConsumeOperandA();
        TAX
        loop
        {
            if (Z) { break; }
            
            Stacks.PushTop();
            
            DEX
        }   
    }
    
    boolNot()
    {
        Stacks.PopTop();
        
        // [top] ? 0 : 1 -> [top] // assumes Type.Bool (0 or 1)
        LDA ZP.TOPL
        EOR # 0x01
        STA ZP.TOPL
        
        LDA #Types.UInt
        STA ZP.TOPT
        Stacks.PushTop();
    }
    boolOr()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        
        // [next] || [top] -> [top] // assumes Type.Bool (0 or 1)
        LDA ZP.NEXTL
        ORA ZP.TOPL
        STA ZP.NEXTL
        
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    boolAnd()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        
        // [next] && [top] -> [top] // assumes Type.Bool (0 or 1)
        LDA ZP.NEXTL
        AND ZP.TOPL
        STA ZP.NEXTL
        
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    bitAnd()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        
        // [next] &  [top] -> [top]
        LDA ZP.NEXTL
        AND ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        AND ZP.TOPH
        STA ZP.NEXTH
        
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    bitAndFF()
    {
        Stacks.PopTop();
        // [top] &  0xFF -> [top]
        LDA #0
        STA ZP.TOPH
        LDA #Types.UInt
        STA ZP.TOPT
        Stacks.PushTop();
    }
    bitOr()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        
        // [next] |  [top] -> [top]
        LDA ZP.NEXTL
        ORA ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        ORA ZP.TOPH
        STA ZP.NEXTH
        
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    bitXor()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        
        // [next] ^  [top] -> [top]
        LDA ZP.NEXTL
        EOR ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        EOR ZP.TOPH
        STA ZP.NEXTH
        
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    bitNot()
    {
        Stacks.PopTop();
        
        //  ~[top] -> [top]
        LDA ZP.TOPL
        EOR # 0xFF
        STA ZP.TOPL
        LDA ZP.TOPH
        EOR # 0xFF
        STA ZP.TOPH
        
        LDA #Types.UInt
        STA ZP.TOPT
        Stacks.PushTop();
    }
    addShared()
    {
        Stacks.PopNext();
        CLC
        LDA ZP.NEXTL
        ADC ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        ADC ZP.TOPH
        STA ZP.NEXTH
    }
    add()
    {
        Stacks.PopTop();
        addShared();
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    addB()
    {
        ConsumeOperandA();
        STA TOPL
        LDA # 0
        STA TOPH
        LDA #Types.Byte
        STA TOPT
        addShared();
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    addi()
    {
        Stacks.PopTop();
        addShared();
        LDA #Types.Int
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    subShared()
    {
        Stacks.PopNext();
        SEC
        LDA ZP.NEXTL
        SBC ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        SBC ZP.TOPH
        STA ZP.NEXTH
    }
    
    subB()
    {
        ConsumeOperandA();
        STA TOPL
        LDA # 0
        STA TOPH
        LDA #Types.Byte
        STA TOPT
        subShared();
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    sub()
    {
        Stacks.PopTop();
        subShared();
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    subi()
    {
        Stacks.PopTop();
        subShared();
        LDA #Types.Int
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    mulShared()
    {
        // https://llx.com/Neil/a2/mult.html
        // Initialize RESULT to 0
        LDA # 0
        STA ZP.UWIDE2
        LDX #16      // there are 16 bits in TOP
        loop
        {
            LSR ZP.TOPH  // get low bit of TOP
            ROR ZP.TOPL
            if (C)       // 0 or 1?
            {
                TAY      // if 1, add NUM1 (hi byte of RESULT is in A)
                CLC
                LDA ZP.NEXTL
                ADC ZP.UWIDE2
                STA ZP.UWIDE2
                TYA
                ADC ZP.NEXTH
            }
            ROR A        // "Stairstep" shift
            ROR ZP.UWIDE2
            ROR ZP.UWIDE1
            ROR ZP.UWIDE0
            DEX
            if (Z) { break; }
        }
        STA ZP.UWIDE3
        
        LDA ZP.UWIDE0
        STA ZP.TOPL
        LDA ZP.UWIDE1
        STA ZP.TOPH
        LDA #Types.UInt
        STA ZP.TOPT
    }
    mul()
    {
        // TODO : there is a lot of fast multiply code in v0 (incluing 8x8)
        Stacks.PopTop();
        Stacks.PopNext();
        mulShared();
        Stacks.PushTop();
    }
    divmod()
    {
        // NEXT = NEXT (dividend=result) / TOP (divisor)
        // ACC (remainder)
        
        // https://codebase64.org/doku.php?id=base:16bit_division_16-bit_result
        // https://llx.com/Neil/a2/mult.html
        LDA #0
        STA ZP.ACCL
        STA ZP.ACCH
        LDX #16
        
        loop
        {
            ASL ZP.NEXTL
            ROL ZP.NEXTH
            ROL ZP.ACCL
            ROL ZP.ACCH
            LDA ZP.ACCL
            SEC
            SBC ZP.TOPL
            TAY
            LDA ZP.ACCH
            SBC ZP.TOPH
            if (C)
            {
                STA ZP.ACCH
                STY ZP.ACCL
                INC ZP.NEXTL
            }
            DEX
            if (Z) { break; }
        }
    }
    div()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        // NEXT = NEXT / TOP
        divmod();
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    mod()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        // ACC = NEXT % TOP
        divmod();
        LDA #Types.UInt
        STA ZP.ACCT
        Stacks.PushACC();
    }
    negateNext()
    {
        // NEXT = 0 - NEXT
        SEC
        LDA #0
        SBC ZP.NEXTL
        STA ZP.NEXTL
        LDA #0
        SBC ZP.NEXTH
        STA ZP.NEXTH
    }
    negateTop()
    {
        // TOP = 0 - TOP
        SEC
        LDA #0
        SBC ZP.TOPL
        STA ZP.TOPL
        LDA #0
        SBC ZP.TOPH
        STA ZP.TOPH        
    }
    doSigns() // munts X
    {   
        LDX #0 
        LDA ZP.NEXTH
        ASL // sign bit into carry
        if (C)
        {
            INX // count the -ve
            negateNext(); // NEXT = -NEXT
        }
        LDA ZP.TOPH
        ASL // sign bit into carry
        if (C)
        {
            INX // count the -ve
            negateTop(); // TOP = -TOP
        }
        STX ZP.FSIGN // store the sign count
    }
    muli()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        doSigns(); // munts X
        mulShared();
        LDA ZP.FSIGN     // load the sign count
        CMP #1
        if (Z)           // 0 or 2negatives
        {
            negateTop(); // TOP = -TOP
        }
        LDA #Types.Int
        STA ZP.TOPT
        PushTop();
    }
    divi()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        doSigns(); // munts X
        divmod();
        
        LDA ZP.FSIGN     // load the sign count
        CMP #1
        if (Z)            // 0 or 2negatives
        {
            negateNext(); // NEXT = -NEXT
        }
        LDA #Types.Int
        STA ZP.NEXTT
        PushNext();
    }
    modi()
    {
        // supporting floored division remainder is always positive
        //
        //   dividend = divisor * quotient + remainder
        //    10 /  3 = q  3, r  1
        //   -10 / -3 = q  3, r -1
        //   -10 /  3 = q -3, r -1
        //    10 / -3 = q -3, r -11 ?!
        
        Stacks.PopTop();
        Stacks.PopNext();
        doSigns();
        divmod();
    
        // always leave remainder ACC as positive
        LDA #Types.Int
        STA ZP.ACCT
        PushACC();
    }
    
    eq()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        
        LDX # 0
        LDA ZP.NEXTL
        CMP ZP.TOPL
        if (Z)
        {
            LDA ZP.NEXTH
            CMP ZP.TOPH
            if (Z)
            {
                LDX # 1
            }
        }
        Stacks.PushBool(); // X
    }
    ne()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        
        LDX # 1
        LDA ZP.NEXTL
        CMP ZP.TOPL
        if (Z)
        {
            LDA ZP.NEXTH
            CMP ZP.TOPH
            if (Z)
            {
                LDX # 0
            }
        }
        Stacks.PushBool(); // X
    }
    
    // http://6502.org/tutorials/compare_instructions.html
    ltShared()
    {
        Stacks.PopNext();
        
        LDX #1 // NEXT < TOP   TODO do I have the results the wrong way around here? see Z80 version
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
        }
        if (C) // NEXT < TOP?
        {
            LDX #0 // NEXT >= TOP
        }
        Stacks.PushBool(); // X
    }
    lt()
    {
        Stacks.PopTop();
        ltShared();
    }
    pushILT()
    {
        ConsumeOperand();
        LDA ZP.IDXL
        STA ZP.TOPL
        LDA ZP.IDXH
        STA ZP.TOPH
        LDA #Types.UInt
        STA ZP.TOPT
        leShared();
    }
    leShared()
    {
        Stacks.PopNext();
        LDX #1 // NEXT <= TOP
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
        }
        if (NZ) // NEXT == TOP (not >)?
        {
            if (C) // NEXT <  TOP (not >)?
            {
                LDX #0  // NEXT > TOP
            }
        }
        Stacks.PushBool(); // X 
    }
    le()
    {
        Stacks.PopTop();
        leShared();
    }
    
    pushIBLE()
    {
        ConsumeOperandA();
        STA ZP.TOPL
        LDA # 0 
        STA ZP.TOPH
        LDA #Types.Byte
        STA ZP.TOPT
        leShared();
    }
    
    ge()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        LDX #0 // NEXT < TOP
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
        }
        if (C) // NEXT < TOP?
        {
            LDX #1   
        }
        Stacks.PushBool(); // X   
    }
    gt()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        LDX #0 // NEXT <= TOP
        LDA ZP.NEXTH
        CMP ZP.TOPH
        if (Z)
        {
            LDA ZP.NEXTL
            CMP ZP.TOPL
        }
        if (NZ) // NEXT == TOP (not >) ?
        {
            if (C) // NEXT <  TOP (not >)?
            {
                LDX #1   // NEXT > TOP
            }
        }
        Stacks.PushBool(); // X   
    }
    lti()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        // NEXT < TOP?
        // TOP - NEXT > 0
        SEC
        LDA ZP.TOPL
        SBC ZP.NEXTL
        STA ZP.TOPL
        LDA ZP.TOPH
        SBC ZP.NEXTH
        STA ZP.TOPH
        
        ASL           // sign bit into carry
        
        LDX #0  // TOP <= 0
        loop
        {
            if (C) { break; }
            //  0 or positive
            LDA ZP.TOPL
            if (Z)
            {
                LDA ZP.TOPH
                if (Z) { break; }
            }
            LDX #1
            break;
        }
        Stacks.PushBool(); // X 
    }
    lei()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        // NEXT <= TOP?
        // TOP - NEXT >= 0
        
        SEC
        LDA ZP.TOPL
        SBC ZP.NEXTL
        STA ZP.TOPL
        LDA ZP.TOPH
        SBC ZP.NEXTH
        STA ZP.TOPH
        
        ASL           // sign bit into carry
        
        LDX #0
        if (NC)
        {
            // 0 or positive
            LDX #1
        }
        Stacks.PushBool(); // X 
    }
    gti()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        // NEXT > TOP?
        // NEXT - TOP > 0
        
        SEC
        LDA ZP.NEXTL
        SBC ZP.TOPL
        STA ZP.TOPL
        LDA ZP.NEXTH
        SBC ZP.TOPH
        STA ZP.TOPH
        
        ASL           // sign bit into carry
        
        LDX #0  // TOP <= 0
        loop
        {
            if (C) { break; }
            //  0 or positive
            LDA ZP.TOPL
            if (Z)
            {
                LDA ZP.TOPH
                if (Z) { break; }
            }
            LDX #1
            break;
        }
        Stacks.PushBool(); // X 
    }
    gei()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        // NEXT >= TOP?
        // NEXT - TOP >= 0
        
        SEC
        LDA ZP.NEXTL
        SBC ZP.TOPL
        STA ZP.TOPL
        LDA ZP.NEXTH
        SBC ZP.TOPH
        STA ZP.TOPH
        
        ASL           // sign bit into carry
        
        LDX #0
        if (NC)
        {
            // 0 or positive
            LDX #1
        }
        Stacks.PushBool(); // X 
    }
    cast()
    {
        ConsumeOperandA(); // type -> A
        LDY ZP.SP
        DEY
        STA Address.TypeStackLSB, Y
    }
    bitShr()
    {
        Stacks.PopTop();
        Stacks.PopNext();

        // next = next >> top
        loop
        {
            LDA ZP.TOPL
            if (Z) { break; }
            
            LSR ZP.NEXTH
            ROR ZP.NEXTL
            
            DEC ZP.TOPL
        }
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    bitShl()
    {
        Stacks.PopTop();
        Stacks.PopNext();
        // next = next << top
        loop
        {
            LDA ZP.TOPL
            if (Z) { break; }
            
            ASL ZP.NEXTL
            ROL ZP.NEXTH
            
            DEC ZP.TOPL
        }
        LDA #Types.UInt
        STA ZP.NEXTT
        Stacks.PushNext();
    }
    bitShr8()
    {
        Stacks.PopTop();
        // top = top >> 8
        LDA ZP.TOPH
        STA ZP.TOPL
        LDA #0
        STA ZP.TOPH
        LDA #Types.UInt
        STA ZP.TOPT
        Stacks.PushTop();
    }
    bitShl8()
    {
        Stacks.PopTop();
        // top = top << 8
        LDA ZP.TOPL
        STA ZP.TOPH
        LDA #0
        STA ZP.TOPL
        LDA #Types.UInt
        STA ZP.TOPT
        Stacks.PushTop();
    }
    
    retShared()
    {
        LDX ZP.IDXL
        loop
        {
            CPX #0
            if (Z) { break; }
            DEC ZP.SP
            
            LDY ZP.SP
            checkDecReferenceY();
            
            DEX
        }
        
        Stacks.PopBP();
        LDA ZP.CSP
        if (Z)
        {
            // PC = Address.InvalidAddress; // exit program
            LDA #(Address.InvalidAddress & 0xFF)
            STA ZP.PCL
            LDA #(Address.InvalidAddress >> 8)
            STA ZP.PCH
        }
        else
        {
            Stacks.PopPC();
        }
    }
    ret()
    {
        ConsumeOperand();
        retShared();
    }
    retB()
    {
        ConsumeOperandB();
        retShared();
    }
    ret0()
    {
        LDA # 0 
        STA ZP.IDXL
        STA ZP.IDXH
        retShared();
    }
    
    retResShared()
    {
        Stacks.PopTop();
        
        // SP -= IDX
        LDX ZP.IDXL
        loop
        {
            CPX #0
            if (Z) { break; }
            DEC ZP.SP
            
            LDY ZP.SP
            checkDecReferenceY();
            
            DEX
        }
        
        Stacks.PushTop();
        
        Stacks.PopBP();
        
        LDA ZP.CSP
        if (Z)
        {
            // PC = Address.InvalidAddress; // exit program
            LDA #(Address.InvalidAddress & 0xFF)
            STA ZP.PCL
            LDA #(Address.InvalidAddress >> 8)
            STA ZP.PCH
        }
        else
        {
            Stacks.PopPC();
        }
    }
    retRes()
    {
        ConsumeOperand();
        retResShared();
    }
    retResB()
    {
        ConsumeOperandB();
        retResShared();
    }
    
    jCommon()
    {
        // PC += offset - 3
        CLC
        LDA ZP.PCL
        ADC ZP.IDXL
        STA ZP.PCL
        LDA ZP.PCH
        ADC ZP.IDXH
        STA ZP.PCH
        SEC
        LDA ZP.PCL
        SBC # 3
        STA ZP.PCL
        LDA ZP.PCH
        SBC # 0
        STA ZP.PCH
    }
    
    jw()
    {
        ConsumeOperand();
        jCommon();
    }
    jz()
    {
        Stacks.PopTop();
        LDA ZP.TOPL
        ORA ZP.TOPH
        if (Z)
        {
            ConsumeOperand();
            jCommon();
            return;
        }
        // skip operand
        Utilities.IncPC();
        Utilities.IncPC();
    }
    jnz()
    {
        Stacks.PopTop();
        LDA ZP.TOPL
        ORA ZP.TOPH
        if (NZ)
        {
            ConsumeOperand();
            jCommon();
            return;
        }
        // skip operand
        Utilities.IncPC();
        Utilities.IncPC();
    }
    
    jCommonB()
    {
        // PC += offset - 2
        CLC
        LDA ZP.PCL
        ADC ZP.IDXL
        STA ZP.PCL
        LDA ZP.PCH
        ADC ZP.IDXH
        STA ZP.PCH
        SEC
        LDA ZP.PCL
        SBC # 2
        STA ZP.PCL
        LDA ZP.PCH
        SBC # 0
        STA ZP.PCH
    }
    jb()
    {
        ConsumeOperandSB();
        jCommonB();
    }
    
    jzb()
    {
        Stacks.PopTop();
        LDA ZP.TOPL
        ORA ZP.TOPH
        if (Z)
        {
            ConsumeOperandSB();
            jCommonB();
            return;
        }
        // skip operand
        Utilities.IncPC();
    }
    jnzb()
    {
        Stacks.PopTop();
        LDA ZP.TOPL
        ORA ZP.TOPH
        if (NZ)
        {
            ConsumeOperandSB();
            jCommon();
            return;
        }
        // skip operand
        Utilities.IncPC();
    }
       
    pushI()
    {
        ConsumeOperand();
        LDA ZP.IDXL
        STA ZP.TOPL
        LDA ZP.IDXH
        STA ZP.TOPH
        LDA #Types.UInt
        STA ZP.TOPT
        Stacks.PushTop();
    }
    pushIB()
    {
        ConsumeOperandB();
        LDA ZP.IDXL
        STA ZP.TOPL
        LDA ZP.IDXH
        STA ZP.TOPH
        LDA #Types.Byte
        STA ZP.TOPT
        Stacks.PushTop();
    }
    sysCallB0()
    {
        pushIB();
        SysCall0();
    }
    
    pushI0()
    {
        LDA # 0
        STA ZP.TOPL
        STA ZP.TOPH
        LDA #Types.Byte
        STA ZP.TOPT
        Stacks.PushTop();
    }
    pushI1()
    {
        LDA # 1
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        LDA #Types.Byte
        STA ZP.TOPT
        Stacks.PushTop();
    }
    
    copyNextPop()
    {
        LDA # 1
        STA ZP.CNP
    }
    
    checkIncReferenceY()
    {
        // Y is pointing at the stack slot in question
        LDA Address.TypeStackLSB, Y
        CMP # Types.ReferenceType // C set if heap type, C clear if value type
        if (C)
        {
            LDA Address.ValueStackLSB, Y
            STA IDXL
            LDA Address.ValueStackMSB, Y
            STA IDXH
            GC.AddReference();
        }
    }
    checkDecReferenceY()
    {
        // Y is pointing at the stack slot in question
        LDA Address.TypeStackLSB, Y
        CMP # Types.ReferenceType // C set if heap type, C clear if value type
        if (C)
        {
            LDA Address.ValueStackLSB, Y
            STA IDXL
            LDA Address.ValueStackMSB, Y
            STA IDXH
            GC.Release(); // munts IDY
        }
    }
    
    dup0()
    {
        LDY ZP.SP
        DEY
        
        LDA Address.ValueStackLSB, Y
        STA ZP.TOPL
        LDA Address.ValueStackMSB, Y
        STA ZP.TOPH
        LDA Address.TypeStackLSB, Y
        STA ZP.TOPT
        
        checkIncReferenceY();
        
        PushTop(); 
    }
    
        
    
    
        
    decSP()
    {
        ConsumeOperandA();
        // SP -= A
        TAX
        loop
        {
            CPX #0
            if (Z) { break; }
            DEC ZP.SP
            
            LDY ZP.SP
            checkDecReferenceY();
            
            DEX
        }
    }
    
    pushStackAddr()
    {
        // <int offset operand> pushed to [top] as uint stack address
        ConsumeOperand(); // -> IDX
        // address = BP + operand
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        LDA # Types.Reference
        STA ZP.TOPT
        PushTop();
    }
    
    popCopyShared()
    {
        // slot to pop to is in Y
        // this is the slot we are about to overwrite: decrease reference count if reference type
        LDA Address.TypeStackLSB, Y
        CMP # Types.ReferenceType // C set if heap type, C clear if value type
        if (C)
        {
            checkDecReferenceY();
        }
        
        PopTop();
        LDA ZP.TOPL
        CMP Address.ValueStackLSB, Y
        if (Z)
        {
            LDA ZP.TOPH
            CMP Address.ValueStackMSB, Y
            if (Z)
            {
                // overwriting self - no more to do
                return;
            }
        }
        // clone self, release the original
        LDA ZP.TOPL
        STA IDYL
        LDA ZP.TOPH
        STA IDYH
        LDA ZP.TOPT
        // type is in A
        // reference type to clone is at IDY, resulting clone in IDX
        GC.Clone();
        
        // store the clone
        LDA IDXL
        STA Address.ValueStackLSB, Y
        LDA IDXH
        STA Address.ValueStackMSB, Y
        LDA ZP.TOPT
        STA Address.TypeStackLSB, Y
        
        LDY ZP.SP
        checkDecReferenceY();
    }
    
    popShared()
    {
        // slot to pop to is in Y
        LDA ZP.CNP
        if (NZ)
        {
            LDA # 0
            STA ZP.CNP
            popCopyShared();
            return;
        }
                        
        PopTop();
        
        LDA ZP.TOPL
        STA Address.ValueStackLSB, Y
        LDA ZP.TOPH
        STA Address.ValueStackMSB, Y
        LDA ZP.TOPT
        STA Address.TypeStackLSB, Y
        
        checkDecReferenceY();
    }
    popLocal()
    {
        ConsumeOperand();
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        popShared(); // slot to pop to is in Y
    }
    popLocalB()
    {
        ConsumeOperandB();
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        popShared(); // slot to pop to is in Y
    }
    popLocalB00()
    {
        LDA # 0 
        STA IDXL
        STA IDXH
        
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        popShared(); // slot to pop to is in Y
    }
    popLocalB01()
    {
        LDA # 1
        STA IDXL
        LDA # 0 
        STA IDXH
        
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        popShared(); // slot to pop to is in Y
    }
    
    popGlobal()
    {
        ConsumeOperand();
        CLC
        LDY ZP.IDXL
        
        popShared(); // slot to pop to is in Y
    }
    popGlobalB()
    {
        ConsumeOperandB();
        CLC
        LDY ZP.IDXL
        
        popShared(); // slot to pop to is in Y
    }
    
    popRel()
    {
        ConsumeOperand(); // -> IDX    
        // address of reference = BP + operand
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAX
        // get the actual address from reference address:
        LDY Address.ValueStackLSB, X
        
        popShared(); // slot to pop to is in Y
    }
    popRelB()
    {
        ConsumeOperandB(); // -> IDX    
        // address of reference = BP + operand
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAX
        // get the actual address from reference address:
        LDY Address.ValueStackLSB, X
        
        popShared(); // slot to pop to is in Y
    }
    
    pushShared()
    {
        // slot to push is in Y
        
        LDA Address.ValueStackLSB, Y
        STA ZP.TOPL
        LDA Address.ValueStackMSB, Y
        STA ZP.TOPH
        LDA Address.TypeStackLSB, Y
        STA ZP.TOPT
        
        checkIncReferenceY();
     
        PushTop(); 
    }
    incLocalB()
    {
        ConsumeOperandA();
        CLC
        ADC ZP.BP
        TAY
        
        // slot to INC is in Y
        CLC
        LDA Address.ValueStackLSB, Y
        ADC # 1
        STA Address.ValueStackLSB, Y
        LDA Address.ValueStackMSB, Y
        ADC # 0
        STA Address.ValueStackMSB, Y
        if (NZ)
        {
            LDA # Types.UInt          // just in case it was Types.Byte
            STA Address.TypeStackLSB, Y
        }
    }
    incLocalBB()
    {
        ConsumeOperandA();
        CLC
        ADC ZP.BP
        PHA
        
        
        ConsumeOperandA();
        CLC
        ADC ZP.BP
        TAY
        
        LDA Address.ValueStackLSB, Y
        STA ZP.TOPL
        LDA Address.ValueStackMSB, Y
        STA ZP.TOPH
        // value to add is in TOP
        
        PLA
        TAY
        // slot to add to is in Y
                
        CLC
        LDA Address.ValueStackLSB, Y
        ADC ZP.TOPL
        STA Address.ValueStackLSB, Y
        LDA Address.ValueStackMSB, Y
        ADC ZP.TOPH
        STA Address.ValueStackMSB, Y
        if (NZ)
        {
            LDA # Types.UInt          // just in case it was Types.Byte
            STA Address.TypeStackLSB, Y
        }
        
    }
    pushLocal()
    {
        ConsumeOperand();
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        pushShared(); // slot to push is in Y
    }
    pushLocalB()
    {
        ConsumeOperandB();
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        pushShared(); // slot to push is in Y
    }
    pushLocalBB()
    {
        pushLocalB();
        pushLocalB();
    }
    
    pushLocalB00()
    {
        LDA # 0
        STA IDXL
        STA IDXH
        
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        pushShared(); // slot to push is in Y
    }
    pushLocalB01()
    {
        LDA # 1
        STA IDXL
        LDA # 0
        STA IDXH
        
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        pushShared(); // slot to push is in Y
    }
    
    pushGlobal()
    {
        ConsumeOperand();
        CLC
        LDY ZP.IDXL
        
        pushShared(); // slot to push is in Y
    }
    pushGlobalB()
    {
        ConsumeOperandB();
        CLC
        LDY ZP.IDXL
        
        pushShared(); // slot to push is in Y
    }
    
    pushRel()
    {
        ConsumeOperand(); // -> IDX    
        // address of reference = BP + operand
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAX
        // get the actual address from reference address:
        LDY Address.ValueStackLSB, X
                 
        pushShared(); // slot to push is in Y
    }
        
    call()
    {
        // change CALL to CALLI
        LDA #Instructions.CALLI
        LDY #0
        STA [ZP.ACC], Y
        
        ConsumeOperand();
        PushPC();
        
        // resolve index (IDX) to address (IDX)
        lookupMethod();
        
        // address store back to [ACC] and PC
        LDY #0
        LDA ZP.IDXL
        STA [ZP.ACC], Y
        INY
        STA ZP.PCL
        LDA ZP.IDXH
        STA [ZP.ACC], Y
        STA ZP.PCH
    }
    
    callRel()
    {
        PopIDX();
        
        PushPC();
        
        // resolve index (IDX) to address (IDX)
        lookupMethod();
        
        // IDX -> PC
        LDA ZP.IDXL
        STA ZP.PCL
        LDA ZP.IDXH
        STA ZP.PCH
    }
    
    callI()
    {
        ConsumeOperand();
        Stacks.PushPC();
        
        // IDX -> PC
        LDA ZP.IDXL
        STA ZP.PCL
        LDA ZP.IDXH
        STA ZP.PCH
    }
    
    missing()
    {
        TXA BRK // OpCode not Implemented!
    }
      
    Execute() // munts X
    {
        LDA # (InvalidAddress & 0xFF) // assume that MSB and LSB of InvalidAddress are the same
        CMP PCH
        if (Z)
        {
            CMP PCL
            if (Z)
            {
                return; // end of program run
            }
        }
        
        // ACC = PC + CODESTART
        GetCurrentAddress();
        
        // load current instruction into X
#ifdef CPU_65C02S
        LDA [ZP.ACC]
#else
        LDY #0
        LDA [ZP.ACC], Y
#endif                
        
        Utilities.IncPC();
        
        TAX
        switch (X)
        {
            case Instructions.NOP:
            {
                nop();
            }
            case Instructions.ENTER:
            {
                enter();
            }
            case Instructions.DECSP:
            {
                decSP();
            }
            case Instructions.DUP0:
            {
                dup0();
            }
            case Instructions.PUSHI:
            case Instructions.PUSHD:
            {
                pushI();
            }
            case Instructions.PUSHGP:
            {
                pushI0();
            }
            case Instructions.COPYNEXTPOP:
            {
                copyNextPop();
            }
            case Instructions.PUSHLOCAL:
            {
                pushLocal();
            }
            case Instructions.POPLOCAL:
            {
                popLocal();
            }
            case Instructions.PUSHGLOBAL:
            {
                pushGlobal();
            }
            case Instructions.POPGLOBAL:
            {
                popGlobal();
            }
            case Instructions.PUSHSTACKADDR:
            {
                pushStackAddr();
            }
            case Instructions.PUSHREL:
            {
                pushRel();
            }
            case Instructions.POPREL:
            {
                popRel();
            }
            case Instructions.ADD:
            {
                add();
            }
            case Instructions.ADDI:
            {
                addi();
            }
            case Instructions.SUB:
            {
                sub();
            }
            case Instructions.SUBI:
            {
                subi();
            }
            case Instructions.MUL:
            {
                mul();
            }
            case Instructions.DIV:
            {
                div();
            }
            case Instructions.MOD:
            {
                mod();
            }
            case Instructions.MULI:
            {
                muli();
            }
            case Instructions.DIVI:
            {
                divi();
            }
            case Instructions.MODI:
            {
                modi();
            }
            
            case Instructions.BOOLAND:
            {
                boolAnd();
            }
            case Instructions.BOOLOR:
            {
                boolOr();
            }
            case Instructions.BOOLNOT:
            {
                boolNot();
            }
            case Instructions.BITAND:
            {
                bitAnd();
            }
            case Instructions.BITANDFF:
            {
                bitAndFF();
            }
            case Instructions.BITOR:
            {
                bitOr();
            }
            case Instructions.BITXOR:
            {
                bitXor();
            }
            case Instructions.BITNOT:
            {
                bitNot();
            }
            
            
            case Instructions.EQ:
            {
                eq();
            }
            case Instructions.NE:
            {
                ne();
            }
            case Instructions.LT:
            {
                lt();
            }
            case Instructions.LE:
            {
                le();
            }
            case Instructions.GT:
            {
                gt();
            }
            case Instructions.GE:
            {
                ge();
            }
            case Instructions.LTI:
            {
                lti();
            }
            case Instructions.LEI:
            {
                lei();
            }
            case Instructions.GTI:
            {
                gti();
            }
            case Instructions.GEI:
            {
                gei();
            }
            case Instructions.CAST:
            {
                cast();
            }
            case Instructions.BITSHR:
            {
                bitShr();
            }
            case Instructions.BITSHL:
            {
                bitShl();
            }
            case Instructions.BITSHR8:
            {
                bitShr8();
            }
            case Instructions.BITSHL8:
            {
                bitShl8();
            }
            
            case Instructions.JW:
            {
                jw();
            }
            case Instructions.JZ:
            {
                jz();
            }
            case Instructions.JNZ:
            {
                jnz();
            }
            
            case Instructions.CALL:
            {
                call();
            }
            case Instructions.CALLREL:
            {
                callRel();
            }
            case Instructions.CALLI:
            {
                callI();
            }
            case Instructions.SYSCALL:
            {
                SysCall();
            }
            case Instructions.RET:
            {
                ret();
            }
            case Instructions.RETRES:
            {
                retRes();
            }
            // PACKED_INSTRUCTIONS
            case Instructions.PUSHI0:
            {
#ifdef PACKED_INSTRUCTIONS
                pushI0();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.PUSHI1:
            {
#ifdef PACKED_INSTRUCTIONS
                pushI1();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.RET0:
            {
#ifdef PACKED_INSTRUCTIONS
                ret0();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.POPLOCALB00:
            {
#ifdef PACKED_INSTRUCTIONS
                popLocalB00();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.POPLOCALB01:
            {
#ifdef PACKED_INSTRUCTIONS
                popLocalB01();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.PUSHLOCALB00:
            {
#ifdef PACKED_INSTRUCTIONS
                pushLocalB00();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.PUSHLOCALB01:
            {
#ifdef PACKED_INSTRUCTIONS
                pushLocalB01();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.PUSHIB:
            {
#ifdef PACKED_INSTRUCTIONS
                pushIB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.POPLOCALB:
            {
#ifdef PACKED_INSTRUCTIONS
                popLocalB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.PUSHLOCALB:
            {
#ifdef PACKED_INSTRUCTIONS
                pushLocalB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.PUSHGLOBALB:
            {
#ifdef PACKED_INSTRUCTIONS
                pushGlobalB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.INCLOCALB:
            {
#ifdef PACKED_INSTRUCTIONS
                incLocalB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.SYSCALL0:
            {
#ifdef PACKED_INSTRUCTIONS
                SysCall0();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.RETB:
            {
#ifdef PACKED_INSTRUCTIONS
                retB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.RETRESB:
            {
#ifdef PACKED_INSTRUCTIONS
                retResB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.JZB:
            {
#ifdef PACKED_INSTRUCTIONS
                jzb();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.JB:
            {
#ifdef PACKED_INSTRUCTIONS
                jb();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.ENTERB:
            {
#ifdef PACKED_INSTRUCTIONS
                enterB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.PUSHIBLE:
            {
#ifdef PACKED_INSTRUCTIONS
                pushIBLE();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.PUSHILT:
            {
#ifdef PACKED_INSTRUCTIONS
                pushILT();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.ADDB:
            {
#ifdef PACKED_INSTRUCTIONS
                addB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.SUBB:
            {
#ifdef PACKED_INSTRUCTIONS
                subB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.INCLOCALBB:
            {
#ifdef PACKED_INSTRUCTIONS
                incLocalBB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.PUSHLOCALBB:
            {
#ifdef PACKED_INSTRUCTIONS
                pushLocalBB();
#else
                LDA 0x0A BRK
#endif
            }
            case Instructions.SYSCALLB0:
            {
#ifdef PACKED_INSTRUCTIONS
                sysCallB0();
#else
                LDA 0x0A BRK
#endif
            }
            default:
            {
                missing();
            }
        }
    }
}
