unit Instruction
{
    uses "SysCalls"
    uses "6502/GC"
    
    uses "6502/IntMath"
    
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
        
        SWAP       = 0x43,
        
        PUSHGP     = 0x47,
        
        COPYNEXTPOP = 0x48,
        
        CALLREL    = 0x4B,
        
        CAST       = 0x51,
        
        PUSHD      = 0x60,
        
        JIX        = 0x69,
        
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
        POPRELB      = 0x1D,
        PUSHRELB     = 0x1E,
        POPGLOBALB   = 0x1F,
        PUSHGLOBALB  = 0x20,
        PUSHSTACKADDRB = 0x21,
        INCLOCALB    = 0x22,
        DECLOCALB    = 0x23,
        INCLOCALIB   = 0xA4,
        INCGLOBALB   = 0x53,
        DECGLOBALB   = 0x54,
        
        SYSCALL0     = 0x24,
        SYSCALL1     = 0x25,
        SYSCALL2     = 0x0B,
        RETB         = 0x2A,
        RETRESB      = 0x2B,
        JZB          = 0x2E,
        JNZB         = 0x2F,
        JB           = 0x30,
        PUSHILT      = 0x55,
        ENTERB       = 0x5F,
        RETFAST      = 0x61,
        PUSHILEI     = 0x65,
        PUSHIBLE     = 0x6B,
        PUSHIBEQ     = 0x6C,
        ADDB         = 0x6D,
        SUBB         = 0x6E,
        
        
        INCLOCALBB   = 0x3F,
        INCLOCALIBB  = 0xA3,
        PUSHLOCALBB  = 0x56,
        SYSCALLB0    = 0xA8,
        SYSCALL00    = 0xA9,
        PUSHIBB      = 0xAA,
        SYSCALLB1    = 0xAB,
        SYSCALL01    = 0xAC,
        SYSCALL10    = 0xAD,
        
        //JIXB       = 0x68,
        
        
        PUSHI0       = 0x44,
        PUSHI1       = 0x45,
        PUSHIM1      = 0x46,
        RET0         = 0x4A,
        POPLOCALB00  = 0x4C,
        POPLOCALB01  = 0x4D,
        PUSHLOCALB00 = 0x4E,
        PUSHLOCALB01 = 0x4F,
        
        POPCOPYLOCALB   = 0x57,
        POPCOPYRELB     = 0x58,
        POPCOPYGLOBALB  = 0x59,
        
        POPCOPYLOCALB00 = 0x5D,
        POPCOPYLOCALB01 = 0x5E,
    }
    
    return1()
    {
        LDA #1
    }
    return2()
    {
        LDA #2
    }
    return3()
    {
        LDA #3
    }
#ifdef CHECKED    
    lengthMissing()
    {
        TXA  // operand length not implemented!
        Diagnostics.Die();
    }
#endif
        
    // get the width of the current instruction (in A), in A (opCode + operands)
    GetInstructionWidth() // munts X
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
            case Instructions.SWAP:
            
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
            case Instructions.PUSHIM1:
            case Instructions.RET0:
            case Instructions.RETFAST:
            case Instructions.POPLOCALB00:
            case Instructions.POPLOCALB01:
            case Instructions.PUSHLOCALB00:
            case Instructions.PUSHLOCALB01:
            case Instructions.POPCOPYLOCALB00:
            case Instructions.POPCOPYLOCALB01:
            {
                return1();
            }
            case Instructions.SYSCALL:
            case Instructions.CAST:
            case Instructions.DECSP:
            
            // PACKED_INSTRUCTIONS
            case Instructions.PUSHIB:
            case Instructions.POPLOCALB:
            case Instructions.POPGLOBALB:
            case Instructions.PUSHLOCALB:
            case Instructions.PUSHGLOBALB:
            case Instructions.INCLOCALB:
            case Instructions.DECLOCALB:
            case Instructions.DECGLOBALB:
            case Instructions.INCGLOBALB:
            case Instructions.INCLOCALIB:
            case Instructions.SYSCALL0:
            case Instructions.SYSCALL1:
            case Instructions.SYSCALL2:
            case Instructions.RETB:
            case Instructions.RETRESB:
            case Instructions.JZB:
            case Instructions.JNZB:
            case Instructions.JB:
            case Instructions.ENTERB:
            case Instructions.PUSHIBLE:
            case Instructions.PUSHIBEQ:
            case Instructions.ADDB:
            case Instructions.SUBB:
            case Instructions.POPCOPYLOCALB:
            case Instructions.POPCOPYRELB:
            case Instructions.POPCOPYGLOBALB:
            case Instructions.PUSHSTACKADDRB:
            case Instructions.PUSHRELB:
            case Instructions.POPRELB:
            {
                return2();
            }
            case Instructions.CALL:
            case Instructions.CALLI:
            
            case Instructions.RET:
            case Instructions.RETRES:

            case Instructions.JZ:
            case Instructions.JNZ:
            case Instructions.JW:
            case Instructions.JIX:
            
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
            case Instructions.INCLOCALIBB:
            case Instructions.PUSHLOCALBB:
            case Instructions.SYSCALLB0:
            case Instructions.SYSCALLB1:
            case Instructions.SYSCALL00:
            case Instructions.SYSCALL01:
            case Instructions.SYSCALL10:
            case Instructions.PUSHIBB:
            case Instructions.PUSHILT:
            case Instructions.PUSHILEI:
            //case Instructions.JIXB:
            {
                return3();
            }
            default:
            {
#ifdef CHECKED                
                lengthMissing();
#else
                return3();
#endif
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
        
        // get the width of the current instruction (in A), in A (opCode + operands)
        GetInstructionWidth(); // munts X
        
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
            ADC # 4
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
        
#ifdef CPU_65C02S
        STZ ZP.IDXH
        LDA [ZP.ACC]
        STA ZP.IDXL
#else        
        LDY #0
        STY ZP.IDXH
        LDA [ZP.ACC], Y
        STA ZP.IDXL
#endif
        
        Utilities.IncPC();
    }
    // load the operand into IDX, increment the PC by 1
    ConsumeOperandSB()
    {
        Utilities.IncACC();
        
#ifdef CPU_65C02S
        STZ ZP.IDXH
        LDA [ZP.ACC]
        STA ZP.IDXL
#else        
        LDY #0
        STY ZP.IDXH
        LDA [ZP.ACC], Y
        STA ZP.IDXL
#endif
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
#ifdef CPU_65C02S        
        LDA [ZP.ACC]
#else
        LDY # 0
        LDA [ZP.ACC], Y
#endif
        Utilities.IncPC();
    }
    
       
    nop()
    {
        NOP
    }
       
    enter()
    {
        Stacks.PushBP(); // munts X, A
        LDA ZP.SP
        STA ZP.BP
    }
    enterB()
    {
        Stacks.PushBP(); // munts X, A
        LDA ZP.SP
        STA ZP.BP
        
#ifdef CPU_65C02S
        STZ ZP.TOPL
        STZ ZP.TOPH
#else        
        LDA # 0
        STA ZP.TOPL
        STA ZP.TOPH
#endif
            
        ConsumeOperandA();
        TAX
        loop
        {
            if (Z) { break; }
            LDA #Types.Byte
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
        
        LDA #Types.Bool
        Stacks.PushTop();
    }
    boolOr()
    {
        Stacks.PopTopNext();
        
        // [next] || [top] -> [top] // assumes Type.Bool (0 or 1)
        LDA ZP.NEXTL
        ORA ZP.TOPL
        STA ZP.NEXTL
        
        LDA #Types.Bool
        Stacks.PushNext();
    }
    boolAnd()
    {
        Stacks.PopTopNext();
        
        // [next] && [top] -> [top] // assumes Type.Bool (0 or 1)
        LDA ZP.NEXTL
        AND ZP.TOPL
        STA ZP.NEXTL
        
        LDA #Types.Bool
        Stacks.PushNext();
    }
    bitAnd()
    {
        Stacks.PopTopNext();
        
        // [next] &  [top] -> [top]
        LDA ZP.NEXTL
        AND ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        AND ZP.TOPH
        STA ZP.NEXTH
        
        LDA #Types.UInt
        Stacks.PushNext();
    }
    bitAndFF()
    {
        Stacks.PopTop();
        // [top] &  0xFF -> [top]
        LDA # 0
        STA ZP.TOPH
        LDA #Types.UInt
        Stacks.PushTop();
    }
    bitOr()
    {
        Stacks.PopTopNext();
        
        // [next] |  [top] -> [top]
        LDA ZP.NEXTL
        ORA ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        ORA ZP.TOPH
        STA ZP.NEXTH
        
        LDA #Types.UInt
        Stacks.PushNext();
    }
    bitXor()
    {
        Stacks.PopTopNext();
        
        // [next] ^  [top] -> [top]
        LDA ZP.NEXTL
        EOR ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        EOR ZP.TOPH
        STA ZP.NEXTH
        
        LDA #Types.UInt
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
        Stacks.PushNext();
    }
    addi()
    {
        Stacks.PopTop();
        addShared();
        LDA #Types.Int
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
        Stacks.PushNext();
    }
    sub()
    {
        Stacks.PopTop();
        subShared();
        LDA #Types.UInt
        Stacks.PushNext();
    }
    subi()
    {
        Stacks.PopTop();
        subShared();
        LDA #Types.Int
        Stacks.PushNext();
    }
      
    eqShared()
    {
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
        Stacks.PushX(); // X
    }      
    eq()
    {
        Stacks.PopTop();
        eqShared();
    }
    pushIBEQ()
    {
        ConsumeOperandA();
        STA ZP.TOPL
        LDA # 0 
        STA ZP.TOPH
        LDA #Types.Byte
        STA ZP.TOPT
        eqShared();
    }
    ne()
    {
        Stacks.PopTopNext();
        
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
        Stacks.PushX(); // X
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
        Stacks.PushX(); // X
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
        Stacks.PushX(); // as Type.Bool
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
        Stacks.PopTopNext();
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
        Stacks.PushX(); // as Type.Bool  
    }
    gt()
    {
        Stacks.PopTopNext();
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
        Stacks.PushX(); // as Type.Bool  
    }
    lti()
    {
        Stacks.PopTopNext();
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
        Stacks.PushX(); // as Type.Bool
    }
    leiShared()
    {
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
        Stacks.PushX(); // as Type.Bool
    }
    lei()
    {
        Stacks.PopTop();
        leiShared();
    }
    pushILEI()
    {
        ConsumeOperand();
        LDA ZP.IDXL
        STA ZP.TOPL
        LDA ZP.IDXH
        STA ZP.TOPH
        LDA #Types.Int
        STA ZP.TOPT
        leiShared();
    }
    
    gti()
    {
        Stacks.PopTopNext();
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
        Stacks.PushX(); // as Type.Bool
    }
    gei()
    {
        Stacks.PopTopNext();
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
        Stacks.PushX(); // as Type.Bool
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
        Stacks.PopTopNext();

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
        Stacks.PushNext();
    }
    bitShl()
    {
        Stacks.PopTopNext();
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
        
        Stacks.PopBP(); // munts Y, A
        LDA ZP.CSP
        if (Z)
        {
#ifdef CPU_65C02S
            SMB6 ZP.FLAGS // set ProgramExited
#else
            LDA ZP.FLAGS
            ORA # 0b01000000
            STA ZP.FLAGS
#endif
        }
        else
        {
            Stacks.PopPC(); // munts Y, A
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
    retFast()
    {
        Stacks.PopPC();  // munts Y, A
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
        
        LDA ZP.TOPT
        Stacks.PushTop();
        
        Stacks.PopBP();  // munts Y, A
        
        LDA ZP.CSP
        if (Z)
        {
#ifdef CPU_65C02S
            SMB6 ZP.FLAGS // set ProgramExited
#else
            LDA ZP.FLAGS
            ORA # 0b01000000
            STA ZP.FLAGS
#endif
        }
        else
        {
            Stacks.PopPC();  // munts Y, A
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
    
    jixArguments()
    {
        Stacks.PopA();             // switchCase -> FVALUEL
        STA FVALUEL
             
        ConsumeOperand();
        LDA IDXL
        STA TOPL            // minRange -> TOPL
        LDA IDXH
        STA TOPH            // maxRange -> TOPL
        
        Utilities.IncACC();
        ConsumeOperand();
        LDA IDXL
        STA NEXTL           // -offset -> NEXT
        LDA IDXH
        STA NEXTH
        
        Utilities.IncACC(); // move to start of table
        Utilities.IncACC();
    }
    
    jixUseTable()
    {
        // switchCase = switchCase - minRange
        SEC
        LDA FVALUEL
        SBC TOPL
        STA FVALUEL
        
        // index = pc + switchCase x 2
        CLC
        LDA ACCL
        ADC FVALUEL
        STA IDXL
        LDA ACCH
        ADC # 0
        STA IDXH
        
        CLC
        LDA IDXL
        ADC FVALUEL
        STA IDXL
        if (C)
        {
            INC IDXH
        }
                
        // offset = code[index]
        LDY # 0
        LDA [IDX], Y
        STA IDYL
        INY
        LDA [IDX], Y
        STA IDYH
        
        // zero offset?
        if (Z)
        {
            LDA IDYL
            if (Z)
            {
                jixDefault();
                return;
            }
        }
        
        // pc = pc - jumpBackOffset - 5 + offset
        SEC
        LDA PCL
        SBC NEXTL
        STA PCL
        LDA PCH
        SBC NEXTH
        STA PCH
        
        SEC
        LDA PCL
        SBC # 5
        STA PCL
        LDA PCH
        SBC # 0
        STA PCH
        
        CLC
        LDA PCL
        ADC IDYL
        STA PCL
        LDA PCH
        ADC IDYH
        STA PCH
    }
    jixbUseTable()
    {
        // switchCase = switchCase - minRange
        SEC
        LDA FVALUEL
        SBC TOPL
        STA FVALUEL
        
        // index = pc + switchCase
        CLC
        LDA ACCL
        ADC FVALUEL
        STA IDXL
        LDA ACCH
        ADC #0
        STA IDXH
        
        // offset = code[index]
#ifdef CPU_65C02S
        LDA [IDX]
#else        
        LDY # 0
        LDA [IDX], Y
#endif
        STA IDYL
        
        // zero offset?
        if (Z)
        {
            jixbDefault();
            return;
        }
        
        // pc = pc - jumpBackOffset - 5 + offset
        SEC
        LDA PCL
        SBC NEXTL
        STA PCL
        LDA PCH
        SBC NEXTH
        STA PCH
        
        SEC
        LDA PCL
        SBC # 5
        STA PCL
        LDA PCH
        SBC # 0
        STA PCH
        
        CLC
        LDA PCL
        ADC IDYL
        STA PCL
        if (C)
        {
            INC PCH
        }
    }
    
    jixDefault()
    {
        // default: simply add PC to tableSize
        // tableSize = (maxRange + 1 - minRange)
        LDA TOPH
#ifdef CPU_65C02S        
        INC
#else
        CLC
        ADC # 1
#endif
        SEC
        SBC TOPL
        
        STA ACCH // tableSize in slots (2 byte slots for JIXW)
        
        // + tableSize x 2
        CLC
        LDA PCL
        ADC ACCH
        STA PCL
        if (C)
        {
            INC PCH
        }
                
        CLC
        LDA PCL
        ADC ACCH
        STA PCL
        if (C)
        {
            INC PCH
        }
        jCommon(); // PC += offset - 3
    }
    /*
    jixbDefault()
    {
        // default: simply add PC to tableSize
        // tableSize = (maxRange + 1 - minRange)
        LDA TOPH
#ifdef CPU_65C02S        
        INC
#else
        CLC
        ADC # 1
#endif
        SEC
        SBC TOPL
        // tableSize is in A
        
        CLC
        // tableSize is in A
        ADC PCL
        STA PCL
        if (C)
        {
            INC PCH
        }
        jCommon(); // PC += offset - 3
    }
    */
    jix()
    {
#ifdef JIX_INSTRUCTIONS
        jixArguments();
        
        LDA FVALUEL
        CMP TOPL
        if (C)                     // switchCase >= minRange
        {
            CMP TOPH
            if (Z)                 // switchCase == maxRange
            {
                jixUseTable();
                return;
            }
            if (NC)                // switchCase < maxRange
            {
                jixUseTable();
                return;
            }
        }
        // default: simply add PC to tableSize
        jixDefault();
#else
        LDA 0x0A // no JIX instructions
        Diagnostics.Die();
#endif
    }
    /*
    jixb()
    {
#ifdef JIX_INSTRUCTIONS
        jixArguments();
        
        LDA FVALUEL
        CMP TOPL
        if (C)                         // switchCase >= minRange
        {
            CMP TOPH
            if (Z)                     // switchCase == maxRange
            {
                jixbUseTable();
                return;
            }
            if (NC)                    // switchCase < maxRange
            {
                jixbUseTable();
                return;
            }
        }
        // default: simply add PC to tableSize
        jixbDefault();
#else
        LDA 0x0A // no JIX instructions
        Diagnostics.Die();
#endif
    }
    */
    
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
            jCommonB();
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
        Stacks.PushTop();
    }
    pushA()
    {
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        LDA #Types.Byte
        Stacks.PushTop();
    }
    sysCallB0()
    {
        ConsumeOperand(); // -> IDX
        LDA ZP.IDXL
        pushA();
        LDA # 0
        STA ACCL
        
        LDA IDXH
        TAX
     
        // iOverload in ACCL
        // iSysCall  in X   
        SysCallShared();
    }
    sysCallB1()
    {
        ConsumeOperand(); // -> IDX
        LDA ZP.IDXL
        pushA();
        LDA # 1
        STA ACCL
        
        LDA IDXH
        TAX
     
        // iOverload in ACCL
        // iSysCall  in X   
        SysCallShared();
    }
    sysCall00()
    {
        ConsumeOperand(); // -> IDX
        LDA ZP.IDXH
        STA ZP.W2
        LDA # 0
        STA ZP.ACCL
        LDX ZP.IDXL
        
        // iOverload in ACCL
        // iSysCall  in X   
        SysCallShared();
        
        LDA # 0
        STA ZP.ACCL
        LDX ZP.W2
     
        // iOverload in ACCL
        // iSysCall  in X   
        SysCallShared();
    }
    sysCall01()
    {
        ConsumeOperand(); // -> IDX
        LDA ZP.IDXH
        STA ZP.W2
        LDA # 0
        STA ZP.ACCL
        LDX ZP.IDXL
        
        // iOverload in ACCL
        // iSysCall  in X   
        SysCallShared();
        
        LDA # 1
        STA ZP.ACCL
        LDX ZP.W2
     
        // iOverload in ACCL
        // iSysCall  in X   
        SysCallShared();
    }
    sysCall10()
    {
        ConsumeOperand(); // -> IDX
        LDA ZP.IDXH
        STA ZP.W2
        LDA # 1
        STA ZP.ACCL
        LDX ZP.IDXL
        
        // iOverload in ACCL
        // iSysCall  in X   
        SysCallShared();
        
        LDA # 0
        STA ZP.ACCL
        LDX ZP.W2
     
        // iOverload in ACCL
        // iSysCall  in X   
        SysCallShared();
    }
    pushIBB()
    {
        ConsumeOperand(); // -> IDX
        LDA ZP.IDXL
        pushA();
        LDA ZP.IDXH
        pushA();
    }
    
    pushI0()
    {
        LDA # 0
        STA ZP.TOPL
        STA ZP.TOPH
        LDA #Types.Byte
        Stacks.PushTop();
    }
    pushI1()
    {
        LDA # 1
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        LDA #Types.Byte
        Stacks.PushTop();
    }
    pushIM1()
    {
        LDA # 0xFF
        STA ZP.TOPL
        STA ZP.TOPH
        LDA #Types.Int
        Stacks.PushTop();
    }
    
    copyNextPop()
    {
        LDA # 1
        STA ZP.CNP
    }
    
    checkIncReferenceY() // munts IDX, A
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
    checkDecReferenceY() // munts IDX, A
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
    swap()
    {
        Stacks.PopTopNext();
        LDA ZP.TOPT
        Stacks.PushTop();
        LDA ZP.NEXTT
        Stacks.PushNext();
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
        
        LDA ZP.TOPT
        Stacks.PushTop(); 
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
        Stacks.PushTop();
    }
    pushStackAddrB()
    {
        // <int offset operand> pushed to [top] as uint stack address
        ConsumeOperandA(); // -> A
        // address = BP + operand
        CLC
        ADC ZP.BP
        
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        LDA # Types.Reference
        Stacks.PushTop();
    }
    
    popCopyShared()
    {
        // slot to pop to is in Y
        // this is the slot we are about to overwrite: decrease reference count if reference type
        checkDecReferenceY();
        
        Stacks.PopTop();
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
                        
        Stacks.PopTop();
        
        // this is the slot we are about to overwrite: decrease reference count if reference type
        checkDecReferenceY();
        
        LDA ZP.TOPL
        STA Address.ValueStackLSB, Y
        LDA ZP.TOPH
        STA Address.ValueStackMSB, Y
        LDA ZP.TOPT
        STA Address.TypeStackLSB, Y
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
    popCopyLocalB()
    {
        ConsumeOperandB();
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        popCopyShared(); // slot to pop to is in Y
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
    popCopyLocalB00()
    {
        LDA # 0 
        STA IDXL
        STA IDXH
        
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        popCopyShared(); // slot to pop to is in Y
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
    popCopyLocalB01()
    {
        LDA # 1
        STA IDXL
        LDA # 0 
        STA IDXH
        
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        
        popCopyShared(); // slot to pop to is in Y
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
    popCopyGlobalB()
    {
        ConsumeOperandB();
        CLC
        LDY ZP.IDXL
        
        popCopyShared(); // slot to pop to is in Y
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
        ConsumeOperandA(); // -> A
        // address of reference = BP + operand
        CLC
        ADC ZP.BP
        TAX
        // get the actual address from reference address:
        LDY Address.ValueStackLSB, X
        
        popShared(); // slot to pop to is in Y
    }
    popCopyRelB()
    {
        ConsumeOperandA(); // -> A
        // address of reference = BP + operand
        CLC
        ADC ZP.BP
        TAX
        // get the actual address from reference address:
        LDY Address.ValueStackLSB, X
        
        popCopyShared(); // slot to pop to is in Y
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
        
        LDA ZP.TOPT
        Stacks.PushTop(); 
    }
    commonInc()
    {
        INC Address.ValueStackLSB, X
        if (Z)
        {
            INC Address.ValueStackMSB, X
            LDA # Types.UInt          // just in case it was Types.Byte
            STA Address.TypeStackLSB, X
        }
    }
    incLocalB()
    {
        ConsumeOperandA();
        CLC
        ADC ZP.BP
        TAX // slot to INC is in X
        commonInc();
    }
    incGlobalB()
    {
        ConsumeOperandB();
        LDX ZP.IDXL // slot to INC is in X
        commonInc();
    }
    
    decLocalB()
    {
        ConsumeOperandA();
        CLC
        ADC ZP.BP
        TAY
        
        // slot to DEC is in Y
        SEC
        LDA Address.ValueStackLSB, Y
        SBC # 1
        STA Address.ValueStackLSB, Y
        LDA Address.ValueStackMSB, Y
        SBC # 0
        STA Address.ValueStackMSB, Y
    }
    incLocalIB()
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
            LDA # Types.Int          // just in case it was Types.Byte
            STA Address.TypeStackLSB, Y
        }
    }
    incLocalBB()
    {
        ConsumeOperand(); // -> IDX
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY 
        // slot to add to is in Y
        
        CLC
        LDA ZP.IDXH
        ADC ZP.BP
        TAX
        
        LDA Address.ValueStackLSB, X
        STA ZP.TOPL
        LDA Address.ValueStackMSB, X
        STA ZP.TOPH
        // value to add is in TOP
        
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
    incLocalIBB()
    {
        ConsumeOperand(); // -> IDX
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY 
        // slot to add to is in Y
        
        CLC
        LDA ZP.IDXH
        ADC ZP.BP
        TAX
        
        LDA Address.ValueStackLSB, X
        STA ZP.TOPL
        LDA Address.ValueStackMSB, X
        STA ZP.TOPH
        // value to add is in TOP
        
        CLC
        LDA Address.ValueStackLSB, Y
        ADC ZP.TOPL
        STA Address.ValueStackLSB, Y
        LDA Address.ValueStackMSB, Y
        ADC ZP.TOPH
        STA Address.ValueStackMSB, Y
        LDA # Types.Int
        STA Address.TypeStackLSB, Y
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
        ConsumeOperand(); // -> IDX
        LDA IDXH
        STA ACCL
        CLC
        LDA ZP.IDXL
        ADC ZP.BP
        TAY
        pushShared(); // slot to push is in Y, munts IDX, A
        
        CLC
        LDA ACCL
        ADC ZP.BP
        TAY
        pushShared(); // slot to push is in Y, munts IDX, A
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
    
    decGlobalB()
    {
        ConsumeOperandB();
        LDY ZP.IDXL
        
        // slot to DEC is in Y
        SEC
        LDA Address.ValueStackLSB, Y
        SBC # 1
        STA Address.ValueStackLSB, Y
        LDA Address.ValueStackMSB, Y
        SBC # 0
        STA Address.ValueStackMSB, Y
    }
    
    
    pushGlobal()
    {
        ConsumeOperand();
        LDY ZP.IDXL
        
        pushShared(); // slot to push is in Y
    }
    pushGlobalB()
    {
        ConsumeOperandB();
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
    pushRelB()
    {
        ConsumeOperandA(); // -> A    
        // address of reference = BP + operand
        CLC
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
#ifdef CPU_65C02S
        STA [ZP.ACC]
#else        
        LDY #0
        STA [ZP.ACC], Y
#endif
        
        ConsumeOperand();
        Stacks.PushPC(); // munts Y, A
        
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
        Stacks.PopIDX();
        
        Stacks.PushPC();  // munts Y, A
        
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
        Stacks.PushPC();  // munts Y, A
        
        // IDX -> PC
        LDA ZP.IDXL
        STA ZP.PCL
        LDA ZP.IDXH
        STA ZP.PCH
    }
    
    missing()
    {
#ifdef CHECKED        
        TXA // OpCode not Implemented!
        Diagnostics.Die();
#endif
    }
      
    Execute() // munts X
    {
#ifdef CPU_65C02S        
        if (BBS6, ZP.FLAGS) // is ProgramExited set?
        {
            return;
        }
#else
        BIT ZP.FLAGS
        if (V) // is ProgramExited set?
        {
            return;
        }
#endif
        // ACC = PC + CODESTART
#ifdef INLINE_EXPANSIONS
        CLC
        LDA ZP.PCL
        ADC ZP.CODESTARTL
        STA ZP.ACCL
        LDA ZP.PCH
        ADC ZP.CODESTARTH
        STA ZP.ACCH
#else
        GetCurrentAddress();
#endif
        
        // load current instruction into X
#ifdef CPU_65C02S
        LDA [ZP.ACC]
#else
        LDY #0
        LDA [ZP.ACC], Y
#endif                

#ifdef INLINE_EXPANSIONS
        INC ZP.PCL
        if (Z)
        {
            INC ZP.PCH
        }
#else                
        Utilities.IncPC();
#endif
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
            case Instructions.SWAP:
            {
                swap();
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
                Mul();
            }
            case Instructions.DIV:
            {
                Div();
            }
            case Instructions.MOD:
            {
                Mod();
            }
            case Instructions.MULI:
            {
                MulI();
            }
            case Instructions.DIVI:
            {
                DivI();
            }
            case Instructions.MODI:
            {
                ModI();
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
            case Instructions.JIX:
            {
                jix();
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
                missing();
#endif
            }
            case Instructions.PUSHI1:
            {
#ifdef PACKED_INSTRUCTIONS
                pushI1();
#else
                missing();
#endif
            }
            case Instructions.PUSHIM1:
            {
#ifdef PACKED_INSTRUCTIONS
                pushIM1();
#else
                missing();
#endif
            }
            case Instructions.RET0:
            {
#ifdef PACKED_INSTRUCTIONS
                ret0();
#else
                missing();
#endif
            }
            case Instructions.RETFAST:
            {
#ifdef PACKED_INSTRUCTIONS
                retFast();
#else
                missing();
#endif
            }
            case Instructions.POPLOCALB00:
            {
#ifdef PACKED_INSTRUCTIONS
                popLocalB00();
#else
                missing();
#endif
            }
            case Instructions.POPLOCALB01:
            {
#ifdef PACKED_INSTRUCTIONS
                popLocalB01();
#else
                missing();
#endif
            }
            case Instructions.POPCOPYLOCALB00:
            {
#ifdef PACKED_INSTRUCTIONS
                popCopyLocalB00();
#else
                missing();
#endif
            }
            case Instructions.POPCOPYLOCALB01:
            {
#ifdef PACKED_INSTRUCTIONS
                popCopyLocalB01();
#else
                missing();
#endif
            }
            case Instructions.PUSHLOCALB00:
            {
#ifdef PACKED_INSTRUCTIONS
                pushLocalB00();
#else
                missing();
#endif
            }
            case Instructions.PUSHLOCALB01:
            {
#ifdef PACKED_INSTRUCTIONS
                pushLocalB01();
#else
                missing();
#endif
            }
            case Instructions.PUSHRELB:
            {
#ifdef PACKED_INSTRUCTIONS
                pushRelB();
#else
                missing();
#endif                
            }
            case Instructions.POPRELB:
            {
#ifdef PACKED_INSTRUCTIONS
                popRelB();
#else
                missing();
#endif                
            }
            case Instructions.PUSHIB:
            {
#ifdef PACKED_INSTRUCTIONS
                pushIB();
#else
                missing();
#endif
            }
            case Instructions.POPLOCALB:
            {
#ifdef PACKED_INSTRUCTIONS
                popLocalB();
#else
                missing();
#endif
            }
            case Instructions.POPGLOBALB:
            {
#ifdef PACKED_INSTRUCTIONS
                popGlobalB();
#else
                missing();
#endif
            }
            
            case Instructions.PUSHLOCALB:
            {
#ifdef PACKED_INSTRUCTIONS
                pushLocalB();
#else
                missing();
#endif
            }
            case Instructions.PUSHGLOBALB:
            {
#ifdef PACKED_INSTRUCTIONS
                pushGlobalB();
#else
                missing();
#endif
            }
            case Instructions.INCGLOBALB:
            {
#ifdef PACKED_INSTRUCTIONS
                incGlobalB();
#else
                missing();
#endif
            }
            case Instructions.INCLOCALB:
            {
#ifdef PACKED_INSTRUCTIONS
                incLocalB();
#else
                missing();
#endif
            }
            case Instructions.DECLOCALB:
            {
#ifdef PACKED_INSTRUCTIONS
                decLocalB();
#else
                missing();
#endif
            }
            case Instructions.DECGLOBALB:
            {
#ifdef PACKED_INSTRUCTIONS
                decGlobalB();
#else
                missing();
#endif
            }
            case Instructions.INCLOCALIB:
            {
#ifdef PACKED_INSTRUCTIONS
                incLocalIB();
#else
                missing();
#endif
            }
            case Instructions.SYSCALL0:
            {
#ifdef PACKED_INSTRUCTIONS
                SysCall0();
#else
                missing();
#endif
            }
            case Instructions.SYSCALL1:
            {
#ifdef PACKED_INSTRUCTIONS
                SysCall1();
#else
                missing();
#endif
            }
            case Instructions.SYSCALL2:
            {
#ifdef PACKED_INSTRUCTIONS
                SysCall2();
#else
                missing();
#endif
            }
            case Instructions.RETB:
            {
#ifdef PACKED_INSTRUCTIONS
                retB();
#else
                missing();
#endif
            }
            case Instructions.RETRESB:
            {
#ifdef PACKED_INSTRUCTIONS
                retResB();
#else
                missing();
#endif
            }
            case Instructions.JZB:
            {
#ifdef PACKED_INSTRUCTIONS
                jzb();
#else
                missing();
#endif
            }
            case Instructions.JNZB:
            {
#ifdef PACKED_INSTRUCTIONS
                jnzb();
#else
                missing();
#endif
            }
            case Instructions.JB:
            {
#ifdef PACKED_INSTRUCTIONS
                jb();
#else
                missing();
#endif
            }
            /*
            case Instructions.JIXB:
            {
#ifdef PACKED_INSTRUCTIONS
                jixb();
#else
                missing();
#endif
            }
            */
            case Instructions.ENTERB:
            {
#ifdef PACKED_INSTRUCTIONS
                enterB();
#else
                missing();
#endif
            }
            case Instructions.PUSHIBLE:
            {
#ifdef PACKED_INSTRUCTIONS
                pushIBLE();
#else
                missing();
#endif
            }
            case Instructions.PUSHIBEQ:
            {
#ifdef PACKED_INSTRUCTIONS
                pushIBEQ();
#else
                missing();
#endif
            }
            case Instructions.PUSHILT:
            {
#ifdef PACKED_INSTRUCTIONS
                pushILT();
#else
                missing();
#endif
            }
            case Instructions.PUSHILEI:
            {
#ifdef PACKED_INSTRUCTIONS
                pushILEI();
#else
                missing();
#endif
            }
            
            case Instructions.ADDB:
            {
#ifdef PACKED_INSTRUCTIONS
                addB();
#else
                missing();
#endif
            }
            case Instructions.SUBB:
            {
#ifdef PACKED_INSTRUCTIONS
                subB();
#else
                missing();
#endif
            }
            case Instructions.INCLOCALBB:
            {
#ifdef PACKED_INSTRUCTIONS
                incLocalBB();
#else
                missing();
#endif
            }
            case Instructions.INCLOCALIBB:
            {
#ifdef PACKED_INSTRUCTIONS
                incLocalIBB();
#else
                missing();
#endif
            }
            case Instructions.PUSHLOCALBB:
            {
#ifdef PACKED_INSTRUCTIONS
                pushLocalBB();
#else
                missing();
#endif
            }
            case Instructions.SYSCALLB0:
            {
#ifdef PACKED_INSTRUCTIONS
                sysCallB0();
#else
                missing();
#endif
            }
            case Instructions.SYSCALLB1:
            {
#ifdef PACKED_INSTRUCTIONS
                sysCallB1();
#else
                missing();
#endif
            }
            case Instructions.SYSCALL00:
            {
#ifdef PACKED_INSTRUCTIONS
                sysCall00();
#else
                missing();
#endif
            }
            case Instructions.SYSCALL10:
            {
#ifdef PACKED_INSTRUCTIONS
                sysCall10();
#else
                missing();
#endif
            }
            case Instructions.SYSCALL01:
            {
#ifdef PACKED_INSTRUCTIONS
                sysCall01();
#else
                missing();
#endif
            }
            case Instructions.PUSHIBB:
            {
#ifdef PACKED_INSTRUCTIONS
                pushIBB();
#else
                missing();
#endif
            }
            case Instructions.POPCOPYLOCALB:
            {
#ifdef PACKED_INSTRUCTIONS
                popCopyLocalB();
#else
                missing();
#endif
            }
            
            case Instructions.POPCOPYGLOBALB:
            {
#ifdef PACKED_INSTRUCTIONS
                popCopyGlobalB();
#else
                missing();
#endif
            }
            case Instructions.POPCOPYRELB:
            {
#ifdef PACKED_INSTRUCTIONS
                popCopyRelB();
#else
                missing();
#endif
            }
            case Instructions.PUSHSTACKADDRB:
            {
#ifdef PACKED_INSTRUCTIONS
                pushStackAddrB();
#else
                missing();
#endif
            }
            
            default:
            {
                missing();
            }
        }
    }
}
