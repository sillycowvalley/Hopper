unit Runtime
{
    const byte runtimeSlots = 0x60; // 0x60..0x6F
    
    // same slot as programMemory : read-only!
    const byte functionTable    = runtimeSlots+0;
    const byte functionTableL   = runtimeSlots+0;
    const uint functionTableH   = runtimeSlots+1;
    
    const byte PC               = runtimeSlots+2;
    const byte BP               = runtimeSlots+3;
    
    const byte globals          = runtimeSlots+4;
    const byte globalsL         = runtimeSlots+4;
    const byte globalsH         = runtimeSlots+5;
    
    const byte constants        = runtimeSlots+6;
    const byte constantsL       = runtimeSlots+6;
    const byte constantsH       = runtimeSlots+7;
    
    const byte codePage         = runtimeSlots+8;
    const byte codePageL        = runtimeSlots+8;
    const byte codePageH        = runtimeSlots+9;
    
    const byte opCode           = runtimeSlots+10;
    const byte operand          = runtimeSlots+11;
    
    const byte vmFlags          = runtimeSlots+12;
    
    const byte stackStore       = runtimeSlots+13;
    
    
    
    // Bit 0 - exiting
    
    enum OpCode
    {
        // Stack Operations
        PUSHB    = 0x00,  // Push 8-bit immediate
        PUSHW    = 0x02,  // Push 16-bit immediate
        PUSH0    = 0x04,  // Push 0 (optimized)
        PUSH1    = 0x06,  // Push 1 (optimized)
        DUP      = 0x08,  // Duplicate byte on TOS
        DUPW     = 0x0A,  // Duplicate word on TOS
        DROP     = 0x0C,  // Remove byte from TOS
        DROPW    = 0x0E,  // Remove word from TOS
        SWAP     = 0x10,  // Swap top two bytes
        SWAPW    = 0x12,  // Swap top two words
        
        // Arithmetic Operations
        ADD      = 0x14,  // Pop 2 words, push sum
        SUB      = 0x16,  // Pop 2 words, push difference
        MUL      = 0x18,  // Pop 2 words, push product
        DIV      = 0x1A,  // Pop 2 words, push quotient (unsigned)
        MOD      = 0x1C,  // Pop 2 words, push remainder
        ADDB     = 0x1E,  // Pop 2 bytes, push sum
        SUBB     = 0x20,  // Pop 2 bytes, push difference
        NEG      = 0x22,  // Negate int (2's complement)
        NEGB     = 0x24,  // Negate char (2's complement)
        
        // Comparison Operations
        EQ       = 0x26,  // Pop 2 words/ints, push 1 if equal, 0 if not
        NE       = 0x28,  // Pop 2 words/ints, push 1 if not equal
        LT       = 0x2A,  // Pop 2 words, unsigned less than
        GT       = 0x2C,  // Pop 2 words, unsigned greater than
        LE       = 0x2E,  // Pop 2 words, unsigned less or equal
        GE       = 0x30,  // Pop 2 words, unsigned greater or equal
        LTC      = 0x32,  // Pop 2 chars, signed less than
        GTC      = 0x34,  // Pop 2 chars, signed greater than
        LTI      = 0x36,  // Pop 2 ints, signed less than
        GTI      = 0x38,  // Pop 2 ints, signed greater than
        
        // Bitwise Operations
        AND      = 0x3A,  // Pop 2 words, push bitwise AND
        OR       = 0x3C,  // Pop 2 words, push bitwise OR
        XOR      = 0x3E,  // Pop 2 words, push bitwise XOR
        NOT      = 0x40,  // Pop word, push bitwise NOT
        SHL      = 0x42,  // Pop word and count, shift left
        SHR      = 0x44,  // Pop word and count, logical shift right
        SAR      = 0x46,  // Pop int and count, arithmetic shift right
        
        // Memory Operations - Globals
        PUSHGB   = 0x48,  // Push byte from global[offset]
        PUSHGB2  = 0x4A,  // Push byte from global[offset] (>255)
        PUSHGW   = 0x4C,  // Push word from global[offset]
        PUSHGW2  = 0x4E,  // Push word from global[offset] (>255)
        POPGB    = 0x50,  // Pop byte to global[offset]
        POPGB2   = 0x52,  // Pop byte to global[offset] (>255)
        POPGW    = 0x54,  // Pop word to global[offset]
        POPGW2   = 0x56,  // Pop word to global[offset] (>255)
        
        // Memory Operations - Locals
        PUSHLB   = 0x58,  // Push byte from BP[offset]
        PUSHLW   = 0x5A,  // Push word from BP[offset]
        POPLB    = 0x5C,  // Pop byte to BP[offset]
        POPLW    = 0x5E,  // Pop word to BP[offset]
        
        // Data/ String Operations
        PUSHD    = 0x60,  // Push string address (byte offset)
        PUSHD2   = 0x62,  // Push string address (word offset)
        STRC     = 0x64,  // Pop index, pop string, push char
        STRCMP   = 0x66,  // Pop 2 strings, push -1/0/1
        
        // Control Flow
        CALL     = 0x68,  // Call function (ID × 2 for table lookup)
        RET      = 0x6A,  // Return from function
        BRAB     = 0x6C,  // Branch backward by byte (0-255)
        BRAF     = 0x6E,  // Branch forward by byte (0-255)
        BZF      = 0x70,  // Branch forward by byte if TOS is zero
        BZB      = 0x72,  // Branch backward by byte if TOS is zero
        BNZF     = 0x74,  // Branch forward by byte if TOS is not zero
        BNZB     = 0x76,  // Branch backward by byte if TOS is not zero
        
        // Zero Page Operations
        PUSHZB   = 0x78,  // Push byte from ZP[offset]
        PUSHZW   = 0x7A,  // Push word from ZP[offset]
        POPZB    = 0x7C,  // Pop byte to ZP[offset]
        POPZW    = 0x7E,  // Pop word to ZP[offset]
        
        // System Operations
        SYSCALL  = 0x80,  // Call BIOS function via X register
        HALT     = 0x82,  // Stop execution (return to BIOS)
        
        // Register Operations
        POPA     = 0x84,  // Pop byte from stack to A register
        POPY     = 0x86,  // Pop byte from stack to Y register
        PUSHA    = 0x88,  // Push A register to stack
        PUSHC    = 0x8A,  // Push carry flag (1 if set, 0 if clear)
        PUSHZ    = 0x8C,  // Push zero flag (1 if set, 0 if clear)
    }
    
    const string msgBadOpCode    = "Bad OpCode: 0x";
    
    const string msgBadOpCodeAt  = " at 0x";
    
    
    Initialize()
    {
        STZ globalsL
        LDA functionTableH
        INC
        STA globalsH
        
        STZ constantsL
        INC
        STA constantsH
        
        LDY #2 // slot of .MAIN in function table
        LDA [functionTable], Y
        STA codePageL
        INY
        LDA [functionTable], Y
        STA codePageH
        
        STZ PC
        STZ BP
        
        STZ vmFlags
    }
    
    halt()
    {
        SMB0 vmFlags // exiting
    }
    badOpCode()
    {
        PHX
        LDA #(msgBadOpCode % 256)
        STA ZP.STRL
        LDA #(msgBadOpCode / 256)
        STA ZP.STRH
        Print.String();
        PLX
        TXA
        PHX
        Print.Hex();
        
        LDA #(msgBadOpCodeAt % 256)
        STA ZP.STRL
        LDA #(msgBadOpCodeAt / 256)
        STA ZP.STRH
        Print.String();
        
        CLC
        LDA codePageL
        ADC PC
        STA ZP.ACCL
        LDA codePageH
        ADC # 0
        Print.Hex();
        LDA ZP.ACCL
        Print.Hex();
        
        PLX
        SMB0 vmFlags // exiting
    }
    
    dispatchBIOS()
    {
        JMP [ZP.BIOSDISPATCH]
    }
    
    Execute()
    {
        TSX
        STX stackStore
        
        loop
        {
#ifdef DEBUG            
            Print.NewLine(); LDA PC Print.Hex(); Print.Space();
#endif            
            LDY PC
            LDA [codePage], Y
#ifdef DEBUG            
            Print.Hex();            
#endif
            INC PC
            TAX
            
            switch (X)
            {
                // Stack Operations
                case OpCode.PUSHB:
                {
                    INC PC
                    INY
                    LDA [codePage], Y // operand LSB
                    PHA
                }
                case OpCode.PUSHW:    
                {
                    INC PC
                    INY
                    LDA [codePage], Y // operand LSB
                    PHA
                    INC PC
                    INY
                    LDA [codePage], Y // operand MSB
                    PHA
                }
                case OpCode.PUSH0:
                { 
                    LDA #0
                    PHA
                    PHA
                }
                case OpCode.PUSH1:    
                { 
                    LDA #1
                    PHA
                    LDA #0
                    PHA
                }
                case OpCode.DUP:      { badOpCode(); }
                case OpCode.DUPW:     
                {
                    TSX               // Get stack pointer to X
                
                    // Read the word from stack (without popping)
                    // TOP: SP+1=TOP1(MSB), SP+2=TOP0(LSB)
                    LDA 0x0101, X     // Load TOP1 (MSB at SP+1)
                    TAY               // Save in Y
                    LDA 0x0102, X     // Load TOP0 (LSB at SP+2)
                    
                    // Push the duplicate (LSB first, then MSB)
                    PHA               // Push TOP0 (LSB)
                    TYA               // Get TOP1 back
                    PHA               // Push TOP1 (MSB)
                }
                case OpCode.DROP:     { badOpCode(); }
                case OpCode.DROPW:    
                {
                    PLA
                    PLA
                }
                case OpCode.SWAP:     { badOpCode(); }
                case OpCode.SWAPW:    
                { 
                    TSX               // Get stack pointer to X
                
                    // Swap TOP and NEXT
                    // TOP: SP+1=TOP1(MSB), SP+2=TOP0(LSB)
                    // NEXT: SP+3=NEXT1(MSB), SP+4=NEXT0(LSB)
                    
                    // Swap the low bytes (TOP0 <-> NEXT0)
                    LDA 0x0102, X     // Load TOP0 (LSB at SP+2)
                    LDY 0x0104, X     // Load NEXT0 (LSB at SP+4)
                    STA 0x0104, X     // Store TOP0 to NEXT0 position
                    TYA
                    STA 0x0102, X     // Store NEXT0 to TOP0 position
                    
                    // Swap the high bytes (TOP1 <-> NEXT1)
                    LDA 0x0101, X     // Load TOP1 (MSB at SP+1)
                    LDY 0x0103, X     // Load NEXT1 (MSB at SP+3)
                    STA 0x0103, X     // Store TOP1 to NEXT1 position
                    TYA
                    STA 0x0101, X     // Store NEXT1 to TOP1 position
                }
                
                // Arithmetic Operations
                case OpCode.ADD:      
                {
                    TSX               // Get stack pointer to X
                    
                    // Add low bytes with carry propagation
                    // TOP:  SP+1=TOP1(MSB), SP+2=TOP0(LSB)
                    // NEXT: SP+3=NEXT1(MSB), SP+4=NEXT0(LSB)
                    CLC               // Clear carry for addition
                    LDA 0x0102, X     // Load TOP0 (LSB at SP+2)
                    ADC 0x0104, X     // Add NEXT0 (LSB at SP+4)
                    STA 0x0104, X     // Store result to NEXT0
                    
                    // Add high bytes with carry
                    LDA 0x0101, X     // Load TOP1 (MSB at SP+1)
                    ADC 0x0103, X     // Add NEXT1 (MSB at SP+3) with carry
                    STA 0x0103, X     // Store result to NEXT1
                    
                    INX INX TXS       // Remove TOP from stack
                }
                case OpCode.SUB:      { badOpCode(); }
                case OpCode.MUL:      { badOpCode(); }
                case OpCode.DIV:      { badOpCode(); }
                case OpCode.MOD:      { badOpCode(); }
                case OpCode.ADDB:     { badOpCode(); }
                case OpCode.SUBB:     { badOpCode(); }
                case OpCode.NEG:      { badOpCode(); }
                case OpCode.NEGB:     { badOpCode(); }
                
                // Comparison Operations
                case OpCode.EQ:       { badOpCode(); }
                case OpCode.NE:       { badOpCode(); }
                case OpCode.LT:       { badOpCode(); }
                case OpCode.GT:       { badOpCode(); }
                case OpCode.LE:       
                {
                    TSX               // Get stack pointer to X
                    
                    LDY #1 // NEXT <= TOP
                    
#ifdef DEBUG           
                    /*         
                    LDA 0x0103, X
                    STA ZP.NEXT1
                    LDA 0x0104, X
                    STA ZP.NEXT0
                    LDA 0x0101, X
                    STA ZP.TOP1
                    LDA 0x0102, X
                    STA ZP.TOP0
                    PHX
                    Print.Space(); LDA ZP.NEXT1 Print.Hex(); LDA ZP.NEXT0 Print.Hex(); LDA #'<' Print.Char();LDA #'=' Print.Char(); LDA ZP.TOP1 Print.Hex(); LDA ZP.TOP0 Print.Hex(); 
                    PLX
                    */
#endif                    
                    
                    
                    // Compare NEXT <= TOP
                    // TOP is at SP+1/SP+2 (top)
                    // NEXT is at SP+3/SP+4
                    // First compare high bytes
                    LDA 0x0103, X     // Load NEXT1
                    CMP 0x0101, X     // Compare with TOP1
                    if (Z)            // If NEXT1 == TOP1
                    {
                        LDA 0x0104, X // Load NEXT0 Low
                        CMP 0x0102, X // Compare with TOP0 Low
                    }
                    if (NZ) // NEXT != TOP
                    {
                        if (C)        // NEXT >= TOP?
                        {
                            LDY #0    // NEXT > TOP
                        }
                    }
                    INX               // Adjust stack by 3
                    INX
                    INX
                    TXS
                    TYA
                    STA 0x0101, X     // store the boolean result
#ifdef DEBUG
                    //Print.Space(); LDA #'-' Print.Char(); LDA #'-' Print.Char(); Print.Space(); TYA Print.Hex();
#endif
                }
                case OpCode.GE:       { badOpCode(); }
                case OpCode.LTC:      { badOpCode(); }
                case OpCode.GTC:      { badOpCode(); }
                case OpCode.LTI:      { badOpCode(); }
                case OpCode.GTI:      { badOpCode(); }
                
                // Bitwise Operations
                case OpCode.AND:      { badOpCode(); }
                case OpCode.OR:       { badOpCode(); }
                case OpCode.XOR:      { badOpCode(); }
                case OpCode.NOT:      { badOpCode(); }
                case OpCode.SHL:      { badOpCode(); }
                case OpCode.SHR:      { badOpCode(); }
                case OpCode.SAR:      { badOpCode(); }
                
                // Memory Operations - Globals
                case OpCode.PUSHGB:   { badOpCode(); }
                case OpCode.PUSHGB2:  { badOpCode(); }
                case OpCode.PUSHGW:   { badOpCode(); }
                case OpCode.PUSHGW2:  { badOpCode(); }
                case OpCode.POPGB:    { badOpCode(); }
                case OpCode.POPGB2:   { badOpCode(); }
                case OpCode.POPGW:    { badOpCode(); }
                case OpCode.POPGW2:   { badOpCode(); }
                
                // Memory Operations - Locals
                case OpCode.PUSHLB:   { badOpCode(); }
                case OpCode.PUSHLW:   { badOpCode(); }
                case OpCode.POPLB:    { badOpCode(); }
                case OpCode.POPLW:    { badOpCode(); }
                
                // Data/String Operations
                case OpCode.PUSHD:    
                {
                    INC PC
                    INY
                    LDA [codePage], Y // byte offset    
                    CLC
                    ADC constantsL
                    PHA               // LSB
                    LDA constantsH
                    ADC #0            // MSB
                    PHA  
                }
                case OpCode.PUSHD2:   { badOpCode(); }
                case OpCode.STRC:     { badOpCode(); }
                case OpCode.STRCMP:   { badOpCode(); }
                
                // Control Flow
                case OpCode.CALL:     { badOpCode(); }
                case OpCode.RET:      { badOpCode(); }
                case OpCode.BRAB:     { badOpCode(); }
                case OpCode.BRAF:     { badOpCode(); }
                case OpCode.BZF:      { badOpCode(); }
                case OpCode.BZB:      { badOpCode(); }
                case OpCode.BNZF:     { badOpCode(); }
                case OpCode.BNZB:     
                {
                    INC PC
                    INY
                    
                    PLX                   // pop the boolean
                    if (NZ)
                    {
                        SEC               // Branch backward by offset in A
                        LDA PC
                        SBC [codePage], Y // Subtract offset from PC
                        STA PC            // PC wraps naturally at page boundary
                    }
                }
                
                // Zero Page Operations
                case OpCode.PUSHZB:   { badOpCode(); }
                case OpCode.PUSHZW:   
                {
                    INC PC
                    INY
                    LDA [codePage], Y // byte offset  
                    TAX
                    LDA 0x00, X 
                    PHA               // LSB
                    INX
                    LDA 0x00, X 
                    PHA               // MSB
                }
                case OpCode.POPZB:    { badOpCode(); }
                case OpCode.POPZW:    
                {
                    INC PC
                    INY
                    LDA [codePage], Y // byte offset  

                    TAX
                    INX
                    PLA               // MSB
                    STA 0x00, X 
                    DEX
                    PLA               // LSB
                    STA 0x00, X 
                }
                
                // System Operations
                case OpCode.SYSCALL:
                {
                    INC PC
                    INY
                    LDA [codePage], Y // byte BIOS call index
                    TAX
#ifdef DEBUG                    
                    //PHX
#endif
                    dispatchBIOS();
#ifdef DEBUG
                    /*
                    PLX
                    CPX # 0x18
                    if (Z)
                    {
Print.NewLine(); LDA TOP3 Print.Hex(); LDA TOP2 Print.Hex(); LDA TOP1 Print.Hex(); LDA TOP0 Print.Hex();
                    }
                    */
#endif
                }
                case OpCode.HALT:     { halt(); }
                
                // Register Operations
                case OpCode.POPA:     { badOpCode(); }
                case OpCode.POPY:     { badOpCode(); }
                case OpCode.PUSHA:    { badOpCode(); }
                case OpCode.PUSHC:    { badOpCode(); }
                case OpCode.PUSHZ:    { badOpCode(); }
                
                default:              { badOpCode(); }
            }
            
            if (BBS0, vmFlags) { break; }  // exiting?
        }
        
        LDX stackStore
        TXS
        
        LDA #'>' Print.Char();
    }
}
