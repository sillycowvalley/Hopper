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
    const byte vmFlags          = runtimeSlots+11;
    
    const byte stackStore       = runtimeSlots+12;
    
    
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
            LDY PC
            LDA [codePage], Y
            INC PC
            TAX
            
            switch (X)
            {
                // Stack Operations
                case OpCode.PUSHB:    { badOpCode(); }
                case OpCode.PUSHW:    { badOpCode(); }
                case OpCode.PUSH0:    { badOpCode(); }
                case OpCode.PUSH1:    { badOpCode(); }
                case OpCode.DUP:      { badOpCode(); }
                case OpCode.DUPW:     { badOpCode(); }
                case OpCode.DROP:     { badOpCode(); }
                case OpCode.DROPW:    { badOpCode(); }
                case OpCode.SWAP:     { badOpCode(); }
                case OpCode.SWAPW:    { badOpCode(); }
                
                // Arithmetic Operations
                case OpCode.ADD:      { badOpCode(); }
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
                case OpCode.LE:       { badOpCode(); }
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
                case OpCode.BNZB:     { badOpCode(); }
                
                // Zero Page Operations
                case OpCode.PUSHZB:   { badOpCode(); }
                case OpCode.PUSHZW:   { badOpCode(); }
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
                    dispatchBIOS();
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
        
    }
}
