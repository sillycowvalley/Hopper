unit OpCodes
{
    enum OpCode
    {
        NOP      = 0x00,
        PUSHR0   = 0x02,       // R0 -> [top]
        POPR0    = 0x03,       // [top] -> R0
        
        LIBCALL  = 0x08,       // <library method index>, iOverload = [top]
        LIBCALL0 = 0x09,       // <library method index>, iOverload = 0
        LIBCALL1 = 0x0A,       // <library method index>, iOverload = 1
        
        PUSHI         = 0x37, // <uint operand>
        PUSHD         = 0x60, // <uint operand> : identical to PUSHI except that optimizer knows it points to a delegate method (reachable code)
        PUSHLOCAL     = 0x39, // <int offset operand>
        PUSHREL       = 0x3B, // [top] contains uint stack address
        PUSHGLOBAL    = 0x3D, // <uint stack address operand>
        PUSHSTACKADDR = 0x3E, // <int offset operand> pushed to [top] as uint stack address
        PUSHGP        = 0x47, // only used for current process 'floor' for globals passed as ref arguments
        
        POPLOCAL    = 0x38,   // <int offset operand>
        POPREL      = 0x3A,   // [top] contains uint stack address
        POPGLOBAL   = 0x3C,   // <uint stack address operand>
        COPYNEXTPOP = 0x48,   // next POP is a copy-on-assigment of a reference type
        
        BOOLNOT = 0x41,       // ![top] -> [top]
        BITNOT  = 0x42,       // ~[top] -> [top]
        
        SWAP    = 0x43,       // swap [top] and [next]
        DUP     = 0x27,       // <byte offset operand> to duplicate to [TOP] (DUP 0 means [top])
        DECSP   = 0x28,       // <byte operand> number of bytes to subtract from SP (release reference types too)
        
        DIE     = 0x29,       // 0x?? fail setting lastError to <byte operand>
        
        ENTER   = 0x49,       // new local stack frame: push BP to callstack, BP = SP
        
        NOP2    = 0x50,       // no operation
        
        CAST    = 0x51,       // change type of [top] to <byte operand>
        
        JZ      = 0x31,       // <int offset> if [top] == 0 then PC = PC + offset else PC += 3
        JNZ     = 0x32,       // <int offset> if [top] != 0 then PC = PC + offset else PC += 3
        JW      = 0x33,       // PC += <int offset>
        JREL    = 0x67,       // PC = [top]
        
        RET     = 0x35,       // <uint operand> of bytes to pop from stack, pop BP from callstack, pop PC from callstack
        RETRES  = 0x36,       // pop [top] to result, <uint operand> of bytes to pop from stack, push result to [top], pop BP from callstack, pop PC from callstack
        
        CALLI   = 0x6A,       // optionally used at runtime to replace CALL <method index> with CALLI <immediate address>
        CALL    = 0x34,       // CALL <method index>
        CALLREL = 0x4B,       // call delegate based on <index> in [top]
        SYSCALL = 0x26,       // J <system method index> [iOverload]
        
        
        // pop 2 -> operation -> push 1: (bit 0 set means 'signed')
        
        ADD   = 0x80,         // [next] + [top] -> [top]
        ADDI  = 0x81,         // [next] + [top] -> [top]
        SUB   = 0x82,         // [next] - [top] -> [top]
        SUBI  = 0x83,         // [next] - [top] -> [top]
        DIV   = 0x84,         // [next] / [top] -> [top]
        DIVI  = 0x85,         // [next] / [top] -> [top]
        MUL   = 0x86,         // [next] * [top] -> [top]
        MULI  = 0x87,         // [next] * [top] -> [top]
        MOD   = 0x88,         // [next] % [top] -> [top]
        MODI  = 0x89,         // [next] % [top] -> [top]
        
        GT    = 0x8A,         // [next] >  [top] ? 1 -> [top] : 0 -> [top]
        GTI   = 0x8B,         // [next] >  [top] ? 1 -> [top] : 0 -> [top]
        LT    = 0x8C,         // [next] <  [top] ? 1 -> [top] : 0 -> [top]
        LTI   = 0x8D,         // [next] <  [top] ? 1 -> [top] : 0 -> [top]
        GE    = 0x8E,         // [next] >= [top] ? 1 -> [top] : 0 -> [top]
        GEI   = 0x8F,         // [next] >= [top] ? 1 -> [top] : 0 -> [top]
        LE    = 0x90,         // [next] <= [top] ? 1 -> [top] : 0 -> [top]
        LEI   = 0x91,         // [next] <= [top] ? 1 -> [top] : 0 -> [top]

        // pop 2 -> operation -> push 1: (bit 0 set means 'signed' so these are always unsigned)

        EQ      = 0x92,       // [next] == [top] ? 1 -> [top] : 0 -> [top]
        NE      = 0x94,       // [next] != [top] ? 1 -> [top] : 0 -> [top]
        BOOLOR  = 0x96,       // [next] || [top] ? 1 -> [top] : 0 -> [top]
        BOOLAND = 0x98,       // [next] && [top] ? 1 -> [top] : 0 -> [top]
        BITAND  = 0x9A,       // [next] &  [top] -> [top]
        BITOR   = 0x9C,       // [next] |  [top] -> [top]
        BITXOR  = 0x9E,       // [next] ^  [top] -> [top]
        BITSHR  = 0xA0,       // [next] >> [top] -> [top]
        BITSHL  = 0xA2,       // [next] << [top] -> [top]
        
        
        
        // not part of the minimal Tiny Hopper opcode set:
        PUSHIB      = 0x1A,
        POPLOCALB   = 0x1B,
        PUSHLOCALB  = 0x1C,
        POPRELB     = 0x1D,
        PUSHRELB    = 0x1E,
        POPGLOBALB  = 0x1F,
        PUSHGLOBALB = 0x20,        
        PUSHSTACKADDRB = 0x21,
        
        RETB        = 0x2A,
        RETRESB     = 0x2B,
        CALLB       = 0x2C,
        TESTBPB     = 0x2D,
        
        JZB     = 0x2E,
        JNZB    = 0x2F,
        JB      = 0x30,
        JIXB    = 0x68,            
        JIX     = 0x69,
        
        PUSHILE = 0x40,
        
        PUSHI0  = 0x44,
        PUSHI1  = 0x45,
        PUSHIM1 = 0x46,
        
        RET0         = 0x4A,
        
        POPLOCALB00  = 0x4C,
        POPLOCALB01  = 0x4D,
        PUSHLOCALB00 = 0x4E,
        PUSHLOCALB01 = 0x4F,
        
        SYSCALL0 = 0x24,
        SYSCALL1 = 0x25,
        
        PUSHGLOBALBB = 0x52,
        
        INCLOCALB   = 0x22,
        INCLOCALIB  = 0xA4,
        DECLOCALB   = 0x23,
        DECLOCALIB  = 0xA6,
        INCGLOBALB = 0x53,
        INCGLOBALIB = 0xA5,
        DECGLOBALB = 0x54,
        DECGLOBALIB = 0xA7,
        INCLOCALBB = 0x3F,
        INCLOCALIBB = 0xA3,
        
        SYSCALLB0   = 0xA8, // PUSHIB SYSCALL0
        SYSCALL00   = 0xA9, // SYSCALL0 SYSCALL0
        PUSHIBB     = 0xAA, // PUSHIB PUSHIB
        SYSCALLB1   = 0xAB, // PUSHIB   SYSCALL1
        SYSCALL01   = 0xAC, // SYSCALL0 SYSCALL1
        SYSCALL10   = 0xAD, // SYSCALL1 SYSCALL0
        
        PUSHILT = 0x55,
        PUSHLOCALBB     = 0x56,
        POPCOPYLOCALB   = 0x57,
        POPCOPYRELB     = 0x58,
        POPCOPYGLOBALB  = 0x59,
        
        POPCOPYLOCALB00 = 0x5D,
        POPCOPYLOCALB01 = 0x5E,
        
        ENTERB   = 0x5F,
        EXIT     = 0x63, // only used in inline code (like GIBL)
      
        PUSHDB   = 0x62,
        
        PUSHILEI = 0x65,
        INCGLOBALBB = 0x66,

        
        RETFAST  = 0x61,
        
        PUSHIBLE  = 0x6B,
        PUSHIBEQ  = 0x6C,
        
        ADDB      = 0x6D,
        SUBB      = 0x6E,  
        
    }
    
    
}

