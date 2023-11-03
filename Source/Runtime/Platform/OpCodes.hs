unit OpCodes
{
    enum OpCode
    {
        ADD = 0x00,
        SUB = 0x01,
        DIV = 0x02,
        MUL = 0x03,
        MOD = 0x04,
        
        EQ = 0x05,
        NE = 0x06,
        GT = 0x07,
        LT = 0x08,
        GE = 0x09,
        LE = 0x0A,
        
        BOOLOR  = 0x0B,
        BOOLAND = 0x0C,
        BITOR   = 0x0D,
        BITAND  = 0x0E,
        BITSHL  = 0x0F,
        BITSHR  = 0x10,
        
        ADDI = 0x11,
        SUBI = 0x12,
        DIVI = 0x13,
        MULI = 0x14,
        MODI = 0x15,
        GTI = 0x16,
        LTI = 0x17,
        GEI = 0x18,
        LEI = 0x19,
        
        PUSHIB      = 0x1A,
        
        POPLOCALB   = 0x1B,
        PUSHLOCALB  = 0x1C,
        POPRELB     = 0x1D,
        
        PUSHRELB    = 0x1E,
        POPGLOBALB  = 0x1F,
        PUSHGLOBALB = 0x20,        
        PUSHSTACKADDRB = 0x21,
        
        INCLOCALB   = 0x22,
        DECLOCALB   = 0x23,
        
        DUP         = 0x27,
        DECSP       = 0x28,
        
        RETB        = 0x2A,
        RETRETB     = 0x2B,
        CALLB       = 0x2C,
        
        JZB  = 0x2E,
        JNZB = 0x2F,
        JB   = 0x30,
        JZW  = 0x31,
        JNZW = 0x32,
        JW   = 0x33,
        
        CALLW  = 0x034,
        
        PUSHIW = 0x37,
        
        INCLOCALBB = 0x3F,
        PUSHIWLE = 0x40,
        BOOLNOT = 0x41,
        BITNOT  = 0x42,
        
        SWAP    = 0x43,
        PUSHI0  = 0x44,
        PUSHI1  = 0x45,
        PUSHIM1 = 0x46,
        PUSHGP  = 0x47,
        
        RET0         = 0x4A,
        CALLREL      = 0x4B,
        
        POPLOCALB00  = 0x4C,
        POPLOCALB02  = 0x4D,
        PUSHLOCALB00 = 0x4E,
        PUSHLOCALB02 = 0x4F,
        
        SYSCALL0 = 0x24,
        SYSCALL1 = 0x25,
        SYSCALL  = 0x26,
        
        COPYNEXTPOP = 0x48,
        ENTER       = 0x49,
        
        CAST     = 0x51,
        
        INCGLOBALB = 0x53,
        DECGLOBALB = 0x54,
        
        PUSHIWLT = 0x55,
        PUSHLOCALBB     = 0x56,
        POPCOPYLOCALB   = 0x57,
        POPCOPYRELB     = 0x58,
        POPCOPYGLOBALB  = 0x59,
        
        POPCOPYLOCALB00 = 0x5D,
        POPCOPYLOCALB02 = 0x5E,
        
        ENTERB   = 0x5F,
        PUSHDW   = 0x60,
        RETFAST  = 0x61,
        PUSHDB   = 0x62,
        
        BITXOR    = 0x64,
        
        PUSHIWLEI = 0x65,
        
        JIXB      = 0x68,
        JIXW      = 0x69,
        
    }
    
}

