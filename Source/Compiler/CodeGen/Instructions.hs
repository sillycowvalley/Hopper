unit Instructions
{
    uses "../Tokens/SysCalls"
    uses "../Tokens/LibCalls"
    
    enum Instruction
    {
        NOP      = 0x00,
        DUP0     = 0x01,       // push [top]
        
        PUSHR0   = 0x02,       // R0 -> [top]
        POPR0    = 0x03,       // [top] -> R0 
        
        BITSHL8  = 0x04,
        BITSHR8  = 0x05,
        BITANDFF = 0x06,
               
        LIBCALL  = 0x08,
        LIBCALL0 = 0x09,
        LIBCALL1 = 0x0A,
        
        SYSCALL2 = 0x0B, // syscall <byte operand>, overload 2
        
        BITSHLB  = 0x0C,
        BITSHRB  = 0x0D,
        BITANDB  = 0x0E,
        BITORB   = 0x0F,
    
        PUSHIB = 0x1A,  // operand is byte
        POPLOCALB,      // operand is the location to pop to: BP + offset
        PUSHLOCALB,     // operand is the location to push from: BP + offset
        POPRELB,        // like POPLOCAL but the absolute address to pop is taken from BP + offset
        PUSHRELB,       // like PUSHLOCAL but the absolute address to push is taken from BP + offset
        POPGLOBALB,     // operand is the absolute address to pop to
        PUSHGLOBALB,    // operand is the absolute address to push from
        PUSHSTACKADDRB, // operand is the offset from BP of the variable - convert to absolute stack address and push that
        
        INCLOCALB,
        DECLOCALB,
        
        SYSCALL0,     // syscall <byte operand>, overload 0
        SYSCALL1,     // syscall <byte operand>, overload 1
        SYSCALL,      // syscall <byte operand>,  overload [next]
        
        DUP,          // operand is offset 0..255 into stack where 0=[top], 1=[next], etc
        DECSP,

        DIE,       // 0x?? fail setting lastError to <byte operand>
  
        RETB,      // RET and pop <byte operand> bytes
        RETRESB,   // RET and pop <byte operand> bytes, but preserve [top] as the return value
      
        CALLB,     // <byte index operand>
        TESTBPB,   // verify that BP is what we expect it to be
                           
        // jump offsets: -1 means address of J instruction - 1, 0 means address after J instruction
        JZB,       // <signed byte offset>
        JNZB,      // <signed byte offset>
        JB,        // <signed byte offset>
        
        // jump offsets: -1 means address of J instruction - 1, 0 means address after J instruction
        JZ,       // <signed int offset>
        JNZ,      // <signed int offset>
        J,        // <signed int offset>
        
        CALL,     // <integer index operand>
  
        RET,      // RET and pop <uint operand> bytes
        RETRES,   // RET and pop <uint operand> bytes, but preserve [top] as the return value
        
        PUSHI,       // operand is uint
        POPLOCAL,    // operand is the location to pop to: BP + offset
        PUSHLOCAL,   // operand is the location to push from: BP + offset
        POPREL,      // like POPLOCAL but the absolute address to pop is taken from BP + offset
        PUSHREL,     // like PUSHLOCAL but the absolute address to push is taken from BP + offset
        POPGLOBAL,   // operand is the absolute address to pop to
        PUSHGLOBAL,  // operand is the absolute address to push from
        PUSHSTACKADDR, // operand is the offset from BP of the variable - convert to absolute stack address and push that
        
        INCLOCALBB,
        PUSHILE,

        BOOLNOT,      // ![top] -> [top]
        BITNOT,       // ~[top] -> [top]
        
        SWAP,         // swap [top] and [next] (consider object manager stack slots?)
        
        PUSHI0,
        PUSHI1,
        PUSHIM1,
        PUSHGP,        // GP, the global "floor" for child processes
        COPYNEXTPOP,   // what follows is a pop of a reference into a variable - should it be made into a copy?
  
        ENTER,
        RET0,
        
        CALLREL,   // call delegate based on <index> in [top]
        
        POPLOCALB00,
        POPLOCALB01,
        PUSHLOCALB00,
        PUSHLOCALB01,
        
        unusedNOP2,
        
        CAST, // operand is value type (byte) - change top of stack to this type

        PUSHGLOBALBB,  // operand is the absolute address to push from, x2
        INCGLOBALB,
        DECGLOBALB,
        
        PUSHILT,
        
        PUSHLOCALBB,
        
        POPCOPYLOCALB,
        POPCOPYRELB,
        POPCOPYGLOBALB,
        POPCOPYLOCAL,   // unused?
        POPCOPYREL,     // unused?
        POPCOPYGLOBAL,
        
        POPCOPYLOCALB00,
        POPCOPYLOCALB01,
        
        ENTERB,
        
        PUSHD, // operand uint, gets resolved at runtime from delegate index to delegate pointer
        RETFAST,
        PUSHDB,
        EXIT, // NOP, only used by inline code
        
        PUSHILEI = 0x65,
        INCGLOBALBB,
        
        JREL,
        JIXB,
        JIX,
        
        CALLI,
        
        PUSHIBLE,
        PUSHIBEQ,
        
        ADDB,
        SUBB,
        INC,
        DEC,
        
        // pop 2 -> operation -> push 1: (bit 0 set means 'signed')
        ADD  = 0x80,
        ADDI = 0x81,
        SUB  = 0x82,
        SUBI = 0x83,
        DIV  = 0x84,
        DIVI = 0x85,
        MUL  = 0x86,
        MULI = 0x87,
        MOD  = 0x88,
        MODI = 0x89,
        
        GT    = 0x8A,
        GTI   = 0x8B,
        LT    = 0x8C,
        LTI   = 0x8D,
        GE    = 0x8E,
        GEI   = 0x8F,
        LE    = 0x90,
        LEI   = 0x91,

        // pop 2 -> operation -> push 1: (bit 0 set means 'signed' so these are always unsigned)

        EQ      = 0x92,
        NE      = 0x94,
        BOOLOR  = 0x96,
        BOOLAND = 0x98,
        BITAND  = 0x9A,
        BITOR   = 0x9C,
        BITXOR  = 0x9E,
        BITSHR  = 0xA0,
        BITSHL  = 0xA2,
        
        INCLOCALIBB = 0xA3,
        INCLOCALIB  = 0xA4,
        INCGLOBALIB = 0xA5,
        DECLOCALIB  = 0xA6,
        DECGLOBALIB = 0xA7,
        
        SYSCALLB0   = 0xA8, // PUSHIB SYSCALL0
        SYSCALL00   = 0xA9, // SYSCALL0 SYSCALL0
        PUSHIBB     = 0xAA, // PUSHIB PUSHIB
        SYSCALLB1   = 0xAB, // PUSHIB   SYSCALL1
        SYSCALL01   = 0xAC, // SYSCALL0 SYSCALL1
        SYSCALL10   = 0xAD, // SYSCALL1 SYSCALL0

        MERGEDRET0  = 0xFF, // placeholder for 'optimize'
        UNDEFINED,
    }
    
    bool IsRET(Instruction instruction)
    {
        switch(instruction)
        {
            case Instruction.RET0:
            case Instruction.MERGEDRET0:
            case Instruction.RETFAST:
            case Instruction.RETB:
            case Instruction.RETRESB:
            case Instruction.RET:
            case Instruction.RETRES:
            {
                return true;   
            }
        }
        return false;
    }
    bool IsPushConstant(Instruction instruction)
    {
        switch(instruction)
        {
            case Instruction.PUSHI0:
            case Instruction.PUSHI1:
            case Instruction.PUSHIM1:
            case Instruction.PUSHIB:
            case Instruction.PUSHI:
            {
                return true;   
            }
        }
        return false;
    }
    
    byte GetKitchenSinkWidth(Instruction instruction, ref bool isStackOffset, ref bool isAddressOffset, ref bool isRET)
    {
        byte width;
        isStackOffset = false;
        isAddressOffset = false;
        isRET = false;
        switch (instruction)
        {
            case Instruction.INCLOCALB:
            case Instruction.INCLOCALIB:
            case Instruction.DECLOCALB:
            case Instruction.DECLOCALIB:
            case Instruction.POPLOCALB:
            case Instruction.POPRELB:
            case Instruction.POPCOPYLOCALB:
            case Instruction.POPCOPYRELB:
            case Instruction.PUSHLOCALB:
            case Instruction.PUSHRELB:
            case Instruction.PUSHSTACKADDRB:
            {
                width = 1;
                isStackOffset = true;
            }
            case Instruction.POPREL:
            case Instruction.POPLOCAL:
            case Instruction.POPCOPYLOCAL:
            case Instruction.POPCOPYREL:
            case Instruction.PUSHLOCAL:
            case Instruction.PUSHSTACKADDR:
            case Instruction.PUSHREL:
            {
                width = 2;
                isStackOffset = true;
            }
            
            case Instruction.RET0:
            case Instruction.MERGEDRET0:
            case Instruction.RETFAST:
            {
                isRET = true;
            }
            case Instruction.RETB:
            case Instruction.RETRESB:
            {
                isRET = true;
                width = 1;
            }
            case Instruction.RET:
            case Instruction.RETRES:
            {
                width = 2;
                isRET = true;
            }
            
            case Instruction.JB:
            case Instruction.JZB:
            case Instruction.JNZB:
            {
                isAddressOffset = true;
                width = 1;
            }
            case Instruction.J:
            case Instruction.JZ:
            case Instruction.JNZ:
            {
                isAddressOffset = true;
                width = 2;
            }
            
            
            case Instruction.CAST:
            case Instruction.CALLB:
            case Instruction.TESTBPB:
            case Instruction.PUSHIB:
            case Instruction.ADDB:
            case Instruction.SUBB:
            
            case Instruction.BITSHLB:
            case Instruction.BITSHRB:
            case Instruction.BITANDB:
            case Instruction.BITORB:
            
            case Instruction.PUSHDB:
            case Instruction.POPGLOBALB:
            case Instruction.POPCOPYGLOBALB:
            case Instruction.PUSHGLOBALB:
            case Instruction.SYSCALL:
            case Instruction.SYSCALL0:
            case Instruction.SYSCALL1:
            case Instruction.SYSCALL2:
            case Instruction.LIBCALL:
            case Instruction.LIBCALL0:
            case Instruction.LIBCALL1:
            case Instruction.INCGLOBALB:
            case Instruction.INCGLOBALIB:
            case Instruction.DECGLOBALB:
            case Instruction.DECGLOBALIB:
            case Instruction.DUP:
            case Instruction.DECSP:
            case Instruction.DIE:
            case Instruction.ENTERB:
            case Instruction.PUSHIBLE:
            case Instruction.PUSHIBEQ:
            {
                width = 1;
            }
            
            case Instruction.CALL:
            case Instruction.PUSHI:
            case Instruction.PUSHD:
            case Instruction.PUSHILE:
            case Instruction.PUSHILEI:
            case Instruction.PUSHILT:
            case Instruction.POPGLOBAL:
            case Instruction.POPCOPYGLOBAL:
            case Instruction.PUSHGLOBALBB:
            case Instruction.PUSHLOCALBB:
            case Instruction.PUSHGLOBAL:
            case Instruction.INCLOCALBB:
            case Instruction.INCLOCALIBB:
            case Instruction.INCGLOBALBB:
            case Instruction.JIXB:
            case Instruction.JIX:
            case Instruction.CALLI:
            
            case Instruction.SYSCALLB0:
            case Instruction.SYSCALLB1:
            case Instruction.SYSCALL00:
            case Instruction.SYSCALL01:
            case Instruction.SYSCALL10:
            case Instruction.PUSHIBB:
            {
                width = 2;
            }
        }
        return width;
    }
    
    bool OperandIsStackOffset(Instruction instruction) // used by DASM
    {
        bool isOffset;
        switch (instruction)
        {
            case Instruction.INCLOCALB:
            case Instruction.INCLOCALIB:
            case Instruction.DECLOCALB:
            case Instruction.DECLOCALIB:
            case Instruction.POPLOCALB:
            case Instruction.POPLOCAL:
            case Instruction.POPRELB:
            case Instruction.POPREL:
            case Instruction.POPCOPYLOCALB:
            case Instruction.POPCOPYLOCAL:
            case Instruction.POPCOPYRELB:
            case Instruction.POPCOPYREL:
            case Instruction.PUSHLOCALB:
            case Instruction.PUSHLOCAL:
            case Instruction.PUSHSTACKADDR:
            case Instruction.PUSHRELB:
            case Instruction.PUSHREL:
            case Instruction.PUSHSTACKADDRB:
            {
                isOffset = true;
            }
        }    
        return isOffset;
    }
    bool OperandIsAddressOffset(Instruction instruction)
    {
        bool isOffset;
        switch (instruction)
        {
            case Instruction.JB:
            case Instruction.JZB:
            case Instruction.JNZB:
            case Instruction.J:
            case Instruction.JZ:
            case Instruction.JNZ:
            {
                isOffset = true;
            }
        }    
        return isOffset;
    }
    byte GetSimpleOperandWidth(Instruction instruction)
    {
        byte result;
        switch (instruction)
        {
            case Instruction.CAST:
            case Instruction.CALLB:
            case Instruction.TESTBPB:
            case Instruction.RETB:
            case Instruction.RETRESB:
            case Instruction.PUSHIB:
            case Instruction.ADDB:
            case Instruction.SUBB:
            
            case Instruction.BITSHLB:
            case Instruction.BITSHRB:
            case Instruction.BITANDB:
            case Instruction.BITORB:
            
            case Instruction.PUSHDB:
            case Instruction.POPGLOBALB:
            case Instruction.POPLOCALB:
            case Instruction.POPRELB:
            case Instruction.POPCOPYGLOBALB:
            case Instruction.POPCOPYLOCALB:
            case Instruction.POPCOPYRELB:
            case Instruction.PUSHGLOBALB:
            case Instruction.PUSHLOCALB:
            case Instruction.PUSHRELB:
            case Instruction.PUSHSTACKADDRB:
            case Instruction.SYSCALL:
            case Instruction.SYSCALL0:
            case Instruction.SYSCALL1:
            case Instruction.SYSCALL2:
            case Instruction.LIBCALL:
            case Instruction.LIBCALL0:
            case Instruction.LIBCALL1:
            case Instruction.JB:
            case Instruction.JZB:
            case Instruction.JNZB:
            case Instruction.INCGLOBALB:
            case Instruction.INCGLOBALIB:
            case Instruction.DECGLOBALB:
            case Instruction.DECGLOBALIB:
            case Instruction.INCLOCALB:
            case Instruction.INCLOCALIB:
            case Instruction.DECLOCALB:
            case Instruction.DECLOCALIB:
            case Instruction.DUP:
            case Instruction.DECSP:
            case Instruction.DIE:
            case Instruction.ENTERB:
            case Instruction.PUSHIBLE:
            case Instruction.PUSHIBEQ:
            {
                result = 1;
            }
            
            case Instruction.CALL:
            case Instruction.RET:
            case Instruction.RETRES:
            case Instruction.PUSHI:
            case Instruction.PUSHD:
            case Instruction.PUSHILE:
            case Instruction.PUSHILEI:
            case Instruction.PUSHILT:
            case Instruction.POPGLOBAL:
            case Instruction.POPLOCAL:
            case Instruction.POPREL:
            case Instruction.POPCOPYGLOBAL:
            case Instruction.POPCOPYLOCAL:
            case Instruction.POPCOPYREL:
            case Instruction.PUSHGLOBALBB:
            case Instruction.PUSHLOCALBB:
            case Instruction.PUSHGLOBAL:
            case Instruction.PUSHLOCAL:
            case Instruction.PUSHREL:
            case Instruction.PUSHSTACKADDR:
            case Instruction.J:
            case Instruction.JZ:
            case Instruction.JNZ:
            case Instruction.INCLOCALBB:
            case Instruction.INCLOCALIBB:
            case Instruction.INCGLOBALBB:
            case Instruction.JIXB:
            case Instruction.JIX:
            case Instruction.CALLI:
            
            case Instruction.SYSCALLB0:
            case Instruction.SYSCALLB1:
            case Instruction.SYSCALL00:
            case Instruction.SYSCALL01:
            case Instruction.SYSCALL10:
            case Instruction.PUSHIBB:
            {
                result = 2;
            }
        }
        return result;
    }
    
    
    string ToString(Instruction instruction)
    {
        string result;
        switch (instruction)
        {
            case Instruction.SYSCALLB0:
            {
                result = "SYSCALLB0";
            }
            case Instruction.SYSCALLB1:
            {
                result = "SYSCALLB1";
            }
            case Instruction.SYSCALL00:
            {
                result = "SYSCALL00";
            }
            case Instruction.SYSCALL01:
            {
                result = "SYSCALL01";
            }
            case Instruction.SYSCALL10:
            {
                result = "SYSCALL10";
            }
            case Instruction.PUSHIBB:
            {
                result = "PUSHIBB";
            }
            case Instruction.CAST:
            {
                result = "CAST";
            }
            case Instruction.ENTER:
            {
                result = "ENTER";
            }
            case Instruction.ENTERB:
            {
                result = "ENTERB";
            }
            case Instruction.NOP:
            {
                result = "NOP";
            }
            case Instruction.PUSHR0:
            {
                result = "PUSHR0";
            }
            case Instruction.POPR0:
            {
                result = "POPR0";
            }
            case Instruction.EXIT:
            {
                result = "EXIT";
            }
            case Instruction.CALLB:
            {
                result = "CALLB";
            }
            case Instruction.TESTBPB:
            {
                result = "TESTBPB";
            }
            case Instruction.CALL:
            {
                result = "CALL";
            }
            case Instruction.CALLI:
            {
                result = "CALLI";
            }
            case Instruction.CALLREL:
            {
                result = "CALLREL";
            }
            case Instruction.JREL:
            {
                result = "JREL";
            }
            case Instruction.RET0:
            {
                result = "RET0";
            }
            case Instruction.MERGEDRET0:
            {
                result = "MERGEDRET0";
            }
            case Instruction.RETFAST:
            {
                result = "RETFAST";
            }
            case Instruction.RETB:
            {
                result = "RETB";
            }
            case Instruction.RET:
            {
                result = "RET";
            }
            case Instruction.RETRESB:
            {
                result = "RETRESB";
            }
            case Instruction.RETRES:
            {
                result = "RETRES";
            }
            case Instruction.SYSCALL:
            {
                result = "SYSCALL";
            }
            case Instruction.SYSCALL0:
            {
                result = "SYSCALL0";
            }
            case Instruction.SYSCALL1:
            {
                result = "SYSCALL1";
            }
            case Instruction.SYSCALL2:
            {
                result = "SYSCALL2";
            }
            case Instruction.LIBCALL:
            {
                result = "LIBCALL";
            }
            case Instruction.LIBCALL0:
            {
                result = "LIBCALL0";
            }
            case Instruction.LIBCALL1:
            {
                result = "LIBCALL1";
            }
            case Instruction.PUSHI0:
            {
                result = "PUSHI0";
            }
            case Instruction.PUSHI1:
            {
                result = "PUSHI1";
            }
            case Instruction.PUSHIM1:
            {
                result = "PUSHIM1";
            }
            case Instruction.PUSHGP:
            {
                result = "PUSHGP";
            }
            case Instruction.PUSHIB:
            {
                result = "PUSHIB";
            }
            case Instruction.ADDB:
            {
                result = "ADDB";
            }
            case Instruction.SUBB:
            {
                result = "SUBB";
            }
            case Instruction.INC:
            {
                result = "INC";
            }
            case Instruction.DEC:
            {
                result = "DEC";
            }
            case Instruction.BITSHLB:
            {
                result = "BITSHLB";
            }
            case Instruction.BITSHRB:
            {
                result = "BITSHRB";
            }
            case Instruction.BITANDB:
            {
                result = "BITANDB";
            }
            case Instruction.BITORB:
            {
                result = "BITORB";
            }
            case Instruction.PUSHI:
            {
                result = "PUSHI";
            }
            case Instruction.PUSHD:
            {
                result = "PUSHD";
            }
            case Instruction.PUSHDB:
            {
                result = "PUSHDB";
            }
            case Instruction.PUSHIBLE:
            {
                result = "PUSHIBLE";
            }
            case Instruction.PUSHIBEQ:
            {
                result = "PUSHIBEQ";
            }
            case Instruction.PUSHILE:
            {
                result = "PUSHILE";
            }
            case Instruction.PUSHILEI:
            {
                result = "PUSHILEI";
            }
            case Instruction.PUSHILT:
            {
                result = "PUSHILT";
            }
            
            case Instruction.INCGLOBALB:
            {
                result = "INCGLOBALB";
            }
            case Instruction.INCGLOBALIB:
            {
                result = "INCGLOBALIB";
            }
            case Instruction.DECGLOBALB:
            {
                result = "DECGLOBALB";
            }
            case Instruction.DECGLOBALIB:
            {
                result = "DECGLOBALIB";
            }
            case Instruction.INCLOCALB:
            {
                result = "INCLOCALB";
            }
            case Instruction.INCLOCALIB:
            {
                result = "INCLOCALIB";
            }
            case Instruction.INCLOCALBB:
            {
                result = "INCLOCALBB";
            }
            case Instruction.INCLOCALIBB:
            {
                result = "INCLOCALIBB";
            }
            case Instruction.INCGLOBALBB:
            {
                result = "INCGLOBALBB";
            }
            case Instruction.DECLOCALB:
            {
                result = "DECLOCALB";
            }
            case Instruction.DECLOCALIB:
            {
                result = "DECLOCALIB";
            }
            
            case Instruction.POPGLOBALB:
            {
                result = "POPGLOBALB";
            }
            case Instruction.POPGLOBAL:
            {
                result = "POPGLOBAL";
            }
            case Instruction.POPLOCALB:
            {
                result = "POPLOCALB";
            }
            case Instruction.POPLOCAL:
            {
                result = "POPLOCAL";
            }
            case Instruction.POPRELB:
            {
                result = "POPRELB";
            }
            case Instruction.POPREL:
            {
                result = "POPREL";
            }
            
            case Instruction.POPCOPYGLOBALB:
            {
                result = "POPCOPYGLOBALB";
            }
            case Instruction.POPCOPYGLOBAL:
            {
                result = "POPCOPYGLOBAL";
            }
            case Instruction.POPCOPYLOCALB:
            {
                result = "POPCOPYLOCALB";
            }
            case Instruction.POPCOPYLOCAL:
            {
                result = "POPCOPYLOCAL";
            }
            case Instruction.POPCOPYRELB:
            {
                result = "POPCOPYRELB";
            }
            case Instruction.POPCOPYREL:
            {
                result = "POPCOPYREL";
            }
            
            
            case Instruction.POPLOCALB01:
            {
                result = "POPLOCALB01";
            }
            case Instruction.POPLOCALB00:
            {
                result = "POPLOCALB00";
            }
            case Instruction.POPCOPYLOCALB01:
            {
                result = "POPCOPYLOCALB01";
            }
            case Instruction.POPCOPYLOCALB00:
            {
                result = "POPCOPYLOCALB00";
            }
            
            
            case Instruction.PUSHGLOBALB:
            {
                result = "PUSHGLOBALB";
            }
            case Instruction.PUSHGLOBALBB:
            {
                result = "PUSHGLOBALBB";
            }
            case Instruction.PUSHLOCALBB:
            {
                result = "PUSHLOCALBB";
            }
            case Instruction.PUSHGLOBAL:
            {
                result = "PUSHGLOBAL";
            }
            case Instruction.PUSHSTACKADDRB:
            {
                result = "PUSHSTACKADDRB";
            }
            case Instruction.PUSHSTACKADDR:
            {
                result = "PUSHSTACKADDR";
            }      
                          
            
            case Instruction.PUSHLOCALB:
            {
                result = "PUSHLOCALB";
            }
            case Instruction.PUSHLOCALB00:
            {
                result = "PUSHLOCALB00";
            }
            case Instruction.PUSHLOCALB01:
            {
                result = "PUSHLOCALB01";
            }
            case Instruction.PUSHLOCAL:
            {
                result = "PUSHLOCAL";
            }
            
            case Instruction.PUSHRELB:
            {
                result = "PUSHRELB";
            }
            case Instruction.PUSHREL:
            {
                result = "PUSHREL";
            }
            
            case Instruction.BOOLNOT:
            {
                result = "BOOLNOT";
            }
            case Instruction.BOOLAND:
            {
                result = "BOOLAND";
            }
            case Instruction.BOOLOR:
            {
                result = "BOOLOR";
            }
            case Instruction.BITAND:
            {
                result = "BITAND";
            }
            case Instruction.BITOR:
            {
                result = "BITOR";
            }
            case Instruction.BITXOR:
            {
                result = "BITXOR";
            }
            case Instruction.BITNOT:
            {
                result = "BITNOT";
            }
            case Instruction.BITSHL8:
            {
                result = "BITSHL8";
            }
            case Instruction.BITSHR8:
            {
                result = "BITSHR8";
            }
            case Instruction.BITANDFF:
            {
                result = "BITANDFF";
            }
            
            case Instruction.JB:
            {
                result = "JB";
            }
            case Instruction.JZB:
            {
                result = "JZB";
            }
            case Instruction.JNZB:
            {
                result = "JNZB";
            }
            case Instruction.J:
            {
                result = "J";
            }
            case Instruction.JZ:
            {
                result = "JZ";
            }
            case Instruction.JNZ:
            {
                result = "JNZ";
            }
            case Instruction.JIXB:
            {
                result = "JIXB";
            }
            case Instruction.JIX:
            {
                result = "JIX";
            }
            case Instruction.COPYNEXTPOP:
            {
                result = "COPYNEXTPOP";
            }
            
            case Instruction.EQ:
            {
                result = "EQ";
            }
            case Instruction.NE:
            {
                result = "NE";
            }
            case Instruction.GT:
            {
                result = "GT";
            }
            case Instruction.LT:
            {
                result = "LT";
            }
            case Instruction.GE:
            {
                result = "GE";
            }
            case Instruction.LE:
            {
                result = "LE";
            }
            case Instruction.GTI:
            {
                result = "GTI";
            }
            case Instruction.LTI:
            {
                result = "LTI";
            }
            case Instruction.GEI:
            {
                result = "GEI";
            }
            case Instruction.LEI:
            {
                result = "LEI";
            }
            
            case Instruction.ADD:
            {
                result = "ADD";
            }
            case Instruction.SUB:
            {
                result = "SUB";
            }
            case Instruction.DIV:
            {
                result = "DIV";
            }
            case Instruction.MUL:
            {
                result = "MUL";
            }
            case Instruction.MOD:
            {
                result = "MOD";
            }
            case Instruction.ADDI:
            {
                result = "ADDI";
            }
            case Instruction.SUBI:
            {
                result = "SUBI";
            }
            case Instruction.DIVI:
            {
                result = "DIVI";
            }
            case Instruction.MULI:
            {
                result = "MULI";
            }
            case Instruction.MODI:
            {
                result = "MODI";
            }
            
            case Instruction.DUP:
            {
                result = "DUP";
            }
            case Instruction.DUP0:
            {
                result = "DUP0";
            }
            case Instruction.SWAP:
            {
                result = "SWAP";
            }
            case Instruction.DECSP:
            {
                result = "DECSP";
            }
            case Instruction.DIE:
            {
                result = "DIE";
            }
            
            case Instruction.BITSHR:
            {
                result = "BITSHR";
            }
            case Instruction.BITSHL:
            {
                result = "BITSHL";
            }
            default:
            {
                byte op = byte(instruction);
                result = "Undefined: 0x" + op.ToHexString(2);
            }
        }
        return result;
    }
    uint AddJIXTableSize(Instruction instruction, uint operand)
    {
        uint tableSize;
        switch (instruction)
        {
            case Instruction.JIX:
            {
                uint entries = (operand >> 8) - (operand & 0xFF) + 1;
                tableSize = entries * 2 + 2; // +2 for the start offset
            }
            case Instruction.JIXB:
            {
                uint entries = (operand >> 8) - (operand & 0xFF) + 1;
                tableSize = entries + 2; // +2 for the start offset
            }
        }
        return tableSize;
    }
    Instruction GetOperandAndNextAddress(<byte> code, ref uint address, ref uint operand)
    {
        byte cd = code[address];
        Instruction instruction = Instruction(cd);
        address++;
        byte operandWidth = Instructions.GetSimpleOperandWidth(instruction);
        switch (operandWidth)
        {
            case 1:
            {
                operand = code[address];
                address++;
            }
            case 2:
            {
                operand = code[address] + (code[address+1] << 8);
                address = address + 2;
            }
        }
        address = address + AddJIXTableSize(instruction, operand);
        return instruction;
    }
    
    
    
    string Disassemble(<byte> code, ref uint address, long entryPointOffset, ref <uint> jumpLabels, ref <uint> jixLabels, bool doLabels)
    {
        string disassembledContent;
        byte cd = code[address];
        Instruction instruction = Instruction(cd);
  
        if ((instruction == Instruction.JIXB) || (instruction == Instruction.JIX))
        {
            long actualAddress = entryPointOffset + address;
            string addressContent = "0x" + actualAddress.ToHexString(4) + "  ";
            string opcode = Instructions.ToString(instruction);
            string opcontent = opcode;
            addressContent = addressContent + "0x" + cd.ToHexString(2) + " ";

            // table range
            address++;
            byte lsb = code[address];
            addressContent = addressContent + "0x" + lsb.ToHexString(2) + " ";
            address++;
            byte msb = code[address];
            uint operand = lsb + (msb << 8);
            addressContent = addressContent + "0x" + msb.ToHexString(2) + "  ";
            opcontent = opcontent + " [0x" + lsb.ToHexString(2) + ".." + "0x" + msb.ToHexString(2) +"]"; // range
            
            // jump back offset
            address++;
            lsb = code[address];
            address++;
            msb = code[address];
            opcontent = opcontent + " -0x" +msb.ToHexString(2) + lsb.ToHexString(2);
            
            uint backJump = (msb << 8) + lsb;
            
            string content = addressContent + opcontent;
            String.Build(ref content, Char.EOL);
            String.Build(ref disassembledContent, content);
            
            uint tableSize = Instructions.AddJIXTableSize(instruction, operand) - 2;
            String.Build(ref content);
            string widePadding = "                        ";
            byte count = 0;
            if (instruction == Instruction.JIX)
            {
                for (uint it = 0; it < tableSize/2; it++)
                {
                    address++;
                    lsb = code[address];
                    address++;
                    msb = code[address];
                    uint offset = (lsb + msb << 8);
                    if (doLabels)
                    {
                        if (offset == 0)
                        {
                            jixLabels.Append(0);
                        }
                        else
                        {
                            uint labelAddress = actualAddress - backJump + offset;
                            jixLabels.Append(labelAddress);
                        }
                    }
                    else
                    {
                        content = content + "0x" + offset.ToHexString(4) + " ";
                        count++;
                        if (count == 8)
                        {
                            String.Build(ref content, Char.EOL);
                            String.Build(ref disassembledContent, widePadding);
                            String.Build(ref disassembledContent, content);
                            String.Build(ref content);
                            count = 0;
                        }
                    }
                }
            }
            else
            {
                for (uint it = 0; it < tableSize; it++)
                {
                    address++;
                    byte offset = code[address];
                    if (doLabels)
                    {
                        if (offset == 0)
                        {
                            jixLabels.Append(0);
                        }
                        else
                        {
                            uint labelAddress = actualAddress - backJump + offset;
                            jixLabels.Append(labelAddress);
                        }
                    }
                    else
                    {
                        content = content + "0x" + offset.ToHexString(2) + " ";
                        count++;
                        if (count == 16)
                        {
                            String.Build(ref content, Char.EOL);
                            String.Build(ref disassembledContent, widePadding);
                            String.Build(ref disassembledContent, content);
                            String.Build(ref content);
                            count = 0;
                        }
                    }
                }
            }
            if (content.Length != 0)
            {
                String.Build(ref content, Char.EOL);
                String.Build(ref disassembledContent, widePadding);
                String.Build(ref disassembledContent, content);
            }
        }
        else
        {
            String.Build(ref disassembledContent, disassembleSingle(code, ref address, entryPointOffset, ref jumpLabels));
        }
        return disassembledContent;
    }
    
    
    string RenderReturnType(string methodKey)
    {
        <string,variant> methodSymbols = Code.GetMethodSymbols(methodKey);
        string returnType;
        if (methodSymbols.Contains("returntype"))
        {
            returnType = methodSymbols["returntype"];
            returnType += " ";
        }
        return returnType;
    }
    string RenderArguments(string methodKey)
    {
        <string,variant> methodSymbols = Code.GetMethodSymbols(methodKey);
        string arguments;
        if (methodSymbols.Contains("arguments"))
        {
            <string, <string> > argumentDict = methodSymbols["arguments"];
            uint i;
            foreach (var kv in argumentDict)
            {
                <string> argumentList = kv.value;
                if (i != 0)
                {
                    arguments += ", ";
                }
                if (argumentList[0] == "true")
                {
                    arguments = arguments + "ref ";
                }
                arguments = arguments + argumentList[1] + " " + argumentList[2];
                i++;   
           
            }      
        }
        return arguments;
    }
    
    bool isPackedBytes(Instruction instruction)
    {
        switch (instruction)
        {
            case Instruction.SYSCALL00:
            case Instruction.SYSCALL01:
            case Instruction.SYSCALL10:
            case Instruction.SYSCALLB0:
            case Instruction.SYSCALLB1:
            {
                return true;
            }
        }
        return false;
    }
    bool isPackedStackOffsets(Instruction instruction)
    {
        switch (instruction)
        {
            case Instruction.PUSHLOCALBB:
            case Instruction.INCLOCALBB:
            {
                return true;
            }
        }
        return false;
    }
    
    string disassembleSingle(<byte> code, ref uint address, long entryPointOffset, ref <uint> jumpLabels)
    {
        long actualAddress = entryPointOffset + address;
        string addressContent = "0x" + actualAddress.ToHexString(4) + "  ";
        
        byte cd = code[address];
        Instruction instruction = Instruction(cd);
        
        string opcode = Instructions.ToString(instruction);
        string content = opcode;
        
        addressContent = addressContent + "0x" + cd.ToHexString(2) + " ";
        
        byte operandWidth = Instructions.GetSimpleOperandWidth(instruction); // JIX never gets here
        uint op;
        
        string methodKey;
        bool isStackOffset = Instructions.OperandIsStackOffset(instruction);
        bool isJumpOffset  = Instructions.OperandIsAddressOffset(instruction);
        
        switch (operandWidth)
        {
            case 0:
            {
                addressContent = addressContent + "           ";
            }
            case 1:
            {
                long jumpTarget = long(address);
                address++;
                op = code[address];
                
                addressContent = addressContent + "0x" + op.ToHexString(2) + "       ";
                
                string offsetString;
                if (isStackOffset || isJumpOffset)
                {
                    int offset = int(op);
                    if (offset > 127)
                    {
                        offset = offset - 256; // 255 -> -1
                    }
                    string sign;
                    if (offset >= 0)
                    {
                        offsetString = "+";
                    }
                    offsetString = offsetString + offset.ToString();
                    jumpTarget = jumpTarget + offset + entryPointOffset;
                }
                if (isJumpOffset)
                {
                    content = content + " 0x" + jumpTarget.ToHexString(4);
                    content = content + " (" + offsetString + ")";
                    jumpLabels.Append(uint(jumpTarget));
                }
                else if (isStackOffset)
                {
                    content = content + " 0x" + op.ToHexString(2);
                    content = content + " (BP" + offsetString + ")";
                }
                else
                {
                    methodKey = "0x" + op.ToHexString(4);
                    content = content + " 0x" + op.ToHexString(2);
                }
            }
            case 2:
            {
                long jumpTarget = address;
                address++;
                byte lsb = code[address];
                addressContent = addressContent + "0x" + lsb.ToHexString(2) + " ";
                address++;
                byte msb = code[address];
                addressContent = addressContent + "0x" + msb.ToHexString(2) + "  ";
                op = lsb + (msb << 8);
                string offsetString;
                if (isStackOffset || isJumpOffset)
                {
                    long offset = op;
                    if (offset > 32767)
                    {
                        offset = offset - 65536; // 0x10000 -> -1
                    }
                    string sign;
                    if (offset >= 0)
                    {
                        offsetString = "+";
                    }
                    offsetString = offsetString + offset.ToString();
                    jumpTarget = jumpTarget + offset + entryPointOffset;
                }
                if (isJumpOffset)
                {
                    content = content + " 0x" + jumpTarget.ToHexString(4);
                    content = content + " (" + offsetString + ")";
                    jumpLabels.Append(uint(jumpTarget));
                }
                else if (isStackOffset)
                {
                    content = content + " 0x" + op.ToHexString(4);
                    content = content + " (BP" + offsetString  + ")";
                }
                else if (isPackedBytes(instruction))
                {
                    content = content + " 0x" + (op & 0xFF).ToHexString(2) + ", 0x" + (op >> 8).ToHexString(2);
                }
                else if (isPackedStackOffsets(instruction))
                {
                    int offset = int(op & 0xFF);
                    if (offset > 127)
                    {
                        offset = offset - 256; // 255 -> -1
                    }
                    string sign;
                    if (offset >= 0)
                    {
                        sign = "+";
                    }
                    content = content + " 0x" + (op & 0xFF).ToHexString(2) + " (BP" + sign + offset.ToString() + ")";
                    offset = int(op >> 8);
                    if (offset > 127)
                    {
                        offset = offset - 256; // 255 -> -1
                    }
                    sign = "";
                    if (offset >= 0)
                    {
                        sign = "+";
                    }
                    content = content + ", 0x" + (op >> 8).ToHexString(2) +" (BP" + sign + offset.ToString() + ")";
                }
                else
                {
                    methodKey = "0x" + op.ToHexString(4);
                    content = content + " " + methodKey;
                }
            }
        }
        switch (instruction)
        {
            case Instruction.PUSHLOCALB00:
            case Instruction.POPLOCALB00:
            case Instruction.POPCOPYLOCALB00:
            {
                content = content + " (BP+0)";
            }
            case Instruction.PUSHLOCALB01:
            case Instruction.POPLOCALB01:
            case Instruction.POPCOPYLOCALB01:
            {
                content = content + " (BP+1)";
            }
            case Instruction.SYSCALL:
            case Instruction.SYSCALL0:
            case Instruction.SYSCALL1:
            case Instruction.SYSCALL2:
            {
                byte iSysCall = byte(op & 0xFF);
                string syscallName = SysCalls.GetSysCallName(iSysCall);
                content = content.Pad(' ', 18);
                content = content + "// " + syscallName + "(..)";
            }
            case Instruction.SYSCALLB0:
            case Instruction.SYSCALLB1:
            {
                byte iSysCall = byte(op >> 8);
                string syscallName = SysCalls.GetSysCallName(iSysCall);
                content = content.Pad(' ', 18);
                content = content + "// " + syscallName + "(..)";
            }
            case Instruction.SYSCALL00:
            case Instruction.SYSCALL01:
            case Instruction.SYSCALL10:
            {
                byte iSysCall = byte(op & 0xFF);
                string syscallName = SysCalls.GetSysCallName(iSysCall);
                content = content.Pad(' ', 18);
                content = content + "// " + syscallName + "(..)";
                
                iSysCall = byte(op >> 8);
                syscallName = SysCalls.GetSysCallName(iSysCall);
                content = content + ", " + syscallName + "(..)";
            }
            case Instruction.LIBCALL:
            case Instruction.LIBCALL0:
            case Instruction.LIBCALL1:
            {
                byte iSysCall = byte(op & 0xFF);
                string libcallName = LibCalls.GetLibCallName(iSysCall);
                content = content.Pad(' ', 18);
                content = content + "// " + libcallName;
            }
            case Instruction.CALLB:
            case Instruction.CALL:
            {
                <string,variant> methodSymbols =  Code.GetMethodSymbols(methodKey);
                if ((methodSymbols.Count == 0) && methodKey.StartsWith("0xC"))
                {
                    // HOPPER_6502 method indices marked with 0xCnnn before replaced with addresses
                    methodKey = methodKey.Replace("0xC", "0x0");
                    methodSymbols =  Code.GetMethodSymbols(methodKey);
                }
                if (methodSymbols.Count != 0)
                {
                    string name = methodSymbols["name"];
                    content = content.Pad(' ', 17);
                    content = content + "// " + RenderReturnType(methodKey) + name + "(" + RenderArguments(methodKey) + ") ";
                }
            }
        } // switch
        content = addressContent + content;
        return content;
    }
}
