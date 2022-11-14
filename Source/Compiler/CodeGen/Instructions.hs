unit Instructions
{
    uses "/Source/System/System"
    
    enum Instruction
    {
        // unsigned
        ADD,
        SUB,
        DIV,
        MUL,
        MOD,
        EQ,
        NE,
        GT,
        LT,
        GE,
        LE,
        
        BOOLEANOR,
        BOOLEANAND,
        BITOR,
        BITAND,
        BITXOR,
        BITSHL,
        BITSHR,
        
        // instructions before here have
        // - no immediate operands, 
        // - pop 2 and push 1
        POP2PUSH1UNSIGNED = 0x20,    // ####
        
        // signed:
        ADDI,
        SUBI,
        DIVI,
        MULI,
        MODI,
        GTI,
        LTI,
        GEI,
        LEI,
        
        // instructions before here have
        // - no immediate operands, 
        // - pop 2 and push 1
        // - are signed operations
        POP2PUSH1SIGNED = 0x30,    // ####

        
        PUSHIB,       // operand is byte
        POPLOCALB,    // operand is the location to pop to: BP + offset
        PUSHLOCALB,   // operand is the location to push from: BP + offset
        POPRELB,      // like POPLOCAL but the absolute address to pop is taken from BP + offset
        PUSHRELB,     // like PUSHLOCAL but the absolute address to push is taken from BP + offset
        POPGLOBALB,   // operand is the absolute address to pop to
        PUSHGLOBALB,  // operand is the absolute address to push from
        PUSHSTACKADDRB, // operand is the offset from BP of the variable - convert to absolute stack address and push that
        
        INCLOCAL,
        DECLOCAL,
        
        SYSCALL0,     // syscall <byte operand>, overload 0
        SYSCALL1,     // syscall <byte operand>, overload 1
        SYSCALL,      // syscall <byte operand>,  overload [next]
        
        DUP,          // operand is offset 0..255 into stack where 0=[top], 1=[next], etc
        DECSP,

        DIE,       // 0x?? fail setting lastError to <byte operand>
  
        RETB,      // RET and pop <byte operand> bytes
        RETRETB,   // RET and pop <byte operand> bytes, but preserve [top] as the return value
      
        CALLB,     // <byte index operand>
        TESTBPB,   // verify that BP is what we expect it to be
                           
        // jump offsets: -1 means address of J instruction - 1, 0 means address after J instruction
        JZB,       // <signed byte offset>
        JNZB,      // <signed byte offset>
        JB,        // <signed byte offset>
        
        BYTEOPERAND = 0x60,  // ####
        
        // jump offsets: -1 means address of J instruction - 1, 0 means address after J instruction
        JZW,       // <signed int offset>
        JNZW,      // <signed int offset>
        JW,        // <signed int offset>
        
        CALLW,     // <integer index operand>
  
        RETW,      // RET and pop <uint operand> bytes
        RETRETW,   // RET and pop <uint operand> bytes, but preserve [top] as the return value
        
        PUSHIW,       // operand is uint
        POPLOCALW,    // operand is the location to pop to: BP + offset
        PUSHLOCALW,   // operand is the location to push from: BP + offset
        POPRELW,      // like POPLOCAL but the absolute address to pop is taken from BP + offset
        PUSHRELW,     // like PUSHLOCAL but the absolute address to push is taken from BP + offset
        POPGLOBALW,   // operand is the absolute address to pop to
        PUSHGLOBALW,  // operand is the absolute address to push from
        PUSHSTACKADDRW, // operand is the offset from BP of the variable - convert to absolute stack address and push that
        
        
        WORDOPERAND = 0x80,  // ####
        
        BOOLEANNOT,   // ![top] -> [top]
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
        
        NOP,
                    
        // instructions before here have no immediate operands
        NOOPERAND = 0xA0,  // ####
    }
    
    bool OperandIsStackOffset(Instruction instruction)
    {
        bool isOffset = false;
        switch (instruction)
        {
            case Instruction.INCLOCAL:
            {
                isOffset = true;
            }
            case Instruction.DECLOCAL:
            {
                isOffset = true;
            }
            
            case Instruction.POPLOCALB:
            {
                isOffset = true;
            }
            case Instruction.POPLOCALW:
            {
                isOffset = true;
            }
            case Instruction.PUSHLOCALB:
            {
                isOffset = true;
            }
            case Instruction.PUSHLOCALW:
            {
                isOffset = true;
            }
            case Instruction.PUSHSTACKADDRW:
            {
                isOffset = true;
            }
            
            
            case Instruction.POPRELB:
            {
                isOffset = true;
            }
            case Instruction.POPRELW:
            {
                isOffset = true;
            }
            case Instruction.PUSHRELB:
            {
                isOffset = true;
            }
            case Instruction.PUSHRELW:
            {
                isOffset = true;
            }
            case Instruction.PUSHSTACKADDRB:
            {
                isOffset = true;
            }
            
        }    
        return isOffset;
    }
    bool OperandIsAddressOffset(Instruction instruction)
    {
        bool isOffset = false;
        switch (instruction)
        {
            case Instruction.JB:
            {
                isOffset = true;
            }
            case Instruction.JZB:
            {
                isOffset = true;
            }
            case Instruction.JNZB:
            {
                isOffset = true;
            }
            case Instruction.JW:
            {
                isOffset = true;
            }
            case Instruction.JZW:
            {
                isOffset = true;
            }
            case Instruction.JNZW:
            {
                isOffset = true;
            }
            
        }    
        return isOffset;
    }
    byte OperandWidth(Instruction instruction)
    {
        byte result = 0;
        switch (instruction)
        {
            case Instruction.CALLB:
            {
                result = 1;
            }
            case Instruction.TESTBPB:
            {
                result = 1;
            }
            case Instruction.CALLW:
            {
                result = 2;
            }
            case Instruction.RETB:
            {
                result = 1;
            }
            case Instruction.RETW:
            {
                result = 2;
            }
            case Instruction.RETRETB:
            {
                result = 1;
            }
            case Instruction.RETRETW:
            {
                result = 2;
            }
            case Instruction.PUSHIB:
            {
                result = 1;
            }
            case Instruction.PUSHIW:
            {
                result = 2;
            }
            
            case Instruction.POPGLOBALB:
            {
                result = 1;
            }
            case Instruction.POPGLOBALW:
            {
                result = 2;
            }
            case Instruction.PUSHGLOBALB:
            {
                result = 1;
            }
            case Instruction.PUSHGLOBALW:
            {
                result = 2;
            }
            
            case Instruction.POPLOCALB:
            {
                result = 1;
            }
            case Instruction.POPLOCALW:
            {
                result = 2;
            }
            case Instruction.PUSHLOCALB:
            {
                result = 1;
            }
            case Instruction.PUSHLOCALW:
            {
                result = 2;
            }
            
            case Instruction.POPRELB:
            {
                result = 1;
            }
            case Instruction.POPRELW:
            {
                result = 2;
            }
            case Instruction.PUSHRELB:
            {
                result = 1;
            }
            case Instruction.PUSHRELW:
            {
                result = 2;
            }
            case Instruction.PUSHSTACKADDRW:
            {
                result = 2;
            }
            case Instruction.PUSHSTACKADDRB:
            {
                result = 1;
            }
            
            
            case Instruction.SYSCALL:
            {
                result = 2;
            }
            case Instruction.SYSCALL0:
            {
                result = 1;
            }
            case Instruction.SYSCALL1:
            {
                result = 1;
            }
            case Instruction.JB:
            {
                result = 1;
            }
            case Instruction.JZB:
            {
                result = 1;
            }
            case Instruction.JNZB:
            {
                result = 1;
            }
            case Instruction.JW:
            {
                result = 2;
            }
            case Instruction.JZW:
            {
                result = 2;
            }
            case Instruction.JNZW:
            {
                result = 2;
            }
            
            case Instruction.INCLOCAL:
            {
                result = 1;
            }
            case Instruction.DECLOCAL:
            {
                result = 1;
            }
            case Instruction.DUP:
            {
                result = 1;
            }
            case Instruction.DECSP:
            {
                result = 1;
            }
            case Instruction.DIE:
            {
                result = 1;
            }
        }
        return result;
    }
    
    
    string ToString(Instruction instruction)
    {
        string result;
        switch (instruction)
        {
            case Instruction.ENTER:
            {
                result = "ENTER";
            }
            case Instruction.NOP:
            {
                result = "NOP";
            }
            case Instruction.CALLB:
            {
                result = "CALLB";
            }
            case Instruction.TESTBPB:
            {
                result = "TESTBPB";
            }
            case Instruction.CALLW:
            {
                result = "CALLW";
            }
            case Instruction.RET0:
            {
                result = "RET0";
            }
            case Instruction.RETB:
            {
                result = "RETB";
            }
            case Instruction.RETW:
            {
                result = "RETW";
            }
            case Instruction.RETRETB:
            {
                result = "RETRETB";
            }
            case Instruction.RETRETW:
            {
                result = "RETRETW";
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
            case Instruction.PUSHIW:
            {
                result = "PUSHIW";
            }
            
            case Instruction.INCLOCAL:
            {
                result = "INCLOCAL";
            }
            case Instruction.DECLOCAL:
            {
                result = "DECLOCAL";
            }
            
            case Instruction.POPGLOBALB:
            {
                result = "POPGLOBALB";
            }
            case Instruction.POPGLOBALW:
            {
                result = "POPGLOBALW";
            }
            case Instruction.PUSHGLOBALB:
            {
                result = "PUSHGLOBALB";
            }
            case Instruction.PUSHGLOBALW:
            {
                result = "PUSHGLOBALW";
            }
            case Instruction.PUSHSTACKADDRB:
            {
                result = "PUSHSTACKADDRB";
            }
            case Instruction.PUSHSTACKADDRW:
            {
                result = "PUSHSTACKADDRW";
            }      
                          
            case Instruction.POPLOCALB:
            {
                result = "POPLOCALB";
            }
            case Instruction.POPLOCALW:
            {
                result = "POPLOCALW";
            }
            case Instruction.PUSHLOCALB:
            {
                result = "PUSHLOCALB";
            }
            case Instruction.PUSHLOCALW:
            {
                result = "PUSHLOCALW";
            }
            
            case Instruction.POPRELB:
            {
                result = "POPRELB";
            }
            case Instruction.POPRELW:
            {
                result = "POPRELW";
            }
            case Instruction.PUSHRELB:
            {
                result = "PUSHRELB";
            }
            case Instruction.PUSHRELW:
            {
                result = "PUSHRELW";
            }
            
            case Instruction.BOOLEANNOT:
            {
                result = "BOOLEANNOT";
            }
            case Instruction.BOOLEANAND:
            {
                result = "BOOLEANAND";
            }
            case Instruction.BOOLEANOR:
            {
                result = "BOOLEANOR";
            }
            case Instruction.BITAND:
            {
                result = "BITAND";
            }
            case Instruction.BITOR:
            {
                result = "BITOR";
            }
            case Instruction.BITNOT:
            {
                result = "BITNOT";
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
            case Instruction.JW:
            {
                result = "JW";
            }
            case Instruction.JZW:
            {
                result = "JZW";
            }
            case Instruction.JNZW:
            {
                result = "JNZW";
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
}
