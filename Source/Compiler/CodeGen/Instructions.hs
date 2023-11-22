unit Instructions
{
    uses "/Source/Compiler/Tokens/SysCalls"
    uses "/Source/Compiler/Tokens/LibCalls"
    
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
        
        BOOLOR,
        BOOLAND,
        BITOR,
        BITAND,

        BITSHL,
        BITSHR,
        
        // instructions before here have
        // - no immediate operands, 
        // - pop 2 and push 1

        //POP2PUSH1UNSIGNED = 0x20,    // OLDOPS
        
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

        //POP2PUSH1SIGNED = 0x30,    // OLDOPS
        
        PUSHIB,       // operand is byte
        POPLOCALB,    // operand is the location to pop to: BP + offset
        PUSHLOCALB,   // operand is the location to push from: BP + offset
        POPRELB,      // like POPLOCAL but the absolute address to pop is taken from BP + offset
        PUSHRELB,     // like PUSHLOCAL but the absolute address to push is taken from BP + offset
        POPGLOBALB,   // operand is the absolute address to pop to
        PUSHGLOBALB,  // operand is the absolute address to push from
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
        RETRETB,   // RET and pop <byte operand> bytes, but preserve [top] as the return value
      
        CALLB,     // <byte index operand>
        TESTBPB,   // verify that BP is what we expect it to be
                           
        // jump offsets: -1 means address of J instruction - 1, 0 means address after J instruction
        JZB,       // <signed byte offset>
        JNZB,      // <signed byte offset>
        JB,        // <signed byte offset>
        
        //BYTEOPERAND = 0x60,  // OLDOPS
        
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
        
        INCLOCALBB,
        PUSHIWLE,

        //WORDOPERAND = 0x80,  // OLDOPS
        
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
        POPLOCALB02,
        PUSHLOCALB00,
        PUSHLOCALB02,
        
        NOP,
        
        CAST, // operand is value type (byte) - change top of stack to this type

        PUSHGLOBALBB,  // operand is the absolute address to push from, x2
        INCGLOBALB,
        DECGLOBALB,
        
        PUSHIWLT,
        
        PUSHLOCALBB,
        
        POPCOPYLOCALB,
        POPCOPYRELB,
        POPCOPYGLOBALB,
        POPCOPYLOCALW,   // unused?
        POPCOPYRELW,     // unused?
        POPCOPYGLOBALW,
        
        POPCOPYLOCALB00,
        POPCOPYLOCALB02,
        
        ENTERB,
        
        PUSHDW, // operand uint, gets resolved at runtime from delegate index to delegate pointer
        RETFAST,
        PUSHDB,
        EXIT, // NOP, only used by inline code
        
        BITXOR, // for 'flags'
        
        PUSHIWLEI,
        INCGLOBALBB,
        
        JREL,
        JIXB,
        JIXW,
        
        CALLIW,
        
        PUSHIBLE,
        PUSHIBEQ,
        
        ADDB,
        SUBB,
        
        LIBCALL,
        
        UNDEFINED,
    }
    
    bool IsRET(Instruction instruction)
    {
        switch(instruction)
        {
            case Instruction.RET0:
            case Instruction.RETFAST:
            case Instruction.RETB:
            case Instruction.RETRETB:
            case Instruction.RETW:
            case Instruction.RETRETW:
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
            case Instruction.DECLOCALB:
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
            case Instruction.POPRELW:
            case Instruction.POPLOCALW:
            case Instruction.POPCOPYLOCALW:
            case Instruction.POPCOPYRELW:
            case Instruction.PUSHLOCALW:
            case Instruction.PUSHSTACKADDRW:
            case Instruction.PUSHRELW:
            {
                width = 2;
                isStackOffset = true;
            }
            
            case Instruction.RET0:
            case Instruction.RETFAST:
            {
                isRET = true;
            }
            case Instruction.RETB:
            case Instruction.RETRETB:
            {
                isRET = true;
                width = 1;
            }
            case Instruction.RETW:
            case Instruction.RETRETW:
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
            case Instruction.JW:
            case Instruction.JZW:
            case Instruction.JNZW:
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
            case Instruction.PUSHDB:
            case Instruction.POPGLOBALB:
            case Instruction.POPCOPYGLOBALB:
            case Instruction.PUSHGLOBALB:
            case Instruction.SYSCALL:
            case Instruction.SYSCALL0:
            case Instruction.SYSCALL1:
            case Instruction.LIBCALL:
            case Instruction.INCGLOBALB:
            case Instruction.DECGLOBALB:
            case Instruction.DUP:
            case Instruction.DECSP:
            case Instruction.DIE:
            case Instruction.ENTERB:
            case Instruction.PUSHIBLE:
            case Instruction.PUSHIBEQ:
            {
                width = 1;
            }
            
            case Instruction.CALLW:
            case Instruction.PUSHIW:
            case Instruction.PUSHDW:
            case Instruction.PUSHIWLE:
            case Instruction.PUSHIWLEI:
            case Instruction.PUSHIWLT:
            case Instruction.POPGLOBALW:
            case Instruction.POPCOPYGLOBALW:
            case Instruction.PUSHGLOBALBB:
            case Instruction.PUSHLOCALBB:
            case Instruction.PUSHGLOBALW:
            case Instruction.INCLOCALBB:
            case Instruction.INCGLOBALBB:
            case Instruction.JIXB:
            case Instruction.JIXW:
            case Instruction.CALLIW:
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
            case Instruction.DECLOCALB:
            case Instruction.POPLOCALB:
            case Instruction.POPLOCALW:
            case Instruction.POPRELB:
            case Instruction.POPRELW:
            case Instruction.POPCOPYLOCALB:
            case Instruction.POPCOPYLOCALW:
            case Instruction.POPCOPYRELB:
            case Instruction.POPCOPYRELW:
            case Instruction.PUSHLOCALB:
            case Instruction.PUSHLOCALW:
            case Instruction.PUSHSTACKADDRW:
            case Instruction.PUSHRELB:
            case Instruction.PUSHRELW:
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
            case Instruction.JW:
            case Instruction.JZW:
            case Instruction.JNZW:
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
            case Instruction.RETRETB:
            case Instruction.PUSHIB:
            case Instruction.ADDB:
            case Instruction.SUBB:
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
            case Instruction.LIBCALL:
            case Instruction.JB:
            case Instruction.JZB:
            case Instruction.JNZB:
            case Instruction.INCGLOBALB:
            case Instruction.DECGLOBALB:
            case Instruction.INCLOCALB:
            case Instruction.DECLOCALB:
            case Instruction.DUP:
            case Instruction.DECSP:
            case Instruction.DIE:
            case Instruction.ENTERB:
            case Instruction.PUSHIBLE:
            case Instruction.PUSHIBEQ:
            {
                result = 1;
            }
            
            case Instruction.CALLW:
            case Instruction.RETW:
            case Instruction.RETRETW:
            case Instruction.PUSHIW:
            case Instruction.PUSHDW:
            case Instruction.PUSHIWLE:
            case Instruction.PUSHIWLEI:
            case Instruction.PUSHIWLT:
            case Instruction.POPGLOBALW:
            case Instruction.POPLOCALW:
            case Instruction.POPRELW:
            case Instruction.POPCOPYGLOBALW:
            case Instruction.POPCOPYLOCALW:
            case Instruction.POPCOPYRELW:
            case Instruction.PUSHGLOBALBB:
            case Instruction.PUSHLOCALBB:
            case Instruction.PUSHGLOBALW:
            case Instruction.PUSHLOCALW:
            case Instruction.PUSHRELW:
            case Instruction.PUSHSTACKADDRW:
            case Instruction.JW:
            case Instruction.JZW:
            case Instruction.JNZW:
            case Instruction.INCLOCALBB:
            case Instruction.INCGLOBALBB:
            case Instruction.JIXB:
            case Instruction.JIXW:
            case Instruction.CALLIW:
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
            case Instruction.CALLW:
            {
                result = "CALLW";
            }
            case Instruction.CALLIW:
            {
                result = "CALLIW";
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
            case Instruction.RETFAST:
            {
                result = "RETFAST";
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
            case Instruction.LIBCALL:
            {
                result = "LIBCALL";
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
            case Instruction.PUSHIW:
            {
                result = "PUSHIW";
            }
            case Instruction.PUSHDW:
            {
                result = "PUSHDW";
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
            case Instruction.PUSHIWLE:
            {
                result = "PUSHIWLE";
            }
            case Instruction.PUSHIWLEI:
            {
                result = "PUSHIWLEI";
            }
            case Instruction.PUSHIWLT:
            {
                result = "PUSHIWLT";
            }
            
            case Instruction.INCGLOBALB:
            {
                result = "INCGLOBALB";
            }
            case Instruction.DECGLOBALB:
            {
                result = "DECGLOBALB";
            }
            case Instruction.INCLOCALB:
            {
                result = "INCLOCALB";
            }
            case Instruction.INCLOCALBB:
            {
                result = "INCLOCALBB";
            }
            case Instruction.INCGLOBALBB:
            {
                result = "INCGLOBALBB";
            }
            case Instruction.DECLOCALB:
            {
                result = "DECLOCALB";
            }
            
            case Instruction.POPGLOBALB:
            {
                result = "POPGLOBALB";
            }
            case Instruction.POPGLOBALW:
            {
                result = "POPGLOBALW";
            }
            case Instruction.POPLOCALB:
            {
                result = "POPLOCALB";
            }
            case Instruction.POPLOCALW:
            {
                result = "POPLOCALW";
            }
            case Instruction.POPRELB:
            {
                result = "POPRELB";
            }
            case Instruction.POPRELW:
            {
                result = "POPRELW";
            }
            
            case Instruction.POPCOPYGLOBALB:
            {
                result = "POPCOPYGLOBALB";
            }
            case Instruction.POPCOPYGLOBALW:
            {
                result = "POPCOPYGLOBALW";
            }
            case Instruction.POPCOPYLOCALB:
            {
                result = "POPCOPYLOCALB";
            }
            case Instruction.POPCOPYLOCALW:
            {
                result = "POPCOPYLOCALW";
            }
            case Instruction.POPCOPYRELB:
            {
                result = "POPCOPYRELB";
            }
            case Instruction.POPCOPYRELW:
            {
                result = "POPCOPYRELW";
            }
            
            
            case Instruction.POPLOCALB02:
            {
                result = "POPLOCALB02";
            }
            case Instruction.POPLOCALB00:
            {
                result = "POPLOCALB00";
            }
            case Instruction.POPCOPYLOCALB02:
            {
                result = "POPCOPYLOCALB02";
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
                          
            
            case Instruction.PUSHLOCALB:
            {
                result = "PUSHLOCALB";
            }
            case Instruction.PUSHLOCALB00:
            {
                result = "PUSHLOCALB00";
            }
            case Instruction.PUSHLOCALB02:
            {
                result = "PUSHLOCALB02";
            }
            case Instruction.PUSHLOCALW:
            {
                result = "PUSHLOCALW";
            }
            
            case Instruction.PUSHRELB:
            {
                result = "PUSHRELB";
            }
            case Instruction.PUSHRELW:
            {
                result = "PUSHRELW";
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
            case Instruction.JIXB:
            {
                result = "JIXB";
            }
            case Instruction.JIXW:
            {
                result = "JIXW";
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
    uint AddJIXTableSize(Instruction instruction, uint operand)
    {
        uint tableSize;
        switch (instruction)
        {
            case Instruction.JIXW:
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
    
    string Disassemble(<byte> code, ref uint address, uint entryPointAddress, ref <uint> jumpLabels, ref <uint> jixLabels, bool doLabels)
    {
        string disassembledContent;
        byte cd = code[address];
        Instruction instruction = Instruction(cd);
  
        if ((instruction == Instruction.JIXB) || (instruction == Instruction.JIXW))
        {
            uint actualAddress = entryPointAddress + address;
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
            byte lsb = code[address];
            address++;
            byte msb = code[address];
            opcontent = opcontent + " -0x" +msb.ToHexString(2) + lsb.ToHexString(2);
            
            uint backJump = (msb << 8) + lsb;
            
            string content = addressContent + opcontent;
            String.Build(ref content, char(0x0A));
            String.Build(ref disassembledContent, content);
            
            uint tableSize = Instructions.AddJIXTableSize(instruction, operand) - 2;
            String.Build(ref content);
            string widePadding = "                        ";
            byte count = 0;
            if (instruction == Instruction.JIXW)
            {
                for (uint it = 0; it < tableSize/2; it++)
                {
                    address++;
                    byte lsb = code[address];
                    address++;
                    byte msb = code[address];
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
                            String.Build(ref content, char(0x0A));
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
                            String.Build(ref content, char(0x0A));
                            String.Build(ref disassembledContent, widePadding);
                            String.Build(ref disassembledContent, content);
                            String.Build(ref content);
                            count = 0;
                        }
                    }
                }
            }
            if (content.Length > 0)
            {
                String.Build(ref content, char(0x0A));
                String.Build(ref disassembledContent, widePadding);
                String.Build(ref disassembledContent, content);
            }
        }
        else
        {
            String.Build(ref disassembledContent, disassembleSingle(code, ref address, entryPointAddress, ref jumpLabels));
        }
        return disassembledContent;
    }
    string disassembleSingle(<byte> code, ref uint address, uint entryPointAddress, ref <uint> jumpLabels)
    {
        uint actualAddress = entryPointAddress + address;
        string addressContent = "0x" + actualAddress.ToHexString(4) + "  ";
        
        byte cd = code[address];
        Instruction instruction = Instruction(cd);
        
        string opcode = Instructions.ToString(instruction);
        string content = opcode;
        
        addressContent = addressContent + "0x" + cd.ToHexString(2) + " ";
        
        byte operandWidth = Instructions.GetSimpleOperandWidth(instruction); // JIX never gets here
        byte iSysCall;
        
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
                long jumpTarget = address;
                address++;
                byte op = code[address];
                
                addressContent = addressContent + "0x" + op.ToHexString(2) + "       ";
                
                iSysCall = op;
                string offsetString;
                if (isStackOffset || isJumpOffset)
                {
                    int offset = op;
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
                    jumpTarget = jumpTarget + offset + long(entryPointAddress);
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
                uint op = lsb + (msb << 8);
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
                    jumpTarget = jumpTarget + offset + long(entryPointAddress);
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
                else
                {
                    methodKey = "0x" + op.ToHexString(4);
                    content = content + " " + methodKey;
                }
            }
        }
        
        if ((instruction == Instruction.SYSCALL)
         || (instruction == Instruction.SYSCALL0)
         || (instruction == Instruction.SYSCALL1)
           )
        {
            string syscallName = SysCalls.GetSysCallName(iSysCall);
            content = content + "  // " + syscallName;
        }
        if (instruction == Instruction.LIBCALL)
        {
            string libcallName = LibCalls.GetLibCallName(iSysCall);
            content = content + "  // " + libcallName;
        }
        if ((instruction == Instruction.CALLB)
         || (instruction == Instruction.CALLW)
           )
        {
            <string,variant> methodSymbols =  Code.GetMethodSymbols(methodKey);
            if ((methodSymbols.Count == 0) && methodKey.StartsWith("0xC"))
            {
                // H6502 method indices marked with 0xCnnn before replaced with addresses
                methodKey = methodKey.Replace("0xC", "0x0");
                methodSymbols =  Code.GetMethodSymbols(methodKey);
            }
            if (methodSymbols.Count > 0)
            {
                string name = methodSymbols["name"];
                if (instruction == Instruction.CALLB)
                {
                    content = content + "  ";
                }
                content = content + "   // " + name;
            }
        }
        if (instruction == Instruction.CALLIW)
        {
            // TODO : reverse lookup of method from address to index to add name to comment
        }
        content = addressContent + content;
        return content;
    }
}
