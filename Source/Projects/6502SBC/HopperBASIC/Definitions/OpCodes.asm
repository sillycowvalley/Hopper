unit OpCodes 
{
   // OpCode definitions for HopperBASIC JIT compiler
   // 
   // Encoding scheme uses 6+2 bits:
   // - Bits 7-6: Operand count (00=none, 01=one byte, 10=two bytes, 11=reserved)
   // - Bits 5-0: OpCode value (0-63 per group)
   //
   // This design enables:
   // - Fast dispatch: Single byte fetch reveals operand count
   // - Compact encoding: Common operations use single byte
   // - Future expansion: 64 opcodes reserved for complex instructions
   // - Resolve-and-replace: Unresolved opcodes patch to fast versions after first execution
   
   enum OpCode
   {
       // === INVALID OPCODE ===
       INVALID      = 0x00,  // Invalid opcode - triggers error
       
       // === OPCODES WITH NO OPERANDS (0x01-0x3F) ===
       // Bits 7-6: 00 (no operands)
       // Bits 5-0: OpCode (1-63 available, 0x00 reserved for INVALID)
       
       // Arithmetic operations
       ADD          = 0x01,  // Pop two values, push sum
       SUB          = 0x02,  // Pop two values, push difference
       MUL          = 0x03,  // Pop two values, push product
       DIV          = 0x04,  // Pop two values, push quotient
       MOD          = 0x05,  // Pop two values, push remainder
       NEG          = 0x06,  // Pop value, push negation
       
       // Bitwise operations
       BITWISE_AND  = 0x07,  // Pop two values, push bitwise AND
       BITWISE_OR   = 0x08,  // Pop two values, push bitwise OR
       
       // Logical operations (BIT type only)
       LOGICAL_AND  = 0x09,  // Pop two BIT values, push logical AND
       LOGICAL_OR   = 0x0A,  // Pop two BIT values, push logical OR
       LOGICAL_NOT  = 0x0B,  // Pop BIT value, push logical NOT
       
       // Comparison operations (return BIT type)
       EQ           = 0x0C,  // Pop two values, push equality result (BIT)
       NE           = 0x0D,  // Pop two values, push inequality result (BIT)
       LT           = 0x0E,  // Pop two values, push less than result (BIT)
       GT           = 0x0F,  // Pop two values, push greater than result (BIT)
       LE           = 0x10,  // Pop two values, push less or equal result (BIT)
       GE           = 0x11,  // Pop two values, push greater or equal result (BIT)
       
       // Stack manipulation
       DECSP        = 0x14,  // Decrement stack pointer (discard top value)
       DUP          = 0x15,  // Duplicate top stack value
       
       // Utility and common literals
       NOP          = 0x16,  // No operation (useful for code generation/optimization)
       PUSH0        = 0x17,  // Push INT 0 (very common literal, no operand)
       PUSH1        = 0x18,  // Push INT 1 (very common literal, no operand)
       PUSHVOID     = 0x19,  // Push VOID 0 (very common literal, no operand)
       
       HALT         = 0x20,  // end of REPL opcode stream
       
       // Function frame management
       ENTER        = 0x1A,  // Enter function frame - push BP, SP->BP
       
       // === OPCODES WITH ONE BYTE OPERAND (0x40-0x7F) ===
       // Bits 7-6: 01 (one byte operand)
       // Bits 5-0: OpCode (0-63 available)
       
       // Literal pushes (small values)
       PUSHBIT      = 0x40,  // Push BIT immediate [value] (0 or 1)
       PUSHBYTE     = 0x41,  // Push BYTE immediate [value] (0-255)
       
       // Local variable operations (stack frame relative)
       PUSHLOCAL    = 0x42,  // Push local variable [signed_offset]
       POPLOCAL     = 0x43,  // Pop to local variable [signed_offset]
       
       // Control flow (short jumps)
       JUMPB        = 0x44,  // Unconditional jump [signed_delta]
       JUMPZB       = 0x45,  // Jump if zero [signed_delta]
       JUMPNZB      = 0x46,  // Jump if non-zero [signed_delta]
       
       // System calls
       SYSCALL      = 0x47,  // System call [function_id]
       
       // Control flow
       RETURN       = 0x49,  // Return from function (no return value)
       RETURNVAL    = 0x4A,  // Return from function (pop return value from stack)
       
       // === OPCODES WITH TWO BYTE OPERANDS (0x80-0xBF) ===
       // Bits 7-6: 10 (two byte operands)
       // Bits 5-0: OpCode (0-63 available)
       
       // Literal pushes (16-bit values)
       PUSHINT      = 0x80,  // Push INT immediate [lsb] [msb]
       PUSHWORD     = 0x81,  // Push WORD immediate [lsb] [msb]
       PUSHCSTRING  = 0x82,  // Push CONSTSTRING pointer [addr_lsb] [addr_msb]
       
       // Function calls (unresolved → resolved)
       CALL         = 0x83,  // Call function by name [name_offset_lsb] [name_offset_msb]
       CALLF        = 0x84,  // Call function fast [node_lsb] [node_msb] (resolved on first call)
       
       // Global variable operations (unresolved → resolved)
       PUSHGLOBAL   = 0x87,  // Push global [node_lsb] [node_msb] (resolved at compile time)
       POPGLOBAL    = 0x88,  // Pop to global [node_lsb] [node_msb] (resolved at compile time)
       
       // Control flow (long jumps)
       JUMPW        = 0x89,  // Unconditional jump [lsb] [msb]
       JUMPZW       = 0x8A,  // Jump if zero [lsb] [msb]
       JUMPNZW      = 0x8B,  // Jump if non-zero [lsb] [msb]
       
       // === RESERVED FOR FUTURE EXTENSIONS (0xC0-0xFF) ===
       // Bits 7-6: 11 (reserved)
       // Reserved for future complex instructions or extensions
       // Potential uses: Variable-length operands, complex string operations,
       //                array access, advanced control flow
   }
   
   
   const string opcodeUNDEFINED = "UNDEFINED\0";
   const string opcodeINVALID = "INVALID\0";
   const string opcodeADD     = "ADD\0";
   
#ifdef DEBUG
   // Input: opcode in X
   // Output: string pointer in ZP.STR
   ToString()
   {
        switch (X)
        {
            case OpCode.INVALID:
            {
                LDA #(opcodeINVALID % 256)
                STA ZP.STRL
                LDA #(opcodeINVALID / 256)
                STA ZP.STRH
            }
            case OpCode.ADD:
            {
                LDA #(opcodeADD % 256)
                STA ZP.STRL
                LDA #(opcodeADD / 256)
                STA ZP.STRH
            }
            default:
            {
                LDA #(opcodeUNDEFINED % 256)
                STA ZP.STRL
                LDA #(opcodeUNDEFINED / 256)
                STA ZP.STRH
            }
        }
   }
#endif

}