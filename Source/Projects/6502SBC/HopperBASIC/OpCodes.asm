unit OpCodes 
{
    // OpCode definitions for HopperBASIC JIT compiler
    // 
    // Encoding scheme uses 6+2 bits:
    // - Bits 7-6: Operand count (00=none, 01=one byte, 10=two bytes, 11=reserved)
    // - Bits 5-0: Opcode value (0-63 per group)
    //
    // This design enables:
    // - Fast dispatch: Single byte fetch reveals operand count
    // - Compact encoding: Common operations use single byte
    // - Future expansion: 64 opcodes reserved for complex instructions
    
    enum OpcodeType
    {
        // === OPCODES WITH NO OPERANDS (0x00-0x3F) ===
        // Bits 7-6: 00 (no operands)
        // Bits 5-0: Opcode (0-63 available)
        
        // Arithmetic operations
        ADD          = 0x00,  // Pop two values, push sum
        SUB          = 0x01,  // Pop two values, push difference
        MUL          = 0x02,  // Pop two values, push product
        DIV          = 0x03,  // Pop two values, push quotient
        MOD          = 0x04,  // Pop two values, push remainder
        NEG          = 0x05,  // Pop value, push negation
        
        // Bitwise operations
        BITWISE_AND  = 0x06,  // Pop two values, push bitwise AND
        BITWISE_OR   = 0x07,  // Pop two values, push bitwise OR
        
        // Logical operations (BIT type only)
        LOGICAL_AND  = 0x08,  // Pop two BIT values, push logical AND
        LOGICAL_OR   = 0x09,  // Pop two BIT values, push logical OR
        LOGICAL_NOT  = 0x0A,  // Pop BIT value, push logical NOT
        
        // Comparison operations (return BIT type)
        EQ           = 0x0B,  // Pop two values, push equality result (BIT)
        NE           = 0x0C,  // Pop two values, push inequality result (BIT)
        LT           = 0x0D,  // Pop two values, push less than result (BIT)
        GT           = 0x0E,  // Pop two values, push greater than result (BIT)
        LE           = 0x0F,  // Pop two values, push less or equal result (BIT)
        GE           = 0x10,  // Pop two values, push greater or equal result (BIT)
        
        // Control flow
        RETURN       = 0x11,  // Return from function (no return value)
        RETURNVAL    = 0x12,  // Return from function (pop return value from stack)
        
        // Stack manipulation
        DECSP        = 0x13,  // Decrement stack pointer (discard top value)
        DUP          = 0x14,  // Duplicate top stack value
        
        // Utility
        NOP          = 0x15,  // No operation (useful for code generation/optimization)
        
        // Available: 0x16-0x3F (42 opcodes remaining in this group)
        
        // === OPCODES WITH ONE BYTE OPERAND (0x40-0x7F) ===
        // Bits 7-6: 01 (one byte operand)
        // Bits 5-0: Opcode (0-63 available)
        
        // Literal pushes (small values)
        PUSHBIT      = 0x40,  // Push BIT immediate [value]
        PUSHBYTE     = 0x41,  // Push BYTE immediate [value]
        
        // Local variable operations
        PUSHLOCAL    = 0x42,  // Push local variable [signed_offset]
        POPLOCAL     = 0x43,  // Pop to local variable [signed_offset]
        
        // Control flow (short jumps)
        JUMPB        = 0x44,  // Unconditional jump [signed_delta]
        JUMPZB       = 0x45,  // Jump if zero [signed_delta]
        JUMPNZB      = 0x46,  // Jump if non-zero [signed_delta]
        
        // Function calls and system calls
        CALL         = 0x47,  // Function call [function_index]
        SYSCALL      = 0x48,  // System call [function_id]
        
        // Available: 0x49-0x7F (55 opcodes remaining in this group)
        
        // === OPCODES WITH TWO BYTE OPERANDS (0x80-0xBF) ===
        // Bits 7-6: 10 (two byte operands)
        // Bits 5-0: Opcode (0-63 available)
        
        // Literal pushes (16-bit values)
        PUSHINT      = 0x80,  // Push INT immediate [lsb] [msb]
        PUSHWORD     = 0x81,  // Push WORD immediate [lsb] [msb]
        
        // Global variable operations (16-bit node addresses)
        PUSHGLOBAL   = 0x82,  // Push global variable [addr_lsb] [addr_msb]
        POPGLOBAL    = 0x83,  // Pop to global variable [addr_lsb] [addr_msb]
        
        // Control flow (long jumps)
        JUMPW        = 0x84,  // Unconditional jump [lsb] [msb]
        JUMPZW       = 0x85,  // Jump if zero [lsb] [msb]
        JUMPNZW      = 0x86,  // Jump if non-zero [lsb] [msb]
        
        // Available: 0x87-0xBF (57 opcodes remaining in this group)
        
        // === RESERVED FOR FUTURE EXTENSIONS (0xC0-0xFF) ===
        // Bits 7-6: 11 (reserved)
        // Reserved for future complex instructions or extensions
    }
    
    // **Instruction Format Summary:**
    //
    // **Group 0: No Operands (0x00-0x3F)**
    //   [OPCODE]
    //   Examples:
    //     ADD             - Pop two values, push sum
    //     NEG             - Pop value, push negation
    //     EQ              - Pop two values, push equality result
    //     RETURN          - Return from function
    //     DUP             - Duplicate top stack value
    //
    // **Group 1: One Byte Operand (0x40-0x7F)**
    //   [OPCODE][OPERAND]
    //   Examples: 
    //     PUSHBIT 0x01        - Push BIT value 1
    //     PUSHBYTE 0xFF       - Push BYTE value 255
    //     PUSHLOCAL 0x02      - Push local at offset +2
    //     PUSHLOCAL 0xFE      - Push argument at offset -2 (signed)
    //     JUMPB 0x0A          - Jump forward 10 bytes
    //     JUMPB 0xF6          - Jump backward 10 bytes (signed)
    //     SYSCALL 0x01        - System call PRINT
    //
    // **Group 2: Two Byte Operands (0x80-0xBF)**
    //   [OPCODE][LSB][MSB]
    //   Examples:
    //     PUSHINT 0x39 0x30   - Push INT value 12345 (0x3039)
    //     PUSHWORD 0xFF 0xFF  - Push WORD value 65535
    //     PUSHGLOBAL 0x10 0x40 - Push global variable at node address 0x4010
    //     POPGLOBAL 0x10 0x40  - Pop to global variable at node address 0x4010
    //     JUMPW 0x00 0x01     - Jump forward 256 bytes
    //     JUMPW 0x00 0xFF     - Jump backward 256 bytes (signed)
    //
    // **Group 3: Reserved (0xC0-0xFF)**
    //   Reserved for future complex instructions or extensions
    //
    // **Variable Access:**
    // - Global addresses: 16-bit node addresses from Variables.Find()
    // - Local offsets: -128 to +127 (signed)
    //   - Negative: Function arguments (-128 to -1)
    //   - Positive: Local variables (1 to +127)  
    //   - Zero: Reserved/invalid
    // - Jump deltas: Signed relative to byte AFTER the instruction
    // - System call IDs: 0x01=PRINT, 0x02=PRINTLN
    //
    // **Execution Speed Benefits:**
    // - Single instruction fetch reveals operand count
    // - No lookup table needed for instruction length
    // - Enables fast opcode dispatching with simple bit operations
}
