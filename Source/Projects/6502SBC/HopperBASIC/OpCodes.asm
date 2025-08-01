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
    // - Resolve-and-replace: Unresolved opcodes patch to fast versions after first execution
    
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
        
        // Stack manipulation
        DECSP        = 0x13,  // Decrement stack pointer (discard top value)
        DUP          = 0x14,  // Duplicate top stack value
        
        // Utility and common literals
        NOP          = 0x15,  // No operation (useful for code generation/optimization)
        PUSH0        = 0x16,  // Push INT 0 (very common literal, no operand)
        PUSH1        = 0x17,  // Push INT 1 (very common literal, no operand)
        
        // Function frame management
        ENTER        = 0x18,  // Enter function frame - push BP, SP->BP
        
        
        // Available: 0x19-0x3F (40 opcodes remaining in this group)
        
        // === OPCODES WITH ONE BYTE OPERAND (0x40-0x7F) ===
        // Bits 7-6: 01 (one byte operand)
        // Bits 5-0: Opcode (0-63 available)
        
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
        
        
        // Available: 0x49-0x7F (55 opcodes remaining in this group)
        
        // === OPCODES WITH TWO BYTE OPERANDS (0x80-0xBF) ===
        // Bits 7-6: 10 (two byte operands)
        // Bits 5-0: Opcode (0-63 available)
        
        // Literal pushes (16-bit values)
        PUSHINT      = 0x80,  // Push INT immediate [lsb] [msb]
        PUSHWORD     = 0x81,  // Push WORD immediate [lsb] [msb]
        PUSHCSTRING  = 0x82,  // Push CONSTSTRING pointer [addr_lsb] [addr_msb]
        
        // Function calls (unresolved ? resolved)
        CALL         = 0x83,  // Call function by name [name_offset_lsb] [name_offset_msb] (unresolved)
        CALLF        = 0x84,  // Call function fast [node_lsb] [node_msb] (resolved)
        
        // Global variable operations (unresolved ? resolved)
        PUSHGLOBAL   = 0x85,  // Push global by name [name_offset_lsb] [name_offset_msb] (unresolved)
        POPGLOBAL    = 0x86,  // Pop to global by name [name_offset_lsb] [name_offset_msb] (unresolved)
        PUSHGLOBALF  = 0x87,  // Push global fast [node_lsb] [node_msb] (resolved)
        POPGLOBALF   = 0x88,  // Pop to global fast [node_lsb] [node_msb] (resolved)
        
        // Control flow (long jumps)
        JUMPW        = 0x89,  // Unconditional jump [lsb] [msb]
        JUMPZW       = 0x8A,  // Jump if zero [lsb] [msb]
        JUMPNZW      = 0x8B,  // Jump if non-zero [lsb] [msb]
        
        // Available: 0x8C-0xBF (52 opcodes remaining in this group)
        
        // === RESERVED FOR FUTURE EXTENSIONS (0xC0-0xFF) ===
        // Bits 7-6: 11 (reserved)
        // Reserved for future complex instructions or extensions
        // Potential uses: Variable-length operands, complex string operations,
        //                array access, advanced control flow
        
        INVALID       = 0xFF // probably better if this were 0x00
    }
    
    // System call IDs for SYSCALL opcode
    enum SysCallType
    {
        PRINT_STRING = 0x01,    // Print CONSTSTRING from stack
        PRINT_NEWLINE = 0x02,   // Print newline character
        MILLIS = 0x03,          // Push current milliseconds to stack (future)
    }
    
    // **Resolve-and-Replace Architecture:**
    //
    // **Unresolved Opcodes (First Execution):**
    // - CALL: Function call by name offset in token buffer
    // - PUSHGLOBAL/POPGLOBAL: Variable access by name offset in token buffer
    //
    // **Resolved Opcodes (Subsequent Executions):**
    // - CALLF: Direct function call via node address (2-3x faster)
    // - PUSHGLOBALF/POPGLOBALF: Direct variable access via node address (2-3x faster)
    //
    // **Resolution Process:**
    // 1. First execution hits unresolved opcode (e.g., CALL)
    // 2. Resolve name to node address via symbol table lookup
    // 3. Patch opcode stream: Change CALL ? CALLF, replace name offset with node address
    // 4. Execute resolved opcode immediately
    // 5. Subsequent executions use fast resolved opcode directly
    //
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
    //     ENTER           - Enter function - setup stack frame
    //
    // **Group 1: One Byte Operand (0x40-0x7F)**
    //   [OPCODE][OPERAND]
    //   Examples: 
    //     PUSH0               - Push INT 0 (no operand needed)
    //     PUSH1               - Push INT 1 (no operand needed)
    //     PUSHBIT 0x01        - Push BIT value 1
    //     PUSHBYTE 0xFF       - Push BYTE value 255
    //     PUSHLOCAL 0x02      - Push local at offset +2
    //     PUSHLOCAL 0xFE      - Push argument at offset -2 (signed)
    //     JUMPB 0x0A          - Jump forward 10 bytes
    //     JUMPB 0xF6          - Jump backward 10 bytes (signed)
    //     SYSCALL 0x01        - System call PRINT_STRING
    //
    // **Group 2: Two Byte Operands (0x80-0xBF)**
    //   [OPCODE][LSB][MSB]
    //   Examples:
    //     PUSHINT 0x39 0x30   - Push INT value 12345 (0x3039)
    //     PUSHWORD 0xFF 0xFF  - Push WORD value 65535
    //     PUSHCSTRING 0x10 0x0A - Push CONSTSTRING pointer to token buffer address 0x0A10
    //     
    //     // Unresolved (first execution):
    //     PUSHGLOBAL 0x0C 0x00 - Push global by name at token offset 0x000C
    //     POPGLOBAL 0x0C 0x00  - Pop to global by name at token offset 0x000C
    //     CALL 0x18 0x00       - Call function by name at token offset 0x0018
    //     
    //     // Resolved (subsequent executions):
    //     PUSHGLOBALF 0x10 0x40 - Push global variable at node address 0x4010
    //     POPGLOBALF 0x10 0x40  - Pop to global variable at node address 0x4010
    //     CALLF 0x20 0x40       - Call function at node address 0x4020
    //     
    //     JUMPW 0x00 0x01     - Jump forward 256 bytes
    //     JUMPW 0x00 0xFF     - Jump backward 256 bytes (signed)
    //
    // **Group 3: Reserved (0xC0-0xFF)**
    //   Reserved for future complex instructions or extensions
    //
    // **Variable Access Encoding:**
    // - **Global addresses**: 16-bit node addresses from Variables.Find() or Functions.Find()
    // - **Name offsets**: 16-bit offset from compilation start in token buffer
    // - **Local offsets**: -128 to +127 (signed byte)
    //   - **Negative**: Function arguments (-128 to -1, closest to BP)
    //   - **Positive**: Local variables (1 to +127, further from BP)  
    //   - **Zero**: Reserved/invalid
    // - **Jump deltas**: Signed relative to byte AFTER the instruction
    //
    // **System Call IDs:**
    // - **0x01**: PRINT_STRING - Pop CONSTSTRING from stack and print
    // - **0x02**: PRINT_NEWLINE - Print newline character
    // - **0x03**: MILLIS - Push current milliseconds to stack (future)
    //
    // **Execution Speed Benefits:**
    // - **Single instruction fetch** reveals operand count via bit pattern
    // - **No lookup table** needed for instruction length determination
    // - **Fast opcode dispatching** with simple bit operations and switch statements
    // - **Resolved opcodes** eliminate symbol table traversal (2-3x performance improvement)
    //
    // **Node-Based Access Benefits:**
    // - **Type Safety**: Node contains both value and type information
    // - **PUSHGLOBALF**: Uses Variables.GetValue() ? ZP.TOP + ZP.TOPT with correct type
    // - **POPGLOBALF**: Uses Variables.GetType() + CheckRHSTypeCompatibility() for assignments
    // - **CALLF**: Direct function execution via node address with parameter type checking
    //
    // **String Literal Architecture:**
    // - **PUSHCSTRING**: Pushes 16-bit pointer to null-terminated string
    // - **CONSTSTRING type**: Immutable string pointers that fit in 16-bit value stack
    // - **Smart equality**: Pointer comparison first, then content comparison if different
    // - **Token buffer persistence**: Strings remain valid in REPL and function contexts
}
