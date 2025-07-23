unit Opcodes
{
    // Bytecode opcodes for the Hopper BASIC VM
    // Used by both compiler and executor
    
    enum OpCode
    {
        OpNop        = 0x00,  // No operation
        OpPushInt    = 0x01,  // Push constant: + 2 bytes value + 1 byte type
        OpPrintInt   = 0x02,  // Print TOS as integer
        OpPrintStr   = 0x03,  // Print TOS as string literal
        OpPrintNL    = 0x04,  // Print newline
        OpReturn     = 0x05,  // Return from function
        OpLoadVar    = 0x06,  // Load variable: + length + name
        OpStoreVar   = 0x07,  // Store to variable: + length + name
        OpHalt       = 0xFF,  // End of REPL function
    }
}
