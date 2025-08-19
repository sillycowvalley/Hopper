unit Address // MemoryMap.asm
{
    const uint HardwareStack        = 0x0100;
    
    const uint SerialInBuffer       = 0x0200;  // 256-byte buffer 0x0200-0x02FF
    
    const uint CallStackLSB         = 0x0300;  // LSBs of 256 call stack slots
    const uint CallStackMSB         = 0x0400;  // MSBs of 256 call stack slots
 
    const uint TypeStackLSB         = 0x0500;  // 256 type stack slots
    const uint TypeStack      = TypeStackLSB;
    
    const uint ValueStackLSB        = 0x0600;  // LSBs of 256 value stack slots
    const uint ValueStackB0  = ValueStackLSB;
    const uint ValueStackMSB        = 0x0700;  // MSBs of 256 value stack slots
    const uint ValueStackB1  = ValueStackMSB;
    const uint ValueStackB2         = 0x0800;  // byte 3 for LONG 
    const uint ValueStackB3         = 0x0900;  // byte 4 for LONG 
    
    const uint I2CInBuffer          = 0x0A00;  // 256-byte buffer 0x0800-0x08FF for I2C.RequestFrom
    
    // HopperBASIC buffers
    const uint BasicInputBuffer        = 0x0B00;                                                       // 128 bytes - raw user input
    
    const uint BasicCompilerWorkspace  = BasicInputBuffer + Limits.BasicInputSize;                     // 32 bytes - compiler.asm
    
    const uint BasicStatementWorkspace = BasicCompilerWorkspace + Limits.BasicCompilerWorkspaceSize;   // 32 bytes - statement.asm
    
    const uint BasicExecutorWorkspace  = BasicStatementWorkspace + Limits.BasicStatementWorkspaceSize; // 32 bytes - executor.asm  
    
    const uint BasicProcessBuffer      = BasicExecutorWorkspace + Limits.BasicExecutorWorkspaceSize;   // 32 bytes - used to convert string literals to uppercase in tokenizer.asm
    
    const uint TokenizerBuffer         = BasicProcessBuffer    + Limits.BasicProcessBufferSize;        // 512 bytes - tokenized line storage
    
    const uint REPLOpCodeBuffer        = TokenizerBuffer  + Limits.TokenizerBufferSize;                // 512 bytes - compiled REPL line OpCode storage

    const uint FunctionOpCodeBuffer    = REPLOpCodeBuffer + Limits.OpCodeBufferSize;                   // 512 bytes - JIT compiled BASIC function opcodes
    
#ifdef HASEEPROM    
    const uint FileSystemBuffers       = FunctionOpCodeBuffer     + Limits.OpCodeBufferSize;          // 768 bytes - file system buffers (may be shared or smaller in future)
        
    const uint HopperData              = FileSystemBuffers        + Limits.FileSystemBufferSize;      // start of Hopper RAM (program, then heap)
#else
    const uint HopperData              = FunctionOpCodeBuffer     + Limits.OpCodeBufferSize;          // start of Hopper RAM (program, then heap)
#endif
    const uint RamSize                 = 0x8000;  // we assume RAM starts at 0x0000 and that we have at least 32K ..
    //const uint RamSize                = 0xC000;  // we assume RAM starts at 0x0000 and that we have at least 48K ..

}
