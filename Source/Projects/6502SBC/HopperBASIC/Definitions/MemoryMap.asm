unit Address // MemoryMap.asm
{
    const uint HardwareStack        = 0x0100;
    
    const uint SerialInBuffer       = 0x0200;  // 256-byte buffer 0x0200-0x02FF
    
    const uint CallStackLSB         = 0x0300;  // LSBs of 256 call stack slots
    const uint CallStackMSB         = 0x0400;  // MSBs of 256 call stack slots
 
    const uint TypeStackLSB         = 0x0500;  // 256 type stack slots
    
    const uint ValueStackLSB        = 0x0600;  // LSBs of 256 value stack slots
    const uint ValueStackMSB        = 0x0700;  // MSBs of 256 value stack slots
    
    const uint I2CInBuffer          = 0x0800;  // 256-byte buffer 0x0800-0x08FF for I2C.RequestFrom
    
    // HopperBASIC buffers
    const uint BasicInputBuffer        = 0x0900;  // 128 bytes - raw user input
    
    const uint BasicCompilerWorkspace  = 0x0980;  // 32 bytes - compiler.asm
    
    const uint BasicStatementWorkspace = 0x09A0;  // 32 bytes - statemet.asm
    
    const uint BasicExecutorWorkspace  = 0x09C0;  // 32 bytes - executor.asm  
    
    const uint BasicProcessBuffer      = 0x09E0;  // 32 bytes - used to convert string literals to uppercase in tokenizer.asm
    
    const uint BASICTokenizerBuffer   = 0x0A00;  // 512 bytes - tokenized BASIC function storage
    
    const uint BASICOpCodeBuffer      = 0x0C00;  // 512 bytes - JIT compiled BASIC function opcodes
    
    const uint REPLTokenizerBuffer    = 0x0E00;  // 512 bytes - tokenized REPL line storage
    
    const uint REPLOpCodeBuffer       = 0x0F00;  // 512 bytes - compiled REPL line OpCode storage

    const uint HopperData             = 0x1000;  // start of Hopper RAM (program, then heap)
    
    const uint RamSize                = 0x8000;  // we assume RAM starts at 0x0000 and that we have at least 32K ..
    //const uint RamSize                = 0xC000;  // we assume RAM starts at 0x0000 and that we have at least 48K ..

}
