unit Address // MemoryMap.asm
{
    const uint SerialInBuffer       = 0x0200;  // 256-byte buffer 0x0200-0x02FF
    
    const uint CallStackLSB         = 0x0300;  // LSBs of 256 call stack slots
    const uint CallStackMSB         = 0x0400;  // MSBs of 256 call stack slots
 
    const uint TypeStackLSB         = 0x0500;  // 256 type stack slots
    
    const uint ValueStackLSB        = 0x0600;  // LSBs of 256 value stack slots
    const uint ValueStackMSB        = 0x0700;  // MSBs of 256 value stack slots
    
    const uint I2CInBuffer          = 0x0800;  // 256-byte buffer 0x0800-0x08FF for I2C.RequestFrom
    
#if defined(HOPPER_BASIC)
    // HopperBASIC buffers
    const uint BasicInputBuffer        = 0x0900;  // 128 bytes - raw user input
    
    const uint BasicCompilerWorkspace  = 0x0980;  // 32 bytes - compiler.asm
    
    const uint BasicStatementWorkspace = 0x09A0;  // 32 bytes - statemet.asm
    
    const uint BasicExecutorWorkspace  = 0x09C0;  // 32 bytes - executor.asm  
    
    const uint BasicProcessBuffer      = 0x09E0;  // 32 bytes - used to convert string literals to uppercase in tokenizer.asm
    
    const uint BasicTokenizerBuffer   = 0x0A00;  // 512 bytes - tokenized line storage
    
    const uint BasicOpCodeBuffer      = 0x0C00;  // 512 bytes - JIT compiled opcodes

    const uint HopperData             = 0x0E00;  // start of Hopper RAM (program, then heap)
    
#else    
    const uint HopperData             = 0x0900;  // start of Hopper RAM (program, then heap)
#endif
    

#if defined(BENEATER_IO)
    const uint RamSize              = 0x5000;  // the IO ports on the Ben Eater 6502 start at 0x5000 ..
#endif    
#if defined(PD6502)
    const uint RamSize              = 0x9F00;  // stop just below bank switching IO for Dave's board
#endif    

#if !defined(BENEATER_IO) && !defined(PD6502) 
    const uint RamSize              = 0x8000;  // we assume RAM starts at 0x0000 and that we have at least 32K ..
#endif
}
