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
    const uint BasicInputBuffer        = 0x0B00;                                                        // 128 bytes - raw user input
    
    const uint BasicCompilerWorkspace  = BasicInputBuffer        + Limits.BasicInputSize;               // 32 bytes - compiler.asm
    
    const uint BasicStatementWorkspace = BasicCompilerWorkspace  + Limits.BasicCompilerWorkspaceSize;   // 32 bytes - statement.asm
    
    const uint BasicExecutorWorkspace  = BasicStatementWorkspace + Limits.BasicStatementWorkspaceSize;  // 32 bytes - executor.asm  
    
    const uint BasicProcessBuffer      = BasicExecutorWorkspace  + Limits.BasicExecutorWorkspaceSize;   // 32 bytes - used to convert string literals to uppercase in tokenizer.asm
    
    const uint TokenizerBuffer         = BasicProcessBuffer      + Limits.BasicProcessBufferSize;       // 1024 bytes - tokenized line storage
    
    const uint FunctionOpCodeBuffer    = TokenizerBuffer         + Limits.TokenizerBufferSize;          // 512 bytes - JIT compiled BASIC function opcodes
    
    const uint REPLOpCodeBuffer        = FunctionOpCodeBuffer    + Limits.OpCodeBufferSize;             // 512 bytes - compiled REPL line OpCode storage
    
#ifdef HASEEPROM    
    const uint FileSystemBuffers       = FunctionOpCodeBuffer    + Limits.OpCodeBufferSize;             // 768 bytes - file system buffers (may be shared or smaller in future)
        
    const uint HopperData              = FileSystemBuffers       + Limits.FileSystemBufferSize;         // start of Hopper RAM (program, then heap)
#else
    const uint HopperData              = FunctionOpCodeBuffer    + Limits.OpCodeBufferSize;             // start of Hopper RAM (program, then heap)
#endif
    const uint RamSize                 = 0x8000;  // we assume RAM starts at 0x0000 and that we have at least 32K ..
    //const uint RamSize                = 0xC000;  // we assume RAM starts at 0x0000 and that we have at least 48K ..
    
/*    
    
        +-------------------------------------------------------------------------------------------+
        ¦                            HopperBASIC Memory Map (32K RAM)                               ¦
        ¦-------------------------------------------------------------------------------------------¦
        ¦ Address Range    ¦ Size    ¦ Description                                                  ¦
        ¦------------------+---------+--------------------------------------------------------------¦
        ¦ $0000 - $00FF    ¦ 256 B   ¦ Zero Page (CPU registers, variables, workspace)              ¦
        ¦ $0100 - $01FF    ¦ 256 B   ¦ Hardware Stack (6502 system stack)                           ¦
        ¦ $0200 - $02FF    ¦ 256 B   ¦ Serial Input Buffer                                          ¦
        ¦ $0300 - $03FF    ¦ 256 B   ¦ Call Stack LSB (256 call stack slots, low bytes)             ¦
        ¦ $0400 - $04FF    ¦ 256 B   ¦ Call Stack MSB (256 call stack slots, high bytes)            ¦
        ¦ $0500 - $05FF    ¦ 256 B   ¦ Type Stack (256 type stack slots)                            ¦
        ¦ $0600 - $06FF    ¦ 256 B   ¦ Value Stack LSB (256 value stack slots, low bytes)           ¦
        ¦ $0700 - $07FF    ¦ 256 B   ¦ Value Stack MSB (256 value stack slots, high bytes)          ¦
        ¦ $0800 - $08FF    ¦ 256 B   ¦ Value Stack B2 (byte 3 for LONG values)                      ¦
        ¦ $0900 - $09FF    ¦ 256 B   ¦ Value Stack B3 (byte 4 for LONG values)                      ¦
        ¦ $0A00 - $0AFF    ¦ 256 B   ¦ I2C Input Buffer (for I2C.RequestFrom operations)            ¦
        ¦------------------+---------+--------------------------------------------------------------¦
        ¦                           HopperBASIC Specific Buffers                                    ¦
        ¦------------------+---------+--------------------------------------------------------------¦
        ¦ $0B00 - $0B7F    ¦ 128 B   ¦ Basic Input Buffer (raw user input)                          ¦
        ¦ $0B80 - $0B9F    ¦  32 B   ¦ Basic Compiler Workspace (compiler.asm)                      ¦
        ¦ $0BA0 - $0BBF    ¦  32 B   ¦ Basic Statement Workspace (statement.asm)                    ¦
        ¦ $0BC0 - $0BDF    ¦  32 B   ¦ Basic Executor Workspace (executor.asm)                      ¦
        ¦ $0BE0 - $0BFF    ¦  32 B   ¦ Basic Process Buffer (tokenizer string processing)           ¦
        ¦ $0C00 - $0DFF    ¦ 512 B   ¦ Tokenizer Buffer (tokenized line storage)                    ¦
        ¦ $0E00 - $0FFF    ¦ 512 B   ¦ REPL OpCode Buffer (compiled REPL line opcodes)              ¦
        ¦ $1000 - $11FF    ¦ 512 B   ¦ Function OpCode Buffer (JIT compiled function opcodes)       ¦
        ¦------------------+---------+--------------------------------------------------------------¦
        ¦                          Configuration Dependent                                          ¦
        ¦------------------+---------+--------------------------------------------------------------¦
        ¦ #ifdef HASEEPROM                                                                          ¦
        ¦ $1200 - $14FF    ¦ 768 B   ¦ File System Buffers (may be shared/smaller in future)        ¦
        ¦ $1500 - $7FFF    ¦~26.25K  ¦ Hopper Data (program code + heap)                            ¦
        ¦ #else                                                                                     ¦
        ¦ $1200 - $7FFF    ¦~27K     ¦ Hopper Data (program code + heap)                            ¦
        ¦ #endif                                                                                    ¦
        ¦------------------+---------+--------------------------------------------------------------¦
        ¦                               Memory Totals                                               ¦
        ¦------------------+---------+--------------------------------------------------------------¦
        ¦ System Buffers   ¦ 2816 B  ¦ Fixed system and I/O buffers                                 ¦
        ¦ BASIC Buffers    ¦ 1792 B  ¦ HopperBASIC compilation and execution buffers                ¦
        ¦ FS Buffers*      ¦ 768 B   ¦ File system buffers (EEPROM config only)                     ¦
        ¦ Available RAM    ¦~26-27K  ¦ Program storage and dynamic heap                             ¦
        ¦ Total RAM        ¦ 32K     ¦ Total system memory                                          ¦
        +-------------------------------------------------------------------------------------------+
        
        Notes:
        * File System Buffers only present when HASEEPROM is defined
        * RamSize can be configured for 48K systems (0xC000) by uncommenting alternate constant
        * Hopper Data section contains both program code and heap (managed by Memory unit)
        * All stack operations use dedicated stack areas rather than hardware stack
        * Value stacks support 4-byte LONG values across multiple 256-byte pages

*/
    
    
    

}
