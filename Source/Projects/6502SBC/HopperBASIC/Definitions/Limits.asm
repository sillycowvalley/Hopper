unit Limits
{
    // Buffer size constants for HopperBASIC
    const byte BasicInputSize            = 128;  // Raw input line buffer
    const uint TokenizerBufferSize       = 1024; // Tokenized line storage (16-bit size)
    const uint OpCodeBufferSize          = 512;  // JITted OpCode storage (16-bit size)
    const uint FileSystemBufferSize      = 768;  // File system buffers
    
    // Working buffer sizes (all in page 0x09 after 128-byte input buffer)
    const byte BasicCompilerWorkspaceSize = 32;  // 0x0980-0x099F - compiler
    const byte BasicStatementWorkspaceSize = 32; // 0x09A0-0x09BF - statement
    const byte BasicExecutorWorkspaceSize = 32;  // 0x09C0-0x09DF - executor
    const byte BasicProcessBufferSize = 32;      // 0x09E0-0x09FF - tokenizer (uppercasing)
}
