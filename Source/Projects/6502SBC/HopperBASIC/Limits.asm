unit Limits
{
    // Buffer size constants for HopperBASIC
    const byte BasicInputLength = 128;           // Raw input line buffer
    const uint BasicTokenizerBufferLength = 512; // Tokenized line storage (16-bit size)
    const uint BasicOpCodeBufferLength = 512;    // JITted OpCode storage (16-bit size)
    
    // Working buffer sizes (all in page 0x09 after 128-byte input buffer)
    const byte BasicCompilerWorkspaceLength = 32;  // 0x0980-0x099F - compiler
    const byte BasicStatementWorkspaceLength = 32; // 0x09A0-0x09BF - statement
    const byte BasicExecutorWorkspaceLength = 32;  // 0x09C0-0x09DF - executor
    const byte BasicProcessBufferLength = 32;      // 0x09E0-0x09FF - tokenizer (uppercasing)
}
