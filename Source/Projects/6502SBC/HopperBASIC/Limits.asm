unit Limits
{
    // Buffer size constants for HopperBASIC
    const byte BasicInputLength = 128;           // Raw input line buffer
    const uint BasicTokenizerBufferLength = 512; // Tokenized line storage (16-bit size)
    
    // Working buffer sizes (all in page 0x09 after 128-byte input buffer)
    const byte BasicProcessBuffer1Length = 64;   // 0x0980-0x09BF
    const byte BasicProcessBuffer2Length = 32;   // 0x09C0-0x09DF  
    const byte BasicProcessBuffer3Length = 32;   // 0x09E0-0x09FF
}