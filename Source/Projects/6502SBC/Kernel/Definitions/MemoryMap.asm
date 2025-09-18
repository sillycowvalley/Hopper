unit Address // MemoryMap.asm
{
    const uint HardwareStack        = 0x0100;
    
    const uint SerialInBuffer       = 0x0200;  // 256-byte buffer 0x0200-0x02FF
    
    const uint I2CInBuffer          = 0x0300;  // 256-byte buffer 0x0800-0x08FF for I2C.RequestFrom
    
    const uint WorkSpace            = 0x0400;  // 256 byte general workspace
    
    const uint LineBuffer           = WorkSpace;                         // first 64 bytes (of the 256) for the command line parser
    const uint HexBuffer            = WorkSpace + Limits.LineBufferSize; // Intel HEX data buffer (128 bytes)
    const uint GeneralBuffer        = HexBuffer + Limits.HexBufferSize;  // temporary workspace (64 bytes
    
    // always have File buffers (even if there is no EEPROM) simply to keep the base for user programs constant
    const uint FileSystemBuffers       = WorkSpace               + 256;                          // 768 bytes - file system buffers (may be shared or smaller in future)
    const uint UserMemory              = FileSystemBuffers       + Limits.FileSystemBufferSize;  // start of user RAM (program, then heap)
}
