unit Address // MemoryMap.asm
{
    const uint HardwareStack        = 0x0100;
    
    const uint SerialInBuffer       = 0x0200;  // 256-byte buffer 0x0200-0x02FF
    
    const uint I2CInBuffer          = 0x0300;  // 256-byte buffer 0x0800-0x08FF for I2C.RequestFrom
    
#ifdef HASEEPROM    
    const uint FileSystemBuffers       = I2CInBuffer             + 256;                          // 768 bytes - file system buffers (may be shared or smaller in future)
        
    const uint HeapStart               = FileSystemBuffers       + Limits.FileSystemBufferSize;  // start of user RAM (program, then heap)
#else
    const uint HeapStart               = I2CInBuffer             + 256;                          // start of user RAM (program, then heap)
#endif
    
}
