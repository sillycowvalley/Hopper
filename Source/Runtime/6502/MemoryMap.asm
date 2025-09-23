unit Address
{
    const uint SerialInBuffer       = 0x0200;  // 256-byte buffer 0x0200-0x02FF
    
    const uint CallStackLSB         = 0x0300;  // LSBs of 256 call stack slots
    const uint CallStackMSB         = 0x0400;  // MSBs of 256 call stack slots
 
    const uint TypeStackLSB         = 0x0500;  // 256 type stack slots
    
    const uint ValueStackLSB        = 0x0600;  // LSBs of 256 value stack slots
    const uint ValueStackMSB        = 0x0700;  // MSBs of 256 value stack slots
    
    const uint I2CInBuffer          = 0x0800;  // 256-byte buffer 0x0800-0x08FF for I2C.RequestFrom
    
    const uint HopperData           = 0x0900;  // start of Hopper RAM (program, then heap)

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
