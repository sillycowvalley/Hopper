unit Address
{
    const uint SerialInBuffer       = 0x0200;  // 256-byte buffer 0x0200-0x02FF
    
    const uint CallStackLSB         = 0x0300;  // LSBs of 256 call stack slots
    const uint CallStackMSB         = 0x0400;  // MSBs of 256 call stack slots
 
    const uint TypeStackLSB         = 0x0500;  // 256 type stack slots
    
    const uint ValueStackLSB        = 0x0600;  // LSBs of 256 value stack slots
    const uint ValueStackMSB        = 0x0700;  // MSBs of 256 value stack slots
    
    const uint HopperData           = 0x0800;  // start of Hopper RAM (program, then heap)
       
    const uint RamSize              = 0xC000;  // we assume RAM always starts at 0x0000 and that we have 48K, for now
    //const uint RamSize            = 0x1800; // 6K on the Mega2560
    
}
