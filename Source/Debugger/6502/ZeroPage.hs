unit ZP
{
    const byte ZPC                   = 0x00;
    const byte ZPCL                  = 0x00;
    const byte ZPCH                  = 0x01;
    
    const byte ZFLAGS                = 0x02; // see HopperFlags below
    // Bit 5 - breakpoint/s exist
    // Bit 3 - 8 bit SP and BP
    // Bit 2 - checked build
    // Bit 1 - running at Warp (no checks for <ctrl><C>)
    // Bit 0 - a program has been loaded
    
    const byte ZSP                   = 0x03;
    const byte ZBP                   = 0x04;
    const byte ZCSP                  = 0x05;

    const byte ZFREELIST             = 0x06;
    const byte ZFREELISTL            = 0x06;
    const byte ZFREELISTH            = 0x07;
    const byte ZHEAPSTART            = 0x08;
    const byte ZHEAPSIZE             = 0x09;
    
    const byte ZCODESTART            = 0x0D;
    const byte ZCODESTARTL           = 0x0D;
    const byte ZCODESTARTH           = 0x0E;
    
    const byte ZCNP                  = 0x0F;
    
    const byte ZACC                  = 0x10;
    const byte ZACCL                 = 0x10;
    const byte ZACCH                 = 0x11;
    
    const byte ZTOP                  = 0x12;
    const byte ZTOPL                 = 0x12;
    const byte ZTOPH                 = 0x13;
    
    const byte ZNEXT                 = 0x14;
    const byte ZNEXTL                = 0x14;
    const byte ZNEXTH                = 0x15;
    
    const byte ZIDX                  = 0x16;
    const byte ZIDXL                 = 0x16;
    const byte ZIDXH                 = 0x17;
    
    const byte ZIDY                  = 0x18;
    const byte ZIDYL                 = 0x18;
    const byte ZIDYH                 = 0x19;
    
    const byte ZACCT                 = 0x1A;
    const byte ZTOPT                 = 0x1B;
    const byte ZNEXTT                = 0x1C;  
    
    const byte ZPLUGNPLAY            = 0x27;
    
    const byte ZT0                   = 0x28;
    const byte ZT1                   = 0x29;
    const byte ZT2                   = 0x2A;
    const byte ZT3                   = 0x2B;
          
    const byte ZBRKL                 = 0x30; // .. 0x3F
    const byte ZBRKH                 = 0x40; // .. 0x4F  
    
    // Zero Page FLAGS:
    flags HopperFlags
    {
        ProgramLoaded  = 0x01, // a program has been loaded
        WarpSpeed      = 0x02, // on 6502 running without checks for <Ctrl><C> or breakpoints
        CheckedBuild   = 0x04,
        SP8Bit         = 0x08, // always true nowadays
        
        BreakpointsSet = 0x20, // breakpoints exist
        ProgramExited  = 0x40, // flag to make checking for program completion (or Die) fast
        MCUPlatform    = 0x80,
    }
    
}
