unit ZP
{
    const byte BRKL                 = 0x50; // .. 0x5F
    const byte BRKH                 = 0x60; // .. 0x6F
    
    const byte PC                   = 0xB0;
    const byte PCL                  = 0xB0;
    const byte PCH                  = 0xB1;
    
    const byte CODESTART            = 0xB2;
    const byte CODESTARTL           = 0xB2;
    const byte CODESTARTH           = 0xB3;
    
    
    const byte SP                   = 0xB4;
    const byte BP                   = 0xB5;
    const byte CSP                  = 0xB6;
    const byte CNP                  = 0xB7;
    
    const byte FLAGS                = 0xBB;
    // Bit 5 - breakpoint/s exist
    // Bit 3 - 8 bit SP and BP
    // Bit 2 - checked build
    // Bit 1 - running at Warp (no checks for <ctrl><C>)
    // Bit 0 - a program has been loaded
    
    const byte FREELIST             = 0xBC;
    const byte FREELISTL            = 0xBC;
    const byte FREELISTH            = 0xBD;
    const byte HEAPSTART            = 0xBE;
    const byte HEAPSIZE             = 0xBF;
    
    
    const byte ACC                  = 0xC0;
    const byte ACCL                 = 0xC0;
    const byte ACCH                 = 0xC1;
    
    const byte TOP                  = 0xC2;
    const byte TOPL                 = 0xC2;
    const byte TOPH                 = 0xC3;
    
    const byte NEXT                 = 0xC4;
    const byte NEXTL                = 0xC4;
    const byte NEXTH                = 0xC5;
    
    const byte IDX                  = 0xC6;
    const byte IDXL                 = 0xC6;
    const byte IDXH                 = 0xC7;
    
    const byte IDY                  = 0xC8;
    const byte IDYL                 = 0xC8;
    const byte IDYH                 = 0xC9;
    
    const byte ACCT                 = 0xCA;
    const byte TOPT                 = 0xCB;
    const byte NEXTT                = 0xCC;
    
    
    const byte SerialInWritePointer = 0xD3;
    const byte SerialInReadPointer  = 0xD4;
    const byte SerialBreakFlag      = 0xDE;
    
    const byte ACIACONTROL          = 0xEC;
    const byte ACIASTATUS           = 0xEC;
    const byte ACIADATA             = 0xED;
    
}
