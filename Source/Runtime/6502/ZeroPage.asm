unit ZP
{
    const byte PC                   = 0x00;
    const byte PCL                  = 0x00;
    const byte PCH                  = 0x01;
    
    const byte FLAGS                = 0x02;
    // Bit 5 - breakpoint/s exist
    // Bit 3 - 8 bit SP and BP
    // Bit 2 - checked build
    // Bit 1 - running at Warp (no checks for <ctrl><C>)
    // Bit 0 - a program has been loaded
    
    const byte SP                   = 0x03;
    const byte BP                   = 0x04;
    const byte CSP                  = 0x05;
    
    const byte FREELIST             = 0x06;
    const byte FREELISTL            = 0x06;
    const byte FREELISTH            = 0x07;
    const byte HEAPSTART            = 0x08;
    const byte HEAPSIZE             = 0x09;
    
    const byte SerialInWritePointer = 0x0A;
    const byte SerialInReadPointer  = 0x0B;
    const byte SerialBreakFlag      = 0x0C;
    
    const byte CODESTART            = 0x0D;
    const byte CODESTARTL           = 0x0D;
    const byte CODESTARTH           = 0x0E;
    
    const byte CNP                  = 0x0F;
    
    const byte ACC                  = 0x10;
    const byte ACCL                 = 0x10;
    const byte ACCH                 = 0x11;
    
    const byte TOP                  = 0x12;
    const byte TOPL                 = 0x12;
    const byte TOPH                 = 0x13;
    
    const byte NEXT                 = 0x14;
    const byte NEXTL                = 0x14;
    const byte NEXTH                = 0x15;
    
    const byte IDX                  = 0x16;
    const byte IDXL                 = 0x16;
    const byte IDXH                 = 0x17;
    
    const byte IDY                  = 0x18;
    const byte IDYL                 = 0x18;
    const byte IDYH                 = 0x19;
    
    const byte ACCT                 = 0x1A;
    const byte TOPT                 = 0x1B;
    const byte NEXTT                = 0x1C;
    
    const byte ACIACONTROL          = 0x1E;
    const byte ACIASTATUS           = 0x1E;
    const byte ACIADATA             = 0x1F;
    
    const byte BRKL                 = 0x20; // .. 0x2F
    const byte BRKH                 = 0x30; // .. 0x3F
    
    const byte F0                   = 0x60;
    
    const byte U0                   = 0x70;
    const byte U1                   = 0x71;
    const byte U2                   = 0x72;
    const byte U3                   = 0x73;
    
    // used for UInt library
    const byte UWIDE0               = U0;
    const byte UWIDE1               = U1;
    const byte UWIDE2               = U2;
    const byte UWIDE3               = U3;
    
    // unsed for Int library for sign
    const byte FSIGN                = F0;
    
}
