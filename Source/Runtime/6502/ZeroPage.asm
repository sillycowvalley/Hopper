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
    
    // used by firmware for 'W'orkspace
    // (for example, Serial also uses these, especially on 6502 where PLX, PLY, PHX and PHY are missing)
    const byte W0                   = 0x20;
    const byte W1                   = 0x21;
    
    // used by 'T'ime APIs Millis and Delay
    const byte T0                   = 0x28;
    const byte T1                   = 0x29;
    const byte T2                   = 0x2A;
    const byte T3                   = 0x2B;
    const byte T4                   = 0x2C;
    const byte T5                   = 0x2D;
    const byte T6                   = 0x2E;
    const byte T7                   = 0x2F;
    
    const byte BRKL                 = 0x30; // .. 0x3F
    const byte BRKH                 = 0x40; // .. 0x4F
    
    // used by 'M'emory manager functions
    const byte M0                   = 0x50;
    const byte M1                   = 0x51;
    const byte M2                   = 0x52;
    const byte M3                   = 0x53;
    
    // used by general syscall 'F'unctions  
    const byte F0                   = 0x60;
    const byte F1                   = 0x61;
    const byte F2                   = 0x62;
    const byte F3                   = 0x63;
    const byte F4                   = 0x64;
    const byte F5                   = 0x65;
    const byte F6                   = 0x66;
    const byte F7                   = 0x67;
    const byte F8                   = 0x68;
    const byte F9                   = 0x69;
    const byte F10                  = 0x6A;
    const byte F11                  = 0x6B;
    const byte F12                  = 0x6C;
    const byte F13                  = 0x6D;
    const byte F14                  = 0x6E;
    const byte F15                  = 0x6F;
    
    // used my 'U'Int functions
    const byte U0                   = 0x80;
    const byte U1                   = 0x81;
    const byte U2                   = 0x82;
    const byte U3                   = 0x83;
    const byte U4                   = 0x84;
    const byte U5                   = 0x85;
    const byte U6                   = 0x86;
    const byte U7                   = 0x87;
    
    // used for UInt library
    const byte UWIDE0               = U0;
    const byte UWIDE1               = U1;
    const byte UWIDE2               = U2;
    const byte UWIDE3               = U3;
    
    // used for Int library for sign
    const byte FSIGN                = F0;
    
    // used by long:
    const byte LTOP0 = F5;
    const byte LTOP1 = F6;
    const byte LTOP2 = F7;
    const byte LTOP3 = F8;
    
    const byte LNEXT0 = F9;
    const byte LNEXT1 = F10;
    const byte LNEXT2 = F11;
    const byte LNEXT3 = F12;
    
    // used by syscallLongDiv, syscallLongMod, syscallLongMul:
    const byte LRESULT0 = U0;
    const byte LRESULT1 = U1;
    const byte LRESULT2 = U2;
    const byte LRESULT3 = U3;
    const byte LRESULT4 = U4;
    const byte LRESULT5 = U5;
    const byte LRESULT6 = U6;
    const byte LRESULT7 = U7;
    
}
