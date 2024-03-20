unit ZP
{
    const byte BRKL                 = 0x50; // .. 0x5F
    const byte BRKH                 = 0x60; // .. 0x6F
    
    const byte PC                   = 0xB0;
    const byte PCL                  = 0xB0;
    const byte PCH                  = 0xB1;
    
    const byte SP                   = 0xB2;
    
    const byte BP                   = 0xB6;
    
    const byte CSP                  = 0xB8;
    
    const byte COPYNEXTPOP          = 0xBA;
    
    const byte FLAGS                = 0xBB;
    // Bit 5 - breakpoint/s exist
    // Bit 3 - 8 bit SP and BP
    // Bit 2 - checked build
    
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
    
    const byte CODESTART            = 0xCA;
    
    const byte SerialInWritePointer = 0xD3;
    const byte SerialInReadPointer  = 0xD4;
    const byte SerialBreakFlag      = 0xDE;
    
    const byte FREELIST             = 0xE8;
    const byte FREELISTL            = 0xE8;
    const byte FREELISTH            = 0xE9;
    const byte HEAPSTART            = 0xEA;
    const byte HEAPSIZE             = 0xEB;
    
    const byte ACIACONTROL          = 0xEC;
    const byte ACIASTATUS           = 0xEC;
    const byte ACIADATA             = 0xED;
    
}
