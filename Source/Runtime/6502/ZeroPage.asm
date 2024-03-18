unit ZP
{
    const byte FLAGS                = 0xBB;
    
    const byte ACC                  = 0xC0;
    const byte ACCL                 = 0xC0;
    const byte ACCH                 = 0xC1;
    
    const byte IDX                  = 0xC6;
    const byte IDXL                 = 0xC6;
    const byte IDXH                 = 0xC7;
    
    const byte IDY                  = 0xC8;
    const byte IDYL                 = 0xC8;
    const byte IDYH                 = 0xC9;
    
    const byte SerialInWritePointer = 0xD3;
    const byte SerialInReadPointer  = 0xD4;
    const byte SerialBreakFlag      = 0xDE;
    
    const byte ACIACONTROL          = 0xEC;
    const byte ACIASTATUS           = 0xEC;
    const byte ACIADATA             = 0xED;
    
}
