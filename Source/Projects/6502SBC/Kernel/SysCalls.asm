unit SysCalls
{

        // System Call IDs - shared enum for client compilation
    enum SysCall
    {
        // Memory Management (0x10-0x1F)
        MemAllocate     = 0x10,
        MemFree         = 0x11, 
        MemAvailable    = 0x12,
        MemMaximum      = 0x13,
        // Reserved       0x13-0x1F
        
        // File Operations (0x20-0x3F)
        FileExists      = 0x20,
        FileDelete      = 0x21,
        FileDir         = 0x22,
        FileStartSave   = 0x23,
        FileAppendStream = 0x24,
        FileEndSave     = 0x25,
        FileStartLoad   = 0x26,
        FileNextStream  = 0x27,
        FileFormat      = 0x28,
        // Reserved       0x28-0x3F
        
        // Serial I/O (0x40-0x4F)
        SerialWriteChar   = 0x40,
        SerialWaitForChar = 0x41,
        SerialIsAvailable = 0x42,
        // Reserved       0x44-0x4F
        
        // Print/Console (0x50-0x5F)
        PrintString     = 0x50,
        PrintChar       = 0x51,
        PrintHex        = 0x52,
        PrintNewLine    = 0x53,
        PrintSpace      = 0x54,
        PrintSpaces     = 0x55,
        // Reserved       0x57-0x5F
        
        // Timer Services (0x60-0x6F)
        TimeDelay       = 0x60,
        TimeMillis      = 0x61,
        TimeSeconds     = 0x62,
        // Reserved       0x63-0x6F
        
        // Long Math (0x70-0x8F)
        LongAdd         = 0x70,
        LongSub         = 0x71,
        LongMul         = 0x72,
        LongDiv         = 0x73,
        LongMod         = 0x74,
        LongPrint       = 0x75,
        LongLT          = 0x76,
        LongGT          = 0x77,
        LongEQ          = 0x78,
        LongNE          = 0x79,
        LongLE          = 0x7A,
        LongGE          = 0x7B,
        // Reserved       0x7D-0x8F
        
        // Float Math (0x90-0xAF)  
        FloatAdd        = 0x90,
        FloatSub        = 0x91,
        FloatMul        = 0x92,
        FloatDiv        = 0x93,
        FloatToLong     = 0x94,
        FloatLT         = 0x95,
        FloatEQ         = 0x96,
        // Reserved       0x97-0xAF
        
        // TODO:
        // - I2C
        // - GPIO
    }
    
    // System Call Dispatcher
    // Input: X = system call ID
    //        A, Y = parameters (call-specific)
    //        ZP.STR* = string parameters (if needed)
    //        ZP.ACC* = size parameters for memory operations
    // Output: A = error code (SysError enum)
    //         C = set on success, clear on error
    //         Y, other registers = return values (call-specific)
    // Preserves: Stack level (JSR/RTS balanced)
    SystemCallDispatcher()
    {
        switch (X)
        {
            // Memory Management
            case SysCall.MemAllocate:
            {
                Memory.Allocate();
            }
            case SysCall.MemFree:
            {
                Memory.Free();
            }
            case SysCall.MemAvailable:
            {
                Memory.Available();
            }
            case SysCall.MemMaximum:
            {
                Memory.Maximum();
            }
            
            // File Operations
            case SysCall.FileExists:
            {
#if defined(HASEEPROM)
                File.Exists();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FileDelete:
            {
#if defined(HASEEPROM)
                File.Delete();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FileDir:
            {
#if defined(HASEEPROM)
                File.Dir();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FileStartSave:
            {
#if defined(HASEEPROM)
                File.StartSave();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FileAppendStream:
            {
#if defined(HASEEPROM)
                File.AppendStream();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FileEndSave:
            {
#if defined(HASEEPROM)
                File.EndSave(); 
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FileStartLoad:
            {
#if defined(HASEEPROM)
                File.StartLoad();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FileNextStream:
            {
#if defined(HASEEPROM)
                File.NextStream();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FileFormat:
            {
#if defined(HASEEPROM)
                File.Format();
#else
                Error.InvalidSystemCall();
#endif
            }
            
            // Serial I/O
            case SysCall.SerialWriteChar:
            {
                Serial.WriteChar();
            }
            case SysCall.SerialWaitForChar:
            {
                Serial.WaitForChar();
            }
            case SysCall.SerialIsAvailable:
            {
                Serial.IsAvailable();
            }
            
            // Print/Console
            case SysCall.PrintString:
            {
                Print.String();
            }
            case SysCall.PrintChar:
            {
                Print.Char();
            }
            case SysCall.PrintHex:
            {
                Print.Hex();
            }
            case SysCall.PrintNewLine:
            {
                Print.NewLine();
            }
            case SysCall.PrintSpace:
            {
                Print.Space();
            }
            case SysCall.PrintSpaces:
            {
                Print.Spaces();
            }
            
            // Timer Services  
            case SysCall.TimeDelay:
            {
                Time.Delay();
            }
            case SysCall.TimeMillis:
            {
                Time.Millis();
            }
            case SysCall.TimeSeconds:
            {
                Time.Seconds();
            }
            
            // Long Math
            case SysCall.LongAdd:
            {
                Long.Add();
            }
            case SysCall.LongSub:
            {
                Long.Sub();
            }
            case SysCall.LongMul:
            {
                Long.Mul();
            }
            case SysCall.LongDiv:
            {
                Long.Div();
            }
            case SysCall.LongMod:
            {
                Long.Mod();
            }
            case SysCall.LongPrint:
            {
                Long.Print();
            }
            case SysCall.LongLT:
            {
                Long.LT();
            }
            case SysCall.LongGT:
            {
                Long.GT();
            }
            case SysCall.LongEQ:
            {
                Long.EQ();
            }
            case SysCall.LongNE:
            {
                Long.NE();
            }
            case SysCall.LongLE:
            {
                Long.LE();
            }
            case SysCall.LongGE:
            {
                Long.GE();
            }
            
            // Float Math
            case SysCall.FloatAdd:
            {
#if defined(HASFLOAT)
                Float.Add();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FloatSub:
            {
#if defined(HASFLOAT)
                Float.Sub();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FloatMul:
            {
#if defined(HASFLOAT)
                Float.Mul();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FloatDiv:
            {
#if defined(HASFLOAT)
                Float.Div();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FloatToLong:
            {
#if defined(HASFLOAT)
                Float.ToLong();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FloatLT:
            {
#if defined(HASFLOAT)
                Float.LT();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FloatEQ:
            {
#if defined(HASFLOAT)
                Float.EQ();
#else
                Error.InvalidSystemCall();
#endif
            }
            
            default:
            {
                Error.InvalidSystemCall();
            }
        }
    }
    
    // Initialize system call dispatcher
    // Sets up the dispatch vector in zero page
    // NOTE: Check your zero page definitions - both DISPATCHL and DISPATCHH are set to 0x2C
    //       DISPATCHH should probably be 0x2D
    Initialize()
    {
        // TODO
        //LDA #(SystemCallDispatcher % 256)
        //STA ZP.DISPATCHL
        //LDA #(SystemCallDispatcher / 256)  
        //STA ZP.DISPATCHH
    }
}
