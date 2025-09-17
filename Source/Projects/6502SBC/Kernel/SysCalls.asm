unit SysCalls
{
     
    isBreak()
    {
        CLC
        if (BBS0, ZP.FLAGS)   // Bit 0 set? (break detected)
        {
            RMB0 ZP.FLAGS // clear it so we don't get a duplicate <Ctrl><C>
            SEC
        }
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
            case SysCall.IsBreak:
            {
                isBreak();
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
            case SysCall.PinMode:
            {
                GPIO.PinMode();
            }
            case SysCall.PinRead:
            {
                GPIO.PinRead();
            }
            case SysCall.PinWrite:
            {
                GPIO.PinWrite();
            }
            case SysCall.FOpen:
            {
#if defined(CFILES)
                File.FOpen();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FGetC:
            {
#if defined(CFILES)
                File.FGetC();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FClose:
            {
#if defined(CFILES)
                File.FClose();
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
}
