unit SysCalls
{
     
    GetArgCount()
    {
        PHY
        
        LDY #0               // LineBuffer index
        LDX #0               // Argument count
        STZ ZP.ACCH          // Track if we found any non-null in this position
        
        loop
        {
            LDA Address.LineBuffer, Y
            if (NZ)
            {
                // Found start of argument
                LDA ZP.ACCH
                if (Z)
                {
                    INX              // Count this argument
                    INC ZP.ACCH      // Mark we're in an argument
                }
            }
            else
            {
                STZ ZP.ACCH          // Reset - we're in nulls now
            }
            
            INY
            CPY #64          // End of buffer
            if (Z) { break; }
        }
        
        TXA                  // Return count in A
        SEC                  // Success
        
        PLY
    }
    
    GetArg()  
    {
        STA ZP.ACCL          // Save target argument index
        PHY
        
        LDY #0               // LineBuffer index  
        LDX #0               // Current argument counter
        
        loop
        {
            // Skip any zeros (gaps between arguments)
            loop
            {
                CPY #64
                if (Z) 
                { 
                    CLC      // End of buffer, argument not found
                    break;
                }
                LDA Address.LineBuffer, Y
                if (NZ) { SEC break; }  // Found non-zero, start of argument
                INY
            }
            if (NC) { break; }
            
            // Now Y points to start of an argument
            // Check if this is the argument we want
            CPX ZP.ACCL
            if (Z)
            {
                // Found our target argument
                TYA
                CLC
                ADC #(Address.LineBuffer % 256)
                STA ZP.STRL
                LDA #(Address.LineBuffer / 256)
                ADC #0
                STA ZP.STRH
                SEC              // Success
                break;
            }
            
            // Skip to end of current argument
            loop
            {
                CPY #64
                if (Z) 
                { 
                    CLC      // End of buffer, argument not found
                    break;
                }
                LDA Address.LineBuffer, Y
                if (Z) { break; }  // Found end of argument
                INY
            }
            
            // Move to next argument
            INX
        }// loop
        
        PLY
    }
     
    isBreak()
    {
        CLC
#ifdef UNIVERSAL        
        PHA   
        LDA ZP.FLAGS
        AND #0b00000001
        if (NZ)   // Bit 0 set? (break detected)
        {
            LDA #0b11111110
            AND ZP.FLAGS
            STA ZP.FLAGS // clear it so we don't get a duplicate <Ctrl><C>
            SEC
        }
        PLA
#else
        if (BBS0, ZP.FLAGS)   // Bit 0 set? (break detected)
        {
            RMB0 ZP.FLAGS // clear it so we don't get a duplicate <Ctrl><C>
            SEC
        }
#endif
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
            case SysCall.ArgCount:
            {
                GetArgCount();
            }
            case SysCall.ArgGet:
            {
                GetArg();
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
            case SysCall.FRead:
            {
#if defined(CFILES)
                File.FRead();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FPutC:
            {
#if defined(CFILES)
                File.FPutC();
#else
                Error.InvalidSystemCall();
#endif
            }
            case SysCall.FWrite:
            {
#if defined(CFILES)
                File.FWrite();
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
