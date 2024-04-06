unit SysCall
{
    enum SysCalls
    {
        DiagnosticsDie  = 0x7C,
        
        SerialConnect   = 0xA2,
        SerialWriteChar = 0xA7,
        
        ByteToHex       = 0xBE,
        IntGetByte      = 0xE1,
        IntFromBytes    = 0xE4,
    }
    
    missing()
    {
        TXA BRK // SysCall not Implemented!
    }
    die()
    {
        PopA(); BRK // user error from Hopper in A
    }
    serialConnect()
    {
        // NOP (we're already connected to serial)
    }
    serialWriteChar()
    {
        PopA();
        Serial.WriteChar();
    }
    
    byteToHex()
    {
        // convert nibble to hex char
        PopA();
        CMP # 0x0A
        if (C)
        {
            // +'A' - 10   = 55
            // + 48 below  = 7
            // + 1 (carry) = 6
            ADC # 6
        }
        // +'0'
        ADC # '0' // 48
        
        STA ZP.TOPL
        LDA # 0
        STA ZP.TOPH
        LDA # Types.Char
        STA ZP.TOPT
        Stacks.PushTop();
    }
    intGetByte()
    {
        PopTop();        // Index
        PopNext();       // Int
        LDA ZP.TOPL
        if (NZ)
        {
            LDA ZP.NEXTH // MSB
            STA ZP.NEXTL
        }
        LDA # 0
        STA ZP.NEXTH
        LDA # Types.Byte
        STA ZP.NEXTT
        PushNext();
    }
    intFromBytes()
    {
        PopTop();   // MSB
        PopNext();  // LSB
        LDA ZP.TOPL
        STA ZP.NEXTH
        LDA # Types.Int
        STA ZP.NEXTT
        PushNext();
    }
    
    sysCall()
    {
        // iOverload in ACC
        // iSysCall  in X
        
#ifdef CHECKED
        TXA TAY // so we can see the original A at BRK
#endif                
        switch (X)
        {
            case SysCalls.DiagnosticsDie:
            {
                die();
            }
            
            case SysCalls.SerialConnect:
            {
                serialConnect();
            }
            case SysCalls.SerialWriteChar:
            {
                serialWriteChar();
            }
            
            case SysCalls.ByteToHex:
            {
                byteToHex();
            }
            case SysCalls.IntGetByte:
            {
                intGetByte();
            }
            case SysCalls.IntFromBytes:
            {
                intFromBytes();
            }
            default:
            {
                missing();
            }
        }
    }
    SysCall()
    {
        ConsumeOperandB(); // iSysCall  -> A (uses ACC)
        PHA
        PopAcc();          // iOverload -> ACC
        
        // load iSyscCall into X (because JMP [nnnn,X] is then possible)
#ifdef CPU_65C02S
        PLX
#else        
        PLA
        TAX
#endif
        sysCall();
    }
}
