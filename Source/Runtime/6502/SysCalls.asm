unit SysCall
{
    uses "6502/Array"
    uses "6502/String"
    uses "6502/Long"
    uses "6502/Time"
    
    enum SysCalls
    {
        StringNewFromConstant = 0x00,
        StringBuild           = 0x01,
        StringNew             = 0x02,
        StringBuildFront      = 0x03,
        ArrayNewFromConstant  = 0x04,
        TimeSeconds           = 0x05,
        StringLengthGet       = 0x06,
        TimeDelay             = 0x07,
        DiagnosticsDie        = 0x08,
        SerialConnect         = 0x09,
        StringGetChar         = 0x0A,
        
        ArrayNew              = 0x0B,
        ArrayCountGet         = 0x0C,
        ArrayGetItem          = 0x0D,
        ArraySetItem          = 0x0E,
        
        SerialReadChar        = 0x0F,
        SerialWriteChar       = 0x10,
        SerialIsAvailable     = 0x11,
        
        MemoryReadByte        = 0x12,
        MemoryWriteByte       = 0x13,
        MemoryAvailable       = 0x14,
        MemoryMaximum         = 0x15,
        MemoryAllocate        = 0x16,
        MemoryFree            = 0x17,
        
        ByteToHex             = 0x18,
        IntGetByte            = 0x19,
        IntFromBytes          = 0x1A,
        
    }
    
    missing()
    {
        TXA BRK // SysCall not Implemented!
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
        PopTopNext();
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
        PopTopNext();
        LDA ZP.TOPL
        STA ZP.NEXTH
        LDA # Types.Int
        STA ZP.NEXTT
        PushNext();
    }
    
    SysCallShared()
    {
        // iOverload in ACCL
        // iSysCall  in X
        
        switch (X)
        {
            case SysCalls.DiagnosticsDie:
            {
                Diagnostics.Die();
            }
            
            case SysCalls.SerialConnect:
            {
                serialConnect();
            }
            case SysCalls.SerialWriteChar:
            {
                serialWriteChar();
            }
            
            case SysCalls.TimeDelay:
            {
                Time.Delay();
            }
            case SysCalls.TimeSeconds:
            {
                Time.Seconds();
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
            
            case SysCalls.MemoryReadByte:
            {
                Memory.ReadByte();
            }
            case SysCalls.MemoryWriteByte:
            {
                Memory.WriteByte();
            }
            case SysCalls.MemoryAvailable:
            {
                Memory.Available();
            }
            case SysCalls.MemoryMaximum:
            {
                Memory.Maximum();
            }
            case SysCalls.MemoryAllocate:
            {
                Memory.Allocate();
            }
            case SysCalls.MemoryFree:
            {
                Memory.Free();
            }
            
            case SysCalls.ArrayNew:
            {
                Array.New();
            }
            case SysCalls.ArrayCountGet:
            {
                Array.CountGet();
            }
            case SysCalls.ArrayGetItem:
            {
                Array.GetItem();
            }
            case SysCalls.ArraySetItem:
            {
                Array.SetItem();
            }
            
            case SysCalls.StringNewFromConstant:
            {
                String.NewFromConstant();
            }
            case SysCalls.StringNew:
            {
                String.New();
            }
            case SysCalls.StringLengthGet:
            {
                String.LengthGet();
            }
            case SysCalls.StringGetChar:
            {
                String.GetChar();
            }
            case SysCalls.StringBuild:
            {
                String.Build();
            }
            case SysCalls.StringBuildFront:
            {
                String.BuildFront();
            }
            default:
            {
                missing();
            }
        }
    }
    SysCall()
    {
        ConsumeOperandA(); // iSysCall  -> A (uses ACC)
        PHA
        PopACC();          // iOverload -> ACC
        
        // load iSyscCall into X (because JMP [nnnn,X] is then possible)
#ifdef CPU_65C02S
        PLX
#else        
        PLA
        TAX
#endif
        // iOverload in ACCL
        // iSysCall  in X
        SysCallShared();
    }
#ifdef PACKED_INSTRUCTIONS    
    SysCall0()
    {
        ConsumeOperandA(); // iSysCall  -> A (uses ACC)
        TAX                // load iSyscCall into X (because JMP [nnnn,X] is then possible)
        
        // iOverload -> ACCL
        LDA # 0
        STA ACCL
        
        // iOverload in ACCL
        // iSysCall  in X
        SysCallShared();
    }
    SysCall1()
    {
        ConsumeOperandA(); // iSysCall  -> A (uses ACC)
        TAX                // load iSyscCall into X (because JMP [nnnn,X] is then possible)
        
        // iOverload -> ACCL
        LDA # 1
        STA ACCL
        
        // iOverload in ACCL
        // iSysCall  in X
        SysCallShared();
    }
#endif
}
