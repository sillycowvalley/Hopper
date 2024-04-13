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
        
        SerialWriteChar       = 0x0F,
        SerialWriteChar       = 0x10,
        SerialIsAvailable     = 0x11,
        
        MemoryReadByte        = 0x12,
        MemoryWriteByte       = 0x13,
        MemoryAvailable       = 0x14,
        MemoryMaximum         = 0x15,
        MemoryAllocate        = 0x16,
        MemoryFree            = 0x17,
        
        ByteToHex             = 0xBE,
        
        IntGetByte            = 0xE1,
        IntFromBytes          = 0xE4,
        
                
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
        sysCall();
    }
#ifdef PACKED_INSTRUCTIONS    
    SysCall0()
    {
        ConsumeOperandA(); // iSysCall  -> A (uses ACC)
        PHA
        
        // iOverload -> ACC
        LDA # 0
        STA ACCL
        STA ACCH
        
        // load iSyscCall into X (because JMP [nnnn,X] is then possible)
#ifdef CPU_65C02S
        PLX
#else        
        PLA
        TAX
#endif
        sysCall();
    }
#endif
#ifdef PACKED_INSTRUCTIONS    
    SysCall1()
    {
        ConsumeOperandA(); // iSysCall  -> A (uses ACC)
        PHA
        
        // iOverload -> ACC
        LDA # 1
        STA ACCL
        STA ACCH
        
        // load iSyscCall into X (because JMP [nnnn,X] is then possible)
#ifdef CPU_65C02S
        PLX
#else        
        PLA
        TAX
#endif
        sysCall();
    }
#endif
}
