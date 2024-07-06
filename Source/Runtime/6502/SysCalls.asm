unit SysCall
{
    uses "6502/Array"
    uses "6502/String"
#ifdef LISTS    
    uses "6502/List"
#endif
#ifdef LONGS    
    uses "6502/Long"
#endif
#ifdef FLOATS
    uses "6502/Float"
#endif
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
        
        ArrayItemTypeGet      = 0x1C,
        
        LongNew               = 0x1D,
        LongNewFromConstant   = 0x1E,
        LongFromBytes         = 0x1F,
        LongGetByte           = 0x20,
        
        FloatNew              = 0x21,
        FloatNewFromConstant  = 0x22,
        FloatFromBytes        = 0x23,
        FloatGetByte          = 0x24,
        
        TimeMillis            = 0x25,
        
        VariantBox            = 0x27,
        
        VariantUnBox          = 0x28, // TODO
        
        // ....
        
        UIntToInt             = 0x37,
        
        // ....
        
        TypesTypeOf           = 0x7E, // TODO
        TypesBoxTypeOf        = 0x81, // TODO
        
        // ....
                
        ListNew               = 0xF4,
        ListCountGet          = 0xF5,
        ListAppend            = 0xF6,
        ListInsert            = 0xF7,
        ListGetItem           = 0xF8,
        ListGetItemAsVariant  = 0xF9,
        ListSetItem           = 0xFA,
        ListClear             = 0xFB,
        ListRemove            = 0xFC,
        ListContains          = 0xFD,
    }
    
    missing()
    {
#ifdef CHECKED
        TXA // SysCall not Implemented!
        Diagnostics.die();
#endif
    }
    serialConnect()
    {
        // NOP (we're already connected to serial)
    }
    serialWriteChar()
    {
        Stacks.PopA();
        Serial.WriteChar();
    }
    
    serialIsAvailable()
    {
        Serial.IsAvailable();
        if (Z)
        {
            LDX # 0
        }
        else
        {
            LDX # 1
        }
        Stacks.PushX();
    }
    serialReadChar()
    {
        Serial.WaitForChar();
        STA ZP.TOPL
#ifdef CPU_65C02S
        STZ ZP.TOPH
#else
        LDA # 0
        STA ZP.TOPH
#endif        
        LDA # Types.Char
        Stacks.PushTop(); // type is in A
    }
    
    byteToHex()
    {
        // convert nibble to hex char
        Stacks.PopA();
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
#ifdef CPU_65C02S
        STZ ZP.TOPH
#else
        LDA # 0
        STA ZP.TOPH
#endif
        LDA # Types.Char
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
        Stacks.PushNext();
    }
    intFromBytes()
    {
        PopTopNext();
        LDA ZP.TOPL
        STA ZP.NEXTH
        LDA # Types.Int
        Stacks.PushNext();
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
            case SysCalls.SerialReadChar:
            {
                serialReadChar();
            }
            case SysCalls.SerialIsAvailable:
            {
                serialIsAvailable();
            }
            
            case SysCalls.TimeDelay:
            {
                Time.Delay();
            }
            case SysCalls.TimeSeconds:
            {
                Time.Seconds();
            }
            case SysCalls.TimeMillis:
            {
#ifdef LONGS
                Time.Millis();
#else
                missing();
#endif
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
            case SysCalls.ArrayItemTypeGet:
            {
                Array.ItemTypeGet();
            }
            case SysCalls.ArrayGetItem:
            {
                Array.GetItem();
            }
            case SysCalls.ArraySetItem:
            {
                Array.SetItem();
            }
            case SysCalls.ArrayNewFromConstant:
            {
                Array.NewFromConstant();
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
            
            case SysCalls.LongNew:
            {
#ifdef LONGS
                Long.New();
#else
                missing();
#endif                
            }
            case SysCalls.LongNewFromConstant:
            {
#ifdef LONGS
                Long.NewFromConstant();
#else
                missing();
#endif                
            }
            case SysCalls.LongFromBytes:
            {
#ifdef LONGS
                Long.FromBytes();
#else
                missing();
#endif                
            }
            case SysCalls.LongGetByte:
            {
#ifdef LONGS
                Long.GetByte();
#else
                missing();
#endif                
            }
            
            case SysCalls.FloatNew:
            {
#ifdef FLOATS
                Float.New();
#else
                missing();
#endif                
            }
            case SysCalls.FloatNewFromConstant:
            {
#ifdef FLOATS
                Float.NewFromConstant();
#else
                missing();
#endif                
            }
            case SysCalls.FloatFromBytes:
            {
#ifdef FLOATS
                Float.FromBytes();
#else
                missing();
#endif                
            }
            case SysCalls.FloatGetByte:
            {
#ifdef FLOATS
                Float.GetByte();
#else
                missing();
#endif                
            }
            case SysCalls.VariantBox:
            {
#ifdef LISTS                
                Variant.Box();
#else
                missing();
#endif                
            }
            
            case SysCalls.VariantUnBox:
            {
#ifdef LISTS                
                Variant.UnBox();
#else
                missing();
#endif                
            }
            
            case SysCalls.UIntToInt:  
            {
                UIntToInt();
            }
            case SysCalls.TypesTypeOf:  
            {
#ifdef LISTS                
                Type.TypeOf();
#else
                missing();
#endif                
            }
            case SysCalls.TypesBoxTypeOf:  
            {
#ifdef LISTS                
                Type.BoxTypeOf();
#else
                missing();
#endif                
            }
            
            case SysCalls.ListNew:
            {
#ifdef LISTS
                List.New();
#else
                missing();
#endif                
            }
            case SysCalls.ListCountGet:
            {
#ifdef LISTS
                List.CountGet();
#else
                missing();
#endif                
            } 
            case SysCalls.ListAppend:
            {
#ifdef LISTS
                List.Append();
#else
                missing();
#endif                
            }  
            case SysCalls.ListInsert:
            {
#ifdef LISTS
                List.Insert();
#else
                missing();
#endif                
            } 
            case SysCalls.ListGetItem:
            {
#ifdef LISTS
                List.GetItem();
#else
                missing();
#endif                
            } 
            case SysCalls.ListGetItemAsVariant:
            {
#ifdef LISTS
                List.GetItemAsVariant();
#else
                missing();
#endif                
            } 
            case SysCalls.ListSetItem:
            {
#ifdef LISTS
                List.SetItem();
#else
                missing();
#endif                
            }  
            case SysCalls.ListClear:
            {
#ifdef LISTS
                List.Clear();
#else
                missing();
#endif                
            }
            case SysCalls.ListRemove:
            {
#ifdef LISTS
                List.Remove();
#else
                missing();
#endif                
            }
            case SysCalls.ListContains:
            {
#ifdef LISTS
                List.Contains();
#else
                missing();
#endif                
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
        Stacks.PopACC();   // iOverload -> ACCL, only care about ACCL (not ACCT or ACCH)
        
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
        TAX                // load iSysCall into X (because JMP [nnnn,X] is then possible)
        
        // iOverload -> ACCL
#ifdef CPU_65C02S
        STZ ACCL
#else        
        LDA # 0
        STA ACCL
#endif
        
        // iOverload in ACCL
        // iSysCall  in X
        SysCallShared();
    }
    SysCall1()
    {
        ConsumeOperandA(); // iSysCall  -> A (uses ACC)
        TAX                // load iSysCall into X (because JMP [nnnn,X] is then possible)
        
        // iOverload -> ACCL
        LDA # 1
        STA ACCL
        
        // iOverload in ACCL
        // iSysCall  in X
        SysCallShared();
    }
    SysCall2()
    {
        ConsumeOperandA(); // iSysCall  -> A (uses ACC)
        TAX                // load iSysCall into X (because JMP [nnnn,X] is then possible)
        
        // iOverload -> ACCL
        LDA # 2
        STA ACCL
        
        // iOverload in ACCL
        // iSysCall  in X
        SysCallShared();
    }
#endif
}
