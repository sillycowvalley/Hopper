unit LibCall
{
#ifdef I2C
    uses "6502/I2C"
#endif
    
    enum LibCalls
    {
        WireBeginTx     = 0x05,
        WireEndTx       = 0x06,
        WireWrite       = 0x07,
        
        WireRead        = 0x09,
        WireRequestFrom = 0x0A,
    }
    missing()
    {
#ifdef CHECKED
        TXA // LibCall not Implemented!
        Diagnostics.Die();
#endif
    }
    
    LibCallShared()
    {
        // iOverload in ACCL
        // iLibCall  in X
        
        switch (X)
        {
            case LibCalls.WireBeginTx:
            {
#ifdef I2C
                I2C.BeginTx();
#else
                missing();
#endif
            }
            case LibCalls.WireEndTx:
            {
#ifdef I2C
                I2C.EndTx();
#else
                missing();
#endif
            }
            case LibCalls.WireWrite:
            {
#ifdef I2C
                I2C.Write();
#else
                missing();
#endif
            }
            case LibCalls.WireRead:
            {
#ifdef I2C
                I2C.Read();
#else
                missing();
#endif
            }
            case LibCalls.WireRequestFrom:
            {
#ifdef I2C
                I2C.RequestFrom();
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
    LibCall()
    {
        ConsumeOperandA(); // iLibCall  -> A (uses ACC)
        PHA
        Stacks.PopACC();   // iOverload -> ACCL, only care about ACCL (not ACCT or ACCH)
        
        // load iLibCall into X (because JMP [nnnn,X] is then possible)
#ifdef CPU_65C02S
        PLX
#else        
        PLA
        TAX
#endif
        // iOverload in ACCL
        // iLibCall  in X
        LibCallShared();
    }
#ifdef PACKED_INSTRUCTIONS    
    LibCall0()
    {
        ConsumeOperandA(); // iLibCall  -> A (uses ACC)
        TAX                // load iLibCall into X (because JMP [nnnn,X] is then possible)
        
        // iOverload -> ACCL
#ifdef CPU_65C02S
        STZ ACCL
#else        
        LDA # 0
        STA ACCL
#endif
        
        // iOverload in ACCL
        // iLibCall  in X
        LibCallShared();
    }
    LibCall1()
    {
        ConsumeOperandA(); // iLibCall  -> A (uses ACC)
        TAX                // load iLibCall into X (because JMP [nnnn,X] is then possible)
        
        // iOverload -> ACCL
        LDA # 1
        STA ACCL
        
        // iOverload in ACCL
        // iLibCall  in X
        LibCallShared();
    }
#endif    
}
