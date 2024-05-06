unit LibCall
{
    uses "6502/I2C"
    
    enum LibCalls
    {
        WireBeginTx = 0x05,
        WireEndTx   = 0x06,
        WireWrite   = 0x07,
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
                I2C.BeginTx();
            }
            case LibCalls.WireEndTx:
            {
                I2C.EndTx();
            }
            case LibCalls.WireWrite:
            {
                I2C.Write();
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
