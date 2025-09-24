unit Stacks // Stacks.asm
{
    #define HOPPER_STACK
    
    uses "/Source/Runtime/6502/Utilities"
    
    Initialize()
    {
#ifdef CPU_65C02S        
        STZ ZP.SP
        STZ ZP.BP
        STZ ZP.CSP
        
        // zeroes mean faster debug protocol
        STZ IDXL  // setting this to 0 once is enough (ClearPages does not modify IDXL)
#else
        LDA # 0
        STA ZP.SP
        STA ZP.BP
        STA ZP.CSP
        
        // zeroes mean faster debug protocol
        STA IDXL // setting this to 0 once is enough (ClearPages does not modify IDXL)
#endif

#ifdef MEMORY_CHECK
        LDA # (ClearPages / 256) STA ACCH LDA # (ClearPages % 256) STA ACCL PrintIndentACC();
#endif

        LDA # (Address.CallStackLSB >> 8)
        STA IDXH
#ifdef MEMORY_CHECK
        Serial.HexOut();
#endif        
        LDX # 5                 // 5 contiguous pages for CallStack, TypeStack and ValueStack
#ifdef MEMORY_CHECK
        PrintCount();
#endif
        Utilities.ClearPages(); // with IDX (memory location) and X (number of pages) initialized (does not modify IDXL)
#ifdef MEMORY_CHECK    
        LDA # (Cleared / 256) STA ACCH LDA # (Cleared % 256) STA ACCL PrintACC();
#endif
        
    }
    
    PopBP()
    {
        DEC ZP.CSP    
        LDY ZP.CSP
        LDA Address.CallStackLSB, Y
        STA ZP.BP
    }
    PushBP()
    {
#ifdef HOPPER_BASIC
        LDY ZP.CSP
        LDA ZP.BP
        STA Address.CallStackLSB, Y
        INC ZP.CSP
#else
        LDX ZP.CSP
        LDA ZP.BP
        STA Address.CallStackLSB, X
        INC ZP.CSP
#endif        
    }
    PopPC()
    {
        DEC ZP.CSP    
        LDY ZP.CSP
        LDA Address.CallStackLSB, Y
        STA ZP.PCL
        LDA Address.CallStackMSB, Y
        STA ZP.PCH
    }
    PushPC()
    {
        LDY ZP.CSP
        LDA ZP.PCL
        STA Address.CallStackLSB, Y
        LDA ZP.PCH
        STA Address.CallStackMSB, Y
        INC ZP.CSP
    }
    
#ifdef HOPPER_BASIC
    PopXID()
    {
        DEC ZP.CSP    
        LDY ZP.CSP
        LDA Address.CallStackLSB, Y
        STA ZP.XIDL
        LDA Address.CallStackMSB, Y
        STA ZP.XIDH
    }
    PushXID()
    {
        LDY ZP.CSP
        LDA ZP.XIDL
        STA Address.CallStackLSB, Y
        LDA ZP.XIDH
        STA Address.CallStackMSB, Y
        INC ZP.CSP
    }
    
    // Input: A = signed offset from BP
    // Output: ZP.TOP = value at BP+offset, ZP.TOPT = type
    // Modifies: A, Y
    GetStackTopBP()
    {
        CLC
        ADC ZP.BP
        TAY
        LDA Address.ValueStackB0, Y
        STA ZP.TOP0
        LDA Address.ValueStackB1, Y
        STA ZP.TOP1
        LDA Address.ValueStackB2, Y
        STA ZP.TOP2
        LDA Address.ValueStackB3, Y
        STA ZP.TOP3
        LDA Address.TypeStackLSB, Y
        AND #BASICType.TYPEMASK  // Strip VAR bit 
        STA ZP.TOPT
    }
    /*
    // Input: A = signed offset from BP
    // Output: ZP.NEXT = value at BP+offset, ZP.NEXTT = type
    // Modifies: A, Y
    GetStackNextBP()
    {
        CLC
        ADC ZP.BP
        TAY
        LDA Address.ValueStackB0, Y
        STA ZP.NEXT0
        LDA Address.ValueStackB1, Y
        STA ZP.NEXT1
        LDA Address.ValueStackB2, Y
        STA ZP.NEXT2
        LDA Address.ValueStackB3, Y
        STA ZP.NEXT3
        LDA Address.TypeStackLSB, Y
        AND #BASICType.TYPEMASK  // Strip VAR bit 
        STA ZP.NEXTT
    }
    */
    // Input: A = signed offset from SP
    // Output: ZP.TOP = value at SP+offset, ZP.TOPT = type
    // Modifies: A, Y
    GetStackTopSP()
    {
        CLC
        ADC ZP.SP
        TAY
        LDA Address.ValueStackB0, Y
        STA ZP.TOP0
        LDA Address.ValueStackB1, Y
        STA ZP.TOP1
        LDA Address.ValueStackB2, Y
        STA ZP.TOP2
        LDA Address.ValueStackB3, Y
        STA ZP.TOP3
        LDA Address.TypeStackLSB, Y
        AND #BASICType.TYPEMASK  // Strip VAR bit 
        STA ZP.TOPT
    }
    
    // Input: A = signed offset from SP
    // Output: ZP.NEXT = value at SP+offset, ZP.NEXTT = type
    // Modifies: A, Y
    GetStackNextSP()
    {
        CLC
        ADC ZP.SP
        TAY
        LDA Address.ValueStackB0, Y
        STA ZP.NEXT0
        LDA Address.ValueStackB1, Y
        STA ZP.NEXT1
        LDA Address.ValueStackB2, Y
        STA ZP.NEXT2
        LDA Address.ValueStackB3, Y
        STA ZP.NEXT3
        LDA Address.TypeStackLSB, Y
        AND #BASICType.TYPEMASK  // Strip VAR bit 
        STA ZP.NEXTT
    }
    
    // Input: A = signed offset from BP, ZP.TOP = value to store
    // Modifies: A, Y
    SetStackTopBP()
    {
        CLC
        ADC ZP.BP
        TAY
        LDA ZP.TOP0
        STA Address.ValueStackB0, Y
        LDA ZP.TOP1
        STA Address.ValueStackB1, Y
        LDA ZP.TOP2
        STA Address.ValueStackB2, Y
        LDA ZP.TOP3
        STA Address.ValueStackB3, Y
        LDA ZP.TOPT
        STA Address.TypeStackLSB, Y
    }
#endif

    PopTop()
    {
        DEC ZP.SP
        LDX ZP.SP
        LDA Address.ValueStackLSB, X
        STA ZP.TOPL
        LDA Address.ValueStackMSB, X
        STA ZP.TOPH
        LDA Address.TypeStackLSB, X
        STA ZP.TOPT
    }
    PopNext()
    {
        DEC ZP.SP
        LDX ZP.SP
        LDA Address.ValueStackLSB, X
        STA ZP.NEXTL
        LDA Address.ValueStackMSB, X
        STA ZP.NEXTH
        LDA Address.TypeStackLSB, X
        STA ZP.NEXTT
    }

    PushTop() // type is in A
    {
        LDY ZP.SP
        STA Address.TypeStackLSB, Y
        LDA ZP.TOPL
        STA Address.ValueStackLSB, Y
        LDA ZP.TOPH
        STA Address.ValueStackMSB, Y
        INC ZP.SP
    }
    PushIDX() // type is in A
    {
        LDY ZP.SP
        STA Address.TypeStackLSB, Y
        LDA ZP.IDXL
        STA Address.ValueStackLSB, Y
        LDA ZP.IDXH
        STA Address.ValueStackMSB, Y
        INC ZP.SP
    }
    PopTopNext()
    {
#ifdef INLINE_EXPANSIONS  
        LDX ZP.SP
        DEX
        LDA Address.ValueStackLSB, X
        STA ZP.TOPL
        LDA Address.ValueStackMSB, X
        STA ZP.TOPH
        LDA Address.TypeStackLSB, X
        STA ZP.TOPT 
        
        DEX
        STX ZP.SP
        LDA Address.ValueStackLSB, X
        STA ZP.NEXTL
        LDA Address.ValueStackMSB, X
        STA ZP.NEXTH
        LDA Address.TypeStackLSB, X
        STA ZP.NEXTT     
#else
        Stacks.PopTop();
        Stacks.PopNext();
#endif
    }
    PushNext() // type is in A
    {
        LDY ZP.SP
        STA Address.TypeStackLSB, Y
        LDA ZP.NEXTL
        STA Address.ValueStackLSB, Y
        LDA ZP.NEXTH
        STA Address.ValueStackMSB, Y
        INC ZP.SP
    }
    PushACC()
    {
        LDY ZP.SP
        LDA ZP.ACCL
        STA Address.ValueStackLSB, Y
        LDA ZP.ACCH
        STA Address.ValueStackMSB, Y
        LDA ZP.ACCT
        STA Address.TypeStackLSB, Y
        INC ZP.SP
    }
    PushX()
    {
        // value is in X: 0 or 1
        STX ZP.NEXTL
#ifdef CPU_65C02S   
        STZ ZP.NEXTH     
#else
        LDA # 0
        STA ZP.NEXTH
#endif
        LDA #Types.Bool
        STA ZP.NEXTT
        PushNext();
    }
    
    PopACC()
    {
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        STA ZP.ACCL
        LDA Address.ValueStackMSB, Y
        STA ZP.ACCH
        // Type is never used / checked
        //LDA Address.TypeStackLSB, Y
        //STA ZP.ACCT
    }
    PopIDX()
    {
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        STA ZP.IDXL
        LDA Address.ValueStackMSB, Y
        STA ZP.IDXH
    }
    PopIDY()
    {
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
        STA ZP.IDYL
        LDA Address.ValueStackMSB, Y
        STA ZP.IDYH
    }
    
    PopA() // pop a byte (don't care about type or MSB)
    {
        DEC ZP.SP
        LDY ZP.SP
        LDA Address.ValueStackLSB, Y
    }
}
