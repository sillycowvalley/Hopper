unit Instructions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Stacks"
    
    // Arithmetic operations
    // All operations work on the top two values on the stack
    
    addShared()
    {
        Stacks.PopNext();
        CLC
        LDA ZP.NEXTL
        ADC ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        ADC ZP.TOPH
        STA ZP.NEXTH
        
        LDA # BasicType.INT // TODO : types
    }
    
    subShared()
    {
        Stacks.PopNext();
        SEC
        LDA ZP.NEXTL
        SBC ZP.TOPL
        STA ZP.NEXTL
        LDA ZP.NEXTH
        SBC ZP.TOPH
        STA ZP.NEXTH
        
        LDA # BasicType.INT // TODO : types
    }
    
    eqShared()
    {
        Stacks.PopNext();
        
        // TODO : types
        
        LDX # 0
        LDA ZP.NEXTL
        CMP ZP.TOPL
        if (Z)
        {
            LDA ZP.NEXTH
            CMP ZP.TOPH
            if (Z)
            {
                LDX # 1
            }
        }
        Stacks.PushX(); // X, type is BasicType.BIT
    }    
    
    Addition()
    {
        // Pop two operands
        Stacks.PopTop();
        addShared();
        Stacks.PushNext();
    }
        
    Subtraction()
    {
        Stacks.PopTop();
        subShared();
        Stacks.PushNext();
    }
    
    UnaryMinus()
    {
        Stacks.PopTop();
        // Negate: TOP = 0 - TOP
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        subShared();
        Stacks.PushNext();
    }
    
    Equals()
    {
        Stacks.PopTop();
        eqShared();
    }
    
    NotEqual()
    {
        Stacks.PopTopNext();
        
        // TODO : types
        
        LDX # 1
        LDA ZP.NEXTL
        CMP ZP.TOPL
        if (Z)
        {
            LDA ZP.NEXTH
            CMP ZP.TOPH
            if (Z)
            {
                LDX # 0
            }
        }
        Stacks.PushX(); // X, type is BasicType.BIT
    }
}
