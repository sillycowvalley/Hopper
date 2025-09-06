unit Long
{
    // 32-bit addition
    // Input:  ZP.NEXT = first operand (32-bit)
    //         ZP.TOP = second operand (32-bit)
    // Output: ZP.NEXT = NEXT + TOP
    // Note:   No overflow detection
    Add()
    {
        LDX # SysCall.LongAdd
        JMP [ZP.BIOSDISPATCH]
    }
    
    // 32-bit subtraction
    // Input:  ZP.NEXT = minuend (32-bit)
    //         ZP.TOP = subtrahend (32-bit)
    // Output: ZP.NEXT = NEXT - TOP
    // Note:   No underflow detection
    Sub()
    {
        LDX # SysCall.LongSub
        JMP [ZP.BIOSDISPATCH]
    }
    
    // 32-bit multiplication
    // Input:  ZP.NEXT = multiplicand (32-bit)
    //         ZP.TOP = multiplier (32-bit)
    // Output: ZP.NEXT = NEXT * TOP
    // Note:   Lower 32 bits of result only
    //         Optimized for powers of 2
    Mul()
    {
        LDX # SysCall.LongMul
        JMP [ZP.BIOSDISPATCH]
    }
    
    // 32-bit division
    // Input:  ZP.NEXT = dividend (32-bit)
    //         ZP.TOP = divisor (32-bit)
    // Output: ZP.NEXT = NEXT / TOP
    //         C clear on division by zero
    // Note:   Integer division, truncates toward zero
    Div()
    {
        LDX # SysCall.LongDiv
        JMP [ZP.BIOSDISPATCH]
    }
    
    // 32-bit modulo
    // Input:  ZP.NEXT = dividend (32-bit)
    //         ZP.TOP = divisor (32-bit)
    // Output: ZP.NEXT = NEXT % TOP
    //         C clear on division by zero
    // Note:   Result has sign of dividend
    Mod()
    {
        LDX # SysCall.LongMod
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Print 32-bit value as decimal
    // Input:  ZP.TOP = value to print (32-bit)
    // Output: None (decimal printed to serial)
    // Note:   Handles negative values with leading '-'
    Print()
    {
        LDX # SysCall.LongPrint
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Less than comparison
    // Input:  ZP.NEXT, ZP.TOP = values to compare (32-bit)
    // Output: C set if NEXT < TOP, clear otherwise
    LT()
    {
        LDX # SysCall.LongLT
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Greater than comparison
    // Input:  ZP.NEXT, ZP.TOP = values to compare (32-bit)
    // Output: C set if NEXT > TOP, clear otherwise
    GT()
    {
        LDX # SysCall.LongGT
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Equality comparison
    // Input:  ZP.NEXT, ZP.TOP = values to compare (32-bit)
    // Output: C set if NEXT == TOP, clear otherwise
    EQ()
    {
        LDX # SysCall.LongEQ
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Not equal comparison
    // Input:  ZP.NEXT, ZP.TOP = values to compare (32-bit)
    // Output: C set if NEXT != TOP, clear otherwise
    NE()
    {
        LDX # SysCall.LongNE
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Less than or equal comparison
    // Input:  ZP.NEXT, ZP.TOP = values to compare (32-bit)
    // Output: C set if NEXT <= TOP, clear otherwise
    LE()
    {
        LDX # SysCall.LongLE
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Greater than or equal comparison
    // Input:  ZP.NEXT, ZP.TOP = values to compare (32-bit)
    // Output: C set if NEXT >= TOP, clear otherwise
    GE()
    {
        LDX # SysCall.LongGE
        JMP [ZP.BIOSDISPATCH]
    }
}