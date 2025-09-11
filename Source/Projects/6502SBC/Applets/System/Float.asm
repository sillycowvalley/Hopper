unit Float
{
    // X = #ZP.NEXT0 or #ZP.TOP0
    IsZero()
    {
        LDA 0x00, X
        ORA 0x01, X
        ORA 0x02, X
        if (Z)
        {
            LDA 0x03, X
            AND # 0x7F    // ignore the sign for -0
            if (Z)
            {
                SEC
                return;
            }
        }
        CLC
    }
    
    // IEEE 754 single-precision addition
    // Input:  ZP.NEXT = first operand (IEEE float)
    //         ZP.TOP = second operand (IEEE float)
    // Output: ZP.NEXT = NEXT + TOP
    // Note:   Handles special values (NaN, Inf) per IEEE 754
    Add()
    {
        LDX # SysCall.FloatAdd
        JMP [ZP.BIOSDISPATCH]
    }
    
    // IEEE 754 single-precision subtraction
    // Input:  ZP.NEXT = minuend (IEEE float)
    //         ZP.TOP = subtrahend (IEEE float)
    // Output: ZP.NEXT = NEXT - TOP
    // Note:   Handles special values (NaN, Inf) per IEEE 754
    Sub()
    {
        LDX # SysCall.FloatSub
        JMP [ZP.BIOSDISPATCH]
    }
    
    // IEEE 754 single-precision multiplication
    // Input:  ZP.NEXT = multiplicand (IEEE float)
    //         ZP.TOP = multiplier (IEEE float)
    // Output: ZP.NEXT = NEXT * TOP
    // Note:   Handles special values (NaN, Inf) per IEEE 754
    Mul()
    {
        LDX # SysCall.FloatMul
        JMP [ZP.BIOSDISPATCH]
    }
    
    // IEEE 754 single-precision division
    // Input:  ZP.NEXT = dividend (IEEE float)
    //         ZP.TOP = divisor (IEEE float)
    // Output: ZP.NEXT = NEXT / TOP
    // Note:   Division by zero returns Inf per IEEE 754
    Div()
    {
        LDX # SysCall.FloatDiv
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Convert IEEE 754 float to 32-bit signed integer
    // Input:  ZP.NEXT = float value (IEEE float)
    // Output: ZP.NEXT = truncated integer value (32-bit)
    // Note:   Truncates toward zero
    //         Out of range values saturate to INT32_MIN/MAX
    ToLong()
    {
        LDX # SysCall.FloatToLong
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Less than comparison
    // Input:  ZP.NEXT, ZP.TOP = values to compare (IEEE floats)
    // Output: C set if NEXT < TOP, clear otherwise
    // Note:   NaN comparisons always return false (C clear)
    LT()
    {
        LDX # SysCall.FloatLT
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Equality comparison
    // Input:  ZP.NEXT, ZP.TOP = values to compare (IEEE floats)
    // Output: C set if NEXT == TOP, clear otherwise
    // Note:   NaN != NaN per IEEE 754
    //         -0.0 == +0.0 per IEEE 754
    EQ()
    {
        LDX # SysCall.FloatEQ
        JMP [ZP.BIOSDISPATCH]
    }
}
