unit Float
{
    // IEEE 754 single precision float addition
    // Input:  ZP.NEXT = first operand (IEEE float, 32-bit)
    //         ZP.TOP = second operand (IEEE float, 32-bit)
    // Output: ZP.NEXT = NEXT + TOP
    // Note:   Handles special values (±0, ±∞, NaN)
    //         Requires HASFLOAT build option
    Add()
    {
        LDX # SysCall.FloatAdd
        JMP [ZP.BIOSDISPATCH]
    }
    
    // IEEE 754 single precision float subtraction
    // Input:  ZP.NEXT = minuend (IEEE float, 32-bit)
    //         ZP.TOP = subtrahend (IEEE float, 32-bit)
    // Output: ZP.NEXT = NEXT - TOP
    // Note:   Implemented as NEXT + (-TOP)
    Sub()
    {
        LDX # SysCall.FloatSub
        JMP [ZP.BIOSDISPATCH]
    }
    
    // IEEE 754 single precision float multiplication
    // Input:  ZP.NEXT = multiplicand (IEEE float, 32-bit)
    //         ZP.TOP = multiplier (IEEE float, 32-bit)
    // Output: ZP.NEXT = NEXT * TOP
    // Note:   Handles overflow/underflow to ±∞/±0
    Mul()
    {
        LDX # SysCall.FloatMul
        JMP [ZP.BIOSDISPATCH]
    }
    
    // IEEE 754 single precision float division
    // Input:  ZP.NEXT = dividend (IEEE float, 32-bit)
    //         ZP.TOP = divisor (IEEE float, 32-bit)
    // Output: ZP.NEXT = NEXT / TOP
    // Note:   Division by zero returns ±∞
    Div()
    {
        LDX # SysCall.FloatDiv
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Convert IEEE float to 32-bit signed integer
    // Input:  ZP.NEXT = float value (IEEE float, 32-bit)
    // Output: ZP.NEXT = truncated integer value (32-bit signed)
    // Note:   Truncates toward zero
    //         Returns 0 for NaN or overflow
    ToLong()
    {
        LDX # SysCall.FloatToLong
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Float less than comparison
    // Input:  ZP.NEXT, ZP.TOP = values to compare (IEEE floats)
    // Output: C set if NEXT < TOP, clear otherwise
    // Note:   NaN comparisons always return false
    LT()
    {
        LDX # SysCall.FloatLT
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Float equality comparison
    // Input:  ZP.NEXT, ZP.TOP = values to compare (IEEE floats)
    // Output: C set if NEXT == TOP, clear otherwise
    // Note:   +0.0 == -0.0, NaN != NaN
    EQ()
    {
        LDX # SysCall.FloatEQ
        JMP [ZP.BIOSDISPATCH]
    }
}