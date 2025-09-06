unit Time
{
    // Delay execution for specified milliseconds
    // Input:  ZP.TOP = delay in milliseconds (32-bit)
    // Output: None (returns after delay)
    // Note:   Based on VIA timer, accurate to 1ms
    //         Maximum delay: ~49 days
    Delay()
    {
        LDX # SysCall.TimeDelay
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Get milliseconds since boot
    // Input:  None
    // Output: ZP.TOP = milliseconds (32-bit)
    // Note:   Wraps after ~49 days
    //         Updated by timer interrupt
    Millis()
    {
        LDX # SysCall.TimeMillis
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Get seconds since boot
    // Input:  None
    // Output: ZP.TOP = seconds (32-bit)
    // Note:   Calculated as Millis()/1000
    //         Maximum: ~136 years
    Seconds()
    {
        LDX # SysCall.TimeSeconds
        JMP [ZP.BIOSDISPATCH]
    }
}