unit Args
{
    // Get number of command line arguments (including command itself)
    // Input:  None
    // Output: A = argument count (1=command only, 2=command+1arg, etc.)
    //         C = set (always succeeds)
    // Preserves: X, Y
    Count()
    {
        LDX # SysCall.ArgCount
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Get pointer to argument N (0-based indexing)
    // Input:  A = argument index (0=command, 1=first arg, 2=second arg, etc.)
    // Output: ZP.STR = pointer to null-terminated argument string
    //         C = set if successful, NC if argument index out of range
    // Preserves: X, Y  
    GetArg()
    {
        LDX # SysCall.ArgGet
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Check if filename argument exists (without retrieving it)
    // Input:  None
    // Output: C set if argument exists, clear if none
    // Note:   Does not modify ZP.STR
    HasFilename()
    {
        PHA
#ifdef UNIVERSAL
        TYA PHA TXA PHA   
#else
        PHY PHX
#endif
        
        Args.Count();
        CMP #2 // C is set if argument count >= 2
               
        
#ifdef UNIVERSAL        
        PLA TAX PLA TAY
#else
        PLX PLY
#endif
        PLA
    }
}
