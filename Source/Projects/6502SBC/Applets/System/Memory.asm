unit Memory
{
    // Allocate memory block
    // Input:  ZP.ACC = requested size in bytes (16-bit)
    // Output: ZP.IDX = allocated address (0x0000 if failed)
    //         C set on success, clear on failure
    // Note:   Minimum allocation is 6 bytes, rounded up to 8-byte boundary
    Allocate()
    {
        LDX # SysCall.MemAllocate
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Free memory block
    // Input:  ZP.IDX = address to free (must not be 0x0000)
    // Output: C set on success, clear on error
    // Note:   Do not free the same block twice
    Free()
    {
        LDX # SysCall.MemFree
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Get available free memory
    // Input:  None
    // Output: ZP.ACC = available bytes (16-bit)
    // Note:   Returns total free space across all free blocks
    Available()
    {
        LDX # SysCall.MemAvailable
        JMP [ZP.BIOSDISPATCH]
    }
    
    // Get largest contiguous free block
    // Input:  None
    // Output: ZP.ACC = size of largest free block (16-bit)
    // Note:   Useful to check before large allocations
    Maximum()
    {
        LDX # SysCall.MemMaximum
        JMP [ZP.BIOSDISPATCH]
    }
}