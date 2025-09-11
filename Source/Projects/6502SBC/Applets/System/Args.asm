unit Args
{
    // Get filename argument from command line
    // Input:  None (reads from command line buffer at Address.LineBuffer)
    // Output: C set if filename found, clear if none
    //         ZP.STR = pointer to filename (null-terminated, uppercase)
    //         A = filename length
    // Note:   Returns first argument after program name
    //         BIOS has already uppercased the input
    GetFilename()
    {
        PHY
        PHX
        
        getArgument();
        
        PLX
        PLY
    }
    
    // Private helper: Check for command line argument
    // Output: C set if argument exists, STR points to argument, A = length
    //         NC if no argument
    getArgument()
    {
        // Find first null terminator (end of command name)
        LDY #0
        loop
        {
            LDA Address.LineBuffer, Y
            if (Z) { break; }  // Found null
            INY
            CPY #64  // Don't run off the end
            if (Z) 
            { 
                CLC  // No null found - malformed
                return;
            }
        }
        
        // Y points to null terminator, check next byte
        INY
        CPY #64  // At end of buffer?
        if (Z)
        {
            CLC  // No room for argument
            return;
        }
        
        LDA Address.LineBuffer, Y
        if (Z)  // No argument
        {
            CLC
            return;
        }
        
        // Argument exists - calculate its address
        TYA
        CLC
        ADC #(Address.LineBuffer % 256)
        STA ZP.STRL
        LDA #(Address.LineBuffer / 256)
        ADC #0
        STA ZP.STRH
        
        // Measure argument length
        LDX #0  // Length counter
        loop
        {
            LDA Address.LineBuffer, Y
            if (Z) { break; }  // Found end of argument
            INX
            INY
            CPY #64
            if (Z) { break; }  // Hit end of buffer
        }
        
        TXA  // Return length in A
        
        SEC  // Argument exists
    }
    
    // Check if filename argument exists (without retrieving it)
    // Input:  None
    // Output: C set if argument exists, clear if none
    // Note:   Does not modify ZP.STR
    HasFilename()
    {
        PHY
        PHX
        PHA
        
        // Save current STR
        LDA ZP.STRL
        PHA
        LDA ZP.STRH
        PHA
        
        getArgument();
        
        // Restore STR
        PLA
        STA ZP.STRH
        PLA
        STA ZP.STRL
        
        PLA
        PLX
        PLY
    }
}
