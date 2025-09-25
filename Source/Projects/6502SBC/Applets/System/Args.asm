unit Args
{
    // Private helper: Check for command line argument
    // Output: C set if argument exists, STR points to argument, A = length, X != 0 - there was a dot
    //         NC if no argument
    GetArgument()
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
        
        // Measure argument length and look for "."
        STZ ZP.TEMP
        LDX #0  // Length counter
        loop
        {
            LDA Address.LineBuffer, Y
            if (Z) { break; }  // Found end of argument
            CMP #'.'
            if (Z)
            {
                INC ZP.TEMP // "." found
            }
            INX
            INY
            CPY #64
            if (Z) { break; }  // Hit end of buffer
        }
        TXA  // Return length in A
        
        LDX ZP.TEMP // non-zero -> dot exists
        
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
        
        GetArgument();
        
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
