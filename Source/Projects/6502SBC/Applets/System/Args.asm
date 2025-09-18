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
        
        getArgument(); // filename length -> A, X != 0 means "." seen
        
        // append ".C" if there is no "."
        CPX #0
        if (Z)
        {
            // getArgument gives us a STR that is pointing into Address.LineBuffer so we can extend the filename safely
            LDY #0
            loop
            {
                LDA [ZP.STR], Y
                if (Z) { break; } 
                INY
            }
            LDA #'.'
            STA [ZP.STR], Y
            INY
            LDA #'C'
            STA [ZP.STR], Y
            INY
            LDA #0
            STA [ZP.STR], Y
            TYA // length -> A
        }
        
        
        PLX
        PLY
    }
    
    // Private helper: Check for command line argument
    // Output: C set if argument exists, STR points to argument, A = length, X != 0 - there was a dot
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
