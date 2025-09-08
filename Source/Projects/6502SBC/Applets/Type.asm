program Type
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/File"
    uses "System/Serial"
    
    // Filename to type - change this or make it dynamic
    const string fileName = "BIGTEST";
    
    // Messages
    const string notFound = "File not found: ";
    const string errorMsg = "Error reading file!\n";
    const string doneMsg = "\n--- End of file ---\n";
    const string dots = "...";
    
    // Workspace
    const byte charCount = 0x58;
    
    Hopper()
    {
        // Check if file exists
        LDA #(fileName % 256)
        STA ZP.STRL
        LDA #(fileName / 256)
        STA ZP.STRH
        
        LDA #File.FileType.Any
        File.Exists();
        if (NC)
        {
            // File not found
            LDA #(notFound % 256)
            STA ZP.STRL
            LDA #(notFound / 256)
            STA ZP.STRH
            Print.String();
            
            LDA #(fileName % 256)
            STA ZP.STRL
            LDA #(fileName / 256)
            STA ZP.STRH
            Print.String();
            
            Print.NewLine();
            return;
        }
        
        // Open file for reading
        LDA #(fileName % 256)
        STA ZP.STRL
        LDA #(fileName / 256)
        STA ZP.STRH
        LDA #File.FileType.Any
        File.StartLoad();
        if (NC)
        {
            LDA #(errorMsg % 256)
            STA ZP.STRL
            LDA #(errorMsg / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        // Read and display file in chunks
        loop
        {
            // Read next chunk
            File.NextStream();
            if (NC) { break; }  // End of file
            
            // Display this chunk
            // Data is in File.FileDataBuffer
            // Length is in File.TransferLengthL/H
            
            LDY #0
            loop
            {
                // Check if done with chunk
                CPY File.TransferLengthL
                if (Z)
                {
                    // Check high byte
                    LDA File.TransferLengthH
                    if (Z) { break; }  // Done if high byte is also 0
                }
                
                // Get and print character
                LDA File.FileDataBuffer, Y
                
                // Handle special characters
                CMP #0x0D  // CR
                if (Z)
                {
                    // Skip CR (we'll handle LF)
                }
                else
                {
                    CMP #0x09  // Tab
                    if (Z)
                    {
                        // Expand tab to spaces
                        LDA #' '
                        Serial.WriteChar();
                        LDA #' '
                        Serial.WriteChar();
                        LDA #' '
                        Serial.WriteChar();
                        LDA #' '
                        Serial.WriteChar();
                    }
                    else
                    {
                        // Normal character or LF
                        Serial.WriteChar();
                    }
                }
                
                // Check for break key
                Serial.IsBreak();
                if (C) 
                { 
                    Print.NewLine();
                    LDA #(dots % 256)
                    STA ZP.STRL
                    LDA #(dots / 256)
                    STA ZP.STRH
                    Print.String();
                    Print.NewLine();
                    return; 
                }
                
                INY
                if (Z) { break; }  // Wrapped after 256 bytes
            }
        }
        
        // Show end of file marker
        LDA #(doneMsg % 256)
        STA ZP.STRL
        LDA #(doneMsg / 256)
        STA ZP.STRH
        Print.String();
    }
}
