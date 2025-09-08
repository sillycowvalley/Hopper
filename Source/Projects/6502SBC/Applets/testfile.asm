program CreateTestFile
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/File"
    uses "System/Serial"
    
    const string testFileName = "BIGTEST";
    
    // Shorter, more manageable strings with newlines
    const string line1 = "The quick brown fox jumps over the lazy dog.\n";
    const string line2 = "Pack my box with five dozen liquor jugs.\n";
    const string line3 = "How vexingly quick daft zebras jump!\n";
    const string line4 = "The five boxing wizards jump quickly.\n";
    const string line5 = "Sphinx of black quartz, judge my vow.\n";
    const string line6 = "0123456789 ABCDEFGHIJKLMNOPQRSTUVWXYZ\n";
    const string line7 = "Testing file I/O on the 6502 system.\n";
    const string line8 = "This is line eight of the test file.\n";
    const string separator = "==========\n";
    
    const string starting = "Creating test file...\n";
    const string done = "File saved successfully!\n";
    const string error = "Error saving file!\n";
    
    // Helper to get string length (not including null)
    getStringLength()  // Input: ZP.STR, Output: A = length
    {
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            if (Z) { break; }  // Found null
            INY
        }
        TYA  // Length in A
    }
    
    // Helper to write a string to file
    writeString()  // Input: ZP.STR points to string
    {
        // Get string length
        getStringLength();
        
        // Set up for AppendStream
        STA File.TransferLengthL
        STZ File.TransferLengthH
        
        LDA ZP.STRL
        STA File.SectorSourceL
        LDA ZP.STRH
        STA File.SectorSourceH
        
        // Write it
        File.AppendStream();
    }
    
    Hopper()
    {
        // Show status
        LDA #(starting % 256)
        STA ZP.STRL
        LDA #(starting / 256)
        STA ZP.STRH
        Print.String();
        
        // Open file for writing
        LDA #(testFileName % 256)
        STA ZP.STRL
        LDA #(testFileName / 256)
        STA ZP.STRH
        File.StartSave();
        if (NC)
        {
            LDA #(error % 256)
            STA ZP.STRL
            LDA #(error / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        // Write the file content - repeat pattern multiple times to get ~2KB
        LDX #10  // Repeat 10 times
        loop
        {
            PHX
            
            // Write separator
            LDA #(separator % 256)
            STA ZP.STRL
            LDA #(separator / 256)
            STA ZP.STRH
            writeString();
            
            // Write each line
            LDA #(line1 % 256)
            STA ZP.STRL
            LDA #(line1 / 256)
            STA ZP.STRH
            writeString();
            
            LDA #(line2 % 256)
            STA ZP.STRL
            LDA #(line2 / 256)
            STA ZP.STRH
            writeString();
            
            LDA #(line3 % 256)
            STA ZP.STRL
            LDA #(line3 / 256)
            STA ZP.STRH
            writeString();
            
            LDA #(line4 % 256)
            STA ZP.STRL
            LDA #(line4 / 256)
            STA ZP.STRH
            writeString();
            
            LDA #(line5 % 256)
            STA ZP.STRL
            LDA #(line5 / 256)
            STA ZP.STRH
            writeString();
            
            LDA #(line6 % 256)
            STA ZP.STRL
            LDA #(line6 / 256)
            STA ZP.STRH
            writeString();
            
            LDA #(line7 % 256)
            STA ZP.STRL
            LDA #(line7 / 256)
            STA ZP.STRH
            writeString();
            
            LDA #(line8 % 256)
            STA ZP.STRL
            LDA #(line8 / 256)
            STA ZP.STRH
            writeString();
            
            // Show progress dot
            LDA #'.'
            Serial.WriteChar();
            
            PLX
            DEX
            if (Z) { break; }
        }
        
        Print.NewLine();
        
        // Close and finalize file
        LDA #0x00  // Data file (not executable)
        File.EndSave();
        if (NC)
        {
            LDA #(error % 256)
            STA ZP.STRL
            LDA #(error / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        // Success!
        LDA #(done % 256)
        STA ZP.STRL
        LDA #(done / 256)
        STA ZP.STRH
        Print.String();
        
        // Show directory to confirm
        File.Dir();
    }
}
