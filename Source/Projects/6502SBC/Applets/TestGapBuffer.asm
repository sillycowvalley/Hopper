program TestGapBuffer
{
    #define CPU_65C02S
    #define DEBUG
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Screen"
    uses "System/Memory"
    uses "System/Debug"
    uses "Editor/GapBuffer"
    
    const string title = "=== GapBuffer Unit Test ===\n\n";
    const string test1 = "1. Initialize buffer (8 bytes)\n";
    const string test2 = "2. Insert characters at start\n";
    const string test3 = "3. Move gap and insert\n";
    const string test4 = "4. Test GetCharAt\n";
    const string test5 = "5. Delete backward (backspace)\n";
    const string test6 = "6. Delete forward (delete)\n";
    const string test7 = "7. Force buffer growth\n";
    const string test8 = "8. Clear buffer\n";
    const string done = "\nAll tests complete!\n";
    
    const string passed = " PASS\n";
    const string failed = " FAIL\n";
    
    // Debug labels
    const string lblTextLen = "Len:";
    const string lblChar = "Char:";
    const string lblPos = "Pos:";
    
    
    displayText()
    {
        PHX
        PHY
        
        Print.String(); // Print the test description first
        
        LDA #'"'
        Print.Char();
        
        GapBuffer.GetTextLength();
        LDA GapBuffer.GapValueL
        STA ZP.IDYL  // Save length
        LDA GapBuffer.GapValueH
        STA ZP.IDYH

        STZ ZP.IDXL  // Position counter
        STZ ZP.IDXH
        
        loop
        {
            // Simple check: if position >= length, we're done
            // Compare high bytes first
            LDA ZP.IDXH
            CMP ZP.IDYH
            if (C)  // position.H >= length.H
            {
                if (NZ)  // position.H > length.H
                {
                    break;  // Definitely done
                }
                // High bytes equal, check low
                LDA ZP.IDXL
                CMP ZP.IDYL
                if (C)  // position.L >= length.L
                {
                    break;  // Done
                }
            }
            // Position < length, so continue
            
            // Get character at position
            LDA ZP.IDXL
            STA GapBuffer.GapValueL
            LDA ZP.IDXH
            STA GapBuffer.GapValueH
            GapBuffer.GetCharAt();
            
            // Print it
            if (Z)
            {
                LDA #'?'  // Show null as ?
            }
            Print.Char();
            
            // Next position
            INC ZP.IDXL
            if (Z) { INC ZP.IDXH }
        }
        
        LDA #'"'
        Print.Char();
        
        PLY
        PLX
    }
    
   Hopper()
{
    Screen.Clear();
    
    // Initialize 512-byte buffer (2 pages)
    LDA #0
    LDY #2
    GapBuffer.Initialize();
    
    // Insert just 10 characters
    LDX #10
    LDA #'0'
    loop
    {
        PHX
        PHA
        GapBuffer.InsertChar();
        PLA
        INC
        PLX
        DEX
        if (Z) { break; }
    }
    
    // Gap is at position 10, extends to 512
    // Buffer spans 2 pages
    
    Print.NewLine();
    LDA #'1'
    Print.Char();
    Print.Space();
    displayText();  // Should show "0123456789"
    Print.NewLine();
    
    // Move gap to position 0
    // This should copy 10 bytes from near start to near end of 512-byte buffer
    // If buffer starts at 0x131A, copying from 0x131A to ~0x1516 (different pages!)
    LDA #'2'
    Print.Char();
    Print.Space();
    
    STZ GapBuffer.GapValueL
    STZ GapBuffer.GapValueH
    GapBuffer.MoveGapTo();
    
    displayText();  // Should still show "0123456789"
    Print.NewLine();
    
    // Move gap to position 5
    // Should copy from high address back to low address (different pages)
    LDA #'3'
    Print.Char();
    Print.Space();
    
    LDA #5
    STA GapBuffer.GapValueL
    STZ GapBuffer.GapValueH
    GapBuffer.MoveGapTo();
    
    LDA #'X'
    GapBuffer.InsertChar();
    
    displayText();  // Should show "01234X56789"
    Print.NewLine();
    
    GapBuffer.Dispose();
    
    Print.NewLine();
    LDA #'D'
    Print.Char();
}    
       
}
