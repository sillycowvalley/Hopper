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
    
    Print.NewLine();
    LDA #'='
    Print.Char();
    Print.Space();
    
    // Initialize with small buffer
    LDA #16
    LDY #0
    GapBuffer.Initialize();
    if (NC)
    {
        LDA #'F'
        Print.Char();
        return;
    }
    
    // Insert "ABCDEF" (6 chars)
    LDX #6
    LDA #'A'
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
    
    // Gap is now at position 6
    displayText();  // Should show "ABCDEF"
    Print.NewLine();
    
    // Test 1: Move gap to same position (6)
    LDA #'1'
    Print.Char();
    Print.Space();
    
    LDA #6
    STA GapBuffer.GapValueL
    STZ GapBuffer.GapValueH
    GapBuffer.MoveGapTo();
    
    displayText();  // Should still show "ABCDEF"
    Print.NewLine();
    
    // Test 2: Move gap left (to position 2)
    LDA #'2'
    Print.Char();
    Print.Space();
    
    LDA #2
    STA GapBuffer.GapValueL
    STZ GapBuffer.GapValueH
    GapBuffer.MoveGapTo();
    
    LDA #'X'
    GapBuffer.InsertChar();
    
    displayText();  // Should show "ABXCDEF"
    Print.NewLine();
    
    // Test 3: Move gap right (to position 5)
    // Gap is at position 3 after inserting X
    LDA #'3'
    Print.Char();
    Print.Space();
    
    LDA #5
    STA GapBuffer.GapValueL
    STZ GapBuffer.GapValueH
    GapBuffer.MoveGapTo();
    
    LDA #'Y'
    GapBuffer.InsertChar();
    
    displayText();  // Should show "ABXCDYEF"
    Print.NewLine();
    
    // Test 4: Move gap to beginning (position 0)
    LDA #'4'
    Print.Char();
    Print.Space();
    
    STZ GapBuffer.GapValueL
    STZ GapBuffer.GapValueH
    GapBuffer.MoveGapTo();
    
    LDA #'['
    GapBuffer.InsertChar();
    
    displayText();  // Should show "[ABXCDYEF"
    Print.NewLine();
    
    // Test 5: Move gap to end
    LDA #'5'
    Print.Char();
    Print.Space();
    
    GapBuffer.GetTextLength();
    GapBuffer.MoveGapTo();
    
    LDA #']'
    GapBuffer.InsertChar();
    
    displayText();  // Should show "[ABXCDYEF]"
    Print.NewLine();
    
    // Cleanup
    GapBuffer.Dispose();
    
    LDA #'D'
    Print.Char();
    Print.NewLine();
}
       
}
