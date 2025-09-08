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
        Debug.Initialize();
        Debug.Clear();
        
        // Title
        LDA #(title % 256)
        STA ZP.STRL
        LDA #(title / 256)
        STA ZP.STRH
        Print.String();
        
        // Test 1: Initialize with minimal buffer
        LDA #(test1 % 256)
        STA ZP.STRL
        LDA #(test1 / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #8  // Start with tiny 8 byte buffer
        LDY #0
        GapBuffer.Initialize();
        if (NC)
        {
            LDA #(failed % 256)
            STA ZP.STRL
            LDA #(failed / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        LDA #(passed % 256)
        STA ZP.STRL
        LDA #(passed / 256)
        STA ZP.STRH
        Print.String();
        
        // Test 2: Multiple growth cycles
        LDA #(test2 % 256)
        STA ZP.STRL
        LDA #(test2 / 256)
        STA ZP.STRH
        
        // Insert 50 chars to force growth from 8->16->32->64
        LDX #20
        LDA #'0'
        loop
        {
            PHX
            PHA
            GapBuffer.InsertChar();
            PLA
            INC
            CMP #'9'+1
            if (Z) { LDA #'0' }  // Cycle 0-9
            PLX
            DEX
            if (Z) { break; }
        }
        
        displayText();
        Print.NewLine();
        
        // Test 3: Clear and test random access
        LDA #(test3 % 256)
        STA ZP.STRL
        LDA #(test3 / 256)
        STA ZP.STRH
        
        GapBuffer.Clear();
        
        // Insert at position 0
        LDA #'A'
        GapBuffer.InsertChar();
        
        // Move to end and insert
        LDA #1
        STA GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.MoveGapTo();
        LDA #'Z'
        GapBuffer.InsertChar();
        
        // Insert in middle
        LDA #1
        STA GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.MoveGapTo();
        LDA #'M'
        GapBuffer.InsertChar();
        
        // Insert at beginning
        STZ GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.MoveGapTo();
        LDA #'*'
        GapBuffer.InsertChar();
        
        displayText();  // Should show "*AMZ"
        Print.NewLine();
        
        // Test 4: Simulate typing with corrections
        LDA #(test4 % 256)
        STA ZP.STRL
        LDA #(test4 / 256)
        STA ZP.STRH
        
        GapBuffer.Clear();
        
        // Type "The quik"
        LDA #'T'
        GapBuffer.InsertChar();
        LDA #'h'
        GapBuffer.InsertChar();
        LDA #'e'
        GapBuffer.InsertChar();
        LDA #' '
        GapBuffer.InsertChar();
        LDA #'q'
        GapBuffer.InsertChar();
        LDA #'u'
        GapBuffer.InsertChar();
        LDA #'i'
        GapBuffer.InsertChar();
        LDA #'c'
        GapBuffer.InsertChar();
        
        // Backspace and add 'k'
        LDA #'k'
        GapBuffer.InsertChar();
        
        displayText();  // Should show "The quick"
        Print.NewLine();
        
        // Test 5: Boundary tests
        LDA #(test5 % 256)
        STA ZP.STRL
        LDA #(test5 / 256)
        STA ZP.STRH
        
        // Try delete at end (should fail)
        GapBuffer.GetTextLength();
        GapBuffer.MoveGapTo();
        GapBuffer.Delete();
        
        // Try backspace at beginning (should fail)
        STZ GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.MoveGapTo();
        GapBuffer.Backspace();
        
        displayText();  // Should still show "The quick"
        Print.NewLine();
        
GapBuffer.Dump();
Serial.WaitForChar();        
        
        // Test 6: Large gap movements
        LDA #(test6 % 256)
        STA ZP.STRL
        LDA #(test6 / 256)
        STA ZP.STRH
        
        GapBuffer.Clear();
        
GapBuffer.Dump();
Serial.WaitForChar();        
        
        // Build string "ABCDEFGHIJ"
        LDX #10
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
        
GapBuffer.Dump();
Serial.WaitForChar();        
        
        // Jump around inserting numbers
        LDA #2
        STA GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.MoveGapTo();
        LDA #'1'
        GapBuffer.InsertChar();
        
        LDA #8
        STA GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.MoveGapTo();
        LDA #'2'
        GapBuffer.InsertChar();
        
        LDA #5
        STA GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.MoveGapTo();
        LDA #'3'
        GapBuffer.InsertChar();
        
        displayText();  // Should show "AB1CD3EFG2HIJ" 
        Print.NewLine();
        
GapBuffer.Dump();
Serial.WaitForChar();
        
        // Test 7: Delete everything
        LDA #(test7 % 256)
        STA ZP.STRL
        LDA #(test7 / 256)
        STA ZP.STRH
        
        // Move to end
        GapBuffer.GetTextLength();
        GapBuffer.MoveGapTo();
        
        // Delete everything with backspace
        loop
        {
            GapBuffer.Backspace();
            if (NC) { break; }
        }
        
        displayText();  // Should show ""
        Print.NewLine();
        
        // Test 8: Final verification
        LDA #(test8 % 256)
        STA ZP.STRL
        LDA #(test8 / 256)
        STA ZP.STRH
        
        // Insert "OK" to verify still works
        LDA #'O'
        GapBuffer.InsertChar();
        LDA #'K'
        GapBuffer.InsertChar();
        
        displayText();  // Should show "OK"
        Print.NewLine();
        
GapBuffer.Dump();
Serial.WaitForChar();
        
        // Clean up
        GapBuffer.Dispose();
        
        // Done
        LDA #(done % 256)
        STA ZP.STRL
        LDA #(done / 256)
        STA ZP.STRH
        Print.String();
    }
       
}
