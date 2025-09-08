program TestGapBuffer
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Screen"
    uses "System/Memory"
    uses "System/Debug"
    uses "Editor/GapBuffer"
    
    const string title = "=== GapBuffer Unit Test ===\n\n";
    const string test1 = "1. Initialize buffer (32 bytes)\n";
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
    
    
    // Label for dump
    const string dumpLabel = "= GAP BUFFER DUMP =";
    const string bufLabel = "Buf@";
    const string gapStartLabel = "GapS:";
    const string gapEndLabel = "GapE:";
    const string sizeLabel = "Size:";
    const string rawLabel = "Raw:";
    const string logLabel = "Log:";
    

    // Debug helper: Dump gap buffer state
    dumpGapBuffer()
    {
        PHX
        PHY
        
        Debug.Clear();
        
        // Header
        LDA #(dumpLabel % 256)
        STA ZP.STRL
        LDA #(dumpLabel / 256)
        STA ZP.STRH
        Debug.String();
        
        // Buffer address
        LDA #(bufLabel % 256)
        STA ZP.STRL
        LDA #(bufLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.gbBufferL
        STA ZP.ACCL
        LDA GapBuffer.gbBufferH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Gap start
        LDA #(gapStartLabel % 256)
        STA ZP.STRL
        LDA #(gapStartLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.gbGapStartL
        STA ZP.ACCL
        LDA GapBuffer.gbGapStartH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Gap end
        LDA #(gapEndLabel % 256)
        STA ZP.STRL
        LDA #(gapEndLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.gbGapEndL
        STA ZP.ACCL
        LDA GapBuffer.gbGapEndH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Buffer size
        LDA #(sizeLabel % 256)
        STA ZP.STRL
        LDA #(sizeLabel / 256)
        STA ZP.STRH
        LDA GapBuffer.gbBufferSizeL
        STA ZP.ACCL
        LDA GapBuffer.gbBufferSizeH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Dump first 16 raw bytes from buffer
        LDA #(rawLabel % 256)
        STA ZP.STRL
        LDA #(rawLabel / 256)
        STA ZP.STRH
        Debug.String();
        
        LDA GapBuffer.gbBufferL
        STA ZP.IDXL
        LDA GapBuffer.gbBufferH
        STA ZP.IDXH
        LDA # 32  // Dump 32 bytes
        Debug.DumpMemory();
        
        // Show what GetCharAt returns for first 8 positions
        LDA #(logLabel % 256)
        STA ZP.STRL
        LDA #(logLabel / 256)
        STA ZP.STRH
        Debug.String();
        
        LDX #8
        STZ ZP.IDYL  // Position counter
        loop
        {
            PHX
            
            // Get character at position IDYL
            LDA ZP.IDYL
            STA GapBuffer.GapValueL
            STZ GapBuffer.GapValueH
            GapBuffer.GetCharAt();
            
            // Show it
            Debug.Byte();
            
            INC ZP.IDYL
            PLX
            DEX
            if (Z) { break; }
        }
        
        PLY
        PLX
    }
    
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
        
        // Test 1: Initialize
        LDA #(test1 % 256)
        STA ZP.STRL
        LDA #(test1 / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #32  // 32 byte buffer
        LDY #0
        GapBuffer.Initialize();
        if (C)
        {
            LDA #(passed % 256)
            STA ZP.STRL
            LDA #(passed / 256)
            STA ZP.STRH
            Print.String();
            dumpGapBuffer();
        }
        else
        {
            LDA #(failed % 256)
            STA ZP.STRL
            LDA #(failed / 256)
            STA ZP.STRH
            Print.String();
            return;
        }
        
        dumpGapBuffer();
        
Serial.WaitForChar();
        
        // Test 2: Insert at start
        LDA #(test2 % 256)
        STA ZP.STRL
        LDA #(test2 / 256)
        STA ZP.STRH
        
        LDA #'H'
        GapBuffer.InsertChar();
        LDA #'e'
        GapBuffer.InsertChar();
        LDA #'l'
        GapBuffer.InsertChar();
        LDA #'l'
        GapBuffer.InsertChar();
        LDA #'o'
        GapBuffer.InsertChar();
        
        displayText();  // Shows: "Hello"
        Print.NewLine();
        
        
        dumpGapBuffer();
        
Serial.WaitForChar();        
        
        // Test 3: Move gap to position 2 and insert
        LDA #(test3 % 256)
        STA ZP.STRL
        LDA #(test3 / 256)
        STA ZP.STRH
        
        LDA #2
        STA GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.MoveGapTo();
        
        LDA #'X'
        GapBuffer.InsertChar();
        LDA #'Y'
        GapBuffer.InsertChar();
        
        displayText();  // Shows: "HeXYllo"
        Print.NewLine();
        
        dumpGapBuffer();

Serial.WaitForChar();                
                        
        // Test 4: GetCharAt various positions
        LDA #(test4 % 256)
        STA ZP.STRL
        LDA #(test4 / 256)
        STA ZP.STRH
        Print.String();
        

        
        // Get char at position 0 (should be 'H')
        STZ GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.GetCharAt();
        PHA
        LDA #(lblPos % 256)
        STA ZP.STRL
        LDA #(lblPos / 256)
        STA ZP.STRH
        LDA #0
        Debug.LabeledByte();
        LDA #(lblChar % 256)
        STA ZP.STRL
        LDA #(lblChar / 256)
        STA ZP.STRH
        PLA
        Debug.LabeledByte();
        
        // Get char at position 3 (should be 'Y')
        LDA #3
        STA GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.GetCharAt();
        PHA
        LDA #(lblPos % 256)
        STA ZP.STRL
        LDA #(lblPos / 256)
        STA ZP.STRH
        LDA #3
        Debug.LabeledByte();
        LDA #(lblChar % 256)
        STA ZP.STRL
        LDA #(lblChar / 256)
        STA ZP.STRH
        PLA
        Debug.LabeledByte();
        
Serial.WaitForChar();         
        
        // Test 5: Delete backward from position 5
        LDA #(test5 % 256)
        STA ZP.STRL
        LDA #(test5 / 256)
        STA ZP.STRH
        
        LDA #5
        STA GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.MoveGapTo();
        
        GapBuffer.DeleteChar();  // Delete 'l'
        GapBuffer.DeleteChar();  // Delete 'l'
        
        displayText();  // Shows: "HeXYo"
        Print.NewLine();
        
        
        dumpGapBuffer();
        
        
Serial.WaitForChar();        
        
        // Test 6: Delete forward from position 2
        LDA #(test6 % 256)
        STA ZP.STRL
        LDA #(test6 / 256)
        STA ZP.STRH
        
        LDA #2
        STA GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.MoveGapTo();
        
        GapBuffer.DeleteForward();  // Delete 'X'
        GapBuffer.DeleteForward();  // Delete 'Y'
        
        displayText();  // Shows: "Heo"
        Print.NewLine();
        
        
        dumpGapBuffer();
        
        
Serial.WaitForChar();        
        
        // Test 7: Force buffer growth (fill beyond 32 bytes)
        LDA #(test7 % 256)
        STA ZP.STRL
        LDA #(test7 / 256)
        STA ZP.STRH
        Print.String();
        
        // Move to end
        GapBuffer.GetTextLength();
        GapBuffer.MoveGapTo();
        
        // Add characters to force growth
        LDX #40  // More than initial 32 byte buffer
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
        
        // Show length to prove growth worked
        LDA #(lblTextLen % 256)
        STA ZP.STRL
        LDA #(lblTextLen / 256)
        STA ZP.STRH
        GapBuffer.GetTextLength();
        LDA GapBuffer.GapValueL
        STA ZP.ACCL
        LDA GapBuffer.GapValueH
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Show a few chars from the middle
        LDA #20
        STA GapBuffer.GapValueL
        STZ GapBuffer.GapValueH
        GapBuffer.GetCharAt();
        PHA
        LDA #(lblPos % 256)
        STA ZP.STRL
        LDA #(lblPos / 256)
        STA ZP.STRH
        LDA #20
        Debug.LabeledByte();
        LDA #(lblChar % 256)
        STA ZP.STRL
        LDA #(lblChar / 256)
        STA ZP.STRH
        PLA
        Debug.LabeledByte();
        
Serial.WaitForChar();
        
        // Test 8: Clear buffer
        LDA #(test8 % 256)
        STA ZP.STRL
        LDA #(test8 / 256)
        STA ZP.STRH
        
        GapBuffer.Clear();
        
        displayText();  // Shows: ""
        Print.NewLine();
        
        dumpGapBuffer();
        
        
        // Verify we can still insert after clear
        LDA #'O'
        GapBuffer.InsertChar();
        LDA #'K'
        GapBuffer.InsertChar();
        
        displayText();  // Shows: "OK"
        Print.NewLine();
        
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
