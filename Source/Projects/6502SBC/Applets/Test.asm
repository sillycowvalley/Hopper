program TestDebugComplete
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Screen"
    uses "System/Memory"
    uses "System/Debug"
    
    const string title = "=== Debug Unit Test ===\n";
    const string test1 = "1. Testing String output\n";
    const string test2 = "2. Testing Byte output\n";
    const string test3 = "3. Testing Word output\n";
    const string test4 = "4. Testing Labeled values\n";
    const string test5 = "5. Testing Memory dump\n";
    const string test6 = "6. Testing Disable/Enable\n";
    const string test7 = "7. Testing row wrap\n";
    const string done = "Tests complete!\n";
    
    // Debug strings
    const string dbg1 = "Debug line 1";
    const string dbg2 = "Debug line 2";
    const string dbg3 = "Disabled?";
    const string dbg4 = "Re-enabled";
    const string label1 = "Byte:";
    const string label2 = "Word:";
    const string label3 = "Addr:";
    
    // Test data
    const byte[] testData = { 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48 };
    
    Hopper()
    {
        Screen.Clear();
        
        // Initialize debug
        Debug.Initialize();
        Debug.Clear();
        
        // Title
        LDA #(title % 256)
        STA ZP.STRL
        LDA #(title / 256)
        STA ZP.STRH
        Print.String();
        
        // Test 1: String output
        LDA #(test1 % 256)
        STA ZP.STRL
        LDA #(test1 / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(dbg1 % 256)
        STA ZP.STRL
        LDA #(dbg1 / 256)
        STA ZP.STRH
        Debug.String();
        
        LDA #(dbg2 % 256)
        STA ZP.STRL
        LDA #(dbg2 / 256)
        STA ZP.STRH
        Debug.String();
        
        // Test 2: Byte output
        LDA #(test2 % 256)
        STA ZP.STRL
        LDA #(test2 / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #0x55
        Debug.Byte();
        LDA #0xAA
        Debug.Byte();
        LDA #0xFF
        Debug.Byte();
        
        // Test 3: Word output
        LDA #(test3 % 256)
        STA ZP.STRL
        LDA #(test3 / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #0x34
        STA ZP.ACCL
        LDA #0x12
        STA ZP.ACCH
        Debug.Word();
        
        LDA #0xCD
        STA ZP.ACCL
        LDA #0xAB
        STA ZP.ACCH
        Debug.Word();
        
        // Test 4: Labeled values
        LDA #(test4 % 256)
        STA ZP.STRL
        LDA #(test4 / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(label1 % 256)
        STA ZP.STRL
        LDA #(label1 / 256)
        STA ZP.STRH
        LDA #0x42
        Debug.LabeledByte();
        
        LDA #(label2 % 256)
        STA ZP.STRL
        LDA #(label2 / 256)
        STA ZP.STRH
        LDA #0x78
        STA ZP.ACCL
        LDA #0x56
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Test 5: Memory dump
        LDA #(test5 % 256)
        STA ZP.STRL
        LDA #(test5 / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(testData % 256)
        STA ZP.IDXL
        LDA #(testData / 256)
        STA ZP.IDXH
        LDA #8
        Debug.DumpMemory();
        
        // Test 6: Disable/Enable
        LDA #(test6 % 256)
        STA ZP.STRL
        LDA #(test6 / 256)
        STA ZP.STRH
        Print.String();
        
        Debug.Disable();
        
        // This should NOT appear
        LDA #(dbg3 % 256)
        STA ZP.STRL
        LDA #(dbg3 / 256)
        STA ZP.STRH
        Debug.String();
        
        Debug.Enable();
        
        // This should appear
        LDA #(dbg4 % 256)
        STA ZP.STRL
        LDA #(dbg4 / 256)
        STA ZP.STRH
        Debug.String();
        
        // Test 7: Row wrap (fill remaining rows)
        LDA #(test7 % 256)
        STA ZP.STRL
        LDA #(test7 / 256)
        STA ZP.STRH
        Print.String();
        
        // Output many lines to test wrap
        LDX # 40  // More than screen height
        loop
        {
            PHX
            TXA
            Debug.Byte();
            PLX
            DEX
            if (Z) { break; }
        }
        
        // Test labeled values after wrap
        LDA #(label3 % 256)
        STA ZP.STRL
        LDA #(label3 / 256)
        STA ZP.STRH
        LDA #0xBE
        STA ZP.ACCL
        LDA #0xEF
        STA ZP.ACCH
        Debug.LabeledWord();
        
        // Done
        LDA #0
        LDY #22
        Screen.GotoXY();
        
        LDA #(done % 256)
        STA ZP.STRL
        LDA #(done / 256)
        STA ZP.STRH
        Print.String();
    }
}
