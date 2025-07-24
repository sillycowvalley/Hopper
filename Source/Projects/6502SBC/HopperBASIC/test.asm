program Test
{
    #define DEBUG
    #define CPU_65C02S
    #define ROM_16K
    
    #define HOPPER_BASIC
    
    uses "/Source/Runtime/6502/Serial"
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "/Source/Runtime/6502/MemoryMap"
    uses "/Source/Runtime/6502/Utilities"
    uses "/Source/Runtime/6502/Stacks"
    
    uses "Table"
    uses "Objects"
    uses "BasicTypes"
    uses "Tools"
    
    // Test Table head
    const byte TableHeadLocation  = 0x3C;
    const byte TableHeadLocationL = 0x3C;
    const byte TableHeadLocationH = 0x3D;
    const byte TableNode          = 0x3E;
    const byte TableNodeL         = 0x3E;
    const byte TableNodeH         = 0x3F;
    
    // Test result tracking
    const string testHeader = "\n=== TABLE & OBJECTS COMPREHENSIVE TESTS ===\n";
    const string testPassed = " PASS\n";
    const string testFailed = " FAIL #";
    const string testComplete = "\nAll tests completed.\n";
    
    // Test data
    const string testName1 = "VAR1";
    const string testName2 = "COUNT";
    const string testName3 = "FLAG";
    const string testName4 = "TEMP";
    
    // Test descriptions
    const string desc1 = "Empty list operations";
    const string desc2 = "Add single node";
    const string desc3 = "Add multiple nodes";
    const string desc4 = "Traverse list";
    const string desc5 = "Delete first node";
    const string desc6 = "Clear entire list";
    const string desc7 = "Objects initialize";
    const string desc8 = "Add symbol";
    const string desc9 = "Find symbol";
    const string desc10 = "Get symbol data";
    const string desc11 = "Set symbol value";
    const string desc12 = "Symbol type filtering";
    const string desc13 = "Destroy symbol table";
    
    // Print null-terminated string using ZP.IDX
    printString()
    {
        LDY #0
        loop
        {
            LDA [ZP.IDX], Y
            if (Z) { break; }
            Serial.WriteChar();
            INY
        }
    }
    
    // Print test result
    // Input: C set = pass, C clear = fail, A = failure code (if failing)
    printResult()
    {
        if (C)
        {
            LDA #(testPassed % 256)
            STA ZP.IDXL
            LDA #(testPassed / 256)
            STA ZP.IDXH
            printString();
        }
        else
        {
            PHA  // Save failure code
            LDA #(testFailed % 256)
            STA ZP.IDXL
            LDA #(testFailed / 256)
            STA ZP.IDXH
            printString();
            
            PLA  // Restore failure code
            Serial.HexOut();
            LDA #'\n'
            Serial.WriteChar();
            PrintZP();
            Tools.DumpHeap();
        }
    }
    PrintZP()
    {
        // Debug output for our test variables
        PHA
        PHX
        
        LDA #'T'
        Serial.WriteChar();
        LDA #'H'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA TableHeadLocationH
        Serial.HexOut();
        LDA TableHeadLocationL
        Serial.HexOut();
        LDA #' '
        Serial.WriteChar();
        
        LDA #'T'
        Serial.WriteChar();
        LDA #'H'
        Serial.WriteChar();
        LDA #'X'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDX # TableHeadLocation
        LDA 0x01, X
        Serial.HexOut();
        LDA 0x00, X
        Serial.HexOut();
        LDA #' '
        Serial.WriteChar();
        
        LDA #'T'
        Serial.WriteChar();
        LDA #'N'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA TableNodeH
        Serial.HexOut();
        LDA TableNodeL
        Serial.HexOut();
        LDA #'\n'
        Serial.WriteChar();
        
        PLX
        PLA
    }
    
    // Print test number and description
    printTestHeader()
    {
        // Input: A = test number (ASCII), ZP.TOP = description pointer
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        LDA ZP.TOPL
        STA ZP.IDXL
        LDA ZP.TOPH
        STA ZP.IDXH
        printString();
        LDA #' '
        Serial.WriteChar();
    }
    
    // === TABLE LAYER TESTS ===
    
    // Test 1: Empty list operations
    testEmptyList()
    {
        LDA #'1'
        LDA #(desc1 % 256)
        STA ZP.TOPL
        LDA #(desc1 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        // Clear test list
        STZ TableHeadLocationL
        STZ TableHeadLocationH
        
        // GetFirst should return null
        LDX # TableHeadLocation
        Table.GetFirst();
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            SEC  // Pass
        }
        else
        {
            LDA #0x10
            CLC  // Fail
        }
        printResult();
    }
    
    // Test 2: Add single node to empty list
    testAddSingle()
    {
        LDA #'2'
        LDA #(desc2 % 256)
        STA ZP.TOPL
        LDA #(desc2 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        // Start with a clean list
        LDX # TableHeadLocation
        Table.Clear();
        
        // Add 10-byte node
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDX # TableHeadLocation
        Table.Add();
        
        // Should succeed (C set) and return valid address
        if (NC)
        {
            LDA #0x20
            CLC  // Fail
            printResult();
            return;
        }
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (NZ)
        {
            SEC  // Pass
        }
        else
        {
            LDA #0x21
            CLC  // Fail
        }
        printResult();
        
    }
    
    // Test 3: Add multiple nodes (builds linked list)
    testAddMultiple()
    {
        
        
        LDA #'3'
        LDA #(desc3 % 256)
        STA ZP.TOPL
        LDA #(desc3 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        // Start with a clean list
        LDX # TableHeadLocation
        Table.Clear();
        
        // Add first node
        LDA #8
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX # TableHeadLocation
        Table.Add();
        if (NC)
        {
            LDA #0x30
            CLC  // Fail
            printResult();
            return;
        }
        
        // Save first node address
        LDA ZP.IDXL
        STA TableNodeL
        LDA ZP.IDXH
        STA TableNodeH
        
        // Add second node  
        LDA #12
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #TableHeadLocationL
        Table.Add();
        if (NC)
        {
            LDA #0x31
            CLC  // Fail
            printResult();
            return;
        }
        
        // List head should now point to the second node we just added
        LDA TableHeadLocationL
        CMP ZP.IDXL
        if (NZ)
        {
            LDA #0x32
            CLC  // Fail
            printResult();
            return;
        }
        LDA TableHeadLocationH
        CMP ZP.IDXH
        if (NZ)
        {
            LDA #0x33
            CLC  // Fail
            printResult();
            return;
        }
        
        // Second node's next pointer should point to first node
        Table.GetNext();
        LDA ZP.IDXL
        CMP TableNodeL
        if (NZ)
        {
            LDA #0x34
            CLC  // Fail
            printResult();
            return;
        }
        LDA ZP.IDXH
        CMP TableNodeH
        if (NZ)
        {
            LDA #0x35
            CLC  // Fail
            printResult();
            return;
        }
        
        SEC  // Pass
        printResult();
    }
    
    // Test 4: Traverse linked list
    testTraverse()
    {
        LDA #'4'
        LDA #(desc4 % 256)
        STA ZP.TOPL
        LDA #(desc4 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        // Start with a clean list
        LDX # TableHeadLocation
        Table.Clear();
        
        // Add nodes (will be in reverse order: 3rd, 2nd, 1st)
        LDY #0
        loop
        {
            CPY #3
            if (Z) { break; }
            
            LDA #10
            STA ZP.ACCL
            STZ ZP.ACCH
            LDX # TableHeadLocation
            Table.Add();
            if (NC)
            {
                LDA #0x40
                CLC  // Fail
                printResult();
                return;
            }
            
            INY
        }
        
        // Count nodes by traversal
        LDX # TableHeadLocation
        Table.GetFirst();
        
        LDY #0  // Node count
        loop
        {
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; } // End of list
            
            INY
            Table.GetNext();
        }
        
        // Should have counted 3 nodes
        CPY #3
        if (Z)
        {
            SEC  // Pass
        }
        else
        {
            LDA #0x41
            CLC  // Fail
        }
        printResult();
    }
    
    // Test 5: Delete first node
    testDeleteFirst()
    {
        LDA #'5'
        LDA #(desc5 % 256)
        STA ZP.TOPL
        LDA #(desc5 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        // Start with a clean list
        LDX #TableHeadLocationL
        Table.Clear();
        
        // Add first node
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #TableHeadLocationL
        Table.Add();
        LDA ZP.IDXL
        STA TableNodeL
        LDA ZP.IDXH
        STA TableNodeH
        
        // Add second node (becomes new head)
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #TableHeadLocationL
        Table.Add();
        
        // Delete the current head (second node)
        LDX #TableHeadLocationL
        Table.Delete();
        
        // List head should now point to first node
        LDA TableHeadLocationL
        CMP TableNodeL
        if (NZ)
        {
            LDA #0x50
            CLC  // Fail
            printResult();
            return;
        }
        LDA TableHeadLocationH
        CMP TableNodeH
        if (NZ)
        {
            LDA #0x51
            CLC  // Fail
            printResult();
            return;
        }
        
        SEC  // Pass
        printResult();
    }
    
    // Test 6: Clear entire list
    testClearList()
    {
        LDA #'6'
        LDA #(desc6 % 256)
        STA ZP.TOPL
        LDA #(desc6 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        // Start with a clean list, then build multiple nodes
        LDX #TableHeadLocationL
        Table.Clear();
        
        LDY #0
        loop
        {
            CPY #5
            if (Z) { break; }
            
            LDA #8
            STA ZP.ACCL
            STZ ZP.ACCH
            LDX #TableHeadLocationL
            Table.Add();
            
            INY
        }
        
        // Clear the list
        LDX #TableHeadLocationL
        Table.Clear();
        
        // List should be empty
        LDA TableHeadLocationL
        ORA TableHeadLocationH
        if (Z)
        {
            SEC  // Pass
        }
        else
        {
            LDA #0x60
            CLC  // Fail
        }
        printResult();
    }
    
    // === OBJECTS LAYER TESTS ===
    
    // Test 7: Initialize Objects
    testObjectsInit()
    {
        LDA #'7'
        LDA #(desc7 % 256)
        STA ZP.TOPL
        LDA #(desc7 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        Objects.Initialize();
        
        LDA ZP.SymbolListL
        ORA ZP.SymbolListH
        if (Z)
        {
            SEC  // Pass
        }
        else
        {
            LDA #0x70
            CLC  // Fail
        }
        printResult();
    }
    
    // Test 8: Add symbol to Objects
    testAddSymbol()
    {
        LDA #'8'
        LDA #(desc8 % 256)
        STA ZP.TOPL
        LDA #(desc8 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        Objects.Initialize();
        
        // Add INT variable "COUNT" = 42
        LDA #(testName2 % 256)
        STA ZP.TOPL
        LDA #(testName2 / 256)
        STA ZP.TOPH
        
        // Pack symbolType|dataType: VARIABLE(1) in high nibble, INT(2) in low nibble
        LDA #((Objects.SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #42
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        Objects.Add();
        
        // Should succeed (C set)
        if (C)
        {
            SEC  // Pass
        }
        else
        {
            LDA #0x80
            CLC  // Fail
        }
        printResult();
    }
    
    // Test 9: Find symbol by name
    testFindSymbol()
    {
        LDA #'9'
        LDA #(desc9 % 256)
        STA ZP.TOPL
        LDA #(desc9 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        Objects.Initialize();
        
        // Add BIT variable "FLAG" = 1
        LDA #(testName3 % 256)
        STA ZP.TOPL
        LDA #(testName3 / 256)
        STA ZP.TOPH
        
        LDA #((Objects.SymbolType.VARIABLE << 4) | BasicType.BIT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        Objects.Add();
        
        // Now find it
        LDA #(testName3 % 256)
        STA ZP.TOPL
        LDA #(testName3 / 256)
        STA ZP.TOPH
        
        Objects.Find();
        
        // Should find it (C set)
        if (C)
        {
            SEC  // Pass
        }
        else
        {
            LDA #0x90
            CLC  // Fail
        }
        printResult();
    }
    
    // Test 10: Get symbol data
    testGetSymbolData()
    {
        LDA #'A'  // Test 10
        LDA #(desc10 % 256)
        STA ZP.TOPL
        LDA #(desc10 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        Objects.Initialize();
        
        // Add WORD variable "VAR1" = 1000
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        
        LDA #((Objects.SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #(1000 % 256)
        STA ZP.NEXTL
        LDA #(1000 / 256)
        STA ZP.NEXTH
        
        Objects.Add();
        
        // Find and get data
        Objects.Find();
        if (NC)
        {
            LDA #0xA0
            CLC  // Fail
            printResult();
            return;
        }
        
        Objects.GetData();
        
        // Check data type (should be WORD = 4)
        LDA ZP.ACCL
        AND #0x0F  // Extract data type (low nibble)
        CMP #BasicType.WORD
        if (NZ)
        {
            LDA #0xA1
            CLC  // Fail
            printResult();
            return;
        }
        
        // Check value (should be 1000)
        LDA ZP.NEXTL
        CMP #(1000 % 256)
        if (NZ)
        {
            LDA #0xA2
            CLC  // Fail
            printResult();
            return;
        }
        LDA ZP.NEXTH
        CMP #(1000 / 256)
        if (NZ)
        {
            LDA #0xA3
            CLC  // Fail
            printResult();
            return;
        }
        
        SEC  // Pass
        printResult();
    }
    
    // Test 11: Set symbol value
    testSetSymbolValue()
    {
        LDA #'B'  // Test 11
        LDA #(desc11 % 256)
        STA ZP.TOPL
        LDA #(desc11 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        Objects.Initialize();
        
        // Add INT variable "TEMP" = 100
        LDA #(testName4 % 256)
        STA ZP.TOPL
        LDA #(testName4 / 256)
        STA ZP.TOPH
        
        LDA #((Objects.SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #100
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        Objects.Add();
        Objects.Find();
        
        // Change value to 200
        LDA #200
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        Objects.SetValue();
        if (NC)
        {
            LDA #0xB0
            CLC  // Fail
            printResult();
            return;
        }
        
        // Verify new value
        Objects.GetData();
        LDA ZP.NEXTL
        CMP #200
        if (NZ)
        {
            LDA #0xB1
            CLC  // Fail
            printResult();
            return;
        }
        
        SEC  // Pass
        printResult();
    }
    
    // Test 12: Symbol type filtering
    testSymbolFiltering()
    {
        LDA #'C'  // Test 12
        LDA #(desc12 % 256)
        STA ZP.TOPL
        LDA #(desc12 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        Objects.Initialize();
        
        // Add variable
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        LDA #((Objects.SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        Objects.Add();
        
        // Add constant
        LDA #(testName2 % 256)
        STA ZP.TOPL
        LDA #(testName2 / 256)
        STA ZP.TOPH
        LDA #((Objects.SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #20
        STA ZP.NEXTL
        STZ ZP.NEXTH
        Objects.Add();
        
        // Iterate looking for variables only
        LDA #Objects.SymbolType.VARIABLE
        STA ZP.ACCL
        Objects.IterateStart();
        
        if (NC)
        {
            LDA #0xC0
            CLC  // Fail
            printResult();
            return;
        }
        
        // Should find the variable
        Objects.GetData();
        LDA ZP.ACCL
        AND #0xF0
        LSR LSR LSR LSR
        CMP #Objects.SymbolType.VARIABLE
        if (NZ)
        {
            LDA #0xC1
            CLC  // Fail
            printResult();
            return;
        }
        
        SEC  // Pass
        printResult();
    }
    
    // Test 13: Destroy symbol table
    testDestroy()
    {
        LDA #'D'  // Test 13
        LDA #(desc13 % 256)
        STA ZP.TOPL
        LDA #(desc13 / 256)
        STA ZP.TOPH
        printTestHeader();
        
        Objects.Initialize();
        
        // Add several symbols
        LDY #0
        loop
        {
            CPY #3
            if (Z) { break; }
            
            LDA #(testName1 % 256)
            STA ZP.TOPL
            LDA #(testName1 / 256)
            STA ZP.TOPH
            LDA #((Objects.SymbolType.VARIABLE << 4) | BasicType.INT)
            STA ZP.ACCL
            LDA #42
            STA ZP.NEXTL
            STZ ZP.NEXTH
            Objects.Add();
            
            INY
        }
        
        // Destroy the table
        Objects.Destroy();
        
        // Should be empty
        LDA ZP.SymbolListL
        ORA ZP.SymbolListH
        if (Z)
        {
            SEC  // Pass
        }
        else
        {
            LDA #0xD0
            CLC  // Fail
        }
        printResult();
    }
    
    // Initialize test environment
    initializeTest()
    {
        Serial.Initialize();
        Memory.InitializeHeapSize();
        Stacks.Initialize();
        
        STZ TableHeadLocationL
        STZ TableHeadLocationH
        
        STZ ZP.FLAGS
        SMB0 ZP.FLAGS
        
        LDA #(testHeader % 256)
        STA ZP.IDXL
        LDA #(testHeader / 256)
        STA ZP.IDXH
        printString();
    }
    
    // Run all tests
    runAllTests()
    {
        // Table layer tests
        testEmptyList();
        testAddSingle();
        testAddMultiple();
        testTraverse();
        testDeleteFirst();
        testClearList();
        
        // Objects layer tests
        /*
        testObjectsInit();
        testAddSymbol();
        testFindSymbol();
        testGetSymbolData();
        testSetSymbolValue();
        testSymbolFiltering();
        testDestroy();
        */
        
        LDA #(testComplete % 256)
        STA ZP.IDXL
        LDA #(testComplete / 256)
        STA ZP.IDXH
        printString();
    }
    
    // Interrupt handlers
    IRQ()
    {
        Serial.ISR();
    }
    
    NMI()
    {
        // Not used
    }
    
    // Main entry point
    Hopper()
    {
        SEI
        initializeTest();
        CLI
        runAllTests();
        loop { }
    }
}
