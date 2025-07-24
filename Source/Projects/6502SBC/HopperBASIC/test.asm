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
    
    // Test working variables (using BASIC zero page range 0x30-0x4F)
    const byte TEST_RESULT_L      = 0x3C;  // Result address low
    const byte TEST_RESULT_H      = 0x3D;  // Result address high
    const byte TEST_EXPECTED_L    = 0x3E;  // Expected address low
    const byte TEST_EXPECTED_H    = 0x3F;  // Expected address high
    const byte TEST_VALUE_L       = 0x40;  // Test value low
    const byte TEST_VALUE_H       = 0x41;  // Test value high
    const byte TEST_TYPE          = 0x42;  // Test type storage
    const byte TEST_DATA_TYPE     = 0x43;  // Test data type storage
    
    // Test result tracking
    const string testHeader = "\n=== TABLE & OBJECTS TESTS ===\n";
    const string testPassed = " PASS\n";
    const string testFailed = " FAIL\n";
    const string testComplete = "\nAll tests completed.\n";
    
    // Test data
    const string testName1 = "VAR1";
    const string testName2 = "COUNT";
    const string testName3 = "FLAG";
    
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
    
    // Print PASS result
    printPass()
    {
        LDA #(testPassed % 256)
        STA ZP.IDXL
        LDA #(testPassed / 256)
        STA ZP.IDXH
        printString();
    }
    
    // Print FAIL result
    printFail()
    {
        LDA #(testFailed % 256)
        STA ZP.IDXL
        LDA #(testFailed / 256)
        STA ZP.IDXH
        printString();
    }
    
    // Test 1: Create empty list
    testCreateEmpty()
    {
        LDA #'1'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        Table.Create();
        
        // Should return empty list (0x0000)
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (Z)
        {
            printPass();
        }
        else
        {
            printFail();
        }
    }
    
    // Test 2: Add single node
    testAddSingle()
    {
        LDA #'2'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        Table.Create();
        
        // Add a 10-byte node
        LDA #10
        STA ZP.LLENGTHL
        STZ ZP.LLENGTHH
        
        Table.Add();
        
        // Should get valid address
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (NZ)
        {
            printPass();
        }
        else
        {
            printFail();
        }
    }
    
    // Test 3: Find node by name (should not find wrong name)
    testFindNode()
    {
        LDA #'3'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        Table.Create();
        
        // Add node and put "TEST" at name offset
        LDA #10
        STA ZP.LLENGTHL
        STZ ZP.LLENGTHH
        Table.Add();
        
        // Write "TEST" at offset 6
        CLC
        LDA ZP.LCURRENTL
        ADC #6
        STA ZP.IDXL
        LDA ZP.LCURRENTH
        ADC #0
        STA ZP.IDXH
        
        LDY #0
        LDA #'T'
        STA [ZP.IDX], Y
        INY
        LDA #'E'
        STA [ZP.IDX], Y
        INY
        LDA #'S'
        STA [ZP.IDX], Y
        INY
        LDA #'T'
        STA [ZP.IDX], Y
        INY
        LDA #0
        STA [ZP.IDX], Y
        
        // Search for wrong name "VAR1"
        LDA #(testName1 % 256)
        STA ZP.FITEML
        LDA #(testName1 / 256)
        STA ZP.FITEMH
        LDA #6
        STA ZP.LCOUNTL
        STZ ZP.LCOUNTH
        
        Table.Find();
        
        // Should not find it (return 0x0000)
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (Z)
        {
            printPass();
        }
        else
        {
            printFail();
        }
    }
    
    // Test 4: Remove first node from two-node list
    testRemoveNode()
    {
        LDA #'4'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        Table.Create();
        
        // Add first node
        LDA #10
        STA ZP.LLENGTHL
        STZ ZP.LLENGTHH
        Table.Add();
        
        // Save first node address
        LDA ZP.LCURRENTL
        STA TEST_EXPECTED_L
        LDA ZP.LCURRENTH
        STA TEST_EXPECTED_H
        
        // Add second node (becomes new head)
        LDA #10
        STA ZP.LLENGTHL
        STZ ZP.LLENGTHH
        Table.Add();
        
        // Remove the head node (second node)
        LDA ZP.LCURRENTL
        STA ZP.FITEML
        LDA ZP.LCURRENTH
        STA ZP.FITEMH
        
        Table.Remove();
        
        // Should now point to first node
        LDA ZP.LCURRENTL
        CMP TEST_EXPECTED_L
        if (Z)
        {
            LDA ZP.LCURRENTH
            CMP TEST_EXPECTED_H
            if (Z)
            {
                printPass();
            }
            else
            {
                printFail();
            }
        }
        else
        {
            printFail();
        }
    }
    
    // Test 5: Clear entire list
    testClearList()
    {
        LDA #'5'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        Table.Create();
        
        // Add a node
        LDA #10
        STA ZP.LLENGTHL
        STZ ZP.LLENGTHH
        Table.Add();
        
        // Clear the list
        Table.Clear();
        
        // Should be empty
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (Z)
        {
            printPass();
        }
        else
        {
            printFail();
        }
    }
    
    // Test 6: Initialize objects
    testObjects()
    {
        LDA #'6'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        Objects.Initialize();
        
        // Should be empty
        LDA ZP.SymbolListL
        ORA ZP.SymbolListH
        if (Z)
        {
            printPass();
        }
        else
        {
            printFail();
        }
    }
    
    // Test 7: Add object
    testAddObject()
    {
        LDA #'7'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        Objects.Initialize();
        
        // Add INT variable "COUNT" = 42
        LDA #(testName2 % 256)
        STA ZP.FITEML
        LDA #(testName2 / 256)
        STA ZP.FITEMH
        LDA #Objects.ObjectType.VARIABLE
        STA ZP.LTYPE
        LDA #BasicType.INT
        STA ZP.LITYPE
        LDA #42
        STA ZP.LCOUNTL
        STZ ZP.LCOUNTH
        
        Objects.Add();
        
        // Should get valid address
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (NZ)
        {
            printPass();
        }
        else
        {
            printFail();
        }
    }
    
    // Test 8: Lookup object
    testLookupObject()
    {
        LDA #'8'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        Objects.Initialize();
        
        // Add BIT variable "FLAG" = 1
        LDA #(testName3 % 256)
        STA ZP.FITEML
        LDA #(testName3 / 256)
        STA ZP.FITEMH
        LDA #Objects.ObjectType.VARIABLE
        STA ZP.LTYPE
        LDA #BasicType.BIT
        STA ZP.LITYPE
        LDA #1
        STA ZP.LCOUNTL
        STZ ZP.LCOUNTH
        
        Objects.Add();
        
        // Lookup the same object
        LDA #(testName3 % 256)
        STA ZP.FITEML
        LDA #(testName3 / 256)
        STA ZP.FITEMH
        
        Objects.Lookup();
        
        // Should find it
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (Z)
        {
            printFail();
            return;
        }
        
        // Verify the data
        Objects.GetData();
        
        // Check object type
        LDA ZP.LTYPE
        CMP #Objects.ObjectType.VARIABLE
        if (NZ)
        {
            printFail();
            return;
        }
        
        // Check data type
        LDA ZP.LITYPE
        CMP #BasicType.BIT
        if (NZ)
        {
            printFail();
            return;
        }
        
        // Check value
        LDA ZP.LCOUNTL
        CMP #1
        if (NZ)
        {
            printFail();
            return;
        }
        LDA ZP.LCOUNTH
        if (NZ)
        {
            printFail();
            return;
        }
        
        printPass();
    }
    
    // Test 9: Remove object
    testRemoveObject()
    {
        LDA #'9'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        Objects.Initialize();
        
        // Add first object "VAR1"
        LDA #(testName1 % 256)
        STA ZP.FITEML
        LDA #(testName1 / 256)
        STA ZP.FITEMH
        LDA #Objects.ObjectType.VARIABLE
        STA ZP.LTYPE
        LDA #BasicType.WORD
        STA ZP.LITYPE
        LDA #100
        STA ZP.LCOUNTL
        STZ ZP.LCOUNTH
        
        Objects.Add();
        
        // Add second object "COUNT"
        LDA #(testName2 % 256)
        STA ZP.FITEML
        LDA #(testName2 / 256)
        STA ZP.FITEMH
        LDA #Objects.ObjectType.VARIABLE
        STA ZP.LTYPE
        LDA #BasicType.INT
        STA ZP.LITYPE
        LDA #200
        STA ZP.LCOUNTL
        STZ ZP.LCOUNTH
        
        Objects.Add();
        
        // Remove "VAR1"
        LDA #(testName1 % 256)
        STA ZP.FITEML
        LDA #(testName1 / 256)
        STA ZP.FITEMH
        
        Objects.Remove();
        if (NZ)
        {
            printFail();
            return;
        }
        
        // "VAR1" should be gone
        LDA #(testName1 % 256)
        STA ZP.FITEML
        LDA #(testName1 / 256)
        STA ZP.FITEMH
        
        Objects.Lookup();
        
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (NZ)
        {
            printFail();
            return;
        }
        
        // "COUNT" should still be there
        LDA #(testName2 % 256)
        STA ZP.FITEML
        LDA #(testName2 / 256)
        STA ZP.FITEMH
        
        Objects.Lookup();
        
        LDA ZP.LCURRENTL
        ORA ZP.LCURRENTH
        if (Z)
        {
            printFail();
            return;
        }
        
        printPass();
    }
    
    // Test 10: Set object value
    testSetValue()
    {
        LDA #'A'
        Serial.WriteChar();
        LDA #':'
        Serial.WriteChar();
        LDA #' '
        Serial.WriteChar();
        
        Objects.Initialize();
        
        // Add WORD variable "COUNT" = 500
        LDA #(testName2 % 256)
        STA ZP.FITEML
        LDA #(testName2 / 256)
        STA ZP.FITEMH
        LDA #Objects.ObjectType.VARIABLE
        STA ZP.LTYPE
        LDA #BasicType.WORD
        STA ZP.LITYPE
        LDA #(500 % 256)
        STA ZP.LCOUNTL
        LDA #(500 / 256)
        STA ZP.LCOUNTH
        
        Objects.Add();
        
        // Look it up
        LDA #(testName2 % 256)
        STA ZP.FITEML
        LDA #(testName2 / 256)
        STA ZP.FITEMH
        
        Objects.Lookup();
        
        // Change value to 1000
        LDA #(1000 % 256)
        STA ZP.LCOUNTL
        LDA #(1000 / 256)
        STA ZP.LCOUNTH
        
        Objects.SetValue();
        if (NZ)
        {
            printFail();
            return;
        }
        
        // Verify new value
        Objects.GetData();
        
        LDA ZP.LCOUNTL
        CMP #(1000 % 256)
        if (NZ)
        {
            printFail();
            return;
        }
        LDA ZP.LCOUNTH
        CMP #(1000 / 256)
        if (NZ)
        {
            printFail();
            return;
        }
        
        printPass();
    }
    
    // Initialize test environment
    initializeTest()
    {
        Serial.Initialize();
        Memory.InitializeHeapSize();
        Stacks.Initialize();
        
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
        testCreateEmpty();
        testAddSingle();
        testFindNode();
        testRemoveNode();
        testClearList();
        testObjects();
        testAddObject();
        testLookupObject();
        testRemoveObject();
        testSetValue();
        
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
