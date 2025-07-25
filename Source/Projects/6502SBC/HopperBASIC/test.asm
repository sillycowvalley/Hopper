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
    uses "Variables"
    uses "BasicTypes"
    uses "Tools"
    
    uses "TestTable"
    uses "TestObjects"
    uses "TestVariables"
    uses "TestConstants"
    
    // Test Table head locations
    const byte TableHeadLocation  = 0x3C;
    const byte TableHeadLocationL = 0x3C;
    const byte TableHeadLocationH = 0x3D;
    const byte TableNode          = 0x3E;
    const byte TableNodeL         = 0x3E;
    const byte TableNodeH         = 0x3F;
    
    // Test result tracking
    const string testHeader = "\n=== COMPREHENSIVE SYMBOL TABLE TESTS ===\n";
    const string testPassed = " PASS\n";
    const string testFailed = " FAIL #";
    const string testComplete = "\nAll tests completed.\n";
    const string tableSection = "\n--- TABLE LAYER TESTS ---\n";
    const string objectsSection = "\n--- OBJECTS LAYER TESTS ---\n";
    const string variablesSection = "\n--- VARIABLES LAYER TESTS ---\n";
    const string constantsSection = "\n--- CONSTANTS LAYER TESTS ---\n";
    
    // Print null-terminated string using ZP.IDX
    PrintString()
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
    PrintResult()
    {
        if (C)
        {
            LDA #(testPassed % 256)
            STA ZP.IDXL
            LDA #(testPassed / 256)
            STA ZP.IDXH
            PrintString();
        }
        else
        {
            PHA  // Save failure code
            LDA #(testFailed % 256)
            STA ZP.IDXL
            LDA #(testFailed / 256)
            STA ZP.IDXH
            PrintString();
            
            PLA  // Restore failure code
            Serial.HexOut();
            LDA #'\n'
            Serial.WriteChar();
            Tools.DumpHeap();
            loop { }
        }
    }
    
    // Print test number and description
    PrintTestHeader()
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
        PrintString();
        LDA #' '
        Serial.WriteChar();
    }
    
    // Print section header
    PrintSectionHeader()
    {
        // Input: ZP.TOP = section header pointer
        LDA ZP.TOPL
        STA ZP.IDXL
        LDA ZP.TOPH
        STA ZP.IDXH
        PrintString();
    }
    
    // Initialize test environment
    InitializeTest()
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
        PrintString();
    }
    
    // Run all tests
    RunAllTests()
    {
        // Table layer tests
        LDA #(tableSection % 256)
        STA ZP.TOPL
        LDA #(tableSection / 256)
        STA ZP.TOPH
        PrintSectionHeader();
        TestTable.RunTableTests();
        
        // Objects layer tests
        LDA #(objectsSection % 256)
        STA ZP.TOPL
        LDA #(objectsSection / 256)
        STA ZP.TOPH
        PrintSectionHeader();
        TestObjects.RunObjectsTests();
        
        // Variables layer tests
        LDA #(variablesSection % 256)
        STA ZP.TOPL
        LDA #(variablesSection / 256)
        STA ZP.TOPH
        PrintSectionHeader();
        TestVariables.RunVariablesTests();
        
        // Constants layer tests
        LDA #(constantsSection % 256)
        STA ZP.TOPL
        LDA #(constantsSection / 256)
        STA ZP.TOPH
        PrintSectionHeader();
        TestConstants.RunConstantsTests();
        
        LDA #(testComplete % 256)
        STA ZP.IDXL
        LDA #(testComplete / 256)
        STA ZP.IDXH
        PrintString();
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
        InitializeTest();
        CLI
        RunAllTests();
        loop { }
    }
}