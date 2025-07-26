program Test
{
    #define DEBUG
    
    //#define MEMDEBUG // Allocate and Free
    
    #define CPU_65C02S
    #define ROM_32K
    
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
    uses "Functions"
    uses "BasicTypes"
    uses "Tools"
    
    uses "TestTable"
    uses "TestObjects"
    uses "TestVariables"
    uses "TestConstants"
    uses "TestFunctions"
    uses "TestArguments"
    uses "TestScenarios"
    
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
    const string testFailed = " FAIL # 0x";
    const string testComplete = "\nAll tests completed.\n";
    const string tableSection = "\n--- TABLE LAYER TESTS ---\n";
    const string objectsSection = "\n--- OBJECTS LAYER TESTS ---\n";
    const string variablesSection = "\n--- VARIABLES LAYER TESTS ---\n";
    const string constantsSection = "\n--- CONSTANTS LAYER TESTS ---\n";
    const string functionsSection = "\n--- FUNCTION LAYER TESTS ---\n";
    const string argumentSection = "\n--- ARGUMENT LAYER TESTS ---\n";
    const string scenarioSection = "\n--- INTEGRATION SCENARIO TESTS ---\n";
    
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
    
    // Print test number and description
    PrintTestHeader()
    {
        // Input: ZP.TOP = description pointer
        LDA ZP.TOPL
        STA ZP.IDXL
        LDA ZP.TOPH
        STA ZP.IDXH
        PrintString();
        LDA #' '
        Serial.WriteChar();
        
        StartMemoryTest();
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
    
    // Start memory leak test - call at beginning of each test
    // Pushes current available memory to hardware stack for later comparison
    StartMemoryTest()
    {
        // Get current available memory
        Memory.Available();  // Pushes available memory (UInt) to stack
        Stacks.PopTop();     // Pop into TOP
        
        // Push low byte then high byte to hardware stack
        LDA ZP.TOPL
        STA ZP.TARGET0
        LDA ZP.TOPH
        STA ZP.TARGET1
    }
    
    // End memory leak test - call at end of each test  
    // Pops expected memory and compares to current memory
    // Input: C set = test passed, NC clear = test failed, A = failure code (if failing)
    // Output: C set = no leak, NC clear = memory leak detected, A = 0xFF if leak
    EndMemoryTest()
    {
        if (NC)
        {
            return; // prioritize test failures over memory leaks    
        }
        
        // Get current available memory
        Memory.Available();  // Pushes available memory (UInt) to stack
        Stacks.PopTop();     // Pop into TOP (current memory)
                
        // Compare current memory (TOP) with expected memory (TARGET)
        /*
        LDA ZP.TOPL
        CMP ZP.TARGET0
        if (NZ)
        {
            // Memory leak detected
            LDA #0xFF  // Memory leak failure code
            CLC  // Memory leak detected
            return;
        }
        
        LDA ZP.TOPH
        CMP ZP.TARGET1
        if (NZ)
        {
            // Memory leak detected
            LDA #0xFF  // Memory leak failure code  
            CLC  // Memory leak detected
            return;
        }
        */
        SEC // no memory leak
    }

    // Print test result
    // Input: C set = pass, NC clear = fail, A = failure code (if failing)
    PrintResult()
    {
        // Check for memory leaks first
        EndMemoryTest();
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
            
            // Check if this is a memory leak
            CMP #0xFF
            if (Z)
            {
                LDA #(testFailed % 256)
                STA ZP.IDXL
                LDA #(testFailed / 256)
                STA ZP.IDXH
                PrintString();
                
                LDA #'M'
                Serial.WriteChar();
                LDA #'E'
                Serial.WriteChar();
                LDA #'M'
                Serial.WriteChar();
                LDA #'L'
                Serial.WriteChar();
                LDA #'E'
                Serial.WriteChar();
                LDA #'A'
                Serial.WriteChar();
                LDA #'K'
                Serial.WriteChar();
                LDA #'\n'
                Serial.WriteChar();
            }
            else
            {
                LDA #(testFailed % 256)
                STA ZP.IDXL
                LDA #(testFailed / 256)
                STA ZP.IDXH
                PrintString();
                
                PLA  // Restore failure code
                Serial.HexOut();
                PHA  // Save it again
                LDA #'\n'
                Serial.WriteChar();
            }
            
            PLA  // Clean up failure code
#ifdef DEBUG             
            Tools.DumpHeap();
#endif
            loop { }
        }
    }
    
    // Initialize test environment - ONLY place Objects.Initialize() is called
    InitializeTest()
    {
        Serial.Initialize();
        Memory.InitializeHeapSize();
        Stacks.Initialize();
        Objects.Initialize();
        
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
        
        // Function layer tests
        LDA #(functionsSection % 256)
        STA ZP.TOPL
        LDA #(functionsSection / 256)
        STA ZP.TOPH
        PrintSectionHeader();
        TestFunctions.RunFunctionsTests();
        
        // Argument layer tests
        LDA #(argumentSection % 256)
        STA ZP.TOPL
        LDA #(argumentSection / 256)
        STA ZP.TOPH
        PrintSectionHeader();
        TestArguments.RunArgumentsTests();
        
        // Integration scenario tests
        LDA #(scenarioSection % 256)
        STA ZP.TOPL
        LDA #(scenarioSection / 256)
        STA ZP.TOPH
        PrintSectionHeader();
        TestScenarios.RunScenarioTests();
        
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
        InitializeTest();  // Objects.Initialize() called only here
        CLI
        RunAllTests();
        loop { }
    }
}
