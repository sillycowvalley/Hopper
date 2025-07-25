unit TestObjects
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Objects"
    uses "BasicTypes"
    
    // Private test data for Objects tests
    const string testName1 = "VAR1";
    const string testName2 = "COUNT";
    const string testName3 = "FLAG";
    const string testName4 = "TEMP";
    const string testName5 = "CONST1";
    
    // Allocate test token memory block
    // Returns address in ZP.SymbolTemp0/1 for use with Objects.Add()
    // MUST be called early before other variables are set up!
    allocateTestTokens()
    {
        // Allocate 16 bytes for test token data
        LDA #16
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();  // Returns address in ZP.IDX, munts everything
        
        // Store result in dedicated temporary slots
        LDA ZP.IDXL
        STA ZP.SymbolTemp0
        LDA ZP.IDXH
        STA ZP.SymbolTemp1
    }
    
    // Objects test descriptions
    const string objectsDesc1 = "Add symbol";
    const string objectsDesc2 = "Find symbol";
    const string objectsDesc3 = "Get symbol data";
    const string objectsDesc4 = "Set symbol value";
    const string objectsDesc5 = "Symbol type filtering";
    const string objectsDesc6 = "Destroy symbol table";
    
    // Test: Add symbol to Objects
    testAddSymbol()
    {
        LDA #(objectsDesc1 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc1 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST (before setting up other variables)
        allocateTestTokens();  // Result in ZP.SymbolTemp0/1
        
        // Add INT variable "COUNT" = 42 with tokens pointer
        LDA #(testName2 % 256)
        STA ZP.TOPL
        LDA #(testName2 / 256)
        STA ZP.TOPH
        
        // Pack symbolType|dataType: VARIABLE(1) in high nibble, INT(2) in low nibble
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #42
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.SymbolTemp0
        STA ZP.IDYL
        LDA ZP.SymbolTemp1
        STA ZP.IDYH
        
        Objects.Add();
        
        // Should succeed (C set)
        if (C)
        {
            Objects.Destroy();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0x20
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test: Find symbol by name
    testFindSymbol()
    {
        LDA #(objectsDesc2 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc2 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.SymbolTemp0/1
        
        // Add BIT variable "FLAG" = 1
        LDA #(testName3 % 256)
        STA ZP.TOPL
        LDA #(testName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.BIT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.SymbolTemp0
        STA ZP.IDYL
        LDA ZP.SymbolTemp1
        STA ZP.IDYH
        
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
            Objects.Destroy();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0x30
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test: Get symbol data
    testGetSymbolData()
    {
        LDA #(objectsDesc3 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc3 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST and save for later comparison
        allocateTestTokens();  // Result in ZP.SymbolTemp0/1
        
        // Add WORD variable "VAR1" = 1000
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #(1000 % 256)
        STA ZP.NEXTL
        LDA #(1000 / 256)
        STA ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.SymbolTemp0
        STA ZP.IDYL
        LDA ZP.SymbolTemp1
        STA ZP.IDYH
        
        Objects.Add();
        
        // Find and get data
        Objects.Find();
        if (NC)
        {
            LDA #0x40
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Objects.GetData();
        
        // Check data type (should be WORD = 4)
        LDA ZP.ACCL
        AND #0x0F  // Extract data type (low nibble)
        CMP #BasicType.WORD
        if (NZ)
        {
            LDA #0x41
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Check value (should be 1000)
        LDA ZP.NEXTL
        CMP #(1000 % 256)
        if (NZ)
        {
            LDA #0x42
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        LDA ZP.NEXTH
        CMP #(1000 / 256)
        if (NZ)
        {
            LDA #0x43
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Check tokens pointer (should match allocated address)
        LDA ZP.IDYL
        CMP ZP.SymbolTemp0  // Compare with saved address
        if (NZ)
        {
            LDA #0x44
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        LDA ZP.IDYH
        CMP ZP.SymbolTemp1
        if (NZ)
        {
            LDA #0x45
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Objects.Destroy();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test: Set symbol value
    testSetSymbolValue()
    {
        LDA #(objectsDesc4 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.SymbolTemp0/1
        
        // Add INT variable "TEMP" = 100
        LDA #(testName4 % 256)
        STA ZP.TOPL
        LDA #(testName4 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #100
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.SymbolTemp0
        STA ZP.IDYL
        LDA ZP.SymbolTemp1
        STA ZP.IDYH
        
        Objects.Add();
        Objects.Find();
        
        // Change value to 200
        LDA #200
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        Objects.SetValue();
        if (NC)
        {
            LDA #0x50
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Verify new value
        Objects.GetData();
        LDA ZP.NEXTL
        CMP #200
        if (NZ)
        {
            LDA #0x51
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Objects.Destroy();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test: Symbol type filtering
    testSymbolFiltering()
    {
        LDA #(objectsDesc5 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc5 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Add variable
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Objects.Add();
        
        // Add constant
        LDA #(testName2 % 256)
        STA ZP.TOPL
        LDA #(testName2 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #20
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Objects.Add();
        
        // Iterate looking for variables only
        LDA #SymbolType.VARIABLE
        STA ZP.ACCL
        Objects.IterateStart();
        
        if (NC)
        {
            LDA #0x60
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Should find the variable
        Objects.GetData();
        LDA ZP.ACCL
        AND #0xF0
        LSR LSR LSR LSR
        CMP #SymbolType.VARIABLE
        if (NZ)
        {
            LDA #0x61
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Objects.Destroy();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test: Destroy symbol table
    testDestroy()
    {
        LDA #(objectsDesc6 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc6 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
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
            LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
            STA ZP.ACCL
            LDA #42
            STA ZP.NEXTL
            STZ ZP.NEXTH
            STZ ZP.IDYL
            STZ ZP.IDYH
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
            SEC  // Pass - already clean, no additional cleanup needed
        }
        else
        {
            LDA #0x70
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Run all objects tests
    RunObjectsTests()
    {
        testAddSymbol();
        testFindSymbol();
        testGetSymbolData();
        testSetSymbolValue();
        testSymbolFiltering();
        testDestroy();
    }
}