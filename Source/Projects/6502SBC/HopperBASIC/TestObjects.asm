unit TestObjects
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Objects"
    uses "BasicTypes"
    uses "TestConstants"
    
    // Test 7: Initialize Objects
    testObjectsInit()
    {
        LDA #'7'
        LDA #(TestConstants.objectsDesc1 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.objectsDesc1 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Objects.Initialize();
        
        LDA ZP.SymbolListL
        ORA ZP.SymbolListH
        if (Z)
        {
            Objects.Destroy();
            SEC  // Pass
        }
        else
        {
            LDA #0x10
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 8: Add symbol to Objects
    testAddSymbol()
    {
        LDA #'8'
        LDA #(TestConstants.objectsDesc2 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.objectsDesc2 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Objects.Initialize();
        
        // Add INT variable "COUNT" = 42 with tokens pointer
        LDA #(TestConstants.testName2 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName2 / 256)
        STA ZP.TOPH
        
        // Pack symbolType|dataType: VARIABLE(1) in high nibble, INT(2) in low nibble
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #42
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        // Dummy tokens pointer
        LDA #(TestConstants.testTokens1 % 256)
        STA ZP.IDYL
        LDA #(TestConstants.testTokens1 / 256)
        STA ZP.IDYH
        
        Objects.Add();
        
        // Should succeed (C set)
        if (C)
        {
            Objects.Destroy();
            SEC  // Pass
        }
        else
        {
            LDA #0x20
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 9: Find symbol by name
    testFindSymbol()
    {
        LDA #'9'
        LDA #(TestConstants.objectsDesc3 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.objectsDesc3 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Objects.Initialize();
        
        // Add BIT variable "FLAG" = 1
        LDA #(TestConstants.testName3 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.BIT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        // Dummy tokens pointer
        LDA #(TestConstants.testTokens2 % 256)
        STA ZP.IDYL
        LDA #(TestConstants.testTokens2 / 256)
        STA ZP.IDYH
        
        Objects.Add();
        
        // Now find it
        LDA #(TestConstants.testName3 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName3 / 256)
        STA ZP.TOPH
        
        Objects.Find();
        
        // Should find it (C set)
        if (C)
        {
            Objects.Destroy();
            SEC  // Pass
        }
        else
        {
            LDA #0x30
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 10: Get symbol data
    testGetSymbolData()
    {
        LDA #'A'  // Test 10
        LDA #(TestConstants.objectsDesc4 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.objectsDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Objects.Initialize();
        
        // Add WORD variable "VAR1" = 1000
        LDA #(TestConstants.testName1 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #(1000 % 256)
        STA ZP.NEXTL
        LDA #(1000 / 256)
        STA ZP.NEXTH
        
        // Test tokens pointer
        LDA #(TestConstants.testTokens3 % 256)
        STA ZP.IDYL
        LDA #(TestConstants.testTokens3 / 256)
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
        
        // Check tokens pointer (should match testTokens3)
        LDA ZP.IDYL
        CMP #(TestConstants.testTokens3 % 256)
        if (NZ)
        {
            LDA #0x44
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        LDA ZP.IDYH
        CMP #(TestConstants.testTokens3 / 256)
        if (NZ)
        {
            LDA #0x45
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Objects.Destroy();
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 11: Set symbol value
    testSetSymbolValue()
    {
        LDA #'B'  // Test 11
        LDA #(TestConstants.objectsDesc5 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.objectsDesc5 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Objects.Initialize();
        
        // Add INT variable "TEMP" = 100
        LDA #(TestConstants.testName4 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName4 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #100
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        // Dummy tokens pointer
        LDA #(TestConstants.testTokens1 % 256)
        STA ZP.IDYL
        LDA #(TestConstants.testTokens1 / 256)
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
        
        Objects.Destroy();        
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 12: Symbol type filtering
    testSymbolFiltering()
    {
        LDA #'C'  // Test 12
        LDA #(TestConstants.objectsDesc6 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.objectsDesc6 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Objects.Initialize();
        
        // Add variable
        LDA #(TestConstants.testName1 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName1 / 256)
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
        LDA #(TestConstants.testName2 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName2 / 256)
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
        Objects.Destroy();
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 13: Destroy symbol table
    testDestroy()
    {
        LDA #'D'  // Test 13
        LDA #(TestConstants.objectsDesc7 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.objectsDesc7 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Objects.Initialize();
        
        // Add several symbols
        LDY #0
        loop
        {
            CPY #3
            if (Z) { break; }
            
            LDA #(TestConstants.testName1 % 256)
            STA ZP.TOPL
            LDA #(TestConstants.testName1 / 256)
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
            SEC  // Pass
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
        testObjectsInit();
        testAddSymbol();
        testFindSymbol();
        testGetSymbolData();
        testSetSymbolValue();
        testSymbolFiltering();
        testDestroy();
    }
}