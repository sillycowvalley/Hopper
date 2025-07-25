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
    const string testName6 = "FUNC1";
    const string testName7 = "MYFUNC";
    
    // Additional test names for similar name comparison
    const string varName1 = "VAR1";
    const string varName2 = "VAR2"; 
    const string varNameA = "VARA";
    const string varNameB = "VARB";
    
    // Additional test names for mixed iteration test
    const string constName1 = "CONST2";
    const string funcName1 = "FUNC2";
    
    // Objects test descriptions - existing
    const string objectsDesc1 = "Add symbol";
    const string objectsDesc2 = "Find symbol";
    const string objectsDesc3 = "Get symbol data";
    const string objectsDesc4 = "Set symbol value";
    const string objectsDesc5 = "Symbol type filtering";
    const string objectsDesc6 = "Destroy symbol table";
    const string objectsDesc7 = "Remove symbol";
    const string objectsDesc8 = "Get/Set tokens";
    const string objectsDesc9 = "Symbol not found";
    
    // New test descriptions
    const string objectsDesc10 = "Mixed symbol iteration";
    const string objectsDesc11 = "Similar name comparison";
    
    // Mock token data for testing
    const uint mockTokensAddr1 = 0x4000;
    const uint mockTokensAddr2 = 0x4100;
    const uint mockTokensAddr3 = 0x4200;
    
    // Test: Add symbol to Objects
    testAddSymbol()
    {
        LDA #(objectsDesc1 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc1 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Add INT variable "COUNT" = 42 with null tokens pointer
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
        
        // No tokens for basic Objects test
        STZ ZP.IDYL
        STZ ZP.IDYH
        
        LDX #ZP.VariablesList
        Objects.Add();
        
        // Should succeed (C set)
        if (C)
        {
            LDX #ZP.VariablesList
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
        
        // Add a symbol first
        LDA #(testName3 % 256)
        STA ZP.TOPL
        LDA #(testName3 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.BYTE)
        STA ZP.ACCL
        STZ ZP.ACCH
        LDA #255
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        LDX #ZP.VariablesList
        Objects.Add();
        
        if (NC)
        {
            LDA #0x30
            CLC  // Fail - couldn't add
            Test.PrintResult();
            return;
        }
        
        // Now find it
        LDA #(testName3 % 256)
        STA ZP.TOPL
        LDA #(testName3 / 256)
        STA ZP.TOPH
        LDX #ZP.VariablesList
        Objects.Find();
        
        if (C)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            SEC  // Pass
        }
        else
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0x31
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
        
        // Use null tokens pointer for this test
        STZ ZP.IDYL
        STZ ZP.IDYH
        
        LDX #ZP.VariablesList
        Objects.Add();
        
        // Find and get data
        LDX #ZP.VariablesList
        Objects.Find();
        if (NC)
        {
            LDA #0x40
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Objects.GetData();
        // NEW INTERFACE: ZP.ACC = symbolType|dataType, ZP.NEXT = tokens pointer, ZP.IDY = value
        
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
        
        // Check value (should be 1000) - now in ZP.IDY instead of ZP.NEXT
        LDA ZP.IDYL
        CMP #(1000 % 256)
        if (NZ)
        {
            LDA #0x42
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        LDA ZP.IDYH
        CMP #(1000 / 256)
        if (NZ)
        {
            LDA #0x43
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Check tokens pointer (should be null for this test) - now in ZP.NEXT instead of ZP.IDY
        LDA ZP.NEXTL
        ORA ZP.NEXTH
        if (NZ)
        {
            LDA #0x44
            CLC  // Fail - should be null
            Test.PrintResult();
            return;
        }
        
        LDX #ZP.VariablesList
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
        
        // No tokens for basic Objects test
        STZ ZP.IDYL
        STZ ZP.IDYH
        
        LDX #ZP.VariablesList
        Objects.Add();
        LDX #ZP.VariablesList
        Objects.Find();
        
        // Change value to 200
        LDA #200
        STA ZP.IDYL
        STZ ZP.IDYH
        
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
        // NEW INTERFACE: value now in ZP.IDY
        LDA ZP.IDYL
        CMP #200
        if (NZ)
        {
            LDA #0x51
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        LDX #ZP.VariablesList
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
        
        // Add a variable
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        LDA #42
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        LDX #ZP.VariablesList
        Objects.Add();
        
        if (NC)
        {
            LDA #0x60
            CLC
            Test.PrintResult();
            return;
        }
        
        // Add a constant
        LDA #(testName5 % 256)
        STA ZP.TOPL
        LDA #(testName5 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        LDA #100
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        LDX #ZP.VariablesList
        Objects.Add();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0x61
            CLC
            Test.PrintResult();
            return;
        }
        
        // Start iteration for variables only
        LDA #SymbolType.VARIABLE
        STA ZP.ACCL
        LDX #ZP.VariablesList
        Objects.IterateStart();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0x62
            CLC
            Test.PrintResult();
            return;
        }
        
        // Should find the variable
        Objects.GetData();
        LDA ZP.ACCL
        AND #0xF0
        CMP #(SymbolType.VARIABLE << 4)
        if (NZ)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0x63
            CLC
            Test.PrintResult();
            return;
        }
        
        LDX #ZP.VariablesList
        Objects.Destroy();
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test: Remove symbol
    testRemoveSymbol()
    {
        LDA #(objectsDesc7 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc7 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Add two symbols
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        LDA #42
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        LDX #ZP.VariablesList
        Objects.Add();
        
        if (NC)
        {
            LDA #0xD0
            CLC
            Test.PrintResult();
            return;
        }
        
        LDA #(testName2 % 256)
        STA ZP.TOPL
        LDA #(testName2 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        LDA #99
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        LDX #ZP.VariablesList
        Objects.Add();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xD1
            CLC
            Test.PrintResult();
            return;
        }
        
        // Remove first symbol
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        LDX #ZP.VariablesList
        Objects.Find();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xD2
            CLC
            Test.PrintResult();
            return;
        }
        
        LDX #ZP.VariablesList
        Objects.Remove();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xD3
            CLC
            Test.PrintResult();
            return;
        }
        
        // Verify first symbol is gone
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        LDX #ZP.VariablesList
        Objects.Find();
        
        if (C)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xD4  // Should not find it
            CLC
            Test.PrintResult();
            return;
        }
        
        // Verify second symbol still exists
        LDA #(testName2 % 256)
        STA ZP.TOPL
        LDA #(testName2 / 256)
        STA ZP.TOPH
        LDX #ZP.VariablesList
        Objects.Find();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xD5  // Should find it
            CLC
            Test.PrintResult();
            return;
        }
        
        LDX #ZP.VariablesList
        Objects.Destroy();
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test: Get/Set tokens
    testGetSetTokens()
    {
        LDA #(objectsDesc8 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc8 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Add symbol with tokens
        LDA #(testName6 % 256)
        STA ZP.TOPL
        LDA #(testName6 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.FUNCTION << 4) | BasicType.VOID)
        STA ZP.ACCL
        STZ ZP.ACCH
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        LDA #(mockTokensAddr1 % 256)
        STA ZP.IDYL
        LDA #(mockTokensAddr1 / 256)
        STA ZP.IDYH
        LDX #ZP.VariablesList
        Objects.Add();
        
        if (NC)
        {
            LDA #0xE0
            CLC
            Test.PrintResult();
            return;
        }
        
        // Find and get tokens
        LDA #(testName6 % 256)
        STA ZP.TOPL
        LDA #(testName6 / 256)
        STA ZP.TOPH
        LDX #ZP.VariablesList
        Objects.Find();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xE1
            CLC
            Test.PrintResult();
            return;
        }
        
        Objects.GetTokens();
        
        // Verify original tokens
        LDA ZP.IDYL
        CMP #(mockTokensAddr1 % 256)
        if (NZ)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xE2
            CLC
            Test.PrintResult();
            return;
        }
        
        LDA ZP.IDYH
        CMP #(mockTokensAddr1 / 256)
        if (NZ)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xE3
            CLC
            Test.PrintResult();
            return;
        }
        
        // Set new tokens
        LDA #(mockTokensAddr2 % 256)
        STA ZP.IDYL
        LDA #(mockTokensAddr2 / 256)
        STA ZP.IDYH
        Objects.SetTokens();
        
        // Get tokens again and verify
        Objects.GetTokens();
        
        LDA ZP.IDYL
        CMP #(mockTokensAddr2 % 256)
        if (NZ)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xE4
            CLC
            Test.PrintResult();
            return;
        }
        
        LDA ZP.IDYH
        CMP #(mockTokensAddr2 / 256)
        if (NZ)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xE5
            CLC
            Test.PrintResult();
            return;
        }
        
        LDX #ZP.VariablesList
        Objects.Destroy();
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test: Symbol not found scenarios
    testSymbolNotFound()
    {
        LDA #(objectsDesc9 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc9 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Try to find in empty table
        LDA #(testName2 % 256)
        STA ZP.TOPL
        LDA #(testName2 / 256)
        STA ZP.TOPH
        LDX #ZP.VariablesList
        Objects.Find();
        
        if (C)
        {
            LDA #0xE2  // Should not find it
            CLC
            Test.PrintResult();
            return;
        }
        
        // Try to remove non-existent symbol - first try to find it
        LDA #(testName2 % 256)
        STA ZP.TOPL
        LDA #(testName2 / 256)
        STA ZP.TOPH
        LDX #ZP.VariablesList
        Objects.Find();
        
        if (C)
        {
            // If found (shouldn't happen), try to remove it
            LDX #ZP.VariablesList
            Objects.Remove();
            if (C)
            {
                LDA #0xE3  // Should not have found/removed it
                CLC
                Test.PrintResult();
                return;
            }
        }
        
        LDX #ZP.VariablesList
        Objects.Destroy();
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test: Mixed symbol iteration
    testMixedSymbolIteration()
    {
        LDA #(objectsDesc10 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc10 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Add variable
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        LDA #42
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        LDX #ZP.VariablesList
        Objects.Add();
        
        if (NC)
        {
            LDA #0xF0
            CLC
            Test.PrintResult();
            return;
        }
        
        // Add constant
        LDA #(constName1 % 256)
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
        STA ZP.ACCL
        STZ ZP.ACCH
        LDA #100
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        LDX #ZP.VariablesList
        Objects.Add();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xF1
            CLC
            Test.PrintResult();
            return;
        }
        
        // Add function
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.FUNCTION << 4) | BasicType.VOID)
        STA ZP.ACCL
        STZ ZP.ACCH
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        LDA #(mockTokensAddr1 % 256)
        STA ZP.IDYL
        LDA #(mockTokensAddr1 / 256)
        STA ZP.IDYH
        LDX #ZP.VariablesList
        Objects.Add();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xF2
            CLC
            Test.PrintResult();
            return;
        }
        
        // Test iteration with VARIABLE filter
        LDA #SymbolType.VARIABLE
        STA ZP.ACCL
        LDX #ZP.VariablesList
        Objects.IterateStart();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xF3
            CLC
            Test.PrintResult();
            return;
        }
        
        // Should find variable
        Objects.GetData();
        LDA ZP.ACCL
        AND #0xF0
        CMP #(SymbolType.VARIABLE << 4)
        if (NZ)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xF4
            CLC
            Test.PrintResult();
            return;
        }
        
        // Try IterateNext for variables - should find none more
        Objects.IterateNext();
        if (C)
        {
            // Found another variable when should only be one
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xF5
            CLC
            Test.PrintResult();
            return;
        }
        
        // Test iteration with CONSTANT filter
        LDA #SymbolType.CONSTANT
        STA ZP.ACCL
        LDX #ZP.VariablesList
        Objects.IterateStart();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xF6
            CLC
            Test.PrintResult();
            return;
        }
        
        // Should find constant
        Objects.GetData();
        LDA ZP.ACCL
        AND #0xF0
        CMP #(SymbolType.CONSTANT << 4)
        if (NZ)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xF7
            CLC
            Test.PrintResult();
            return;
        }
        
        // Test iteration with FUNCTION filter
        LDA #SymbolType.FUNCTION
        STA ZP.ACCL
        LDX #ZP.VariablesList
        Objects.IterateStart();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xF8
            CLC
            Test.PrintResult();
            return;
        }
        
        // Should find function
        Objects.GetData();
        LDA ZP.ACCL
        AND #0xF0
        CMP #(SymbolType.FUNCTION << 4)
        if (NZ)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xF9
            CLC
            Test.PrintResult();
            return;
        }
        
        LDX #ZP.VariablesList
        Objects.Destroy();
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test: Similar name comparison
    testSimilarNameComparison()
    {
        LDA #(objectsDesc11 % 256)
        STA ZP.TOPL
        LDA #(objectsDesc11 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Add VAR1
        LDA #(varName1 % 256)  // "VAR1"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        LDX #ZP.VariablesList
        Objects.Add();
        
        if (NC)
        {
            LDA #0xA0
            CLC
            Test.PrintResult();
            return;
        }
        
        // Test finding VAR1 immediately - should get value 10
        LDA #(varName1 % 256)  // "VAR1"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        LDX #ZP.VariablesList
        Objects.Find();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xA4
            CLC
            Test.PrintResult();
            return;
        }
        
        Objects.GetData();
        LDA ZP.IDYL
        CMP #10
        if (NZ)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xA5
            CLC
            Test.PrintResult();
            return;
        }
        
        // Now add VAR2 to the same table
        LDA #(varName2 % 256)  // "VAR2"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        LDA #20
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        LDX #ZP.VariablesList
        Objects.Add();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xA1
            CLC
            Test.PrintResult();
            return;
        }
        
        // Test finding VAR2 - should get value 20
        LDA #(varName2 % 256)  // "VAR2"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        LDX #ZP.VariablesList
        Objects.Find();
        
        if (NC)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xA6
            CLC
            Test.PrintResult();
            return;
        }
        
        Objects.GetData();
        LDA ZP.IDYL
        CMP #20
        if (NZ)
        {
            LDX #ZP.VariablesList
            Objects.Destroy();
            LDA #0xA7
            CLC
            Test.PrintResult();
            return;
        }
        
        LDX #ZP.VariablesList
        Objects.Destroy();
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
            LDX #ZP.VariablesList
            Objects.Add();
            
            INY
        }
        
        // Destroy the table
        LDX #ZP.VariablesList
        Objects.Destroy();
        
        // Should be empty
        LDA ZP.VariablesListL
        ORA ZP.VariablesListH
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
        testRemoveSymbol();
        testGetSetTokens();
        testSymbolNotFound();
        testMixedSymbolIteration();
        testSimilarNameComparison();
        testDestroy();
    }
}
