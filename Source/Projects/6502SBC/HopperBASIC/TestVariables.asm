unit TestVariables
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "Variables"
    uses "BasicTypes"
    
    // Private test data for Variables tests
    const string testName1 = "VAR1";
    const string testName2 = "COUNT";
    const string testName3 = "FLAG";
    const string testName4 = "TEMP";
    const string testName5 = "CONST1";
    const string testName6 = "INVALID";
    
    // Allocate test token memory block
    // Returns address in ZP.SymbolTemp0/1 for use with Variables.Declare()
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
    
    // Variables test descriptions
    const string variablesDesc1 = "Declare INT variable";
    const string variablesDesc2 = "Declare WORD constant";
    const string variablesDesc3 = "Find variable by name";
    const string variablesDesc4 = "Get variable value";
    const string variablesDesc5 = "Set variable value";
    const string variablesDesc6 = "Set constant value (should fail)";
    const string variablesDesc7 = "Get variable tokens";
    const string variablesDesc8 = "Iterate variables only";
    
    // Test: Declare INT variable
    testDeclareIntVariable()
    {
        LDA #(variablesDesc1 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc1 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.SymbolTemp0/1
        
        // Declare INT variable "COUNT" = 42
        LDA #(testName2 % 256)
        STA ZP.TOPL
        LDA #(testName2 / 256)
        STA ZP.TOPH
        
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
        
        Variables.Declare();
        
        if (C)
        {
            Variables.Clear();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0x20
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test: Declare WORD constant
    testDeclareWordConstant()
    {
        LDA #(variablesDesc2 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc2 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.SymbolTemp0/1
        
        // Declare WORD constant "CONST1" = 1000
        LDA #(testName5 % 256)
        STA ZP.TOPL
        LDA #(testName5 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
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
        
        Variables.Declare();
        
        if (C)
        {
            Variables.Clear();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0x30
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test: Find variable by name
    testFindVariableByName()
    {
        LDA #(variablesDesc3 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc3 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare variable first
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.BIT)
        STA ZP.ACCL
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Now find it with type filter
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        LDA #SymbolType.VARIABLE
        STA ZP.ACCL
        
        Variables.Find();
        
        if (C)
        {
            Variables.Clear();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0x40
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test: Get variable value
    testGetVariableValue()
    {
        LDA #(variablesDesc4 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare INT variable "TEMP" = 255
        LDA #(testName4 % 256)
        STA ZP.TOPL
        LDA #(testName4 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #255
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Find and get value
        STZ ZP.ACCL  // Any type
        Variables.Find();
        if (NC)
        {
            LDA #0x50
            CLC  // Fail - not found
            Test.PrintResult();
            return;
        }
        
        Variables.GetValue();
        if (NC)
        {
            LDA #0x51
            CLC  // Fail - GetValue failed
            Test.PrintResult();
            return;
        }
        
        // Check value and type
        LDA ZP.TOPL
        CMP #255
        if (NZ)
        {
            LDA #0x52
            CLC  // Fail - wrong value
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPT
        CMP #BasicType.INT
        if (NZ)
        {
            LDA #0x53
            CLC  // Fail - wrong type
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test: Set variable value
    testSetVariableValue()
    {
        LDA #(variablesDesc5 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc5 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare variable
        LDA #(testName2 % 256)
        STA ZP.TOPL
        LDA #(testName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Find it
        STZ ZP.ACCL
        Variables.Find();
        
        // Set new value
        LDA #200
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();
        if (NC)
        {
            LDA #0x60
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Verify new value
        Variables.GetValue();
        LDA ZP.TOPL
        CMP #200
        if (NZ)
        {
            LDA #0x61
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test: Set constant value (should fail)
    testSetConstantValue()
    {
        LDA #(variablesDesc6 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc6 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare constant
        LDA #(testName5 % 256)
        STA ZP.TOPL
        LDA #(testName5 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #50
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Find it
        STZ ZP.ACCL
        Variables.Find();
        
        // Try to set value (should fail)
        LDA #100
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();
        if (C)
        {
            LDA #0x70
            CLC  // Fail - should have failed
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass (correctly failed)
        Test.PrintResult();
    }
    
    // Test: Get variable tokens
    testGetVariableTokens()
    {
        LDA #(variablesDesc7 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc7 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST and save for later comparison
        allocateTestTokens();  // Result in ZP.SymbolTemp0/1
        
        // Declare variable with specific tokens pointer
        LDA #(testName1 % 256)
        STA ZP.TOPL
        LDA #(testName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCL
        LDA #(300 % 256)
        STA ZP.NEXTL
        LDA #(300 / 256)
        STA ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.SymbolTemp0
        STA ZP.IDYL
        LDA ZP.SymbolTemp1
        STA ZP.IDYH
        
        Variables.Declare();
        
        // Find and get tokens
        STZ ZP.ACCL
        Variables.Find();
        Variables.GetTokens();
        
        // Check tokens pointer matches allocated address
        LDA ZP.NEXTL
        CMP ZP.SymbolTemp0
        if (NZ)
        {
            LDA #0x80
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        LDA ZP.NEXTH
        CMP ZP.SymbolTemp1
        if (NZ)
        {
            LDA #0x81
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test: Iterate variables only
    testIterateVariablesOnly()
    {
        LDA #(variablesDesc8 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc8 / 256)
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
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Add constant
        LDA #(testName5 % 256)
        STA ZP.TOPL
        LDA #(testName5 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #20
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Iterate variables only
        Variables.IterateVariables();
        if (NC)
        {
            LDA #0x90
            CLC  // Fail - no variables found
            Test.PrintResult();
            return;
        }
        
        // Check type
        Variables.GetType();
        LDA ZP.ACCL
        AND #0xF0
        LSR LSR LSR LSR
        CMP #SymbolType.VARIABLE
        if (NZ)
        {
            LDA #0x91
            CLC  // Fail - wrong type
            Test.PrintResult();
            return;
        }
        
        // Should be no more variables
        Variables.IterateNext();
        if (C)
        {
            LDA #0x92
            CLC  // Fail - found more than one variable
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Run all variables tests
    RunVariablesTests()
    {
        testDeclareIntVariable();
        testDeclareWordConstant();
        testFindVariableByName();
        testGetVariableValue();
        testSetVariableValue();
        testSetConstantValue();
        testGetVariableTokens();
        testIterateVariablesOnly();
    }
}