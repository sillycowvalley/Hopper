unit TestVariables
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Variables"
    uses "BasicTypes"
    uses "TestConstants"
    
    // Test 14: Variables initialize
    testVariablesInit()
    {
        LDA #'E'  // Test 14
        LDA #(TestConstants.variablesDesc1 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.variablesDesc1 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Variables.Initialize();
        
        // Should have empty symbol table
        LDA ZP.SymbolListL
        ORA ZP.SymbolListH
        if (Z)
        {
            SEC  // Pass
        }
        else
        {
            LDA #0x10
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 15: Declare INT variable
    testDeclareIntVariable()
    {
        LDA #'F'  // Test 15
        LDA #(TestConstants.variablesDesc2 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.variablesDesc2 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Variables.Initialize();
        
        // Declare INT variable "COUNT" = 42
        LDA #(TestConstants.testName2 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #42
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        LDA #(TestConstants.testTokens1 % 256)
        STA ZP.IDYL
        LDA #(TestConstants.testTokens1 / 256)
        STA ZP.IDYH
        
        Variables.Declare();
        
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
    
    // Test 16: Declare WORD constant
    testDeclareWordConstant()
    {
        LDA #'G'  // Test 16
        LDA #(TestConstants.variablesDesc3 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.variablesDesc3 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Variables.Initialize();
        
        // Declare WORD constant "CONST1" = 1000
        LDA #(TestConstants.testName5 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName5 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #(1000 % 256)
        STA ZP.NEXTL
        LDA #(1000 / 256)
        STA ZP.NEXTH
        
        LDA #(TestConstants.testTokens2 % 256)
        STA ZP.IDYL
        LDA #(TestConstants.testTokens2 / 256)
        STA ZP.IDYH
        
        Variables.Declare();
        
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
    
    // Test 17: Find variable by name
    testFindVariableByName()
    {
        LDA #'H'  // Test 17
        LDA #(TestConstants.variablesDesc4 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.variablesDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Variables.Initialize();
        
        // Declare variable first
        LDA #(TestConstants.testName1 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.BIT)
        STA ZP.ACCL
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Variables.Declare();
        
        // Now find it with type filter
        LDA #(TestConstants.testName1 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName1 / 256)
        STA ZP.TOPH
        LDA #SymbolType.VARIABLE
        STA ZP.ACCL
        
        Variables.Find();
        
        if (C)
        {
            Objects.Destroy();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0x40
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 18: Get variable value
    testGetVariableValue()
    {
        LDA #'I'  // Test 18
        LDA #(TestConstants.variablesDesc6 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.variablesDesc6 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Variables.Initialize();
        
        // Declare INT variable "TEMP" = 255
        LDA #(TestConstants.testName4 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName4 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #255
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
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
        
        Objects.Destroy();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 19: Set variable value
    testSetVariableValue()
    {
        LDA #'J'  // Test 19
        LDA #(TestConstants.variablesDesc8 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.variablesDesc8 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Variables.Initialize();
        
        // Declare variable
        LDA #(TestConstants.testName2 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
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
        
        Objects.Destroy();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 20: Set constant value (should fail)
    testSetConstantValue()
    {
        LDA #'K'  // Test 20
        LDA #(TestConstants.variablesDesc9 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.variablesDesc9 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Variables.Initialize();
        
        // Declare constant
        LDA #(TestConstants.testName5 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName5 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #50
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
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
        
        Objects.Destroy();  // Clean up
        SEC  // Pass (correctly failed)
        Test.PrintResult();
    }
    
    // Test 21: Get variable tokens
    testGetVariableTokens()
    {
        LDA #'L'  // Test 21
        LDA #(TestConstants.variablesDesc12 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.variablesDesc12 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Variables.Initialize();
        
        // Declare variable with specific tokens pointer
        LDA #(TestConstants.testName1 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCL
        LDA #(300 % 256)  // Fixed: proper 16-bit literal handling
        STA ZP.NEXTL
        LDA #(300 / 256)
        STA ZP.NEXTH
        
        LDA #(TestConstants.testTokens3 % 256)
        STA ZP.IDYL
        LDA #(TestConstants.testTokens3 / 256)
        STA ZP.IDYH
        Variables.Declare();
        
        // Find and get tokens
        STZ ZP.ACCL
        Variables.Find();
        Variables.GetTokens();
        
        // Check tokens pointer
        LDA ZP.NEXTL
        CMP #(TestConstants.testTokens3 % 256)
        if (NZ)
        {
            LDA #0x80
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        LDA ZP.NEXTH
        CMP #(TestConstants.testTokens3 / 256)
        if (NZ)
        {
            LDA #0x81
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Objects.Destroy();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 22: Iterate variables only
    testIterateVariablesOnly()
    {
        LDA #'M'  // Test 22
        LDA #(TestConstants.variablesDesc14 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.variablesDesc14 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Variables.Initialize();
        
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
        Variables.Declare();
        
        // Add constant
        LDA #(TestConstants.testName5 % 256)
        STA ZP.TOPL
        LDA #(TestConstants.testName5 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #20
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
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
        
        Objects.Destroy();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Run all variables tests
    RunVariablesTests()
    {
        testVariablesInit();
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