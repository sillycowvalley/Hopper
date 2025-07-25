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
    const string testName7 = "BITVAR";
    const string testName8 = "TESTVAR";
    
    // Allocate test token memory block
    // Returns address in ZP.U5|U6 for use with Variables.Declare()
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
        STA ZP.U5
        LDA ZP.IDXH
        STA ZP.U6
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
    const string variablesDesc9 = "Declare BIT variable";
    const string variablesDesc10 = "Remove variable";
    const string variablesDesc11 = "Get variable type";
    const string variablesDesc12 = "Get variable name";
    const string variablesDesc13 = "Iterate all symbols";
    const string variablesDesc14 = "Iterate constants via Variables";
    
    // Test: Declare INT variable
    testDeclareIntVariable()
    {
        LDA #(variablesDesc1 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc1 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
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
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
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
        allocateTestTokens();  // Result in U5|U6        
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
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
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
        LDA # SymbolType.VARIABLE
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
        allocateTestTokens();  // Result in U5|U6 
        
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
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Variables.Declare();
        
        // Find and get tokens
        STZ ZP.ACCL
        Variables.Find();
        Variables.GetTokens();
        
        // Check tokens pointer matches allocated address
        LDA ZP.NEXTL
        CMP ZP.U5
        if (NZ)
        {
            LDA #0x80
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        LDA ZP.NEXTH
        CMP ZP.U6
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
    
    // NEW TEST: Declare BIT variable
    testDeclareBitVariable()
    {
        LDA #(variablesDesc9 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc9 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare BIT variable "BITVAR" = 1
        LDA #(testName7 % 256)
        STA ZP.TOPL
        LDA #(testName7 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.BIT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Variables.Declare();
        
        if (C)
        {
            Variables.Clear();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0xA0
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // NEW TEST: Remove variable
    testRemoveVariable()
    {
        LDA #(variablesDesc10 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc10 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare variable first
        LDA #(testName8 % 256)
        STA ZP.TOPL
        LDA #(testName8 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #100
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Verify it exists
        STZ ZP.ACCL  // Any type
        Variables.Find();
        if (NC)
        {
            LDA #0xB0
            CLC  // Fail - variable not found after declaration
            Test.PrintResult();
            return;
        }
        
        // Remove it
        LDA #(testName8 % 256)
        STA ZP.TOPL
        LDA #(testName8 / 256)
        STA ZP.TOPH
        Variables.Remove();
        
        if (NC)
        {
            LDA #0xB1
            CLC  // Fail - remove failed
            Test.PrintResult();
            return;
        }
        
        // Verify it's gone
        Variables.Find();
        if (C)
        {
            LDA #0xB2
            CLC  // Fail - variable still exists after removal
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // NEW TEST: Get variable type
    testGetVariableType()
    {
        LDA #(variablesDesc11 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc11 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare WORD variable
        LDA #(testName4 % 256)
        STA ZP.TOPL
        LDA #(testName4 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCL
        LDA # (500 % 256)
        STA ZP.NEXTL
        LDA # (500 / 256)
        STA ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Find and get type
        STZ ZP.ACCL  // Any type
        Variables.Find();
        if (NC)
        {
            LDA #0xC0
            CLC  // Fail - not found
            Test.PrintResult();
            return;
        }
        
        Variables.GetType();
        if (NC)
        {
            LDA #0xC1
            CLC  // Fail - GetType failed
            Test.PrintResult();
            return;
        }
        
        // Check symbolType (high nibble)
        LDA ZP.ACCL
        AND #0xF0
        LSR LSR LSR LSR
        CMP #SymbolType.VARIABLE
        if (NZ)
        {
            LDA #0xC2
            CLC  // Fail - wrong symbol type
            Test.PrintResult();
            return;
        }
        
        // Check dataType (low nibble)
        LDA ZP.ACCL
        AND #0x0F
        CMP #BasicType.WORD
        if (NZ)
        {
            LDA #0xC3
            CLC  // Fail - wrong data type
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // NEW TEST: Get variable name
    testGetVariableName()
    {
        LDA #(variablesDesc12 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc12 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare variable
        LDA #(testName3 % 256)
        STA ZP.TOPL
        LDA #(testName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.BIT)
        STA ZP.ACCL
        LDA #0
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Find and get name
        STZ ZP.ACCL  // Any type
        Variables.Find();
        if (NC)
        {
            LDA #0xD0
            CLC  // Fail - not found
            Test.PrintResult();
            return;
        }
        
        Variables.GetName();
        
        // Compare name - use ZP.ACC as returned name pointer, ZP.TOP as expected name
        LDA #(testName3 % 256)
        STA ZP.TOPL
        LDA #(testName3 / 256)
        STA ZP.TOPH
        
        // Compare strings
        LDY #0
        loop
        {
            LDA [ZP.TOP], Y      // Expected name character
            STA ZP.SymbolTemp0   // Temporary storage
            LDA [ZP.ACC], Y      // Actual name character
            CMP ZP.SymbolTemp0   // Compare
            if (NZ)
            {
                LDA #0xD1
                CLC  // Fail - names don't match
                Test.PrintResult();
                return;
            }
            
            if (Z) { break; }    // Both null terminators - strings equal
            INY
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // NEW TEST: Iterate all symbols
    testIterateAllSymbols()
    {
        LDA #(variablesDesc13 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc13 / 256)
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
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
        STA ZP.ACCL
        LDA #20
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Iterate all symbols - should find both
        Variables.IterateAll();
        if (NC)
        {
            LDA #0xE0
            CLC  // Fail - no symbols found
            Test.PrintResult();
            return;
        }
        
        // Count symbols
        LDX #1  // Found first one
        loop
        {
            Variables.IterateNext();
            if (NC) { break; }  // No more symbols
            INX
        }
        
        // Should have found exactly 2 symbols
        CPX #2
        if (NZ)
        {
            LDA #0xE1
            CLC  // Fail - wrong count
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // NEW TEST: Iterate constants via Variables interface
    testIterateConstants()
    {
        LDA #(variablesDesc14 % 256)
        STA ZP.TOPL
        LDA #(variablesDesc14 / 256)
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
        LDA #((SymbolType.CONSTANT << 4) | BasicType.BIT)
        STA ZP.ACCL
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Iterate constants only via Variables interface
        Variables.IterateConstants();
        if (NC)
        {
            LDA #0xF0
            CLC  // Fail - no constants found
            Test.PrintResult();
            return;
        }
        
        // Check type
        Variables.GetType();
        LDA ZP.ACCL
        AND #0xF0
        LSR LSR LSR LSR
        CMP #SymbolType.CONSTANT
        if (NZ)
        {
            LDA #0xF1
            CLC  // Fail - wrong type
            Test.PrintResult();
            return;
        }
        
        // Should be no more constants
        Variables.IterateNext();
        if (C)
        {
            LDA #0xF2
            CLC  // Fail - found more than one constant
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
        testDeclareBitVariable();
        testRemoveVariable();
        testGetVariableType();
        testGetVariableName();
        testIterateAllSymbols();
        testIterateConstants();
    }
}
