unit TestFunctions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "Functions"
    uses "Arguments"
    uses "BasicTypes"
    
    // Private test data for Functions tests
    const string funcName1 = "ADD";
    const string funcName2 = "MULTIPLY";
    const string funcName3 = "CALCULATE";
    const string argName1 = "A";
    const string argName2 = "B";
    const string argName3 = "COUNT";
    
    // Allocate test token memory block
    // Returns address in ZP.U5|U6 for use with Functions.Declare()
    // MUST be called early before other variables are set up!
    allocateTestTokens()
    {
        // Allocate 32 bytes for test function body tokens
        LDA #32
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();  // Returns address in ZP.IDX, munts everything
        
        // Store result in U5|U6 (consistent with Variables/Constants tests)
        LDA ZP.IDXL
        STA ZP.U5
        LDA ZP.IDXH
        STA ZP.U6
    }
    
    // Test descriptions
    const string funcDesc1 = "Declare function with no arguments";
    const string funcDesc2 = "Declare function with arguments";
    const string funcDesc3 = "Find function by name";
    const string funcDesc4 = "Get function signature";
    const string funcDesc5 = "Get function body tokens";
    const string funcDesc6 = "Get function arguments";
    const string funcDesc7 = "Remove function";
    const string funcDesc8 = "Iterate functions only";
    const string funcDesc9 = "Duplicate function (should fail)";
    const string funcDesc10 = "Function type filtering";
    const string funcDesc11 = "Clear all functions";
    
    // Test 32: Declare function with no arguments
    testDeclareFunctionNoArgs()
    {
        LDA #'W'  // Test 32
        LDA #(funcDesc1 % 256)
        STA ZP.TOPL
        LDA #(funcDesc1 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare INT function "ADD" with no arguments
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        STZ ZP.NEXTL  // No arguments list
        STZ ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        
        if (C)
        {
            Functions.Clear();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0xA0
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 33: Declare function with arguments
    testDeclareFunctionWithArgs()
    {
        LDA #'X'  // Test 33
        LDA #(funcDesc2 % 256)
        STA ZP.TOPL
        LDA #(funcDesc2 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare WORD function "MULTIPLY" with no arguments initially
        LDA #(funcName2 % 256)
        STA ZP.TOPL
        LDA #(funcName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.WORD)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        STZ ZP.NEXTL  // No arguments initially
        STZ ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0xB0
            CLC  // Fail - function declaration failed
            Test.PrintResult();
            return;
        }
        
        // Now add arguments to the function
        // Find the function we just created
        LDA #(funcName2 % 256)
        STA ZP.TOPL
        LDA #(funcName2 / 256)
        STA ZP.TOPH
        Functions.Find(); // This sets ZP.IDX to function node address

        // Save function node address on stack
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Add first argument "A" of type INT
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        LDA #BasicType.INT
        STA ZP.ACCL
        Arguments.Add();
        
        // Restore function node address from stack
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // Add second argument "B" of type WORD
        LDA #(argName2 % 256)
        STA ZP.TOPL
        LDA #(argName2 / 256)
        STA ZP.TOPH
        LDA #BasicType.WORD
        STA ZP.ACCL
        Arguments.Add();
        
        Functions.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 34: Find function by name
    testFindFunctionByName()
    {
        LDA #'Y'  // Test 34
        LDA #(funcDesc3 % 256)
        STA ZP.TOPL
        LDA #(funcDesc3 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare function first
        LDA #(funcName3 % 256)
        STA ZP.TOPL
        LDA #(funcName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.BIT)
        STA ZP.ACCL
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        STZ ZP.IDYL   // No tokens for this test
        STZ ZP.IDYH
        Functions.Declare();
        
        // Now find it
        LDA #(funcName3 % 256)
        STA ZP.TOPL
        LDA #(funcName3 / 256)
        STA ZP.TOPH
        
        Functions.Find();
        
        if (C)
        {
            Functions.Clear();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0xC0
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 35: Get function signature
    testGetFunctionSignature()
    {
        LDA #'Z'  // Test 35
        LDA #(funcDesc4 % 256)
        STA ZP.TOPL
        LDA #(funcDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare function with known signature
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCL
        
        STZ ZP.NEXTL  // No arguments for this test
        STZ ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        
        // Find and get signature
        Functions.Find();
        if (NC)
        {
            LDA #0xD1
            CLC  // Fail - not found
            Test.PrintResult();
            return;
        }
        
        Functions.GetSignature();
        // NEW INTERFACE: ZP.ACCL = returnType, ZP.NEXT = function body tokens, ZP.IDY = arguments list head
        
        // Check return type (should be INT = 2)
        LDA ZP.ACCL
        CMP #BasicType.INT
        if (NZ)
        {
            LDA #0xD2
            CLC  // Fail - wrong return type
            Test.PrintResult();
            return;
        }
        
        // Check arguments list head (should be null for this test)
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (NZ)
        {
            LDA #0xD3
            CLC  // Fail - should have no arguments
            Test.PrintResult();
            return;
        }
        
        // Check function body tokens match
        LDA ZP.NEXTL
        CMP ZP.U5
        if (NZ)
        {
            LDA #0xD5
            CLC  // Fail - wrong tokens pointer low
            Test.PrintResult();
            return;
        }
        
        LDA ZP.NEXTH
        CMP ZP.U6
        if (NZ)
        {
            LDA #0xD6
            CLC  // Fail - wrong tokens pointer high
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 36: Get function body tokens
    testGetFunctionBody()
    {
        LDA #'['  // Test 36
        LDA #(funcDesc5 % 256)
        STA ZP.TOPL
        LDA #(funcDesc5 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare function
        LDA #(funcName2 % 256)
        STA ZP.TOPL
        LDA #(funcName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.WORD)
        STA ZP.ACCL
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        
        // Find and get body
        Functions.Find();
        Functions.GetBody();
        
        // Check tokens pointer matches (Functions.GetBody() returns in ZP.IDY)
        LDA ZP.IDYL
        CMP ZP.U5
        if (NZ)
        {
            LDA #0xE0
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        LDA ZP.IDYH
        CMP ZP.U6
        if (NZ)
        {
            LDA #0xE1
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 37: Get function arguments
    testGetFunctionArguments()
    {
        LDA #'\\' // Test 37
        LDA #(funcDesc6 % 256)
        STA ZP.TOPL
        LDA #(funcDesc6 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare function with no arguments initially
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCL
        
        STZ ZP.NEXTL  // No arguments initially
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        
        Functions.Declare();
        
        // Find the function
        Functions.Find();
        
        // Add an argument to the function
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        LDA #BasicType.INT
        STA ZP.ACCL
        Arguments.Add();
        
        // Get arguments and check if it has arguments now
        Functions.GetArguments();
        // NEW INTERFACE: Returns ZP.IDY = arguments list head pointer
        
        if (NC)
        {
            LDA #0xE9
            CLC  // Fail - should have arguments now
            Test.PrintResult();
            return;
        }
        
        // Check that arguments list head is non-zero
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (Z)
        {
            LDA #0xEA
            CLC  // Fail - arguments list head should be non-zero
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 38: Iterate functions only
    testIterateFunctionsOnly()
    {
        LDA #']'  // Test 38
        LDA #(funcDesc8 % 256)
        STA ZP.TOPL
        LDA #(funcDesc8 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Add variable (to mix symbol types)
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Variables.Declare();
        
        // Add function
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.FUNCTION << 4) | BasicType.BIT)
        STA ZP.ACCL
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Functions.Declare();
        
        // Iterate functions only
        Functions.IterateFunctions();
        if (NC)
        {
            LDA #0xF0
            CLC  // Fail - no functions found
            Test.PrintResult();
            return;
        }
        
        // Check type via Objects.GetData()
        Objects.GetData();
        LDA ZP.ACCL
        AND #0xF0
        LSR LSR LSR LSR
        CMP #SymbolType.FUNCTION
        if (NZ)
        {
            LDA #0xF1
            CLC  // Fail - wrong type
            Test.PrintResult();
            return;
        }
        
        // Should be no more functions
        Functions.IterateNext();
        if (C)
        {
            LDA #0xF2
            CLC  // Fail - found more than one function
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up functions
        Variables.Clear();  // Clean up variables
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 39: Duplicate function declaration (should fail)
    testDuplicateFunction()
    {
        LDA #'^'  // Test 39
        LDA #(funcDesc9 % 256)
        STA ZP.TOPL
        LDA #(funcDesc9 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare first function
        LDA #(funcName3 % 256)
        STA ZP.TOPL
        LDA #(funcName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Functions.Declare();
        
        if (NC)
        {
            LDA #0xA1
            CLC  // Fail - first declaration failed
            Test.PrintResult();
            return;
        }
        
        // Try to declare same function again (should fail)
        LDA #(funcName3 % 256)
        STA ZP.TOPL
        LDA #(funcName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.WORD)  // Different return type
        STA ZP.ACCL
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Functions.Declare();
        
        if (C)
        {
            LDA #0xA2
            CLC  // Fail - should have failed
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up
        SEC  // Pass (correctly failed)
        Test.PrintResult();
    }
    
    // Test 40: Remove function by name
    testRemoveFunction()
    {
        LDA #'_'  // Test 40
        LDA #(funcDesc7 % 256)
        STA ZP.TOPL
        LDA #(funcDesc7 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare function first
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        STZ ZP.IDYL   // No tokens
        STZ ZP.IDYH
        Functions.Declare();
        
        // Verify it exists
        Functions.Find();
        if (NC)
        {
            LDA #0xA3
            CLC  // Fail - function not found after declaration
            Test.PrintResult();
            return;
        }
        
        // Remove it
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        Functions.Remove();
        
        if (NC)
        {
            LDA #0xA4
            CLC  // Fail - remove failed
            Test.PrintResult();
            return;
        }
        
        // Verify it's gone
        Functions.Find();
        if (C)
        {
            LDA #0xA5
            CLC  // Fail - function still exists after removal
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Run all functions tests
    RunFunctionsTests()
    {
        testDeclareFunctionNoArgs();
        testDeclareFunctionWithArgs();
        testFindFunctionByName();
        testGetFunctionSignature();
        testGetFunctionBody();
        testGetFunctionArguments();
        testIterateFunctionsOnly();
        testDuplicateFunction();
        testRemoveFunction();
    }
}
