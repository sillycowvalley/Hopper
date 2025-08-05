unit TestFunctions
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "../Functions"
    uses "../Arguments"
    uses "../BasicTypes"
    uses "../Tools"
    
    // Private test data for Functions tests
    const string funcName1 = "ADD";
    const string funcName2 = "MULTIPLY";
    const string funcName3 = "CALCULATE";
    const string funcName4 = "NAMETEST";
    const string funcName5 = "SETARGS";
    const string funcName6 = "ITER1";
    const string funcName7 = "ITER2";
    const string funcName8 = "ITER3";
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
        
        // Store result in U5|U6 (consistent with other test files)
        LDA ZP.IDXL
        STA ZP.U5
        LDA ZP.IDXH
        STA ZP.U6
    }
    
    // Test descriptions (updated for new functionality)
    const string funcDesc1 = "Declare function with no arguments";
    const string funcDesc2 = "Declare function with arguments";
    const string funcDesc3 = "Find function by name";
    const string funcDesc4 = "Get function body tokens";
    const string funcDesc5 = "Get function arguments";
    const string funcDesc6 = "Remove function";
    const string funcDesc7 = "Iterate functions only";
    const string funcDesc8 = "Duplicate function (should fail)";
    const string funcDesc9 = "Get function name";
    const string funcDesc10 = "Set function arguments";
    const string funcDesc11 = "Multiple function iteration";
    const string funcDesc12 = "Clear all functions";
    
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
        
        // Declare function "ADD" with no arguments (no return type)
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        
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
        
        // Declare function "MULTIPLY" with no arguments initially (no return type)
        LDA #(funcName2 % 256)
        STA ZP.TOPL
        LDA #(funcName2 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        
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

        // Add first argument "A" (no type parameter)
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
        // Add second argument "B" (no type parameter)
        LDA #(argName2 % 256)
        STA ZP.TOPL
        LDA #(argName2 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
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
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
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
    
    // Test 35: Get function body tokens
    testGetFunctionBody()
    {
        LDA #'Z'  // Test 35
        LDA #(funcDesc4 % 256)
        STA ZP.TOPL
        LDA #(funcDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare function
        LDA #(funcName2 % 256)
        STA ZP.TOPL
        LDA #(funcName2 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
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
            LDA #0xD0
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        LDA ZP.IDYH
        CMP ZP.U6
        if (NZ)
        {
            LDA #0xD1
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 36: Get function arguments
    testGetFunctionArguments()
    {
        LDA #'['  // Test 36
        LDA #(funcDesc5 % 256)
        STA ZP.TOPL
        LDA #(funcDesc5 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare function with no arguments initially
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        
        STZ ZP.NEXTL  // No arguments initially
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        
        Functions.Declare();
        
        // Find the function
        Functions.Find();
        
        // Add an argument to the function (no type parameter)
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
        // Get arguments and check if it has arguments now
        Functions.GetArguments();
        // Returns ZP.IDY = arguments list head pointer, C set if has arguments
        
        if (NC)
        {
            LDA #0xE0
            CLC  // Fail - should have arguments now
            Test.PrintResult();
            return;
        }
        
        // Check that arguments list head is non-zero
        LDA ZP.IDYL
        ORA ZP.IDYH
        if (Z)
        {
            LDA #0xE1
            CLC  // Fail - arguments list head should be non-zero
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 37: Remove function by name
    testRemoveFunction()
    {
        LDA #'\\'  // Test 37
        LDA #(funcDesc6 % 256)
        STA ZP.TOPL
        LDA #(funcDesc6 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare function first
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        STZ ZP.IDYL   // No tokens
        STZ ZP.IDYH
        Functions.Declare();
        
        // Verify it exists
        Functions.Find();
        if (NC)
        {
            LDA #0xF0
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
            LDA #0xF1
            CLC  // Fail - remove failed
            Test.PrintResult();
            return;
        }
        
        // Verify it's gone
        Functions.Find();
        if (C)
        {
            LDA #0xF2
            CLC  // Fail - function still exists after removal
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
        LDA #(funcDesc7 % 256)
        STA ZP.TOPL
        LDA #(funcDesc7 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Add variable (to mix symbol types)
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCT
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
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Functions.Declare();
        
        // Iterate functions only
        Functions.IterateFunctions();
        if (NC)
        {
            LDA #0xA0
            CLC  // Fail - no functions found
            Test.PrintResult();
            return;
        }
        
        // Check type via Objects.GetData()
        Objects.GetData();
        LDA ZP.ACCT
        AND #0xF0
        LSR LSR LSR LSR
        CMP #SymbolType.FUNCTION
        if (NZ)
        {
            LDA #0xA1
            CLC  // Fail - wrong type
            Test.PrintResult();
            return;
        }
        
        // Should be no more functions
        Functions.IterateNext();
        if (C)
        {
            LDA #0xA2
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
        LDA #(funcDesc8 % 256)
        STA ZP.TOPL
        LDA #(funcDesc8 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare first function
        LDA #(funcName3 % 256)
        STA ZP.TOPL
        LDA #(funcName3 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Functions.Declare();
        
        if (NC)
        {
            LDA #0xB0
            CLC  // Fail - first declaration failed
            Test.PrintResult();
            return;
        }
        
        // Try to declare same function again (should fail)
        LDA #(funcName3 % 256)
        STA ZP.TOPL
        LDA #(funcName3 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Functions.Declare();
        
        if (C)
        {
            LDA #0xB1
            CLC  // Fail - should have failed
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up
        SEC  // Pass (correctly failed)
        Test.PrintResult();
    }
    
    // Test 40: Get function name
    testGetFunctionName()
    {
        LDA #'_'  // Test 40
        LDA #(funcDesc9 % 256)
        STA ZP.TOPL
        LDA #(funcDesc9 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare function
        LDA #(funcName4 % 256)
        STA ZP.TOPL
        LDA #(funcName4 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        STZ ZP.IDYL   // No tokens for this test
        STZ ZP.IDYH
        Functions.Declare();
        if (NC)
        {
            LDA #0xC0
            CLC  // Fail - not found
            Test.PrintResult();
            return;
        }
        // Find and get name
        Functions.Find();
        if (NC)
        {
            LDA #0xC1
            CLC  // Fail - not found
            Test.PrintResult();
            return;
        }
        
        Functions.GetName();
        
        // Compare name - use ZP.TOP as returned name pointer, ZP.ACC as expected name
        LDA #(funcName4 % 256)
        STA ZP.ACCL
        LDA #(funcName4 / 256)
        STA ZP.ACCH
        
        // Compare strings
        LDY #0
        loop
        {
            LDA [ZP.ACC], Y      // Expected name character
            STA ZP.SymbolTemp0   // Temporary storage
            LDA [ZP.TOP], Y      // Actual name character
            CMP ZP.SymbolTemp0   // Compare
            if (NZ)
            {
                DumpHeap();
                DumpVariables();
            
                LDA #0xC2
                CLC  // Fail - names don't match
                Test.PrintResult();
                return;
            }
            
            if (Z) { break; }    // Both null terminators - strings equal
            INY
        }
        
        Functions.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 41: Set function arguments
    testSetFunctionArguments()
    {
        LDA #'`'  // Test 41
        LDA #(funcDesc10 % 256)
        STA ZP.TOPL
        LDA #(funcDesc10 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare function with no arguments initially
        LDA #(funcName5 % 256)
        STA ZP.TOPL
        LDA #(funcName5 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL  // No arguments initially
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        Functions.Find(); // Sets ZP.IDX to function node
        
        // Add an argument to set as arguments list (no type parameter)
        LDA #(argName3 % 256)
        STA ZP.TOPL
        LDA #(argName3 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
        // Get the new arguments list head
        Functions.GetArguments();
        if (NC)
        {
            LDA #0xD0
            CLC  // Fail - should have arguments now
            Test.PrintResult();
            return;
        }
        
        // Test SetArguments by setting it to null (clearing arguments)
        STZ ZP.IDYL
        STZ ZP.IDYH
        Functions.SetArguments();
        if (NC)
        {
            LDA #0xD1
            CLC  // Fail - SetArguments failed
            Test.PrintResult();
            return;
        }
        
        // Verify arguments list is now empty
        Functions.GetArguments();
        if (C)
        {
            LDA #0xD2
            CLC  // Fail - should have no arguments after clearing
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 42: Multiple function iteration
    testMultipleFunctionIteration()
    {
        LDA #'a'  // Test 42
        LDA #(funcDesc11 % 256)
        STA ZP.TOPL
        LDA #(funcDesc11 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Add variable (to mix symbol types)
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCT
        LDA #42
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Variables.Declare();
        
        // Add first function
        LDA #(funcName6 % 256)
        STA ZP.TOPL
        LDA #(funcName6 / 256)
        STA ZP.TOPH
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        STZ ZP.IDYL   // No tokens
        STZ ZP.IDYH
        Functions.Declare();
         
        // Add second function
        LDA #(funcName7 % 256)
        STA ZP.TOPL
        LDA #(funcName7 / 256)
        STA ZP.TOPH
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        STZ ZP.IDYL   // No tokens
        STZ ZP.IDYH
        Functions.Declare();
        
        // Add third function
        LDA #(funcName8 % 256)
        STA ZP.TOPL
        LDA #(funcName8 / 256)
        STA ZP.TOPH
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        STZ ZP.IDYL   // No tokens
        STZ ZP.IDYH
        Functions.Declare();
        
        // Iterate through all functions and count them
        Functions.IterateFunctions();
        if (NC)
        {
            LDA #0xE0
            CLC  // Fail - no functions found
            Test.PrintResult();
            return;
        }
        
        // Count functions via iteration
        LDX #1  // Found first one
        PHX
        loop
        {
            // Verify each one is actually a function
            Objects.GetData();
            LDA ZP.ACCT
            AND #0xF0
            LSR LSR LSR LSR
            CMP #SymbolType.FUNCTION
            if (NZ)
            {
                PLX  // Clean up stack
                LDA #0xE1
                CLC  // Fail - found non-function during function iteration
                Test.PrintResult();
                return;
            }
            
            Functions.IterateNext();
            if (NC) 
            {
                break; // No more functions
            }  
            PLX
            INX
            PHX
        }
        PLX
        
        // Should have found exactly 3 functions
        CPX #3
        if (NZ)
        {
            LDA #0xE2
            CLC  // Fail - wrong function count
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up functions
        Variables.Clear();  // Clean up variables
        
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 43: Clear all functions
    testClearAllFunctions()
    {
        LDA #'b'  // Test 43
        LDA #(funcDesc12 % 256)
        STA ZP.TOPL
        LDA #(funcDesc12 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Add some functions
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        STZ ZP.IDYL   // No tokens
        STZ ZP.IDYH
        Functions.Declare();
        
        LDA #(funcName2 % 256)
        STA ZP.TOPL
        LDA #(funcName2 / 256)
        STA ZP.TOPH
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        STZ ZP.IDYL   // No tokens
        STZ ZP.IDYH
        Functions.Declare();
        
        // Verify we have functions before clearing
        Functions.IterateFunctions();
        if (NC)
        {
            LDA #0xF0
            CLC  // Fail - should have functions before clear
            Test.PrintResult();
            return;
        }
        
        // Clear all functions
        Functions.Clear();
        
        // Verify no functions remain
        Functions.IterateFunctions();
        if (C)
        {
            LDA #0xF1
            CLC  // Fail - functions not cleared
            Test.PrintResult();
            return;
        }
        
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Run all functions tests
    RunFunctionsTests()
    {
        testDeclareFunctionNoArgs();
        testDeclareFunctionWithArgs();
        testFindFunctionByName();
        testGetFunctionBody();
        testGetFunctionArguments();
        testRemoveFunction();
        testIterateFunctionsOnly();
        testDuplicateFunction();
        testGetFunctionName();
        testSetFunctionArguments();
        testMultipleFunctionIteration();
        testClearAllFunctions();
    }
}
