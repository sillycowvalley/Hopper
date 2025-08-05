unit TestArguments
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "../Functions"
    uses "../Arguments"
    uses "../BasicTypes"
    
    // Private test data for Arguments tests
    const string funcName1 = "TESTFUNC";
    const string funcName2 = "SEARCHFUNC";
    const string argName1 = "ARG1";
    const string argName2 = "ARG2";
    const string argName3 = "ARG3";
    const string argName4 = "PARAM";
    const string argName5 = "VALUE";
    
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
    const string argDesc1 = "Function argument count";
    const string argDesc2 = "Find argument by name";
    const string argDesc3 = "Get argument name";
    const string argDesc4 = "Find argument by index";
    const string argDesc5 = "Iterate function arguments";
    const string argDesc6 = "Clear function arguments";
    
    // Test 41: Function argument count
    testFunctionArgumentCount()
    {
        LDA #'`'  // Test 41
        LDA #(argDesc1 % 256)
        STA ZP.TOPL
        LDA #(argDesc1 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare function with no arguments initially
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
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
            LDA #0xA0
            CLC  // Fail - function declaration failed
            Test.PrintResult();
            return;
        }
        
        // Find the function we just created
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        Functions.Find(); // This sets ZP.IDX to function node address
        
        // Test count with no arguments (should be 0)
        Arguments.GetCount();
        LDA ZP.ACCL
        if (NZ)
        {
            LDA #0xA1
            CLC  // Fail - should be 0 arguments
            Test.PrintResult();
            return;
        }
        
        // Add first argument "ARG1" (no type parameter)
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        if (NC)
        {
            LDA #0xA2
            CLC  // Fail - first argument add failed
            Test.PrintResult();
            return;
        }
        
        // Test count with 1 argument
        Arguments.GetCount();
        LDA ZP.ACCL
        CMP #1
        if (NZ)
        {
            LDA #0xA3
            CLC  // Fail - should be 1 argument
            Test.PrintResult();
            return;
        }
        
        // Add second argument "ARG2" (no type parameter)
        LDA #(argName2 % 256)
        STA ZP.TOPL
        LDA #(argName2 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        if (NC)
        {
            LDA #0xA4
            CLC  // Fail - second argument add failed
            Test.PrintResult();
            return;
        }
        
        // Test count with 2 arguments
        Arguments.GetCount();
        LDA ZP.ACCL
        CMP #2
        if (NZ)
        {
            LDA #0xA5
            CLC  // Fail - should be 2 arguments
            Test.PrintResult();
            return;
        }
        
        // Add third argument "ARG3" (no type parameter)
        LDA #(argName3 % 256)
        STA ZP.TOPL
        LDA #(argName3 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        if (NC)
        {
            LDA #0xA6
            CLC  // Fail - third argument add failed
            Test.PrintResult();
            return;
        }
        
        // Test count with 3 arguments
        Arguments.GetCount();
        LDA ZP.ACCL
        CMP #3
        if (NZ)
        {
            LDA #0xA7
            CLC  // Fail - should be 3 arguments
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up (also clears arguments)
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 42: Find argument by name
    testFindArgumentByName()
    {
        LDA #'a'  // Test 42
        LDA #(argDesc2 % 256)
        STA ZP.TOPL
        LDA #(argDesc2 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare function
        LDA #(funcName2 % 256)
        STA ZP.TOPL
        LDA #(funcName2 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        Functions.Find(); // Sets ZP.IDX to function node
        
        // Save function node address
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Add argument "PARAM" (no type parameter)
        LDA #(argName4 % 256)
        STA ZP.TOPL
        LDA #(argName4 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
        // Restore function node address
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // Find the argument by name
        LDA #(argName4 % 256)
        STA ZP.TOPL
        LDA #(argName4 / 256)
        STA ZP.TOPH
        
        Arguments.Find();
        if (C)
        {
            // Check that index is 0 (first argument)
            LDA ZP.ACCL
            if (NZ)
            {
                LDA #0xB1
                CLC  // Fail - wrong index
                Test.PrintResult();
                return;
            }
            
            Functions.Clear();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0xB0
            CLC  // Fail - argument not found
        }
        Test.PrintResult();
    }
    
    // Test 43: Get argument name
    testGetArgumentName()
    {
        LDA #'b'  // Test 43
        LDA #(argDesc3 % 256)
        STA ZP.TOPL
        LDA #(argDesc3 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare function
        LDA #(funcName2 % 256)
        STA ZP.TOPL
        LDA #(funcName2 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        Functions.Find(); // Sets ZP.IDX to function node
        
        // Save function node address
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Add argument "ARG3" (no type parameter)
        LDA #(argName3 % 256)
        STA ZP.TOPL
        LDA #(argName3 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
        // Restore function node address
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // Find the argument and get its name
        LDA #(argName3 % 256)
        STA ZP.TOPL
        LDA #(argName3 / 256)
        STA ZP.TOPH
        Arguments.Find();
        if (NC)
        {
            LDA #0xC0
            CLC  // Fail - argument not found
            Test.PrintResult();
            return;
        }
        
        Arguments.GetName();
        // ZP.TOP now contains pointer to argument name in node
        
        // Compare with expected name
        LDA #(argName3 % 256)
        STA ZP.ACCL
        LDA #(argName3 / 256)
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
                LDA #0xC1
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
    
    // Test 44: Find argument by index
    testFindArgumentByIndex()
    {
        LDA #'c'  // Test 44
        LDA #(argDesc4 % 256)
        STA ZP.TOPL
        LDA #(argDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare function
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        Functions.Find(); // Sets ZP.IDX to function node
        
        // Add first argument "ARG1" (no type parameter)
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
        // Add second argument "ARG2" (no type parameter)
        LDA #(argName2 % 256)
        STA ZP.TOPL
        LDA #(argName2 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
        // Find argument by index 0 (should be ARG1)
        LDA #0
        STA ZP.ACCL
        Arguments.FindByIndex();
        if (NC)
        {
            LDA #0xD0
            CLC  // Fail - index 0 not found
            Test.PrintResult();
            return;
        }
        
        // Get name to verify it's the first argument (ARG1)
        Arguments.GetName();
        // ZP.TOP now contains pointer to argument name
        
        // Compare with expected name (ARG1)
        LDA #(argName1 % 256)
        STA ZP.ACCL
        LDA #(argName1 / 256)
        STA ZP.ACCH
        
        // Simple string comparison
        LDY #0
        LDA [ZP.ACC], Y      // Expected first character
        STA ZP.SymbolTemp0
        LDA [ZP.TOP], Y      // Actual first character
        CMP ZP.SymbolTemp0
        if (NZ)
        {
            LDA #0xD1
            CLC  // Fail - wrong name for index 0
            Test.PrintResult();
            return;
        }
        
        // Find argument by index 1 (should be ARG2)
        LDA #1
        STA ZP.ACCL
        Arguments.FindByIndex();
        if (NC)
        {
            LDA #0xD2
            CLC  // Fail - index 1 not found
            Test.PrintResult();
            return;
        }
        
        // Get name to verify it's the second argument (ARG2)
        Arguments.GetName();
        
        // Compare with expected name (ARG2)
        LDA #(argName2 % 256)
        STA ZP.ACCL
        LDA #(argName2 / 256)
        STA ZP.ACCH
        
        LDY #0
        LDA [ZP.ACC], Y      // Expected first character
        STA ZP.SymbolTemp0
        LDA [ZP.TOP], Y      // Actual first character
        CMP ZP.SymbolTemp0
        if (Z)
        {
            Functions.Clear();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0xD3
            CLC  // Fail - wrong name for index 1
        }
        Test.PrintResult();
    }
    
    // Test 45: Iterate function arguments
    testIterateFunctionArguments()
    {
        LDA #'d'  // Test 45
        LDA #(argDesc5 % 256)
        STA ZP.TOPL
        LDA #(argDesc5 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare function
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        Functions.Find(); // Sets ZP.IDX to function node
        
        // Save function node address
        LDA ZP.IDXL
        STA ZP.SymbolTemp0
        LDA ZP.IDXH
        STA ZP.SymbolTemp1
        
        // Add two arguments (no type parameters)
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
        // Restore function node address
        LDA ZP.SymbolTemp0
        STA ZP.IDXL
        LDA ZP.SymbolTemp1
        STA ZP.IDXH
        
        LDA #(argName2 % 256)
        STA ZP.TOPL
        LDA #(argName2 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
        // Restore function node address for iteration
        LDA ZP.SymbolTemp0
        STA ZP.IDXL
        LDA ZP.SymbolTemp1
        STA ZP.IDXH
        
        // Start iteration
        Arguments.IterateStart();
        if (NC)
        {
            LDA #0xE0
            CLC  // Fail - no arguments found
            Test.PrintResult();
            return;
        }
        
        // Count arguments via iteration
        LDX #1  // Found first one
        loop
        {
            Arguments.IterateNext();
            if (NC) { break; }  // No more arguments
            INX
        }
        
        // Should have found exactly 2 arguments
        CPX #2
        if (Z)
        {
            Functions.Clear();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0xE1
            CLC  // Fail - wrong argument count via iteration
        }
        Test.PrintResult();
    }
    
    // Test 46: Clear function arguments
    testClearFunctionArguments()
    {
        LDA #'e'  // Test 46
        LDA #(argDesc6 % 256)
        STA ZP.TOPL
        LDA #(argDesc6 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare function
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #(SymbolType.FUNCTION << 4)  // Functions have no data type
        STA ZP.ACCT
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        Functions.Find(); // Sets ZP.IDX to function node
        
        // Add some arguments
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
        LDA #(argName2 % 256)
        STA ZP.TOPL
        LDA #(argName2 / 256)
        STA ZP.TOPH
        Arguments.Add();  // No type parameter
        
        // Verify we have 2 arguments before clearing
        Arguments.GetCount();
        LDA ZP.ACCL
        CMP #2
        if (NZ)
        {
            LDA #0xF0
            CLC  // Fail - should have 2 arguments before clear
            Test.PrintResult();
            return;
        }
        
        // Clear all arguments
        Arguments.Clear();
        
        // Verify count is now 0
        Arguments.GetCount();
        LDA ZP.ACCL
        if (Z)
        {
            Functions.Clear();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0xF1
            CLC  // Fail - arguments not cleared
        }
        Test.PrintResult();
    }
    
    // Run all arguments tests
    RunArgumentsTests()
    {
        testFunctionArgumentCount();
        testFindArgumentByName();
        testGetArgumentName();
        testFindArgumentByIndex();
        testIterateFunctionArguments();
        testClearFunctionArguments();
    }
}