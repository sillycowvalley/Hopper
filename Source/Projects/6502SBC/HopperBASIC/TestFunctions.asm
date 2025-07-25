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
    // Returns address in ZP.SymbolTemp0/1 for use with Functions.Declare()
    // MUST be called early before other variables are set up!
    allocateTestTokens()
    {
        // Allocate 32 bytes for test function body tokens
        LDA #32
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();  // Returns address in ZP.IDX, munts everything
        
        // Store result in dedicated temporary slots
        LDA ZP.IDXL
        STA ZP.SymbolTemp0
        LDA ZP.IDXH
        STA ZP.SymbolTemp1
    }
    
    // Test descriptions
    const string funcDesc1 = "Declare function with no arguments";
    const string funcDesc2 = "Declare function with arguments";
    const string funcDesc3 = "Find function by name";
    const string funcDesc4 = "Get function signature";
    const string funcDesc5 = "Get function body tokens";
    const string funcDesc6 = "Get function arguments table";
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
        allocateTestTokens();  // Result in ZP.SymbolTemp0/1
        
        Functions.Initialize();
        
        // Declare INT function "ADD" with no arguments
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        STZ ZP.NEXTL  // No arguments table
        STZ ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.SymbolTemp0
        STA ZP.IDYL
        LDA ZP.SymbolTemp1
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
        allocateTestTokens();  // Result in ZP.SymbolTemp0/1
        
        Functions.Initialize();
        
        // Create arguments table
        Arguments.Create();
        if (NC)
        {
            LDA #0xB0
            CLC  // Fail - couldn't create arguments table
            Test.PrintResult();
            return;
        }
        
        // Save arguments table head
        LDA ZP.IDXL
        STA 0x7A  // Temporary storage
        LDA ZP.IDXH
        STA 0x7B
        
        // Add first argument "A" of type INT
        LDA #(argName1 % 256)
        STA ZP.TOPL
        LDA #(argName1 / 256)
        STA ZP.TOPH
        LDA #BasicType.INT
        STA ZP.ACCL
        Arguments.Add();
        
        // Add second argument "B" of type WORD
        LDA #(argName2 % 256)
        STA ZP.TOPL
        LDA #(argName2 / 256)
        STA ZP.TOPH
        LDA #BasicType.WORD
        STA ZP.ACCL
        Arguments.Add();
        
        // Restore arguments table head
        LDA 0x7A
        STA ZP.IDXL
        LDA 0x7B
        STA ZP.IDXH
        
        // Declare WORD function "MULTIPLY" with arguments
        LDA #(funcName2 % 256)
        STA ZP.TOPL
        LDA #(funcName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.WORD)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA ZP.IDXL  // Arguments table head
        STA ZP.NEXTL
        LDA ZP.IDXH
        STA ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.SymbolTemp0
        STA ZP.IDYL
        LDA ZP.SymbolTemp1
        STA ZP.IDYH
        
        Functions.Declare();
        
        if (C)
        {
            Functions.Clear();  // Clean up (includes arguments table)
            SEC  // Pass
        }
        else
        {
            LDA #0xB1
            CLC  // Fail
        }
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
        
        Functions.Initialize();
        
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
        allocateTestTokens();  // Result in ZP.SymbolTemp0/1
        
        Functions.Initialize();
        
        // Create simple arguments table
        Arguments.Create();
        if (NC)
        {
            LDA #0xD0
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Save arguments table head
        LDA ZP.IDXL
        STA 0x7A
        LDA ZP.IDXH
        STA 0x7B
        
        // Declare function with known signature
        LDA #(funcName1 % 256)
        STA ZP.TOPL
        LDA #(funcName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCL
        
        LDA 0x7A  // Arguments table head
        STA ZP.NEXTL
        LDA 0x7B
        STA ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.SymbolTemp0
        STA ZP.IDYL
        LDA ZP.SymbolTemp1
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
        
        // Check arguments table head matches
        LDA ZP.NEXTL
        CMP 0x7A
        if (NZ)
        {
            LDA #0xD3
            CLC  // Fail - wrong arguments table low
            Test.PrintResult();
            return;
        }
        
        LDA ZP.NEXTH
        CMP 0x7B
        if (NZ)
        {
            LDA #0xD4
            CLC  // Fail - wrong arguments table high
            Test.PrintResult();
            return;
        }
        
        // Check function body tokens match
        LDA ZP.IDYL
        CMP ZP.SymbolTemp0
        if (NZ)
        {
            LDA #0xD5
            CLC  // Fail - wrong tokens pointer low
            Test.PrintResult();
            return;
        }
        
        LDA ZP.IDYH
        CMP ZP.SymbolTemp1
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
        allocateTestTokens();  // Result in ZP.SymbolTemp0/1
        
        Functions.Initialize();
        
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
        LDA ZP.SymbolTemp0
        STA ZP.IDYL
        LDA ZP.SymbolTemp1
        STA ZP.IDYH
        
        Functions.Declare();
        
        // Find and get body
        Functions.Find();
        Functions.GetBody();
        
        // Check tokens pointer matches
        LDA ZP.IDYL
        CMP ZP.SymbolTemp0
        if (NZ)
        {
            LDA #0xE0
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        LDA ZP.IDYH
        CMP ZP.SymbolTemp1
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
    
    // Test 37: Iterate functions only
    testIterateFunctionsOnly()
    {
        LDA #(funcDesc8 % 256)
        STA ZP.TOPL
        LDA #(funcDesc8 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Functions.Initialize();
        
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
        
        Functions.Clear();  // Clean up (includes variables)
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 38: Duplicate function declaration (should fail)
    testDuplicateFunction()
    {
        LDA #']'  // Test 38
        LDA #(funcDesc9 % 256)
        STA ZP.TOPL
        LDA #(funcDesc9 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        Functions.Initialize();
        
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
    
    // Run all functions tests
    RunFunctionsTests()
    {
        testDeclareFunctionNoArgs();
        testDeclareFunctionWithArgs();
        testFindFunctionByName();
        testGetFunctionSignature();
        testGetFunctionBody();
        testIterateFunctionsOnly();
        testDuplicateFunction();
    }
}
