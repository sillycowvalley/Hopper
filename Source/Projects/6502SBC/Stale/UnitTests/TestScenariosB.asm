unit TestScenariosB
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "../Variables"
    uses "../Functions"
    uses "../BasicTypes"
    
    // Test data for integration scenarios
    const string varName1 = "X";
    const string varName2 = "COUNTER";
    const string varName3 = "STATUS";
    const string constName1 = "PI";
    const string mainProgName = "main";
    const string regularFuncName = "FOO";
    
    
    // Test descriptions
    const string scenarioDesc6 = "LIST command data retrieval";
    const string scenarioDesc7 = "Symbol table serialization readiness";
    const string scenarioDesc8 = "Mixed global symbol usage";
    const string scenarioDesc9 = "Safe symbol creation pattern";
    
    // Allocate test token memory block
    // Returns address in ZP.U5|U6 for use with Variables.Declare()
    // MUST be called early before other variables are set up!
    allocateTestTokens()
    {
        // Allocate 32 bytes for test token data
        LDA #32
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();  // Returns address in ZP.IDX, munts everything
        
        // Store result in dedicated temporary slots
        LDA ZP.IDXL
        STA ZP.U5
        LDA ZP.IDXH
        STA ZP.U6
    }
    
    testSafeSymbolCreationPattern()
    {
        LDA #(scenarioDesc9 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc9 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Step 1: Test safe variable creation - check both Variables and Functions namespaces
        // Try to create INT X - should succeed since "X" doesn't exist anywhere
        
        // First check Variables namespace for "X"
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type - checking for existence
        
        Variables.Find();
        if (C)
        {
            LDA #0x60
            CLC  // Fail - X should not exist in Variables yet
            Test.PrintResult();
            return;
        }
        
        // Then check Functions namespace for "X"
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (C)
        {
            LDA #0x61
            CLC  // Fail - X should not exist in Functions yet
            Test.PrintResult();
            return;
        }
        
        // Both namespaces clear - safe to create INT X = 10
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCT
        
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x62
            CLC  // Fail - variable declaration should have succeeded
            Test.PrintResult();
            return;
        }
        
        // Step 2: Verify variable "X" was created successfully
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type
        
        Variables.Find();
        if (NC)
        {
            LDA #0x63
            CLC  // Fail - X should now exist in Variables
            Test.PrintResult();
            return;
        }
        
        // Verify it has correct value
        Variables.GetValue();
        if (NC)
        {
            LDA #0x64
            CLC  // Fail - GetValue failed
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPL
        CMP #10
        if (NZ)
        {
            LDA #0x65
            CLC  // Fail - wrong value
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPT
        CMP #BasicType.INT
        if (NZ)
        {
            LDA #0x66
            CLC  // Fail - wrong type
            Test.PrintResult();
            return;
        }
        
        // Step 3: Test conflict detection - attempt to create FUNC X (should fail)
        // HopperBASIC responsible pattern: check Variables first
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type
        
        Variables.Find();
        if (NC)
        {
            LDA #0x67
            CLC  // Fail - X should exist in Variables from step 1
            Test.PrintResult();
            return;
        }
        
        // Variable "X" found - should NOT attempt to create function "X"
        // This simulates proper HopperBASIC interpreter behavior
        // In real interpreter: error message "Name already in use" would be shown
        
        // Verify we can still find the original variable (not corrupted)
        Variables.GetValue();
        LDA ZP.TOPL
        CMP #10
        if (NZ)
        {
            LDA #0x68
            CLC  // Fail - variable X value changed during conflict check
            Test.PrintResult();
            return;
        }
        
        // Step 4: Test the reverse - try to create variable with same name as existing function
        // First create a function "STATUS"
        allocateTestTokens();  // Tokens in ZP.U5|U6
        
        LDA #(varName3 % 256)  // "STATUS"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCT
        
        STZ ZP.NEXTL  // No arguments for this test
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0x69
            CLC  // Fail - function declaration should have succeeded
            Test.PrintResult();
            return;
        }
        
        // Step 5: Verify function "STATUS" exists
        LDA #(varName3 % 256)  // "STATUS"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            LDA #0x6A
            CLC  // Fail - STATUS function should exist
            Test.PrintResult();
            return;
        }
        
        // Step 6: Now attempt to create variable "STATUS" - should detect conflict
        // HopperBASIC responsible pattern: check Variables first, then Functions
        LDA #(varName3 % 256)  // "STATUS"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type
        
        Variables.Find();
        if (C)
        {
            LDA #0x6B
            CLC  // Fail - STATUS should not exist in Variables yet
            Test.PrintResult();
            return;
        }
        
        // Check Functions namespace for "STATUS" - should find it
        LDA #(varName3 % 256)  // "STATUS"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            LDA #0x6C
            CLC  // Fail - STATUS function should be found
            Test.PrintResult();
            return;
        }
        
        // Function "STATUS" found - should NOT attempt to create variable "STATUS"
        // In real interpreter: error message "Name already in use" would be shown
        
        // Step 7: Test creating symbol with unique name (should succeed)
        // Check both namespaces for "COUNTER" - should be clear
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type
        
        Variables.Find();
        if (C)
        {
            LDA #0x6D
            CLC  // Fail - COUNTER should not exist in Variables
            Test.PrintResult();
            return;
        }
        
        Functions.Find();
        if (C)
        {
            LDA #0x6E
            CLC  // Fail - COUNTER should not exist in Functions
            Test.PrintResult();
            return;
        }
        
        // Both namespaces clear - safe to create WORD COUNTER = 0
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCT
        
        STZ ZP.NEXTL  // Value = 0
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x6F
            CLC  // Fail - COUNTER variable declaration should have succeeded
            Test.PrintResult();
            return;
        }
        
        // Step 8: Verify all three symbols coexist properly
        // Check X (variable)
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        if (NC)
        {
            Variables.Clear();
            Functions.Clear();
            LDA #0x60  // Reuse 0x60 as final verification step
            CLC  // Fail - X variable missing in final check
            Test.PrintResult();
            return;
        }
        
        // Check STATUS (function)
        LDA #(varName3 % 256)  // "STATUS"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            Variables.Clear();
            Functions.Clear();
            LDA #0x61  // Reuse 0x61 as final verification step
            CLC  // Fail - STATUS function missing in final check
            Test.PrintResult();
            return;
        }
        
        // Check COUNTER (variable)
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        if (NC)
        {
            Variables.Clear();
            Functions.Clear();
            LDA #0x62  // Reuse 0x62 as final verification step
            CLC  // Fail - COUNTER variable missing in final check
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up all variables
        Functions.Clear();  // Clean up all functions
        SEC  // Pass
        Test.PrintResult();
    }
    
    testTokenMemoryLifecycle()
    {
        LDA #(scenarioDesc5 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc5 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Step 1: Allocate token memory and declare variable with tokens
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Store original token pointer for verification
        LDA ZP.U5
        STA ZP.W0  // Save original token pointer low byte
        LDA ZP.U6
        STA ZP.W1  // Save original token pointer high byte
        
        // Declare INT X = 42 with allocated token stream
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCT
        
        LDA #42
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        // Use allocated token pointer
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x70
            CLC  // Fail - variable declaration with tokens failed
            Test.PrintResult();
            return;
        }
        
        // Step 2: Verify token memory is properly stored in variable
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type
        
        Variables.Find();
        if (NC)
        {
            LDA #0x71
            CLC  // Fail - could not find declared variable
            Test.PrintResult();
            return;
        }
        
        // Get token pointer from variable
        Variables.GetTokens();
        if (NC)
        {
            LDA #0x72
            CLC  // Fail - GetTokens failed
            Test.PrintResult();
            return;
        }
        
        // Verify token pointer matches what we allocated
        LDA ZP.W1  // Original token pointer high byte
        CMP ZP.IDYH
        if (NZ)
        {
            LDA #0x73
            CLC  // Fail - token pointer high byte mismatch
            Test.PrintResult();
            return;
        }
        
        LDA ZP.W0  // Original token pointer low byte
        CMP ZP.IDYL
        if (NZ)
        {
            LDA #0x74
            CLC  // Fail - token pointer low byte mismatch
            Test.PrintResult();
            return;
        }
        
        // Step 3: Test token memory preservation across multiple operations
        // Store current token pointer for comparison
        LDA ZP.IDYL
        STA ZP.W2  // Save current token pointer low byte
        LDA ZP.IDYH
        STA ZP.W3  // Save current token pointer high byte
        
        // Multiple GetTokens() calls should return same pointer
        Variables.GetTokens();
        if (NC)
        {
            LDA #0x75
            CLC  // Fail - second GetTokens failed
            Test.PrintResult();
            return;
        }
        
        // Verify pointer is still the same
        LDA ZP.W3  // Saved high byte
        CMP ZP.IDYH
        if (NZ)
        {
            LDA #0x76
            CLC  // Fail - token pointer changed after multiple calls
            Test.PrintResult();
            return;
        }
        
        LDA ZP.W2  // Saved low byte
        CMP ZP.IDYL
        if (NZ)
        {
            LDA #0x77
            CLC  // Fail - token pointer changed after multiple calls
            Test.PrintResult();
            return;
        }
        
        // Step 4: Test that GetValue/SetValue preserve token pointer access
        Variables.GetValue();
        if (NC)
        {
            LDA #0x78
            CLC  // Fail - GetValue failed
            Test.PrintResult();
            return;
        }
        
        // Verify original value
        LDA ZP.TOPL
        CMP #42
        if (NZ)
        {
            LDA #0x79
            CLC  // Fail - wrong value retrieved
            Test.PrintResult();
            return;
        }
        
        // Set new value
        LDA #84
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();
        if (NC)
        {
            LDA #0x7A
            CLC  // Fail - SetValue failed
            Test.PrintResult();
            return;
        }
        
        // Verify tokens still accessible after value change
        Variables.GetTokens();
        if (NC)
        {
            LDA #0x7B
            CLC  // Fail - GetTokens failed after SetValue
            Test.PrintResult();
            return;
        }
        
        // Verify pointer unchanged after value modification
        LDA ZP.W3  // Saved high byte
        CMP ZP.IDYH
        if (NZ)
        {
            LDA #0x7C
            CLC  // Fail - token pointer changed after SetValue
            Test.PrintResult();
            return;
        }
        
        LDA ZP.W2  // Saved low byte
        CMP ZP.IDYL
        if (NZ)
        {
            LDA #0x7D
            CLC  // Fail - token pointer changed after SetValue
            Test.PrintResult();
            return;
        }
        
        // Step 5: Test token memory cleanup during Variables.Remove()
        // Store the token pointer for post-removal verification
        LDA ZP.IDYL
        STA ZP.W4  // Save token pointer low byte for cleanup test
        LDA ZP.IDYH
        STA ZP.W5  // Save token pointer high byte for cleanup test
        
        // Remove the variable (should automatically free token memory)
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        Variables.Remove();
        if (NC)
        {
            LDA #0x7E
            CLC  // Fail - variable removal failed
            Test.PrintResult();
            return;
        }
        
        // Step 6: Verify variable is completely gone
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        if (C)
        {
            LDA #0x7F
            CLC  // Fail - variable still exists after removal
            Test.PrintResult();
            return;
        }
        
        // Step 7: Test function token memory lifecycle
        allocateTestTokens();  // New tokens for function body
        
        // Store function token pointer for verification
        LDA ZP.U5
        STA ZP.W6  // Save function token pointer low byte
        LDA ZP.U6
        STA ZP.W7  // Save function token pointer high byte
        
        // Declare function with token body
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.WORD)
        STA ZP.ACCT
        
        STZ ZP.NEXTL  // No arguments for this test
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0x70  // Reuse 0x70 for function section
            CLC  // Fail - function declaration with tokens failed
            Test.PrintResult();
            return;
        }
        
        // Step 8: Verify function body tokens are properly stored
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            LDA #0x71  // Reuse for function section
            CLC  // Fail - could not find declared function
            Test.PrintResult();
            return;
        }
        
        // Get function body tokens
        Functions.GetBody();
        if (NC)
        {
            LDA #0x72  // Reuse for function section
            CLC  // Fail - GetBody failed
            Test.PrintResult();
            return;
        }
        
        // Verify body token pointer matches what we allocated
        LDA ZP.W7  // Original function token pointer high byte
        CMP ZP.IDYH
        if (NZ)
        {
            LDA #0x73  // Reuse for function section
            CLC  // Fail - function token pointer high byte mismatch
            Test.PrintResult();
            return;
        }
        
        LDA ZP.W6  // Original function token pointer low byte
        CMP ZP.IDYL
        if (NZ)
        {
            LDA #0x74  // Reuse for function section
            CLC  // Fail - function token pointer low byte mismatch
            Test.PrintResult();
            return;
        }
        
        // Step 9: Test function token memory cleanup during Functions.Remove()
        // Store body token pointer for cleanup verification
        LDA ZP.IDYL
        STA ZP.U0  // Save body token pointer low byte
        LDA ZP.IDYH
        STA ZP.U1  // Save body token pointer high byte
        
        // Remove the function (should automatically free body tokens)
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        Functions.Remove();
        if (NC)
        {
            LDA #0x75  // Reuse for function section
            CLC  // Fail - function removal failed
            Test.PrintResult();
            return;
        }
        
        // Step 10: Verify function is completely gone
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (C)
        {
            LDA #0x76  // Reuse for function section
            CLC  // Fail - function still exists after removal
            Test.PrintResult();
            return;
        }
        
        // Step 11: Test complex token memory scenario - variable with tokens + function with tokens
        // Create both simultaneously to test memory management
        allocateTestTokens();  // Tokens for variable
        
        LDA ZP.U5
        STA ZP.U0  // Save variable token pointer low
        LDA ZP.U6
        STA ZP.U1  // Save variable token pointer high
        
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCT
        
        LDA #200
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x77  // Reuse for complex section
            CLC  // Fail - complex variable declaration failed
            Test.PrintResult();
            return;
        }
        
        allocateTestTokens();  // New tokens for function
        
        LDA ZP.U5
        STA ZP.U2  // Save function token pointer low
        LDA ZP.U6
        STA ZP.U3  // Save function token pointer high
        
        LDA #(varName3 % 256)  // "STATUS"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.BIT)
        STA ZP.ACCT
        
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0x78  // Reuse for complex section
            CLC  // Fail - complex function declaration failed
            Test.PrintResult();
            return;
        }
        
        // Step 12: Verify both symbols exist with correct token pointers
        // Check variable tokens
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        Variables.GetTokens();
        
        // Compare with saved variable token pointer
        LDA ZP.U1  // Variable token high byte
        CMP ZP.IDYH
        if (NZ)
        {
            LDA #0x79  // Reuse for complex section
            CLC  // Fail - variable token pointer mismatch in complex test
            Test.PrintResult();
            return;
        }
        
        LDA ZP.U0  // Variable token low byte
        CMP ZP.IDYL
        if (NZ)
        {
            LDA #0x7A  // Reuse for complex section
            CLC  // Fail - variable token pointer mismatch in complex test
            Test.PrintResult();
            return;
        }
        
        // Check function tokens
        LDA #(varName3 % 256)  // "STATUS"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        Functions.Find();
        Functions.GetBody();
        
        // Compare with saved function token pointer
        LDA ZP.U3  // Function token high byte
        CMP ZP.IDYH
        if (NZ)
        {
            LDA #0x7B  // Reuse for complex section
            CLC  // Fail - function token pointer mismatch in complex test
            Test.PrintResult();
            return;
        }
        
        LDA ZP.U2  // Function token low byte
        CMP ZP.IDYL
        if (NZ)
        {
            LDA #0x7C  // Reuse for complex section
            CLC  // Fail - function token pointer mismatch in complex test
            Test.PrintResult();
            return;
        }
        
        // Step 13: Remove both symbols (test cleanup of multiple token streams)
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        
        Variables.Remove();
        if (NC)
        {
            LDA #0x7D  // Reuse for complex section
            CLC  // Fail - complex variable removal failed
            Test.PrintResult();
            return;
        }
        
        LDA #(varName3 % 256)  // "STATUS"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        Functions.Remove();
        if (NC)
        {
            LDA #0x7E  // Reuse for complex section
            CLC  // Fail - complex function removal failed
            Test.PrintResult();
            return;
        }
        
        // Step 14: Verify both symbols are completely gone
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        if (C)
        {
            LDA #0x7F  // Final error code
            CLC  // Fail - variable should be gone after complex cleanup
            Test.PrintResult();
            return;
        }
        
        LDA #(varName3 % 256)  // "STATUS"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (C)
        {
            LDA #0x70  // Wrap back to start of range for final check
            CLC  // Fail - function should be gone after complex cleanup
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up any remaining symbols
        Functions.Clear();
        SEC  // Pass
        Test.PrintResult();
    }


    testMixedGlobalSymbolUsage()
    {
        LDA #(scenarioDesc8 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc8 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Simulate pattern: function accesses global variables and constants
        // This is the core lookup pattern used throughout HopperBASIC interpreter
        
        // Step 1: Create mixed global symbol environment
        // INT counter = 1
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCT
        
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x80
            CLC  // Fail - INT variable declaration failed
            Test.PrintResult();
            return;
        }
        
        // WORD status = 200
        LDA #(varName3 % 256)  // "STATUS"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCT
        
        LDA #200
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x81
            CLC  // Fail - WORD variable declaration failed
            Test.PrintResult();
            return;
        }
        
        // BIT flag = 1
        LDA #(varName1 % 256)  // "X" used as flag name
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.BIT)
        STA ZP.ACCT
        
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x82
            CLC  // Fail - BIT variable declaration failed
            Test.PrintResult();
            return;
        }
        
        // CONST MAX_SIZE = 1024
        LDA #(constName1 % 256)  // "PI" repurposed as MAX_SIZE
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
        STA ZP.ACCT
        
        LDA #(1024 % 256)
        STA ZP.NEXTL
        LDA #(1024 / 256)
        STA ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            // Debug output for constant creation failure
            LDA #'C'  // Debug marker for constant 
            LDA #0x83
            CLC  // Fail - CONST declaration failed
            Test.PrintResult();
            return;
        }
        
        // CONST ERROR_CODE = -1 (INT constant)
        LDA #(mainProgName % 256)  // "main" repurposed as ERROR_CODE
        STA ZP.TOPL
        LDA #(mainProgName / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCT
        
        LDA #(65535 % 256)  // -1 as unsigned 16-bit
        STA ZP.NEXTL
        LDA #(65535 / 256)
        STA ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            // Debug output for INT constant creation failure  
            LDA #'I'  // Debug marker for INT constant
            LDA #0x84
            CLC  // Fail - INT constant declaration failed
            Test.PrintResult();
            return;
        }
        
        // Step 2: Create function that will access these globals
        allocateTestTokens();  // Body tokens for function
        
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCT
        
        STZ ZP.NEXTL  // No arguments for this test
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0x85
            CLC  // Fail - function declaration failed
            Test.PrintResult();
            return;
        }
        
        // Step 3: Simulate function execution context - global variable access patterns
        // Pattern 1: Access INT variable (typical arithmetic usage)
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any symbol type
        
        Variables.Find();
        if (NC)
        {
            LDA #0x86
            CLC  // Fail - could not find INT variable from function context
            Test.PrintResult();
            return;
        }
        
        Variables.GetValue();
        if (NC)
        {
            LDA #0x87
            CLC  // Fail - could not get INT variable value
            Test.PrintResult();
            return;
        }
        
        // Verify correct value and type
        LDA ZP.TOPL
        CMP #1
        if (NZ)
        {
            LDA #0x88
            CLC  // Fail - wrong INT variable value
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPT
        CMP #BasicType.INT
        if (NZ)
        {
            LDA #0x89
            CLC  // Fail - wrong INT variable type
            Test.PrintResult();
            return;
        }
        
        // Pattern 2: Modify variable from function (simulates assignment statement)
        LDA #5  // New value
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();  // ZP.IDX preserved from Find() above
        if (NC)
        {
            LDA #0x8A
            CLC  // Fail - could not modify INT variable from function
            Test.PrintResult();
            return;
        }
        
        // Verify modification worked
        Variables.GetValue();
        LDA ZP.TOPL
        CMP #5
        if (NZ)
        {
            LDA #0x8B
            CLC  // Fail - INT variable modification failed
            Test.PrintResult();
            return;
        }
        
        // Pattern 3: Access WORD variable (different type)
        LDA #(varName3 % 256)  // "STATUS"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any symbol type
        
        Variables.Find();
        if (NC)
        {
            LDA #0x8C
            CLC  // Fail - could not find WORD variable
            Test.PrintResult();
            return;
        }
        
        Variables.GetValue();
        LDA ZP.TOPL
        CMP #200
        if (NZ)
        {
            LDA #0x8D
            CLC  // Fail - wrong WORD variable value
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPT
        CMP #BasicType.WORD
        if (NZ)
        {
            LDA #0x8E
            CLC  // Fail - wrong WORD variable type
            Test.PrintResult();
            return;
        }
        
        // Pattern 4: Access BIT variable (boolean logic context)
        LDA #(varName1 % 256)  // "X" (flag)
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        if (NC)
        {
            LDA #0x8F
            CLC  // Fail - could not find BIT variable
            Test.PrintResult();
            return;
        }
        
        Variables.GetValue();
        LDA ZP.TOPL
        CMP #1
        if (NZ)
        {
            LDA #0x80  // Wrap back for BIT error codes
            CLC  // Fail - wrong BIT variable value
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPT
        CMP #BasicType.BIT
        if (NZ)
        {
            LDA #0x81  // Continue wrapped codes
            CLC  // Fail - wrong BIT variable type
            Test.PrintResult();
            return;
        }
        
        // Pattern 5: Access WORD constant (typical in expressions)
        LDA #(constName1 % 256)  // "PI" (MAX_SIZE)
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        if (NC)
        {
            LDA #0x82  // Continue wrapped codes
            CLC  // Fail - could not find WORD constant
            Test.PrintResult();
            return;
        }
        
        Variables.GetValue();
        LDA ZP.TOPL
        CMP #(1024 % 256)
        if (NZ)
        {
            LDA #0x83
            CLC  // Fail - wrong WORD constant value low byte
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPH
        CMP #(1024 / 256)
        if (NZ)
        {
            LDA #0x84
            CLC  // Fail - wrong WORD constant value high byte
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPT
        CMP #BasicType.WORD
        if (NZ)
        {
            LDA #0x85
            CLC  // Fail - wrong WORD constant type
            Test.PrintResult();
            return;
        }
        
        // Pattern 6: Access INT constant (negative values)
        LDA #(mainProgName % 256)  // "main" (ERROR_CODE)
        STA ZP.TOPL
        LDA #(mainProgName / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        if (NC)
        {
            LDA #0x86
            CLC  // Fail - could not find INT constant
            Test.PrintResult();
            return;
        }
        
        Variables.GetValue();
        LDA ZP.TOPL
        CMP #(65535 % 256)  // -1 as unsigned
        if (NZ)
        {
            LDA #0x87
            CLC  // Fail - wrong INT constant value low byte
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPH
        CMP #(65535 / 256)
        if (NZ)
        {
            LDA #0x88
            CLC  // Fail - wrong INT constant value high byte
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPT
        CMP #BasicType.INT
        if (NZ)
        {
            LDA #0x89
            CLC  // Fail - wrong INT constant type
            Test.PrintResult();
            return;
        }
        
        // Pattern 7: Test constant immutability (should fail)
        LDA #42  // Try to change constant
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();  // ZP.IDX still points to INT constant
        if (C)
        {
            LDA #0x8A
            CLC  // Fail - should not have been able to modify constant
            Test.PrintResult();
            return;
        }
        
        // Verify constant value unchanged
        Variables.GetValue();
        LDA ZP.TOPL
        CMP #(65535 % 256)  // Still -1
        if (NZ)
        {
            LDA #0x8B
            CLC  // Fail - constant value changed when it shouldn't have
            Test.PrintResult();
            return;
        }
        
        // Step 4: Test mixed symbol iteration (simulates VARS command)
        // Count variables vs constants
        LDX #0  // Variable counter
        LDY #0  // Constant counter
        
        Variables.IterateAll();  // Start with all symbols
        if (NC)
        {
            LDA #0x8C
            CLC  // Fail - no symbols found for iteration
            Test.PrintResult();
            return;
        }
        
        loop
        {
            // Check if current symbol is variable or constant
            Variables.GetSignature();
            LDA ZP.ACCT
            AND #0xF0  // Extract symbol type (high nibble)
            CMP #(SymbolType.VARIABLE << 4)
            if (Z)
            {
                INX  // Count variable
            }
            else
            {
                CMP #(SymbolType.CONSTANT << 4)
                if (Z)
                {
                    INY  // Count constant
                }
            }
            
            Variables.IterateNext();
            if (NC) 
            { 
                break; 
            }
        }
        
        // Verify counts: 3 variables + 2 constants = 5 total
        TXA  // Variable count
        CMP #3
        if (NZ)
        {
            LDA #0x8D
            CLC  // Fail - wrong variable count
            Test.PrintResult();
            return;
        }
        
        TYA  // Constant count
        CMP #2
        if (NZ)
        {
            LDA #0x8E
            CLC  // Fail - wrong constant count
            Test.PrintResult();
            return;
        }
        
        // Step 5: Test cross-namespace lookup (variable vs function names)
        // Find the function we created
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            LDA #0x8F
            CLC  // Fail - could not find function from mixed context
            Test.PrintResult();
            return;
        }
        
        // Verify function still accessible using Objects.GetData() + bit extraction
        Objects.GetData();
        LDA ZP.ACCT
        AND #0x0F  // Extract return type (low nibble)
        CMP #BasicType.INT  // Return type
        if (NZ)
        {
            LDA #0x80  // Final wrap
            CLC  // Fail - wrong function return type
            Test.PrintResult();
            return;
        }
        
        // Step 6: Test symbol name conflict detection 
        // Try to create variable "FOO" (should work - different namespace)
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCT
        
        LDA #99
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x81
            CLC  // Fail - should be able to create variable "FOO" (different namespace)
            Test.PrintResult();
            return;
        }
        
        // Step 7: Verify both "FOO" symbols coexist
        // Find variable "FOO"
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        if (NC)
        {
            LDA #0x82
            CLC  // Fail - variable FOO not found
            Test.PrintResult();
            return;
        }
        
        Variables.GetValue();
        LDA ZP.TOPL
        CMP #99
        if (NZ)
        {
            LDA #0x83
            CLC  // Fail - wrong variable FOO value
            Test.PrintResult();
            return;
        }
        
        // Find function "FOO"
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            LDA #0x84
            CLC  // Fail - function FOO not found after variable creation
            Test.PrintResult();
            return;
        }
        
        Objects.GetData();
        LDA ZP.ACCT
        AND #0x0F  // Extract return type (low nibble)
        CMP #BasicType.INT
        if (NZ)
        {
            LDA #0x85
            CLC  // Fail - function FOO signature changed
            Test.PrintResult();
            return;
        }
        
        // Step 8: Test realistic expression evaluation pattern
        // Simulate: result = counter + MAX_SIZE - 1
        // This tests the typical global symbol lookup pattern in expressions
        
        // Get counter value (5 from earlier modification)
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        Variables.GetValue();
        
        // Store counter value temporarily
        LDA ZP.TOPL
        PHA  // Save counter value (5)
        
        // Get MAX_SIZE constant value (1024)
        LDA #(constName1 % 256)  // "PI" (MAX_SIZE)
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        Variables.GetValue();
        
        // Add counter + MAX_SIZE
        PLA  // Restore counter value (5)
        CLC
        ADC ZP.TOPL  // Add MAX_SIZE low byte
        STA ZP.U0    // Store result low byte
        
        LDA #0
        ADC ZP.TOPH  // Add MAX_SIZE high byte with carry
        STA ZP.U1    // Store result high byte
        
        // Subtract 1: result = counter + MAX_SIZE - 1 = 5 + 1024 - 1 = 1028
        LDA ZP.U0
        SEC
        SBC #1
        STA ZP.U0
        
        LDA ZP.U1
        SBC #0
        STA ZP.U1
        
        // Verify result: 1028 = 0x0404
        LDA ZP.U0
        CMP #(1028 % 256)
        if (NZ)
        {
            LDA #0x86
            CLC  // Fail - wrong expression result low byte
            Test.PrintResult();
            return;
        }
        
        LDA ZP.U1
        CMP #(1028 / 256)
        if (NZ)
        {
            LDA #0x87
            CLC  // Fail - wrong expression result high byte
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up all variables and constants
        Functions.Clear();  // Clean up all functions
        SEC  // Pass
        Test.PrintResult();
    }
    
    
    testSymbolTableSerializationReadiness()
    {
        LDA #(scenarioDesc7 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc7 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Step 1: Create comprehensive program state for serialization testing
        // This simulates a complete HopperBASIC session ready for SAVE command
        
        // Variables: INT counter = 42, BIT flag = 1, WORD size = 512
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCT
        
        LDA #42
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens for basic variables
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x01
            CLC  // Fail - INT variable declaration failed
            Test.PrintResult();
            return;
        }
        
        LDA #(varName1 % 256)  // "X" as flag
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.BIT)
        STA ZP.ACCT
        
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x02
            CLC  // Fail - BIT variable declaration failed
            Test.PrintResult();
            return;
        }
        
        LDA #(varName3 % 256)  // "STATUS" as size
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCT
        
        LDA #(512 % 256)
        STA ZP.NEXTL
        LDA #(512 / 256)
        STA ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x03
            CLC  // Fail - WORD variable declaration failed
            Test.PrintResult();
            return;
        }
        
        // Constants: CONST PI = 314, CONST VERSION = 1
        LDA #(constName1 % 256)  // "PI"
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCT
        
        LDA #(314 % 256)
        STA ZP.NEXTL
        LDA #(314 / 256)
        STA ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x04
            CLC  // Fail - PI constant declaration failed
            Test.PrintResult();
            return;
        }
        
        LDA #(mainProgName % 256)  // "main" as VERSION
        STA ZP.TOPL
        LDA #(mainProgName / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
        STA ZP.ACCT
        
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x05
            CLC  // Fail - VERSION constant declaration failed
            Test.PrintResult();
            return;
        }
        
        // Functions with tokens: FUNC CALCULATE(VALUE), FUNC PRINT()
        allocateTestTokens();  // Function body tokens for CALCULATE
        
        LDA #(regularFuncName % 256)  // "FOO" as CALCULATE
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCT
        
        STZ ZP.NEXTL  // Arguments added separately
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0x06
            CLC  // Fail - CALCULATE function declaration failed
            Test.PrintResult();
            return;
        }
        
        // Add argument to CALCULATE function
        Functions.Find();  // Find CALCULATE to add argument
        if (NC)
        {
            LDA #0x07
            CLC  // Fail - could not find CALCULATE function
            Test.PrintResult();
            return;
        }
        
        LDA #(varName1 % 256)  // "X" as VALUE argument
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #BasicType.INT
        STA ZP.ACCT
        
        Arguments.Add();
        if (NC)
        {
            LDA #0x08
            CLC  // Fail - could not add VALUE argument
            Test.PrintResult();
            return;
        }
        
        // Step 2: Test complete state enumeration for SAVE preparation
        // Calculate total size needed for EEPROM storage simulation
        
        LDX #0  // Variable count
        LDY #0  // Constant count
        LDA #0  // Function count (stored in zero page)
        STA ZP.U0
        
        // Debug: Check if Variables list head pointer is valid
        LDA ZP.VariablesListL
        ORA ZP.VariablesListH
        if (Z)
        {
            LDA #0x09
            CLC  // Fail - Variables list head pointer is null (debug check)
            Test.PrintResult();
            return;
        }
        
        // First, verify we can find individual symbols to ensure they exist
        // Clear any iteration state that might interfere
        STZ ZP.IDXL
        STZ ZP.IDXH
        
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type
        
        Variables.Find();
        if (NC)
        {
            LDA #0x0A
            CLC  // Fail - COUNTER variable not found (debug check)
            Test.PrintResult();
            return;
        }
        
        // Clear iteration state again before IterateAll
        STZ ZP.IDXL
        STZ ZP.IDXH
        
        // Count all variables and constants
        Variables.IterateAll();
        if (NC)
        {
            LDA #0x0B
            CLC  // Fail - no symbols found for serialization enumeration
            Test.PrintResult();
            return;
        }
        
        loop
        {
            // Get symbol signature
            Variables.GetSignature();
            if (NC)
            {
                LDA #0x0C
                CLC  // Fail - could not get symbol signature for serialization
                Test.PrintResult();
                return;
            }
            
            LDA ZP.ACCT
            AND #0xF0  // Extract symbol type
            CMP #(SymbolType.VARIABLE << 4)
            if (Z)
            {
                INX  // Count variable
                
                // Verify all variable data accessible for serialization
                Variables.GetName();
                if (NC)
                {
                    LDA #0x0D
                    CLC  // Fail - variable name not accessible for serialization
                    Test.PrintResult();
                    return;
                }
                
                Variables.GetValue();
                if (NC)
                {
                    LDA #0x0E
                    CLC  // Fail - variable value not accessible for serialization
                    Test.PrintResult();
                    return;
                }
                
                // Check if variable has tokens (for expression storage)
                Variables.GetTokens();
                // Note: NC is acceptable here - not all variables have tokens
            }
            else
            {
                CMP #(SymbolType.CONSTANT << 4)
                if (Z)
                {
                    INY  // Count constant
                    
                    // Verify all constant data accessible for serialization
                    Variables.GetName();
                    if (NC)
                    {
                        LDA #0x0F
                        CLC  // Fail - constant name not accessible for serialization
                        Test.PrintResult();
                        return;
                    }
                    
                    Variables.GetValue();
                    if (NC)
                    {
                        LDA #0x10
                        CLC  // Fail - constant value not accessible for serialization
                        Test.PrintResult();
                        return;
                    }
                }
            }
            
            Variables.IterateNext();
            if (NC) 
            { 
                break; 
            }
        }
        
        TYA  // Constant count
        CMP #2
        if (NZ)
        {
            LDA #0x1B
            CLC  // Fail - wrong constant count for serialization
            Test.PrintResult();
            return;
        }
        TXA  // Variable count
        CMP #3
        if (NZ)
        {
            LDA #0x1A
            CLC  // Fail - wrong variable count for serialization
            Test.PrintResult();
            return;
        }
        
        // Count all functions
        Functions.IterateFunctions();
        if (NC)
        {
            LDA #0x11
            CLC  // Fail - no functions found for serialization enumeration
            Test.PrintResult();
            return;
        }
        
        loop
        {
            INC ZP.U0  // Count function
            
            // Verify all function data accessible for serialization
            Functions.GetName();
            if (NC)
            {
                LDA #0x12
                CLC  // Fail - function name not accessible for serialization
                Test.PrintResult();
                return;
            }
            
            // Get return type using Objects.GetData()
            Objects.GetData();
            if (NC)
            {
                LDA #0x13
                CLC  // Fail - function return type not accessible for serialization
                Test.PrintResult();
                return;
            }
            
            Functions.GetBody();
            if (NC)
            {
                LDA #0x14
                CLC  // Fail - function body not accessible for serialization
                Test.PrintResult();
                return;
            }
            
            // Check function arguments for serialization
            Functions.GetArguments();
            if (C)  // Function has arguments
            {
                // Save function node address
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Set IDX to arguments list for Arguments.GetCount()
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                
                Arguments.GetCount();
                if (NC)
                {
                    PLA  // Clean stack
                    PLA
                    LDA #0x15
                    CLC  // Fail - function argument count not accessible
                    Test.PrintResult();
                    return;
                }
                
                LDA ZP.ACCL  // Argument count
                if (NZ)
                {
                    // Function has arguments - verify all argument data accessible
                    Arguments.IterateStart();
                    if (NC)
                    {
                        PLA  // Clean stack
                        PLA
                        LDA #0x16
                        CLC  // Fail - cannot start argument iteration for serialization
                        Test.PrintResult();
                        return;
                    }
                    
                    LDA ZP.ACCL  // Load argument count for loop
                    STA ZP.U4   // Save argument count
                    loop
                    {
                        Arguments.GetName();
                        if (NC)
                        {
                            PLA  // Clean stack
                            PLA
                            LDA #0x17
                            CLC  // Fail - argument name not accessible for serialization
                            Test.PrintResult();
                            return;
                        }
                        
                        DEC ZP.U4  // Decrement argument count
                        if (Z) 
                        { 
                            break; 
                        }
                        
                        Arguments.IterateNext();
                        if (NC)
                        {
                            PLA  // Clean stack
                            PLA
                            LDA #0x19
                            CLC  // Fail - argument iteration failed during serialization
                            Test.PrintResult();
                            return;
                        }
                    }
                }
                
                // Restore function node address
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
            
            Functions.IterateNext();
            if (NC) 
            { 
                break; 
            }
        }
        
        // Step 3: Verify expected counts for complete program state
        // 3 variables + 2 constants + 1 function = 6 total symbols
                       
        LDA ZP.U0  // Function count
        CMP #1
        if (NZ)
        {
            LDA #0x1C
            CLC  // Fail - wrong function count for serialization
            Test.PrintResult();
            return;
        }
        
        // Step 4: Test consistent iteration order for reliable save/restore
        // Run iteration multiple times to verify deterministic ordering
        
        // First iteration - save symbol names in order
        Variables.IterateAll();
        if (NC)
        {
            LDA #0x1D
            CLC  // Fail - first iteration failed
            Test.PrintResult();
            return;
        }
        
        // Get name of first symbol
        Variables.GetName();
        if (NC)
        {
            LDA #0x1E
            CLC  // Fail - could not get first symbol name
            Test.PrintResult();
            return;
        }
        
        // Store first character of first symbol name for comparison
        LDY #0
        LDA [ZP.TOP], Y  // Get first character of symbol name
        STA ZP.U1  // Save for comparison
        
        // Get name of second symbol
        Variables.IterateNext();
        if (NC)
        {
            LDA #0x1F
            CLC  // Fail - could not get second symbol
            Test.PrintResult();
            return;
        }
        
        Variables.GetName();
        if (NC)
        {
            LDA #0x20
            CLC  // Fail - could not get second symbol name
            Test.PrintResult();
            return;
        }
        
        // Store first character of second symbol name
        LDY #0
        LDA [ZP.TOP], Y
        STA ZP.U2  // Save for comparison
        
        // Second iteration - verify same ordering
        Variables.IterateAll();
        if (NC)
        {
            LDA #0x21
            CLC  // Fail - second iteration failed
            Test.PrintResult();
            return;
        }
        
        Variables.GetName();
        LDY #0
        LDA [ZP.TOP], Y
        CMP ZP.U1  // Compare with first iteration
        if (NZ)
        {
            LDA #0x22
            CLC  // Fail - iteration order changed between runs
            Test.PrintResult();
            return;
        }
        
        Variables.IterateNext();
        Variables.GetName();
        LDY #0
        LDA [ZP.TOP], Y
        CMP ZP.U2  // Compare with first iteration
        if (NZ)
        {
            LDA #0x23
            CLC  // Fail - second symbol order changed between runs
            Test.PrintResult();
            return;
        }
        
        // Step 5: Test size calculation for EEPROM storage planning
        // This simulates calculating how much EEPROM space is needed
        
        LDA #0  // Initialize total size counter
        STA ZP.W0  // Low byte
        STA ZP.W1  // High byte
        
        // Calculate approximate size: each symbol needs ~16 bytes minimum
        // Variables: 3 * 16 = 48 bytes
        LDA ZP.W0
        CLC
        ADC #48
        STA ZP.W0
        LDA ZP.W1
        ADC #0
        STA ZP.W1
        
        // Constants: 2 * 16 = 32 bytes
        LDA ZP.W0
        CLC
        ADC #32
        STA ZP.W0
        LDA ZP.W1
        ADC #0
        STA ZP.W1
        
        // Functions: 1 * 32 = 32 bytes (functions need more space)
        LDA ZP.W0
        CLC
        ADC #32
        STA ZP.W0
        LDA ZP.W1
        ADC #0
        STA ZP.W1
        
        // Total should be approximately 112 bytes (0x0070)
        LDA ZP.W0
        CMP #112
        if (NZ)
        {
            LDA #0x24
            CLC  // Fail - size calculation low byte incorrect
            Test.PrintResult();
            return;
        }
        
        LDA ZP.W1
        CMP #0
        if (NZ)
        {
            LDA #0x25
            CLC  // Fail - size calculation high byte incorrect
            Test.PrintResult();
            return;
        }
        
        // Step 6: Test session state completeness verification
        // This ensures no data is missed during save/restore operations
        
        // Verify we can access every piece of data needed for complete restore
        LDX #0  // Success counter
        
        // Test variable data completeness
        Variables.IterateAll();
        loop
        {
            // Get symbol signature
            Variables.GetSignature();
            
            LDA ZP.ACCT
            AND #0xF0  // Extract symbol type
            CMP #(SymbolType.VARIABLE << 4)
            if (Z)
            {
                INX  // Count variable
            }
            else
            {
                CMP #(SymbolType.CONSTANT << 4)
                if (Z)
                {
                    INY  // Count constant
                }
             }
            
            Variables.IterateNext();
            if (NC) { break; }
        }

        
        // Should have verified 3 variables
        TXA
        CMP #3
        if (NZ)
        {
            LDA #0x26
            CLC  // Fail - incomplete variable data verification
            Test.PrintResult();
            return;
        }
        
        // Test function data completeness
        LDX #0  // Reset counter
        Functions.IterateFunctions();
        loop
        {
            // Function found - verify complete data set
            Functions.GetName();
            
            // Get return type using Objects.GetData()
            Objects.GetData();
            // Return type is in low nibble of ZP.ACCT
            
            Functions.GetBody();
            
            Functions.GetArguments();
            if (C)  // Has arguments
            {
                // Save function node address
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Set IDX to arguments list for GetCount
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                
                Arguments.GetCount();
                
                // Restore function node address
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
            
            INX  // Count successful function verification
            
            Functions.IterateNext();
            if (NC) 
            { 
                break; 
            }
        }
        
        // Should have verified 1 function
        TXA
        CMP #1
        if (NZ)
        {
            LDA #0x27
            CLC  // Fail - incomplete function data verification
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up all symbols
        Functions.Clear();
        SEC  // Pass
        Test.PrintResult();
    }
    
    testListCommandDataRetrieval()
    {
        LDA #(scenarioDesc6 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc6 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Step 1: Create complete program environment for LIST command
        // Variables: INT X = 10, WORD COUNTER = 0
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCT
        
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens for basic variables
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x90
            CLC  // Fail - INT variable declaration failed
            Test.PrintResult();
            return;
        }
        
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCT
        
        STZ ZP.NEXTL  // Value = 0
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x91
            CLC  // Fail - WORD variable declaration failed
            Test.PrintResult();
            return;
        }
        
        // Constants: CONST PI = 314, CONST MAX = 100
        LDA #(constName1 % 256)  // "PI"
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCT
        
        LDA #(314 % 256)
        STA ZP.NEXTL
        LDA #(314 / 256)
        STA ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x92
            CLC  // Fail - PI constant declaration failed
            Test.PrintResult();
            return;
        }
        
        LDA #(varName3 % 256)  // "STATUS" repurposed as "MAX"
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
        STA ZP.ACCT
        
        LDA #100
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x93
            CLC  // Fail - MAX constant declaration failed
            Test.PrintResult();
            return;
        }
        
        // Functions: FUNC ADD(A, B) returning INT
        allocateTestTokens();  // Function body tokens
        
        LDA #(regularFuncName % 256)  // "FOO" repurposed as "ADD"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCT
        
        STZ ZP.NEXTL  // Arguments added separately
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0x94
            CLC  // Fail - ADD function declaration failed
            Test.PrintResult();
            return;
        }
        
        // Add arguments to ADD function
        Functions.Find();  // Find ADD function to add arguments
        if (NC)
        {
            LDA #0x95
            CLC  // Fail - could not find ADD function to add arguments
            Test.PrintResult();
            return;
        }
        
        // Add argument A (INT)
        LDA #(varName1 % 256)  // "X" repurposed as argument "A"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #BasicType.INT
        STA ZP.ACCT
        
        Arguments.Add();
        if (NC)
        {
            LDA #0x96
            CLC  // Fail - could not add argument A
            Test.PrintResult();
            return;
        }
        
        // Add argument B (INT)
        LDA #(varName2 % 256)  // "COUNTER" repurposed as argument "B"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        
        LDA #BasicType.INT
        STA ZP.ACCT
        
        Arguments.Add();
        if (NC)
        {
            LDA #0x97
            CLC  // Fail - could not add argument B
            Test.PrintResult();
            return;
        }
        
        // FUNC PRINT() returning VOID (no arguments)
        allocateTestTokens();  // New function body tokens
        
        LDA #(mainProgName % 256)  // "main" repurposed as "PRINT"
        STA ZP.TOPL
        LDA #(mainProgName / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.VOID)
        STA ZP.ACCT
        
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0x98
            CLC  // Fail - PRINT function declaration failed
            Test.PrintResult();
            return;
        }
        
        // Step 2: Test Variables.IterateAll() for listing all variables/constants
        // This simulates the VARS command or variable section of LIST
        
        LDX #0  // Variable counter
        LDY #0  // Constant counter
        
        Variables.IterateAll();
        if (NC)
        {
            LDA #0x99
            CLC  // Fail - no symbols found for variable iteration
            Test.PrintResult();
            return;
        }
        
        loop
        {
            // Get symbol signature to determine type
            Variables.GetSignature();
            
            LDA ZP.ACCT
            AND #0xF0  // Extract symbol type (high nibble)
            
            CMP #(SymbolType.VARIABLE << 4)
            if (Z)
            {
                // This is a variable - verify we can get name, type, and value
                Variables.GetName();
                if (NC)
                {
                    LDA #0x9A
                    CLC  // Fail - could not get variable name
                    Test.PrintResult();
                    return;
                }
                
                Variables.GetValue();
                if (NC)
                {
                    LDA #0x9B
                    CLC  // Fail - could not get variable value
                    Test.PrintResult();
                    return;
                }
                
                // Count this variable
                INX
            }
            else
            {
                CMP #(SymbolType.CONSTANT << 4)
                if (Z)
                {
                    // This is a constant - verify accessibility
                    Variables.GetName();
                    if (NC)
                    {
                        LDA #'a' Debug.COut(); 
                        
                        LDA #0x9C
                        CLC  // Fail - could not get constant name
                        Test.PrintResult();
                        return;
                    }
                    
                    Variables.GetValue();
                    if (NC)
                    {
                        LDA #0x9D
                        CLC  // Fail - could not get constant value
                        Test.PrintResult();
                        return;
                    }
                    
                    // Count this constant
                    INY
                }
            }
            Variables.IterateNext();
            if (NC) 
            { 
                LDA #'X' Debug.COut();
                break; 
            }
        }
        
        // Verify expected counts: 2 variables + 2 constants = 4 total
        TXA  // Variable count
        CMP #2
        if (NZ)
        {
            LDA #0x9E
            CLC  // Fail - wrong variable count in listing
            Test.PrintResult();
            return;
        }
        
        TYA  // Constant count
        CMP #2
        if (NZ)
        {
            LDA #0x9F
            CLC  // Fail - wrong constant count in listing
            Test.PrintResult();
            return;
        }
        
        // Step 3: Test Functions.IterateFunctions() for listing all functions
        // This simulates the function section of LIST command
        
        LDX #0  // Function counter
        
        Functions.IterateFunctions();
        if (NC)
        {
            LDA #0x90  // Reuse error codes for function section
            CLC  // Fail - no functions found for iteration
            Test.PrintResult();
            return;
        }
        
        loop
        {
            // Get function return type using Objects.GetData()
            Objects.GetData();
            // Return type is in low nibble of ZP.ACCT
            if (NC)
            {
                LDA #0x91
                CLC  // Fail - could not get function return type
                Test.PrintResult();
                return;
            }
            
            // Get function name
            Functions.GetName();
            if (NC)
            {
                LDA #0x92
                CLC  // Fail - could not get function name
                Test.PrintResult();
                return;
            }
            
            // Get function body for potential token display
            Functions.GetBody();
            if (NC)
            {
                LDA #0x93
                CLC  // Fail - could not get function body
                Test.PrintResult();
                return;
            }
            
            // Check if function has arguments
            Functions.GetArguments();
            if (C)  // Function has arguments
            {
                // Save function node address
                LDA ZP.IDXL
                PHA
                LDA ZP.IDXH
                PHA
                
                // Set IDX to arguments list for Arguments operations
                LDA ZP.IDYL
                STA ZP.IDXL
                LDA ZP.IDYH
                STA ZP.IDXH
                
                Arguments.GetCount();
                if (NC)
                {
                    PLA  // Clean stack
                    PLA
                    LDA #0x94
                    CLC  // Fail - could not get argument count
                    Test.PrintResult();
                    return;
                }
                
                LDA ZP.ACCL  // Argument count
                if (NZ)
                {
                    // Function has arguments - test argument iteration
                    Arguments.IterateStart();
                    if (NC)
                    {
                        PLA  // Clean stack
                        PLA
                        LDA #0x95
                        CLC  // Fail - could not start argument iteration
                        Test.PrintResult();
                        return;
                    }
                    
                    // Iterate through all arguments using IterateNext pattern
                    loop
                    {
                        // Get argument name
                        Arguments.GetName();
                        if (NC)
                        {
                            PLA  // Clean stack
                            PLA
                            LDA #0x96
                            CLC  // Fail - could not get argument name
                            Test.PrintResult();
                            return;
                        }
                        
                        // Move to next argument
                        Arguments.IterateNext();
                        if (NC) 
                        { 
                            break;  // No more arguments - this is normal exit
                        }
                    }
                }
                
                // Restore function node address
                PLA
                STA ZP.IDXH
                PLA
                STA ZP.IDXL
            }
            
            // Count this function
            INX
            
            Functions.IterateNext();
            if (NC) 
            { 
                break; 
            }
        }
        
        // Verify expected function count: 2 functions (ADD and PRINT)
        TXA  // Function count
        CMP #2
        if (NZ)
        {
            LDA #0x99
            CLC  // Fail - wrong function count in listing
            Test.PrintResult();
            return;
        }
        
        // Step 4: Test specific function details for LIST command accuracy
        // Verify ADD function details
        LDA #(regularFuncName % 256)  // "FOO" (ADD)
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            LDA #0x9A
            CLC  // Fail - could not find ADD function for detailed check
            Test.PrintResult();
            return;
        }
        
        // Get return type using Objects.GetData()
        Objects.GetData();
        LDA ZP.ACCT
        AND #0x0F  // Extract return type (low nibble)
        CMP #BasicType.INT  // Return type
        if (NZ)
        {
            LDA #0x9B
            CLC  // Fail - wrong ADD function return type
            Test.PrintResult();
            return;
        }
        
        Functions.GetArguments();
        if (NC)
        {
            LDA #'b' Debug.COut();
            
            LDA #0x9C
            CLC  // Fail - ADD function should have arguments
            Test.PrintResult();
            return;
        }
        
        // Input: ZP.IDX = function node address
        Arguments.GetCount();
        
        LDA ZP.ACCL
        CMP #2  // Should have 2 arguments
        if (NZ)
        {
            PLA  // Clean stack
            PLA
            
            LDA #'c' Debug.COut(); Tools.AOut();
            
            LDA #0x9C
            CLC  // Fail - wrong ADD function argument count
            Test.PrintResult();
            return;
        }
        
        // Verify PRINT function details
        LDA #(mainProgName % 256)  // "main" (PRINT)
        STA ZP.TOPL
        LDA #(mainProgName / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            LDA #0x9D
            CLC  // Fail - could not find PRINT function for detailed check
            Test.PrintResult();
            return;
        }
        
        // Get return type using Objects.GetData()
        Objects.GetData();
        LDA ZP.ACCT
        AND #0x0F  // Extract return type (low nibble)
        CMP #BasicType.VOID  // Return type
        if (NZ)
        {
            LDA #0x9E
            CLC  // Fail - wrong PRINT function return type
            Test.PrintResult();
            return;
        }
        
        Functions.GetArguments();
        if (C)
        {
            LDA #0x9F
            CLC  // Fail - PRINT function should have no arguments
            Test.PrintResult();
            return;
        }
        
        // Step 5: Test declaration order consistency for LIST command
        // LIST command needs symbols in declaration order for proper display
        
        // Re-iterate variables and verify we get consistent ordering
        Variables.IterateAll();
        if (NC)
        {
            LDA #0x90  // Final error code section
            CLC  // Fail - second variable iteration failed
            Test.PrintResult();
            return;
        }
        
        // Get first symbol and verify it's one of our declared variables
        Variables.GetName();
        if (NC)
        {
            LDA #0x91
            CLC  // Fail - could not get first symbol name in second iteration
            Test.PrintResult();
            return;
        }
        
        // Don't need to verify exact order, just that iteration is consistent
        Variables.IterateNext();  // Move to second symbol
        if (NC)
        {
            LDA #0x92
            CLC  // Fail - second symbol not accessible in consistent iteration
            Test.PrintResult();
            return;
        }
        
        Variables.GetName();
        if (NC)
        {
            LDA #0x93
            CLC  // Fail - could not get second symbol name in consistent iteration
            Test.PrintResult();
            return;
        }
        
        // Step 6: Test complete program reconstruction capability
        // This verifies LIST command can reconstruct the entire program structure
        
        // Count total accessible symbols across both namespaces
        LDX #0  // Total symbol counter
        
        // Count all variables/constants
        Variables.IterateAll();
        if (C)  // Only if symbols exist
        {
            loop
            {
                INX  // Count this symbol
                Variables.IterateNext();
                if (NC) 
                { 
                    break; 
                }
            }
        }
        
        // Count all functions
        Functions.IterateFunctions();
        if (C)  // Only if functions exist
        {
            loop
            {
                INX  // Count this function
                Functions.IterateNext();
                if (NC) 
                { 
                    break; 
                }
            }
        }
        
        // Verify total symbol accessibility: 4 variables/constants + 2 functions = 6 total
        TXA  // Total symbol count
        CMP #6
        if (NZ)
        {
            LDA #0x94
            CLC  // Fail - wrong total symbol count for complete program listing
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up all variables and constants
        Functions.Clear();  // Clean up all functions
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Run all scenario tests
    RunScenarioTests()
    {
        testSafeSymbolCreationPattern();
        testMixedGlobalSymbolUsage();
        testListCommandDataRetrieval();
        testSymbolTableSerializationReadiness();
    }
}
