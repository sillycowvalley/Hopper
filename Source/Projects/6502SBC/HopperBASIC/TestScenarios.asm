unit TestScenarios
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "/Source/Runtime/6502/Memory"
    uses "Variables"
    uses "Functions"
    uses "BasicTypes"
    
    // Test data for integration scenarios
    const string varName1 = "X";
    const string varName2 = "COUNTER";
    const string varName3 = "STATUS";
    const string constName1 = "PI";
    const string mainProgName = "main";
    const string regularFuncName = "FOO";
    
    
    // Test descriptions
    const string scenarioDesc1 = "Variable reassignment after declaration";
    const string scenarioDesc2 = "CLEAR command implementation";
    const string scenarioDesc3 = "Main program storage (BEGIN/END)";
    const string scenarioDesc4 = "FORGET command integration";
    const string scenarioDesc5 = "Token memory lifecycle";
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
    
    // Scenario 1: Variable Reassignment After Declaration
    // Tests that global variables can be updated multiple times after initial declaration
    // Methods tested: Variables.Find(), Variables.SetValue(), Variables.GetValue()
    testVariableReassignmentAfterDeclaration()
    {
        LDA #(scenarioDesc1 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc1 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Step 1: Declare INT X = 10
        LDA #(varName1 % 256)
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x10
            CLC  // Fail - initial declaration failed
            Test.PrintResult();
            return;
        }
        
        // Step 2: Use Variables.Find("X", 0) to locate variable and verify initial value
        LDA #(varName1 % 256)
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type
        
        Variables.Find();
        if (NC)
        {
            LDA #0x11
            CLC  // Fail - could not find declared variable
            Test.PrintResult();
            return;
        }
        
        // Variables.Find() sets ZP.IDX to variable node - this is preserved by GetValue() and SetValue()
        Variables.GetValue();
        if (NC)
        {
            LDA #0x12
            CLC  // Fail - GetValue failed
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPL
        CMP #10
        if (NZ)
        {
            LDA #0x13
            CLC  // Fail - wrong initial value
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPT
        CMP #BasicType.INT
        if (NZ)
        {
            LDA #0x14
            CLC  // Fail - wrong initial type
            Test.PrintResult();
            return;
        }
        
        // Step 3: Update X = 20 using Variables.SetValue()
        // ZP.IDX still points to variable from Variables.Find() above
        LDA #20
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();
        if (NC)
        {
            LDA #0x15
            CLC  // Fail - first update failed
            Test.PrintResult();
            return;
        }
        
        // Verify value is now 20 - ZP.IDX preserved by SetValue()
        Variables.GetValue();
        if (NC)
        {
            LDA #0x16
            CLC  // Fail - GetValue after first update failed
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPL
        CMP #20
        if (NZ)
        {
            LDA #0x17
            CLC  // Fail - wrong value after first update
            Test.PrintResult();
            return;
        }
        
        // Step 4: Update X = 30 using Variables.SetValue()
        // ZP.IDX still preserved from original Find()
        LDA #30
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();
        if (NC)
        {
            LDA #0x18
            CLC  // Fail - second update failed
            Test.PrintResult();
            return;
        }
        
        // Step 5: Verify final value is 30 with Variables.GetValue()
        Variables.GetValue();
        if (NC)
        {
            LDA #0x19
            CLC  // Fail - final GetValue failed
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPL
        CMP #30
        if (NZ)
        {
            LDA #0x1A
            CLC  // Fail - wrong final value
            Test.PrintResult();
            return;
        }
        
        // Verify type is still INT
        LDA ZP.TOPT
        CMP #BasicType.INT
        if (NZ)
        {
            LDA #0x1B
            CLC  // Fail - type changed during updates
            Test.PrintResult();
            return;
        }
        
        // Test multiple rapid updates to simulate real usage
        // ZP.IDX still valid from original Find()
        LDX #100  // Start value
        LDY #5    // Number of updates
        
        loop
        {
            // Set value to X register value
            TXA
            STA ZP.TOPL
            STZ ZP.TOPH
            Variables.SetValue();
            if (NC)
            {
                LDA #0x1C
                CLC  // Fail - rapid update failed
                Test.PrintResult();
                return;
            }
            
            // Verify the value matches
            Variables.GetValue();
            if (NC)
            {
                LDA #0x1D
                CLC  // Fail - rapid GetValue failed
                Test.PrintResult();
                return;
            }
            
            TXA
            CMP ZP.TOPL
            if (NZ)
            {
                LDA #0x1E
                CLC  // Fail - rapid update value mismatch
                Test.PrintResult();
                return;
            }
            
            INX  // Next value
            DEY
            if (NZ) { continue; }
            break;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Placeholder for additional scenario tests
    // These will be implemented in subsequent iterations
    
    testClearCommandImplementation()
    {
        LDA #(scenarioDesc2 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc2 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens for declarations
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Step 1: Declare INT X = 10, WORD Y = 100, BIT Z = 1
        LDA #(varName1 % 256)
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x20
            CLC  // Fail - X declaration failed
            Test.PrintResult();
            return;
        }
        
        LDA #(varName2 % 256)
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.WORD)
        STA ZP.ACCL
        LDA #100
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens for this variable
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x21
            CLC  // Fail - Y declaration failed
            Test.PrintResult();
            return;
        }
        
        LDA #(varName3 % 256)
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.BIT)
        STA ZP.ACCL
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens for this variable
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x22
            CLC  // Fail - Z declaration failed
            Test.PrintResult();
            return;
        }
        
        // Step 2: Declare CONST PI = 314
        LDA #(constName1 % 256)
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #(314 % 256)
        STA ZP.NEXTL
        LDA #(314 / 256)
        STA ZP.NEXTH
        
        STZ ZP.IDYL  // No tokens for this constant
        STZ ZP.IDYH
        
        Variables.Declare();
        
        if (NC)
        {
            LDA #0x23
            CLC  // Fail - PI declaration failed
            Test.PrintResult();
            return;
        }
        
        // Step 3: Modify variables to non-default values
        // Modify X = 50
        LDA #(varName1 % 256)
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type
        
        Variables.Find();
        if (NC)
        {
            LDA #0x24
            CLC  // Fail - could not find X
            Test.PrintResult();
            return;
        }
        
        LDA #50
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();
        if (NC)
        {
            LDA #0x25
            CLC  // Fail - could not set X
            Test.PrintResult();
            return;
        }
        
        // Modify Y = 200
        LDA #(varName2 % 256)
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type
        
        Variables.Find();
        if (NC)
        {
            LDA #0x26
            CLC  // Fail - could not find Y
            Test.PrintResult();
            return;
        }
        
        LDA #200
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();
        if (NC)
        {
            LDA #0x27
            CLC  // Fail - could not set Y
            Test.PrintResult();
            return;
        }
        
        // Modify Z = 0
        LDA #(varName3 % 256)
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type
        
        Variables.Find();
        if (NC)
        {
            LDA #0x28
            CLC  // Fail - could not find Z
            Test.PrintResult();
            return;
        }
        
        LDA #0
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();
        if (NC)
        {
            LDA #0x29
            CLC  // Fail - could not set Z
            Test.PrintResult();
            return;
        }
        
        // Step 4: Implement CLEAR - iterate through variables and reset to 0
        Variables.IterateVariables();
        if (NC)
        {
            LDA #0x2A
            CLC  // Fail - no variables found for clearing
            Test.PrintResult();
            return;
        }

        loop
        {
            // Reset current variable to 0 (type default)
            STZ ZP.TOPL
            STZ ZP.TOPH
            Variables.SetValue();
            if (NC)
            {
                LDA #0x2B
                CLC  // Fail - could not reset variable
                Test.PrintResult();
                return;
            }
            
            // Move to next variable
            Variables.IterateNext();
            if (NC) 
            { 
                break; 
            }
        }
        
        // Step 5: Verify variables are reset: X = 0, Y = 0, Z = 0
        LDA #(varName1 % 256)
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.ACCL
        
        Variables.Find();
        Variables.GetValue();
        LDA ZP.TOPL
        ORA ZP.TOPH
        if (NZ)
        {
            LDA #0x2C
            CLC  // Fail - X not reset to 0
            Test.PrintResult();
            return;
        }
        
        LDA #(varName2 % 256)
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        STZ ZP.ACCL
        
        Variables.Find();
        Variables.GetValue();
        LDA ZP.TOPL
        ORA ZP.TOPH
        if (NZ)
        {
            LDA #0x2D
            CLC  // Fail - Y not reset to 0
            Test.PrintResult();
            return;
        }
        
        LDA #(varName3 % 256)
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        STZ ZP.ACCL
        
        Variables.Find();
        Variables.GetValue();
        LDA ZP.TOPL
        ORA ZP.TOPH
        if (NZ)
        {
            LDA #0x2E
            CLC  // Fail - Z not reset to 0
            Test.PrintResult();
            return;
        }
        
        // Step 6: Verify constant PI = 314 (unchanged)
        LDA #(constName1 % 256)
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        if (NC)
        {
            LDA #0x2F
            CLC  // Fail - could not find PI constant
            Test.PrintResult();
            return;
        }
        
        
        
        Variables.GetValue();
        LDA ZP.TOPL
        CMP #(314 % 256)
        if (NZ)
        {
            LDA #0x30
            CLC  // Fail - PI low byte changed
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPH
        CMP #(314 / 256)
        if (NZ)
        {
            LDA #0x31
            CLC  // Fail - PI high byte changed
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    testMainProgramStorage()
    {
        LDA #(scenarioDesc3 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc3 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens for main program body
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Step 1: Store BEGIN/END block as Functions.Declare() with name "main"
        // Use lowercase "main" as system-reserved name
        LDA #(mainProgName % 256)
        STA ZP.TOPL
        LDA #(mainProgName / 256)
        STA ZP.TOPH
        
        // Use type FUNCTION|VOID to indicate no return value
        LDA #((SymbolType.FUNCTION << 4) | BasicType.VOID)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        STZ ZP.NEXTL  // No arguments for main program
        STZ ZP.NEXTH
        
        // Store main program tokens in function body
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0x40
            CLC  // Fail - main program declaration failed
            Test.PrintResult();
            return;
        }
        
        // Step 2: Verify Functions.Find("main") retrieves it for RUN command
        LDA #(mainProgName % 256)
        STA ZP.TOPL
        LDA #(mainProgName / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            LDA #0x41
            CLC  // Fail - could not find main program
            Test.PrintResult();
            return;
        }
        
        // Verify it has correct type (FUNCTION|VOID)
        Functions.GetSignature();
        LDA ZP.ACCL
        CMP #BasicType.VOID  // Compare against VOID (0x00), not the full combined type
        if (NZ)
        {
            LDA #0x42
            CLC  // Fail - wrong main program type
            Test.PrintResult();
            return;
        }
        
        // Verify function body tokens are accessible
        Functions.GetBody();
        LDA ZP.IDYL  // Changed from ZP.TOPL
        CMP ZP.U5
        if (NZ)
        {
            LDA #0x43
            CLC  // Fail - wrong token pointer low byte
            Test.PrintResult();
            return;
        }
        
        LDA ZP.IDYH  // Changed from ZP.TOPH
        CMP ZP.U6
        if (NZ)
        {
            LDA #0x44
            CLC  // Fail - wrong token pointer high byte
            Test.PrintResult();
            return;
        }        
        
        // Step 3: Verify "main" cannot be created by user (simulate user attempt)
        // This simulates what would happen if user tried: FUNC main()
        // The interpreter should check for existing "main" and prevent creation
        
        // Try to declare another function with name "main" (should fail)
        LDA #(mainProgName % 256)
        STA ZP.TOPL
        LDA #(mainProgName / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)  // Different type
        STA ZP.ACCL
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this attempt
        STZ ZP.IDYH
        
        Functions.Declare();
        if (C)
        {
            LDA #0x45
            CLC  // Fail - should not have allowed duplicate "main"
            Test.PrintResult();
            return;
        }
        
        // Step 4: Test main program replacement (legitimate system operation)
        // This simulates what happens when user enters new BEGIN/END block
        
        // First remove existing main
        LDA #(mainProgName % 256)
        STA ZP.TOPL
        LDA #(mainProgName / 256)
        STA ZP.TOPH
        
        Functions.Remove();
        if (NC)
        {
            LDA #0x46
            CLC  // Fail - could not remove main for replacement
            Test.PrintResult();
            return;
        }
        
        // Verify main is gone
        Functions.Find();
        if (C)
        {
            LDA #0x47
            CLC  // Fail - main still exists after removal
            Test.PrintResult();
            return;
        }
        
        // Allocate new tokens for replacement main
        allocateTestTokens();  // New result in ZP.U5|U6
        
        // Create new main program (simulates new BEGIN/END block)
        LDA #(mainProgName % 256)
        STA ZP.TOPL
        LDA #(mainProgName / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.VOID)
        STA ZP.ACCL
        STZ ZP.NEXTL
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0x48
            CLC  // Fail - replacement main declaration failed
            Test.PrintResult();
            return;
        }
        
        // Step 5: Verify replacement main is accessible
        Functions.Find();
        if (NC)
        {
            LDA #0x49
            CLC  // Fail - replacement main not found
            Test.PrintResult();
            return;
        }
        
        Functions.GetBody();
        LDA ZP.IDYL  // Changed from ZP.TOPL
        CMP ZP.U5
        if (NZ)
        {
            LDA #0x4A
            CLC  // Fail - replacement main wrong token pointer low
            Test.PrintResult();
            return;
        }
        
        LDA ZP.IDYH  // Changed from ZP.TOPH
        CMP ZP.U6
        if (NZ)
        {
            LDA #0x4B
            CLC  // Fail - replacement main wrong token pointer high
            Test.PrintResult();
            return;
        }
        
        // Step 6: Test main program with mixed symbol table
        // Verify main coexists properly with variables and other functions
        
        // Add a variable
        LDA #(varName1 % 256)
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        LDA #42
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for variable
        STZ ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x4C
            CLC  // Fail - variable declaration failed
            Test.PrintResult();
            return;
        }
        
        // Add a regular function
        LDA #(regularFuncName % 256)
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
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
            LDA #0x4D
            CLC  // Fail - regular function declaration failed
            Test.PrintResult();
            return;
        }
        
        // Verify main still exists and is accessible among other symbols
        LDA #(mainProgName % 256)
        STA ZP.TOPL
        LDA #(mainProgName / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            LDA #0x4E
            CLC  // Fail - main not found in mixed symbol table
            Test.PrintResult();
            return;
        }
        
        // Verify main still has correct properties
        Functions.GetSignature();
        LDA ZP.ACCL
        CMP # BasicType.VOID
        if (NZ)
        {
            LDA #0x4F
            CLC  // Fail - main type changed in mixed table
            Test.PrintResult();
            return;
        }
        
        Functions.Clear();  // Clean up functions
        Variables.Clear();  // Clean up variables
        SEC  // Pass
        Test.PrintResult();
    }
    
    
    testForgetCommandIntegration()
    {
        LDA #(scenarioDesc4 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Step 1: Declare INT X = 10 with token stream
        allocateTestTokens();  // Result in ZP.U5|U6
        
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        // Store token pointer for variable
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Variables.Declare();
        if (NC)
        {
            LDA #0x50
            CLC  // Fail - variable declaration failed
            Test.PrintResult();
            return;
        }
        
        // Step 2: Declare FUNC FOO() with body tokens
        allocateTestTokens();  // New tokens in ZP.U5|U6
        
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.INT)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        STZ ZP.NEXTL  // No arguments
        STZ ZP.NEXTH
        
        // Store token pointer for function body
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0x51
            CLC  // Fail - function declaration failed
            Test.PrintResult();
            return;
        }
        
        // Step 3: Verify both symbols exist before removal
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter  // Any type
        
        Variables.Find();
        if (NC)
        {
            LDA #0x52
            CLC  // Fail - variable X not found before removal
            Test.PrintResult();
            return;
        }
        
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            LDA #0x53
            CLC  // Fail - function FOO not found before removal
            Test.PrintResult();
            return;
        }
        
        // Step 4: Test FORGET X pattern - Find first, then remove with token cleanup
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        if (NC)
        {
            LDA #0x54
            CLC  // Fail - could not find X for removal
            Test.PrintResult();
            return;
        }
        
        // Get token pointer before removal for verification
        Variables.GetTokens();
        
        // Store token pointer to verify it gets freed
        LDA ZP.IDYL
        PHA  // Save token pointer low byte
        LDA ZP.IDYH
        PHA  // Save token pointer high byte
        
        // Remove the variable (should free tokens automatically)
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        Variables.Remove();
        if (NC)
        {
            PLA  // Clean stack
            PLA
            LDA #0x55
            CLC  // Fail - variable removal failed
            Test.PrintResult();
            return;
        }
        
        // Clean stack (token pointers no longer valid)
        PLA
        PLA
        
        // Step 5: Verify X is completely gone
        LDA #(varName1 % 256)  // "X"
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();
        if (C)
        {
            LDA #0x56
            CLC  // Fail - variable X still exists after removal
            Test.PrintResult();
            return;
        }
        
        // Step 6: Test FORGET FOO pattern - Find first, then remove
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (NC)
        {
            LDA #0x57
            CLC  // Fail - could not find FOO for removal
            Test.PrintResult();
            return;
        }
        
        // Get function body tokens before removal
        Functions.GetBody();
        
        // Store body token pointer for verification
        LDA ZP.IDYL
        PHA  // Save body token pointer low byte
        LDA ZP.IDYH
        PHA  // Save body token pointer high byte
        
        // Remove the function (should free body tokens and any arguments)
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        Functions.Remove();
        if (NC)
        {
            PLA  // Clean stack
            PLA
            LDA #0x58
            CLC  // Fail - function removal failed
            Test.PrintResult();
            return;
        }
        
        // Clean stack (body token pointers no longer valid)
        PLA
        PLA
        
        // Step 7: Verify FOO is completely gone
        LDA #(regularFuncName % 256)  // "FOO"
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        Functions.Find();
        if (C)
        {
            LDA #0x59
            CLC  // Fail - function FOO still exists after removal
            Test.PrintResult();
            return;
        }
        
        // Step 8: Test FORGET of function with arguments (complex cleanup test)
        allocateTestTokens();  // New tokens for function body
        
        LDA #(varName2 % 256)  // Use "COUNTER" as function name
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.FUNCTION << 4) | BasicType.WORD)
        STA ZP.ACCL
        STZ ZP.ACCH
        
        STZ ZP.NEXTL  // We'll add arguments separately
        STZ ZP.NEXTH
        
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Functions.Declare();
        if (NC)
        {
            LDA #0x5A
            CLC  // Fail - function with args declaration failed
            Test.PrintResult();
            return;
        }
        
        // Find the function to add arguments
        Functions.Find();
        if (NC)
        {
            LDA #0x5B
            CLC  // Fail - could not find function to add arguments
            Test.PrintResult();
            return;
        }
        
        // Add two arguments to the function
        LDA #(varName1 % 256)  // "X" as first argument
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        
        LDA #BasicType.INT
        STA ZP.ACCL
        
        Arguments.Add();
        if (NC)
        {
            LDA #0x5C
            CLC  // Fail - could not add first argument
            Test.PrintResult();
            return;
        }
        
        LDA #(varName3 % 256)  // "STATUS" as second argument
        STA ZP.TOPL
        LDA #(varName3 / 256)
        STA ZP.TOPH
        
        LDA #BasicType.BIT
        STA ZP.ACCL
        
        Arguments.Add();
        if (NC)
        {
            LDA #0x5D
            CLC  // Fail - could not add second argument
            Test.PrintResult();
            return;
        }
        
        // Verify arguments exist before removal
        Arguments.GetCount();
        LDA ZP.ACCL
        CMP #2
        if (NZ)
        {
            LDA #0x5E
            CLC  // Fail - wrong argument count before removal
            Test.PrintResult();
            return;
        }
        
        // Remove function with arguments (should cleanup arguments too)
        LDA #(varName2 % 256)  // "COUNTER"
        STA ZP.TOPL
        LDA #(varName2 / 256)
        STA ZP.TOPH
        
        Functions.Remove();
        if (NC)
        {
            LDA #0x5F
            CLC  // Fail - function with arguments removal failed
            Test.PrintResult();
            return;
        }
        
        // Verify function is completely gone
        Functions.Find();
        if (C)
        {
            LDA #0x60  // Using 0x60 as this is edge case beyond allocated range
            CLC  // Fail - function with arguments still exists after removal
            Test.PrintResult();
            return;
        }
        
        // Step 9: Test FORGET of non-existent symbol (should handle gracefully)
        // These calls should return NC for non-existent symbols - this is expected behavior
        
        // Try to remove non-existent variable "X" (already removed)
        LDA #(varName1 % 256)  
        STA ZP.TOPL
        LDA #(varName1 / 256)
        STA ZP.TOPH
        STZ ZP.SymbolIteratorFilter
        
        Variables.Find();  // Should return NC (not found)
        if (C)  // If found, then remove it
        {
            Variables.Remove();
        }
        // Either way, this is not an error condition
        
        // Try to remove non-existent function "FOO" (already removed)  
        LDA #(regularFuncName % 256)
        STA ZP.TOPL
        LDA #(regularFuncName / 256)
        STA ZP.TOPH
        
        Functions.Find();  // Should return NC (not found)
        if (C)  // If found, then remove it
        {
            Functions.Remove();
        }
        // Either way, this is not an error condition
        
        Variables.Clear();  // Clean up any remaining symbols
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
        
        // TODO: Implement LIST command data retrieval test
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        CLC  // Not implemented yet
        Test.PrintResult();
    }
    
    testSymbolTableSerializationReadiness()
    {
        LDA #(scenarioDesc7 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc7 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // TODO: Implement serialization readiness test
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        CLC  // Not implemented yet
        Test.PrintResult();
    }
    
    testMixedGlobalSymbolUsage()
    {
        LDA #(scenarioDesc8 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc8 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // TODO: Implement mixed global symbol usage test
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        CLC  // Not implemented yet
        Test.PrintResult();
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
        STA ZP.ACCL
        STZ ZP.ACCH
        
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
        STA ZP.ACCL
        STZ ZP.ACCH
        
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
        STA ZP.ACCL
        STZ ZP.ACCH
        
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
    STA ZP.ACCL
    STZ ZP.ACCH
    
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
    STA ZP.ACCL
    STZ ZP.ACCH
    
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
    STA ZP.ACCL
    STZ ZP.ACCH
    
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
    STA ZP.ACCL
    STZ ZP.ACCH
    
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

    
    // Run all scenario tests
    RunScenarioTests()
    {
        testClearCommandImplementation();
        testVariableReassignmentAfterDeclaration();
        testMainProgramStorage();
        testForgetCommandIntegration();
        testSafeSymbolCreationPattern();
        testTokenMemoryLifecycle();
        /*
        testListCommandDataRetrieval();
        testSymbolTableSerializationReadiness();
        testMixedGlobalSymbolUsage();
        */
    }
}
