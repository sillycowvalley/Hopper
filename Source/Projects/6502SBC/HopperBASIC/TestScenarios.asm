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
        STZ ZP.ACCL  // Any type
        
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
        STZ ZP.ACCL  // Any type
        
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
        STZ ZP.ACCL  // Any type
        
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
        STZ ZP.ACCL  // Any type
        
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
        STZ ZP.ACCL
        
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
        
        // TODO: Implement main program storage test
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        CLC  // Not implemented yet
        Test.PrintResult();
    }
    
    testForgetCommandIntegration()
    {
        LDA #(scenarioDesc4 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // TODO: Implement FORGET command test
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        CLC  // Not implemented yet
        Test.PrintResult();
    }
    
    testTokenMemoryLifecycle()
    {
        LDA #(scenarioDesc5 % 256)
        STA ZP.TOPL
        LDA #(scenarioDesc5 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // TODO: Implement token memory lifecycle test
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        CLC  // Not implemented yet
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
        
        // TODO: Implement safe symbol creation pattern test
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        CLC  // Not implemented yet
        Test.PrintResult();
    }
    
    // Run all scenario tests
    RunScenarioTests()
    {
        testVariableReassignmentAfterDeclaration();
        testClearCommandImplementation();
        /* TODO
        testMainProgramStorage();
        testForgetCommandIntegration();
        testTokenMemoryLifecycle();
        testListCommandDataRetrieval();
        testSymbolTableSerializationReadiness();
        testMixedGlobalSymbolUsage();
        testSafeSymbolCreationPattern();
        */
    }
}
