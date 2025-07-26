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

    LDA #'S'
    Tools.COut();  // Signal start of rapid updates
    TYA
    Tools.HOut();  // Show initial Y value

    loop
    {
        LDA #'L'
        Tools.COut();  // Show we're in the loop
        TYA
        Tools.HOut();  // Show current Y value
        
        // Set value to X register value
        TXA
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();
        if (NC)
        {
            LDA #'F'
            Tools.COut();  // SetValue failed
            LDA #0x1C
            CLC  // Fail - rapid update failed
            Test.PrintResult();
            return;
        }
        
        LDA #'G'
        Tools.COut();  // SetValue succeeded
        
        // Verify the value matches
        Variables.GetValue();
        if (NC)
        {
            LDA #'H'
            Tools.COut();  // GetValue failed
            LDA #0x1D
            CLC  // Fail - rapid GetValue failed
            Test.PrintResult();
            return;
        }
        
        LDA #'V'
        Tools.COut();  // GetValue succeeded
        
        TXA
        CMP ZP.TOPL
        if (NZ)
        {
            LDA #'M'
            Tools.COut();  // Value mismatch
            LDA #0x1E
            CLC  // Fail - rapid update value mismatch
            Test.PrintResult();
            return;
        }
        
        LDA #'O'
        Tools.COut();  // Values match
        
        INX  // Next value
        DEY
        
        LDA #'D'
        Tools.COut();  // Decremented Y
        TYA
        Tools.HOut();  // Show Y after decrement
        
        if (NZ) 
        { 
            LDA #'C'
            Tools.COut();  // Continuing loop
            continue; 
        }
        
        LDA #'E'
        Tools.COut();  // Exiting loop
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
        
        // TODO: Implement CLEAR command test
        LDA #(Messages.NotImplemented % 256)
        STA ZP.LastErrorL
        LDA #(Messages.NotImplemented / 256)
        STA ZP.LastErrorH
        CLC  // Not implemented yet
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
        testMainProgramStorage();
        testForgetCommandIntegration();
        testTokenMemoryLifecycle();
        testListCommandDataRetrieval();
        testSymbolTableSerializationReadiness();
        testMixedGlobalSymbolUsage();
        testSafeSymbolCreationPattern();
    }
}
