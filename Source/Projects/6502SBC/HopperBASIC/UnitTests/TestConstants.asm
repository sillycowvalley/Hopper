unit TestConstants
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "../Variables"
    uses "../BasicTypes"
    
    // Private test data for constants tests
    const string constName1 = "PI";
    const string constName2 = "MAX_SIZE";
    const string constName3 = "DEBUG_FLAG";
    const string constName4 = "VERSION";
    const string constName5 = "TYPETEST";
    const string constName6 = "NAMETEST";
    const string constName7 = "REMOVETEST";
    const string invalidName = "NONEXISTENT";
    
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
    
    // Test descriptions
    const string constDesc1 = "Declare INT constant";
    const string constDesc2 = "Declare WORD constant";
    const string constDesc3 = "Declare BIT constant";
    const string constDesc4 = "Find constant by name";
    const string constDesc5 = "Get constant value";
    const string constDesc6 = "Try to modify constant (should fail)";
    const string constDesc7 = "Get constant type";
    const string constDesc8 = "Get constant tokens";
    const string constDesc9 = "Remove constant";
    const string constDesc10 = "Iterate constants only";
    const string constDesc11 = "Duplicate constant (should fail)";
    const string constDesc12 = "Constant type filtering";
    const string constDesc13 = "Get constant type via Variables";
    const string constDesc14 = "Get constant name via Variables";
    const string constDesc15 = "Remove constant via Variables";
    
    // Test 23: Declare INT constant
    testDeclareIntConstant()
    {
        LDA #'N'  // Test 23
        LDA #(constDesc1 % 256)
        STA ZP.TOPL
        LDA #(constDesc1 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare INT constant "PI" = 314
        LDA #(constName1 % 256)
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCT
        
        LDA #(314 % 256)
        STA ZP.NEXTL
        LDA #(314 / 256)
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
            LDA #0xA0
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 24: Declare WORD constant
    testDeclareWordConstant()
    {
        LDA #'O'  // Test 24
        LDA #(constDesc2 % 256)
        STA ZP.TOPL
        LDA #(constDesc2 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare WORD constant "MAX_SIZE" = 65000
        LDA #(constName2 % 256)
        STA ZP.TOPL
        LDA #(constName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
        STA ZP.ACCT
                
        LDA #(65000 % 256)
        STA ZP.NEXTL
        LDA #(65000 / 256)
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
            LDA #0xB0
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 25: Declare BIT constant
    testDeclareBitConstant()
    {
        LDA #'P'  // Test 25
        LDA #(constDesc3 % 256)
        STA ZP.TOPL
        LDA #(constDesc3 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare BIT constant "DEBUG_FLAG" = 1
        LDA #(constName3 % 256)
        STA ZP.TOPL
        LDA #(constName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.BIT)
        STA ZP.ACCT
        
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
            LDA #0xC0
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 26: Find constant by name
    testFindConstantByName()
    {
        LDA #'Q'  // Test 26
        LDA #(constDesc4 % 256)
        STA ZP.TOPL
        LDA #(constDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare constant first
        LDA #(constName4 % 256)
        STA ZP.TOPL
        LDA #(constName4 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
        STA ZP.ACCT
        LDA #200
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Variables.Declare();
        
        // Now find it with type filter
        LDA #(constName4 % 256)
        STA ZP.TOPL
        LDA #(constName4 / 256)
        STA ZP.TOPH
        LDA #SymbolType.CONSTANT
        STA ZP.SymbolIteratorFilter 
        
        Variables.Find();
        
        if (C)
        {
            Variables.Clear();  // Clean up
            SEC  // Pass
        }
        else
        {
            LDA #0xD0
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 27: Get constant value
    testGetConstantValue()
    {
        LDA #'R'  // Test 27
        LDA #(constDesc5 % 256)
        STA ZP.TOPL
        LDA #(constDesc5 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare INT constant "PI" = 314
        LDA #(constName1 % 256)
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCT
        LDA #(314 % 256)
        STA ZP.NEXTL
        LDA #(314 / 256)
        STA ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Variables.Declare();
        
        // Find and get value
        STZ ZP.SymbolIteratorFilter  // Any type
        Variables.Find();
        if (NC)
        {
            LDA #0xE0
            CLC  // Fail - not found
            Test.PrintResult();
            return;
        }
        
        Variables.GetValue();
        if (NC)
        {
            LDA #0xE1
            CLC  // Fail - GetValue failed
            Test.PrintResult();
            return;
        }
        
        // Check value and type
        LDA ZP.TOPL
        CMP #(314 % 256)
        if (NZ)
        {
            LDA #0xE2
            CLC  // Fail - wrong value low
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPH
        CMP #(314 / 256)
        if (NZ)
        {
            LDA #0xE3
            CLC  // Fail - wrong value high
            Test.PrintResult();
            return;
        }
        
        LDA ZP.TOPT
        CMP #BasicType.INT
        if (NZ)
        {
            LDA #0xE4
            CLC  // Fail - wrong type
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 28: Try to modify constant (should fail)
    testModifyConstant()
    {
        LDA #'S'  // Test 28
        LDA #(constDesc6 % 256)
        STA ZP.TOPL
        LDA #(constDesc6 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare constant
        LDA #(constName2 % 256)
        STA ZP.TOPL
        LDA #(constName2 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
        STA ZP.ACCT
        LDA #100
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Variables.Declare();
        
        // Find it
        STZ ZP.SymbolIteratorFilter 
        Variables.Find();
        
        // Try to set value (should fail)
        LDA #200
        STA ZP.TOPL
        STZ ZP.TOPH
        Variables.SetValue();
        if (C)
        {
            LDA #0xF0
            CLC  // Fail - should have failed
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass (correctly failed)
        Test.PrintResult();
    }
    
    // Test 29: Get constant tokens
    testGetConstantTokens()
    {
        LDA #'T'  // Test 29
        LDA #(constDesc8 % 256)
        STA ZP.TOPL
        LDA #(constDesc8 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Allocate test tokens FIRST and save for later comparison
        allocateTestTokens();  // Result in ZP.U5|U6
        
        // Declare constant with specific tokens pointer
        LDA #(constName3 % 256)
        STA ZP.TOPL
        LDA #(constName3 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.BIT)
        STA ZP.ACCT
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        
        // Copy tokens pointer from temporary storage
        LDA ZP.U5
        STA ZP.IDYL
        LDA ZP.U6
        STA ZP.IDYH
        
        Variables.Declare();
        
        // Find and get tokens
        STZ ZP.SymbolIteratorFilter 
        Variables.Find();
        Variables.GetTokens();
        
        // Check tokens pointer matches allocated address
        LDA ZP.NEXTL
        CMP ZP.U5
        if (NZ)
        {
            LDA #0xA1
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        LDA ZP.NEXTH
        CMP ZP.U6
        if (NZ)
        {
            LDA #0xA2
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 30: Iterate constants only
    testIterateConstantsOnly()
    {
        LDA #'U'  // Test 30
        LDA #(constDesc10 % 256)
        STA ZP.TOPL
        LDA #(constDesc10 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Add variable
        LDA #(constName1 % 256)
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.VARIABLE << 4) | BasicType.INT)
        STA ZP.ACCT
        LDA #10
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Variables.Declare();
        
        // Add constant
        LDA #(constName2 % 256)
        STA ZP.TOPL
        LDA #(constName2 / 256)
        STA ZP.TOPH
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
        STA ZP.ACCT
        LDA #20
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Variables.Declare();
        
        // Iterate constants only
        Variables.IterateConstants();
        if (NC)
        {
            LDA #0xB1
            CLC  // Fail - no constants found
            Test.PrintResult();
            return;
        }
        
        // Check type
        Variables.GetType();
        LDA ZP.ACCT
        AND #0xF0
        LSR LSR LSR LSR
        CMP #SymbolType.CONSTANT
        if (NZ)
        {
            LDA #0xB2
            CLC  // Fail - wrong type
            Test.PrintResult();
            return;
        }
        
        // Should be no more constants
        Variables.IterateNext();
        if (C)
        {
            LDA #0xB3
            CLC  // Fail - found more than one constant
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 31: Duplicate constant declaration (should fail)
    testDuplicateConstant()
    {
        LDA #'V'  // Test 31
        LDA #(constDesc11 % 256)
        STA ZP.TOPL
        LDA #(constDesc11 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare first constant
        LDA #(constName1 % 256)
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCT
        LDA #100
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Variables.Declare();
        
        if (NC)
        {
            LDA #0xC1
            CLC  // Fail - first declaration failed
            Test.PrintResult();
            return;
        }
        
        // Try to declare same constant again (should fail)
        LDA #(constName1 % 256)
        STA ZP.TOPL
        LDA #(constName1 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)  // Different type
        STA ZP.ACCT
        LDA #200
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL
        STZ ZP.IDYH
        Variables.Declare();
        
        if (C)
        {
            LDA #0xC2
            CLC  // Fail - should have failed
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass (correctly failed)
        Test.PrintResult();
    }
    
    // NEW TEST: Get constant type via Variables interface
    testGetConstantType()
    {
        LDA #'W'  // Test 32
        LDA #(constDesc13 % 256)
        STA ZP.TOPL
        LDA #(constDesc13 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare BIT constant
        LDA #(constName5 % 256)
        STA ZP.TOPL
        LDA #(constName5 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.BIT)
        STA ZP.ACCT
        LDA #1
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Find and get type
        STZ ZP.SymbolIteratorFilter  // Any type
        Variables.Find();
        if (NC)
        {
            LDA #0xA3
            CLC  // Fail - not found
            Test.PrintResult();
            return;
        }
        
        Variables.GetType();
        if (NC)
        {
            LDA #0xA4
            CLC  // Fail - GetType failed
            Test.PrintResult();
            return;
        }
        
        // Check symbolType (high nibble)
        LDA ZP.ACCT
        AND #0xF0
        LSR LSR LSR LSR
        CMP #SymbolType.CONSTANT
        if (NZ)
        {
            LDA #0xA5
            CLC  // Fail - wrong symbol type
            Test.PrintResult();
            return;
        }
        
        // Check dataType (low nibble)
        LDA ZP.ACCT
        AND #0x0F
        CMP #BasicType.BIT
        if (NZ)
        {
            LDA #0xA6
            CLC  // Fail - wrong data type
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // NEW TEST: Get constant name via Variables interface
    testGetConstantName()
    {
        LDA #'X'  // Test 33
        LDA #(constDesc14 % 256)
        STA ZP.TOPL
        LDA #(constDesc14 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare constant
        LDA #(constName6 % 256)
        STA ZP.TOPL
        LDA #(constName6 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.WORD)
        STA ZP.ACCT
        LDA #123
        STA ZP.NEXTL
        STZ ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Find and get name
        STZ ZP.SymbolIteratorFilter  // Any type
        Variables.Find();
        if (NC)
        {
            LDA #0xA7
            CLC  // Fail - not found
            Test.PrintResult();
            return;
        }
        
        Variables.GetName();
        
        // Compare name - use ZP.ACC as returned name pointer, ZP.TOP as expected name
        LDA #(constName6 % 256)
        STA ZP.TOPL
        LDA #(constName6 / 256)
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
                LDA #0xA8
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
    
    // NEW TEST: Remove constant via Variables interface
    testRemoveConstant()
    {
        LDA #'Y'  // Test 34
        LDA #(constDesc15 % 256)
        STA ZP.TOPL
        LDA #(constDesc15 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Declare constant first
        LDA #(constName7 % 256)
        STA ZP.TOPL
        LDA #(constName7 / 256)
        STA ZP.TOPH
        
        LDA #((SymbolType.CONSTANT << 4) | BasicType.INT)
        STA ZP.ACCT
        LDA # (456 % 255)
        STA ZP.NEXTL
        LDA # (456 / 256)
        STA ZP.NEXTH
        STZ ZP.IDYL  // No tokens for this test
        STZ ZP.IDYH
        Variables.Declare();
        
        // Verify it exists
        STZ ZP.SymbolIteratorFilter  // Any type
        Variables.Find();
        if (NC)
        {
            LDA #0xA9
            CLC  // Fail - constant not found after declaration
            Test.PrintResult();
            return;
        }
        
        // Remove it
        LDA #(constName7 % 256)
        STA ZP.TOPL
        LDA #(constName7 / 256)
        STA ZP.TOPH
        Variables.Remove();
        
        if (NC)
        {
            LDA #0xAA
            CLC  // Fail - remove failed
            Test.PrintResult();
            return;
        }
        
        // Verify it's gone
        Variables.Find();
        if (C)
        {
            LDA #0xAB
            CLC  // Fail - constant still exists after removal
            Test.PrintResult();
            return;
        }
        
        Variables.Clear();  // Clean up
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Run all constants tests
    RunConstantsTests()
    {
        testDeclareIntConstant();
        testDeclareWordConstant();
        testDeclareBitConstant();
        testFindConstantByName();
        testGetConstantValue();
        testModifyConstant();
        testGetConstantTokens();
        testIterateConstantsOnly();
        testDuplicateConstant();
        testGetConstantType();
        testGetConstantName();
        testRemoveConstant();
    }
}
