unit TestTable
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Table"
    
    // Private test descriptions for Table tests
    const string tableDesc1 = "Empty list operations";
    const string tableDesc2 = "Add single node";
    const string tableDesc3 = "Add multiple nodes";
    const string tableDesc4 = "Traverse list";
    const string tableDesc5 = "Delete first node";
    const string tableDesc6 = "Clear entire list";
    
    // Test 1: Empty list operations
    testEmptyList()
    {
        LDA #'1'
        LDA #(tableDesc1 % 256)
        STA ZP.TOPL
        LDA #(tableDesc1 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Clear test list
        STZ Test.TableHeadLocationL
        STZ Test.TableHeadLocationH
        
        // GetFirst should return null
        LDX #Test.TableHeadLocation
        Table.GetFirst();
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            SEC  // Pass
        }
        else
        {
            LDA #0x10
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 2: Add single node to empty list
    testAddSingle()
    {
        LDA #'2'
        LDA #(tableDesc2 % 256)
        STA ZP.TOPL
        LDA #(tableDesc2 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with a clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add 10-byte node
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        
        LDX #Test.TableHeadLocation
        Table.Add();
        
        // Should succeed (C set) and return valid address
        if (NC)
        {
            LDA #0x20
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (NZ)
        {
            // Clean up before success
            LDX #Test.TableHeadLocation
            Table.Clear();
            SEC  // Pass
        }
        else
        {
            LDA #0x21
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 3: Add multiple nodes (builds linked list with tail insertion)
    testAddMultiple()
    {
        LDA #'3'
        LDA #(tableDesc3 % 256)
        STA ZP.TOPL
        LDA #(tableDesc3 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with a clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add first node
        LDA #8
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        if (NC)
        {
            LDA #0x30
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Save first node address
        LDA ZP.IDXL
        STA Test.TableNodeL
        LDA ZP.IDXH
        STA Test.TableNodeH
        
        // Add second node  
        LDA #12
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        if (NC)
        {
            LDA #0x31
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // With tail insertion: List head should still point to the first node
        LDA Test.TableHeadLocationL
        CMP Test.TableNodeL
        if (NZ)
        {
            LDA #0x32
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        LDA Test.TableHeadLocationH
        CMP Test.TableNodeH
        if (NZ)
        {
            LDA #0x33
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // First node's next pointer should point to second node
        LDA Test.TableNodeL
        STA ZP.IDXL
        LDA Test.TableNodeH
        STA ZP.IDXH
        Table.GetNext();
        LDA ZP.IDXL
        CMP Test.TableNodeL  // Should NOT equal first node (would be circular)
        if (Z)
        {
            LDA ZP.IDXH
            CMP Test.TableNodeH
            if (Z)
            {
                LDA #0x34
                CLC  // Fail - circular reference
                Test.PrintResult();
                return;
            }
        }
        
        // Second node should exist (IDX should be non-zero after GetNext)
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            LDA #0x35
            CLC  // Fail - second node not linked
            Test.PrintResult();
            return;
        }
        
        // Clean up before success
        LDX #Test.TableHeadLocation
        Table.Clear();
        SEC  // Pass
        Test.PrintResult();
    }
       
    // Test 4: Traverse linked list
    testTraverse()
    {
        LDA #'4'
        LDA #(tableDesc4 % 256)
        STA ZP.TOPL
        LDA #(tableDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with a clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add nodes (will be in reverse order: 3rd, 2nd, 1st)
        LDY #0
        loop
        {
            CPY #3
            if (Z) { break; }
            
            LDA #10
            STA ZP.ACCL
            STZ ZP.ACCH
            LDX #Test.TableHeadLocation
            Table.Add();
            if (NC)
            {
                LDA #0x40
                CLC  // Fail
                Test.PrintResult();
                return;
            }
            
            INY
        }
        
        // Count nodes by traversal
        LDX #Test.TableHeadLocation
        Table.GetFirst();
        
        LDY #0  // Node count
        loop
        {
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z) { break; } // End of list
            
            INY
            Table.GetNext();
        }
        
        // Should have counted 3 nodes
        CPY #3
        if (Z)
        {
            // Clean up before success
            LDX #Test.TableHeadLocation
            Table.Clear();
            SEC  // Pass
        }
        else
        {
            LDA #0x41
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 5: Delete first node
    testDeleteFirst()
    {
        LDA #'5'
        LDA #(tableDesc5 % 256)
        STA ZP.TOPL
        LDA #(tableDesc5 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with a clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add first node
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA Test.TableNodeL
        LDA ZP.IDXH
        STA Test.TableNodeH
        
        // Add second node (becomes new head)
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        
        // Delete the current head (second node)
        LDX #Test.TableHeadLocation
        Table.Delete();
        
        // List head should now point to first node
        LDA Test.TableHeadLocationL
        CMP Test.TableNodeL
        if (NZ)
        {
            LDA #0x50
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        LDA Test.TableHeadLocationH
        CMP Test.TableNodeH
        if (NZ)
        {
            LDA #0x51
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Clean up before success
        LDX #Test.TableHeadLocation
        Table.Clear();
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 6: Clear entire list
    testClearList()
    {
        LDA #'6'
        LDA #(tableDesc6 % 256)
        STA ZP.TOPL
        LDA #(tableDesc6 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with a clean list, then build multiple nodes
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        LDY #0
        loop
        {
            CPY #5
            if (Z) { break; }
            
            LDA #8
            STA ZP.ACCL
            STZ ZP.ACCH
            LDX #Test.TableHeadLocation
            Table.Add();
            
            INY
        }
        
        // Clear the list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // List should be empty
        LDA Test.TableHeadLocationL
        ORA Test.TableHeadLocationH
        if (Z)
        {
            SEC  // Pass - already clean, no additional cleanup needed
        }
        else
        {
            LDA #0x60
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Run all table tests
    RunTableTests()
    {
        testEmptyList();
        testAddSingle();
        testAddMultiple();
        testTraverse();
        testDeleteFirst();
        testClearList();
    }
}
