unit TestTable
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "../Table"
    
    // Private test descriptions for Table tests
    const string tableDesc1 = "Empty list operations";
    const string tableDesc2 = "Add single node";
    const string tableDesc3 = "Add multiple nodes";
    const string tableDesc4 = "Traverse list";
    const string tableDesc5 = "Delete first node";
    const string tableDesc6 = "Clear entire list";
    const string tableDesc7 = "Delete middle node";
    const string tableDesc8 = "Delete last node";
    const string tableDesc9 = "Delete from single-node list";
    const string tableDesc10 = "Delete non-existent node";
    const string tableDesc11 = "Complex add/delete sequence";
    const string tableDesc12 = "Delete all nodes individually";

    
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
    
    // Test 3: Add multiple nodes to list
    testAddMultiple()
    {
        LDA #'3'
        LDA #(tableDesc3 % 256)
        STA ZP.TOPL
        LDA #(tableDesc3 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add first node
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        
        // Save address of first node
        LDA ZP.IDXL
        STA Test.TableNodeL
        LDA ZP.IDXH
        STA Test.TableNodeH
        
        // Add second node
        LDA #20
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        
        // Verify we have 2 nodes
        LDX #Test.TableHeadLocation
        Table.GetFirst();
        
        Table.GetNext();
        if (NC)
        {
            LDA #0x30
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Table.GetNext();
        if (C)  // Should be at end
        {
            LDA #0x31
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
    
    // Test 4: Traverse list
    testTraverse()
    {
        LDA #'4'
        LDA #(tableDesc4 % 256)
        STA ZP.TOPL
        LDA #(tableDesc4 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Build 3-node list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add 3 nodes
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        
        // Count nodes by traversing
        LDY #0
        LDX #Test.TableHeadLocation
        Table.GetFirst();
        if (NC)
        {
            LDA #0x40
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        INY  // Count first node
        
        loop
        {
            Table.GetNext();
            if (NC) { break; }  // No more nodes
            INY  // Count this node
        }
        
        CPY #3
        if (Z)
        {
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
        
        // Start with clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add first node and save address
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        
        // Save the head node address  
        LDA Test.TableHeadLocationL
        STA Test.TableNodeL
        LDA Test.TableHeadLocationH
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
    
    // Test 7: Delete middle node from 3-node list
    testDeleteMiddle()
    {
        LDA #'7'
        LDA #(tableDesc7 % 256)
        STA ZP.TOPL
        LDA #(tableDesc7 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add 3 nodes, save addresses
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA ZP.U0  // First node
        LDA ZP.IDXH
        STA ZP.U1
        
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA ZP.U2  // Middle node (to delete)
        LDA ZP.IDXH
        STA ZP.U3
        
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA ZP.U4  // Last node
        LDA ZP.IDXH
        STA ZP.U5
        
        // Delete middle node
        LDA ZP.U2
        STA ZP.IDXL
        LDA ZP.U3
        STA ZP.IDXH
        LDX #Test.TableHeadLocation
        Table.Delete();
        
        if (NC)
        {
            LDA #0x70
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Verify 2 nodes remain and are connected
        LDX #Test.TableHeadLocation
        Table.GetFirst();
        Table.GetNext();
        if (NC)
        {
            LDA #0x71
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        Table.GetNext();
        if (C)  // Should be at end
        {
            LDA #0x72
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Clean up
        LDX #Test.TableHeadLocation
        Table.Clear();
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 8: Delete last node
    testDeleteLast()
    {
        LDA #'8'
        LDA #(tableDesc8 % 256)
        STA ZP.TOPL
        LDA #(tableDesc8 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add 2 nodes
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA ZP.U0  // Save last node
        LDA ZP.IDXH
        STA ZP.U1
        
        // Delete last node
        LDA ZP.U0
        STA ZP.IDXL
        LDA ZP.U1
        STA ZP.IDXH
        LDX #Test.TableHeadLocation
        Table.Delete();
        
        if (NC)
        {
            LDA #0x80
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Verify only 1 node remains
        LDX #Test.TableHeadLocation
        Table.GetFirst();
        Table.GetNext();
        if (C)  // Should be at end
        {
            LDA #0x81
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Clean up
        LDX #Test.TableHeadLocation
        Table.Clear();
        SEC  // Pass
        Test.PrintResult();
    }
    
    // Test 9: Delete from single-node list
    testDeleteSingle()
    {
        LDA #'9'
        LDA #(tableDesc9 % 256)
        STA ZP.TOPL
        LDA #(tableDesc9 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add single node
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        
        // Delete it (using head value)
        LDA Test.TableHeadLocationL
        STA ZP.IDXL
        LDA Test.TableHeadLocationH
        STA ZP.IDXH
        
        LDX #Test.TableHeadLocation
        Table.Delete();
        
        if (NC)
        {
            LDA #0x90
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // List should be empty
        LDA Test.TableHeadLocationL
        ORA Test.TableHeadLocationH
        if (Z)
        {
            SEC  // Pass - already clean
        }
        else
        {
            LDA #0x91
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 10: Try to delete non-existent node
    testDeleteNonExistent()
    {
        LDA #'A'  // Test 10
        LDA #(tableDesc10 % 256)
        STA ZP.TOPL
        LDA #(tableDesc10 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add one real node
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        
        // Try to delete a fake address (should fail)
        LDA #0x12  // Fake address
        STA ZP.IDXL
        LDA #0x34
        STA ZP.IDXH
        
        LDX #Test.TableHeadLocation
        Table.Delete();
        
        // Should fail (NC)
        if (C)
        {
            LDA #0xA0
            CLC  // Fail - should have failed
            Test.PrintResult();
            return;
        }
        
        // Original node should still be there
        LDX #Test.TableHeadLocation
        Table.GetFirst();
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (NZ)
        {
            LDX #Test.TableHeadLocation
            Table.Clear();
            SEC  // Pass
        }
        else
        {
            LDA #0xA1
            CLC  // Fail
        }
        Test.PrintResult();
    }

    // Test 11: Complex add/delete sequence
    testComplexSequence()
    {
        LDA #'B'  // Test 11
        LDA #(tableDesc11 % 256)
        STA ZP.TOPL
        LDA #(tableDesc11 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add 3 nodes
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA Test.TableHeadLocationL
        STA ZP.U0  // Node 1
        LDA Test.TableHeadLocationH
        STA ZP.U1
        
        LDA #12
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA ZP.U2  // Node 2
        LDA ZP.IDXH
        STA ZP.U3
        
        LDA #14
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA ZP.U4  // Node 3
        LDA ZP.IDXH
        STA ZP.U5
        
        // Delete middle node
        LDA ZP.U2
        STA ZP.IDXL
        LDA ZP.U3
        STA ZP.IDXH
        LDX #Test.TableHeadLocation
        Table.Delete();
        
        // Add another node (should go at end)
        LDA #16
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA ZP.U6  // Node 4
        LDA ZP.IDXH
        STA ZP.U7
        
        // Delete first node
        LDA ZP.U0
        STA ZP.IDXL
        LDA ZP.U1
        STA ZP.IDXH
        LDX #Test.TableHeadLocation
        Table.Delete();
        
        // Should have 2 nodes left (3 and 4)
        LDY #0
        LDX #Test.TableHeadLocation
        Table.GetFirst();
        if (NC)
        {
            LDA #0xB0
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        loop
        {
            INY
            Table.GetNext();
            if (NC) { break; }
        }
        
        CPY #2
        if (Z)
        {
            LDX #Test.TableHeadLocation
            Table.Clear();
            SEC  // Pass
        }
        else
        {
            LDA #0xB1
            CLC  // Fail
        }
        Test.PrintResult();
    }
    
    // Test 12: Delete all nodes individually
    testDeleteAllIndividually()
    {
        LDA #'C'  // Test 12
        LDA #(tableDesc12 % 256)
        STA ZP.TOPL
        LDA #(tableDesc12 / 256)
        STA ZP.TOPH
        Test.PrintTestHeader();
        
        // Start with clean list
        LDX #Test.TableHeadLocation
        Table.Clear();
        
        // Add 4 nodes
        LDA #10
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA ZP.U0  // Node 1
        LDA ZP.IDXH
        STA ZP.U1
        
        LDA #12
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA ZP.U2  // Node 2
        LDA ZP.IDXH
        STA ZP.U3
        
        LDA #14
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA ZP.U4  // Node 3
        LDA ZP.IDXH
        STA ZP.U5
        
        LDA #16
        STA ZP.ACCL
        STZ ZP.ACCH
        LDX #Test.TableHeadLocation
        Table.Add();
        LDA ZP.IDXL
        STA ZP.U6  // Node 4
        LDA ZP.IDXH
        STA ZP.U7
        
        // Delete in specific order: 3rd, 1st, 4th, 2nd
        // Delete Node 3 (middle)
        LDA ZP.U4
        STA ZP.IDXL
        LDA ZP.U5
        STA ZP.IDXH
        LDX #Test.TableHeadLocation
        Table.Delete();
        if (NC)
        {
            LDA #0xC0
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Delete Node 1 (now first)
        LDA ZP.U0
        STA ZP.IDXL
        LDA ZP.U1
        STA ZP.IDXH
        LDX #Test.TableHeadLocation
        Table.Delete();
        if (NC)
        {
            LDA #0xC1
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Delete Node 4 (now last)
        LDA ZP.U6
        STA ZP.IDXL
        LDA ZP.U7
        STA ZP.IDXH
        LDX #Test.TableHeadLocation
        Table.Delete();
        if (NC)
        {
            LDA #0xC2
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // Delete Node 2 (now only remaining)
        LDA ZP.U2
        STA ZP.IDXL
        LDA ZP.U3
        STA ZP.IDXH
        LDX #Test.TableHeadLocation
        Table.Delete();
        if (NC)
        {
            LDA #0xC3
            CLC  // Fail
            Test.PrintResult();
            return;
        }
        
        // List should be completely empty
        LDA Test.TableHeadLocationL
        ORA Test.TableHeadLocationH
        if (Z)
        {
            SEC  // Pass - already clean
        }
        else
        {
            LDA #0xC4
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
        testDeleteMiddle();
        testDeleteLast();
        testDeleteSingle();
        testDeleteNonExistent();
        testComplexSequence();
        testDeleteAllIndividually();
    }
}
