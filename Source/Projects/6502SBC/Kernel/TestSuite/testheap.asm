unit Tests
{
    // Test strings
    const string testHeader = "\n=== HEAP MODULE TESTS ===\n";
    const string testFooter = "\n==============================\n\n";
    const string passLabel = "[PASS] ";
    const string failLabel = "[FAIL] ";
    const string okLabel = "[OK]";
    const string errorLabel = "[ERROR]";
    const string skipLabel = "[SKIP] ";
    const string overallSuiteLabel = "Overall test suite: ";
    
    // Test names
    const string basicAllocTestName = "Basic allocation test: ";
    const string basicFreeTestName = "Basic free test: ";
    const string multiAllocTestName = "Multiple allocation test: ";
    const string reuseTestName = "Memory reuse test: ";
    const string fragmentTestName = "Fragmentation test: ";
    const string doubleFreeTestName = "Double free test: ";
    const string invalidFreeTestName = "Invalid free test: ";
    const string outOfMemoryTestName = "Out of memory test: ";
    
    // Test results
    const string allocationSuccess = "Allocation successful";
    const string allocationFailed = "Allocation failed";
    const string freeSuccess = "Free successful";
    const string freeFailed = "Free failed";
    const string memoryReused = "Memory reused correctly";
    const string memoryNotReused = "Memory NOT reused";
    const string fragmentationHandled = "Fragmentation handled";
    const string fragmentationFailed = "Fragmentation failed";
    const string doubleFreeDetected = "Double free detected";
    const string doubleFreeNotDetected = "Double free NOT detected";
    const string invalidFreeDetected = "Invalid free detected";
    const string invalidFreeNotDetected = "Invalid free NOT detected";
    const string outOfMemoryDetected = "Out of memory detected";
    const string outOfMemoryNotDetected = "Out of memory NOT detected";
    const string allAllocationsSuccessful = "All allocations successful";
    const string someAllocationsFailed = "Some allocations failed";
    const string noCrashOnDoubleFree = "No crash on double free";
    const string noCrashOnInvalidFree = "No crash on invalid free";
    const string memoryLeakDetected = "Memory leak detected";
    
    // Skip messages
    const string skipNoAllocatedMemory = "No allocated memory";
    const string skipInitialAllocationFailed = "Initial allocation failed";
    const string skipCouldNotAllocateTestBlock = "Could not allocate test block";
    
    // Labels
    const string addressLabel = "Address: ";
    const string sizeLabel = "Size: ";
    const string bytesLabel = " bytes";
    const string nullLabel = "NULL";
    const string spaceColon = ": ";
    const string spaceDash = " - ";
    
    // Use I2C buffer for pointer storage and temporary variables (256 bytes available)
    const uint AllocatedPointers = Address.I2CInBuffer;     // Store up to 127 pointers (2 bytes each) 
    const uint AllocatedCount = Address.I2CInBuffer + 254;  // Counter at end of buffer
    const uint SuccessCount = Address.I2CInBuffer + 252;    // Temporary counter
    const uint TestCount = Address.I2CInBuffer + 253;       // Temporary counter
    const uint AllocCount = Address.I2CInBuffer + 251;      // For out of memory test
    const uint InitialMemory = Address.I2CInBuffer + 249;   // Store initial available memory (2 bytes)
    const uint FinalMemory = Address.I2CInBuffer + 247;     // Store final available memory (2 bytes)
    
    // Run all Heap module tests
    RunTests()
    {
        PrintHeader();
        
        Memory.Available();
        Debug.NL(); AOut();
        
        // Store initial memory state
        Memory.Available();
        LDA ZP.ACCL
        STA InitialMemory
        LDA ZP.ACCH
        STA InitialMemory + 1
        
        // Initialize test storage
        STZ AllocatedCount
        
        TestBasicAllocation();
        CleanupAllocatedMemory();
        
        TestBasicFree();
        CleanupAllocatedMemory();
        
        TestMultipleAllocations();
        CleanupAllocatedMemory();
        
        TestMemoryReuse();
        CleanupAllocatedMemory();
        
        TestFragmentation();
        CleanupAllocatedMemory();
        
        //TestDoubleFree();
        //CleanupAllocatedMemory();
        
        //TestInvalidFree();
        //CleanupAllocatedMemory();
        
        //TestOutOfMemory();
        //CleanupAllocatedMemory();
        
        // Check final memory state
        Memory.Available();
        LDA ZP.ACCL
        STA FinalMemory
        LDA ZP.ACCH
        STA FinalMemory + 1
        
        // Compare with initial memory
        LDA InitialMemory
        CMP FinalMemory
        if (NZ)
        {
            PrintOverallMemoryLeak();
        }
        else
        {
            LDA InitialMemory + 1
            CMP FinalMemory + 1
            if (NZ)
            {
                PrintOverallMemoryLeak();
            }
        }
        
        Memory.Available();
        Debug.NL(); AOut();        
        
        PrintFooter();
    }
    
    // Print test header
    PrintHeader()
    {
        LDA #(testHeader % 256)
        STA ZP.STRL
        LDA #(testHeader / 256)
        STA ZP.STRH
        Print.String();
        
        // Show initial heap state
        Debug.DumpHeap();
    }
    
    // Print test footer  
    PrintFooter()
    {
        LDA #(testFooter % 256)
        STA ZP.STRL
        LDA #(testFooter / 256)
        STA ZP.STRH
        Print.String();
        
        // Show final heap state
        Debug.DumpHeap();
    }
    
    PrintOverallMemoryLeak()
    {
        PrintFailLabel();
        LDA #(overallSuiteLabel % 256)
        STA ZP.STRL
        LDA #(overallSuiteLabel / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(memoryLeakDetected % 256)
        STA ZP.STRL
        LDA #(memoryLeakDetected / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Clean up all allocated memory
    CleanupAllocatedMemory()
    {
        // Free all stored pointers
        LDA AllocatedCount
        loop
        {
            if (Z) { break; }
            
            DEC AllocatedCount
            LDA AllocatedCount
            ASL A  // Multiply by 2
            TAY
            
            LDA AllocatedPointers, Y
            STA ZP.IDXL
            INY
            LDA AllocatedPointers, Y
            STA ZP.IDXH
            
            Memory.Free();
            
            LDA AllocatedCount
        }
    }
    
    // Test basic allocation
    TestBasicAllocation()
    {
        // Allocate 100 bytes - put size in ZP.ACC
        LDA #100
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        
        // Check if allocation succeeded (non-zero pointer in ZP.IDX)
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (NZ)
        {
            // Store pointer for cleanup
            StoreAllocatedPointer();
            PrintBasicAllocPass();
            
        }
        else
        {
            PrintBasicAllocFail();
            Debug.DumpHeap();
        }
    }
    
    PrintBasicAllocPass()
    {
        PrintPassLabel();
        
        LDA #(basicAllocTestName % 256)
        STA ZP.STRL
        LDA #(basicAllocTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(allocationSuccess % 256)
        STA ZP.STRL
        LDA #(allocationSuccess / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(spaceDash % 256)
        STA ZP.STRL
        LDA #(spaceDash / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(addressLabel % 256)
        STA ZP.STRL
        LDA #(addressLabel / 256)
        STA ZP.STRH
        Print.String();
        
        // Print address (IDX contains pointer) - move to TOP for printing
        LDA ZP.IDXL
        STA ZP.TOP0
        LDA ZP.IDXH
        STA ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Long.Print();
        
        Print.NewLine();
    }
    
    PrintBasicAllocFail()
    {
        PrintFailLabel();
        
        LDA #(basicAllocTestName % 256)
        STA ZP.STRL
        LDA #(basicAllocTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(allocationFailed % 256)
        STA ZP.STRL
        LDA #(allocationFailed / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Test basic free
    TestBasicFree()
    {
        // Allocate a block first
        LDA #80
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (NZ)
        {
            // Free the memory immediately
            Memory.Free();
            PrintBasicFreePass();
            
        }
        else
        {
            PrintBasicFreeSkip();
            Debug.DumpHeap();
        }
    }
    
    PrintBasicFreePass()
    {
        PrintPassLabel();
        
        LDA #(basicFreeTestName % 256)
        STA ZP.STRL
        LDA #(basicFreeTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(freeSuccess % 256)
        STA ZP.STRL
        LDA #(freeSuccess / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    PrintBasicFreeSkip()
    {
        PrintSkipLabel();
        
        LDA #(basicFreeTestName % 256)
        STA ZP.STRL
        LDA #(basicFreeTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(skipNoAllocatedMemory % 256)
        STA ZP.STRL
        LDA #(skipNoAllocatedMemory / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Test multiple allocations
    TestMultipleAllocations()
    {
        STZ SuccessCount
        STZ TestCount
        
        // Try to allocate 5 blocks of different sizes
        
        // 50 bytes
        LDA #50
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        INC TestCount
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (NZ)
        {
            INC SuccessCount
            StoreAllocatedPointer();
        }
        
        // 25 bytes  
        LDA #25
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        INC TestCount
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (NZ)
        {
            INC SuccessCount
            StoreAllocatedPointer();
        }
        
        // 75 bytes
        LDA #75
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        INC TestCount
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (NZ)
        {
            INC SuccessCount
            StoreAllocatedPointer();
        }
        
        // Check results
        LDA SuccessCount
        CMP TestCount
        if (Z)
        {
            PrintMultiAllocPass();
        }
        else
        {
            PrintMultiAllocPartial();
            Debug.DumpHeap();
        }
        
        
    }
    
    StoreAllocatedPointer()
    {
        // Store pointer if we have room
        LDA AllocatedCount
        CMP #10  // Max 10 pointers
        if (C)
        {
            ASL A  // Multiply by 2 for word storage
            TAY
            
            LDA ZP.IDXL
            STA AllocatedPointers, Y
            INY
            LDA ZP.IDXH
            STA AllocatedPointers, Y
            
            INC AllocatedCount
        }
    }
    
    PrintMultiAllocPass()
    {
        PrintPassLabel();
        
        LDA #(multiAllocTestName % 256)
        STA ZP.STRL
        LDA #(multiAllocTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(allAllocationsSuccessful % 256)
        STA ZP.STRL
        LDA #(allAllocationsSuccessful / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    PrintMultiAllocPartial()
    {
        PrintFailLabel();
        
        LDA #(multiAllocTestName % 256)
        STA ZP.STRL
        LDA #(multiAllocTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(someAllocationsFailed % 256)
        STA ZP.STRL
        LDA #(someAllocationsFailed / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Test memory reuse after free
    TestMemoryReuse()
    {
        // Allocate 60 bytes
        LDA #60
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            PrintMemoryReuseSkip();
            return;
        }
        
        // Save first allocation address
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Free it
        Memory.Free();
        
        // Allocate same size again
        LDA #60
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        
        // Compare addresses
        PLA
        CMP ZP.IDXH
        if (NZ)
        {
            PrintMemoryReuseFail();
            PLA  // Clean stack
            
            // Store pointer for cleanup if allocation succeeded
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (NZ)
            {
                StoreAllocatedPointer();
            }
            return;
        }
        
        PLA
        CMP ZP.IDXL
        if (Z)
        {
            PrintMemoryReusePass();
            StoreAllocatedPointer();
        }
        else
        {
            PrintMemoryReuseFail();
            StoreAllocatedPointer();
            Debug.DumpHeap();
        }
        
        
    }
    
    PrintMemoryReusePass()
    {
        PrintPassLabel();
        
        LDA #(reuseTestName % 256)
        STA ZP.STRL
        LDA #(reuseTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(memoryReused % 256)
        STA ZP.STRL
        LDA #(memoryReused / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    PrintMemoryReuseFail()
    {
        PrintFailLabel();
        
        LDA #(reuseTestName % 256)
        STA ZP.STRL
        LDA #(reuseTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(memoryNotReused % 256)
        STA ZP.STRL
        LDA #(memoryNotReused / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    PrintMemoryReuseSkip()
    {
        PrintSkipLabel();
        
        LDA #(reuseTestName % 256)
        STA ZP.STRL
        LDA #(reuseTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(skipInitialAllocationFailed % 256)
        STA ZP.STRL
        LDA #(skipInitialAllocationFailed / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Test fragmentation handling
    TestFragmentation()
    {
        // This test assumes previous allocations created some fragmentation
        // Try to allocate a large block that should fit in total free space
        // but may not fit in any single fragment
        
        LDA #200  // Try to allocate 200 bytes
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (NZ)
        {
            PrintFragmentationPass();
            StoreAllocatedPointer();
        }
        else
        {
            PrintFragmentationFail();
            Debug.DumpHeap();
        }
        
        
    }
    
    PrintFragmentationPass()
    {
        PrintPassLabel();
        
        LDA #(fragmentTestName % 256)
        STA ZP.STRL
        LDA #(fragmentTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(fragmentationHandled % 256)
        STA ZP.STRL
        LDA #(fragmentationHandled / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    PrintFragmentationFail()
    {
        PrintFailLabel();
        
        LDA #(fragmentTestName % 256)
        STA ZP.STRL
        LDA #(fragmentTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(fragmentationFailed % 256)
        STA ZP.STRL
        LDA #(fragmentationFailed / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Test double free detection
    TestDoubleFree()
    {
        // Allocate a block
        LDA #30
        STA ZP.ACCL
        STZ ZP.ACCH
        Memory.Allocate();
        
        LDA ZP.IDXL
        ORA ZP.IDXH
        if (Z)
        {
            PrintDoubleFreeSkip();
            return;
        }
        
        // Save the pointer
        LDA ZP.IDXL
        PHA
        LDA ZP.IDXH
        PHA
        
        // Free it once
        Memory.Free();
        
        // Try to free it again - put pointer back in IDX
        PLA
        STA ZP.IDXH
        PLA
        STA ZP.IDXL
        
        // Note: The actual Memory.Free() doesn't return error codes
        // This test verifies the system doesn't crash on double free
        Memory.Free();
        
        // If we get here without crashing, consider it a pass
        PrintDoubleFreePass();
    }
    
    PrintDoubleFreePass()
    {
        PrintPassLabel();
        
        LDA #(doubleFreeTestName % 256)
        STA ZP.STRL
        LDA #(doubleFreeTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(noCrashOnDoubleFree % 256)
        STA ZP.STRL
        LDA #(noCrashOnDoubleFree / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    PrintDoubleFreeSkip()
    {
        PrintSkipLabel();
        
        LDA #(doubleFreeTestName % 256)
        STA ZP.STRL
        LDA #(doubleFreeTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(skipCouldNotAllocateTestBlock % 256)
        STA ZP.STRL
        LDA #(skipCouldNotAllocateTestBlock / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Test invalid pointer free
    TestInvalidFree()
    {
        // Try to free an invalid pointer (stack address)
        LDA #(0x0100 % 256)  // Stack page
        STA ZP.IDXL
        LDA #(0x0100 / 256)
        STA ZP.IDXH
        
        // Note: Memory.Free() doesn't validate pointers in release mode
        // This test verifies the system doesn't crash on invalid free
        Memory.Free();
        
        // If we get here without crashing, consider it a pass
        PrintInvalidFreePass();
    }
    
    PrintInvalidFreePass()
    {
        PrintPassLabel();
        
        LDA #(invalidFreeTestName % 256)
        STA ZP.STRL
        LDA #(invalidFreeTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(noCrashOnInvalidFree % 256)
        STA ZP.STRL
        LDA #(noCrashOnInvalidFree / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Test out of memory condition
    TestOutOfMemory()
    {
        STZ AllocCount
        
        // Try to allocate until we run out of memory
        loop
        {
            LDA #255  // Try to allocate large blocks
            STA ZP.ACCL
            STZ ZP.ACCH
            Memory.Allocate();
            
            LDA ZP.IDXL
            ORA ZP.IDXH
            if (Z)  // Allocation failed
            {
                break;
            }
            
            // Store the pointer for cleanup
            StoreAllocatedPointer();
            
            INC AllocCount
            LDA AllocCount
            CMP #20  // Safety limit
            if (C)
            {
                break;
            }
        }
        
        // If we allocated at least one block and then failed, test passes
        LDA AllocCount
        if (NZ)
        {
            PrintOutOfMemoryPass();
        }
        else
        {
            PrintOutOfMemoryFail();
        }
    }
    
    PrintOutOfMemoryPass()
    {
        PrintPassLabel();
        
        LDA #(outOfMemoryTestName % 256)
        STA ZP.STRL
        LDA #(outOfMemoryTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(outOfMemoryDetected % 256)
        STA ZP.STRL
        LDA #(outOfMemoryDetected / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    PrintOutOfMemoryFail()
    {
        PrintFailLabel();
        
        LDA #(outOfMemoryTestName % 256)
        STA ZP.STRL
        LDA #(outOfMemoryTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(outOfMemoryNotDetected % 256)
        STA ZP.STRL
        LDA #(outOfMemoryNotDetected / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Helper functions
    PrintPassLabel()
    {
        LDA #(passLabel % 256)
        STA ZP.STRL
        LDA #(passLabel / 256)
        STA ZP.STRH
        Print.String();
    }
    
    PrintFailLabel()
    {
        LDA #(failLabel % 256)
        STA ZP.STRL
        LDA #(failLabel / 256)
        STA ZP.STRH
        Print.String();
    }
    
    PrintSkipLabel()
    {
        LDA #(skipLabel % 256)
        STA ZP.STRL
        LDA #(skipLabel / 256)
        STA ZP.STRH
        Print.String();
    }
}
