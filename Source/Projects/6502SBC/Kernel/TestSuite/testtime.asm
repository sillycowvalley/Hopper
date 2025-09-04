unit Tests
{
    // Test strings
    const string testHeader = "\n=== TIME MODULE TESTS ===\n";
    const string testFooter = "\n==============================\n\n";
    const string passLabel = "[PASS] ";
    const string failLabel = "[FAIL] ";
    const string okLabel = "[OK]";
    const string errorLabel = "[ERROR]";
    
    // Test names
    const string millisTestName = "Millis() counter test: ";
    const string delayTestName = "Delay() accuracy test ";
    const string secondsTestName = "Seconds() conversion test: ";
    
    // Test results
    const string counterIncrementing = "Counter incrementing";
    const string counterNotIncrementing = "Counter NOT incrementing";
    const string conversionCorrect = "Millis/1000 = Seconds";
    const string conversionError = "Conversion error > 1000ms";
    const string withinTolerance = "Within tolerance";
    const string outsideTolerance = "Outside tolerance";
    
    // Labels
    const string startLabel = "Start: ";
    const string endLabel = "End: ";
    const string deltaLabel = "Delta: ";
    const string expectedLabel = "Expected: ";
    const string actualLabel = "Actual: ";
    const string toleranceLabel = "Tolerance: ";
    const string msLabel = "ms";
    const string percentLabel = "%";
    const string spaceColon = ": ";
    const string spaceDash = " - ";
    
    // Run all Time module tests
    RunTests()
    {
        PrintHeader();
        
        TestMillisIncrement();
        TestDelay1ms();
        TestDelay10ms();
        TestDelay100ms();
        TestDelay1000ms();
        TestSecondsConversion();
        TestDelayAccuracy();
        
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
    }
    
    // Print test footer
    PrintFooter()
    {
        LDA #(testFooter % 256)
        STA ZP.STRL
        LDA #(testFooter / 256)
        STA ZP.STRH
        Print.String();
    }
    
    // Test that Millis() counter is incrementing
    TestMillisIncrement()
    {
        // Get first reading
        Time.Millis();
        
        // Save first reading using hardware stack
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        LDA ZP.TOP2
        PHA
        LDA ZP.TOP3
        PHA
        
        // Small delay (about 5ms)
        LDA #5
        Shared.LoadTopByte(); // Sets TOP0 = A, zeros TOP1-3
        Time.Delay();
        
        // Get second reading
        Time.Millis();
        
        // Pull first reading from stack into NEXT for comparison
        PLA
        STA ZP.NEXT3
        PLA
        STA ZP.NEXT2
        PLA
        STA ZP.NEXT1
        PLA
        STA ZP.NEXT0
        
        // Compare NEXT (first reading) with TOP (second reading)
        LDA ZP.NEXT3
        CMP ZP.TOP3
        if (NZ) { PrintMillisPass(); return; }
        
        LDA ZP.NEXT2
        CMP ZP.TOP2
        if (NZ) { PrintMillisPass(); return; }
        
        LDA ZP.NEXT1
        CMP ZP.TOP1
        if (NZ) { PrintMillisPass(); return; }
        
        LDA ZP.NEXT0
        CMP ZP.TOP0
        if (NZ) { PrintMillisPass(); return; }
        
        // Values are identical - fail
        PrintMillisFail();
    }
    
    PrintMillisPass()
    {
        PrintPassLabel();
        
        LDA #(millisTestName % 256)
        STA ZP.STRL
        LDA #(millisTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(counterIncrementing % 256)
        STA ZP.STRL
        LDA #(counterIncrementing / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    PrintMillisFail()
    {
        PrintFailLabel();
        
        LDA #(millisTestName % 256)
        STA ZP.STRL
        LDA #(millisTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(counterNotIncrementing % 256)
        STA ZP.STRL
        LDA #(counterNotIncrementing / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Test 1ms delay
    TestDelay1ms()
    {
        LDA #1
        TestDelayWithValue();
    }
    
    // Test 10ms delay
    TestDelay10ms()
    {
        LDA #10
        TestDelayWithValue();
    }
    
    // Test 100ms delay
    TestDelay100ms()
    {
        LDA #100
        TestDelayWithValue();
    }
    
    // Test 1000ms delay with detailed timing
    TestDelay1000ms()
    {
        // Get start time
        Time.Millis();
        
        // Move start time to NEXT (for subtraction: NEXT = NEXT - TOP)
        Shared.MoveTopToNext();
        
        // Delay for 1000ms
        LDA #(1000 % 256)
        STA ZP.TOP0
        LDA #(1000 / 256)
        STA ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Time.Delay();
        
        // Get end time in TOP
        Time.Millis();
        
        // Calculate delta: NEXT = end - start (since NEXT has start, TOP has end)
        // We need end - start, so swap them first
        Shared.SwapNextTop(); // Now NEXT=end, TOP=start
        Long.Sub(); // NEXT = NEXT - TOP = end - start
        
        // Result is now in NEXT, move to TOP for printing
        Shared.MoveNextToTop();
        
        // Save delta for later comparison
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        LDA ZP.TOP2
        PHA
        LDA ZP.TOP3
        PHA
        
        // Print result header
        LDA #(delayTestName % 256)
        STA ZP.STRL
        LDA #(delayTestName / 256)
        STA ZP.STRH
        Print.String();
        
        // Print delay value
        LDA #(1000 % 256)
        STA ZP.TOP0
        LDA #(1000 / 256)
        STA ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Long.Print();
        
        LDA #(msLabel % 256)
        STA ZP.STRL
        LDA #(msLabel / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(spaceColon % 256)
        STA ZP.STRL
        LDA #(spaceColon / 256)
        STA ZP.STRH
        Print.String();
        
        // Restore delta from stack
        PLA
        STA ZP.TOP3
        PLA
        STA ZP.TOP2
        PLA
        STA ZP.TOP1
        PLA
        STA ZP.TOP0
        
        // Check if delta is between 995 and 1005 (0.5% tolerance)
        // First check if >= 995
        
        LDA #(995 % 256)
        STA ZP.NEXT0
        LDA #(995 / 256)
        STA ZP.NEXT1
        STZ ZP.NEXT2
        STZ ZP.NEXT3

        Long.LE(); // NEXT <= TOP?
        if (C)  // >= 995
        {
            // Now check if <= 1005
            LDA #(1005 % 256)
            STA ZP.NEXT0
            LDA #(1005 / 256)
            STA ZP.NEXT1
            STZ ZP.NEXT2
            STZ ZP.NEXT3
            
            Long.GE(); // NEXT >= TOP?
            if (C)  // <= 1005
            {
                PrintOK();    
            }
            else
            {
                PrintFailWithDelta();
            }
        }
        else
        {
            PrintFailWithDelta();
        }
    }
    
    // Generic delay test with specified value (1-255ms)
    TestDelayWithValue()
    {
        // A = delay value in ms (1-255)
        PHA  // Save delay value on hardware stack
        
        // Get start time
        Time.Millis();
        
        // Save start time (only need low 16 bits for small delays)
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        
        // Set up delay - get delay value from stack
        PLA  // Get TOP1 back
        PLA  // Get TOP0 back  
        PLA  // Get delay value
        Shared.LoadTopByte(); // Sets TOP0 = A, zeros TOP1-3
        
        PHA  // Save delay value again for printing
        
        Time.Delay();
        
        // Get end time (don't need to calculate delta for simple test)
        Time.Millis();
        
        // Print test name
        LDA #(delayTestName % 256)
        STA ZP.STRL
        LDA #(delayTestName / 256)
        STA ZP.STRH
        Print.String();
        
        // Print delay value
        PLA
        Shared.LoadTopByte();
        Long.Print();
        
        LDA #(msLabel % 256)
        STA ZP.STRL
        LDA #(msLabel / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(spaceColon % 256)
        STA ZP.STRL
        LDA #(spaceColon / 256)
        STA ZP.STRH
        Print.String();
        
        PrintOK();
    }
    
    // Test Seconds() conversion
    TestSecondsConversion()
    {
        
        // Delay for 5000ms
        LDA #(5000 % 256)
        STA ZP.TOP0
        LDA #(5000 / 256)
        STA ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Time.Delay();
        
        Time.Millis();
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        LDA ZP.TOP2
        PHA
        LDA ZP.TOP3
        PHA
        
        Time.Seconds();
        
        // TOP now has seconds
        // Multiply by 1000 to convert back to milliseconds
        
        // Set NEXT to 1000
        LDA #(1000 % 256)
        STA ZP.NEXT0
        LDA #(1000 / 256)
        STA ZP.NEXT1
        STZ ZP.NEXT2
        STZ ZP.NEXT3
        
        Long.Mul();  // NEXT = NEXT * TOP = seconds * 1000
        
        PLA
        STA ZP.TOP3
        PLA
        STA ZP.TOP2
        PLA
        STA ZP.TOP1
        PLA
        STA ZP.TOP0
        
        // Calculate difference: NEXT has seconds*1000, TOP has current millis
        Long.Sub(); // NEXT = NEXT - TOP
        
        // Get absolute value of difference
        LDA ZP.NEXT3
        if (MI)  // Negative?
        {
            Long.NegateNext();
        }
        
        // Check if difference is less than 1000
        LDA ZP.NEXT1
        ORA ZP.NEXT2
        ORA ZP.NEXT3
        if (NZ)  // High 24 bits are zero
        {
            PrintSecondsConversionFail();
            return;
        }
        PrintSecondsConversionPass();
    }
    
    PrintSecondsConversionPass()
    {
        PrintPassLabel();
        
        LDA #(secondsTestName % 256)
        STA ZP.STRL
        LDA #(secondsTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(conversionCorrect % 256)
        STA ZP.STRL
        LDA #(conversionCorrect / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    PrintSecondsConversionFail()
    {
        PrintFailLabel();
        
        LDA #(secondsTestName % 256)
        STA ZP.STRL
        LDA #(secondsTestName / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(conversionError % 256)
        STA ZP.STRL
        LDA #(conversionError / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
    
    // Test delay accuracy with percentage calculation
    TestDelayAccuracy()
    {
        // Test 250ms delay for reasonable accuracy measurement
        Time.Millis();
        
        // Save start to NEXT
        Shared.MoveTopToNext();
        
        // Delay 250ms
        LDA #250
        Shared.LoadTopByte(); // Sets TOP0 = 250, zeros TOP1-3
        Time.Delay();
        
        // Get end time in TOP
        Time.Millis();
        
        // Calculate actual delay: NEXT = end - start
        Shared.SwapNextTop(); // Swap so NEXT=end, TOP=start
        Long.Sub(); // NEXT = NEXT - TOP = end - start
        
        // Print header
        LDA #(delayTestName % 256)
        STA ZP.STRL
        LDA #(delayTestName / 256)
        STA ZP.STRH
        Print.String();
        
        // Print expected value
        LDA #250
        Shared.LoadTopByte();
        Long.Print();
        
        LDA #(msLabel % 256)
        STA ZP.STRL
        LDA #(msLabel / 256)
        STA ZP.STRH
        Print.String();
        
        LDA #(spaceColon % 256)
        STA ZP.STRL
        LDA #(spaceColon / 256)
        STA ZP.STRH
        Print.String();
        
        // Print actual time
        LDA #(actualLabel % 256)
        STA ZP.STRL
        LDA #(actualLabel / 256)
        STA ZP.STRH
        Print.String();
        
        // Move result to TOP for printing
        Shared.MoveNextToTop();        
        Long.Print();
        
        LDA #(msLabel % 256)
        STA ZP.STRL
        LDA #(msLabel / 256)
        STA ZP.STRH
        Print.String();
        
        Print.Space();
        
        // Check tolerance (245-255ms = ±2%)
        LDA #245
        STA ZP.NEXT0
        STZ ZP.NEXT1
        STZ ZP.NEXT2
        STZ ZP.NEXT3
        
        Long.LE(); // NEXT (245) <= TOP
        if (C)  // 245 <= TOP
        {
            LDA #255
            STA ZP.NEXT0
            Long.GE(); // NEXT >= TOP
            if (C)  // 255 >= TOP
            {
                PrintOK();
            }
            else
            {
                PrintError();
            }
        }
        else
        {
            PrintError();
        }
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
    
    PrintOK()
    {
        LDA #(okLabel % 256)
        STA ZP.STRL
        LDA #(okLabel / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
    }
    
    PrintError()
    {
        LDA #(errorLabel % 256)
        STA ZP.STRL
        LDA #(errorLabel / 256)
        STA ZP.STRH
        Print.String();
        Print.NewLine();
    }
    
    PrintFailWithDelta()
    {
        LDA #(actualLabel % 256)
        STA ZP.STRL
        LDA #(actualLabel / 256)
        STA ZP.STRH
        Print.String();
        
        Long.Print();
        
        LDA #(msLabel % 256)
        STA ZP.STRL
        LDA #(msLabel / 256)
        STA ZP.STRH
        Print.String();
        
        Print.Space();
        
        LDA #(errorLabel % 256)
        STA ZP.STRL
        LDA #(errorLabel / 256)
        STA ZP.STRH
        Print.String();
        
        Print.NewLine();
    }
}
