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
        
        // Save first reading
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
        STA ZP.TOP0
        STZ ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Time.Delay();
        
        // Get second reading
        Time.Millis();
        
        // Compare with first reading
        PLA
        CMP ZP.TOP3
        if (NZ) { PrintMillisPass(); return; }
        PLA
        CMP ZP.TOP2
        if (NZ) { PrintMillisPass(); return; }
        PLA
        CMP ZP.TOP1
        if (NZ) { PrintMillisPass(); return; }
        PLA
        CMP ZP.TOP0
        if (NZ) { PrintMillisPass(); return; }
        
        // Values are identical - fail
        PrintMillisFail();
    }
    
    PrintMillisPass()
    {
        // Clean up stack if needed
        PLA PLA PLA PLA
        
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
        TestDelayWithValue(1);
    }
    
    // Test 10ms delay
    TestDelay10ms()
    {
        TestDelayWithValue(10);
    }
    
    // Test 100ms delay
    TestDelay100ms()
    {
        TestDelayWithValue(100);
    }
    
    // Test 1000ms delay with detailed timing
    TestDelay1000ms()
    {
        // Get start time
        Time.Millis();
        
        // Save start time
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        LDA ZP.TOP2
        PHA
        LDA ZP.TOP3
        PHA
        
        // Delay for 1000ms
        LDA #(1000 % 256)
        STA ZP.TOPL
        LDA #(1000 / 256)
        STA ZP.TOPH
        Time.Delay();
        
        // Get end time
        Time.Millis();
        
        // Calculate delta: end - start
        // Pop start time into NEXT
        PLA
        STA ZP.NEXT3
        PLA
        STA ZP.NEXT2
        PLA
        STA ZP.NEXT1
        PLA
        STA ZP.NEXT0
        
        // TOP already has end time
        // Calculate TOP - NEXT
        Long.Sub();
        Long.PopTop();
        
        // Print result header
        LDA #(delayTestName % 256)
        STA ZP.STRL
        LDA #(delayTestName / 256)
        STA ZP.STRH
        Print.String();
        
        // Print delay value
        LDA #(1000 % 256)
        STA ZP.TOPL
        LDA #(1000 / 256)
        STA ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
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
        
        // Check if delta is between 995 and 1005 (0.5% tolerance)
        // First check if >= 995
        LDA ZP.TOP0
        CMP #(995 % 256)
        LDA ZP.TOP1
        SBC #(995 / 256)
        LDA ZP.TOP2
        SBC #0
        LDA ZP.TOP3
        SBC #0
        if (NC)  // >= 995
        {
            // Now check if <= 1005
            LDA #(1005 % 256)
            CMP ZP.TOP0
            LDA #(1005 / 256)
            SBC ZP.TOP1
            LDA #0
            SBC ZP.TOP2
            LDA #0
            SBC ZP.TOP3
            if (NC)  // <= 1005
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
        PHA  // Save delay value
        
        // Get start time
        Time.Millis();
        
        // Save start time (only need low 16 bits for small delays)
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        
        // Set up delay
        PLA  // Get TOP1 back
        PLA  // Get TOP0 back
        PLA  // Get delay value
        STA ZP.TOPL
        STZ ZP.TOPH
        
        PHA  // Save delay value again
        
        Time.Delay();
        
        // Get end time
        Time.Millis();
        
        PLA  // Get delay value
        
        // Print test name
        LDA #(delayTestName % 256)
        STA ZP.STRL
        LDA #(delayTestName / 256)
        STA ZP.STRH
        Print.String();
        
        // Print delay value
        STA ZP.TOPL
        STZ ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
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
        // Get current milliseconds
        Time.Millis();
        
        // Save milliseconds
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        LDA ZP.TOP2
        PHA
        LDA ZP.TOP3
        PHA
        
        // Get seconds
        Time.Seconds();
        
        // TOP now has seconds
        // Multiply by 1000 to convert back to milliseconds
        LDA #(1000 % 256)
        STA ZP.NEXT0
        LDA #(1000 / 256)
        STA ZP.NEXT1
        STZ ZP.NEXT2
        STZ ZP.NEXT3
        
        Long.Mul();
        Long.PopTop();
        
        // Pop original millis into NEXT
        PLA
        STA ZP.NEXT3
        PLA
        STA ZP.NEXT2
        PLA
        STA ZP.NEXT1
        PLA
        STA ZP.NEXT0
        
        // Calculate difference
        Long.Sub();  // TOP = TOP - NEXT
        Long.PopTop();
        
        // Check if difference is less than 1000
        LDA ZP.TOP2
        ORA ZP.TOP3
        if (Z)  // High 16 bits are zero
        {
            LDA ZP.TOP1
            CMP #(1000 / 256)
            if (C)  // >= 4 (1000 = 0x03E8)
            {
                PrintSecondsConversionFail();
                return;
            }
        }
        else
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
        
        // Save start (32-bit)
        LDA ZP.TOP0
        STA ZP.NEXT0
        LDA ZP.TOP1
        STA ZP.NEXT1
        LDA ZP.TOP2
        STA ZP.NEXT2
        LDA ZP.TOP3
        STA ZP.NEXT3
        
        // Delay 250ms
        LDA #250
        STA ZP.TOPL
        STZ ZP.TOPH
        Time.Delay();
        
        // Get end time
        Time.Millis();
        
        // Calculate actual delay: TOP = TOP - NEXT
        Long.Sub();
        Long.PopTop();
        
        // Print header
        LDA #(delayTestName % 256)
        STA ZP.STRL
        LDA #(delayTestName / 256)
        STA ZP.STRH
        Print.String();
        
        // Print expected value
        LDA #250
        STA ZP.TOPL
        STZ ZP.TOPH
        STZ ZP.TOPT
        Print.Decimal();
        
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
        
        STZ ZP.TOPT
        Print.Decimal();
        
        LDA #(msLabel % 256)
        STA ZP.STRL
        LDA #(msLabel / 256)
        STA ZP.STRH
        Print.String();
        
        Print.Space();
        
        // Check tolerance (245-255ms = ±2%)
        LDA ZP.TOP0
        CMP #245
        LDA ZP.TOP1
        SBC #0
        LDA ZP.TOP2
        SBC #0
        LDA ZP.TOP3
        SBC #0
        if (NC)  // >= 245
        {
            LDA #255
            CMP ZP.TOP0
            LDA #0
            SBC ZP.TOP1
            LDA #0
            SBC ZP.TOP2
            LDA #0
            SBC ZP.TOP3
            if (NC)  // <= 255
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
        
        STZ ZP.TOPT
        Print.Decimal();
        
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
