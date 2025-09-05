unit Tests
{
    
    RunTests()
    {
        TestBasics();
        TestComparisons();
        TestUtilities();
    }
    
    TestBasics()
    {
        // Test basic operations with simple values
        // We'll manually create small IEEE 754 values
        
        // Test Add: 2.0 + 1.0 = 3.0
        CreateFloat2();  // NEXT = 2.0
        CreateFloat1();  // TOP = 1.0  
        Float.Add();
        PrintFloatAsLong();
        
        // Test Sub: 5.0 - 2.0 = 3.0
        CreateFloat5();  // NEXT = 5.0
        CreateFloat2();  // TOP = 2.0
        Float.Sub();
        PrintFloatAsLong();
        
        // Test simple zero
        Float.New();  // Creates 0.0 in NEXT
        PrintFloatAsLong();
        
        NewLine();
    }
    
    TestComparisons()
    {
        // Test LT: 2.0 < 3.0 (should be true)
        CreateFloat2();  // NEXT = 2.0
        CreateFloat3();  // TOP = 3.0
        Float.LT();
        PrintBool();
        
        // Test EQ: 2.0 == 2.0 (should be true)
        CreateFloat2();  // NEXT = 2.0
        CreateFloat2();  // TOP = 2.0
        Float.EQ();
        PrintBool();
        
        NewLine();
    }
    
    TestUtilities()
    {
        // Test New() creates zero
        Float.New();
        Float.IsZeroNext();
        PrintBool();
        
        // Test non-zero detection
        CreateFloat1();
        Float.IsZeroNext();
        // Should return false (X=0), we want to show 'F'
        CPX #0
        if (Z)
        {
            LDA #'F'  // Correctly detected non-zero
        }
        else
        {
            LDA #'E'  // Error
        }
        Serial.WriteChar();
        
        NewLine();
    }
    
    // Create simple IEEE 754 single precision floats
    // IEEE 754: [sign][8-bit exponent][23-bit mantissa]
    // Stored as: [mantissa-low][mantissa-mid][mantissa-high+exp-low][sign+exp-high]
    
    CreateFloat1()  // 1.0 = 0x3F800000
    {
        LDA #0x00
        STA ZP.TOP0     // Mantissa low
        LDA #0x00  
        STA ZP.TOP1     // Mantissa mid
        LDA #0x80
        STA ZP.TOP2     // Mantissa high + exp low bit
        LDA #0x3F
        STA ZP.TOP3     // Sign + exp high
    }
    
    CreateFloat2()  // 2.0 = 0x40000000  
    {
        LDA #0x00
        STA ZP.NEXT0    // Mantissa low
        LDA #0x00
        STA ZP.NEXT1    // Mantissa mid  
        LDA #0x00
        STA ZP.NEXT2    // Mantissa high + exp low bit
        LDA #0x40
        STA ZP.NEXT3    // Sign + exp high
    }
    
    CreateFloat3()  // 3.0 = 0x40400000
    {
        LDA #0x00
        STA ZP.TOP0     // Mantissa low
        LDA #0x00
        STA ZP.TOP1     // Mantissa mid
        LDA #0x40
        STA ZP.TOP2     // Mantissa high + exp low bit  
        LDA #0x40
        STA ZP.TOP3     // Sign + exp high
    }
    
    CreateFloat5()  // 5.0 = 0x40A00000
    {
        LDA #0x00
        STA ZP.NEXT0    // Mantissa low
        LDA #0x00
        STA ZP.NEXT1    // Mantissa mid
        LDA #0xA0
        STA ZP.NEXT2    // Mantissa high + exp low bit
        LDA #0x40 
        STA ZP.NEXT3    // Sign + exp high
    }
    
    // Print float value as integer (for simple verification)
    PrintFloatAsLong()
    {
        Shared.MoveNextToTop();
        Float.ToLong();
        Long.Print();
        LDA #' '
        Serial.WriteChar();
    }
    
    NewLine()
    {
        LDA #'\n'
        Serial.WriteChar();
    }
    
    // Print T for true or F for false (X register holds result)
    PrintBool()
    {
        CPX #1
        if (Z)
        {
            LDA #'T'
        }
        else
        {
            LDA #'F'
        }
        Serial.WriteChar();
    }
}
