unit Tests
{
    
    RunTests()
    {
        // Expected output:
        // 5 3 3 -5
        // 3 5 -5 -3
        // 6 -6 -6 6
        // 3 -3 -3 3
        // 
        // TTTFTFFTF
        // TF
        // !
        TestBasics();
        TestComparisons();
        TestUtilities();
    }
    
    TestBasics()
    {
        // Add tests: +ve+ve, +ve-ve, -ve+ve, -ve-ve
        CreateFloat2();         // NEXT = 2.0
        CreateFloat3();         // TOP = 3.0
        Float.Add();           // 2.0 + 3.0 = 5.0
        PrintFloatAsLong();
        
        CreateFloat5();         // NEXT = 5.0
        CreateFloatNegTwoTop(); // TOP = -2.0
        Float.Add();           // 5.0 + (-2.0) = 3.0
        PrintFloatAsLong();
        
        CreateFloatNegTwo();    // NEXT = -2.0
        CreateFloatFiveTop();   // TOP = 5.0
        Float.Add();           // (-2.0) + 5.0 = 3.0
        PrintFloatAsLong();
        
        CreateFloatNegTwo();    // NEXT = -2.0
        CreateFloatNegThreeTop(); // TOP = -3.0
        Float.Add();           // (-2.0) + (-3.0) = -5.0
        PrintFloatAsLong();
        
        NewLine();
        
        // Sub tests: +ve+ve, +ve-ve, -ve+ve, -ve-ve
        CreateFloat5();         // NEXT = 5.0
        CreateFloatTwoTop();    // TOP = 2.0
        Float.Sub();           // 5.0 - 2.0 = 3.0
        PrintFloatAsLong();
        
        CreateFloatThreeNext(); // NEXT = 3.0
        CreateFloatNegTwoTop(); // TOP = -2.0
        Float.Sub();           // 3.0 - (-2.0) = 5.0
        PrintFloatAsLong();
        
        CreateFloatNegTwo();    // NEXT = -2.0
        CreateFloat3();         // TOP = 3.0
        Float.Sub();           // (-2.0) - 3.0 = -5.0
        PrintFloatAsLong();
        
        CreateFloatNegFive();   // NEXT = -5.0
        CreateFloatNegTwoTop(); // TOP = -2.0
        Float.Sub();           // (-5.0) - (-2.0) = -3.0
        PrintFloatAsLong();
        
        NewLine();
        
        // Mul tests: +ve+ve, +ve-ve, -ve+ve, -ve-ve
        CreateFloat2();         // NEXT = 2.0
        CreateFloat3();         // TOP = 3.0
        Float.Mul();           // 2.0 * 3.0 = 6.0
        PrintFloatAsLong();
        
        CreateFloatThreeNext(); // NEXT = 3.0
        CreateFloatNegTwoTop(); // TOP = -2.0
        Float.Mul();           // 3.0 * (-2.0) = -6.0
        PrintFloatAsLong();
        
        CreateFloatNegTwo();    // NEXT = -2.0
        CreateFloat3();         // TOP = 3.0
        Float.Mul();           // (-2.0) * 3.0 = -6.0
        PrintFloatAsLong();
        
        CreateFloatNegTwo();    // NEXT = -2.0
        CreateFloatNegThreeTop(); // TOP = -3.0
        Float.Mul();           // (-2.0) * (-3.0) = 6.0
        PrintFloatAsLong();
        
        NewLine();
        
        // Div tests: +ve+ve, +ve-ve, -ve+ve, -ve-ve
        CreateFloatSix();       // NEXT = 6.0
        CreateFloatTwoTop();    // TOP = 2.0
        Float.Div();           // 6.0 / 2.0 = 3.0
        PrintFloatAsLong();
        
        CreateFloatSix();       // NEXT = 6.0
        CreateFloatNegTwoTop(); // TOP = -2.0
        Float.Div();           // 6.0 / (-2.0) = -3.0
        PrintFloatAsLong();
        
        CreateFloatNegSixNext(); // NEXT = -6.0
        CreateFloatTwoTop();    // TOP = 2.0
        Float.Div();           // (-6.0) / 2.0 = -3.0
        PrintFloatAsLong();
        
        CreateFloatNegSixNext(); // NEXT = -6.0
        CreateFloatNegTwoTop(); // TOP = -2.0
        Float.Div();           // (-6.0) / (-2.0) = 3.0
        PrintFloatAsLong();
        
        NewLine();
    }
    
    
    TestComparisons()
    {
        // EQ corner cases
        
        // Test 1: 0.0 == 0.0 (both zero)
        CreateFloatZero();       // TOP = 0.0
        Shared.MoveTopToNext();  // NEXT = 0.0
        CreateFloatZero();       // TOP = 0.0
        Float.EQ();              // 0.0 == 0.0 ? T
        PrintBool();
        
        // Test 2: 0.0 == -0.0 (positive zero vs negative zero)
        CreateFloatZero();       // TOP = 0.0
        Shared.MoveTopToNext();  // NEXT = 0.0
        CreateFloatNegZero();    // TOP = -0.0
        Float.EQ();              // 0.0 == -0.0 ? T
        PrintBool();
        
        // LT corner cases
        
        // Test 3: -1.0 < 1.0 (signs differ: negative < positive)
        CreateFloatNegOne();     // TOP = -1.0
        Shared.MoveTopToNext();  // NEXT = -1.0
        CreateFloat1();          // TOP = 1.0
        Float.LT();              // -1.0 < 1.0 ? T
        PrintBool();
        
        // Test 4: 1.0 < -1.0 (signs differ: positive < negative)
        CreateFloat1();          // TOP = 1.0
        Shared.MoveTopToNext();  // NEXT = 1.0
        CreateFloatNegOne();     // TOP = -1.0
        Float.LT();              // 1.0 < -1.0 ? F
        PrintBool();
        
        // Test 5: 1.0 < 2.0 (same sign, different exponents)
        CreateFloat1();          // TOP = 1.0
        Shared.MoveTopToNext();  // NEXT = 1.0
        CreateFloatTwo();        // TOP = 2.0
        Float.LT();              // 1.0 < 2.0 ? T
        PrintBool();
        
        // Test 6: 2.0 < 1.0 (same sign, different exponents, reverse)
        CreateFloatTwo();        // TOP = 2.0
        Shared.MoveTopToNext();  // NEXT = 2.0
        CreateFloat1();          // TOP = 1.0
        Float.LT();              // 2.0 < 1.0 ? F
        PrintBool();
        
        // Test 7: 1.0 < 1.0 (same sign, same exponent, equal mantissas)
        CreateFloat1();          // TOP = 1.0
        Shared.MoveTopToNext();  // NEXT = 1.0
        CreateFloat1();          // TOP = 1.0
        Float.LT();              // 1.0 < 1.0 ? F
        PrintBool();
        
        // Test 8: 1.0 < 1.5 (same sign, same exponent, different mantissas)
        CreateFloat1();          // TOP = 1.0
        Shared.MoveTopToNext();  // NEXT = 1.0
        CreateFloatOnePointFive(); // TOP = 1.5
        Float.LT();              // 1.0 < 1.5 ? T
        PrintBool();
        
        // Test 9: 1.5 < 1.0 (same sign, same exponent, different mantissas, reverse)
        CreateFloatOnePointFive(); // TOP = 1.5
        Shared.MoveTopToNext();  // NEXT = 1.5
        CreateFloat1();          // TOP = 1.0
        Float.LT();              // 1.5 < 1.0 ? F
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
        if (C)
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
    
    CreateFloatZero()  // 0.0 = 0x00000000
    {
        STZ ZP.TOP0
        STZ ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
    }
    
    CreateFloatNegZero()  // -0.0 = 0x80000000
    {
        STZ ZP.TOP0
        STZ ZP.TOP1
        STZ ZP.TOP2
        LDA #0x80
        STA ZP.TOP3
    }
    
    CreateFloatNegOne()  // -1.0 = 0xBF800000
    {
        STZ ZP.TOP0
        STZ ZP.TOP1
        LDA #0x80
        STA ZP.TOP2
        LDA #0xBF
        STA ZP.TOP3
    }
    
    CreateFloatTwo()  // 2.0 = 0x40000000 (puts in TOP, unlike existing CreateFloat2)
    {
        STZ ZP.TOP0
        STZ ZP.TOP1
        STZ ZP.TOP2
        LDA #0x40
        STA ZP.TOP3
    }
    
    CreateFloatOnePointFive()  // 1.5 = 0x3FC00000
    {
        STZ ZP.TOP0
        STZ ZP.TOP1
        LDA #0xC0
        STA ZP.TOP2
        LDA #0x3F
        STA ZP.TOP3
    }
    
    
    CreateFloatSix()  // 6.0 = 0x40C00000 (puts in NEXT)
    {
        STZ ZP.NEXT0
        STZ ZP.NEXT1
        LDA #0xC0
        STA ZP.NEXT2
        LDA #0x40
        STA ZP.NEXT3
    }
    
    CreateFloatTwoTop()  // 2.0 = 0x40000000 (puts in TOP)
    {
        STZ ZP.TOP0
        STZ ZP.TOP1
        STZ ZP.TOP2
        LDA #0x40
        STA ZP.TOP3
    }
    
    CreateFloatFiveTop()  // 5.0 = 0x40A00000 (puts in TOP)
    {
        STZ ZP.TOP0
        STZ ZP.TOP1
        LDA #0xA0
        STA ZP.TOP2
        LDA #0x40
        STA ZP.TOP3
    }
    
    CreateFloatNegTwo()  // -2.0 = 0xC0000000 (puts in NEXT)
    {
        STZ ZP.NEXT0
        STZ ZP.NEXT1
        STZ ZP.NEXT2
        LDA #0xC0
        STA ZP.NEXT3
    }
    
    CreateFloatNegTwoTop()  // -2.0 = 0xC0000000 (puts in TOP)
    {
        STZ ZP.TOP0
        STZ ZP.TOP1
        STZ ZP.TOP2
        LDA #0xC0
        STA ZP.TOP3
    }
    
    CreateFloatNegOneTop()  // -1.0 = 0xBF800000 (puts in TOP)
    {
        STZ ZP.TOP0
        STZ ZP.TOP1
        LDA #0x80
        STA ZP.TOP2
        LDA #0xBF
        STA ZP.TOP3
    }
    
    CreateFloatNegFive()  // -5.0 = 0xC0A00000 (puts in NEXT)
    {
        STZ ZP.NEXT0
        STZ ZP.NEXT1
        LDA #0xA0
        STA ZP.NEXT2
        LDA #0xC0
        STA ZP.NEXT3
    }
    
    CreateFloatThreeNext()  // 3.0 = 0x40400000 (puts in NEXT)
    {
        STZ ZP.NEXT0
        STZ ZP.NEXT1
        LDA #0x40
        STA ZP.NEXT2
        LDA #0x40
        STA ZP.NEXT3
    }
    
    CreateFloatNegThreeTop()  // -3.0 = 0xC0400000 (puts in TOP)
    {
        STZ ZP.TOP0
        STZ ZP.TOP1
        LDA #0x40
        STA ZP.TOP2
        LDA #0xC0
        STA ZP.TOP3
    }
    
    CreateFloatNegSixNext()  // -6.0 = 0xC0C00000 (puts in NEXT)
    {
        STZ ZP.NEXT0
        STZ ZP.NEXT1
        LDA #0xC0
        STA ZP.NEXT2
        LDA #0xC0
        STA ZP.NEXT3
    }
    
    
    // Print float value as integer (for simple verification)
    // assumes float is in NEXT
    PrintFloatAsLong()
    {
        Float.ToLong(); // NEXT -> NEXT
        
        Shared.MoveNextToTop();
        Long.Print();
        LDA #' '
        Serial.WriteChar();
    }
    
    // Print T for true or F for false (C flag holds result)
    PrintBool()
    {
        if (C)
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
