unit Tests
{
    RunTests()
    {
        
        TestArithmetic();
        TestComparisons();
        TestUtilities();
    }
    
    TestArithmetic()
    {
        // Test Add: 100 + 50 = 150
        LDA #100
        STA ZP.NEXT0
        Shared.ZeroNext3();
        LDA #50
        Shared.LoadTopByte();
        Long.Add();
        // Should print 150
        Shared.MoveNextToTop();
        Long.Print();
        LDA #' '
        Serial.WriteChar();
        
        // Test Sub: 200 - 75 = 125  
        LDA #200
        STA ZP.NEXT0
        Shared.ZeroNext3();
        LDA #75
        Shared.LoadTopByte();
        Long.Sub();
        // Should print 125
        Shared.MoveNextToTop();
        Long.Print();
        LDA #' '
        Serial.WriteChar();
        
        // Test Mul: 12 * 5 = 60
        LDA #12
        STA ZP.NEXT0
        Shared.ZeroNext3();
        LDA #5
        Shared.LoadTopByte();
        Long.Mul();
        // Should print 60
        Shared.MoveNextToTop();
        Long.Print();
        LDA #' '
        Serial.WriteChar();
        
        // Test Div: 84 / 7 = 12
        LDA #84
        STA ZP.NEXT0
        Shared.ZeroNext3();
        LDA #7
        Shared.LoadTopByte();
        Long.Div();
        // Should print 12
        Shared.MoveNextToTop();
        Long.Print();
        LDA #' '
        Serial.WriteChar();
        
        // Test Mod: 17 % 5 = 2
        LDA #17
        STA ZP.NEXT0
        Shared.ZeroNext3();
        LDA #5
        Shared.LoadTopByte();
        Long.Mod();
        // Should print 2
        Shared.MoveNextToTop();
        Long.Print();
        NewLine();
    }
    
    TestComparisons()
    {
        // Test LT: 50 < 100 (should be true = T)
        LDA #50
        STA ZP.NEXT0
        Shared.ZeroNext3();
        LDA #100
        Shared.LoadTopByte();
        Long.LT();
        PrintResult();
        
        // Test GT: 100 > 50 (should be true = T)
        LDA #100
        STA ZP.NEXT0
        Shared.ZeroNext3();
        LDA #50
        Shared.LoadTopByte();
        Long.GT();
        PrintResult();
        
        // Test EQ: 75 == 75 (should be true = T)
        LDA #75
        STA ZP.NEXT0
        Shared.ZeroNext3();
        LDA #75
        Shared.LoadTopByte();
        Long.EQ();
        PrintResult();
        
        // Test NE: 10 != 20 (should be true = T)
        LDA #10
        STA ZP.NEXT0
        Shared.ZeroNext3();
        LDA #20
        Shared.LoadTopByte();
        Long.NE();
        PrintResult();
        
        NewLine();
    }
    
    TestUtilities()
    {
        // Test Print function
        LDA #123
        Shared.LoadTopByte();
        Long.Print();
        LDA #' '
        Serial.WriteChar();
        
        // Test ZeroCheckTop
        Shared.ZeroTop();
        Shared.ZeroCheckTop();
        if (Z)
        {
            LDA #'P'  // Pass
        }
        else
        {
            LDA #'F'  // Fail
        }
        Serial.WriteChar();
        
        NewLine();
    }
    
    // Print T for true (carry set) or F for false (carry clear)
    PrintResult()
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
