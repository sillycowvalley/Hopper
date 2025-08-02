unit Error
{
    uses "/Source/Runtime/6502/ZeroPage"
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // One-liner error methods for simplified error handling
    
#ifdef TERSE_ERRORS
    // Future enhancement: error codes instead of strings
    const uint syntaxError = 0x0001;
    const uint notImplemented = 0x0002;
    const uint internalError = 0x0003;
    const uint onlyInDebug = 0x0004;
    const uint typeMismatch = 0x0005;
    const uint functionExists = 0x0006;
    const uint constantExists = 0x0007;
    const uint variableExists = 0x0008;
    const uint outOfMemory = 0x0009;
    const uint fileNotFound = 0x000A;
    const uint nextWithoutFor = 0x000B;
    const uint divisionByZero = 0x000C;
    const uint numericOverflow = 0x000D;
    const uint stringTooLong = 0x000E;
    const uint badIndex = 0x000F;
    const uint undefinedIdentifier = 0x0010;
    const uint constantExpected = 0x0011;
    const uint constantExpressionExpected = 0x0012;
    const uint illegalIdentifier = 0x0013;
    const uint illegalAssignment = 0x0014;
    const uint invalidOperator = 0x0015;
    const uint bufferOverflow = 0x0016;
    const uint expectedRightParen = 0x0017;
    const uint expectedLeftParen = 0x0018;
    const uint expectedQuote = 0x0019;
    const uint expectedExpression = 0x001A;
    const uint invalidBitValue = 0x001B;
    const uint illegalInFunctionMode = 0x001C;
#else
    // Error message strings (moved from Messages.asm) and made private
    const string syntaxError = "SYNTAX ERROR";
    const string notImplemented = "NOT IMPLEMENTED";
    const string internalError = "INTERNAL ERROR";
    const string onlyInDebug = "ONLY IN DEBUG BUILD";
    const string typeMismatch = "TYPE MISMATCH";
    const string functionExists = "FUNCTION EXISTS";
    const string constantExists = "CONSTANT EXISTS";
    const string variableExists = "VARIABLE EXISTS";
    const string outOfMemory = "OUT OF MEMORY";
    const string fileNotFound = "FILE NOT FOUND";
    const string nextWithoutFor = "NEXT WITHOUT FOR";
    const string divisionByZero = "DIVISION BY ZERO";
    const string numericOverflow = "NUMERIC OVERFLOW";
    const string stringTooLong = "STRING TOO LONG";
    const string badIndex = "BAD INDEX";
    const string undefinedIdentifier = "UNDEFINED IDENTIFIER";
    const string constantExpected = "CONSTANT EXPECTED";
    const string constantExpressionExpected = "CONSTANT EXPRESSION EXPECTED";
    const string illegalIdentifier = "ILLEGAL IDENTIFIER";
    const string illegalAssignment = "ILLEGAL ASSIGNMENT";
    const string invalidOperator = "INVALID OPERATOR";
    const string bufferOverflow = "BUFFER OVERFLOW";
    const string expectedRightParen = ") EXPECTED";
    const string expectedLeftParen = "( EXPECTED";
    const string expectedQuote = "QUOTE EXPECTED";
    const string expectedExpression = "EXPRESSION EXPECTED";
    const string invalidBitValue = "INVALID BIT VALUE";
    const string illegalInFunctionMode = "ILLEGAL IN FUNCTION MODE";
    const string onlyAtConsole = "ONLY AT CONSOLE";
    
#endif
    
    // One-liner error methods (PC must be set at call site with BIT ZP.EmulatorPCL)
    // Each method sets ZP.LastError and clears carry flag
    
    SyntaxError() 
    { 
        LDA #(syntaxError % 256)
        STA ZP.LastErrorL
        LDA #(syntaxError / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    NotImplemented() 
    { 
        LDA #(notImplemented % 256)
        STA ZP.LastErrorL
        LDA #(notImplemented / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    InternalError() 
    { 
        LDA #(internalError % 256)
        STA ZP.LastErrorL
        LDA #(internalError / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    OnlyInDebug() 
    { 
        LDA #(onlyInDebug % 256)
        STA ZP.LastErrorL
        LDA #(onlyInDebug / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    OnlyAtConsole() 
    { 
        LDA #(onlyAtConsole % 256)
        STA ZP.LastErrorL
        LDA #(onlyAtConsole / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    
    TypeMismatch() 
    { 
        LDA #(typeMismatch % 256)
        STA ZP.LastErrorL
        LDA #(typeMismatch / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    FunctionExists() 
    { 
        LDA #(functionExists % 256)
        STA ZP.LastErrorL
        LDA #(functionExists / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    ConstantExists() 
    { 
        LDA #(constantExists % 256)
        STA ZP.LastErrorL
        LDA #(constantExists / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    VariableExists() 
    { 
        LDA #(variableExists % 256)
        STA ZP.LastErrorL
        LDA #(variableExists / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    OutOfMemory() 
    { 
        LDA #(outOfMemory % 256)
        STA ZP.LastErrorL
        LDA #(outOfMemory / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    FileNotFound() 
    { 
        LDA #(fileNotFound % 256)
        STA ZP.LastErrorL
        LDA #(fileNotFound / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    NextWithoutFor() 
    { 
        LDA #(nextWithoutFor % 256)
        STA ZP.LastErrorL
        LDA #(nextWithoutFor / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    DivisionByZero() 
    { 
        LDA #(divisionByZero % 256)
        STA ZP.LastErrorL
        LDA #(divisionByZero / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    NumericOverflow() 
    { 
        LDA #(numericOverflow % 256)
        STA ZP.LastErrorL
        LDA #(numericOverflow / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    StringTooLong() 
    { 
        LDA #(stringTooLong % 256)
        STA ZP.LastErrorL
        LDA #(stringTooLong / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    BadIndex() 
    { 
        LDA #(badIndex % 256)
        STA ZP.LastErrorL
        LDA #(badIndex / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    UndefinedIdentifier() 
    { 
        LDA #(undefinedIdentifier % 256)
        STA ZP.LastErrorL
        LDA #(undefinedIdentifier / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    ConstantExpected() 
    { 
        LDA #(constantExpected % 256)
        STA ZP.LastErrorL
        LDA #(constantExpected / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    ConstantExpressionExpected() 
    { 
        LDA #(constantExpressionExpected % 256)
        STA ZP.LastErrorL
        LDA #(constantExpressionExpected / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    IllegalIdentifier() 
    { 
        LDA #(illegalIdentifier % 256)
        STA ZP.LastErrorL
        LDA #(illegalIdentifier / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    IllegalAssignment() 
    { 
        LDA #(illegalAssignment % 256)
        STA ZP.LastErrorL
        LDA #(illegalAssignment / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    InvalidOperator() 
    { 
        LDA #(invalidOperator % 256)
        STA ZP.LastErrorL
        LDA #(invalidOperator / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    BufferOverflow() 
    { 
        LDA #(bufferOverflow % 256)
        STA ZP.LastErrorL
        LDA #(bufferOverflow / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    ExpectedRightParen() 
    { 
        LDA #(expectedRightParen % 256)
        STA ZP.LastErrorL
        LDA #(expectedRightParen / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    ExpectedLeftParen() 
    { 
        LDA #(expectedLeftParen % 256)
        STA ZP.LastErrorL
        LDA #(expectedLeftParen / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    ExpectedQuote() 
    { 
        LDA #(expectedQuote % 256)
        STA ZP.LastErrorL
        LDA #(expectedQuote / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    ExpectedExpression() 
    { 
        LDA #(expectedExpression % 256)
        STA ZP.LastErrorL
        LDA #(expectedExpression / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    InvalidBitValue() 
    { 
        LDA #(invalidBitValue % 256)
        STA ZP.LastErrorL
        LDA #(invalidBitValue / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    IllegalInFunctionMode() 
    { 
        LDA #(illegalInFunctionMode % 256)
        STA ZP.LastErrorL
        LDA #(illegalInFunctionMode / 256)
        STA ZP.LastErrorH
        CLC
    }
    
    
    // PC -> IDY
    //
    // Note: don't mess with the stack on entry, you'll break this method
    /*
    StorePC()
    {
        PHP // preserve NC
                
        // PC is now on stack - 1
        TSX           // Transfer Stack Pointer to X
        INX
        
        LDA 0x0101,X         // Get return address high byte from stack
        STA ZP.EmulatorPCH   // Store in your PC variable
        LDA 0x0100,X         // Get return address low byte from stack  
        STA ZP.EmulatorPCL   // Store in your PC variable
        
        // IDY -= 2
        SEC
        LDA ZP.EmulatorPCL
        SBC #2
        STA ZP.EmulatorPCL
        LDA ZP.EmulatorPCH
        SBC #0
        STA ZP.EmulatorPCH
        
        PLP // preserve NC
    }
    */
    // Clear error state
    // Input: None
    // Output: ZP.LastError cleared (set to 0x0000)
    ClearError()
    {
        STZ ZP.LastErrorL
        STZ ZP.LastErrorH
    }
    
    // Check if error has occurred
    // Input: None
    // Output: C set if ok, NC if not ok (error occurred)
    // Modifies: Processor flags only
    CheckError()
    {
        PHA
        
        // Returns C if no error, NC if error occurred
        LDA ZP.LastErrorL
        ORA ZP.LastErrorH
        if (Z)
        {
            SEC  // No error
        }
        else
        {
            CLC  // Error occurred
#if defined(DEBUG) || defined(TRACE)
            LDA #'!'
            Serial.WriteChar();
#endif
        }
        PLA
    }
    
    // Check if error has occurred
    // Input: None
    // Output: C set if ok, NC if not ok (error occurred)
    // Modifies: Processor flags only
    CheckErrorAndStatus()
    {
        PHA
        Error.CheckError(); // C if ok, NC if not ok (error)
        if (C)
        {
            // LastError not set, check SystemState
            State.CanContinue(); // C if all good, NC if error or exit
        }
        PLA
    }
    
    // Check for error and print it if found
    // Input: None
    // Output: C set if no error, NC if error was printed
    //         Error cleared after printing
    // Modifies: ZP.LastError (cleared if error was printed)
    CheckAndPrint()
    {
        PHA  // Preserve A register
        PHX  // Preserve X register  
        PHY  // Preserve Y register
        
        // Returns C if no error, NC if error was printed
        CheckError();
        if (C) 
        { 
            // Restore registers
            PLY
            PLX
            PLA
            return;  // No error
        }
        
        // Print the error message
        LDA #'?'
        Serial.WriteChar(); // '?' prefix
        LDA ZP.LastErrorL
        STA ZP.ACCL
        LDA ZP.LastErrorH
        STA ZP.ACCH
        Tools.PrintStringACC();
#if defined(DEBUG) || defined(TRACE)
        // 6502 PC
        LDA #' '
        Serial.WriteChar();
        LDA #'('
        Serial.WriteChar();
        LDA #'0'
        Serial.WriteChar();
        LDA #'x'
        Serial.WriteChar();
        LDA ZP.EmulatorPCH
        Serial.HexOut();
        LDA ZP.EmulatorPCL
        Serial.HexOut();
        LDA #')'
        Serial.WriteChar();
#endif        
        LDA #'\n'
        Serial.WriteChar(); // '\n' suffix
        
        // Clear the error
        ClearError();
        
        // Restore registers
        PLY
        PLX
        PLA
        
        CLC  // Error was found and printed
    }
}
