unit Messages
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Tools"
    
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // No accidental side effects or register corruption
    
    // System messages
    const string Welcome = "\nHopper BASIC v2.0\n";
    const string MemoryMsg = "Memory: ";
    const string BytesMsg = " bytes available\n";
    
    // Status messages
    const string ReadyPrompt = "READY\n> ";
    const string OK = "OK\n";
    
    // Error messages
    const string SyntaxError    = "SYNTAX ERROR";
    const string NotImplemented = "NOT IMPLEMENTED";
    const string InternalError  = "INTERNAL ERROR";
    const string OnlyInDebug = "ONLY IN DEBUG BUILD";
    
    const string TypeMismatch   = "TYPE MISMATCH";
    const string FunctionExists = "FUNCTION EXISTS";
    //const string ConstantExists = "CONSTANT EXISTS";
    const string OutOfMemory    = "OUT OF MEMORY";
    const string FileNotFound   = "FILE NOT FOUND";
    const string NextWithoutFor = "NEXT WITHOUT FOR";
    const string DivisionByZero = "DIVISION BY ZERO";
    const string NumericOverflow = "NUMERIC OVERFLOW";
    const string StringTooLong  = "STRING TOO LONG";
    const string BadIndex       = "BAD INDEX"; // STRING and ARRAY
    const string UndefinedIdentifier = "UNDEFINED IDENTIFIER"; // FUNC or VAR
    const string ConstantExpected  = "CONSTANT EXPECTED";
    const string ConstantExpressionExpected  = "CONSTANT EXPRESSION EXPECTED";
    const string IllegalVariableName  = "ILLEGAL VARIABLE NAME";
    const string IllegalAssignment  = "ILLEGAL ASSIGNMENT";
    const string InvalidOperator = "INVALID OPERATOR";
    const string BufferOverflow = "BUFFER OVERFLOW";
    const string ExpectedRightParen = ") EXPECTED";
    const string ExpectedExpression = "EXPRESSION EXPECTED";
    const string InvalidBitValue = "INVALID BIT VALUE";
    
    
    // PC -> IDY
    //
    // Note: don't mess with the stack on entry, you'll break this method
    StorePC()
    {
        PHP // preserve NC
                
        // PC is now on stack - 1
        TSX           // Transfer Stack Pointer to X
        INX
        
        LDA 0x0101,X  // Get return address high byte from stack
        STA ZP.IDYH   // Store in your PC variable
        LDA 0x0100,X  // Get return address low byte from stack  
        STA ZP.IDYL   // Store in your PC variable
        
        // IDY -= 2
        SEC
        LDA ZP.IDYL
        SBC #2
        STA ZP.IDYL
        LDA ZP.IDYH
        SBC #0
        STA ZP.IDYH
        
        PLP // preserve NC
    }
    
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
    // Output: C set if no error, NC if error occurred
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
#ifdef DEBUG
            LDA #'!'
            Serial.WriteChar();
#endif
        }
        PLA
    }
    
    // Check for error and print it if found
    // Input: None
    // Output: C set if no error, NC if error was printed
    //         Error cleared after printing
    // Modifies: ZP.LastError (cleared if error was printed)
    CheckAndPrintError()
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
#ifdef DEBUG
        // 6502 PC
        LDA #' '
        Serial.WriteChar();
        LDA #'('
        Serial.WriteChar();
        LDA #'0'
        Serial.WriteChar();
        LDA #'x'
        Serial.WriteChar();
        LDA ZP.IDYH
        Serial.HexOut();
        LDA ZP.IDYL
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
    
    // Print OK message
    // Input: None
    // Output: "OK\n" printed to serial
    PrintOK()
    {
        PHA  // Preserve A register
        
        LDA #(OK % 256)
        STA ZP.ACCL
        LDA #(OK / 256)
        STA ZP.ACCH
        Tools.PrintStringACC();
        
        PLA  // Restore A register
    }
}
