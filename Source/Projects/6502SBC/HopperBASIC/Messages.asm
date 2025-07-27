unit Messages
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Tools"
    
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
    
    const string TypeMismatch   = "TYPE MISMATCH";
    const string FunctionExists = "FUNCTION EXISTS";
    const string ConstantExists = "CONSTANT EXISTS";
    const string OutOfMemory    = "OUT OF MEMORY";
    const string FileNotFound   = "FILE NOT FOUND";
    const string NextWithoutFor = "NEXT WITHOUT FOR";
    const string DivisionByZero = "DIVISION BY ZERO";
    const string NumericOverflow = "NUMERIC OVERFLOW";
    const string StringTooLong  = "STRING TOO LONG";
    const string BadIndex       = "BAD INDEX"; // STRING and ARRAY
    const string UndefinedIdentifier = "UNDEFINED IDENTIFIER"; // FUNC or VAR
    const string ConstantExpected  = "CONSTANT EXPECTED";
    
    
    
    ClearError()
    {
        STZ ZP.LastErrorL
        STZ ZP.LastErrorH
    }
    
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
    
    CheckAndPrintError()
    {
        // Returns C if no error, NC if error was printed
        CheckError();
        if (C) { return; }  // No error
        
        // Print the error message
        LDA #'?'
        Serial.WriteChar(); // '?' prefix
        LDA ZP.LastErrorL
        STA ZP.IDXL
        LDA ZP.LastErrorH
        STA ZP.IDXH
        Tools.PrintString();
        LDA #'\n'
        Serial.WriteChar(); // '\n' suffix
        
        // Clear the error
        ClearError();
        
        CLC  // Error was found and printed
    }
    
    // Helper functions for clean message printing
    PrintOK()
    {
        LDA #(OK % 256)
        STA ZP.IDXL
        LDA #(OK / 256)
        STA ZP.IDXH
        Tools.PrintString();
    }
}
