unit Messages
{
    uses "/Source/Runtime/6502/ZeroPage"
    uses "Tools"
    
    // System messages
    const string Welcome = "\nHopper BASIC v2.0\n";
    const string MemoryMsg = "Memory: ";
    const string BytesMsg = " bytes available\n";
    const string ReadyPrompt = "READY\n> ";
    const string Goodbye = "GOODBYE\n";
    
    // Status messages
    const string OK = "OK\n";
    const string SyntaxError = "?SYNTAX ERROR\n";
    const string NotImplemented = "NOT IMPLEMENTED\n";
    
    // Error messages for future use
    const string UndefinedVariable = "?UNDEFINED VARIABLE\n";
    const string CannotAssignConstant = "?CANNOT ASSIGN TO CONSTANT\n";
    const string MissingExpression = "?MISSING EXPRESSION\n";
    const string ExpectedEquals = "?EXPECTED =\n";
    const string InvalidType = "?INVALID TYPE\n";
    const string ExpectedIdentifier = "?EXPECTED IDENTIFIER\n";
    
    // Error handling - consistent with v1 pattern
    
    ClearError()
    {
        STZ ZP.LastErrorL
        STZ ZP.LastErrorH
    }
    
    CheckError()
    {
        // Returns Z if no error, NZ if error occurred
        LDA ZP.LastErrorL
        ORA ZP.LastErrorH
    }
    
    CheckAndPrintError()
    {
        // Returns Z if no error, NZ if error was printed
        CheckError();
        if (Z) { return; }  // No error
        
        // Print the error message
        LDA ZP.LastErrorL
        STA ZP.IDXL
        LDA ZP.LastErrorH
        STA ZP.IDXH
        Tools.PrintString();
        
        // Clear the error
        ClearError();
        
        // Set NZ to indicate error was found
        LDA #1
        CMP #0
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
