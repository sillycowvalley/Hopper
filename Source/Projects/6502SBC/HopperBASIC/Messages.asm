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
    const string OutOfMemory    = "OUT OF MEMORY";
    const string FileNotFound   = "FILE NOT FOUND";
    const string NextWithoutFor = "NEXT WITHOUT FOR";
    const string DivisionByZero = "DIVISION BY ZERO";
    const string StringTooLong  = "STRING TOO LONG";
    const string BadIndex       = "BAD INDEX"; // STRING and ARRAY
    const string UndefinedIdentifier = "UNDEFINED IDENTIFIER"; // FUNC or VAR
    
    
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
