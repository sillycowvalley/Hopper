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
    const string IllegalVariableName  = "ILLEGAL VARIABLE NAME";
    
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
