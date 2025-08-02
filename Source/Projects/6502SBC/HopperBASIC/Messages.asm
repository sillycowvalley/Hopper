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
    const string NoMainProgram = "NO MAIN PROGRAM";
    
    // String constant for BEGIN function name
    const string BeginFunctionName = "$MAIN";
    
    
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
