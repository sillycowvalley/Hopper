unit Messages // Messages.asm
{
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
    
    const string BeginFunctionName = "$MAIN"; // String constant for BEGIN function name
    const string ForVarName = "$F";           // String constant for name of "fake" TO and STEP variable slots in FOR loop
    
    // used in BASICSysCalls.ToString
    const string PrintChar  = "PRINTCHAR";
    const string PrintValue = "PRINTVALUE";
    
    const string ErrorMarker = "<------";
    
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
