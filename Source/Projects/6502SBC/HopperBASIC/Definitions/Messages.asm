unit Messages // Messages.asm
{
    // API Status: Clean
    // All public methods preserve caller state except for documented outputs
    // No accidental side effects or register corruption
    
    // System messages
    const string Welcome = "\nHopper BASIC v2.0\n";
    const string MemoryMsg = "MEMORY: ";
    const string BytesMsg = " BYTES AVAILABLE\n";
    
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
    
    const string FormatWarning    = "FORMAT WILL ERASE ALL FILES. CONTINUE (Y/N)? ";
    const string OverwriteWarning = "OVERWRITE (Y/N)? ";
    const string InvalidResponse = "Y or N: ";
    
    const string VoidName = "VOID";
    
#ifdef HASEEPROM    
    const string DirListHeader       = "FILES:";
    const string BytesLabel          = " BYTES";
    const string FilesLabel          = " FILES, ";
    const string BytesUsedLabel      = " BYTES USED";
#endif
    
    
    // Print OK message
    // Input: None
    // Output: "OK\n" printed to serial
    PrintOK()
    {
        PHA  // Preserve A register
        
        LDA #(OK % 256)
        STA ZP.STRL
        LDA #(OK / 256)
        STA ZP.STRH
        Print.String();
        
        PLA  // Restore A register
    }
}
