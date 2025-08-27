unit Messages // Messages.asm
{
    // System messages
    const string Welcome = "\nHopper BASIC v2.0\n";
    
    const string BeginFunctionName = "$MAIN"; // String constant for BEGIN function name
    const string ForVarName = "$F";           // String constant for name of "fake" TO and STEP variable slots in FOR loop
    const string AutoexecName = "AUTOEXEC";   // String constant for name for "autoexec" feature
    
    const string ErrorMarker = "<-----";
    
    const string VoidName = "VOID";                     // used in BASICTypes.PrintType()

#ifdef DEBUG
    // used in BASICSysCalls.ToString
    const string PrintChar  = "PRINTCHAR";
    const string PrintValue = "PRINTVALUE";
#endif

    Main() // point ZP.TOP -> "$MAIN"
    {
        LDA #(Messages.BeginFunctionName % 256) // TOPL
        STA ZP.TOPL
        LDA #(Messages.BeginFunctionName / 256)
        STA ZP.TOPH
    }

}
