unit Runtime
{
    
#if defined(MCU)
    // used from RTC.SetFromDebugger(..)
    bool InDebugger { get system; }
    string DateTime { get system; }
#endif
    
    // launch another application 
    //    (on exit, restore the currently running one)
    uint Execute(string programPath, <string> arguments) system;
    
    // base address of the inline code RuntTime.Inline(..)
    uint UserCode { get system; }
    
    // execute an array of Hopper opCodes inline
    //   (use & operator to determine offsets of locals and globals)
    uint Inline(byte[] code, uint startIndex) system;
    
    // currently used in TiggerBasic to get current line number from PC - probably doesn't work as expected
    uint PC { get system; }
    
}
