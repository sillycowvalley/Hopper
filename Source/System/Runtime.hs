unit Runtime
{
    uint PC { get system; }
    uint SP { get system; }
    uint CSP { get system; }
    uint BP { get system; }
    
    uint UserCode { get system; }
    
    uint    GetStackWord(uint address) system;     // address offset in bytes
    type    GetStackType(uint address) system;     // address offset in bytes
    uint    GetCallStackWord(uint address) system; // address offset in words
    
    // launch another application 
    //    (on exit, restore the currently running one)
    uint Execute(string programPath, <string> arguments) system;
    
    // execute an array of Hopper opCodes inline
    //   (use & operator to determine offsets of locals and globals)
    uint Inline(byte[] code, uint startIndex) system;
    
}
