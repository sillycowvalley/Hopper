unit Runtime
{
    uint PC { get system; }
    uint SP { get system; }
    uint CSP { get system; }
    uint BP { get system; }
    bool Halted { get system; set system; }
    bool Waiting { get system; }

    bool Load(string hexePath, <string> arguments) system;
    uint BytesLoaded { get system; }
    
    ClearBreakpoints() system;
    SetBreakpoint(uint address) system;
    ClearStatements() system;
    SetStatement(uint address) system;
    
    Run() system;
    
    uint    GetStackWord(uint address) system;     // address offset in bytes
    variant GetStackVariant(uint address) system;  // address offset in bytes
    type    GetStackType(uint address) system;     // address offset in bytes
    uint    GetCallStackWord(uint address) system; // address offset in words
    
    SetVisibility(bool visible) system;
    SetStepping(bool stepping) system;
    StepInto() system;
    StepOver() system;
    StepRun() system;
}
