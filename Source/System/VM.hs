unit VM
{
    int PC { get system; }
    int SP { get system; }
    int BP { get system; }
    bool Halted { get system; }

    bool Load(string hexePath) system;
    int InstructionsLoaded { get system; }
    
    ClearBreakpoints() system;
    SetBreakpoint(int address) system;
    
    Step() system;
    Run() system;
    
    byte GetStackByte(int sp) system;
    byte GetInstruction(int pc) system;
    byte GetInstructionType(int pc) system;
    long GetOperand(int pc) system;
}